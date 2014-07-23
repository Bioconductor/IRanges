/****************************************************************************
 *                An Nested Containment List implementation                 *
 *                                                                          *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h>  /* for malloc, realloc, free, qsort */
#include <math.h>    /* for log10 */


typedef struct pnclist {
	int buflength;           /* always >= 0 */
	int nelt;                /* always >= 0 and <= buflength */
	struct pnclistelt *elts;
} preNCList;

typedef struct pnclistelt {
	int i;
	struct pnclist *sublist;
} preNCListElt;


/****************************************************************************
 * preNCList_new()
 */

static void init_preNCList(preNCList *pnclist)
{
	pnclist->buflength = pnclist->nelt = 0;
	pnclist->elts = NULL;
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP preNCList_new()
{
	preNCList *top_pnclist;

	top_pnclist = (preNCList *) malloc(sizeof(preNCList));
	if (top_pnclist == NULL)
		error("preNCList_new: memory allocation failed");
	init_preNCList(top_pnclist);
	return R_MakeExternalPtr(top_pnclist, R_NilValue, R_NilValue);
}


/****************************************************************************
 * preNCList_free()
 */

static void free_preNCList(preNCList *pnclist)
{
	int n;
	const preNCListElt *elt;

	if (pnclist->buflength != 0) {
		for (n = 0, elt = pnclist->elts; n < pnclist->nelt; n++, elt++)
			free_preNCList(elt->sublist);
		free(pnclist->elts);
	}
	free(pnclist);
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP preNCList_free(SEXP pnclist)
{
	preNCList *top_pnclist;

	top_pnclist = (preNCList *) R_ExternalPtrAddr(pnclist);
	if (top_pnclist == NULL)
		error("preNCList_free: pointer to preNCList struct is NULL");
	free_preNCList(top_pnclist);
	R_SetExternalPtrAddr(pnclist, NULL);
	return R_NilValue;
}


/****************************************************************************
 * preNCList_build()
 */

static void init_preNCListElt(preNCListElt *elt, int i)
{
	elt->sublist = (preNCList *) malloc(sizeof(preNCList));
	if (elt->sublist == NULL)
		error("init_preNCListElt: memory allocation failed");
	elt->i = i;
	init_preNCList(elt->sublist);
	return;
}

static void extend_preNCList(preNCList *pnclist)
{
	int old_buflength, new_buflength;
	size_t elt_size;
	preNCListElt *new_elts;

	old_buflength = pnclist->buflength;
	elt_size = sizeof(preNCListElt);
	if (old_buflength == 0) {
		new_buflength = 4;
		new_elts = (preNCListElt *) malloc(new_buflength * elt_size);
	} else {
		if (old_buflength < 16384)
			new_buflength = 8 * old_buflength;
		else if (old_buflength < 4194304)
			new_buflength = 4 * old_buflength;
		else if (old_buflength < 67108864)
			new_buflength = 2 * old_buflength;
		else
			new_buflength = old_buflength + 33554432;
		new_elts = (preNCListElt *) realloc(pnclist->elts,
						    new_buflength * elt_size);
	}
	if (new_elts == NULL)
		error("extend_preNCList: memory allocation failed");
	pnclist->buflength = new_buflength;
	pnclist->elts = new_elts;
	return;
}

static preNCListElt *add_preNCList_elt(preNCList *pnclist, int i)
{
	preNCListElt *new_elt;

	if (pnclist->nelt == pnclist->buflength)
		extend_preNCList(pnclist);
	new_elt = pnclist->elts + pnclist->nelt;
	init_preNCListElt(new_elt, i);
	pnclist->nelt++;
	return new_elt;
}

static const int *aa, *bb;

static int qsort_compar(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = aa[i1] - aa[i2];
	if (ret != 0)
		return ret;
	ret = bb[i2] - bb[i1];
	return ret;
}

static preNCListElt **stack = NULL;
static int stack_maxdepth = 0;

static void extend_stack()
{
	int new_maxdepth;
	preNCListElt **new_stack;

	if (stack_maxdepth == 0) {
		new_maxdepth = 1000;
		new_stack = (preNCListElt **) malloc(new_maxdepth *
						     sizeof(preNCListElt *));
	} else {
		new_maxdepth = 2 * stack_maxdepth;
		new_stack = (preNCListElt **) realloc(stack,
						      new_maxdepth *
						      sizeof(preNCListElt *));
	}
	if (new_stack == NULL)
		error("extend_stack: memory allocation failed");
	stack_maxdepth = new_maxdepth;
	stack = new_stack;
	return;
}

static void build_preNCList(preNCList *top_pnclist,
			    const int *x_start, const int *x_end, int x_len)
{
	preNCListElt *new_elt;

	int *oo, k, d, i, current_end;

	// Determine order of 'x'. 'oo' will be such that 'x[oo]' is sorted
	// first by ascending start then by descending end.
	oo = (int *) R_alloc(sizeof(int), x_len);
	for (i = 0; i < x_len; i++)
		oo[i] = i;
	aa = x_start;
	bb = x_end;
	qsort(oo, x_len, sizeof(int), qsort_compar);

	init_preNCList(top_pnclist);
	for (k = 0, d = -1; k < x_len; k++) {
		i = oo[k];
		current_end = x_end[i];
		while (d >= 0 && x_end[stack[d]->i] < current_end)
			d--;
		if (d == -1) {
			// append range i to top-level
			new_elt = add_preNCList_elt(top_pnclist, i);
		} else {
			// append range i to sublist of stack[d]
			new_elt = add_preNCList_elt(stack[d]->sublist, i);
		}
		if (++d == stack_maxdepth)
			extend_stack();
		stack[d] = new_elt;
	}
	return;
}

SEXP preNCList_build(SEXP pnclist, SEXP x_start, SEXP x_end)
{
	preNCList *top_pnclist;
	int x_len;
	const int *x_start_p, *x_end_p;

	top_pnclist = (preNCList *) R_ExternalPtrAddr(pnclist);
	if (top_pnclist == NULL)
		error("preNCList_build: pointer to preNCList struct is NULL");
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	build_preNCList(top_pnclist, x_start_p, x_end_p, x_len);
	return pnclist;
}


/****************************************************************************
 * new_NCList_from_preNCList()
 */

#define	NCLIST_NELT(nclist) ((nclist)[0])
#define	NCLIST_I(nclist, n) ((nclist)[((n)<<1)+1])
#define	NCSUBLIST_OFFSET(nclist, n) ((nclist)[((n)<<1)+2])

static int compute_length_of_preNCList_as_INTEGER(const preNCList *pnclist)
{
	int nelt, n;
	unsigned int ans_len, dump_len;
	const preNCListElt *elt;

	nelt = pnclist->nelt;
	if (nelt == 0)
		return 0;
	ans_len = 1U + 2U * (unsigned int) nelt;
	for (n = 0, elt = pnclist->elts; n < nelt; n++, elt++) {
		dump_len = compute_length_of_preNCList_as_INTEGER(elt->sublist);
		ans_len += dump_len;
		if (ans_len < dump_len)
			goto too_big;
	}
	if (ans_len <= INT_MAX)
		return (int) ans_len;
too_big:
	error("compute_length_of_preNCList_as_INTEGER: "
	      "preNCList object is too big to fit in an integer vector");
}

static int dump_preNCList_as_int_array(const preNCList *pnclist, int *out)
{
	int nelt, offset, dump_len, n;
	const preNCListElt *elt;

	nelt = pnclist->nelt;
	if (nelt == 0)
		return 0;
	offset = 1 + 2 * nelt;
	NCLIST_NELT(out) = nelt;
	for (n = 0, elt = pnclist->elts; n < nelt; n++, elt++) {
		NCLIST_I(out, n) = elt->i;
		dump_len = dump_preNCList_as_int_array(elt->sublist,
						       out + offset);
		NCSUBLIST_OFFSET(out, n) = dump_len != 0 ? offset : -1;
		offset += dump_len;
	}
	return offset;
}

/* --- .Call ENTRY POINT --- */
SEXP new_NCList_from_preNCList(SEXP pnclist)
{
	SEXP ans;
	const preNCList *top_pnclist;
	int ans_len;

	top_pnclist = (preNCList *) R_ExternalPtrAddr(pnclist);
	if (top_pnclist == NULL)
		error("new_NCList_from_preNCList: "
		      "pointer to preNCList struct is NULL");
	ans_len = compute_length_of_preNCList_as_INTEGER(top_pnclist);
	PROTECT(ans = NEW_INTEGER(ans_len));
	dump_preNCList_as_int_array(top_pnclist, INTEGER(ans));
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * NCList_print()
 */

/* Print 1 line per range in 'nclist'. Return max depth. */
static int print_NCList(const int *nclist,
			const int *x_start, const int *x_end,
			int depth, const char *format)
{
	int max_depth, nelt, n, d, i, offset, tmp;

	max_depth = depth;
	nelt = NCLIST_NELT(nclist);
	for (n = 0; n < nelt; n++) {
		for (d = 1; d < depth; d++)
			Rprintf("|");
		i = NCLIST_I(nclist, n);
		Rprintf(format, i + 1);
		Rprintf(": [%d, %d]\n", x_start[i], x_end[i]);
		offset = NCSUBLIST_OFFSET(nclist, n);
		if (offset != -1) {
			tmp = print_NCList(nclist + offset,
					   x_start, x_end, depth + 1,
					   format);
			if (tmp > max_depth)
				max_depth = tmp;
		}
	}
	return max_depth;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_print(SEXP x_nclist, SEXP x_start, SEXP x_end)
{
	const int *top_nclist;
	int x_len, max_digits, max_depth;
	const int *x_start_p, *x_end_p;
	char format[10];

	top_nclist = INTEGER(x_nclist);
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	if (x_len == 0) {
		max_depth = 0;
	} else {
		max_digits = (int) log10((double) x_len) + 1;
		sprintf(format, "%c%d%c", '%', max_digits, 'd');
		max_depth = print_NCList(top_nclist, x_start_p, x_end_p,
					 1, format);
	}
	Rprintf("max depth = %d\n", max_depth);
	return R_NilValue;
}


/****************************************************************************
 * NCList_find_overlaps()
 */

static SEXP new_Hits_from_IntAEAE(const IntAEAE *x, int s_len)
{
	SEXP classdef, ans, ans_queryHits, ans_subjectHits,
	     ans_queryLength, ans_subjectLength;
	int q_len, ans_len, i, x_elt_len, j, k;
	const IntAE *x_elt;

	q_len = IntAEAE_get_nelt(x);
	ans_len = 0;
	for (i = 0, x_elt = x->elts; i < q_len; i++, x_elt++)
		ans_len += IntAE_get_nelt(x_elt);
	PROTECT(ans_queryHits = NEW_INTEGER(ans_len));
	PROTECT(ans_subjectHits = NEW_INTEGER(ans_len));
	k = 0;
	for (i = 0, x_elt = x->elts; i < q_len; i++, x_elt++) {
		x_elt_len = IntAE_get_nelt(x_elt);
		for (j = 0; j < x_elt_len; j++) {
			INTEGER(ans_queryHits)[k] = i + 1;
			INTEGER(ans_subjectHits)[k] = x_elt->elts[j];
			k++;
		}
	}
	PROTECT(classdef = MAKE_CLASS("Hits"));
	PROTECT(ans = NEW_OBJECT(classdef));
	SET_SLOT(ans, install("queryHits"), ans_queryHits);
	SET_SLOT(ans, install("subjectHits"), ans_subjectHits);
	PROTECT(ans_queryLength = ScalarInteger(q_len));
	SET_SLOT(ans, install("queryLength"), ans_queryLength);
	PROTECT(ans_subjectLength = ScalarInteger(s_len));
	SET_SLOT(ans, install("subjectLength"), ans_subjectLength);
	UNPROTECT(6);
	return ans;
}

static int bsearch_n1(int q_start, const int *nclist, const int *s_end)
{
	int n1, n2, nelt, n, end;

	/* Check first element. */
	n1 = 0;
	end = s_end[NCLIST_I(nclist, n1)];
	if (end >= q_start)
		return n1;

	/* Check last element. */
	nelt = NCLIST_NELT(nclist);
	n2 = nelt - 1;
	end = s_end[NCLIST_I(nclist, n2)];
	if (end < q_start)
		return nelt;
	if (end == q_start)
		return n2;

	/* Binary search. */
	while ((n = (n1 + n2) / 2) != n1) {
		end = s_end[NCLIST_I(nclist, n)];
		if (end == q_start)
			return n;
		if (end < q_start)
			n1 = n;
		else
			n2 = n;
	}
	return n2;
}

static void NCList_overlap(int q_start, int q_end,
			   const int *nclist,
			   const int *s_start, const int *s_end,
			   IntAE *out)
{
	int nelt, n, i, start, offset;

	nelt = NCLIST_NELT(nclist);
	n = bsearch_n1(q_start, nclist, s_end);
	for ( ; n < nelt; n++) {
		i = NCLIST_I(nclist, n);
		start = s_start[i];
		if (start > q_end)
			break;
		IntAE_insert_at(out, IntAE_get_nelt(out), i + 1);
		offset = NCSUBLIST_OFFSET(nclist, n);
		if (offset != -1)
			NCList_overlap(q_start, q_end,
				       nclist + offset,
				       s_start, s_end,
				       out);
	}
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_find_overlaps(SEXP q_start, SEXP q_end,
			  SEXP s_nclist, SEXP s_start, SEXP s_end)
{
	const int *top_nclist;
	int q_len, s_len, m;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	IntAEAE buf;
	IntAE *buf_elt;

	top_nclist = INTEGER(s_nclist);
	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(query)", "end(query)");
	s_len = check_integer_pairs(s_start, s_end,
				    &s_start_p, &s_end_p,
				    "start(subject)", "end(subject)");
	buf = new_IntAEAE(q_len, q_len);
	if (s_len != 0) {
		for (m = 0, buf_elt = buf.elts;
		     m < q_len;
		     m++, buf_elt++, q_start_p++, q_end_p++)
		{
			NCList_overlap(*q_start_p, *q_end_p,
				       top_nclist, s_start_p, s_end_p,
				       buf_elt);
			IntAE_qsort(buf_elt, 0);
		}
	}
	return new_Hits_from_IntAEAE(&buf, s_len);
}

