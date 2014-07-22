/****************************************************************************
 *                An Nested Containment List implementation                 *
 *                                                                          *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h>  /* for malloc, realloc, free, qsort */
#include <math.h>    /* for log10 */


typedef struct nclist {
	int buflength;           /* always >= 0 */
	int nelt;                /* always >= 0 and <= buflength */
	struct nclistelt *elts;
} NCList;

typedef struct nclistelt {
	int i;
	struct nclist *sublist;
} NCListElt;


/****************************************************************************
 * new_INTEGER_from_NCList()
 */

#define	NCLIST_NELT(nclist) ((nclist)[0])
#define	NCLIST_I(nclist, n) ((nclist)[((n)<<1)+1])
#define	SUBLIST_OFFSET(nclist, n) ((nclist)[((n)<<1)+2])

static int compute_length_of_NCList_as_INTEGER(const NCList *nclist)
{
	int nelt, n;
	unsigned int ans_len, dump_len;
	const NCListElt *elt;

	nelt = nclist->nelt;
	if (nelt == 0)
		return 0;
	ans_len = 1U + 2U * (unsigned int) nelt;
	for (n = 0, elt = nclist->elts; n < nelt; n++, elt++) {
		dump_len = compute_length_of_NCList_as_INTEGER(elt->sublist);
		ans_len += dump_len;
		if (ans_len < dump_len)
			goto too_big;
	}
	if (ans_len <= INT_MAX)
		return (int) ans_len;
too_big:
	error("compute_length_of_NCList_as_INTEGER: "
	      "NCList object is too big to fit in an integer vector");
}

static int dump_NCList_as_int_array(const NCList *nclist, int *out)
{
	int nelt, offset, dump_len, n;
	const NCListElt *elt;

	nelt = nclist->nelt;
	if (nelt == 0)
		return 0;
	offset = 1 + 2 * nelt;
	NCLIST_NELT(out) = nelt;
	for (n = 0, elt = nclist->elts; n < nelt; n++, elt++) {
		NCLIST_I(out, n) = elt->i;
		dump_len = dump_NCList_as_int_array(elt->sublist, out + offset);
		SUBLIST_OFFSET(out, n) = dump_len != 0 ? offset : -1;
		offset += dump_len;
	}
	return offset;
}

static SEXP new_INTEGER_from_NCList(const NCList *top_nclist)
{
	SEXP ans;
	int ans_len;

	ans_len = compute_length_of_NCList_as_INTEGER(top_nclist);
	PROTECT(ans = NEW_INTEGER(ans_len));
	dump_NCList_as_int_array(top_nclist, INTEGER(ans));
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * NCList_new()
 */

static void init_NCList(NCList *nclist)
{
	nclist->buflength = nclist->nelt = 0;
	nclist->elts = NULL;
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_new()
{
	NCList *top_nclist;

	top_nclist = (NCList *) malloc(sizeof(NCList));
	if (top_nclist == NULL)
		error("NCList_new: memory allocation failed");
	init_NCList(top_nclist);
	return R_MakeExternalPtr(top_nclist, R_NilValue, R_NilValue);
}


/****************************************************************************
 * NCList_free()
 */

static void free_NCList(NCList *nclist)
{
	int n;
	const NCListElt *elt;

	if (nclist->buflength != 0) {
		for (n = 0, elt = nclist->elts; n < nclist->nelt; n++, elt++)
			free_NCList(elt->sublist);
		free(nclist->elts);
	}
	free(nclist);
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_free(SEXP nclist)
{
	NCList *top_nclist;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist);
	if (top_nclist == NULL)
		error("NCList_free: pointer to NCList struct is NULL");
	free_NCList(top_nclist);
	R_SetExternalPtrAddr(nclist, NULL);
	return R_NilValue;
}


/****************************************************************************
 * NCList_build()
 */

static void init_NCListElt(NCListElt *elt, int i)
{
	elt->sublist = (NCList *) malloc(sizeof(NCList));
	if (elt->sublist == NULL)
		error("init_NCListElt: memory allocation failed");
	elt->i = i;
	init_NCList(elt->sublist);
	return;
}

static void extend_NCList(NCList *nclist)
{
	int old_buflength, new_buflength;
	size_t elt_size;
	NCListElt *new_elts;

	old_buflength = nclist->buflength;
	elt_size = sizeof(NCListElt);
	if (old_buflength == 0) {
		new_buflength = 4;
		new_elts = (NCListElt *) malloc(new_buflength * elt_size);
	} else {
		if (old_buflength < 16384)
			new_buflength = 8 * old_buflength;
		else if (old_buflength < 4194304)
			new_buflength = 4 * old_buflength;
		else if (old_buflength < 67108864)
			new_buflength = 2 * old_buflength;
		else
			new_buflength = old_buflength + 33554432;
		new_elts = (NCListElt *) realloc(nclist->elts,
						 new_buflength * elt_size);
	}
	if (new_elts == NULL)
		error("extend_NCList: memory allocation failed");
	nclist->buflength = new_buflength;
	nclist->elts = new_elts;
	return;
}

static NCListElt *add_NCList_elt(NCList *nclist, int i)
{
	NCListElt *new_elt;

	if (nclist->nelt == nclist->buflength)
		extend_NCList(nclist);
	new_elt = nclist->elts + nclist->nelt;
	init_NCListElt(new_elt, i);
	nclist->nelt++;
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

static void build_NCList(NCList *top_nclist,
			 const int *x_start, const int *x_end, int x_len)
{
	NCListElt *new_elt;

	int *oo, k, d, i, current_end;
	static NCListElt *stack[1000];

	// Determine order of 'x'. 'oo' will be such that 'x[oo]' is sorted
	// first by ascending start then by descending end.
	oo = (int *) R_alloc(sizeof(int), x_len);
	for (i = 0; i < x_len; i++)
		oo[i] = i;
	aa = x_start;
	bb = x_end;
	qsort(oo, x_len, sizeof(int), qsort_compar);

	init_NCList(top_nclist);
	for (k = 0, d = -1; k < x_len; k++) {
		i = oo[k];
		current_end = x_end[i];
		while (d >= 0 && x_end[stack[d]->i] < current_end)
			d--;
		if (d == -1) {
			// append range i to top-level
			new_elt = add_NCList_elt(top_nclist, i);
		} else {
			// append range i to sublist of stack[d]
			new_elt = add_NCList_elt(stack[d]->sublist, i);
		}
		stack[++d] = new_elt;
	}
	return;
}

SEXP NCList_build(SEXP nclist, SEXP x_start, SEXP x_end)
{
	NCList *top_nclist;
	int x_len;
	const int *x_start_p, *x_end_p;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist);
	if (top_nclist == NULL)
		error("NCList_build: pointer to NCList struct is NULL");
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	build_NCList(top_nclist, x_start_p, x_end_p, x_len);
	return new_INTEGER_from_NCList(top_nclist);
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
	if (nelt == 0)
		return max_depth;
	for (n = 0; n < nelt; n++) {
		for (d = 1; d < depth; d++)
			Rprintf("|");
		i = NCLIST_I(nclist, n);
		Rprintf(format, i + 1);
		Rprintf(": [%d, %d]\n", x_start[i], x_end[i]);
		offset = SUBLIST_OFFSET(nclist, n);
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
	if (nelt == 0)
		return;
	n = bsearch_n1(q_start, nclist, s_end);
	for ( ; n < nelt; n++) {
		i = NCLIST_I(nclist, n);
		start = s_start[i];
		if (start > q_end)
			break;
		IntAE_insert_at(out, IntAE_get_nelt(out), i + 1);
		offset = SUBLIST_OFFSET(nclist, n);
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

