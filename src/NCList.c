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
	return R_NilValue;
}


/****************************************************************************
 * NCList_print()
 */

/* Print 1 line per range in 'nclist'. Return max depth. */
static int print_NCList(const NCList *nclist,
			const int *x_start, const int *x_end,
			int depth, const char *format)
{
	int max_depth, n, d, tmp;
	const NCListElt *elt;

	max_depth = depth;
	for (n = 0, elt = nclist->elts; n < nclist->nelt; n++, elt++) {
		for (d = 0; d < depth; d++)
			Rprintf("|");
		Rprintf(format, elt->i + 1);
		Rprintf(": [%d, %d]\n", x_start[elt->i], x_end[elt->i]);
		tmp = print_NCList(elt->sublist,
				   x_start, x_end, depth + 1,
				   format);
		if (tmp > max_depth)
			max_depth = tmp;
	}
	return max_depth;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_print(SEXP x_nclist, SEXP x_start, SEXP x_end)
{
	const NCList *top_nclist;
	int x_len, max_digits, max_depth;
	const int *x_start_p, *x_end_p;
	char format[10];

	top_nclist = (NCList *) R_ExternalPtrAddr(x_nclist);
	if (top_nclist == NULL)
		error("NCList_print: pointer to NCList struct is NULL");
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	max_digits = x_len ? (int) log10((double) x_len) + 1 : 0;
	sprintf(format, "%c%d%c", '%', max_digits, 'd');
	max_depth = print_NCList(top_nclist, x_start_p, x_end_p, 0, format);
	Rprintf("max depth = %d\n", max_depth);
	return R_NilValue;
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
 * NCList_find_overlaps()
 */

static void NCList_unlist(const NCList *nclist, IntAE *out)
{
	int n;
	const NCListElt *elt;

	for (n = 0, elt = nclist->elts; n < nclist->nelt; n++, elt++) {
		IntAE_insert_at(out, IntAE_get_nelt(out), elt->i + 1);
		NCList_unlist(elt->sublist, out);
	}
	return;
}

/*
 * Look for the first element in 'nclist' that points to a range with an
 * end >= 'q_start'.
 */
static int bsearch_n1(int q_start, const NCList *nclist, const int *s_end)
{
	int n1, n2, n, end;

	/* Check first element. */
	n1 = 0;
	end = s_end[nclist->elts[n1].i];
	if (end >= q_start)
		return n1;

	/* Check last element. */
	n2 = nclist->nelt - 1;
	end = s_end[nclist->elts[n2].i];
	if (end < q_start)
		return nclist->nelt;
	if (end == q_start)
		return n2;

	/* Binary search. */
	while ((n = (n1 + n2) / 2) != n1) {
		end = s_end[nclist->elts[n].i];
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
			   const NCList *nclist,
			   const int *s_start, const int *s_end,
			   IntAE *out)
{
	int n, start;
	const NCListElt *elt;

	if (nclist->nelt == 0)
		return;
	n = bsearch_n1(q_start, nclist, s_end);
	for (elt = nclist->elts + n;
	     n < nclist->nelt;
	     n++, elt++)
	{
		start = s_start[elt->i];
		if (start > q_end)
			break;
		IntAE_insert_at(out, IntAE_get_nelt(out), elt->i + 1);
		//if (start >= q_start && s_end[elt->i] <= q_end)
		//	NCList_unlist(elt->sublist, out);
		//else
			NCList_overlap(q_start, q_end,
				       elt->sublist,
				       s_start, s_end,
				       out);
	}
	return;
}

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

/* --- .Call ENTRY POINT ---
 * Usage:
 *   query <- IRanges(c(5, 1), c(7, 4))
 *   .Call("NCList_find_overlaps", start(query), end(query),
 *                                 nclist, start(x), end(x),
 *                                 PACKAGE="IRanges")
 */
SEXP NCList_find_overlaps(SEXP q_start, SEXP q_end,
			  SEXP s_nclist, SEXP s_start, SEXP s_end)
{
	const NCList *top_nclist;
	int q_len, s_len, m;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	IntAEAE buf;
	IntAE *buf_elt;

	top_nclist = (const NCList *) R_ExternalPtrAddr(s_nclist);
	if (top_nclist == NULL)
		error("NCList_find_overlaps: pointer to NCList struct is NULL");
	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(query)", "end(query)");
	s_len = check_integer_pairs(s_start, s_end,
				    &s_start_p, &s_end_p,
				    "start(subject)", "end(subject)");
	buf = new_IntAEAE(q_len, q_len);
	for (m = 0, buf_elt = buf.elts;
	     m < q_len;
	     m++, buf_elt++, q_start_p++, q_end_p++)
	{
		NCList_overlap(*q_start_p, *q_end_p,
			       top_nclist, s_start_p, s_end_p,
			       buf_elt);
		IntAE_qsort(buf_elt, 0);
	}
	return new_Hits_from_IntAEAE(&buf, s_len);
}

