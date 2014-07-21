/****************************************************************************
 *                An Nested Containment List implementation                 *
 *                                                                          *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h>  /* for malloc, realloc, free, qsort */

typedef struct nclist {
	int buflength;
	struct nclistelt *elts;
	int nelt;
} NCList;

typedef struct nclistelt {
	int i;
	struct nclist sublist;
} NCListElt;


/****************************************************************************
 * NCList_build()
 */

static void init_NCList(NCList *nclist)
{
	nclist->buflength = 0;
	nclist->elts = NULL;
	nclist->nelt = 0;
	return;
}

static void init_NCListElt(NCListElt *elt, int i)
{
	elt->i = i;
	init_NCList(&(elt->sublist));
	return;
}

static void extend_NCList(NCList *nclist)
{
	size_t size;
	int new_buflength;

	size = sizeof(NCListElt);
	// FIXME: Handle malloc()/realloc() failure.
	if (nclist->elts == NULL) {
		new_buflength = 8;
		nclist->elts = malloc(new_buflength * size);
	} else {
		if (nclist->buflength < 32768)
			new_buflength = 8 * nclist->buflength;
		else if (nclist->buflength < 8388608)
			new_buflength = 4 * nclist->buflength;
		else if (nclist->buflength < 134217728)
			new_buflength = 2 * nclist->buflength;
		else
			new_buflength = nclist->buflength + 67108864;
		nclist->elts = realloc(nclist->elts, new_buflength * size);
	}
	nclist->buflength = new_buflength;
	return;
}

static NCListElt *add_NCList_elt(NCList *nclist, int i)
{
	NCListElt *new_elt;

	if (nclist->nelt == nclist->buflength)
		extend_NCList(nclist);
	new_elt = nclist->elts + nclist->nelt++;
	init_NCListElt(new_elt, i);
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
			new_elt = add_NCList_elt(&(stack[d]->sublist), i);
		}
		stack[++d] = new_elt;
	}
	return;
}

/* --- .Call ENTRY POINT ---
 * Usage:
 *   x <- IRanges(c(1, 4, 3), c(5, 4, 4))
 *   nclist <- .Call("NCList_build", start(x), end(x), PACKAGE="IRanges")
 */
SEXP NCList_build(SEXP x_start, SEXP x_end)
{
	NCList *top_nclist;
	int x_len;
	const int *x_start_p, *x_end_p;

	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	// FIXME: Handle malloc() failure.
	top_nclist = (NCList *) malloc(sizeof(NCList));
	build_NCList(top_nclist, x_start_p, x_end_p, x_len);
	return R_MakeExternalPtr(top_nclist, R_NilValue, R_NilValue);
}


/****************************************************************************
 * NCList_free()
 */

static void free_NCList(const NCList *nclist)
{
	int n;

	if (nclist->elts == NULL)
		return;
	for (n = 0; n < nclist->nelt; n++)
		free_NCList(&(nclist->elts[n].sublist));
	free(nclist->elts);
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_free(SEXP x)
{
	NCList *top_nclist;

	top_nclist = (NCList *) R_ExternalPtrAddr(x);
	free_NCList(top_nclist);
	free(top_nclist);
	return R_NilValue;
}


/****************************************************************************
 * NCList_find_overlaps()
 */

/*
 * Look for the first element in 'nclist' that points to a range with an
 * end >= 'q_start'.
 */
static int bsearch_n1(int q_start, const NCList *nclist, const int *s_end)
{
	int i, cmp, n1, n2, n;

	i = nclist->elts[0].i;
	cmp = s_end[i] - q_start;
	if (cmp >= 0)
		return 0;
	i = nclist->elts[nclist->nelt - 1].i;
	cmp = s_end[i] - q_start;
	if (cmp < 0)
		return nclist->nelt;
	if (cmp == 0)
		return nclist->nelt - 1;
	n1 = 0;
	n2 = nclist->nelt - 1;
	while ((n = (n1 + n2) / 2) != n1) {
		i = nclist->elts[n].i;
		cmp = s_end[i] - q_start;
		if (cmp == 0)
			return n;
		if (cmp < 0)
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
        int n, i;

	if (nclist->nelt == 0)
		return;
	for (n = bsearch_n1(q_start, nclist, s_end);
	     n < nclist->nelt;
	     n++)
	{
		i = nclist->elts[n].i;
		if (s_start[i] > q_end)
			break;
		IntAE_insert_at(out, IntAE_get_nelt(out), i + 1);
		NCList_overlap(q_start, q_end,
			       &(nclist->elts[n].sublist),
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
	int q_len, s_len, m;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	const NCList *top_nclist;
	IntAEAE buf;
	IntAE *buf_elt;

	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(query)", "end(query)");
	s_len = check_integer_pairs(s_start, s_end,
				    &s_start_p, &s_end_p,
				    "start(subject)", "end(subject)");
	top_nclist = (const NCList *) R_ExternalPtrAddr(s_nclist);
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

