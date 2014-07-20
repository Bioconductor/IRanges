#include "IRanges.h"
#include "S4Vectors_interface.h"
#include <stdlib.h>  /* for malloc, realloc, free, qsort */

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

typedef struct nclist {
	int buflength;
	struct nclistelt *elts;
	int nelt;
} NCList;

typedef struct nclistelt {
	int i;
	struct nclist sublist;
} NCListElt;

static void NCList_init(NCList *nclist)
{
	nclist->buflength = 0;
	nclist->elts = NULL;
	nclist->nelt = 0;
	return;
}

static void NCListElt_init(NCListElt *elt, int i)
{
	elt->i = i;
	NCList_init(&(elt->sublist));
	return;
}

static void NCList_extend(NCList *nclist)
{
	size_t size;
	int new_buflength;

	size = sizeof(NCListElt);
	// FIXME: Handle malloc()/realloc() failure.
	if (nclist->elts == NULL) {
		new_buflength = 100;
		nclist->elts = malloc(new_buflength * size);
	} else {
		new_buflength = 2 * nclist->buflength;
		nclist->elts = realloc(nclist->elts, new_buflength * size);
	}
	nclist->buflength = new_buflength;
	return;
}

static void NCList_free(const NCList *nclist)
{
	int n;

	for (n = 0; n < nclist->nelt; n++)
		NCList_free(&(nclist->elts[n].sublist));
	free(nclist->elts);
	return;
}

/* Return NULL if 'nclist' is empty. */
static SEXP new_LIST_from_NCList(const NCList *nclist)
{
	SEXP ans, ans_elt0, ans_elt1, sub_ans;
	int n;
	const NCListElt *elt;

	if (nclist->nelt == 0)
		return R_NilValue;
	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_elt0 = NEW_INTEGER(nclist->nelt));
	SET_VECTOR_ELT(ans, 0, ans_elt0);
	UNPROTECT(1);
	PROTECT(ans_elt1 = NEW_LIST(nclist->nelt));
	SET_VECTOR_ELT(ans, 1, ans_elt1);
	UNPROTECT(1);
	for (n = 0; n < nclist->nelt; n++) {
		elt = nclist->elts + n;
		INTEGER(ans_elt0)[n] = elt->i;
		PROTECT(sub_ans = new_LIST_from_NCList(&(elt->sublist)));
		SET_VECTOR_ELT(ans_elt1, n, sub_ans);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

static NCListElt *NCList_add_elt(NCList *nclist, int i)
{
	NCListElt *new_elt;

	if (nclist->nelt == nclist->buflength)
		NCList_extend(nclist);
	new_elt = nclist->elts + nclist->nelt++;
	NCListElt_init(new_elt, i);
	return new_elt;
}

static NCList new_NCList(const int *x_start, const int *x_end, int x_len)
{
	NCList top_nclist;
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

	NCList_init(&top_nclist);
	for (k = 0, d = -1; k < x_len; k++) {
		i = oo[k];
		current_end = x_end[i];
		while (d >= 0 && x_end[stack[d]->i] < current_end)
			d--;
		if (d == -1) {
			// append range i to top-level
			new_elt = NCList_add_elt(&top_nclist, i);
		} else {
			// append range i to sublist of stack[d]
			new_elt = NCList_add_elt(&(stack[d]->sublist), i);
		}
		stack[++d] = new_elt;
	}
	return top_nclist;
}

/* --- .Call ENTRY POINT ---
 * Usage:
 *   x <- IRanges(c(1, 4, 3), c(5, 4, 4))
 *   nclist <- .Call("NCList_build", start(x), end(x), PACKAGE="IRanges")
 */
SEXP NCList_build(SEXP x_start, SEXP x_end)
{
	SEXP ans;
	NCList nclist;
	int x_len;
	const int *x_start_p, *x_end_p;

	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	nclist = new_NCList(x_start_p, x_end_p, x_len);
	PROTECT(ans = new_LIST_from_NCList(&nclist));
	NCList_free(&nclist);
	UNPROTECT(1);
	return ans;
}

// Look for the first element in 'slide' that points to a range with
// an end >= 'q_start'.
static int bsearch_n1(int q_start, const int *slide, int slide_len,
		     const int *s_end)
{
	int i, cmp, n1, n2, n;

	i = slide[0];
	cmp = s_end[i] - q_start;
	if (cmp >= 0)
		return 0;
	i = slide[slide_len - 1];
	cmp = s_end[i] - q_start;
	if (cmp < 0)
		return slide_len;
	if (cmp == 0)
		return slide_len - 1;
	n1 = 0;
	n2 = slide_len - 1;
	while ((n = (n1 + n2) / 2) != n1) {
		i = slide[n];
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
			   SEXP s_nclist, const int *s_start, const int *s_end,
			   IntAE *out)
{
	SEXP s_elt0, s_elt1;
	const int *slide;
        int slide_len, n, i;

	if (s_nclist == R_NilValue)
		return;
	s_elt0 = VECTOR_ELT(s_nclist, 0); // integer vector
	slide_len = LENGTH(s_elt0);
	slide = INTEGER(s_elt0);
	s_elt1 = VECTOR_ELT(s_nclist, 1); // list
	for (n = bsearch_n1(q_start, slide, slide_len, s_end);
	     n < slide_len;
	     n++)
	{
		i = slide[n];
		if (s_start[i] > q_end)
			break;
		IntAE_insert_at(out, IntAE_get_nelt(out), i + 1);
		NCList_overlap(q_start, q_end,
			       VECTOR_ELT(s_elt1, n), s_start, s_end,
			       out);
	}
	IntAE_qsort(out, 0);
	return;
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
	int q_len, m;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	IntAEAE ans_buf;

	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(query)", "end(query)");
	check_integer_pairs(s_start, s_end,
			    &s_start_p, &s_end_p,
			    "start(subject)", "end(subject)");
	ans_buf = new_IntAEAE(q_len, q_len);
	for (m = 0; m < q_len; m++) {
		NCList_overlap(q_start_p[m], q_end_p[m],
			       s_nclist, s_start_p, s_end_p,
			       ans_buf.elts + m);
	}
	return new_LIST_from_IntAEAE(&ans_buf, 1);
}

