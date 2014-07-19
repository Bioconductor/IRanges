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

typedef struct ncl {
	int buflength;
	struct ncl_elt *elts;
	int nelt;
} NCL;

typedef struct ncl_elt {
	int i;
	struct ncl child_ncl;
} NCL_ELT;

static void NCL_init(NCL *ncl)
{
	ncl->buflength = 0;
	ncl->elts = NULL;
	ncl->nelt = 0;
	return;
}

static void NCL_ELT_init(NCL_ELT *ncl_elt, int i)
{
	ncl_elt->i = i;
	NCL_init(&(ncl_elt->child_ncl));
	return;
}

static void NCL_extend(NCL *ncl)
{
	size_t size;
	int new_buflength;

	size = sizeof(struct ncl_elt);
	// FIXME: Handle malloc()/realloc() failure.
	if (ncl->elts == NULL) {
		new_buflength = 100;
		ncl->elts = malloc(new_buflength * size);
	} else {
		new_buflength = 2 * ncl->buflength;
		ncl->elts = realloc(ncl->elts, new_buflength * size);
	}
	ncl->buflength = new_buflength;
	return;
}

static void NCL_free(const NCL *ncl)
{
	int n;

	for (n = 0; n < ncl->nelt; n++)
		NCL_free(&(ncl->elts[n].child_ncl));
	free(ncl->elts);
	return;
}

/* Return NULL if 'ncl' is empty. */
static SEXP new_LIST_from_NCL(const NCL *ncl)
{
	SEXP ans, ans_elt0, ans_elt1, sub_ans;
	int n;
	const NCL_ELT *ncl_elt;

	if (ncl->nelt == 0)
		return R_NilValue;
	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_elt0 = NEW_INTEGER(ncl->nelt));
	SET_VECTOR_ELT(ans, 0, ans_elt0);
	UNPROTECT(1);
	PROTECT(ans_elt1 = NEW_LIST(ncl->nelt));
	SET_VECTOR_ELT(ans, 1, ans_elt1);
	UNPROTECT(1);
	for (n = 0; n < ncl->nelt; n++) {
		ncl_elt = ncl->elts + n;
		INTEGER(ans_elt0)[n] = ncl_elt->i + 1;
		PROTECT(sub_ans = new_LIST_from_NCL(&(ncl_elt->child_ncl)));
		SET_VECTOR_ELT(ans_elt1, n, sub_ans);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

static NCL_ELT *NCL_add_elt(NCL *ncl, int i)
{
	NCL_ELT *new_elt;

	//Rprintf("append range %d to top-level\n",
	//	i + 1);
	if (ncl->nelt == ncl->buflength)
		NCL_extend(ncl);
	new_elt = ncl->elts + ncl->nelt++;
	NCL_ELT_init(new_elt, i);
	return new_elt;
}

static NCL new_NCL(const int *x_start, const int *x_end, int x_len)
{
	NCL ncl;
	NCL_ELT *new_elt;

	int *oo, k, d, i, current_end;
	static NCL_ELT *stack[1000];

	// Determine order of 'x'. 'oo' will be such that 'x[oo]' is sorted
	// first by ascending start then by descending end.
	oo = (int *) R_alloc(sizeof(int), x_len);
	for (i = 0; i < x_len; i++)
		oo[i] = i;
	aa = x_start;
	bb = x_end;
	qsort(oo, x_len, sizeof(int), qsort_compar);

	NCL_init(&ncl);
	for (k = 0, d = -1; k < x_len; k++) {
		i = oo[k];
		current_end = x_end[i];
		while (d >= 0 && x_end[stack[d]->i] < current_end)
			d--;
		if (d == -1) {
			// append range i to top-level
			new_elt = NCL_add_elt(&ncl, i);
		} else {
			// append range i to children of stack[d]
			new_elt = NCL_add_elt(&(stack[d]->child_ncl), i);
		}
		stack[++d] = new_elt;
	}
	return ncl;
}

/* --- .Call ENTRY POINT ---
 * Usage:
 *   x <- IRanges(c(1, 4, 3), c(5, 4, 4))
 *   ncl <- .Call("build_NCL", start(x), end(x), PACKAGE="IRanges")
 */
SEXP build_NCL(SEXP x_start, SEXP x_end)
{
	SEXP ans;
	NCL ncl;
	int x_len;
	const int *x_start_p, *x_end_p;

	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	ncl = new_NCL(x_start_p, x_end_p, x_len);
	PROTECT(ans = new_LIST_from_NCL(&ncl));
	NCL_free(&ncl);
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
	cmp = s_end[i - 1] - q_start;
	if (cmp >= 0)
		return 0;
	i = slide[slide_len - 1];
	cmp = s_end[i - 1] - q_start;
	if (cmp < 0)
		return slide_len;
	if (cmp == 0)
		return slide_len - 1;
	n1 = 0;
	n2 = slide_len - 1;
	while (n2 - n1 >= 2) {
		n = (n1 + n2) / 2;
		i = slide[n];
		cmp = s_end[i - 1] - q_start;
		if (cmp == 0)
			return n;
		if (cmp < 0)
			n1 = n;
		else
			n2 = n;
	}
	return n2;
}

static void overlap_NCL(int q_start, int q_end,
			SEXP s_ncl, const int *s_start, const int *s_end,
			IntAE *out)
{
	SEXP s_ncl_elt0, s_ncl_elt1;
	const int *slide;
        int slide_len, n, i;

	if (s_ncl == R_NilValue)
		return;
	s_ncl_elt0 = VECTOR_ELT(s_ncl, 0); // integer vector
	s_ncl_elt1 = VECTOR_ELT(s_ncl, 1); // list
	slide = INTEGER(s_ncl_elt0);
	slide_len = LENGTH(s_ncl_elt0);
	for (n = bsearch_n1(q_start, slide, slide_len, s_end);
	     n < slide_len;
	     n++)
	{
		i = slide[n];
		if (s_start[i - 1] > q_end)
			break;
		IntAE_insert_at(out, IntAE_get_nelt(out), i);
		overlap_NCL(q_start, q_end,
			    VECTOR_ELT(s_ncl_elt1, n), s_start, s_end,
			    out);
	}
	IntAE_qsort(out, 0);
	return;
}

/* --- .Call ENTRY POINT ---
 * Usage:
 *   query <- IRanges(c(5, 1), c(7, 4))
 *   .Call("overlaps_NCL", start(query), end(query),
 *                         ncl, start(x), end(x), PACKAGE="IRanges")
 */
SEXP overlaps_NCL(SEXP q_start, SEXP q_end,
		 SEXP s_ncl, SEXP s_start, SEXP s_end)
{
	int q_len, i;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	IntAEAE ans_buf;

	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(query)", "end(query)");
	check_integer_pairs(s_start, s_end,
			    &s_start_p, &s_end_p,
			    "start(subject)", "end(subject)");
	ans_buf = new_IntAEAE(q_len, q_len);
	for (i = 0; i < q_len; i++) {
		overlap_NCL(q_start_p[i], q_end_p[i],
			    s_ncl, s_start_p, s_end_p,
			    ans_buf.elts + i);
	}
	return new_LIST_from_IntAEAE(&ans_buf, 1);
}

