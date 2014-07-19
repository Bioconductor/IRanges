#include "IRanges.h"
#include "S4Vectors_interface.h"
#include <stdlib.h> /* for qsort() */
#include <stdlib.h>  /* for malloc, free, realloc */

static const int *aa, *bb;

static int compar_int_pairs(const void *p1, const void *p2)
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
	qsort(oo, x_len, sizeof(int), compar_int_pairs);

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

