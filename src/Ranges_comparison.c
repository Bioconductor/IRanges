#include "IRanges.h"
#include <stdlib.h> /* for qsort() */

static int cmp_startwidth(const int *start, const int *width, int i1, int i2)
{
	int ret;

	ret = start[i1] - start[i2];
	if (ret != 0)
		return ret;
	ret = width[i1] - width[i2];
	return ret;
}

static const int *base_start;
static const int *base_width;

/* Passed to qsort() */
static int cmp_startwidth_indices_for_ordering(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = cmp_startwidth(base_start, base_width, i1, i2);
	if (ret != 0)
		return ret;
	// How qsort() will break ties is undefined (the Quicksort algo is
	// not "stable"). By returning i1 - i2 in case of a tie, we ensure that
	// the _get_Ranges_order() function below is "stable" and returns
	// the same thing on all platforms. In addition, this coincides with
	// what the order() function does in R.
	return i1 - i2;
}

/* Passed to qsort() */
static int cmp_startwidth_indices_for_descordering(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = cmp_startwidth(base_start, base_width, i1, i2);
	if (ret != 0)
		return -ret;
	return i1 - i2;
}


/****************************************************************************
 * Order Ranges
 */

void _get_Ranges_order(int *order, int nelt,
		const int *start, const int *width, int decreasing, int base1)
{
	int i, *ord, (*compar)(const void *, const void *);

	if (base1 == 0) {
		base_start = start;
		base_width = width;
		for (i = 0, ord = order; i < nelt; i++, ord++)
			*ord = i;
	} else {
		base_start = start - 1; // because we will sort 1-based indices
		base_width = width - 1; // because we will sort 1-based indices
		for (i = 0, ord = order; i < nelt; i++, ord++)
			*ord = i + 1; // 1-based indices
	}
	compar = decreasing ? cmp_startwidth_indices_for_descordering
			    : cmp_startwidth_indices_for_ordering;
	qsort(order, nelt, sizeof(int), compar);
	return;
}


/* --- .Call ENTRY POINT --- */
SEXP Ranges_order(SEXP start, SEXP width, SEXP decreasing)
{
	int ans_length;
	SEXP ans;

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	_get_Ranges_order(INTEGER(ans), ans_length,
		INTEGER(start), INTEGER(width), LOGICAL(decreasing)[0], 1);
	UNPROTECT(1);
	return ans;
}
