#include "IRanges.h"
#include <stdlib.h> /* for qsort() */

static const int *base_start;
static const int *base_width;

static int cmp_sw_indices_for_ordering(const void *p1, const void *p2)
{
	int i1, i2, ret1, ret2;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret1 = *(base_start + i1) - *(base_start + i2);
	ret2 = *(base_width + i1) - *(base_width + i2);
	return ret1 != 0 ? ret1 : ret2;
}

/****************************************************************************
 * Order Ranges
 */

void _get_Ranges_order(const int *start, const int *width, int nelt, int *order, int base1)
{
	int i, *ord;

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
	qsort(order, nelt, sizeof(int), cmp_sw_indices_for_ordering);
	return;
}


/* --- .Call ENTRY POINT --- */
SEXP Ranges_order(SEXP start, SEXP width)
{
	int ans_length;
	SEXP ans;

	ans_length = LENGTH(start);
	PROTECT(ans = NEW_INTEGER(ans_length));
	_get_Ranges_order(INTEGER(start), INTEGER(width), ans_length, INTEGER(ans), 1);
	UNPROTECT(1);
	return ans;
}
