/*****************************************************************************
 * Low-level sorting utilities
 * ---------------------------
 */
#include "IRanges.h"
#include <stdlib.h> /* for qsort() */

/*
 * Sort an array of ints.
 */
static int cmpintp(const void *p1, const void *p2)
{
	return *((const int *) p1) - *((const int *) p2);
}

void _sort_int_array(int *x, int x_nelt)
{
	qsort(x, x_nelt, sizeof(int), cmpintp);
}

/*
 * Get the order of an array of ints.
 */
static int cmpintpp(const void *p1, const void *p2)
{
	return cmpintp(*((const int **) p1), *((const int **) p2));
}

void _get_int_array_order(const int *x, int x_nelt, int *order)
{
	const int **x_p, *tmp0, **tmp1;
	int k, *tmp2;

	x_p = (const int **) malloc(x_nelt * sizeof(const int *));
	if (x_p == NULL)
		error("IRanges internal error in _get_int_array_order(): malloc failed");
	for (k = 0, tmp0 = x, tmp1 = x_p; k < x_nelt; k++, tmp0++, tmp1++)
		*tmp1 = tmp0;
	qsort(x_p, x_nelt, sizeof(int *), cmpintpp);
	for (k = 0, tmp1 = x_p, tmp2 = order; k < x_nelt; k++, tmp1++, tmp2++)
		*tmp2 = *tmp1 - x;
	free(x_p);
	return;
}

