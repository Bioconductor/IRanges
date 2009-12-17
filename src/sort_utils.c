/****************************************************************************
 * Low-level sorting utilities                                              *
 * ---------------------------                                              *
 *                                                                          *
 * All sortings/orderings are based on the qsort() function from the        *
 * standard C lib.                                                          *
 * Note that C qsort() is NOT "stable" so the ordering functions below      *
 * (_get_order_of_*() functions) need to ultimately break ties by position  *
 * (this is done by adding a little extra code at the end of the comparison *
 * function used in the call to qsort()).                                   *
 ****************************************************************************/
#include "IRanges.h"
#include <stdlib.h> /* for qsort() */


static const int *xx, *yy;


/****************************************************************************
 * Sorting or getting the order of an int array.
 */

static int compar_ints_for_asc_sort(const void *p1, const void *p2)
{
	return *((const int *) p1) - *((const int *) p2);
}

static int compar_ints_for_desc_sort(const void *p1, const void *p2)
{
	return compar_ints_for_asc_sort(p2, p1);
}

void _sort_int_array(int *x, int nelt, int desc)
{
	int (*compar)(const void *, const void *);

	compar = desc ? compar_ints_for_desc_sort : compar_ints_for_asc_sort;
	qsort(x, nelt, sizeof(int), compar);
	return;
}

static int compar_xx_for_asc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = xx[i1] - xx[i2];
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/* We cannot just define compar_xx_for_desc_order(p1, p2) to be
 * compar_xx_for_asc_order(p2, p1) because of the tie-break by position. */
static int compar_xx_for_desc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = xx[i2] - xx[i1];
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

void _get_order_of_int_array(const int *x, int nelt, int desc,
		int *out, int out_shift)
{
	int i, (*compar)(const void *, const void *);

	xx = x - out_shift;
	for (i = 0; i < nelt; i++)
		out[i] = i + out_shift;
	compar = desc ? compar_xx_for_desc_order : compar_xx_for_asc_order;
	qsort(out, nelt, sizeof(int), compar);
	return;
}


/****************************************************************************
 * Getting the order of 2 int arrays of the same length.
 * The second array ('y') is used to break ties in the first array ('x').
 */

static int compar_xxyy_for_asc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = xx[i1] - xx[i2];
	if (ret != 0)
		return ret;
	ret = yy[i1] - yy[i2];
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/* We cannot just define compar_xxyy_for_desc_order(p1, p2) to be
 * compar_xxyy_for_asc_order(p2, p1) because of the tie-break by position. */
static int compar_xxyy_for_desc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = xx[i2] - xx[i1];
	if (ret != 0)
		return ret;
	ret = yy[i2] - yy[i1];
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

void _get_order_of_two_int_arrays(const int *x, const int *y, int nelt,
		int desc, int *out, int out_shift)
{
	int i, (*compar)(const void *, const void *);

	xx = x - out_shift;
	yy = y - out_shift;
	for (i = 0; i < nelt; i++)
		out[i] = i + out_shift;
	compar = desc ? compar_xxyy_for_desc_order : compar_xxyy_for_asc_order;
	qsort(out, nelt, sizeof(int), compar);
	return;
}

