#include "IRanges.h"
#include <stdlib.h> /* for qsort() */

static const int *base_start;

static int cmp_start_indices_for_ordering(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = *(base_start + i1) - *(base_start + i2);
	return ret != 0 ? ret : i1 - i2;
}


/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRanges_coverage(SEXP x, SEXP weight, SEXP width)
{
	int i, j, weight_elt;
	int index_start, index_end, index_width;
	const int *index_elt;
	SEXP ans, ans_lengths, ans_values;

	int x_length = _get_IRanges_length(x);
	const int *x_start = INTEGER(_get_IRanges_start(x));
	const int *x_width = INTEGER(_get_IRanges_width(x));

	// order start values
	const int *wd, *wt;
	int order_length = 0;
	int *order_start = (int *) R_alloc((long) x_length, sizeof(int));
	memset(order_start, -1, x_length * sizeof(int));
	if (LENGTH(weight) == 1) {
		for (i = 0, wd = x_width; i < x_length; i++, wd++) {
			if (*wd > 0) {
				order_start[order_length] = i;
				order_length++;
			}
		}
	} else {
		for (i = 0, wd = x_width, wt = INTEGER(weight); i < x_length;
		     i++, wd++, wt++) {
			if (*wd > 0 && *wt != 0) {
				order_start[order_length] = i;
				order_length++;
			}
		}
	}
	base_start = x_start;
	qsort(order_start, order_length, sizeof(int), cmp_start_indices_for_ordering);

	// find buffer size
	int prev_index = 0;
	int sparse_data_length = 0;
	for (i = 0, index_elt = order_start; i < order_length; i++, index_elt++)
	{
		int curr_start = x_start[*index_elt];
		int curr_width = x_width[*index_elt];
		index_start = curr_start > prev_index ? curr_start : prev_index;
		index_end = curr_start + curr_width - 1;
		int shift = index_end - index_start + 1;
		if (shift > 0) {
			sparse_data_length += shift;
			prev_index = index_end + 1;
		}
	}
	int *sparse_data = (int *) R_alloc((long) sparse_data_length, sizeof(int));
	int *sparse_index = (int *) R_alloc((long) sparse_data_length, sizeof(int));
	memset(sparse_data, 0, sparse_data_length * sizeof(int));
	memset(sparse_index, 0, sparse_data_length * sizeof(int));

	// perform coverage calculation
	int values_length = 0, prev_width = 0;
	int *prev_sdata, *curr_sdata, *prev_sindex, *curr_sindex;
	if (sparse_data_length > 0) {
		int *sparse_data_elt = sparse_data;
		int *sparse_index_elt = sparse_index;
		for (i = 0, index_elt = order_start; i < order_length; i++, index_elt++)
		{
			if (*index_elt >= LENGTH(weight)) {
				weight_elt = INTEGER(weight)[0];
			} else {
				weight_elt = INTEGER(weight)[*index_elt];
			}
			index_start = x_start[*index_elt];
			index_width = x_width[*index_elt];
			if (*sparse_index_elt < index_start) {
				int ptr_shift = index_start - *sparse_index_elt;
				ptr_shift = ptr_shift < prev_width ? ptr_shift : prev_width;
				if (ptr_shift > 0) {
					sparse_index_elt += ptr_shift;
					sparse_data_elt += ptr_shift;
				}
			}
			for (j = 0; j < index_width; j++, sparse_index_elt++, sparse_data_elt++, index_start++)
			{
				*sparse_index_elt = index_start;
				*sparse_data_elt += weight_elt;
			}
			sparse_index_elt -= index_width;
			sparse_data_elt -= index_width;
			prev_width = index_width;
		}
		values_length = 1 + (*sparse_index != 1);
		for (i = 1, prev_sdata = sparse_data, curr_sdata = (sparse_data + 1),
			 prev_sindex = sparse_index, curr_sindex = (sparse_index + 1);
		     i < sparse_data_length;
		     i++, prev_sdata++, curr_sdata++, prev_sindex++, curr_sindex++)
		{
			if ((*prev_sindex + 1) != *curr_sindex)
				values_length += 2;
			else if (*prev_sdata != *curr_sdata)
				values_length++;
		}
		values_length += (sparse_index[sparse_data_length - 1] != INTEGER(width)[0]);
	}

	// create output object
	if (values_length == 0) {
		PROTECT(ans_lengths = NEW_INTEGER(1));
		PROTECT(ans_values = NEW_INTEGER(1));
		INTEGER(ans_values)[0] = 0;
		INTEGER(ans_lengths)[0] = INTEGER(width)[0];
	} else {
		PROTECT(ans_lengths = NEW_INTEGER(values_length));
		PROTECT(ans_values = NEW_INTEGER(values_length));
		int *ans_lengths_ptr = INTEGER(ans_lengths);
		int *ans_values_ptr = INTEGER(ans_values);
		memset(ans_lengths_ptr, 0, values_length * sizeof(int));
		memset(ans_values_ptr, 0, values_length * sizeof(int));

		if (*sparse_index != 1) {
			*ans_values_ptr = 0;
			*ans_lengths_ptr = *sparse_index - 1;
			ans_values_ptr++;
			ans_lengths_ptr++;
		}
		*ans_values_ptr = *sparse_data;
		*ans_lengths_ptr = 1;
		for (i = 1, prev_sdata = sparse_data, curr_sdata = (sparse_data + 1),
			 prev_sindex = sparse_index, curr_sindex = (sparse_index + 1);
		     i < sparse_data_length;
		     i++, prev_sdata++, curr_sdata++, prev_sindex++, curr_sindex++)
		{
			if ((*prev_sindex + 1) != *curr_sindex) {
				ans_values_ptr++;
				ans_lengths_ptr++;
				*ans_values_ptr = 0;
				*ans_lengths_ptr = *curr_sindex - *prev_sindex - 1;

				ans_values_ptr++;
				ans_lengths_ptr++;
				*ans_values_ptr = *curr_sdata;
				*ans_lengths_ptr = 1;
			} else if (*prev_sdata != *curr_sdata) {
				ans_values_ptr++;
				ans_lengths_ptr++;
				*ans_values_ptr = *curr_sdata;
				*ans_lengths_ptr = 1;
			} else {
				*ans_lengths_ptr += 1;
			}
		}
		if (sparse_index[sparse_data_length - 1] != INTEGER(width)[0]) {
			ans_values_ptr++;
			ans_lengths_ptr++;
			*ans_values_ptr = 0;
			*ans_lengths_ptr = INTEGER(width)[0] - sparse_index[sparse_data_length - 1];
		}
	}

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, mkChar("vectorLength"), ScalarInteger(INTEGER(width)[0]));
	SET_SLOT(ans, mkChar("lengths"), ans_lengths);
	SET_SLOT(ans, mkChar("values"), ans_values);
	UNPROTECT(3);
	return ans;
}
