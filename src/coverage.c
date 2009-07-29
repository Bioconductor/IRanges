#include "IRanges.h"
#include <stdlib.h> /* for qsort() */

static const int *base_start;
static const int *base_width;

static int cmp_start_indices_for_ordering(const void *p1, const void *p2)
{
	int i1, i2, ret1, ret2;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret1 = *(base_start + i1) - *(base_start + i2);
	ret2 = *(base_width + i1) - *(base_width + i2);
	return ret1 != 0 ? ret1 : ret2;
}


/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRanges_coverage(SEXP x, SEXP weight, SEXP width)
{
	int i, j;
	SEXP ans, ans_lengths, ans_values;

	int x_length = _get_IRanges_length(x);
	const int *x_start = INTEGER(_get_IRanges_start(x));
	const int *x_width = INTEGER(_get_IRanges_width(x));

	// order start values
	const int *wd, *wt;
	int order_length = 0;
	int *order = (int *) R_alloc((long) x_length, sizeof(int));
	memset(order, -1, x_length * sizeof(int));
	if (LENGTH(weight) == 1) {
		for (i = 0, wd = x_width; i < x_length; i++, wd++) {
			if (*wd > 0) {
				order[order_length] = i;
				order_length++;
			}
		}
	} else {
		for (i = 0, wd = x_width, wt = INTEGER(weight); i < x_length;
		     i++, wd++, wt++) {
			if (*wd > 0 && *wt != 0) {
				order[order_length] = i;
				order_length++;
			}
		}
	}

	const int *order_elt;
	int weight_elt, index_start, index_end;

	int *sparse_data;
	int *sparse_index;
	int sparse_data_length = 0;
	if (order_length > 0) {
		base_start = x_start;
		base_width = x_width;
		qsort(order, order_length, sizeof(int), cmp_start_indices_for_ordering);

		int prev_index = 0;
		const int *x_start = INTEGER(_get_IRanges_start(x));
		const int *x_width = INTEGER(_get_IRanges_width(x));
		for (i = 0, order_elt = order; i < x_length; i++, order_elt++)
		{
			if (x_width[*order_elt] == 0)
				continue;
			if (*order_elt >= LENGTH(weight)) {
				weight_elt = INTEGER(weight)[0];
			} else {
				weight_elt = INTEGER(weight)[*order_elt];
			}
			if (weight_elt == 0)
				continue;
			index_start = (x_start[*order_elt] > prev_index ? x_start[*order_elt] : prev_index);
			index_end = x_start[*order_elt] + x_width[*order_elt] - 1;
			int shift = index_end - index_start + 1;
			if (shift > 0) {
				sparse_data_length += shift;
				prev_index = index_end + 1;
			}
		}
	}

	// perform coverage calculation
	int values_length = 0;
	int *prev_sdata, *curr_sdata, *prev_sindex, *curr_sindex;
	if (sparse_data_length > 0) {
		sparse_data = (int *) R_alloc((long) sparse_data_length, sizeof(int));
		sparse_index = (int *) R_alloc((long) sparse_data_length, sizeof(int));
		memset(sparse_data, 0, sparse_data_length * sizeof(int));
		memset(sparse_index, 0, sparse_data_length * sizeof(int));

		int *sparse_data_elt = sparse_data;
		int *sparse_index_elt = sparse_index;
		for (i = 0, order_elt = order; i < x_length; i++, order_elt++)
		{
			if (x_width[*order_elt] == 0)
				continue;
			if (*order_elt >= LENGTH(weight)) {
				weight_elt = INTEGER(weight)[0];
			} else {
				weight_elt = INTEGER(weight)[*order_elt];
			}
			if (weight_elt == 0)
				continue;
			index_start = x_start[*order_elt];
			while (*sparse_index_elt > index_start) {
				sparse_index_elt--;
				sparse_data_elt--;
			}
			while ((*sparse_index_elt > 0) && (*sparse_index_elt < index_start)) {
				sparse_index_elt++;
				sparse_data_elt++;
			}
			for (j = 0; j < x_width[*order_elt]; j++, sparse_index_elt++, sparse_data_elt++, index_start++)
			{
				*sparse_index_elt = index_start;
				*sparse_data_elt += weight_elt;
			}
			sparse_index_elt--;
			sparse_data_elt--;
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
	SET_SLOT(ans, install("lengths"), ans_lengths);
	SET_SLOT(ans, install("values"), ans_values);
	UNPROTECT(3);
	return ans;
}
