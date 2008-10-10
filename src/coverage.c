#include "IRanges.h"

SEXP IRanges_coverage(SEXP x, SEXP weight, SEXP order, SEXP width)
{
	int i, j, weight_elt;
	int index, index_start, index_end;
	SEXP ans, ans_values, ans_lengths, ans_vectorLength;
	SEXP ans_values_xdata, ans_values_offset, ans_values_length;
	SEXP ans_lengths_xdata, ans_lengths_offset, ans_lengths_length;

	int prev_index = 0;
	int sparse_data_length = 0;
	int x_length = _get_IRanges_length(x);
	const int *order_elt;
	const int *x_start = _get_IRanges_start0(x);
	const int *x_width = _get_IRanges_width0(x);
	for (i = 0, order_elt = INTEGER(order); i < x_length; i++, order_elt++)
	{
		index = *order_elt - 1;
		if (index >= LENGTH(weight)) {
			weight_elt = INTEGER(weight)[0];
		} else {
			weight_elt = INTEGER(weight)[index];
		}
		if (weight_elt != 0) {
			index_start = (x_start[index] > prev_index ? x_start[index] : prev_index);
			index_end = x_start[index] + x_width[index] - 1;
			int shift = index_end - index_start + 1;
			if (shift > 0) {
				sparse_data_length += shift;
				prev_index = index_end + 1;
			}
		}
	}
	int *sparse_data = (int *) R_alloc((long) sparse_data_length, sizeof(int));
	int *sparse_index = (int *) R_alloc((long) sparse_data_length, sizeof(int));
	memset(sparse_data, 0, sparse_data_length * sizeof(int));
	memset(sparse_index, 0, sparse_data_length * sizeof(int));

	int values_length = 0;
	int *prev_sdata, *curr_sdata, *prev_sindex, *curr_sindex;
	if (sparse_data_length > 0) {
		int *sparse_data_elt = sparse_data;
		int *sparse_index_elt = sparse_index;
		for (i = 0, order_elt = INTEGER(order); i < x_length; i++, order_elt++)
		{
			index = *order_elt - 1;
			if (index >= LENGTH(weight)) {
				weight_elt = INTEGER(weight)[0];
			} else {
				weight_elt = INTEGER(weight)[index];
			}
			if (weight_elt != 0) {
				index_start = x_start[index];
				while (*sparse_index_elt > index_start) {
					sparse_index_elt--;
					sparse_data_elt--;
				}
				while ((*sparse_index_elt > 0) && (*sparse_index_elt < index_start)) {
					sparse_index_elt++;
					sparse_data_elt++;
				}
				for (j = 0; j < x_width[index]; j++, sparse_index_elt++, sparse_data_elt++, index_start++)
				{
					*sparse_index_elt = index_start;
					*sparse_data_elt += weight_elt;
				}
				sparse_index_elt--;
				sparse_data_elt--;
			}
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

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("XRleInteger")));
	PROTECT(ans_vectorLength = NEW_INTEGER(1));
	INTEGER(ans_vectorLength)[0] = INTEGER(width)[0];

	PROTECT(ans_values = NEW_OBJECT(MAKE_CLASS("XInteger")));
	PROTECT(ans_values_length = NEW_INTEGER(1));
	INTEGER(ans_values_length)[0] = values_length;
	PROTECT(ans_values_xdata = IntegerPtr_new(ans_values_length, R_NilValue));
	PROTECT(ans_values_offset = NEW_INTEGER(1));
	INTEGER(ans_values_offset)[0] = 0;
	SET_SLOT(ans_values, mkChar("xdata"), ans_values_xdata);
	SET_SLOT(ans_values, mkChar("offset"), ans_values_offset);
	SET_SLOT(ans_values, mkChar("length"), ans_values_length);

	PROTECT(ans_lengths = NEW_OBJECT(MAKE_CLASS("XInteger")));
	PROTECT(ans_lengths_length = NEW_INTEGER(1));
	INTEGER(ans_lengths_length)[0] = values_length;
	PROTECT(ans_lengths_xdata = IntegerPtr_new(ans_lengths_length, R_NilValue));
	PROTECT(ans_lengths_offset = NEW_INTEGER(1));
	INTEGER(ans_lengths_offset)[0] = 0;
	SET_SLOT(ans_lengths, mkChar("xdata"), ans_lengths_xdata);
	SET_SLOT(ans_lengths, mkChar("offset"), ans_lengths_offset);
	SET_SLOT(ans_lengths, mkChar("length"), ans_lengths_length);

    SET_SLOT(ans, mkChar("values"), ans_values);
    SET_SLOT(ans, mkChar("lengths"), ans_lengths);
    SET_SLOT(ans, mkChar("vectorLength"), ans_vectorLength);

	int *values_tag = INTEGER(_get_SequencePtr_tag(GET_SLOT(ans_values, install("xdata"))));
	int *lengths_tag = INTEGER(_get_SequencePtr_tag(GET_SLOT(ans_lengths, install("xdata"))));
	memset(values_tag, 0, values_length * sizeof(int));
	memset(lengths_tag, 0, values_length * sizeof(int));

	if (values_length == 0) {
		*values_tag = 0;
		*lengths_tag = INTEGER(width)[0];
	} else {
		if (*sparse_index != 1) {
			*values_tag = 0;
			*lengths_tag = *sparse_index - 1;
			values_tag++;
			lengths_tag++;
		}
		*values_tag = *sparse_data;
		*lengths_tag = 1;
		for (i = 1, prev_sdata = sparse_data, curr_sdata = (sparse_data + 1),
			 prev_sindex = sparse_index, curr_sindex = (sparse_index + 1);
		     i < sparse_data_length;
		     i++, prev_sdata++, curr_sdata++, prev_sindex++, curr_sindex++)
		{
			if ((*prev_sindex + 1) != *curr_sindex) {
				values_tag++;
				lengths_tag++;
				*values_tag = 0;
				*lengths_tag = *curr_sindex - *prev_sindex - 1;

				values_tag++;
				lengths_tag++;
				*values_tag = *curr_sdata;
				*lengths_tag = 1;
			} else if (*prev_sdata != *curr_sdata) {
				values_tag++;
				lengths_tag++;
				*values_tag = *curr_sdata;
				*lengths_tag = 1;
			} else {
				*lengths_tag += 1;
			}
		}
		if (sparse_index[sparse_data_length - 1] != INTEGER(width)[0]) {
			values_tag++;
			lengths_tag++;
			*values_tag = 0;
			*lengths_tag = INTEGER(width)[0] - sparse_index[sparse_data_length - 1];
		}
	}
	UNPROTECT(10);

	return ans;
}
