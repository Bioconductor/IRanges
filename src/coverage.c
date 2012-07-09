// Code based on timing enhancements by Charles C. Berry <ccberry@ucsd.edu>

#include "IRanges.h"
#include <stdlib.h> /* for qsort() */
#include <R_ext/Utils.h> /* for R_CheckUserInterrupt() */

/* TODO: Move this to Rle_class.c */
static SEXP _new_Rle(SEXP values, SEXP lengths)
{
	SEXP ans;

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), values);
	SET_SLOT(ans, install("lengths"), lengths);
	UNPROTECT(1);
	return ans;
}

static const int *base_start;
static const int *base_width;

static int cmp_sw_subset_for_ordering(const void *p1, const void *p2)
{
	int i1, i2;
	int s1, s2;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	// if i is index for width, then s = end + 1
	// if i is index for start, then s = start
	s1 = (i1 % 2) ? *(base_start + i1 / 2) + *(base_width + i1 / 2) : *(base_start + i1 / 2);
	s2 = (i2 % 2) ? *(base_start + i2 / 2) + *(base_width + i2 / 2) : *(base_start + i2 / 2);
	return s1 - s2;

}

static SEXP IRanges_coverage_sort(SEXP x, SEXP weight, SEXP width)
{
	int i;
	int ans_nrun = 0;
	int max_nrun = 0;
	int *values_buf, *lengths_buf;
	SEXP ans, ans_lengths, ans_values;

	int x_length = _get_IRanges_length(x);
	const int *x_start = INTEGER(_get_IRanges_start(x));
	const int *x_width = INTEGER(_get_IRanges_width(x));

	int weight_length = LENGTH(weight);
	const int *weight_ptr = INTEGER(weight);

	// use i / 2 and  i % 2 to find start, width
	int *order = (int *) R_alloc((long) 2 * x_length, sizeof(int));
	memset(order, -1, 2 * x_length * sizeof(int));

	int order_length = 0;
	int *order_elt;
	const int *wd, *wt;
	order_elt = order;
	if (weight_length == 1) {
		for (i = 0, wd = x_width; i < x_length; i++, wd++) {
			if (*wd > 0) {
				// start order
				*order_elt = 2 * i;
				order_elt++;
				order_length++;
				// width order
				*order_elt = 2 * i + 1;
				order_elt++;
				order_length++;
			}
		}
	} else {
		for (i = 0, wd = x_width, wt = weight_ptr; i < x_length;
			 i++, wd++, wt++) {
			if (*wd > 0 && *wt != 0) {
				// start order
				*order_elt = 2 * i;
				order_elt++;
				order_length++;
				// width order
				*order_elt = 2 * i + 1;
				order_elt++;
				order_length++;
			}
		}
	}

	if (order_length > 0) {
		base_start = x_start;
		base_width = x_width;
		qsort(order, order_length, sizeof(int), cmp_sw_subset_for_ordering);
		values_buf = (int *) R_alloc((long) order_length, sizeof(int));
		lengths_buf = (int *) R_alloc((long) order_length, sizeof(int));

		int index, is_end;
		int prev_pos, curr_pos, prev_weight, curr_weight;

		// pos is either a start position or an end position + 1
		prev_pos = 1;
		prev_weight = 0;
		curr_weight = 0;
		if (weight_length == 1) {
			_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
			for (i = 0, order_elt = order; i < order_length;
				 i++, order_elt++) {
				if (i % 100000 == 99999)
					R_CheckUserInterrupt();
				index = *order_elt / 2;
				is_end = *order_elt % 2;
				if (is_end) {
					curr_pos = x_start[index] + x_width[index];
					curr_weight = _safe_int_add(curr_weight,
								    -weight_ptr[0]);
				} else {
					curr_pos = x_start[index];
					curr_weight = _safe_int_add(curr_weight,
								    weight_ptr[0]);
				}
				if (curr_pos != prev_pos) {
					lengths_buf[max_nrun] = curr_pos - prev_pos;
					values_buf[max_nrun] = prev_weight;
					max_nrun++;
					prev_pos = curr_pos;
				}
				prev_weight = curr_weight;
			}
			if (_get_ovflow_flag())
				warning("NAs produced by integer overflow");
		} else {
			_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
			for (i = 0, order_elt = order; i < order_length;
				 i++, order_elt++) {
				if (i % 100000 == 99999)
					R_CheckUserInterrupt();
				index = *order_elt / 2;
				is_end = *order_elt % 2;
				if (is_end) {
					curr_pos = x_start[index] + x_width[index];
					curr_weight = _safe_int_add(curr_weight,
								    -weight_ptr[index]);
				} else {
					curr_pos = x_start[index];
					curr_weight = _safe_int_add(curr_weight,
								    weight_ptr[index]);
				}
				if (curr_pos != prev_pos) {
					lengths_buf[max_nrun] = curr_pos - prev_pos;
					values_buf[max_nrun] = prev_weight;
					max_nrun++;
					prev_pos = curr_pos;
				}
				prev_weight = curr_weight;
			}
			if (_get_ovflow_flag())
				warning("NAs produced by integer overflow");
		}
		// extend vector length if user-supplied width exceeds coverage domain
		curr_pos = INTEGER(width)[0] + 1;
		if (curr_pos != prev_pos) {
			lengths_buf[max_nrun] = curr_pos - prev_pos;
			values_buf[max_nrun] = 0;
			max_nrun++;
		}
	}

	// combine adjacent equal values
	if (max_nrun == 0) {
		ans_nrun = 0;
	} else {
		ans_nrun = 1;
		int *curr_val, *prev_val;
		for (i = 1, curr_val = (values_buf+1), prev_val = values_buf;
			 i < max_nrun; i++, curr_val++, prev_val++) {
			if (*curr_val != *prev_val)
				ans_nrun++;
		}
	}
	// create output object
	if (max_nrun == 0) {
		PROTECT(ans_lengths = NEW_INTEGER(1));
		PROTECT(ans_values = NEW_INTEGER(1));
		INTEGER(ans_values)[0] = 0;
		INTEGER(ans_lengths)[0] = INTEGER(width)[0];
	} else {
		PROTECT(ans_lengths = NEW_INTEGER(ans_nrun));
		PROTECT(ans_values = NEW_INTEGER(ans_nrun));
		int *ans_lengths_ptr = INTEGER(ans_lengths);
		int *ans_values_ptr = INTEGER(ans_values);

		if (ans_nrun == max_nrun) {
			memcpy(ans_lengths_ptr, lengths_buf, max_nrun * sizeof(int));
			memcpy(ans_values_ptr, values_buf, max_nrun * sizeof(int));
		} else {
			int *values_buf_ptr, *lengths_buf_ptr;
			*ans_lengths_ptr = *lengths_buf;
			*ans_values_ptr = *values_buf;
			for (i = 1, values_buf_ptr = (values_buf+1),
				 lengths_buf_ptr = (lengths_buf+1); i < max_nrun;
				 i++, values_buf_ptr++, lengths_buf_ptr++) {
				if (*values_buf_ptr != *ans_values_ptr) {
					ans_values_ptr++;
					ans_lengths_ptr++;
					*ans_values_ptr = *values_buf_ptr;
					*ans_lengths_ptr = *lengths_buf_ptr;
				} else {
					*ans_lengths_ptr += *lengths_buf_ptr;
				}
			}
		}
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

static SEXP IRanges_coverage_hash(SEXP x, SEXP weight, SEXP width)
{
	int i;
	int ans_nrun = 0;
	int *values_buf, *lengths_buf;
	SEXP ans, ans_lengths, ans_values;

	int x_length = _get_IRanges_length(x);
	const int *x_start = INTEGER(_get_IRanges_start(x));
	const int *x_width = INTEGER(_get_IRanges_width(x));

	int weight_length = LENGTH(weight);
	const int *weight_ptr = INTEGER(weight);

	int width_int = INTEGER(width)[0];

	if (width_int == 0) {
		//do nothing, returns an empty Rle
	} else if (x_length == 0) {
		//return an Rle with one run of 0's
		values_buf = (int *) R_alloc(1, sizeof(int));
		lengths_buf = (int *) R_alloc(1, sizeof(int));
		*values_buf = 0;
		*lengths_buf = width_int;
		ans_nrun = 1;
	} else {
		//create coverage vector
		int *coverage = (int *) R_alloc((long) width_int, sizeof(int)), *cov_ptr;
		memset(coverage, 0, width_int * sizeof(int));
		_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
		for (int i = 0; i < x_length; i++) {
			int weight_val;
			if (weight_length == 1)
				weight_val = *weight_ptr;
			else
				weight_val = weight_ptr[i];
			for (int j = 0; j < x_width[i]; j++) {
				cov_ptr = coverage + x_start[i] - 1 + j;
				*cov_ptr = _safe_int_add(*cov_ptr, weight_val);
			}
		}
		if (_get_ovflow_flag())
			warning("NAs produced by integer overflow");

		//Determine the number of runs.
		//While traversing array, buffer values to pass to Rle SEXP.
		int prev_value = coverage[0];
		int cur_run_len = 1;
		values_buf = (int *) R_alloc((long) 2 * x_length + 1, sizeof(int));
		lengths_buf = (int *) R_alloc((long) 2 * x_length + 1, sizeof(int));
		int *values_buf_ptr = values_buf;
		int *lengths_buf_ptr = lengths_buf;

		if (width_int == 1) {
			ans_nrun = 1;
			*values_buf_ptr = coverage[0];
			*lengths_buf_ptr = 1;
		}

		for (i = 1; i < width_int; i++) {
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			if (i == width_int - 1) { //handle final array element
				if (prev_value == coverage[i]) { //if a run continues to the end element
					*lengths_buf_ptr = ++cur_run_len;
					*values_buf_ptr = prev_value;
					ans_nrun++;
				} else { //if a new run starts (and stops) on the last element
					*lengths_buf_ptr = cur_run_len;
					*values_buf_ptr = prev_value;
					ans_nrun++;
					lengths_buf_ptr++;
					values_buf_ptr++;
					*lengths_buf_ptr = 1;
					*values_buf_ptr = coverage[i];
					ans_nrun++;
				}
			} else if (prev_value == coverage[i]) {
				cur_run_len++;
			} else {
				*lengths_buf_ptr = cur_run_len;
				*values_buf_ptr = prev_value;
				ans_nrun++;
				lengths_buf_ptr++;
				values_buf_ptr++;
				prev_value = coverage[i];
				cur_run_len = 1;
			}
		}
	}
	PROTECT(ans_lengths = NEW_INTEGER(ans_nrun));
	PROTECT(ans_values = NEW_INTEGER(ans_nrun));
	int *ans_lengths_ptr = INTEGER(ans_lengths);
	int *ans_values_ptr = INTEGER(ans_values);
	memcpy(ans_lengths_ptr, lengths_buf, ans_nrun * sizeof(int));
	memcpy(ans_values_ptr, values_buf, ans_nrun * sizeof(int));
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRanges_coverage(SEXP x, SEXP weight, SEXP width, SEXP method)
{
	const char *method_ptr = CHAR(STRING_ELT(method, 0));

	if (strcmp(method_ptr, "sort") == 0)
		return IRanges_coverage_sort(x, weight, width);
	return IRanges_coverage_hash(x, weight, width);
}

