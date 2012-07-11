// Code based on timing enhancements by Charles C. Berry <ccberry@ucsd.edu>

#include "IRanges.h"
#include <stdlib.h> /* for qsort() */
#include <R_ext/Utils.h> /* for R_CheckUserInterrupt() */

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
	int max_nrun = 0;
	int *values_buf, *lengths_buf;

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

	if (max_nrun == 0) {
		//return an Rle with one run of 0's
		int zero = 0;
		return _integer_Rle_constructor(&zero, 1, INTEGER(width), 1);
	}
	return _integer_Rle_constructor(values_buf, max_nrun, lengths_buf, 1);
}

static SEXP IRanges_coverage_hash(SEXP x, SEXP weight, SEXP width)
{
	int x_length = _get_IRanges_length(x);
	const int *x_start = INTEGER(_get_IRanges_start(x));
	const int *x_width = INTEGER(_get_IRanges_width(x));

	int weight_length = LENGTH(weight);
	const int *weight_ptr = INTEGER(weight);

	int width_int = INTEGER(width)[0];

	if (x_length == 0) {
		//return an Rle with one run of 0's
		int zero = 0;
		return _integer_Rle_constructor(&zero, 1, INTEGER(width), 1);
	}
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
	return _integer_Rle_constructor(coverage, width_int, NULL, 1);
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

