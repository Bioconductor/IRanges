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

static SEXP IRanges_coverage_sort(const int *x_start, const int *x_width,
		int x_length, const int *weight, int weight_length,
		int ans_length)
{
	int i;
	int max_nrun = 0;
	int *values_buf, *lengths_buf;

	// use i / 2 and  i % 2 to find start, width
	int *order = (int *) R_alloc((long) 2 * x_length, sizeof(int));
	memset(order, -1, 2 * x_length * sizeof(int));

	int order_length = 0;
	int *order_elt;
	const int *wd, *wt;
	wt = weight;
	order_elt = order;
	for (i = 0, wd = x_width; i < x_length; i++, wd++) {
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
		if (weight_length != 1)
			wt++;
	}
	if (order_length == 0) {
		//return an Rle with one run of 0's
		int zero = 0;
		return _integer_Rle_constructor(&zero, 1, &ans_length, 0);
	}

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
							    -weight[0]);
			} else {
				curr_pos = x_start[index];
				curr_weight = _safe_int_add(curr_weight,
							    weight[0]);
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
							    -weight[index]);
			} else {
				curr_pos = x_start[index];
				curr_weight = _safe_int_add(curr_weight,
							    weight[index]);
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
	curr_pos = ans_length + 1;
	if (curr_pos != prev_pos) {
		lengths_buf[max_nrun] = curr_pos - prev_pos;
		values_buf[max_nrun] = 0;
		max_nrun++;
	}
	return _integer_Rle_constructor(values_buf, max_nrun, lengths_buf, 0);
}

static SEXP IRanges_coverage_hash(
		const int *x_start, const int *x_width, int x_length,
		const int *weight, int weight_length,
		int ans_length)
{
	int *cvg_buf, i, cvg_offset, *cvg_p, j, buflength;

	cvg_buf = (int *) R_alloc((long) ans_length, sizeof(int));
	memset(cvg_buf, 0, ans_length * sizeof(int));
	_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
	for (i = 0; i < x_length; i++, x_start++, x_width++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		cvg_offset = *x_start - 1;
		cvg_p = cvg_buf + cvg_offset;
		for (j = 0; j < *x_width; j++, cvg_offset++, cvg_p++)
		{
			if (cvg_offset >= ans_length)
				continue;
			*cvg_p = _safe_int_add(*cvg_p, *weight);
		}
		if (weight_length != 1)
			weight++;
	}
	if (_get_ovflow_flag())
		warning("NAs produced by integer overflow");
	/* the nb of runs must be <= 2 * length(x) + 1 */
	buflength = 2 * x_length + 1;
	return _integer_Rle_constructor(cvg_buf, ans_length, NULL, buflength);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRanges_coverage(SEXP x, SEXP weight, SEXP width, SEXP method)
{
	int x_length, width0, weight_length, zero;
	const int *weight_p, *x_start_p, *x_width_p;
	const char *method_ptr = CHAR(STRING_ELT(method, 0));

	x_length = _get_IRanges_length(x);
	width0 = INTEGER(width)[0];
	weight_length = LENGTH(weight);
	weight_p = INTEGER(weight);
	if (x_length == 0 || (weight_length == 1 && weight_p[0] == 0)) {
		//return an Rle with one run of 0's
		zero = 0;
		return _integer_Rle_constructor(&zero, 1, &width0, 0);
	}
	x_start_p = INTEGER(_get_IRanges_start(x));
	x_width_p = INTEGER(_get_IRanges_width(x));
	if (strcmp(method_ptr, "sort") == 0)
		return IRanges_coverage_sort(x_start_p, x_width_p, x_length,
					     weight_p, weight_length, width0);
	return IRanges_coverage_hash(x_start_p, x_width_p, x_length,
				     weight_p, weight_length, width0);
}

