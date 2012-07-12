// Code based on timing enhancements by Charles C. Berry <ccberry@ucsd.edu>

#include "IRanges.h"
#include <stdlib.h> /* for qsort() */
#include <R_ext/Utils.h> /* for R_CheckUserInterrupt() */

static const int *base_start;
static const int *base_width;

static int compar_SEids_for_asc_order(const void *p1, const void *p2)
{
	int SEid1, SEid2, index1, index2, s1, s2;

	SEid1 = *((const int *) p1);
	SEid2 = *((const int *) p2);
	index1 = SEid1 / 2;
	index2 = SEid2 / 2;
	/* If SEid is a Start id, then s = start
	   If SEid is an End id, then s = end + 1 */
	s1 = base_start[index1];
	if (SEid1 % 2)
		s1 += base_width[index1];
	s2 = base_start[index2];
	if (SEid2 % 2)
		s2 += base_width[index2];
	return s1 - s2;
}

/* Initialize the buffer of Start/End ids. */
static int init_SEids(int *SEids, const int *x_width, int x_length,
		const int *weight, int weight_length)
{
	int SEids_length, index, SEid;

	SEids_length = 0;
	for (index = 0; index < x_length; index++, x_width++) {
		if (*x_width > 0 && *weight != 0) {
			SEid = 2 * index;
			*(SEids++) = SEid; /* Start id */
			*(SEids++) = SEid + 1; /* End id */
			SEids_length += 2;
		}
		if (weight_length != 1)
			weight++;
	}
	return SEids_length;
}

/* Sort the buffer of Start/End ids. */
static void sort_SEids(int *SEids, int SEids_length,
		const int *x_start, const int *x_width)
{
	base_start = x_start;
	base_width = x_width;
	qsort(SEids, SEids_length, sizeof(int), compar_SEids_for_asc_order);
	return;
}

static SEXP IRanges_coverage_sort(const int *x_start, const int *x_width,
		int x_length, const int *weight, int weight_length,
		int ans_length)
{
	int *SEids, SEids_length, zero,
	    *values_buf, *lengths_buf,
	    max_nrun, prev_pos, curr_pos, prev_value, curr_value, curr_weight,
	    i, index, is_end;
	const int *SEids_elt;

	// use SEid / 2 and SEid % 2 to find start, width
	SEids = (int *) R_alloc((long) 2 * x_length, sizeof(int));
	SEids_length = init_SEids(SEids, x_width, x_length,
				  weight, weight_length);
	if (SEids_length == 0) {
		//return an Rle with one run of 0's
		zero = 0;
		return _integer_Rle_constructor(&zero, 1, &ans_length, 0);
	}
	sort_SEids(SEids, SEids_length, x_start, x_width);

	values_buf = (int *) R_alloc((long) SEids_length, sizeof(int));
	lengths_buf = (int *) R_alloc((long) SEids_length, sizeof(int));

	// pos is either a start position or an end position + 1
	max_nrun = 0;
	prev_pos = 1;
	prev_value = 0;
	curr_value = 0;
	curr_weight = weight[0];
	_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
	for (i = 0, SEids_elt = SEids; i < SEids_length; i++, SEids_elt++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		index = *SEids_elt / 2;
		is_end = *SEids_elt % 2;
		if (weight_length != 1)
			curr_weight = weight[index];
		curr_pos = x_start[index];
		if (is_end) {
			curr_weight = - curr_weight;
			curr_pos += x_width[index];
		}
		curr_value = _safe_int_add(curr_value, curr_weight);
		if (curr_pos != prev_pos) {
			lengths_buf[max_nrun] = curr_pos - prev_pos;
			values_buf[max_nrun] = prev_value;
			max_nrun++;
			prev_pos = curr_pos;
		}
		prev_value = curr_value;
	}
	if (_get_ovflow_flag())
		warning("NAs produced by integer overflow");

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

