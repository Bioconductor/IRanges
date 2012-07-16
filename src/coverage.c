/****************************************************************************
 *                                                                          *
 *               Weighted coverage of a set of integer ranges               *
 *               --------------------------------------------               *
 *                                                                          *
 *                 Authors: Patrick Aboyoun and Herve Pages                 *
 *            Code for "sort" method based on timing enhancements           *
 *                  by Charles C. Berry <ccberry@ucsd.edu>                  *
 *                                                                          *
 ****************************************************************************/
#include "IRanges.h"
#include <stdlib.h> /* for qsort() */
#include <R_ext/Utils.h> /* for R_CheckUserInterrupt() */


/****************************************************************************
 *                              "sort" method                               *
 ****************************************************************************/

/****************************************************************************
 * Basic manipulation of the SEids buffer (Start/End ids).
 */

#define SEid_TO_1BASED_INDEX(SEid) ((SEid) >= 0 ? (SEid) : -(SEid))
#define SEid_IS_END(SEid) ((SEid) >= 0)

static const int *base_start;
static const int *base_width;

static int compar_SEids_for_asc_order(const void *p1, const void *p2)
{
	int SEid1, SEid2, index1, index2, s1, s2;

	SEid1 = *((const int *) p1);
	SEid2 = *((const int *) p2);
	index1 = SEid_TO_1BASED_INDEX(SEid1);
	index2 = SEid_TO_1BASED_INDEX(SEid2);
	/* If SEid is a Start id, then s = start
	   If SEid is an End id, then s = end + 1 */
	s1 = base_start[index1];
	if (SEid_IS_END(SEid1))
		s1 += base_width[index1];
	s2 = base_start[index2];
	if (SEid_IS_END(SEid2))
		s2 += base_width[index2];
	return s1 - s2;
}

/* Initialize the SEids buffer (integer weights). */
static int init_SEids_int_weight(int *SEids, const int *x_width, int x_len,
		const int *weight, int weight_len)
{
	int SEids_len, index;

	SEids_len = 0;
	for (index = 1; index <= x_len; index++, x_width++) {
		if (*x_width != 0 && *weight != 0) {
			*(SEids++) = index; /* Start id */
			*(SEids++) = - index; /* End id */
			SEids_len += 2;
		}
		if (weight_len != 1)
			weight++;
	}
	return SEids_len;
}

/* Initialize the SEids buffer (numeric weights). */
static int init_SEids_double_weight(int *SEids, const int *x_width, int x_len,
		const double *weight, int weight_len)
{
	int SEids_len, index;

	SEids_len = 0;
	for (index = 1; index <= x_len; index++, x_width++) {
		if (*x_width != 0 && *weight != 0.0) {
			*(SEids++) = index; /* Start id */
			*(SEids++) = - index; /* End id */
			SEids_len += 2;
		}
		if (weight_len != 1)
			weight++;
	}
	return SEids_len;
}

/* Sort the SEids buffer. */
static void sort_SEids(int *SEids, int SEids_len,
		const int *x_start, const int *x_width)
{
	base_start = x_start - 1;
	base_width = x_width - 1;
	qsort(SEids, SEids_len, sizeof(int), compar_SEids_for_asc_order);
	return;
}

/****************************************************************************
 * int_coverage_sort(), double_coverage_sort()
 */

/* 'values_buf' and 'lengths_buf' must have a length >= SEids_len + 1 */
static void compute_int_coverage_in_bufs(const int *SEids, int SEids_len,
		const int *x_start, const int *x_width,
		const int *weight, int weight_len, int ans_len,
		int *values_buf, int *lengths_buf)
{
	int weight0, curr_val, curr_pos, curr_weight,
	    i, prev_pos, index;

	weight0 = weight[0];
	*(values_buf++) = curr_val = 0;
	curr_pos = 1;
	_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
	for (i = 0; i < SEids_len; i++, SEids++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		prev_pos = curr_pos;
		index = SEid_TO_1BASED_INDEX(*SEids) - 1;
		curr_pos = x_start[index];
		curr_weight = weight_len == 1 ? weight0 : weight[index];
		if (SEid_IS_END(*SEids)) {
			curr_weight = - curr_weight;
			curr_pos += x_width[index];
		}
		curr_val = _safe_int_add(curr_val, curr_weight);
		*(values_buf++) = curr_val;
		*(lengths_buf++) = curr_pos - prev_pos;
	}
	if (_get_ovflow_flag())
		warning("NAs produced by integer overflow");
	*lengths_buf = ans_len + 1 - curr_pos;
	return;
}

static void compute_double_coverage_in_bufs(const int *SEids, int SEids_len,
		const int *x_start, const int *x_width,
		const double *weight, int weight_len, int ans_len,
		double *values_buf, int *lengths_buf)
{
	double weight0, curr_val, curr_weight;
	int curr_pos, i, prev_pos, index;

	weight0 = weight[0];
	*(values_buf++) = curr_val = 0.0;
	curr_pos = 1;
	for (i = 0; i < SEids_len; i++, SEids++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		prev_pos = curr_pos;
		index = SEid_TO_1BASED_INDEX(*SEids) - 1;
		curr_pos = x_start[index];
		curr_weight = weight_len == 1 ? weight0 : weight[index];
		if (SEid_IS_END(*SEids)) {
			curr_weight = - curr_weight;
			curr_pos += x_width[index];
		}
		curr_val += curr_weight;
		*(values_buf++) = curr_val;
		*(lengths_buf++) = curr_pos - prev_pos;
	}
	*lengths_buf = ans_len + 1 - curr_pos;
	return;
}

static SEXP int_coverage_sort(const int *x_start, const int *x_width,
		int x_len, const int *weight, int weight_len,
		int ans_len)
{
	int *SEids, SEids_len, zero, buf_len, *values_buf, *lengths_buf;

	SEids = (int *) R_alloc((long) 2 * x_len, sizeof(int));
	SEids_len = init_SEids_int_weight(SEids, x_width, x_len,
					  weight, weight_len);
	if (SEids_len == 0) {
		//return an Rle with one run of 0's
		zero = 0;
		return _integer_Rle_constructor(&zero, 1, &ans_len, 0);
	}
	sort_SEids(SEids, SEids_len, x_start, x_width);
	buf_len = SEids_len + 1;
	values_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	lengths_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	compute_int_coverage_in_bufs(SEids, SEids_len,
			x_start, x_width, weight, weight_len, ans_len,
			values_buf, lengths_buf);
	return _integer_Rle_constructor(values_buf, buf_len, lengths_buf, 0);
}

static SEXP double_coverage_sort(const int *x_start, const int *x_width,
		int x_len, const double *weight, int weight_len,
		int ans_len)
{
	int *SEids, SEids_len, buf_len, *lengths_buf;
	double zero, *values_buf;

	SEids = (int *) R_alloc((long) 2 * x_len, sizeof(int));
	SEids_len = init_SEids_double_weight(SEids, x_width, x_len,
					     weight, weight_len);
	if (SEids_len == 0) {
		//return an Rle with one run of 0's
		zero = 0.0;
		return _numeric_Rle_constructor(&zero, 1, &ans_len, 0);
	}
	sort_SEids(SEids, SEids_len, x_start, x_width);
	buf_len = SEids_len + 1;
	values_buf = (double *) R_alloc((long) buf_len, sizeof(double));
	lengths_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	compute_double_coverage_in_bufs(SEids, SEids_len,
			x_start, x_width, weight, weight_len, ans_len,
			values_buf, lengths_buf);
	return _numeric_Rle_constructor(values_buf, buf_len, lengths_buf, 0);
}


/****************************************************************************
 *                              "hash" method                               *
 ****************************************************************************/

static SEXP int_coverage_hash(
		const int *x_start, const int *x_width, int x_len,
		const int *weight, int weight_len,
		int ans_len)
{
	int *cvg_buf, i, *cvg_p, cumsum;

	cvg_buf = (int *) R_alloc((long) ans_len + 1, sizeof(int));
	memset(cvg_buf, 0, ans_len * sizeof(int));
	_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
	for (i = 0; i < x_len; i++, x_start++, x_width++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		cvg_p = cvg_buf + *x_start - 1;
		*cvg_p = _safe_int_add(*cvg_p,   *weight);
		cvg_p += *x_width;
		*cvg_p = _safe_int_add(*cvg_p, - *weight);
		if (weight_len != 1)
			weight++;
	}
	cumsum = 0;
	for (i = 0, cvg_p = cvg_buf; i < ans_len; i++, cvg_p++) {
		cumsum = _safe_int_add(*cvg_p, cumsum);
		*cvg_p = cumsum;
	}
	if (_get_ovflow_flag())
		warning("NAs produced by integer overflow");
	return _integer_Rle_constructor(cvg_buf, ans_len, NULL, 0);
}

static SEXP double_coverage_hash(
		const int *x_start, const int *x_width, int x_len,
		const double *weight, int weight_len,
		int ans_len)
{
	double *cvg_buf, *cvg_p, cumsum;
	int i;

	cvg_buf = (double *) R_alloc((long) ans_len + 1, sizeof(double));
	for (i = 0, cvg_p = cvg_buf; i < ans_len; i++, cvg_p++)
		*cvg_p = 0.0;
	for (i = 0; i < x_len; i++, x_start++, x_width++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		cvg_p = cvg_buf + *x_start - 1;
		*cvg_p += *weight;
		cvg_p += *x_width;
		*cvg_p -= *weight;
		if (weight_len != 1)
			weight++;
	}
	cumsum = 0.0;
	for (i = 0, cvg_p = cvg_buf; i < ans_len; i++, cvg_p++) {
		cumsum += *cvg_p;
		*cvg_p = cumsum;
	}
	return _numeric_Rle_constructor(cvg_buf, ans_len, NULL, 0);
}


/****************************************************************************
 *                        --- .Call ENTRY POINTS ---                        *
 *                                                                          *
 * IMPORTANT: For the functions below, the 'x_start' and 'x_width' args     *
 * must come from a Ranges object 'x' that has already been "restricted" to *
 * the [1,width] interval, that is, 'x_start' must be >= 1 and 'x_width'    *
 * must be <= width for all the ranges in 'x'.                              *
 ****************************************************************************/

SEXP Ranges_integer_coverage(SEXP x_start, SEXP x_width,
		SEXP width, SEXP weight, SEXP method)
{
	int x_len, width0, weight_len, zero;
	const int *x_start_p, *x_width_p, *weight_p;
	const char *method_ptr = CHAR(STRING_ELT(method, 0));

	x_len = _check_integer_pairs(x_start, x_width,
				     &x_start_p, &x_width_p,
				     "start(x)", "width(x)");
	width0 = INTEGER(width)[0];
	weight_len = LENGTH(weight);
	weight_p = INTEGER(weight);
	if (x_len == 0 || width0 == 0
	 || (weight_len == 1 && weight_p[0] == 0))
	{
		//return an Rle with no run (empty Rle) or one run of 0's
		zero = 0;
		return _integer_Rle_constructor(&zero, 1, &width0, 0);
	}
	if (strcmp(method_ptr, "sort") == 0)
		return int_coverage_sort(x_start_p, x_width_p, x_len,
					 weight_p, weight_len, width0);
	return int_coverage_hash(x_start_p, x_width_p, x_len,
				 weight_p, weight_len, width0);
}

SEXP Ranges_numeric_coverage(SEXP x_start, SEXP x_width,
		SEXP width, SEXP weight, SEXP method)
{
	int x_len, width0, weight_len;
	const int *x_start_p, *x_width_p;
	const double *weight_p;
	double zero;
	const char *method_ptr = CHAR(STRING_ELT(method, 0));

	x_len = _check_integer_pairs(x_start, x_width,
				     &x_start_p, &x_width_p,
				     "start(x)", "width(x)");
	width0 = INTEGER(width)[0];
	weight_len = LENGTH(weight);
	weight_p = REAL(weight);
	if (x_len == 0 || width0 == 0
	 || (weight_len == 1 && weight_p[0] == 0.0))
	{
		//return an Rle with no run (empty Rle) or one run of 0's
		zero = 0.0;
		return _numeric_Rle_constructor(&zero, 1, &width0, 0);
	}
	if (strcmp(method_ptr, "sort") == 0)
		return double_coverage_sort(x_start_p, x_width_p, x_len,
					    weight_p, weight_len, width0);
	return double_coverage_hash(x_start_p, x_width_p, x_len,
				    weight_p, weight_len, width0);
}

