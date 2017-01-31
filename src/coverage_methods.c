/****************************************************************************
 *                                                                          *
 *               Weighted coverage of a set of integer ranges               *
 *               --------------------------------------------               *
 *                                                                          *
 *                    Authors: H. Pag\`es and P. Aboyoun                    *
 *            Code for "sort" method based on timing enhancements           *
 *                  by Charles C. Berry <ccberry@ucsd.edu>                  *
 *                                                                          *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h> /* for qsort() */
#include <R_ext/Utils.h> /* for R_CheckUserInterrupt() */


static const char *x_label, *shift_label, *width_label, *weight_label;

static void check_recycling_was_round(int last_pos_in_current, int current_len,
		const char *current_label, const char *target_label)
{
	if (current_len >= 2 && last_pos_in_current < current_len)
		warning("'%s' length is not a divisor of '%s' length",
			current_label, target_label);
	return;
}


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
	int SEids_len, i, j, index;

	SEids_len = 0;
	for (i = j = 0, index = 1; i < x_len; i++, j++, index++) {
		if (j >= weight_len)
			j = 0; /* recycle j */
		if (x_width[i] == 0 || weight[j] == 0)
			continue;
		*(SEids++) = index; /* Start id */
		*(SEids++) = - index; /* End id */
		SEids_len += 2;
	}
	check_recycling_was_round(j, weight_len, weight_label, x_label);
	return SEids_len;
}

/* Initialize the SEids buffer (numeric weights). */
static int init_SEids_double_weight(int *SEids, const int *x_width, int x_len,
		const double *weight, int weight_len)
{
	int SEids_len, i, j, index;

	SEids_len = 0;
	for (i = j = 0, index = 1; i < x_len; i++, j++, index++) {
		if (j >= weight_len)
			j = 0; /* recycle j */
		if (x_width[i] == 0 || weight[j] == 0.0)
			continue;
		*(SEids++) = index; /* Start id */
		*(SEids++) = - index; /* End id */
		SEids_len += 2;
	}
	check_recycling_was_round(j, weight_len, weight_label, x_label);
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
		const int *weight, int weight_len, int cvg_len,
		int *values_buf, int *lengths_buf)
{
	int curr_val, curr_weight,
	    curr_pos, i, prev_pos, index;

	*(values_buf++) = curr_val = 0;
	curr_pos = 1;
	reset_ovflow_flag(); /* we use safe_int_add() in loop below */
	for (i = 0; i < SEids_len; i++, SEids++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		prev_pos = curr_pos;
		index = SEid_TO_1BASED_INDEX(*SEids) - 1;
		curr_pos = x_start[index];
		curr_weight = weight[index % weight_len];
		if (SEid_IS_END(*SEids)) {
			curr_weight = - curr_weight;
			curr_pos += x_width[index];
		}
		curr_val = safe_int_add(curr_val, curr_weight);
		*(values_buf++) = curr_val;
		*(lengths_buf++) = curr_pos - prev_pos;
	}
	if (get_ovflow_flag())
		warning("NAs produced by integer overflow");
	*lengths_buf = cvg_len + 1 - curr_pos;
	return;
}

static void compute_double_coverage_in_bufs(const int *SEids, int SEids_len,
		const int *x_start, const int *x_width,
		const double *weight, int weight_len, int cvg_len,
		double *values_buf, int *lengths_buf)
{
	double curr_val, curr_weight;
	int curr_pos, i, prev_pos, index;

	*(values_buf++) = curr_val = 0.0;
	curr_pos = 1;
	for (i = 0; i < SEids_len; i++, SEids++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		prev_pos = curr_pos;
		index = SEid_TO_1BASED_INDEX(*SEids) - 1;
		curr_pos = x_start[index];
		curr_weight = weight[index % weight_len];
		if (SEid_IS_END(*SEids)) {
			curr_weight = - curr_weight;
			curr_pos += x_width[index];
		}
		curr_val += curr_weight;
		*(values_buf++) = curr_val;
		*(lengths_buf++) = curr_pos - prev_pos;
	}
	*lengths_buf = cvg_len + 1 - curr_pos;
	return;
}

static SEXP int_coverage_sort(const int *x_start, const int *x_width,
		int x_len, const int *weight, int weight_len,
		int cvg_len)
{
	int *SEids, SEids_len, zero, buf_len, *values_buf, *lengths_buf;

	SEids = (int *) R_alloc((long) 2 * x_len, sizeof(int));
	SEids_len = init_SEids_int_weight(SEids, x_width, x_len,
					  weight, weight_len);
	if (SEids_len == 0) {
		//return an Rle with one run of 0's
		zero = 0;
		return construct_integer_Rle(1, &zero, &cvg_len, 0);
	}
	sort_SEids(SEids, SEids_len, x_start, x_width);
	buf_len = SEids_len + 1;
	values_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	lengths_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	compute_int_coverage_in_bufs(SEids, SEids_len,
			x_start, x_width, weight, weight_len, cvg_len,
			values_buf, lengths_buf);
	return construct_integer_Rle(buf_len, values_buf, lengths_buf, 0);
}

static SEXP double_coverage_sort(const int *x_start, const int *x_width,
		int x_len, const double *weight, int weight_len,
		int cvg_len)
{
	int *SEids, SEids_len, buf_len, *lengths_buf;
	double zero, *values_buf;

	SEids = (int *) R_alloc((long) 2 * x_len, sizeof(int));
	SEids_len = init_SEids_double_weight(SEids, x_width, x_len,
					     weight, weight_len);
	if (SEids_len == 0) {
		//return an Rle with one run of 0's
		zero = 0.0;
		return construct_numeric_Rle(1, &zero, &cvg_len, 0);
	}
	sort_SEids(SEids, SEids_len, x_start, x_width);
	buf_len = SEids_len + 1;
	values_buf = (double *) R_alloc((long) buf_len, sizeof(double));
	lengths_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	compute_double_coverage_in_bufs(SEids, SEids_len,
			x_start, x_width, weight, weight_len, cvg_len,
			values_buf, lengths_buf);
	return construct_numeric_Rle(buf_len, values_buf, lengths_buf, 0);
}

static SEXP coverage_sort(const int *x_start, const int *x_width, int x_len,
		SEXP weight, int cvg_len)
{
	int weight_len;

	weight_len = LENGTH(weight);
	return IS_INTEGER(weight) ?
	       int_coverage_sort(x_start, x_width, x_len,
				INTEGER(weight), weight_len, cvg_len) :
	       double_coverage_sort(x_start, x_width, x_len,
				REAL(weight), weight_len, cvg_len);
}


/****************************************************************************
 *                              "hash" method                               *
 ****************************************************************************/

static SEXP int_coverage_hash(
		const int *x_start, const int *x_width, int x_len,
		const int *weight, int weight_len,
		int cvg_len)
{
	int *cvg_buf, *cvg_p, w, cumsum,
	    i, j;

	cvg_buf = (int *) R_alloc((long) cvg_len + 1, sizeof(int));
	memset(cvg_buf, 0, cvg_len * sizeof(int));
	reset_ovflow_flag(); /* we use safe_int_add() in loop below */
	for (i = j = 0; i < x_len; i++, j++, x_start++, x_width++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		if (j >= weight_len)
			j = 0; /* recycle j */
		cvg_p = cvg_buf + *x_start - 1;
		w = weight[j];
		*cvg_p = safe_int_add(*cvg_p, w);
		cvg_p += *x_width;
		*cvg_p = safe_int_add(*cvg_p, - w);
	}
	check_recycling_was_round(j, weight_len, weight_label, x_label);
	cumsum = 0;
	for (i = 0, cvg_p = cvg_buf; i < cvg_len; i++, cvg_p++) {
		cumsum = safe_int_add(*cvg_p, cumsum);
		*cvg_p = cumsum;
	}
	if (get_ovflow_flag())
		warning("NAs produced by integer overflow");
	return construct_integer_Rle(cvg_len, cvg_buf, NULL, 0);
}

static SEXP double_coverage_hash(
		const int *x_start, const int *x_width, int x_len,
		const double *weight, int weight_len,
		int cvg_len)
{
	double *cvg_buf, *cvg_p, w, cumsum;
	int i, j;

	cvg_buf = (double *) R_alloc((long) cvg_len + 1, sizeof(double));
	for (i = 0, cvg_p = cvg_buf; i < cvg_len; i++, cvg_p++)
		*cvg_p = 0.0;
	for (i = j = 0; i < x_len; i++, j++, x_start++, x_width++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		if (j >= weight_len)
			j = 0; /* recycle j */
		cvg_p = cvg_buf + *x_start - 1;
		w = weight[j];
		*cvg_p += w;
		cvg_p += *x_width;
		*cvg_p -= w;
	}
	check_recycling_was_round(j, weight_len, weight_label, x_label);
	cumsum = 0.0;
	for (i = 0, cvg_p = cvg_buf; i < cvg_len; i++, cvg_p++) {
		cumsum += *cvg_p;
		*cvg_p = cumsum;
	}
	return construct_numeric_Rle(cvg_len, cvg_buf, NULL, 0);
}

static SEXP coverage_hash(const int *x_start, const int *x_width, int x_len,
		SEXP weight, int cvg_len)
{
	int weight_len;

	weight_len = LENGTH(weight);
	return IS_INTEGER(weight) ?
	       int_coverage_hash(x_start, x_width, x_len,
				INTEGER(weight), weight_len, cvg_len) :
	       double_coverage_hash(x_start, x_width, x_len,
				REAL(weight), weight_len, cvg_len);
}


/****************************************************************************
 * Helper functions for checking args of type SEXP.                         *
 * They either pass (and return nothing) or raise an error with an          *
 * informative message.                                                     *
 ****************************************************************************/

static void check_arg_is_integer(SEXP arg, const char *arg_label)
{
	if (!IS_INTEGER(arg))
		error("'%s' must be an integer vector", arg_label);
	return;
}

static void check_arg_is_numeric(SEXP arg, const char *arg_label)
{
	if (!(IS_INTEGER(arg) || IS_NUMERIC(arg)))
		error("'%s' must be an integer or numeric vector", arg_label);
	return;
}

static void check_arg_is_list(SEXP arg, const char *arg_label)
{
	if (!IS_LIST(arg))
		error("'%s' must be a list", arg_label);
	return;
}

/*
 * Check that 'arg_len' is equal to 'x_len', or that it's the length of an
 * argument that can be recycled to the length of 'x'.
 * Assumes that 'arg_len' and 'x_len' are >= 0.
 */
static void check_arg_is_recyclable(int arg_len, int x_len,
		const char *arg_label, const char *x_label)
{
	if (arg_len < x_len) {
		if (arg_len == 0)
			error("cannot recycle zero-length '%s' "
			      "to the length of '%s'",
			      arg_label, x_label);
	} else if (arg_len > x_len) {
		if (arg_len >= 2)
			error("'%s' is longer than '%s'",
			      arg_label, x_label);
	}
	return;
}


/****************************************************************************
 *                  compute_coverage_from_IRanges_holder()                  *
 ****************************************************************************/

/*
 * This is probably overly cautious. Could be that the cast from double to int
 * with (int) already does exactly this (i.e. produces an NA_INTEGER for all
 * the cases explicitely handled here) and is portable.
 */
static int double2int(double x)
{
	if (x == R_PosInf
	 || x == R_NegInf
	 || ISNAN(x) /* NA or NaN */
	 || x >= (double) INT_MAX + 1.00
	 || x <= (double) INT_MIN)
		return NA_INTEGER;
	return (int) x;
}

/*
 * Args:
 *   x_holder:   A IRanges_holder struct holding the input ranges, those
 *               ranges being those of a fictive IRanges object 'x'.
 *   shift:      A numeric (integer or double) vector parallel to 'x' (will
 *               get recycled if necessary) with no NAs.
 *   width:      A single integer. NA or >= 0.
 *   circle_len: A single integer. NA or > 0.
 * After the input ranges are shifted:
 *   - If 'width' is a non-negative integer, then the ranges are clipped with
 *     respect to the [1, width] interval and the function returns 'width'.
 *   - If 'width' is NA, then the ranges are clipped with respect to the
 *     [1, +inf) interval (i.e. they're only clipped on the left) and the
 *     function returns 'max(end(x))' or 0 if 'x' is empty.
 * The shifted and clipped ranges are returned in 'out_ranges'.
 * Let's call 'cvg_len' the value returned by the function. If the output
 * ranges are in a tiling configuration with respect to the [1, cvg_len]
 * interval (i.e. they're non-overlapping, ordered from left to right, and
 * they fully cover the interval), then '*out_ranges_are_tiles' is set to 1.
 * Otherwise, it's set to 0.
 */
static int shift_and_clip_ranges(const IRanges_holder *x_holder,
		SEXP shift, int width, int circle_len,
		IntPairAE *out_ranges, int *out_ranges_are_tiles)
{
	int x_len, shift_len, cvg_len, auto_cvg_len, prev_end,
	    i, j, x_start, x_end, shift_elt, tmp;

	x_len = _get_length_from_IRanges_holder(x_holder);

	/* Check 'shift'. */
	check_arg_is_numeric(shift, shift_label);
	shift_len = LENGTH(shift);
	check_arg_is_recyclable(shift_len, x_len, shift_label, x_label);

	/* Infer 'cvg_len' from 'width' and 'circle_len'. */
	*out_ranges_are_tiles = 1;
	if (width == NA_INTEGER) {
		auto_cvg_len = 1;
	} else if (width < 0) {
		error("'%s' cannot be negative", width_label);
	} else if (width == 0) {
		return width;
	} else if (circle_len == NA_INTEGER) {
		auto_cvg_len = 0;
	} else if (circle_len <= 0) {
		error("length of underlying circular sequence is <= 0");
	} else if (width > circle_len) {
		error("'%s' cannot be greater than length of "
		      "underlying circular sequence", width_label);
	} else {
		auto_cvg_len = 1;
	}
	cvg_len = auto_cvg_len ? 0 : width;
	if (x_len == 0) {
		if (cvg_len != 0)
			*out_ranges_are_tiles = 0;
		return cvg_len;
	}

	IntPairAE_set_nelt(out_ranges, 0);
	prev_end = 0;
	for (i = j = 0; i < x_len; i++, j++) {
		if (j >= shift_len)
			j = 0; /* recycle j */
		x_start = _get_start_elt_from_IRanges_holder(x_holder, i);
		x_end = _get_end_elt_from_IRanges_holder(x_holder, i);
		if (IS_INTEGER(shift)) {
			shift_elt = INTEGER(shift)[j];
			if (shift_elt == NA_INTEGER)
				error("'%s' contains NAs", shift_label);
		} else {
			shift_elt = double2int(REAL(shift)[j]);
			if (shift_elt == NA_INTEGER)
				error("'%s' contains NAs, NaNs, or numbers "
				      "that cannot be turned into integers",
				      shift_label);
		}
		/* Risk of integer overflow! */
		x_start += shift_elt;
		x_end += shift_elt;
		if (circle_len != NA_INTEGER) {
			tmp = x_start % circle_len;
			if (tmp <= 0)
				tmp += circle_len;
			x_end += tmp - x_start;
			x_start = tmp;
		}
		if (x_end < 0) {
			x_end = 0;
		} else if (x_end > cvg_len) {
			if (auto_cvg_len)
				cvg_len = x_end;
			else
				x_end = cvg_len;
		}
		if (x_start < 1)
			x_start = 1;
		else if (x_start > (tmp = cvg_len + 1))
			x_start = tmp;
		if (*out_ranges_are_tiles) {
			if (x_start == prev_end + 1)
				prev_end = x_end;
			else
				*out_ranges_are_tiles = 0;
		}
		IntPairAE_insert_at(out_ranges, i,
				    x_start, x_end - x_start + 1);
	}
	check_recycling_was_round(j, shift_len, shift_label, x_label);
	if (*out_ranges_are_tiles && x_end != cvg_len)
		*out_ranges_are_tiles = 0;
	return cvg_len;
}

/*
 * Args:
 *   x_holder:   A IRanges_holder struct holding the input ranges, those
 *               ranges being those of a fictive IRanges object 'x'.
 *   shift:      A numeric (integer or double) vector parallel to 'x' (will
 *               get recycled if necessary) with no NAs.
 *   width:      A single integer. NA or >= 0.
 *   weight:     A numeric (integer or double) vector parallel to 'x' (will
 *               get recycled if necessary).
 *   circle_len: A single integer. NA or > 0.
 *   method:     Either "auto", "sort", or "hash".
 * Returns an Rle object.
 */
static SEXP compute_coverage_from_IRanges_holder(
		const IRanges_holder *x_holder,
		SEXP shift, int width, SEXP weight, int circle_len,
		SEXP method, IntPairAE *ranges_buf)
{
	int x_len, cvg_len, out_ranges_are_tiles, weight_len,
	    effective_method, take_short_path;
	const int *x_start, *x_width;
	const char *method0;

	x_len = _get_length_from_IRanges_holder(x_holder);
	cvg_len = shift_and_clip_ranges(x_holder, shift, width, circle_len,
					ranges_buf, &out_ranges_are_tiles);
	x_start = ranges_buf->a->elts;
	x_width = ranges_buf->b->elts;

	/* Check 'weight'. */
	check_arg_is_numeric(weight, weight_label);
	weight_len = LENGTH(weight);
	check_arg_is_recyclable(weight_len, x_len, weight_label, x_label);

	/* Infer 'effective_method' from 'method' and 'cvg_len'. */
	if (!IS_CHARACTER(method) || LENGTH(method) != 1)
		error("'method' must be a single string");
	method = STRING_ELT(method, 0);
	if (method == NA_STRING)
		error("'method' cannot be NA");
	method0 = CHAR(method);
	if (strcmp(method0, "auto") == 0) {
		/* Based on empirical observation. */
		effective_method = x_len <= 0.25 * cvg_len ? 1 : 2;
	} else if (strcmp(method0, "sort") == 0) {
		effective_method = 1;
	} else if (strcmp(method0, "hash") == 0) {
		effective_method = 2;
	} else {
		error("'method' must be \"auto\", \"sort\", or \"hash\"");
	}

	//Rprintf("out_ranges_are_tiles = %d\n", out_ranges_are_tiles);
	//Rprintf("x_len = %d\n", x_len);
	//Rprintf("cvg_len = %d\n", cvg_len);

	if (out_ranges_are_tiles) {
		if (cvg_len == 0) {
			take_short_path = 1;
			x_len = 0;
		} else if (weight_len == 1) {
			take_short_path = 1;
			x_len = 1;
			x_width = &cvg_len;
		} else if (weight_len == x_len) {
			take_short_path = 1;
		} else {
			take_short_path = 0;
		}
		if (take_short_path) {
			/* Short path for the tiling case. */
			//Rprintf("taking short path\n");
			return IS_INTEGER(weight) ?
			       construct_integer_Rle(x_len, INTEGER(weight),
						     x_width, 0) :
			       construct_numeric_Rle(x_len, REAL(weight),
						     x_width, 0);
		}
	}
	//Rprintf("taking normal path\n");
	return effective_method == 1 ?
	       coverage_sort(x_start, x_width, x_len, weight, cvg_len) :
	       coverage_hash(x_start, x_width, x_len, weight, cvg_len);
}

/* --- .Call ENTRY POINT ---
 * Args:
 *   x:          An IRanges object.
 *   shift:      A numeric (integer or double) vector parallel to 'x' (will
 *               get recycled if necessary) with no NAs.
 *   width:      A single integer. NA or >= 0.
 *   weight:     A numeric (integer or double) vector parallel to 'x' (will
 *               get recycled if necessary).
 *   circle_len: A single integer. NA or > 0.
 *   method:     Either "auto", "sort", or "hash".
 * Returns an Rle object.
 */
SEXP IRanges_coverage(SEXP x, SEXP shift, SEXP width, SEXP weight,
		SEXP circle_len, SEXP method)
{
	IRanges_holder x_holder;
	int x_len;
	IntPairAE *ranges_buf;

	x_holder = _hold_IRanges(x);
	x_len = _get_length_from_IRanges_holder(&x_holder);

	/* Check 'width'. */
	check_arg_is_integer(width, "width");
	if (LENGTH(width) != 1)
		error("'%s' must be a single integer", "width");

	/* Check 'circle_len'. */
	check_arg_is_integer(circle_len, "circle.length");
	if (LENGTH(circle_len) != 1)
		error("'%s' must be a single integer", "circle.length");

	ranges_buf = new_IntPairAE(x_len, 0);
	x_label = "x";
	shift_label = "shift";
	width_label = "width";
	weight_label = "weight";
	return compute_coverage_from_IRanges_holder(&x_holder,
				shift, INTEGER(width)[0],
				weight, INTEGER(circle_len)[0],
				method, ranges_buf);
}

/* --- .Call ENTRY POINT ---
 * Args:
 *   x:           A CompressedIRangesList object of length N.
 *   shift:       A list of length N (will get recycled if necessary). After
 *                recycling, each list element must be a numeric (integer or
 *                double) vector parallel to x[[i]] that will itself get
 *                recycled if necessary, and with no NAs.
 *   width:       An integer vector of length N (will get recycled if
 *                necessary). Values must be NAs or >= 0.
 *                or a single non-negative number.
 *   weight:      A list of length N (will get recycled if necessary). After
 *                recycling, each list element must be a numeric (integer or
 *                double) vector parallel to x[[i]] that will itself get
 *                recycled if necessary.
 *   circle_lens: An integer vector of length N (will get recycled if
 *                necessary). Values must be NAs or > 0.
 *   method:      Either "auto", "sort", or "hash".
 * Returns a list of N RleList objects.
 */
SEXP CompressedIRangesList_coverage(SEXP x,
		SEXP shift, SEXP width, SEXP weight, SEXP circle_lens,
		SEXP method)
{
	CompressedIRangesList_holder x_holder;
	int x_len, shift_len, width_len, weight_len, circle_lens_len,
	    i, j, k, l, m;
	IntPairAE *ranges_buf;
	SEXP ans, ans_elt, shift_elt, weight_elt;
	IRanges_holder x_elt_holder;
	char x_label_buf[40], shift_label_buf[40],
	     width_label_buf[40], weight_label_buf[40];

	x_holder = _hold_CompressedIRangesList(x);
	x_len = _get_length_from_CompressedIRangesList_holder(&x_holder);

	/* Check 'shift'. */
	check_arg_is_list(shift, "shift");
	shift_len = LENGTH(shift);
	check_arg_is_recyclable(shift_len, x_len, "shift", "x");

	/* Check 'width'. */
	check_arg_is_integer(width, "width");
	width_len = LENGTH(width);
	check_arg_is_recyclable(width_len, x_len, "width", "x");

	/* Check 'weight'. */
	check_arg_is_list(weight, "weight");
	weight_len = LENGTH(weight);
	check_arg_is_recyclable(weight_len, x_len, "weight", "x");

	/* Check 'circle_lens'. */
	check_arg_is_integer(circle_lens, "circle.length");
	circle_lens_len = LENGTH(circle_lens);
	check_arg_is_recyclable(circle_lens_len, x_len, "circle.length", "x");

	ranges_buf = new_IntPairAE(0, 0);
	x_label = x_label_buf;
	shift_label = shift_label_buf;
	width_label = width_label_buf;
	weight_label = weight_label_buf;
	PROTECT(ans = NEW_LIST(x_len));
	for (i = j = k = l = m = 0; i < x_len; i++, j++, k++, l++, m++) {
		if (j >= shift_len)
			j = 0; /* recycle j */
		if (k >= width_len)
			k = 0; /* recycle k */
		if (l >= weight_len)
			l = 0; /* recycle l */
		if (m >= circle_lens_len)
			m = 0; /* recycle m */
		snprintf(x_label_buf, sizeof(x_label_buf),
			 "x[[%d]]", i + 1);
		snprintf(shift_label_buf, sizeof(shift_label_buf),
			 "shift[[%d]]", j + 1);
		snprintf(width_label_buf, sizeof(width_label_buf),
			 "width[%d]", k + 1);
		snprintf(weight_label_buf, sizeof(weight_label_buf),
			 "weight[[%d]]", l + 1);
		x_elt_holder = _get_elt_from_CompressedIRangesList_holder(
						&x_holder, i);
		shift_elt = VECTOR_ELT(shift, j);
		weight_elt = VECTOR_ELT(weight, l);
		PROTECT(ans_elt = compute_coverage_from_IRanges_holder(
						&x_elt_holder,
						shift_elt,
						INTEGER(width)[k],
						weight_elt,
						INTEGER(circle_lens)[m],
						method, ranges_buf));
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	check_recycling_was_round(j, shift_len, "shift", "x");
	check_recycling_was_round(k, width_len, "width", "x");
	check_recycling_was_round(l, weight_len, "weight", "x");
	check_recycling_was_round(m, circle_lens_len, "circle.length", "x");
	UNPROTECT(1);
	return ans;
}

