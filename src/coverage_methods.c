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


static const char *x_label, *shift_label, *width_label, *weight_label;


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
	if (j != weight_len)
		warning("'%s' length is not a divisor of '%s' length",
			weight_label, x_label);
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
	if (j != weight_len)
		warning("'%s' length is not a divisor of '%s' length",
			weight_label, x_label);
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
	_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
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
		curr_val = _safe_int_add(curr_val, curr_weight);
		*(values_buf++) = curr_val;
		*(lengths_buf++) = curr_pos - prev_pos;
	}
	if (_get_ovflow_flag())
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
		return _integer_Rle_constructor(&zero, 1, &cvg_len, 0);
	}
	sort_SEids(SEids, SEids_len, x_start, x_width);
	buf_len = SEids_len + 1;
	values_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	lengths_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	compute_int_coverage_in_bufs(SEids, SEids_len,
			x_start, x_width, weight, weight_len, cvg_len,
			values_buf, lengths_buf);
	return _integer_Rle_constructor(values_buf, buf_len, lengths_buf, 0);
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
		return _numeric_Rle_constructor(&zero, 1, &cvg_len, 0);
	}
	sort_SEids(SEids, SEids_len, x_start, x_width);
	buf_len = SEids_len + 1;
	values_buf = (double *) R_alloc((long) buf_len, sizeof(double));
	lengths_buf = (int *) R_alloc((long) buf_len, sizeof(int));
	compute_double_coverage_in_bufs(SEids, SEids_len,
			x_start, x_width, weight, weight_len, cvg_len,
			values_buf, lengths_buf);
	return _numeric_Rle_constructor(values_buf, buf_len, lengths_buf, 0);
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
	_reset_ovflow_flag(); /* we use _safe_int_add() in loop below */
	for (i = j = 0; i < x_len; i++, j++, x_start++, x_width++) {
		if (i % 500000 == 499999)
			R_CheckUserInterrupt();
		if (j >= weight_len)
			j = 0; /* recycle j */
		cvg_p = cvg_buf + *x_start - 1;
		w = weight[j];
		*cvg_p = _safe_int_add(*cvg_p, w);
		cvg_p += *x_width;
		*cvg_p = _safe_int_add(*cvg_p, - w);
	}
	if (j != weight_len)
		warning("'%s' length is not a divisor of '%s' length",
			weight_label, x_label);
	cumsum = 0;
	for (i = 0, cvg_p = cvg_buf; i < cvg_len; i++, cvg_p++) {
		cumsum = _safe_int_add(*cvg_p, cumsum);
		*cvg_p = cumsum;
	}
	if (_get_ovflow_flag())
		warning("NAs produced by integer overflow");
	return _integer_Rle_constructor(cvg_buf, cvg_len, NULL, 0);
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
	if (j != weight_len)
		warning("'%s' length is not a divisor of '%s' length",
			weight_label, x_label);
	cumsum = 0.0;
	for (i = 0, cvg_p = cvg_buf; i < cvg_len; i++, cvg_p++) {
		cumsum += *cvg_p;
		*cvg_p = cumsum;
	}
	return _numeric_Rle_constructor(cvg_buf, cvg_len, NULL, 0);
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
 *                         cachedIRanges_coverage()                         *
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
 *   cached_x: a cachedIRanges struct holding the input ranges, those ranges
 *             being those of a fictive IRanges object 'x'.
 *   shift: an integer or numeric vector that is parallel to 'x'.
 *   width: either NULL, a single NA, or a single non-negative integer.
 * After the input ranges are shifted:
 *   - If 'width' is a non-negative integer, then the ranges are clipped with
 *     respect to the [1, width] interval and the function returns 'width' (as
 *     an int).
 *   - If 'width' is NULL or NA, then the ranges are clipped with respect to
 *     the [1, +inf) interval (i.e. they're only clipped on the left) and the
 *     function returns 'max(end(x))' or 0 if 'x' is empty.
 * The shifted and clipped ranges are returned in 'out_ranges'.
 * Let's call 'cvg_len' the value returned by the function. If the output
 * ranges are in a tiling configuration with respect to the [1, cvg_len]
 * interval (i.e. they're non-overlapping, ordered from left to right, and
 * they fully cover the interval), then '*out_ranges_are_tiles' is set to 1.
 * Otherwise, it's set to 0.
 */
static int shift_and_clip_ranges(const cachedIRanges *cached_x,
		SEXP shift, SEXP width,
		RangeAE *out_ranges, int *out_ranges_are_tiles)
{
	int x_len, shift_len, cvg_len, auto_cvg_len, prev_end,
	    i, j, x_start, x_end, shift_elt, tmp;

	x_len = _get_cachedIRanges_length(cached_x);

	/* Check 'shift'. */
	if (!(IS_INTEGER(shift) || IS_NUMERIC(shift)))
		error("'%s' must be a vector of integers", shift_label);
	shift_len = LENGTH(shift);
	if (shift_len > x_len) {
		if (shift_len != 1)
			error("'%s' is longer than '%s'", shift_label,
			      x_label);
	} else if (shift_len < x_len) {
		if (shift_len == 0)
			error("cannot recycle zero-length '%s' "
			      "to the length of '%s'", shift_label,
			      x_label);
	}

	/* Infer 'cvg_len' from 'width'. */
	if (width == R_NilValue) {
		cvg_len = NA_INTEGER;
	} else if (IS_LOGICAL(width)) {
		if (LENGTH(width) != 1 || LOGICAL(width)[0] != NA_INTEGER)
			error("when '%s' is a logical vector, "
			      "it must be a single NA", width_label);
		cvg_len = NA_INTEGER;
	} else if (IS_INTEGER(width)) {
		if (LENGTH(width) != 1)
			error("when '%s' is an integer vector, "
			      "it must be of length 1", width_label);
		cvg_len = INTEGER(width)[0];
	} else if (IS_NUMERIC(width)) {
		if (LENGTH(width) != 1)
			error("when '%s' is a numeric vector, "
			      "it must be of length 1", width_label);
		cvg_len = double2int(REAL(width)[0]);
	} else {
		error("'%s' must be either NULL, a single NA, "
		      "or a single non-negative integer", width_label);
	}

	if (cvg_len == 0) {
		*out_ranges_are_tiles = 1;
		return 0;
	}
	auto_cvg_len = cvg_len == NA_INTEGER;
	if (auto_cvg_len)
		cvg_len = 0;
	else if (cvg_len < 0)
		error("'%s' cannot be negative", width_label);
	if (x_len == 0) {
		*out_ranges_are_tiles = auto_cvg_len;
		return cvg_len;
	}

	*out_ranges_are_tiles = 1;
	_RangeAE_set_nelt(out_ranges, 0);
	prev_end = 0;
	for (i = j = 0; i < x_len; i++, j++) {
		if (j >= shift_len)
			j = 0; /* recycle j */
		x_start = _get_cachedIRanges_elt_start(cached_x, i);
		x_end = _get_cachedIRanges_elt_end(cached_x, i);
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
		_RangeAE_insert_at(out_ranges, i,
				   x_start, x_end - x_start + 1);
	}
	if (j != shift_len)
		warning("'%s' length is not a divisor of '%s' length",
			shift_label, x_label);
	if (*out_ranges_are_tiles && x_end != cvg_len)
		*out_ranges_are_tiles = 0;
	return cvg_len;
}

static SEXP cachedIRanges_coverage(const cachedIRanges *cached_x,
		SEXP shift, SEXP width, SEXP weight, SEXP method,
		RangeAE *ranges_buf)
{
	int x_len, cvg_len, out_ranges_are_tiles, weight_len,
	    effective_method, take_short_path;
	const int *x_start, *x_width;
	const char *method0;

	x_len = _get_cachedIRanges_length(cached_x);
	cvg_len = shift_and_clip_ranges(cached_x, shift, width, ranges_buf,
					&out_ranges_are_tiles);
	x_start = ranges_buf->start.elts;
	x_width = ranges_buf->width.elts;

	/* Check 'weight'. */
	if (!(IS_INTEGER(weight) || IS_NUMERIC(weight)))
		error("'%s' must be an integer or numeric vector",
		      weight_label);
	weight_len = LENGTH(weight);
	if (weight_len > x_len) {
		if (weight_len != 1)
			error("'%s' is longer than '%s'",
			      weight_label, x_label);
	} else if (weight_len < x_len) {
		if (weight_len == 0)
			error("cannot recycle zero-length '%s' "
			      "to the length of '%s'",
			      weight_label, x_label);
	}

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
		take_short_path = 0;
		if (cvg_len == 0) {
			take_short_path = 1;
			x_len = 0;
		} else if (weight_len == 1) {
			take_short_path = 1;
			x_len = 1;
			x_width = &cvg_len;
		} else if (weight_len == x_len) {
			take_short_path = 1;
		}
		if (take_short_path) {
			/* Short path for the tiling case. */
			//Rprintf("taking short path\n");
			if (IS_INTEGER(weight)) {
				return _integer_Rle_constructor(
							INTEGER(weight), x_len,
							x_width, 0);
			} else {
				return _numeric_Rle_constructor(
							REAL(weight), x_len,
							x_width, 0);
			}
		}
	}
	//Rprintf("taking normal path\n");
	return effective_method == 1 ?
	       coverage_sort(x_start, x_width, x_len, weight, cvg_len) :
	       coverage_hash(x_start, x_width, x_len, weight, cvg_len);
}

/* --- .Call ENTRY POINT ---
 * Args:
 *   x:      an IRanges object.
 *   shift:  a numeric (integer or double) vector.
 *   width:  a NULL, a single NA, or a single non-negative number.
 *   weight: a numeric (integer or double) vector.
 *   method: either "auto", "sort", or "hash".
 * Returns an Rle object.
 */
SEXP IRanges_coverage(SEXP x, SEXP shift, SEXP width, SEXP weight, SEXP method)
{
	cachedIRanges cached_x;
	int x_len;
	RangeAE ranges_buf;

	cached_x = _cache_IRanges(x);
	x_len = _get_cachedIRanges_length(&cached_x);
	ranges_buf = _new_RangeAE(x_len, 0);
	x_label = "x";
	shift_label = "shift";
	width_label = "width";
	weight_label = "weight";
	return cachedIRanges_coverage(&cached_x, shift, width, weight, method,
				      &ranges_buf);
}

/* --- .Call ENTRY POINT ---
 * Args:
 *   x:      a CompressedIRangesList object of length N.
 *   shift:  a list of length N. Each element must be a numeric (integer or
 *           double) vector.
 *   width:  a list of length N. Each element must be a NULL, a single NA,
 *           or a single non-negative number.
 *   weight: a list of length N. Each element must be a numeric (integer or
 *           double) vector.
 *   method: either "auto", "sort", or "hash".
 * Returns a list of N RleList objects.
 */
SEXP CompressedIRangesList_coverage(SEXP x, SEXP shift,
		SEXP width, SEXP weight, SEXP method)
{
	cachedCompressedIRangesList cached_x;
	int x_len, shift_len, width_len, weight_len, i, j, k, l;
	RangeAE ranges_buf;
	SEXP ans, ans_elt, shift_elt, width_elt, weight_elt;
	cachedIRanges cached_x_elt;
	char x_label_buf[40], shift_label_buf[40],
	     width_label_buf[40], weight_label_buf[40];

	cached_x = _cache_CompressedIRangesList(x);
	x_len = _get_cachedCompressedIRangesList_length(&cached_x);
	shift_len = LENGTH(shift);
	width_len = LENGTH(width);
	weight_len = LENGTH(weight);
	ranges_buf = _new_RangeAE(0, 0);
	x_label = x_label_buf;
	shift_label = shift_label_buf;
	width_label = width_label_buf;
	weight_label = weight_label_buf;
	PROTECT(ans = NEW_LIST(x_len));
	for (i = j = k = l = 0; i < x_len; i++, j++, k++, l++) {
		if (j >= shift_len)
			j = 0; /* recycle j */
		if (k >= width_len)
			k = 0; /* recycle k */
		if (l >= weight_len)
			l = 0; /* recycle l */
		cached_x_elt = _get_cachedCompressedIRangesList_elt(&cached_x,
						i);
		shift_elt = VECTOR_ELT(shift, j);
		width_elt = VECTOR_ELT(width, k);
		weight_elt = VECTOR_ELT(weight, l);
		snprintf(x_label_buf, sizeof(x_label_buf),
			 "x[[%d]]", i + 1);
		snprintf(shift_label_buf, sizeof(shift_label_buf),
			 "shift[[%d]]", j + 1);
		snprintf(width_label_buf, sizeof(width_label_buf),
			 "width[[%d]]", k + 1);
		snprintf(weight_label_buf, sizeof(weight_label_buf),
			 "weight[[%d]]", l + 1);
		PROTECT(ans_elt = cachedIRanges_coverage(&cached_x_elt,
						shift_elt, width_elt,
						weight_elt, method,
						&ranges_buf));
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

