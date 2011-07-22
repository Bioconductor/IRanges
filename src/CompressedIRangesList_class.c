/****************************************************************************
 *         Low-level manipulation of CompressedIRangesList objects          *
 ****************************************************************************/

#include "IRanges.h"
#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)


/****************************************************************************
 * C-level abstract getters.
 */

cachedCompressedIRangesList _cache_CompressedIRangesList(SEXP x)
{
	cachedCompressedIRangesList cached_x;
	SEXP x_end;

	cached_x.classname = _get_classname(x);
	x_end = _get_PartitioningByEnd_end(
			_get_CompressedList_partitioning(x));
	cached_x.length = LENGTH(x_end);
	cached_x.end = INTEGER(x_end);
	cached_x.cached_unlistData = _cache_IRanges(
			_get_CompressedList_unlistData(x));
	return cached_x;
}

int _get_cachedCompressedIRangesList_length(
		const cachedCompressedIRangesList *cached_x)
{
	return cached_x->length;
}

cachedIRanges _get_cachedCompressedIRangesList_elt(
		const cachedCompressedIRangesList *cached_x, int i)
{
	int offset, length;

	offset = i == 0 ? 0 : cached_x->end[i - 1];
	length = cached_x->end[i] - offset;
	return _sub_cachedIRanges(&(cached_x->cached_unlistData),
			offset, length);
}

int _get_cachedCompressedIRangesList_eltLength(
		const cachedCompressedIRangesList *cached_x, int i)
{
/*
	cachedIRanges cached_ir;

	cached_ir = _get_cachedCompressedIRangesList_elt(cached_x, i);
	return _get_cachedIRanges_length(&cached_ir);
*/
	int offset;

	offset = i == 0 ? 0 : cached_x->end[i - 1];
	return cached_x->end[i] - offset; /* faster than the above */
}


/****************************************************************************
 * CompressedIRangesList methods.
 */

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_isNormal(SEXP x, SEXP use_names)
{
	SEXP ans, ans_names;
	cachedCompressedIRangesList cached_x;
	cachedIRanges cached_ir;
	int x_length, i;

	cached_x = _cache_CompressedIRangesList(x);
	x_length = _get_cachedCompressedIRangesList_length(&cached_x);
	PROTECT(ans = NEW_LOGICAL(x_length));
	for (i = 0; i < x_length; i++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_x, i);
		LOGICAL(ans)[i] = _is_normal_cachedIRanges(&cached_ir);
	}
	if (LOGICAL(use_names)[0]) {
		PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

static int get_cachedCompressedIRangesList_max_eltLengths(
		const cachedCompressedIRangesList *cached_x)
{
	int max_ir_length, x_length, i, ir_length;

	max_ir_length = 0;
	x_length = _get_cachedCompressedIRangesList_length(cached_x);
	for (i = 0; i < x_length; i++) {
		ir_length = _get_cachedCompressedIRangesList_eltLength(
				cached_x, i);
		if (ir_length > max_ir_length)
			max_ir_length = ir_length;
	}
	return max_ir_length;
}

static int append_cachedIRanges_to_RangeAE(RangeAE *range_ae,
		const cachedIRanges *cached_ir)
{
	int ir_length, j, start, width;

	ir_length = _get_cachedIRanges_length(cached_ir);
	for (j = 0; j < ir_length; j++) {
		start = _get_cachedIRanges_elt_start(cached_ir, j);
		width = _get_cachedIRanges_elt_width(cached_ir, j);
		_RangeAE_insert_at(range_ae, _RangeAE_get_nelt(range_ae),
				start, width);
	}
	return ir_length;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_reduce(SEXP x, SEXP drop_empty_ranges,
		SEXP min_gapwidth)
{
	SEXP ans, ans_names, ans_unlistData,
	     ans_partitioning, ans_partitioning_end;
	cachedCompressedIRangesList cached_x;
	cachedIRanges cached_ir;
	int max_in_length, x_length, i;
	RangeAE in_ranges, out_ranges;
	IntAE tmpbuf;

	cached_x = _cache_CompressedIRangesList(x);
	max_in_length = get_cachedCompressedIRangesList_max_eltLengths(
				&cached_x);
	in_ranges = _new_RangeAE(0, 0);
	out_ranges = _new_RangeAE(0, 0);
	tmpbuf = _new_IntAE(max_in_length, 0, 0);
	x_length = _get_cachedCompressedIRangesList_length(&cached_x);
	PROTECT(ans_partitioning_end = NEW_INTEGER(x_length));
	for (i = 0; i < x_length; i++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_x, i);
		_RangeAE_set_nelt(&in_ranges, 0);
		append_cachedIRanges_to_RangeAE(&in_ranges, &cached_ir);
		_reduce_ranges(in_ranges.start.elts, in_ranges.width.elts,
			_RangeAE_get_nelt(&in_ranges),
			LOGICAL(drop_empty_ranges)[0], INTEGER(min_gapwidth)[0],
			tmpbuf.elts, &out_ranges, NULL);
		INTEGER(ans_partitioning_end)[i] = _RangeAE_get_nelt(&out_ranges);
	}
	PROTECT(ans_unlistData = _new_IRanges_from_RangeAE("IRanges",
			&out_ranges));
	PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
	PROTECT(ans_partitioning = _new_PartitioningByEnd(
			"PartitioningByEnd",
			ans_partitioning_end, ans_names));
	PROTECT(ans = _new_CompressedList(_get_classname(x),
			ans_unlistData, ans_partitioning));
	UNPROTECT(5);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_gaps(SEXP x, SEXP start, SEXP end)
{
	SEXP ans, ans_names, ans_unlistData,
	     ans_partitioning, ans_partitioning_end;
	cachedCompressedIRangesList cached_x;
	cachedIRanges cached_ir;
	int max_in_length, x_length, start_length, *start_elt, *end_elt, i;
	RangeAE in_ranges, out_ranges;
	IntAE tmpbuf;

	cached_x = _cache_CompressedIRangesList(x);
	max_in_length = get_cachedCompressedIRangesList_max_eltLengths(
				&cached_x);
	in_ranges = _new_RangeAE(0, 0);
	out_ranges = _new_RangeAE(0, 0);
	tmpbuf = _new_IntAE(max_in_length, 0, 0);
	x_length = _get_cachedCompressedIRangesList_length(&cached_x);
	start_length = LENGTH(start);
	if ((start_length != 1 && start_length != x_length) ||
		start_length != LENGTH(end))
        error("'start' and 'end' should both be integer vectors of length 1 or length(x)");
	PROTECT(ans_partitioning_end = NEW_INTEGER(x_length));
	start_elt = INTEGER(start);
	end_elt = INTEGER(end);
	for (i = 0; i < x_length; i++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_x, i);
		_RangeAE_set_nelt(&in_ranges, 0);
		append_cachedIRanges_to_RangeAE(&in_ranges, &cached_ir);
		_gaps_ranges(in_ranges.start.elts, in_ranges.width.elts,
			_RangeAE_get_nelt(&in_ranges),
			*start_elt, *end_elt,
			tmpbuf.elts, &out_ranges);
		INTEGER(ans_partitioning_end)[i] = _RangeAE_get_nelt(&out_ranges);
		if (start_length != 1) {
			start_elt++;
			end_elt++;
		}
	}
	PROTECT(ans_unlistData = _new_IRanges_from_RangeAE("IRanges",
			&out_ranges));
	PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
	PROTECT(ans_partitioning = _new_PartitioningByEnd(
			"PartitioningByEnd",
			ans_partitioning_end, ans_names));
	PROTECT(ans = _new_CompressedList(_get_classname(x),
			ans_unlistData, ans_partitioning));
	UNPROTECT(5);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedIRangesList_summary(SEXP object)
{
	int ans_len;
	SEXP part_end;
	SEXP ans, ans_names, col_names;

	part_end = _get_PartitioningByEnd_end(
			_get_CompressedList_partitioning(object));
	ans_len = LENGTH(part_end);
	PROTECT(ans = allocMatrix(INTSXP, ans_len, 2));
	memset(INTEGER(ans), 0, 2 * ans_len * sizeof(int));
	if (ans_len > 0) {
		int i, j, prev_end = 0;
		int *ans1_elt, *ans2_elt;
		const int *part_end_elt, *ranges_width;
		SEXP unlistData = _get_CompressedList_unlistData(object);
		ranges_width = INTEGER(_get_IRanges_width(unlistData));
		for (i = 0, ans1_elt = INTEGER(ans), ans2_elt = (INTEGER(ans) + ans_len),
			 part_end_elt = INTEGER(part_end);
		     i < ans_len; i++, ans1_elt++, ans2_elt++, part_end_elt++)
		{
			*ans1_elt = *part_end_elt - prev_end;
			for (j = 0; j < *ans1_elt; j++) {
				*ans2_elt += *ranges_width;
				ranges_width++;
			}
			prev_end = *part_end_elt;
		}
	}
	PROTECT(ans_names = NEW_LIST(2));
	PROTECT(col_names = NEW_CHARACTER(2));
	SET_STRING_ELT(col_names, 0, mkChar("Length"));
	SET_STRING_ELT(col_names, 1, mkChar("WidthSum"));
	SET_VECTOR_ELT(ans_names, 0,
			duplicate(_get_CompressedList_names(object)));
	SET_VECTOR_ELT(ans_names, 1, col_names);
	SET_DIMNAMES(ans, ans_names);
	UNPROTECT(3);
	return ans;
}


/****************************************************************************
 * CompressedNormalIRangesList methods.
 */

/* --- .Call ENTRY POINT --- */
SEXP CompressedNormalIRangesList_min(SEXP x, SEXP use_names)
{
	SEXP ans, ans_names;
	cachedCompressedIRangesList cached_x;
	cachedIRanges cached_ir;
	int x_length, ir_length, i;
	int *ans_elt;

	cached_x = _cache_CompressedIRangesList(x);
	x_length = _get_cachedCompressedIRangesList_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(x_length));
	for (i = 0, ans_elt = INTEGER(ans); i < x_length; i++, ans_elt++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_x, i);
		ir_length = _get_cachedIRanges_length(&cached_ir);
		if (ir_length == 0) {
			*ans_elt = INT_MAX;
		} else {
			*ans_elt = _get_cachedIRanges_elt_start(&cached_ir, 0);
		}
	}
	if (LOGICAL(use_names)[0]) {
		PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP CompressedNormalIRangesList_max(SEXP x, SEXP use_names)
{
	SEXP ans, ans_names;
	cachedCompressedIRangesList cached_x;
	cachedIRanges cached_ir;
	int x_length, ir_length, i;
	int *ans_elt;

	cached_x = _cache_CompressedIRangesList(x);
	x_length = _get_cachedCompressedIRangesList_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(x_length));
	for (i = 0, ans_elt = INTEGER(ans); i < x_length; i++, ans_elt++) {
		cached_ir = _get_cachedCompressedIRangesList_elt(&cached_x, i);
		ir_length = _get_cachedIRanges_length(&cached_ir);
		if (ir_length == 0) {
			*ans_elt = R_INT_MIN;
		} else {
			*ans_elt = _get_cachedIRanges_elt_end(&cached_ir, ir_length - 1);
		}
	}
	if (LOGICAL(use_names)[0]) {
		PROTECT(ans_names = duplicate(_get_CompressedList_names(x)));
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

