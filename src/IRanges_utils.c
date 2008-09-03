/****************************************************************************
 *                          Fast IRanges utilities                          *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_IRanges_utils()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in 'IRanges_utils.c'\n",
	        debug ? "on" : "off");
#else
	Rprintf("Debug mode not available in 'IRanges_utils.c'\n");
#endif
	return R_NilValue;
}

static void normargs_startend(int *start, int *end, int width, const char *prefix)
{
	if (*start == 0)
		error("'%sstart' must be a single >= 1, <= -1 "
		      "or NA integer", prefix);
	if (*end == 0)
		error("'%send' must be a single >= 1, <= -1 "
		      "or NA integer", prefix);
	if (width == NA_INTEGER) {
		if (*start == NA_INTEGER)
			*start = 1;
		if (*end == NA_INTEGER)
			*end = -1;
		if ((*end > 0 || *start < 0) && *end < *start)
			error("invalid ('%sstart','%send') combination",
			      prefix, prefix);
	} else if (width < 0) {
		error("'%swidth' must be a single >= 0 or NA integer", prefix);
	} else if ((*start == NA_INTEGER) == (*end == NA_INTEGER)) {
		error("either '%sstart' or '%send' (but not both) must be NA "
		      "when '%swidth' is not NA", prefix, prefix, prefix);
	} else if (*start == NA_INTEGER) {
		// '*end' is not NA
		if (0 < *end && *end < width)
			error("invalid ('%send','%swidth') combination",
			      prefix, prefix);
		// '*start' will be 0 iff '*end' = -1 and 'width' = 0 
		*start = *end - width + 1;
	} else {
		// '*end' is NA
		if (*start < 0 && -*start < width)
			error("invalid ('%sstart','%swidth') combination",
			      prefix, prefix);
		// '*end' will be 0 iff '*start' = 1 and 'width' = 0
		*end = *start + width - 1;
	}
	// '*start' and '*end' cannot be NA anymore!
	return;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP narrow_IRanges(SEXP x, SEXP start, SEXP end, SEXP width)
{
	int start0, end0, x_length, i, *new_start, *new_width, shift1, shift2, start_length;
	const int *old_start, *old_width;
	SEXP ans_start, ans_width, ans, ans_names;

	x_length = _get_IRanges_length(x);
	PROTECT(ans_start = NEW_INTEGER(x_length));
	PROTECT(ans_width = NEW_INTEGER(x_length));
	start_length = LENGTH(start);
	if (start_length == 1) {
		start0 = INTEGER(start)[0];
		end0 = INTEGER(end)[0];
		normargs_startend(&start0, &end0, INTEGER(width)[0], "");
		for (i = 0, old_start = _get_IRanges_start0(x),
			    old_width = _get_IRanges_width0(x),
			    new_start = INTEGER(ans_start),
			    new_width = INTEGER(ans_width);
		     i < x_length;
		     i++, old_start++, old_width++, new_start++, new_width++)
		{
			if (start0 > 0)
				shift1 = start0 - 1;
			else
				shift1 = start0 + *old_width;
			if (end0 < 0)
				shift2 = end0 + 1;
			else
				shift2 = end0 - *old_width;
			*new_width = *old_width - shift1 + shift2;
			if (shift1 < 0 || shift2 > 0 || *new_width < 0) {
				UNPROTECT(2);
				error("width of range %d is too small (%d) for this narrowing",
				      i + 1, *old_width);
			}
			*new_start = *old_start + shift1;
		}
	} else {
		for (i = 0, old_start = _get_IRanges_start0(x),
			    old_width = _get_IRanges_width0(x),
			    new_start = INTEGER(ans_start),
			    new_width = INTEGER(ans_width);
		     i < x_length;
		     i++, old_start++, old_width++, new_start++, new_width++)
		{
			start0 = INTEGER(start)[i];
			end0 = INTEGER(end)[i];
			normargs_startend(&start0, &end0, INTEGER(width)[i], "");
			if (start0 > 0)
				shift1 = start0 - 1;
			else
				shift1 = start0 + *old_width;
			if (end0 < 0)
				shift2 = end0 + 1;
			else
				shift2 = end0 - *old_width;
			*new_width = *old_width - shift1 + shift2;
			if (shift1 < 0 || shift2 > 0 || *new_width < 0) {
				UNPROTECT(2);
				error("width of range %d is too small (%d) for this narrowing",
				      i + 1, *old_width);
			}
			*new_start = *old_start + shift1;
		}
	}
	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_NAMES(ans, ans_names);
	SET_ELEMENT(ans, 0, ans_start);
	SET_ELEMENT(ans, 1, ans_width);
	UNPROTECT(4);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP int_to_adjacent_ranges(SEXP x)
{
	SEXP ans;
	int x_length, i, *x_elt, *ans_elt0, *ans_elt1;

	x_length = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(x_length));
	if (x_length >= 1)
		INTEGER(ans)[0] = 1;
	if (x_length >= 2)
		for (i = 1, x_elt = INTEGER(x),
			    ans_elt0 = INTEGER(ans),
			    ans_elt1 = INTEGER(ans)+1;
		     i < x_length;
		     i++, x_elt++, ans_elt0++, ans_elt1++) {
			*ans_elt1 = *ans_elt0 + *x_elt;
		}
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP which_as_ranges(SEXP x)
{
	SEXP ans, start, width;
	int i, x_length, ans_length, *x_elt, prev_elt, *start_elt, *width_elt;

	x_length = LENGTH(x);
	ans_length = 0;
	prev_elt = 0;
	for (i = 1, x_elt = LOGICAL(x); i <= x_length; i++, x_elt++) {
		if (*x_elt && !prev_elt)
			ans_length++;
		prev_elt = *x_elt;
	}

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("NormalIRanges")));
	PROTECT(start = NEW_INTEGER(ans_length));
	PROTECT(width = NEW_INTEGER(ans_length));
	SET_SLOT(ans, mkChar("start"), start);
	SET_SLOT(ans, mkChar("width"), width);
	if (ans_length > 0) {
		start_elt = INTEGER(start) - 1;
		width_elt = INTEGER(width) - 1;
		prev_elt = 0;
		for (i = 1, x_elt = LOGICAL(x); i <= x_length; i++, x_elt++) {
			if (*x_elt) {
				if (prev_elt)
					*width_elt += 1;
				else {
					start_elt++;
					width_elt++;
					*start_elt = i;
					*width_elt = 1;
				}
			}
			prev_elt = *x_elt;
		}
	}
	UNPROTECT(3);
	return ans;
}

/****************************************************************************
 * Reduction (aka extracting the frame)
 */

static RangeAE reduced_ranges;
static int max_end, inframe_offset;

static void add_to_reduced_ranges(int start, int width)
{
	int buf_length, end, gap;

	buf_length = reduced_ranges.start.nelt;
	end = start + width - 1;
	if (buf_length == 0 || (gap = start - max_end - 1) > 0) {
		_RangeAE_insert_at(&reduced_ranges, buf_length, start, width);
		if (buf_length == 0)
			inframe_offset = start - 1;
		else
			inframe_offset += gap;
		max_end = end;
		return;
	}
	if (end <= max_end)
		return;
	reduced_ranges.width.elts[buf_length - 1] += end - max_end;
	max_end = end;
	return;
}

static void reduce_ranges(int length, const int *start, const int *width, int *inframe_start)
{
	int i, j;
	IntAE start_order;

	start_order = _new_IntAE(length, 0, 0);
	_get_int_array_order(start, length, start_order.elts);
	reduced_ranges = _new_RangeAE(0, 0);
	for (i = 0; i < length; i++) {
		j = start_order.elts[i];
		add_to_reduced_ranges(start[j], width[j]);
		if (inframe_start != NULL)
			inframe_start[j] = start[j] - inframe_offset;
	}
	return;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP reduce_IRanges(SEXP x, SEXP with_inframe_start)
{
	int x_length;
	const int *x_start, *x_width;
	SEXP ans, ans_names, ans_inframe_start;
	int *inframe_start;

	x_length = _get_IRanges_length(x);
	x_start = _get_IRanges_start0(x);
	x_width = _get_IRanges_width0(x);
	if (LOGICAL(with_inframe_start)[0]) {
		PROTECT(ans_inframe_start = NEW_INTEGER(x_length));
		inframe_start = INTEGER(ans_inframe_start);
	} else {
		inframe_start = NULL;
	}
	reduce_ranges(x_length, x_start, x_width, inframe_start);

	PROTECT(ans = NEW_LIST(3));
	PROTECT(ans_names = NEW_CHARACTER(3));
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("width"));
	SET_STRING_ELT(ans_names, 2, mkChar("inframe.start"));
	SET_NAMES(ans, ans_names);
	UNPROTECT(1);
	SET_ELEMENT(ans, 0, _IntAE_asINTEGER(&(reduced_ranges.start)));
	SET_ELEMENT(ans, 1, _IntAE_asINTEGER(&(reduced_ranges.width)));
	if (inframe_start != NULL) {
		SET_ELEMENT(ans, 2, ans_inframe_start);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP summary_IRanges_list(SEXP x)
{
	int x_len, *ans1_elt, *ans2_elt, i, j;
	const int *x_elt_width;
	SEXP x_elt, ans, ans_names, col_names;

	x_len = LENGTH(x);
	PROTECT(ans = allocMatrix(INTSXP, x_len, 2));
	memset(INTEGER(ans), 0, 2 * x_len * sizeof(int));
	for (i = 0, ans1_elt = INTEGER(ans), ans2_elt = INTEGER(ans) + x_len;
	     i < x_len;
	     i++, ans1_elt++, ans2_elt++)
	{
		x_elt = VECTOR_ELT(x, i);
		*ans1_elt = LENGTH(_get_IRanges_width(x_elt));
		for (j = 0, x_elt_width = _get_IRanges_width0(x_elt);
		     j < *ans1_elt;
		     j++, x_elt_width++)
		{
			*ans2_elt += *x_elt_width;
		}
	}
	PROTECT(ans_names = NEW_LIST(2));
	PROTECT(col_names = NEW_CHARACTER(2));
	SET_STRING_ELT(col_names, 0, mkChar("Length"));
	SET_STRING_ELT(col_names, 1, mkChar("WidthSum"));
	SET_ELEMENT(ans_names, 0, R_NilValue);
	SET_ELEMENT(ans_names, 1, col_names);
	SET_DIMNAMES(ans, ans_names);
	UNPROTECT(3);
	return ans;
}

