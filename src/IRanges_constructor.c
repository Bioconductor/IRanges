/****************************************************************************
 *               Support functions for the IRanges constructor              *
 ****************************************************************************/
#include "IRanges.h"


static char errmsg_buf[200];


/****************************************************************************
 * solve_user_SEW0()
 */

static int solve_user_SEW0_row(int start, int end, int width,
		int *solved_start, int *solved_width)
{
	int nb_of_unknowns;

	nb_of_unknowns = (start == NA_INTEGER) + (end == NA_INTEGER) + (width == NA_INTEGER);
	if (nb_of_unknowns >= 2) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "range cannot be determined from the supplied arguments (too many NAs)");
		return -1;
	}
	if (start == NA_INTEGER) {
		start = end - width + 1;
	} else if (width == NA_INTEGER) {
		width = end - start + 1;
	} else if (end != NA_INTEGER && end != start + width - 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "supplied arguments are incompatible");
		return -1;
	}
	if (width < 0) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "negative widths are not allowed");
		return -1;
	}
	*solved_start = start;
	*solved_width = width;
	return 0;
}

SEXP solve_user_SEW0(SEXP start, SEXP end, SEXP width)
{
	SEXP ans, ans_start, ans_width;
	int ans_length, i;

	ans_length = LENGTH(start);
	PROTECT(ans_start = NEW_INTEGER(ans_length));
	PROTECT(ans_width = NEW_INTEGER(ans_length));
	for (i = 0; i < ans_length; i++) {
		if (solve_user_SEW0_row(INTEGER(start)[i],
					INTEGER(end)[i],
					INTEGER(width)[i],
					INTEGER(ans_start) + i,
					INTEGER(ans_width) + i) != 0)
		{
			UNPROTECT(2);
			error("solving row %d: %s", i + 1, errmsg_buf);
		}
	}
	PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width, R_NilValue));
	UNPROTECT(3);
	return ans;
}


/****************************************************************************
 * solve_user_SEW()
 */

static int translate_negative_coord0;
static int nonnarrowing_is_OK;

static int translate_negative_startorend(int refwidth, int startorend)
{
	if (startorend < 0)
		startorend += refwidth + 1;
	return startorend;
}

static int check_start(int refwidth, int start, const char *what)
{
	if (nonnarrowing_is_OK)
		return 0;
	if (start < 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'allow.nonnarrowing' is FALSE and the %s start "
			 "(%d) is < 1", what, start);
		return -1;
	}
	if (start > refwidth + 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'allow.nonnarrowing' is FALSE and the %s start "
			 "(%d) is > refwidth + 1", what, start);
		return -1;
	}
	return 0;
}

static int check_end(int refwidth, int end, const char *what)
{
	if (nonnarrowing_is_OK)
		return 0;
	if (end < 0) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'allow.nonnarrowing' is FALSE and the %s end "
			 "(%d) is < 0", what, end);
		return -1;
	}
	if (end > refwidth) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "'allow.nonnarrowing' is FALSE and the %s end "
			 "(%d) is > refwidth", what, end);
		return -1;
	}
	return 0;
}

static int solve_user_SEW_row(int refwidth, int start, int end, int width,
		int *solved_start, int *solved_width)
{
	if (refwidth == NA_INTEGER || refwidth < 0) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "negative values or NAs are not allowed in 'refwidths'");
		return -1;
	}
	if (start != NA_INTEGER) {
		if (translate_negative_coord0)
			start = translate_negative_startorend(refwidth, start);
		if (check_start(refwidth, start, "supplied") != 0)
			return -1;
	}
	if (end != NA_INTEGER) {
		if (translate_negative_coord0)
			end = translate_negative_startorend(refwidth, end);
		if (check_end(refwidth, end, "supplied") != 0)
			return -1;
	}
	if (width == NA_INTEGER) {
		if (start == NA_INTEGER)
			start = 1;
		if (end == NA_INTEGER)
			end = refwidth;
		width = end - start + 1;
		if (width < 0) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "the supplied start/end lead to a "
				 "negative width");
			return -1;
		}
	} else if (width < 0) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "negative values are not allowed in 'width'");
		return -1;
	} else if ((start == NA_INTEGER) == (end == NA_INTEGER)) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "either the supplied start or the supplied end "
			 "(but not both) must be NA when the supplied width "
			 "is not NA");
		return -1;
	} else {
		// Either 'start' or 'end' is NA
		if (start == NA_INTEGER) {
			start = end - width + 1;
			if (check_start(refwidth, start, "solved") != 0)
				return -1;
		} else {
			end = start + width - 1;
			if (check_end(refwidth, end, "solved") != 0)
				return -1;
		}
	}
	*solved_start = start;
	*solved_width = width;
	return 0;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP solve_user_SEW(SEXP refwidths, SEXP start, SEXP end, SEXP width,
		SEXP translate_negative_coord, SEXP allow_nonnarrowing)
{
	SEXP ans, ans_start, ans_width;
	int ans_length, i0, i1, i2, i3;

	translate_negative_coord0 = LOGICAL(translate_negative_coord)[0];
	nonnarrowing_is_OK = LOGICAL(allow_nonnarrowing)[0];
	ans_length = LENGTH(refwidths);
	PROTECT(ans_start = NEW_INTEGER(ans_length));
	PROTECT(ans_width = NEW_INTEGER(ans_length));
	for (i0 = i1 = i2 = i3 = 0; i0 < ans_length; i0++, i1++, i2++, i3++) {
		/* recycling */
		if (i1 >= LENGTH(start)) i1 = 0; 
		if (i2 >= LENGTH(end)) i2 = 0; 
		if (i3 >= LENGTH(width)) i3 = 0; 
		if (solve_user_SEW_row(INTEGER(refwidths)[i0],
				       INTEGER(start)[i1],
				       INTEGER(end)[i2],
				       INTEGER(width)[i3],
				       INTEGER(ans_start) + i0,
				       INTEGER(ans_width) + i0) != 0)
		{
			UNPROTECT(2);
			error("solving row %d: %s", i0 + 1, errmsg_buf);
		}
	}
	PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width, R_NilValue));
	UNPROTECT(3);
	return ans;
}

