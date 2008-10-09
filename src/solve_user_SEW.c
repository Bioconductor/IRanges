/****************************************************************************
 *                             solve_user_SEW()                             *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

#define TRANSLATE_IFNONPOSITIVE	1
#define KEEP_IFNONPOSITIVE	2
#define ERROR_IFNONPOSITIVE	3

static int ifnonpositive_action;
static char errmsg_buf[200];

static void set_ifnonpositive_action(const char *if_nonpositive_startorend)
{
	if (strcmp(if_nonpositive_startorend, "translate") == 0)
		ifnonpositive_action = TRANSLATE_IFNONPOSITIVE;
	else if (strcmp(if_nonpositive_startorend, "keep") == 0)
		ifnonpositive_action = KEEP_IFNONPOSITIVE;
	else if (strcmp(if_nonpositive_startorend, "error") == 0)
		ifnonpositive_action = ERROR_IFNONPOSITIVE;
	else
		error("invalid 'if_nonpositive_startorend' value %s",
		      if_nonpositive_startorend);
	return;
}

static int translate_nonpositive_startorend(int refwidth, int *startorend,
		const char *argname)
{
	if (ifnonpositive_action == KEEP_IFNONPOSITIVE || *startorend > 0)
		return 0;
	if (ifnonpositive_action == ERROR_IFNONPOSITIVE) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "non-positive values are not allowed in '%s'",
			 argname);
		return -1;
	}
	// From here, 'ifnonpositive_action' is TRANSLATE_IFNONPOSITIVE
	if (*startorend == 0) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
		         "0s are not allowed in '%s'", argname);
		return -1;
	}
	*startorend += refwidth + 1;
	return 0;
}

static int check_start(int refwidth, int start, const char *what)
{
	if (ifnonpositive_action == KEEP_IFNONPOSITIVE)
		return 0;
	if (start < 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "the %s start (%d) is < 1", what, start);
		return -1;
	}
	if (start > refwidth + 1) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "the %s start (%d) is > refwidth + 1", what, start);
		return -1;
	}
	return 0;
}

static int check_end(int refwidth, int end, const char *what)
{
	if (ifnonpositive_action == KEEP_IFNONPOSITIVE)
		return 0;
	if (end < 0) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "the %s end (%d) is < 0", what, end);
		return -1;
	}
	if (end > refwidth) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "the %s end (%d) is > refwidth", what, end);
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
		if (translate_nonpositive_startorend(refwidth, &start, "start") != 0)
			return -1;
		if (check_start(refwidth, start, "supplied") != 0)
			return -1;
	}
	if (end != NA_INTEGER) {
		if (translate_nonpositive_startorend(refwidth, &end, "end") != 0)
			return -1;
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
				 "the supplied start and end lead to a "
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
			 "(but not both) must be NA when the width is not NA");
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
		SEXP if_nonpositive_startorend)
{
	SEXP ans, ans_start, ans_width;
	int ans_length, i0, i1, i2, i3;

	set_ifnonpositive_action(CHAR(STRING_ELT(if_nonpositive_startorend, 0)));
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

