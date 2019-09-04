/****************************************************************************
 *               Support functions for the IRanges constructor              *
 ****************************************************************************/
#include "IRanges.h"

#define	R_INT_MIN (INT_MIN + 1)
#define	R_INT_MAX INT_MAX

static char errmsg_buf[200];


/****************************************************************************
 * solve_user_SEW0()
 */

/* Return -1 if the range specified by 'start', 'end', and 'width' is
   invalid, and 0 otherwise. */
static int solve_range(int start, int end, int width,
		int *solved_start, int *solved_width)
{
	long long int tmp;
	static const char *too_many_NAs = "at least two out of 'start', "
					  "'end', and 'width', must\n  be "
					  "supplied";

	*solved_start = start;
	*solved_width = width;
	if (width == NA_INTEGER) {
		if (start == NA_INTEGER || end == NA_INTEGER) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "%s", too_many_NAs);
			return -1;
		}
		/* Compute and check 'width'. */
		tmp = (long long int) end - start + 1;
		if (tmp < 0) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "'end' must be >= 'start' - 1");
			return -1;
		}
		if (tmp > R_INT_MAX) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "the 'width' (%lld) inferred from the "
				 "supplied 'start'\n  and 'end' is too big "
				 "(>= 2^31)", tmp);
			return -1;
		}
		*solved_width = (int) tmp;
		return 0;
	}
	if (width < 0) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "negative widths are not allowed");
		return -1;
	}
	if (start == NA_INTEGER) {
		if (end == NA_INTEGER) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "%s", too_many_NAs);
			return -1;
		}
		/* Compute and check 'start'. */
		tmp = (long long int) end - width + 1;
		if (tmp < R_INT_MIN || tmp > R_INT_MAX) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "the 'start' (%lld) inferred from the "
				 "supplied 'end'\n  and 'width' is beyond "
				 "the limits of what is currently supported "
				 "(must\n  be > -2^31 and < 2^31 for now)",
				 tmp);
			return -1;
		}
		*solved_start = (int) tmp;
		return 0;
	}
	if (end == NA_INTEGER) {
		if (start == NA_INTEGER) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "%s", too_many_NAs);
			return -1;
		}
		/* Compute and check 'end'. */
		tmp = (long long int) start + width - 1;
		if (tmp < R_INT_MIN || tmp > R_INT_MAX) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "the 'end' (%lld) inferred from the "
				 "supplied 'start'\n  and 'width' is beyond "
				 "the limits of what is currently supported "
				 "(must\n  be > -2^31 and < 2^31 for now)",
				 tmp);
			return -1;
		}
		return 0;
	}
	tmp = (long long int) end - start + 1;
	if (width != tmp) {
		snprintf(errmsg_buf, sizeof(errmsg_buf),
			 "the supplied 'width' (%d) doesn't match "
			 "the width\n  inferred from the supplied "
			 "'start' and 'end' (%lld)", width, tmp);
		return -1;
	}
	return 0;
}

/* --- .Call ENTRY POINT ---
  'start' and 'width' can be used **as-is** to construct the IRanges object
  to return if they satisfy at least both criteria:
    (a) They don't have a "dim" or "names" attribute on them.
    (b) They don't contain NAs.
  Note that this just reflects what validObject() expects to see in the
  "start" and "width" slots of an IRanges object.
  If they can't be used **as-is** then they need to be modified (i.e. the
  names need to be removed and/or the NAs in them need to be resolved).
  This requires duplicating them first.
  Of course they also must define valid ranges, that is, after resolution
  of the NAs, the width must be >= 0 and < 2^31, the start must be > -2^31
  and < 2^31, and the implicit end must be > -2^31 and < 2^31. This is
  checked when resolving the NAs.
*/
SEXP solve_user_SEW0(SEXP start, SEXP end, SEXP width)
{
	int ans_len, use_start_as_is, use_width_as_is,
	    i, solved_start, solved_width,
		start_value, end_value, width_value;
	SEXP ans, ans_start, ans_width,
		check_start_NA, check_width_NA;

	//These functions might not only be called by constructor?
	if (!(IS_INTEGER(start) && IS_INTEGER(end) && IS_INTEGER(width)))
		error("the supplied 'start', 'end', and 'width', "
                      "must be integer vectors");
	ans_len = LENGTH(start);
	if (LENGTH(end) != ans_len || LENGTH(width) != ans_len)
		error("'start', 'end', and 'width' must have the same length");

	/*Prepare the expression to check any NA values in start and width
	  The function anyNA can be more efficient than a loop in C.*/
	PROTECT(check_start_NA = lang2(install("anyNA"), start));
	PROTECT(check_width_NA = lang2(install("anyNA"), width));

	use_start_as_is = GET_DIM(start) == R_NilValue &&
		GET_NAMES(start) == R_NilValue &&
		!asLogical(R_tryEval(check_start_NA, R_GlobalEnv, NULL));
	use_width_as_is = GET_DIM(width) == R_NilValue &&
		GET_NAMES(width) == R_NilValue&&
		!asLogical(R_tryEval(check_width_NA, R_GlobalEnv, NULL));

	ans_start = start;
	ans_width = width;
	if (!(use_start_as_is && use_width_as_is)) {
		/* Allocate and populate 'ans_start' and/or 'ans_width'. 
		   Call the duplicate function to duplicate an object for
		   an ALTREP might has a more efficient way to duplicate itself*/
		if (!use_start_as_is)
			PROTECT(ans_start = duplicate(start));
		if (!use_width_as_is)
			PROTECT(ans_width = duplicate(width));
		
		for (i = 0; i < ans_len; i++) {
			start_value = INTEGER_ELT(start, i);
			end_value = INTEGER_ELT(end, i);
			width_value = INTEGER_ELT(width, i);
			if (solve_range(start_value, end_value, width_value,
				&solved_start, &solved_width) != 0)
				error("In range %d: %s.", i + 1, errmsg_buf);
			/*The if statement is used to avoid unnecessary assignment*/
			if (!use_start_as_is)
				SET_INTEGER_ELT(ans_start, i, solved_start);
			if (!use_width_as_is)
				SET_INTEGER_ELT(ans_width, i, solved_width);
		}
	}
	PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width,
					      R_NilValue));
	UNPROTECT(3 + !use_start_as_is + !use_width_as_is);
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
	SEXP ans, ans_start, ans_width,
		check_start_NA, check_width_NA;
	int ans_len, i0, i1, i2, i3,
		refwidths_value, start_value, width_value, end_value,
		use_start_as_is, use_width_as_is,
		solved_start, solved_width;

	translate_negative_coord0 = LOGICAL(translate_negative_coord)[0];
	nonnarrowing_is_OK = LOGICAL(allow_nonnarrowing)[0];
	ans_len = LENGTH(refwidths);

	/*Prepare the expression to check any NA values in start and width
	  The function anyNA can be more efficient than a loop in C.*/
	PROTECT(check_start_NA = lang2(install("anyNA"), start));
	PROTECT(check_width_NA = lang2(install("anyNA"), width));

	/*Please see comments in solve_user_SEW0
	  The length should alse be check since it is not checked in R*/
	use_start_as_is = GET_DIM(start) == R_NilValue &&
		GET_NAMES(start) == R_NilValue &&
		!asLogical(R_tryEval(check_start_NA, R_GlobalEnv, NULL))&&
		LENGTH(start) == ans_len
		;
	use_width_as_is = GET_DIM(width) == R_NilValue &&
		GET_NAMES(width) == R_NilValue &&
		!asLogical(R_tryEval(check_width_NA, R_GlobalEnv, NULL))&&
		LENGTH(width) == ans_len;

	/*
	If user's input cannot be used `as-is`, depending on the vector's length:
	  1. If the length is the same as ans_len, the vector will be duplicated
	  2. If the length is different from ans_len, a new vector will be created.
	*/
	if (!use_start_as_is) {
		if (LENGTH(start) == ans_len) 
			PROTECT(ans_start = duplicate(start));
		else
			PROTECT(ans_start = NEW_INTEGER(ans_len));
	}
	else {
		ans_start = start;
	}

	if (!use_width_as_is) {
		if (LENGTH(width) == ans_len)
			PROTECT(ans_width = duplicate(width));
		else
			PROTECT(ans_width = NEW_INTEGER(ans_len));
	}
	else {
		ans_width = width;
	}


	for (i0 = i1 = i2 = i3 = 0; i0 < ans_len; i0++, i1++, i2++, i3++) {
		/* recycling */
		if (i1 >= LENGTH(start)) i1 = 0;
		if (i2 >= LENGTH(end)) i2 = 0;
		if (i3 >= LENGTH(width)) i3 = 0;

		refwidths_value = INTEGER_ELT(refwidths, i0);
		start_value = INTEGER_ELT(start, i1);
		end_value = INTEGER_ELT(end, i2);
		width_value = INTEGER_ELT(width, i3);

		if (solve_user_SEW_row(INTEGER(refwidths)[i0],
				       INTEGER(start)[i1],
				       INTEGER(end)[i2],
				       INTEGER(width)[i3],
			           solved_start,
			           solved_width
				       ) != 0)
		{
			UNPROTECT(2 + use_start_as_is + use_width_as_is);
			error("solving row %d: %s", i0 + 1, errmsg_buf);
		}
		if (!use_start_as_is)
			SET_INTEGER_ELT(ans_start, i0, solved_start);
		if (!use_width_as_is)
			SET_INTEGER_ELT(ans_width, i0, solved_width);
	}
	PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width, R_NilValue));
	UNPROTECT(3 + use_start_as_is + use_width_as_is);
	return ans;
}

