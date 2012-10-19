/****************************************************************************
 *                Range-wise comparison of 2 Ranges objects                 *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"


/****************************************************************************
 * Generalized comparison of 2 integer ranges.
 *
 * There are 13 different ways 2 integer ranges x and y can be positioned
 * with respect to each other. They are summarized in the following table
 * together with the codes we assign them:
 *
 *                     numeric code & |                   numeric code &
 *                    1-letter code & |                  1-letter code &
 *                          long code |                        long code
 *   ---------------  --------------- | ---------------  ---------------
 *   x: .oooo.......  -6   'a'  "x y" | x: .......oooo.   6   'm'  "y x"
 *   y: .......oooo.                  | y: .oooo.......                 
 *   ---------------  --------------- | ---------------  ---------------
 *   x: ..oooo......  -5   'b'  "xy"  | x: ......oooo..   5   'l'  "yx" 
 *   y: ......oooo..                  | y: ..oooo......                 
 *   ---------------  --------------- | ---------------  ---------------
 *   x: ...oooo.....  -4   'c'  "x=y" | x: .....oooo...   4   'k'  "y=x"
 *   y: .....oooo...                  | y: ...oooo.....                 
 *   ---------------  --------------- | ---------------  ---------------
 *   x: ...oooooo...  -3   'd'  "x="  | x: .....oooo...   3   'j'  "y=" 
 *   y: .....oooo...                  | y: ...oooooo...                 
 *   ---------------  --------------- | ---------------  ---------------
 *   x: ..oooooooo..  -2   'e'  "x=x" | x: ....oooo....   2   'i'  "y=y"
 *   y: ....oooo....                  | y: ..oooooooo..                 
 *   ---------------  --------------- | ---------------  ---------------
 *   x: ...oooo.....  -1   'f'  "=y"  | x: ...oooooo...   1   'h'  "=x" 
 *   y: ...oooooo...                  | y: ...oooo.....                 
 *   ---------------  ---------------------------------  ---------------
 *                 \   x: ...oooooo...   0   'g'  "="    /
 *                  \  y: ...oooooo...                  /
 *                   \---------------------------------/
 * Notes:
 *   o This way of comparing ranges is a refinement over the standard ranges
 *     comparison defined by the ==, !=, <=, >=, < and > operators. In
 *     particular a numeric code that is < 0, = 0, or > 0 corresponds to
 *     x < y, x == y, or x > y, respectively.
 *   o In this file we use the term "overlap" in a loose way even when there
 *     is actually no overlap between ranges x and y. Real overlaps correspond
 *     to numeric codes >= -4 and <= 4, and to long codes that contain an
 *     equal ("=").
 *   o Long codes are designed to be user-friendly whereas numeric and
 *     1-letter codes are designed to be more compact and memory efficient.
 *     Typically the formers will be exposed to the end-user and translated
 *     internally into the latters.
 *   o Swapping x and y changes the sign of the corresponding numeric code and
 *     substitutes "x" by "y" and "y" by "x" in the corresponding long code.
 *   o Reflecting ranges x and y relative to an arbitrary position (i.e. doing
 *     a symetry with respect to a vertical axis) has the effect of reversing
 *     the associated long code e.g. "x=y" becomes "y=x". The effect on the
 *     numeric code is implemented by the _invert_overlap_code() function.
 *
 * 'x_start', 'x_width', 'y_start' and 'y_width' are assumed to be non NA (not
 * checked). 'x_start' and 'y_start' must be 1-based. 'x_width' and 'y_width'
 * are assumed to be >= 0 (not checked).
 */
int _overlap_code(int x_start, int x_width, int y_start, int y_width)
{
	int x_end_plus1, y_end_plus1;

	x_end_plus1 = x_start + x_width;
	if (x_end_plus1 < y_start)
		return -6;
	if (x_end_plus1 == y_start) {
		if (x_width == 0 && y_width == 0)
			return 0;
		return -5;
	}
	y_end_plus1 = y_start + y_width;
	if (y_end_plus1 < x_start)
		return 6;
	if (y_end_plus1 == x_start)
		return 5;
	if (x_start < y_start) {
		if (x_end_plus1 < y_end_plus1)
			return -4;
		if (x_end_plus1 == y_end_plus1)
			return -3;
		return -2;
	}
	if (x_start == y_start) {
		if (x_end_plus1 < y_end_plus1)
			return -1;
		if (x_end_plus1 == y_end_plus1)
			return 0;
		return 1;
	}
	if (x_end_plus1 < y_end_plus1)
		return 2;
	if (x_end_plus1 == y_end_plus1)
		return 3;
	return 4;
}

int _invert_overlap_code(int code)
{
	if (code == -2 || code == 0 || code == 2)
		return code;
	if (code <= -4 || code >= 4)
		return - code;
	/* Only possible values left: -3, -1, 1, 3 */
	return code < 0 ? code + 4 : code - 4;
}

/* "Parallel" generalized comparison of 2 Ranges objects. */
static void ranges_pcompar(
		const int *x_start, const int *x_width, int x_len,
		const int *y_start, const int *y_width, int y_len,
		int *out, int out_len, int with_warning)
{
	int i, j, k;

	for (i = j = k = 0; k < out_len; i++, j++, k++) {
		if (i >= x_len)
			i = 0; /* recycle i */
		if (j >= y_len)
			j = 0; /* recycle j */
		out[k] = _overlap_code(x_start[i], x_width[i],
				       y_start[j], y_width[j]);
	}
	/* Warning message appropriate only when 'out_len' is
           'max(x_len, y_len)' */
	if (with_warning && out_len != 0 && (i != x_len || j != y_len))
		warning("longer object length is not a multiple "
			"of shorter object length");
	return;
}

/* --- .Call ENTRY POINT ---
 * 'x_start' and 'x_width': integer vectors of the same length M.
 * 'y_start' and 'y_width': integer vectors of the same length N.
 * If M != N then the shorter object is recycled to the length of the longer
 * object, except if M or N is 0 in which case the object with length != 0 is
 * truncated to length 0.
 * The 4 integer vectors are assumed to be NA free and 'x_width' and
 * 'y_width' are assumed to contain non-negative values. For efficiency
 * reasons, those assumptions are not checked.
 */
SEXP Ranges_compare(SEXP x_start, SEXP x_width,
		    SEXP y_start, SEXP y_width)
{
	int x_len, y_len, ans_len;
	const int *x_start_p, *x_width_p, *y_start_p, *y_width_p;
	SEXP ans;

	x_len = _check_integer_pairs(x_start, x_width,
				     &x_start_p, &x_width_p,
				     "start(x)", "width(x)");
	y_len = _check_integer_pairs(y_start, y_width,
				     &y_start_p, &y_width_p,
				     "start(y)", "width(y)");
	if (x_len == 0 || y_len == 0)
		ans_len = 0;
	else
		ans_len = x_len >= y_len ? x_len : y_len;
	PROTECT(ans = NEW_INTEGER(ans_len));
	ranges_pcompar(x_start_p, x_width_p, x_len,
		       y_start_p, y_width_p, y_len,
		       INTEGER(ans), ans_len, 1);
	UNPROTECT(1);
	return ans;
}

