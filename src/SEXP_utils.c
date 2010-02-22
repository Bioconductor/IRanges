#include "IRanges.h"


const char *_get_classname(SEXP x)
{
	return CHAR(STRING_ELT(GET_CLASS(x), 0));
}

/*
 * --- .Call ENTRY POINT ---
 * From R:
 *   .Call("address_asSTRSXP", 6:4, PACKAGE="IRanges")
 *   .Call("address_asSTRSXP", new("externalptr"), PACKAGE="IRanges")
 */
SEXP address_asSTRSXP(SEXP s)
{
	char buf[40]; /* should be enough, even for 128-bit addresses */

	snprintf(buf, sizeof(buf), "%p", s);
	return mkString(buf);
}

/*
 * --- .Call ENTRY POINT ---
 * A fast implementation of 'sapply(x, length)' that works only on a list of
 * vectors (or NULLs).
 */
SEXP listofvectors_lengths(SEXP x)
{
	SEXP ans, x_elt;
	int n = LENGTH(x);

	PROTECT(ans = NEW_INTEGER(n));
	for (int i = 0; i < n; i++) {
		x_elt = VECTOR_ELT(x, i);
		if (x_elt == R_NilValue) {
			INTEGER(ans)[i] = 0;
			continue;
		}
		if (!IS_VECTOR(x_elt))
			error("element %d not a vector (or NULL)", i + 1);
		INTEGER(ans)[i] = LENGTH(x_elt);
	}
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 * any(is.na(x) | x < lower | x > upper)
 */
SEXP Integer_any_missing_or_outside(SEXP x, SEXP lower, SEXP upper)
{
	  SEXP ans;
	  int i, n, *x_ptr, lower_value, upper_value;

	  n = length(x);
	  lower_value = INTEGER(lower)[0];
	  upper_value = INTEGER(upper)[0];

	  PROTECT(ans = NEW_LOGICAL(1));
	  LOGICAL(ans)[0] = 0;
      for (i = 0, x_ptr = INTEGER(x); i < n; i++, x_ptr++) {
        if ((*x_ptr == NA_INTEGER) ||
        	(*x_ptr < lower_value) ||
        	(*x_ptr > upper_value)) {
          LOGICAL(ans)[0] = 1;
          break;
        }
      }
      UNPROTECT(1);
      return(ans);
}

/*
 * --- .Call ENTRY POINT ---
 * diff(c(0L, x))
 */
SEXP Integer_diff_with_0(SEXP x)
{
	int i, len, *x_ptr1, *x_ptr2, *ans_ptr;
	SEXP ans;

	len = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(len));
	if (len > 0) {
		INTEGER(ans)[0] = INTEGER(x)[0];
		if (len > 1) {
			for (i = 1, x_ptr1 = INTEGER(x), x_ptr2 = INTEGER(x) + 1,
				 ans_ptr = INTEGER(ans) + 1; i < len;
				 i++, x_ptr1++, x_ptr2++, ans_ptr++) {
				*ans_ptr = *x_ptr2 - *x_ptr1;
			}
		}
	}
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP Integer_order(SEXP x, SEXP decreasing)
{
	int ans_length;
	SEXP ans;

	ans_length = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	_get_order_of_int_array(INTEGER(x), ans_length,
			LOGICAL(decreasing)[0], INTEGER(ans), 1);
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP Integer_order_two(SEXP x, SEXP y, SEXP decreasing)
{
	int ans_length;
	SEXP ans;

	ans_length = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	_get_order_of_two_int_arrays(INTEGER(x), INTEGER(y),
		ans_length, LOGICAL(decreasing)[0], INTEGER(ans), 1);
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 * Creates the (sorted) union of two sorted integer vectors
 */
SEXP Integer_sorted_merge(SEXP x, SEXP y)
{
	int x_i, y_i, x_len, y_len, ans_len;
	const int *x_ptr, *y_ptr;
	int *ans_ptr;
	SEXP ans;

	x_len = LENGTH(x);
	y_len = LENGTH(y);

	x_i = 0;
	y_i = 0;
	x_ptr = INTEGER(x);
	y_ptr = INTEGER(y);
	ans_len = 0;
	while (x_i < x_len && y_i < y_len) {
		if (*x_ptr == *y_ptr) {
			x_ptr++;
			x_i++;
			y_ptr++;
			y_i++;
		} else if (*x_ptr < *y_ptr) {
			x_ptr++;
			x_i++;
		} else {
			y_ptr++;
			y_i++;
		}
		ans_len++;
	}
	if (x_i < x_len) {
		ans_len += x_len - x_i;
	} else if (y_i < y_len) {
		ans_len += y_len - y_i;
	}

	PROTECT(ans = NEW_INTEGER(ans_len));
	x_i = 0;
	y_i = 0;
	x_ptr = INTEGER(x);
	y_ptr = INTEGER(y);
	ans_ptr = INTEGER(ans);
	while (x_i < x_len && y_i < y_len) {
		if (*x_ptr == *y_ptr) {
			*ans_ptr = *x_ptr;
			x_ptr++;
			x_i++;
			y_ptr++;
			y_i++;
		} else if (*x_ptr < *y_ptr) {
			*ans_ptr = *x_ptr;
			x_ptr++;
			x_i++;
		} else {
			*ans_ptr = *y_ptr;
			y_ptr++;
			y_i++;
		}
		ans_ptr++;
	}
	if (x_i < x_len) {
		memcpy(ans_ptr, x_ptr, (x_len - x_i) * sizeof(int));
	} else if (y_i < y_len) {
		memcpy(ans_ptr, y_ptr, (y_len - y_i) * sizeof(int));
	}
	UNPROTECT(1);

	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP Integer_mseq(SEXP from, SEXP to)
{
	int i, j, n, ans_length, *from_elt, *to_elt, *ans_elt;
	SEXP ans;

	if (!IS_INTEGER(from) || !IS_INTEGER(to))
		error("'from' and 'to' must be integer vectors");

	n = LENGTH(from);
	if (n != LENGTH(to))
		error("lengths of 'from' and 'to' must be equal");

	ans_length = 0;
	for (i = 0, from_elt = INTEGER(from), to_elt = INTEGER(to); i < n;
		 i++, from_elt++, to_elt++) {
		ans_length += *to_elt - *from_elt + 1;
	}

	PROTECT(ans = NEW_INTEGER(ans_length));
	ans_elt = INTEGER(ans);
	for (i = 0, from_elt = INTEGER(from), to_elt = INTEGER(to); i < n;
		 i++, from_elt++, to_elt++) {
		if (*from_elt == NA_INTEGER || *to_elt == NA_INTEGER)
			error("'from' and 'to' contain NAs");

		if (*from_elt <= *to_elt) {
			for (j = *from_elt; j <= *to_elt; j++) {
				*ans_elt = j;
				ans_elt++;
			}
		} else {
			for (j = *from_elt; j >= *to_elt; j--) {
				*ans_elt = j;
				ans_elt++;
			}
		}
	}
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 * a faster version of which
 */
SEXP Logical_whichAsVector(SEXP x)
{
	SEXP ans;
	int i, x_length, buf_length, ans_length;
	int *x_elt, *buf, *buf_elt;

	if (!IS_LOGICAL(x))
		error("argument to 'which' is not logical");

	x_length = LENGTH(x);
	buf_length = x_length;
	buf = (int *) R_alloc((long) buf_length, sizeof(int));
	buf_elt = buf;
	ans_length = 0;
	for (i = 0, x_elt = LOGICAL(x); i < x_length; i++, x_elt++) {
		if (*x_elt && (*x_elt != NA_LOGICAL)) {
			*buf_elt = i + 1;
			buf_elt++;
			ans_length++;
		}
	}
	PROTECT(ans = NEW_INTEGER(ans_length));
	memcpy(INTEGER(ans), buf, sizeof(int) * ans_length);
	UNPROTECT(1);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 * findIntervalAndStartFromWidth for when x and width are integer vectors
 */

SEXP findIntervalAndStartFromWidth(SEXP x, SEXP width)
{
	int i, x_len, width_len, interval, start;
	const int *x_elt, *width_elt;
	int *interval_elt, *start_elt;
	SEXP ans, ans_class, ans_names, ans_rownames, ans_interval, ans_start;

	if (!IS_INTEGER(x))
		error("'x' must be an integer vector");
	if (!IS_INTEGER(width))
		error("'width' must be an integer vector");

	x_len = LENGTH(x);
	width_len = LENGTH(width);
	width_elt = INTEGER(width);
	ans_rownames = R_NilValue;
	PROTECT(ans_interval = NEW_INTEGER(x_len));
	PROTECT(ans_start = NEW_INTEGER(x_len));
	if (x_len > 0 && width_len > 0) {
		start = 1;
		interval = 1;
		for (i = 0, x_elt = INTEGER(x), interval_elt = INTEGER(ans_interval),
			 start_elt = INTEGER(ans_start); i < x_len;
		     i++, x_elt++, interval_elt++, start_elt++) {
			if (*x_elt == NA_INTEGER)
				error("'x' cannot contain missing values");
			else if (*x_elt < 0)
				error("'x' must contain non-negative values");
			if (*x_elt == 0) {
				*interval_elt = 0;
				*start_elt = NA_INTEGER;
			} else {
				while (interval > 1 && *x_elt < start) {
					interval--;
					width_elt--;
					start -= *width_elt;
				}
				while (interval < width_len && *x_elt >= (start + *width_elt)) {
					interval++;
					start += *width_elt;
					width_elt++;
				}
				if (*x_elt > start + *width_elt)
					error("'x' must be less than 'sum(width)'");
				*interval_elt = interval;
				*start_elt = start;
			}
		}
		PROTECT(ans_rownames = NEW_INTEGER(2));
		INTEGER(ans_rownames)[0] = NA_INTEGER;
		INTEGER(ans_rownames)[1] = -x_len;
	} else {
		PROTECT(ans_rownames = NEW_INTEGER(0));
	}

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_class = NEW_CHARACTER(1));
	PROTECT(ans_names = NEW_CHARACTER(2));

	SET_STRING_ELT(ans_class, 0, mkChar("data.frame"));
	SET_STRING_ELT(ans_names, 0, mkChar("interval"));
	SET_STRING_ELT(ans_names, 1, mkChar("start"));

	SET_NAMES(ans, ans_names);
	SET_VECTOR_ELT(ans, 0, ans_interval);
	SET_VECTOR_ELT(ans, 1, ans_start);
	setAttrib(ans, install("row.names"), ans_rownames);
	SET_CLASS(ans, ans_class);

	UNPROTECT(6);

	return ans;
}
