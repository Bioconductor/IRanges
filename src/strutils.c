#include "IRanges.h"

#include <limits.h> /* for UINT_MAX and UINT_MIN */
#include <ctype.h> /* for isblank() and isdigit() */


/*
 * --- .Call ENTRY POINT ---
 * We cannot rely on the strsplit() R function to split a string into single
 * characters when the string contains junk. For example:
 *   > r <- as.raw(c(10, 255))
 *   > s <- rawToChar(r)
 *   > s
 *   [1] "\n\xff"
 *   > strsplit(s, NULL, fixed=TRUE)[[1]]
 *   [1] NA
 * doesn't work!
 * The function below should be safe, whatever the content of 's' is!
 * The length of the returned string is the number of chars in single
 * string 's'. Not vectorized.
 */
SEXP safe_strexplode(SEXP s)
{
	SEXP s0, ans;
	int s0_length, i;
	char buf[2] = "X"; /* we only care about having buf[1] == 0 */

	s0 = STRING_ELT(s, 0);
	s0_length = LENGTH(s0);

	PROTECT(ans = NEW_CHARACTER(s0_length));
	for (i = 0; i < s0_length; i++) {
		buf[0] = CHAR(s0)[i];
	SET_STRING_ELT(ans, i, mkChar(buf));
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * strsplit_asIntList()
 *
 * Similar to
 *   tmp <- strsplit(x, sep, fixed=TRUE)
 *   lapply(tmp, as.integer)
 * except that:
 *   - strsplit() accepts NAs, we don't (raise an error);
 *   - as.integer() introduces NAs by coercion (with a warning), we don't
 *     (raise an error);
 *   - as.integer() supports "inaccurate integer conversion in coercion"
 *     when the value to coerce is > INT_MAX (then it's coerced to INT_MAX),
 *     we don't (raise an error);
 *   - as.integer() will coerce non-integer values (e.g. 10.3) to an int
 *     by truncating them, we don't (raise an error).
 * When it fails, strsplit_asIntList() will print a detailed parse error
 * message.
 */

static char errmsg_buf[200];

static SEXP parse_string_as_integer_vector(SEXP s, const char *format_buf)
{
	const char *str;
	int ans_length, offset, n, ret, j;
	long int val;
	SEXP ans;

	str = CHAR(s);
	ans_length = offset = 0;
	while (1) {
		n = 0;
		ret = sscanf(str + offset, "%*d%n", &n);
		if (ret == EOF)
			break;
		if (n == 0) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "decimal integer expected at char %d",
				 offset + 1);
			return R_NilValue;
		}
		offset += n;
		while (isblank(str[offset])) offset++;
		ans_length++;
		n = 0;
		ret = sscanf(str + offset, format_buf, &n);
		if (ret == EOF)
			break;
		if (n == 0) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "separator expected at char %d",
				 offset + 1);
			return R_NilValue;
		}
		offset += n;
	}
	PROTECT(ans = NEW_INTEGER(ans_length));
	offset = 0;
	for (j = 0; j < ans_length; j++) {
		ret = sscanf(str + offset, "%ld%n", &val, &n);
		offset += n;
		while (isblank(str[offset])) offset++;
		if (val < INT_MIN || val > INT_MAX) {
			UNPROTECT(1);
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "out of range integer at char %d",
				 offset + 1);
			return R_NilValue;
		}
		INTEGER(ans)[j] = (int) val;
		ret = sscanf(str + offset, format_buf, &n);
		offset += n;
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP strsplit_asIntList(SEXP x, SEXP sep)
{
	SEXP ans, x_elt, ans_elt;
	int ans_length, i;
	char sep0;
	static char format_buf[5];

	ans_length = LENGTH(x);
	sep0 = CHAR(STRING_ELT(sep, 0))[0];
	if (isdigit(sep0))
		error("'sep' cannot be a digit");
	if (sep0 != '%')
		sprintf(format_buf, "%c%s", sep0, "%n");
	else
		sprintf(format_buf, "%s%s", "%%", "%n");
	PROTECT(ans = NEW_LIST(ans_length));
	for (i = 0; i < ans_length; i++) {
		x_elt = STRING_ELT(x, i);
		if (x_elt == NA_STRING) {
			UNPROTECT(1);
			error("'x' contains NAs");
		}
		ans_elt = parse_string_as_integer_vector(x_elt, format_buf);
		if (ans_elt == R_NilValue) {
			UNPROTECT(1);
			error("in list element %d: %s", i + 1, errmsg_buf);
		}
		PROTECT(ans_elt);
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

