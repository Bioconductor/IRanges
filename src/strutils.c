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
 * strsplit_as_list_of_ints()
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
 * When it fails, strsplit_as_list_of_ints() will print a detailed parse
 * error message.
 * It's also faster and uses much less memory. E.g. it's 8x faster and uses
 * < 1 Mb versus > 60 Mb on the character vector 'biginput' created with:
 *   library(GenomicFeatures.Hsapiens.UCSC.hg18)
 *   genes <- geneHuman()
 *   biginput <- c(genes$exonStarts, genes$exonEnds)  # 133606 elements
 */

static IntAE int_ae_buf;

static char errmsg_buf[200];

static SEXP explode_string_as_integer_vector(SEXP s, char sep0)
{
	const char *str;
	int offset, n, ret;
	long int val;

	str = CHAR(s);
	int_ae_buf.nelt = offset = 0;
	while (str[offset]) {
		ret = sscanf(str + offset, "%ld%n", &val, &n);
		if (ret != 1) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "decimal integer expected at char %d",
				 offset + 1);
			return R_NilValue;
		}
		offset += n;
		while (isblank(str[offset])) offset++;
		if (val < INT_MIN || val > INT_MAX) {
			UNPROTECT(1);
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "out of range integer at char %d",
				 offset + 1);
			return R_NilValue;
		}
		_IntAE_insert_at(&int_ae_buf, int_ae_buf.nelt, (int) val);
		if (str[offset] == '\0')
			break;
		if (str[offset] != sep0) {
			snprintf(errmsg_buf, sizeof(errmsg_buf),
				 "separator expected at char %d",
				 offset + 1);
			return R_NilValue;
		}
		offset++;
	}
	return _IntAE_asINTEGER(&int_ae_buf);
}

/* --- .Call ENTRY POINT --- */
SEXP strsplit_as_list_of_ints(SEXP x, SEXP sep)
{
	SEXP ans, x_elt, ans_elt;
	int ans_length, i;
	char sep0;

	ans_length = LENGTH(x);
	sep0 = CHAR(STRING_ELT(sep, 0))[0];
	if (isdigit(sep0) || sep0 == '+' || sep0 == '-')
		error("'sep' cannot be a digit, \"+\" or \"-\"");
	int_ae_buf = _new_IntAE(0, 0, 0);
	PROTECT(ans = NEW_LIST(ans_length));
	for (i = 0; i < ans_length; i++) {
		x_elt = STRING_ELT(x, i);
		if (x_elt == NA_STRING) {
			UNPROTECT(1);
			error("'x' contains NAs");
		}
		ans_elt = explode_string_as_integer_vector(x_elt, sep0);
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

