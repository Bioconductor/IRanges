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
 * findInterval for when x is a sorted integer vector,
 * and 'vec' are the run lengths, not breaks
 */

SEXP Integer_sorted_findInterval(SEXP x, SEXP vec)
{
	int i, x_len, vec_len, index, vec_sum;
	const int *x_ptr, *vec_ptr;
	int *ans_ptr;
	SEXP ans;

	x_len = LENGTH(x);
	vec_len = LENGTH(vec);
	vec_ptr = INTEGER(vec);
	vec_sum = *vec_ptr + 1;
	PROTECT(ans = NEW_INTEGER(x_len));
	index = 1;
	for (i = 0, x_ptr = INTEGER(x), ans_ptr = INTEGER(ans); i < x_len;
	     i++, x_ptr++, ans_ptr++) {
		while (index < vec_len && *x_ptr >= vec_sum) {
			vec_ptr++;
			vec_sum += *vec_ptr;
			index++;
		}
		*ans_ptr = index;
	}
	UNPROTECT(1);

	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP Integer_mseq(SEXP from, SEXP to) {
  int k = 0;
  SEXP ans;
  for (int i = 0; i < length(from); i++)
    k += INTEGER(to)[i] - INTEGER(from)[i] + 1;
  PROTECT(ans = NEW_INTEGER(k));
  k = 0;
  for (int i = 0; i < length(from); i++)
    for (int j = INTEGER(from)[i]; j <= INTEGER(to)[i]; j++)
      INTEGER(ans)[k++] = j;
  UNPROTECT(1);
  return ans;
}
