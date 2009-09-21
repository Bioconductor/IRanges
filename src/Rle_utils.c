#include "IRanges.h"
#include <R_ext/Utils.h>


SEXP Rle_integer_rollSum(SEXP x, SEXP width)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int prev_offset, curr_offset;
	int stat;
	int *prev_length, *curr_length, *buf_lengths, *buf_lengths_elt;
	int *prev_value, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER ||
		INTEGER(width)[0] <= 0)
		error("'width' must be a positive integer");

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(width)[0];

	ans_len = 0;
	x_vec_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, curr_length = INTEGER(lengths); i < nrun; i++, curr_length++) {
		x_vec_len += *curr_length;
		buf_len += *curr_length;
		if (window_len < *curr_length)
			buf_len -= *curr_length - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		buf_values = (int *) R_alloc((long) buf_len, sizeof(int));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		stat = 0;
		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		prev_value = INTEGER(values);
		curr_value = INTEGER(values);
		prev_length = INTEGER(lengths);
		curr_length = INTEGER(lengths);
		prev_offset = INTEGER(lengths)[0];
		curr_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			// calculate stat
			if (i == 0) {
				if (*curr_value == NA_INTEGER)
					error("some values are NAs");
				j = 0;
		    	ans_len = 1;
				while (j < window_len) {
					if (curr_offset == 0) {
						curr_value++;
						curr_length++;
						curr_offset = *curr_length;
						if (*curr_value == NA_INTEGER)
							error("some values are NAs");
					}
					int times =
						curr_offset < window_len - j ?
							curr_offset : window_len - j;
					stat += times * (*curr_value);
					curr_offset -= times;
					j += times;
				}
			} else {
				stat += (*curr_value - *prev_value);
				if (stat != *buf_values_elt) {
			    	ans_len++;
			    	buf_values_elt++;
			    	buf_lengths_elt++;
			    }
			}
	    	*buf_values_elt = stat;
		    // determine length
		    if (window_len < prev_offset) {
		    	*buf_lengths_elt += *prev_length - window_len + 1;
		    	prev_offset = window_len - 1;
		    	curr_offset = 0;
		    } else {
		    	*buf_lengths_elt += 1;
			    if (i > 0) {
			    	prev_offset--;
				    curr_offset--;
			    }
		    }
		    // move pointers if end of run
			if (prev_offset == 0) {
				prev_value++;
				prev_length++;
				prev_offset = *prev_length;
			}
			if (curr_offset == 0) {
				curr_value++;
				curr_length++;
				curr_offset = *curr_length;
				if (*curr_value == NA_INTEGER)
					error("some values are NAs");
			}
		}
	}

	PROTECT(ans_values = NEW_INTEGER(ans_len));
	PROTECT(ans_lengths = NEW_INTEGER(ans_len));
	memcpy(INTEGER(ans_values), buf_values, ans_len * sizeof(int));
	memcpy(INTEGER(ans_lengths), buf_lengths, ans_len * sizeof(int));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(3);

	return ans;
}


SEXP Rle_real_rollSum(SEXP x, SEXP width)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int prev_offset, curr_offset;
	double stat;
	int *prev_length, *curr_length, *buf_lengths, *buf_lengths_elt;
	double *prev_value, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER ||
		INTEGER(width)[0] <= 0)
		error("'width' must be a positive integer");

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(width)[0];

	ans_len = 0;
	x_vec_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, curr_length = INTEGER(lengths); i < nrun; i++, curr_length++) {
		x_vec_len += *curr_length;
		buf_len += *curr_length;
		if (window_len < *curr_length)
			buf_len -= *curr_length - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		buf_values = (double *) R_alloc((long) buf_len, sizeof(double));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		stat = 0;
		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		prev_value = REAL(values);
		curr_value = REAL(values);
		prev_length = INTEGER(lengths);
		curr_length = INTEGER(lengths);
		prev_offset = INTEGER(lengths)[0];
		curr_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			// calculate stat
			if (i == 0) {
				if (*curr_value == NA_REAL)
					error("some values are NAs");
				j = 0;
		    	ans_len = 1;
				while (j < window_len) {
					if (curr_offset == 0) {
						curr_value++;
						curr_length++;
						curr_offset = *curr_length;
						if (*curr_value == NA_REAL)
							error("some values are NAs");
					}
					int times =
						curr_offset < window_len - j ?
							curr_offset : window_len - j;
					stat += times * (*curr_value);
					curr_offset -= times;
					j += times;
				}
			} else {
				stat += (*curr_value - *prev_value);
				if (stat != *buf_values_elt) {
			    	ans_len++;
			    	buf_values_elt++;
			    	buf_lengths_elt++;
			    }
			}
	    	*buf_values_elt = stat;
		    // determine length
		    if (window_len < prev_offset) {
		    	*buf_lengths_elt += *prev_length - window_len + 1;
		    	prev_offset = window_len - 1;
		    	curr_offset = 0;
		    } else {
		    	*buf_lengths_elt += 1;
			    if (i > 0) {
			    	prev_offset--;
				    curr_offset--;
			    }
		    }
		    // move pointers if end of run
			if (prev_offset == 0) {
				prev_value++;
				prev_length++;
				prev_offset = *prev_length;
			}
			if (curr_offset == 0) {
				curr_value++;
				curr_length++;
				curr_offset = *curr_length;
				if (*curr_value == NA_REAL)
					error("some values are NAs");
			}
		}
	}

	PROTECT(ans_values = NEW_NUMERIC(ans_len));
	PROTECT(ans_lengths = NEW_INTEGER(ans_len));
	memcpy(REAL(ans_values), buf_values, ans_len * sizeof(double));
	memcpy(INTEGER(ans_lengths), buf_lengths, ans_len * sizeof(int));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(3);

	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_rollSum(SEXP x, SEXP width)
{
	SEXP ans = R_NilValue;
	switch(TYPEOF(GET_SLOT(x, install("values")))) {
    case INTSXP:
    	PROTECT(ans = Rle_integer_rollSum(x, width));
    	break;
    case REALSXP:
    	PROTECT(ans = Rle_real_rollSum(x, width));
        break;
    default:
		error("rollSum only supported for integer and numeric Rle objects");
	}
	UNPROTECT(1);
	return ans;
}



SEXP Rle_integer_rollWeightedSum(SEXP x, SEXP width, SEXP weight)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	double stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	int *values_elt, *curr_value;
	double *weight_elt, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER ||
		INTEGER(width)[0] <= 0)
		error("'width' must be a positive integer");

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(width)[0];

	if (!IS_NUMERIC(weight) || LENGTH(weight) != window_len)
		error("'weight' must be a numeric vector of length 'width'");

	for (j = 0, weight_elt = REAL(weight); j < window_len; j++, weight_elt++) {
		if (*weight_elt == NA_REAL)
			error("'weight' contains NAs");
	}

	ans_len = 0;
	x_vec_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		x_vec_len += *lengths_elt;
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		buf_values = (double *) R_alloc((long) buf_len, sizeof(double));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = INTEGER(values);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			// calculate stat
			stat = 0;
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for (j = 0, weight_elt = REAL(weight); j < window_len;
			     j++, weight_elt++) {
				if (*curr_value == NA_INTEGER)
					error("some values are NAs");
				stat += (*weight_elt) * (*curr_value);
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
		    // assign value
		    if (ans_len == 0) {
		    	ans_len = 1;
		    } else if (stat != *buf_values_elt) {
		    	ans_len++;
		    	buf_values_elt++;
		    	buf_lengths_elt++;
		    }
	    	*buf_values_elt = stat;
		    // determine length
		    if (window_len < start_offset) {
		    	*buf_lengths_elt += *lengths_elt - window_len + 1;
		    	start_offset = window_len - 1;
		    } else {
		    	*buf_lengths_elt += 1;
			    start_offset--;
		    }
		    // move pointers if end of run
			if (start_offset == 0) {
				values_elt++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}

	PROTECT(ans_values = NEW_NUMERIC(ans_len));
	PROTECT(ans_lengths = NEW_INTEGER(ans_len));
	memcpy(REAL(ans_values), buf_values, ans_len * sizeof(double));
	memcpy(INTEGER(ans_lengths), buf_lengths, ans_len * sizeof(int));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(3);

	return ans;
}


SEXP Rle_real_rollWeightedSum(SEXP x, SEXP width, SEXP weight)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	double stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	double *values_elt, *curr_value;
	double *weight_elt, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER ||
		INTEGER(width)[0] <= 0)
		error("'width' must be a positive integer");

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(width)[0];

	if (!IS_NUMERIC(weight) || LENGTH(weight) != window_len)
		error("'weight' must be a numeric vector of length 'width'");

	for (j = 0, weight_elt = REAL(weight); j < window_len; j++, weight_elt++) {
		if (*weight_elt == NA_REAL)
			error("'weight' contains NAs");
	}

	ans_len = 0;
	x_vec_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		x_vec_len += *lengths_elt;
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		buf_values = (double *) R_alloc((long) buf_len, sizeof(double));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = REAL(values);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			// calculate stat
			stat = 0;
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for (j = 0, weight_elt = REAL(weight); j < window_len;
			     j++, weight_elt++) {
				if (*curr_value == NA_REAL)
					error("some values are NAs");
				stat += (*weight_elt) * (*curr_value);
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
		    // assign value
		    if (ans_len == 0) {
		    	ans_len = 1;
		    } else if (stat != *buf_values_elt) {
		    	ans_len++;
		    	buf_values_elt++;
		    	buf_lengths_elt++;
		    }
	    	*buf_values_elt = stat;
		    // determine length
		    if (window_len < start_offset) {
		    	*buf_lengths_elt += *lengths_elt - window_len + 1;
		    	start_offset = window_len - 1;
		    } else {
		    	*buf_lengths_elt += 1;
			    start_offset--;
		    }
		    // move pointers if end of run
			if (start_offset == 0) {
				values_elt++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}

	PROTECT(ans_values = NEW_NUMERIC(ans_len));
	PROTECT(ans_lengths = NEW_INTEGER(ans_len));
	memcpy(REAL(ans_values), buf_values, ans_len * sizeof(double));
	memcpy(INTEGER(ans_lengths), buf_lengths, ans_len * sizeof(int));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(3);

	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_rollWeightedSum(SEXP x, SEXP width, SEXP weight)
{
	SEXP ans = R_NilValue;
	switch(TYPEOF(GET_SLOT(x, install("values")))) {
    case INTSXP:
    	PROTECT(ans =  Rle_integer_rollWeightedSum(x, width, weight));
    	break;
    case REALSXP:
    	PROTECT(ans =  Rle_real_rollWeightedSum(x, width, weight));
        break;
    default:
		error("rollWeightedSum only supported for integer and numeric Rle objects");
	}
	UNPROTECT(1);
	return ans;
}



SEXP Rle_integer_rollQ(SEXP x, SEXP width, SEXP which)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	int q_index;
	int stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	int *window, *values_elt, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER ||
		INTEGER(width)[0] <= 0)
		error("'width' must be a positive integer");

	if (!IS_INTEGER(which) || LENGTH(which) != 1 ||
		INTEGER(which)[0] == NA_INTEGER ||
		INTEGER(which)[0] < 1 || INTEGER(which)[0] > INTEGER(width)[0])
		error("'which' must be an integer in [0, width]");

	q_index = INTEGER(which)[0] - 1;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(width)[0];

	ans_len = 0;
	x_vec_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		x_vec_len += *lengths_elt;
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		window = (int *) R_alloc(window_len, sizeof(int));
		buf_values = (int *) R_alloc((long) buf_len, sizeof(int));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = INTEGER(values);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			// create window
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for(j = 0; j < window_len; j++) {
				if (*curr_value == NA_INTEGER)
					error("some values are NAs");
				window[j] = *curr_value;
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
			// calculate stat
		    iPsort(window, window_len, q_index);
		    stat = window[q_index];
		    // assign value
		    if (ans_len == 0) {
		    	ans_len = 1;
		    } else if (stat != *buf_values_elt) {
		    	ans_len++;
		    	buf_values_elt++;
		    	buf_lengths_elt++;
		    }
	    	*buf_values_elt = stat;
		    // determine length
		    if (window_len < start_offset) {
		    	*buf_lengths_elt += *lengths_elt - window_len + 1;
		    	start_offset = window_len - 1;
		    } else {
		    	*buf_lengths_elt += 1;
			    start_offset--;
		    }
		    // move pointers if end of run
			if (start_offset == 0) {
				values_elt++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}

	PROTECT(ans_values = NEW_INTEGER(ans_len));
	PROTECT(ans_lengths = NEW_INTEGER(ans_len));
	memcpy(INTEGER(ans_values), buf_values, ans_len * sizeof(int));
	memcpy(INTEGER(ans_lengths), buf_lengths, ans_len * sizeof(int));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(3);

	return ans;
}

SEXP Rle_real_rollQ(SEXP x, SEXP width, SEXP which)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	int q_index;
	double stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	double *window, *values_elt, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER ||
		INTEGER(width)[0] <= 0)
		error("'width' must be a positive integer");

	if (!IS_INTEGER(which) || LENGTH(which) != 1 ||
		INTEGER(which)[0] == NA_INTEGER ||
		INTEGER(which)[0] < 1 || INTEGER(which)[0] > INTEGER(width)[0])
		error("'which' must be an integer in [0, width]");

	q_index = INTEGER(which)[0] - 1;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(width)[0];

	ans_len = 0;
	x_vec_len = 0;
	buf_len = - window_len + 1;
	for(i = 0, lengths_elt = INTEGER(lengths); i < nrun; i++, lengths_elt++) {
		x_vec_len += *lengths_elt;
		buf_len += *lengths_elt;
		if (window_len < *lengths_elt)
			buf_len -= *lengths_elt - window_len;
	}

	buf_values = NULL;
	buf_lengths = NULL;
	if (buf_len > 0) {
		window = (double *) R_alloc(window_len, sizeof(double));
		buf_values = (double *) R_alloc((long) buf_len, sizeof(double));
		buf_lengths = (int *) R_alloc((long) buf_len, sizeof(int));
		memset(buf_lengths, 0, buf_len * sizeof(int));

		buf_values_elt = buf_values;
		buf_lengths_elt = buf_lengths;
		values_elt = REAL(values);
		lengths_elt = INTEGER(lengths);
		start_offset = INTEGER(lengths)[0];
		for (i = 0; i < buf_len; i++) {
			// create window
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for(j = 0; j < window_len; j++) {
				if (*curr_value == NA_REAL)
					error("some values are NAs");
				window[j] = *curr_value;
				curr_offset--;
				if (curr_offset == 0) {
					curr_value++;
					curr_length++;
					curr_offset = *curr_length;
				}
			}
			// calculate stat
		    rPsort(window, window_len, q_index);
		    stat = window[q_index];
		    // assign value
		    if (ans_len == 0) {
		    	ans_len = 1;
		    } else if (stat != *buf_values_elt) {
		    	ans_len++;
		    	buf_values_elt++;
		    	buf_lengths_elt++;
		    }
	    	*buf_values_elt = stat;
		    // determine length
		    if (window_len < start_offset) {
		    	*buf_lengths_elt += *lengths_elt - window_len + 1;
		    	start_offset = window_len - 1;
		    } else {
		    	*buf_lengths_elt += 1;
			    start_offset--;
		    }
		    // move pointers if end of run
			if (start_offset == 0) {
				values_elt++;
				lengths_elt++;
				start_offset = *lengths_elt;
			}
		}
	}

	PROTECT(ans_values = NEW_NUMERIC(ans_len));
	PROTECT(ans_lengths = NEW_INTEGER(ans_len));
	memcpy(REAL(ans_values), buf_values, ans_len * sizeof(double));
	memcpy(INTEGER(ans_lengths), buf_lengths, ans_len * sizeof(int));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(3);

	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_rollQ(SEXP x, SEXP width, SEXP which)
{
	SEXP ans = R_NilValue;
	switch(TYPEOF(GET_SLOT(x, install("values")))) {
    case INTSXP:
    	PROTECT(ans = Rle_integer_rollQ(x, width, which));
    	break;
    case REALSXP:
    	PROTECT(ans = Rle_real_rollQ(x, width, which));
        break;
    default:
		error("rollQ only supported for integer and numeric Rle objects");
	}
	UNPROTECT(1);
	return ans;
}
