#include "IRanges.h"
#include <R_ext/Utils.h>

SEXP Rle_integer_runsum(SEXP x, SEXP k, SEXP na_rm)
{
 int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
    int prev_offset, curr_offset;
    int stat, stat_na;
    int *prev_length, *curr_length, *buf_lengths, *buf_lengths_elt;
    int *prev_value, *curr_value, *buf_values, *buf_values_elt;
    int *prev_value_na, *curr_value_na;
    SEXP values, lengths, ans, ans_values, ans_lengths;
    SEXP orig_values, na_index;
    const int narm = LOGICAL(na_rm)[0];

    if (!IS_INTEGER(k) || LENGTH(k) != 1
                       || INTEGER(k)[0] == NA_INTEGER
                       || INTEGER(k)[0] <= 0)
        error("'k' must be a positive integer");

    /* Set NA values to 0
     * Create NA index : 1 = NA; 0 = not NA 
     */
    orig_values = GET_SLOT(x, install("values"));
    values = PROTECT(Rf_allocVector(INTSXP, LENGTH(orig_values)));
    na_index = PROTECT(Rf_allocVector(INTSXP, LENGTH(orig_values)));
    int *vlu = INTEGER(orig_values);
    for(i = 0; i < LENGTH(orig_values); i++) {
        if (vlu[i] == NA_INTEGER) {
            INTEGER(na_index)[i] = 1;
            INTEGER(values)[i] = 0;
        } else {
            INTEGER(na_index)[i] = 0;
            INTEGER(values)[i] = INTEGER(orig_values)[i];
        }
    }

lengths = GET_SLOT(x, install("lengths"));
    nrun = LENGTH(lengths);
    window_len = INTEGER(k)[0];
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
        stat_na = 0;
        buf_values_elt = buf_values;
        buf_lengths_elt = buf_lengths;
        prev_value = INTEGER(values);
        curr_value = INTEGER(values);
        prev_length = INTEGER(lengths);
        curr_length = INTEGER(lengths);
        prev_offset = INTEGER(lengths)[0];
        curr_offset = INTEGER(lengths)[0];
        prev_value_na = INTEGER(na_index);
        curr_value_na = INTEGER(na_index);

       for (i = 0; i < buf_len; i++) {
            if (i % 100000 == 99999)
                R_CheckUserInterrupt();
            /* calculate stat */
            if (i == 0) {
                j = 0;
                ans_len = 1;
                while (j < window_len) {
                    if (curr_offset == 0) {
                        curr_value++;
                        curr_value_na++;
                        curr_length++;
                        curr_offset = *curr_length;
                    }
                    int times = curr_offset < window_len - j ?
                                curr_offset : window_len - j;
                    stat += times * (*curr_value);
                    stat_na += times * (*curr_value_na);
                    curr_offset -= times;
                    j += times;
                }
            } else {
                stat += (*curr_value - *prev_value);
                stat_na += (*curr_value_na - *prev_value_na);
                /* increment values and lengths based on stat */
                if (narm | stat_na == 0) {
                    if (stat != *buf_values_elt) {
                        ans_len++;
                        buf_values_elt++;
                        buf_lengths_elt++;
                    }
                } else {
                    if ((stat_na != 0) & (*buf_values_elt != NA_INTEGER)) {
                       ans_len++;
                       buf_values_elt++;
                       buf_lengths_elt++;
                    }
                }

            }
           /* NA handling */
            if (!narm & (stat_na != 0))
                *buf_values_elt = NA_INTEGER;
            else
                *buf_values_elt = stat;

            /* determine length */
            if (i == 0) {
                if (prev_value == curr_value) {
                    /* NA handling */
                    if (!narm & (*curr_value_na == 1)) {
                        if (prev_value_na  == curr_value_na)
                            *buf_lengths_elt += *curr_length - window_len + 1;
                        else
                            *buf_lengths_elt += *curr_length - window_len + 1;
                    } else {
                        *buf_lengths_elt += *curr_length - window_len + 1;
                    }
                    prev_offset = window_len;
                    curr_offset = 0;
                } else {
                    *buf_lengths_elt += 1;
                }
            } else {
                if ((prev_offset == 1) && (window_len < *curr_length) &&
                ((prev_value + 1) == curr_value)) {
                    /* moving through run lengths > window size
                     * no NA handling necessary
                     */
                    *buf_lengths_elt += *curr_length - window_len + 1;
                    prev_offset = window_len;
                    curr_offset = 0;
                    prev_value++;
                    prev_value_na++;
                    prev_length++;
                } else {
                    /* NA handling */
                    if (!narm & (*curr_value_na == 1)) {
                        if (prev_value_na  == curr_value_na)
                            *buf_lengths_elt += *curr_length - window_len + 1;
                        else
                            *buf_lengths_elt += 1;
                    } else {
                        *buf_lengths_elt += 1;
                    }
                    prev_offset--;
                    curr_offset--;
                    if (prev_offset == 0) {
                        prev_value++;
                        prev_value_na++;
                        prev_length++;
                        prev_offset = *prev_length;
                    }
                }
            }
            if ((curr_offset == 0) && (i != buf_len - 1)) {
                curr_value++;
                curr_value_na++;
                curr_length++;
                curr_offset = *curr_length;
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
    UNPROTECT(5);

    return ans;
}

SEXP Rle_real_runsum(SEXP x, SEXP k, SEXP na_rm)
{
    int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
    int prev_offset, curr_offset, m_offset;
    double stat;
    int *prev_length, *curr_length, *buf_lengths, *buf_lengths_elt;
    double *prev_value, *curr_value, *buf_values, *buf_values_elt;
    double *m_value;
    int *m_length;
    SEXP values, lengths, ans, ans_values, ans_lengths;
    SEXP orig_values;
    const int narm = LOGICAL(na_rm)[0];

    if (!IS_INTEGER(k) || LENGTH(k) != 1 
                       || INTEGER(k)[0] == NA_INTEGER 
                       || INTEGER(k)[0] <= 0)
    error("'k' must be a positive integer");

    if (narm) {
        /* set NA and NaN values to 0 */
        orig_values = GET_SLOT(x, install("values"));
        values = PROTECT(Rf_allocVector(REALSXP, LENGTH(orig_values)));
        double *vlu = REAL(orig_values);
        for(i = 0; i < LENGTH(orig_values); i++) {
            if (ISNAN(vlu[i])) {
                REAL(values)[i] = 0; 
            } else {
                REAL(values)[i] = REAL(orig_values)[i];
            } 
        }
    } else {
        values = GET_SLOT(x, install("values"));
    }
    lengths = GET_SLOT(x, install("lengths"));
    nrun = LENGTH(lengths);
    window_len = INTEGER(k)[0];
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
            if (i % 100000 == 99999)
                R_CheckUserInterrupt();
            /* calculate stat */
            if (i == 0) {
                j = 0;
                stat = 0;
                ans_len = 1;
                while (j < window_len) {
                    if (curr_offset == 0) {
                        curr_value++;
                        curr_length++;
                        curr_offset = *curr_length;
                    }
                    int times = curr_offset < window_len - j ?
                                curr_offset : window_len - j;
                    stat += times * (*curr_value);
                    curr_offset -= times;
                    j += times;
                }
            } else {
                j = 0;
                stat = 0;
                m_offset = prev_offset - 1;
                m_value = prev_value;
                m_length = prev_length;

                while (j < window_len) {
                    if (m_offset == 0) {
                        m_value++;
                        m_length++;
                        m_offset = *m_length;
                    }
                    int times = m_offset < window_len - j ?
                                m_offset : window_len - j;
                    stat += times * (*m_value);
                    m_offset -= times;
                    j += times;
                }
                if (!R_FINITE(stat) & !R_FINITE(*buf_values_elt)) {
                    if (R_IsNA(stat) & !R_IsNA(*buf_values_elt) ||
                        !R_IsNA(stat) & R_IsNA(*buf_values_elt) ||
                        R_IsNaN(stat) & !R_IsNaN(*buf_values_elt) ||
                        !R_IsNaN(stat) & R_IsNaN(*buf_values_elt) ||
                        (stat == R_PosInf) & (*buf_values_elt != R_PosInf) ||
                        (stat != R_PosInf) & (*buf_values_elt == R_PosInf) ||
                        (stat == R_NegInf) & (*buf_values_elt != R_NegInf) ||
                        (stat != R_NegInf) & (*buf_values_elt == R_NegInf)) {
                        ans_len++;
                        buf_values_elt++;
                        buf_lengths_elt++;
                    }
                } else {
                    if (stat != *buf_values_elt) {
                        ans_len++;
                        buf_values_elt++;
                        buf_lengths_elt++;
                    }
                }
            }
            *buf_values_elt = stat;

            /* determine length */
            if (i == 0) {
                if (prev_value == curr_value) {
                    *buf_lengths_elt += *curr_length - window_len + 1;
                    prev_offset = window_len;
                    curr_offset = 0;
                } else {
                    *buf_lengths_elt += 1;
                }
            } else {
                if ((prev_offset == 1) && (window_len < *curr_length) &&
                ((prev_value + 1) == curr_value)) {
                    /* moving through run lengths > window size */
                    *buf_lengths_elt += *curr_length - window_len + 1;
                    prev_offset = window_len;
                    curr_offset = 0;
                    prev_value++;
                    prev_length++;
                } else {
                    *buf_lengths_elt += 1;
                    prev_offset--;
                    curr_offset--;
                    if (prev_offset == 0) {
                        prev_value++;
                        prev_length++;
                        prev_offset = *prev_length;
                    }
                }
            }
            if ((curr_offset == 0) && (i != buf_len - 1)) {
            curr_value++;
            curr_length++;
            curr_offset = *curr_length;
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

    if (narm)
        UNPROTECT(4);
    else
        UNPROTECT(3);

    return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_runsum(SEXP x, SEXP k, SEXP na_rm)
{
        SEXP ans = R_NilValue;
        switch(TYPEOF(GET_SLOT(x, install("values")))) {
    case INTSXP:
        PROTECT(ans = Rle_integer_runsum(x, k, na_rm));
        break;
    case REALSXP:
        PROTECT(ans = Rle_real_runsum(x, k, na_rm));
        break;
    default:
        error("runsum only supported for integer and numeric Rle objects");
        }
        UNPROTECT(1);
        return ans;
}



SEXP Rle_integer_runwtsum(SEXP x, SEXP k, SEXP wt)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	double stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	int *values_elt, *curr_value;
	double *wt_elt, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(k) || LENGTH(k) != 1 ||
		INTEGER(k)[0] == NA_INTEGER ||
		INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(k)[0];

	if (!IS_NUMERIC(wt) || LENGTH(wt) != window_len)
		error("'wt' must be a numeric vector of length 'k'");

	for (j = 0, wt_elt = REAL(wt); j < window_len; j++, wt_elt++) {
		if (!R_FINITE(*wt_elt))
			error("'wt' contains NA, NaN, +/-Inf");
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
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			// calculate stat
			stat = 0;
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for (j = 0, wt_elt = REAL(wt); j < window_len;
			     j++, wt_elt++) {
				if (*curr_value == NA_INTEGER)
					error("some values are NA");
				stat += (*wt_elt) * (*curr_value);
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


SEXP Rle_real_runwtsum(SEXP x, SEXP k, SEXP wt)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	double stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	double *values_elt, *curr_value;
	double *wt_elt, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(k) || LENGTH(k) != 1 ||
		INTEGER(k)[0] == NA_INTEGER ||
		INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(k)[0];

	if (!IS_NUMERIC(wt) || LENGTH(wt) != window_len)
		error("'wt' must be a numeric vector of length 'k'");

	for (j = 0, wt_elt = REAL(wt); j < window_len; j++, wt_elt++) {
		if (!R_FINITE(*wt_elt))
			error("'wt' contains NA, NaN, +/-Inf");
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
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			// calculate stat
			stat = 0;
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for (j = 0, wt_elt = REAL(wt); j < window_len;
			     j++, wt_elt++) {
				if (!R_FINITE(*curr_value))
					error("some values are NA, NaN, +/-Inf");
				stat += (*wt_elt) * (*curr_value);
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

SEXP Rle_runwtsum(SEXP x, SEXP k, SEXP wt)
{
	SEXP ans = R_NilValue;
	switch(TYPEOF(GET_SLOT(x, install("values")))) {
    case INTSXP:
    	PROTECT(ans =  Rle_integer_runwtsum(x, k, wt));
    	break;
    case REALSXP:
    	PROTECT(ans =  Rle_real_runwtsum(x, k, wt));
        break;
    default:
		error("runwtsum only supported for integer and numeric Rle objects");
	}
	UNPROTECT(1);
	return ans;
}



SEXP Rle_integer_runq(SEXP x, SEXP k, SEXP which)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	int q_index;
	int stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	int *window, *values_elt, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(k) || LENGTH(k) != 1 ||
		INTEGER(k)[0] == NA_INTEGER ||
		INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	if (!IS_INTEGER(which) || LENGTH(which) != 1 ||
		INTEGER(which)[0] == NA_INTEGER ||
		INTEGER(which)[0] < 1 || INTEGER(which)[0] > INTEGER(k)[0])
		error("'which' must be an integer in [0, k]");

	q_index = INTEGER(which)[0] - 1;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(k)[0];

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
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			// create window
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for(j = 0; j < window_len; j++) {
				if (*curr_value == NA_INTEGER)
					error("some values are NA");
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

SEXP Rle_real_runq(SEXP x, SEXP k, SEXP which)
{
	int i, j, nrun, window_len, buf_len, x_vec_len, ans_len;
	int start_offset, curr_offset;
	int q_index;
	double stat;
	int *lengths_elt, *curr_length, *buf_lengths, *buf_lengths_elt;
	double *window, *values_elt, *curr_value, *buf_values, *buf_values_elt;
	SEXP values, lengths,  ans, ans_values, ans_lengths;

	if (!IS_INTEGER(k) || LENGTH(k) != 1 ||
		INTEGER(k)[0] == NA_INTEGER ||
		INTEGER(k)[0] <= 0)
		error("'k' must be a positive integer");

	if (!IS_INTEGER(which) || LENGTH(which) != 1 ||
		INTEGER(which)[0] == NA_INTEGER ||
		INTEGER(which)[0] < 1 || INTEGER(which)[0] > INTEGER(k)[0])
		error("'which' must be an integer in [0, k]");

	q_index = INTEGER(which)[0] - 1;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	nrun = LENGTH(lengths);
	window_len = INTEGER(k)[0];

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
			if (i % 100000 == 99999)
				R_CheckUserInterrupt();
			// create window
			curr_value = values_elt;
			curr_length = lengths_elt;
			curr_offset = start_offset;
			for(j = 0; j < window_len; j++) {
				if (!R_FINITE(*curr_value))
					error("some values are NA, NaN, +/-Inf");
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

SEXP Rle_runq(SEXP x, SEXP k, SEXP which)
{
	SEXP ans = R_NilValue;
	switch(TYPEOF(GET_SLOT(x, install("values")))) {
    case INTSXP:
    	PROTECT(ans = Rle_integer_runq(x, k, which));
    	break;
    case REALSXP:
    	PROTECT(ans = Rle_real_runq(x, k, which));
        break;
    default:
		error("runq only supported for integer and numeric Rle objects");
	}
	UNPROTECT(1);
	return ans;
}
