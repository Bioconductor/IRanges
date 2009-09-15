#include "IRanges.h"


SEXP Rle_logical_constructor(SEXP x, SEXP counts) {
	int n, unpro;
	SEXP ans, ans_values, ans_lengths;

    n = LENGTH(x);

    if (n == 0) {
    	PROTECT(ans_values = NEW_LOGICAL(0));
    	PROTECT(ans_lengths = NEW_INTEGER(0));
        unpro = 3;
    } else if (n == 1) {
    	PROTECT(ans_values = NEW_LOGICAL(1));
    	PROTECT(ans_lengths = NEW_INTEGER(1));
    	LOGICAL(ans_values)[0] = LOGICAL(x)[0];
    	if (LENGTH(counts) == 0) {
    		INTEGER(ans_lengths)[0] = 1;
    	} else {
        	INTEGER(ans_lengths)[0] = INTEGER(counts)[0];
    	}
    	unpro = 3;
    } else {
    	int i, index;
    	int *val1, *val2;
    	SEXP buf_values, buf_lengths;

    	PROTECT(buf_values = NEW_LOGICAL(n));
    	PROTECT(buf_lengths = NEW_INTEGER(n));
    	memset(INTEGER(buf_lengths), 0, n * sizeof(int));
    	LOGICAL(buf_values)[0] = LOGICAL(x)[0];
    	index = 0;
    	if (LENGTH(counts) == 0) {
        	INTEGER(buf_lengths)[0] = 1;
        	for (i = 1, val1 = LOGICAL(x), val2 = LOGICAL(x) + 1; i < n;
        	     i++, val1++, val2++) {
                if (*val1 != *val2) {
                	index++;
                	LOGICAL(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index]++;
        	}
    	} else {
    		int *cnt;
        	INTEGER(buf_lengths)[0] = INTEGER(counts)[0];
        	for (i = 1, val1 = LOGICAL(x), val2 = LOGICAL(x) + 1,
        		 cnt = INTEGER(counts) + 1; i < n; i++, val1++, val2++, cnt++) {
                if (*val1 != *val2) {
                	index++;
                	LOGICAL(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index] += *cnt;
        	}
    	}
    	index++;
    	PROTECT(ans_values = NEW_LOGICAL(index));
    	PROTECT(ans_lengths = NEW_INTEGER(index));
    	memcpy(LOGICAL(ans_values), LOGICAL(buf_values), index * sizeof(int));
    	memcpy(INTEGER(ans_lengths), INTEGER(buf_lengths), index * sizeof(int));
    	unpro = 5;
    }

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(unpro);

	return ans;
}

SEXP Rle_integer_constructor(SEXP x, SEXP counts) {
	int n, unpro;
	SEXP ans, ans_values, ans_lengths;

    n = LENGTH(x);

    if (n == 0) {
    	PROTECT(ans_values = NEW_INTEGER(0));
    	PROTECT(ans_lengths = NEW_INTEGER(0));
        unpro = 3;
    } else if (n == 1) {
    	PROTECT(ans_values = NEW_INTEGER(1));
    	PROTECT(ans_lengths = NEW_INTEGER(1));
    	INTEGER(ans_values)[0] = INTEGER(x)[0];
    	if (LENGTH(counts) == 0) {
    		INTEGER(ans_lengths)[0] = 1;
    	} else {
        	INTEGER(ans_lengths)[0] = INTEGER(counts)[0];
    	}
    	unpro = 3;
    } else {
    	int i, index;
    	int *val1, *val2;
    	SEXP buf_values, buf_lengths;

    	PROTECT(buf_values = NEW_INTEGER(n));
    	PROTECT(buf_lengths = NEW_INTEGER(n));
    	memset(INTEGER(buf_lengths), 0, n * sizeof(int));
    	INTEGER(buf_values)[0] = INTEGER(x)[0];
    	index = 0;
    	if (LENGTH(counts) == 0) {
        	INTEGER(buf_lengths)[0] = 1;
        	for (i = 1, val1 = INTEGER(x), val2 = INTEGER(x) + 1; i < n;
        	     i++, val1++, val2++) {
                if (*val1 != *val2) {
                	index++;
                	INTEGER(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index]++;
        	}
    	} else {
    		int *cnt;
        	INTEGER(buf_lengths)[0] = INTEGER(counts)[0];
        	for (i = 1, val1 = INTEGER(x), val2 = INTEGER(x) + 1,
        		 cnt = INTEGER(counts) + 1; i < n; i++, val1++, val2++, cnt++) {
                if (*val1 != *val2) {
                	index++;
                	INTEGER(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index] += *cnt;
        	}
    	}
    	index++;
    	PROTECT(ans_values = NEW_INTEGER(index));
    	PROTECT(ans_lengths = NEW_INTEGER(index));
    	memcpy(INTEGER(ans_values), INTEGER(buf_values), index * sizeof(int));
    	memcpy(INTEGER(ans_lengths), INTEGER(buf_lengths), index * sizeof(int));
    	unpro = 5;
    }

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(unpro);

	return ans;
}

SEXP Rle_real_constructor(SEXP x, SEXP counts) {
	int n, unpro;
	SEXP ans, ans_values, ans_lengths;

    n = LENGTH(x);

    if (n == 0) {
    	PROTECT(ans_values = NEW_NUMERIC(0));
    	PROTECT(ans_lengths = NEW_INTEGER(0));
        unpro = 3;
    } else if (n == 1) {
    	PROTECT(ans_values = NEW_NUMERIC(1));
    	PROTECT(ans_lengths = NEW_INTEGER(1));
    	REAL(ans_values)[0] = REAL(x)[0];
    	if (LENGTH(counts) == 0) {
    		INTEGER(ans_lengths)[0] = 1;
    	} else {
        	INTEGER(ans_lengths)[0] = INTEGER(counts)[0];
    	}
    	unpro = 3;
    } else {
    	int i, index;
    	double *val1, *val2;
    	SEXP buf_values, buf_lengths;

    	PROTECT(buf_values = NEW_NUMERIC(n));
    	PROTECT(buf_lengths = NEW_INTEGER(n));
    	memset(INTEGER(buf_lengths), 0, n * sizeof(int));
    	REAL(buf_values)[0] = REAL(x)[0];
    	index = 0;
    	if (LENGTH(counts) == 0) {
        	INTEGER(buf_lengths)[0] = 1;
        	for (i = 1, val1 = REAL(x), val2 = REAL(x) + 1; i < n;
        	     i++, val1++, val2++) {
                if (*val1 != *val2) {
                	index++;
                	REAL(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index]++;
        	}
    	} else {
    		int *cnt;
        	INTEGER(buf_lengths)[0] = INTEGER(counts)[0];
        	for (i = 1, val1 = REAL(x), val2 = REAL(x) + 1,
        		 cnt = INTEGER(counts) + 1; i < n; i++, val1++, val2++, cnt++) {
                if (*val1 != *val2) {
                	index++;
                	REAL(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index] += *cnt;
        	}
    	}
    	index++;
    	PROTECT(ans_values = NEW_NUMERIC(index));
    	PROTECT(ans_lengths = NEW_INTEGER(index));
    	memcpy(REAL(ans_values), REAL(buf_values), index * sizeof(double));
    	memcpy(INTEGER(ans_lengths), INTEGER(buf_lengths), index * sizeof(int));
    	unpro = 5;
    }

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(unpro);

	return ans;
}

SEXP Rle_complex_constructor(SEXP x, SEXP counts) {
	int n, unpro;
	SEXP ans, ans_values, ans_lengths;

    n = LENGTH(x);

    if (n == 0) {
    	PROTECT(ans_values = NEW_COMPLEX(0));
    	PROTECT(ans_lengths = NEW_INTEGER(0));
        unpro = 3;
    } else if (n == 1) {
    	PROTECT(ans_values = NEW_COMPLEX(1));
    	PROTECT(ans_lengths = NEW_INTEGER(1));
    	COMPLEX(ans_values)[0].r = COMPLEX(x)[0].r;
    	COMPLEX(ans_values)[0].i = COMPLEX(x)[0].i;
    	if (LENGTH(counts) == 0) {
    		INTEGER(ans_lengths)[0] = 1;
    	} else {
        	INTEGER(ans_lengths)[0] = INTEGER(counts)[0];
    	}
    	unpro = 3;
    } else {
    	int i, index;
    	Rcomplex val1, val2;
    	SEXP buf_values, buf_lengths;

    	PROTECT(buf_values = NEW_COMPLEX(n));
    	PROTECT(buf_lengths = NEW_INTEGER(n));
    	memset(INTEGER(buf_lengths), 0, n * sizeof(int));
    	COMPLEX(buf_values)[0].r = COMPLEX(x)[0].r;
    	COMPLEX(buf_values)[0].i = COMPLEX(x)[0].i;
    	index = 0;
    	if (LENGTH(counts) == 0) {
        	INTEGER(buf_lengths)[0] = 1;
    		val2 = COMPLEX(x)[0];
        	for (i = 1; i < n; i++) {
        		val1 = val2;
        		val2 = COMPLEX(x)[i];
                if (val1.r != val2.r || val1.i != val2.i) {
                	index++;
                	COMPLEX(buf_values)[index].r = val2.r;
                	COMPLEX(buf_values)[index].i = val2.i;
                }
                INTEGER(buf_lengths)[index]++;
        	}
    	} else {
    		int *cnt;
        	INTEGER(buf_lengths)[0] = INTEGER(counts)[0];
    		val2 = COMPLEX(x)[0];
        	for (i = 1, cnt = INTEGER(counts) + 1; i < n; i++, cnt++) {
        		val1 = val2;
        		val2 = COMPLEX(x)[i];
                if (val1.r != val2.r || val1.i != val2.i) {
                	index++;
                	COMPLEX(buf_values)[index].r = val2.r;
                	COMPLEX(buf_values)[index].i = val2.i;
                }
                INTEGER(buf_lengths)[index] += *cnt;
        	}
    	}
    	index++;
    	PROTECT(ans_values = NEW_COMPLEX(index));
    	PROTECT(ans_lengths = NEW_INTEGER(index));
    	for (i = 0; i < index; i++) {
    		COMPLEX(ans_values)[i].r = COMPLEX(buf_values)[i].r;
    		COMPLEX(ans_values)[i].i = COMPLEX(buf_values)[i].i;
    	}
    	memcpy(INTEGER(ans_lengths), INTEGER(buf_lengths), index * sizeof(int));
    	unpro = 5;
    }

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(unpro);

	return ans;
}

SEXP Rle_string_constructor(SEXP x, SEXP counts) {
	int n, unpro;
	SEXP ans, ans_values, ans_lengths;

    n = LENGTH(x);

    if (n == 0) {
    	PROTECT(ans_values = NEW_CHARACTER(0));
    	PROTECT(ans_lengths = NEW_INTEGER(0));
        unpro = 3;
    } else if (n == 1) {
    	PROTECT(ans_values = NEW_CHARACTER(1));
    	PROTECT(ans_lengths = NEW_INTEGER(1));
    	SET_STRING_ELT(ans_values, 0, STRING_ELT(x, 0));
    	if (LENGTH(counts) == 0) {
    		INTEGER(ans_lengths)[0] = 1;
    	} else {
        	INTEGER(ans_lengths)[0] = INTEGER(counts)[0];
    	}
    	unpro = 3;
    } else {
    	int i, index;
    	SEXP val1, val2;
    	SEXP buf_values, buf_lengths;

    	PROTECT(buf_values = NEW_CHARACTER(n));
    	PROTECT(buf_lengths = NEW_INTEGER(n));
    	memset(INTEGER(buf_lengths), 0, n * sizeof(int));
    	SET_STRING_ELT(buf_values, 0, STRING_ELT(x, 0));
    	index = 0;
    	if (LENGTH(counts) == 0) {
        	INTEGER(buf_lengths)[0] = 1;
    		val2 = STRING_ELT(x, 0);
        	for (i = 1; i < n; i++) {
        		val1 = val2;
        		val2 = STRING_ELT(x, i);
                if (strcmp(CHAR(val1), CHAR(val2)) != 0) {
                	index++;
                	SET_STRING_ELT(buf_values, index, val2);
                }
                INTEGER(buf_lengths)[index]++;
        	}
    	} else {
    		int *cnt;
        	INTEGER(buf_lengths)[0] = INTEGER(counts)[0];
    		val2 = STRING_ELT(x, 0);
        	for (i = 1, cnt = INTEGER(counts) + 1; i < n; i++, cnt++) {
        		val1 = val2;
        		val2 = STRING_ELT(x, i);
                if (strcmp(CHAR(val1), CHAR(val2)) != 0) {
                	index++;
                	SET_STRING_ELT(buf_values, index, val2);
                }
                INTEGER(buf_lengths)[index] += *cnt;
        	}
    	}
    	index++;
    	PROTECT(ans_values = NEW_CHARACTER(index));
    	PROTECT(ans_lengths = NEW_INTEGER(index));
    	for (i = 0; i < index; i++) {
    		SET_STRING_ELT(ans_values, i, STRING_ELT(buf_values, i));
    	}
    	memcpy(INTEGER(ans_lengths), INTEGER(buf_lengths), index * sizeof(int));
    	unpro = 5;
    }

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(unpro);

	return ans;
}

SEXP Rle_raw_constructor(SEXP x, SEXP counts) {
	int n, unpro;
	SEXP ans, ans_values, ans_lengths;

    n = LENGTH(x);

    if (n == 0) {
    	PROTECT(ans_values = NEW_RAW(0));
    	PROTECT(ans_lengths = NEW_INTEGER(0));
        unpro = 3;
    } else if (n == 1) {
    	PROTECT(ans_values = NEW_RAW(1));
    	PROTECT(ans_lengths = NEW_INTEGER(1));
    	RAW(ans_values)[0] = RAW(x)[0];
    	if (LENGTH(counts) == 0) {
    		INTEGER(ans_lengths)[0] = 1;
    	} else {
        	INTEGER(ans_lengths)[0] = INTEGER(counts)[0];
    	}
    	unpro = 3;
    } else {
    	int i, index;
    	Rbyte *val1, *val2;
    	SEXP buf_values, buf_lengths;

    	PROTECT(buf_values = NEW_RAW(n));
    	PROTECT(buf_lengths = NEW_INTEGER(n));
    	memset(INTEGER(buf_lengths), 0, n * sizeof(int));
    	RAW(buf_values)[0] = RAW(x)[0];
    	index = 0;
    	if (LENGTH(counts) == 0) {
        	INTEGER(buf_lengths)[0] = 1;
        	for (i = 1, val1 = RAW(x), val2 = RAW(x) + 1; i < n;
        	     i++, val1++, val2++) {
                if (*val1 != *val2) {
                	index++;
                	RAW(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index]++;
        	}
    	} else {
    		int *cnt;
        	INTEGER(buf_lengths)[0] = INTEGER(counts)[0];
        	for (i = 1, val1 = RAW(x), val2 = RAW(x) + 1,
        		 cnt = INTEGER(counts) + 1; i < n; i++, val1++, val2++, cnt++) {
                if (*val1 != *val2) {
                	index++;
                	RAW(buf_values)[index] = *val2;
                }
                INTEGER(buf_lengths)[index] += *cnt;
        	}
    	}
    	index++;
    	PROTECT(ans_values = NEW_RAW(index));
    	PROTECT(ans_lengths = NEW_INTEGER(index));
    	memcpy(RAW(ans_values), RAW(buf_values), index);
    	memcpy(INTEGER(ans_lengths), INTEGER(buf_lengths), index * sizeof(int));
    	unpro = 5;
    }

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, install("values"), ans_values);
	SET_SLOT(ans, install("lengths"), ans_lengths);
	UNPROTECT(unpro);

	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_constructor(SEXP x, SEXP counts)
{
	SEXP ans;

	if (LENGTH(counts) > 0 && (LENGTH(counts) != LENGTH(x)))
		error("'length(values)' != 'length(lengths)'");

	ans = R_NilValue;
	switch(TYPEOF(x)) {
    case LGLSXP:
    	PROTECT(ans = Rle_logical_constructor(x, counts));
    	break;
    case INTSXP:
    	PROTECT(ans = Rle_integer_constructor(x, counts));
    	break;
    case REALSXP:
    	PROTECT(ans = Rle_real_constructor(x, counts));
        break;
    case CPLXSXP:
    	PROTECT(ans = Rle_complex_constructor(x, counts));
        break;
    case STRSXP:
    	PROTECT(ans = Rle_string_constructor(x, counts));
        break;
    case RAWSXP:
    	PROTECT(ans = Rle_raw_constructor(x, counts));
        break;
    default:
		error("Rle computation of these types is not implemented");
	}
	UNPROTECT(1);
	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_start(SEXP x)
{
	int i, n, *len_elt, *prev_start, *curr_start;
	SEXP lengths, ans;

	lengths = GET_SLOT(x, install("lengths"));
	n = LENGTH(lengths);

	PROTECT(ans = NEW_INTEGER(n));

	if (n > 0) {
		INTEGER(ans)[0] = 1;
		for(i = 1, len_elt = INTEGER(lengths),
			prev_start = INTEGER(ans), curr_start = INTEGER(ans) + 1;
		    i < n; i++, len_elt++, prev_start++, curr_start++) {
			*curr_start = *prev_start + *len_elt;
		}
	}

	UNPROTECT(1);

	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_end(SEXP x)
{
	int i, n, *len_elt, *prev_end, *curr_end;
	SEXP lengths, ans;

	lengths = GET_SLOT(x, install("lengths"));
	n = LENGTH(lengths);

	PROTECT(ans = NEW_INTEGER(n));

	if (n > 0) {
		INTEGER(ans)[0] = INTEGER(lengths)[0];
		for(i = 1, len_elt = INTEGER(lengths) + 1,
			prev_end = INTEGER(ans), curr_end = INTEGER(ans) + 1;
		    i < n; i++, len_elt++, prev_end++, curr_end++) {
			*curr_end = *prev_end + *len_elt;
		}
	}

	UNPROTECT(1);

	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

SEXP Rle_window_aslist(SEXP x, SEXP runStart, SEXP runEnd,
		               SEXP offsetStart, SEXP offsetEnd)
{
	SEXP values, lengths, runWidth, ans, ans_names, ans_values, ans_lengths;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	if (!IS_INTEGER(runStart) || LENGTH(runStart) != 1 ||
		INTEGER(runStart)[0] == NA_INTEGER || INTEGER(runStart)[0] < 1)
		error("invalid 'runStart' argument");

	if (!IS_INTEGER(runEnd) || LENGTH(runEnd) != 1 ||
		INTEGER(runEnd)[0] == NA_INTEGER ||
		(INTEGER(runEnd)[0] + 1) < INTEGER(runStart)[0] ||
		INTEGER(runEnd)[0] > LENGTH(values))
		error("invalid 'runWidth' argument");

	PROTECT(runWidth = NEW_INTEGER(1));
	INTEGER(runWidth)[0] = INTEGER(runEnd)[0] - INTEGER(runStart)[0] + 1;

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));
	PROTECT(ans_values = vector_seqselect(values, runStart, runWidth));
	PROTECT(ans_lengths = vector_seqselect(lengths, runStart, runWidth));

    if (INTEGER(runWidth)[0] > 0) {
        INTEGER(ans_lengths)[0] -= INTEGER(offsetStart)[0];
    	INTEGER(ans_lengths)[INTEGER(runWidth)[0] - 1] -= INTEGER(offsetEnd)[0];
    }

	SET_VECTOR_ELT(ans, 0, ans_values);
	SET_VECTOR_ELT(ans, 1, ans_lengths);
	SET_STRING_ELT(ans_names, 0, mkChar("values"));
	SET_STRING_ELT(ans_names, 1, mkChar("lengths"));
	SET_NAMES(ans, ans_names);

	UNPROTECT(5);

	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */

/*
 * Rle_window accepts an Rle object to support fast R-level aggregate usage
 */
SEXP Rle_window(SEXP x, SEXP runStart, SEXP runEnd,
		        SEXP offsetStart, SEXP offsetEnd, SEXP ans)
{
	SEXP ans_list;

	PROTECT(ans_list = Rle_window_aslist(x, runStart, runEnd,
                                         offsetStart, offsetEnd));

	SET_SLOT(ans, install("values"), VECTOR_ELT(ans_list, 0));
	SET_SLOT(ans, install("lengths"), VECTOR_ELT(ans_list, 1));

	UNPROTECT(1);

	return ans;
}
