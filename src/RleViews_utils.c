#include "IRanges.h"
#include <R_ext/Arith.h>
#include <R_ext/Utils.h>
#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP RleViews_viewMins(SEXP x, SEXP na_rm)
{
	char type = '?';
	int i, start, width, ans_length, index, lower_run, upper_run, upper_bound;
	int max_index, *lengths_elt;
	SEXP ans, subject, values, lengths, ranges, names;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	ranges = GET_SLOT(x, install("ranges"));
	cached_ranges = _cache_IRanges(ranges);
	ans_length = _get_cachedIRanges_length(&cached_ranges);

	ans = R_NilValue;
	switch (TYPEOF(values)) {
    case LGLSXP:
    case INTSXP:
		type = 'i';
		PROTECT(ans = NEW_INTEGER(ans_length));
		break;
    case REALSXP:
		type = 'r';
		PROTECT(ans = NEW_NUMERIC(ans_length));
		break;
    default:
		error("Rle must contain either 'integer' or 'numeric' values");
    }

	if (!IS_LOGICAL(na_rm) || LENGTH(na_rm) != 1 || LOGICAL(na_rm)[0] == NA_LOGICAL)
		error("'na.rm' must be TRUE or FALSE");

	lengths_elt = INTEGER(lengths);
	max_index = LENGTH(lengths) - 1;
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0; i < ans_length; i++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		if (type == 'i') {
			INTEGER(ans)[i] = INT_MAX;
		} else if (type == 'r') {
			REAL(ans)[i] = R_PosInf;
		}
		if (width > 0) {
			while (index > 0 && upper_run > start) {
				upper_run -= *lengths_elt;
				lengths_elt--;
				index--;
			}
			while (upper_run < start) {
				lengths_elt++;
				index++;
				upper_run += *lengths_elt;
			}
			lower_run = upper_run - *lengths_elt + 1;
			upper_bound = start + width - 1;
			if (type == 'i') {
				while (lower_run <= upper_bound) {
					if (INTEGER(values)[index] == NA_INTEGER) {
						if (!LOGICAL(na_rm)[0]) {
							INTEGER(ans)[i] = NA_INTEGER;
							break;
						}
					} else if (INTEGER(values)[index] < INTEGER(ans)[i]) {
						INTEGER(ans)[i] = INTEGER(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			} else if (type == 'r') {
				while (lower_run <= upper_bound) {
					if (ISNAN(REAL(values)[index])) {
						if (!LOGICAL(na_rm)[0]) {
							REAL(ans)[i] = NA_REAL;
							break;
						}
					} else if (REAL(values)[index] < REAL(ans)[i]) {
						REAL(ans)[i] = REAL(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			}
		}
	}
	PROTECT(names = duplicate(_get_IRanges_names(ranges)));
	SET_NAMES(ans, names);
	UNPROTECT(2);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP RleViews_viewMaxs(SEXP x, SEXP na_rm)
{
	char type = '?';
	int i, start, width, ans_length, index, lower_run, upper_run, upper_bound;
	int max_index, *lengths_elt;
	SEXP ans, subject, values, lengths, ranges, names;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	ranges = GET_SLOT(x, install("ranges"));
	cached_ranges = _cache_IRanges(ranges);
	ans_length = _get_cachedIRanges_length(&cached_ranges);

	ans = R_NilValue;
	switch (TYPEOF(values)) {
    case LGLSXP:
    case INTSXP:
		type = 'i';
		PROTECT(ans = NEW_INTEGER(ans_length));
		break;
    case REALSXP:
		type = 'r';
		PROTECT(ans = NEW_NUMERIC(ans_length));
		break;
    default:
		error("Rle must contain either 'integer' or 'numeric' values");
    }

	if (!IS_LOGICAL(na_rm) || LENGTH(na_rm) != 1 || LOGICAL(na_rm)[0] == NA_LOGICAL)
		error("'na.rm' must be TRUE or FALSE");

	lengths_elt = INTEGER(lengths);
	max_index = LENGTH(lengths) - 1;
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0; i < ans_length; i++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		if (type == 'i') {
			INTEGER(ans)[i] = R_INT_MIN;
		} else if (type == 'r') {
			REAL(ans)[i] = R_NegInf;
		}
		if (width > 0) {
			while (index > 0 && upper_run > start) {
				upper_run -= *lengths_elt;
				lengths_elt--;
				index--;
			}
			while (upper_run < start) {
				lengths_elt++;
				index++;
				upper_run += *lengths_elt;
			}
			lower_run = upper_run - *lengths_elt + 1;
			upper_bound = start + width - 1;
			if (type == 'i') {
				while (lower_run <= upper_bound) {
					if (INTEGER(values)[index] == NA_INTEGER) {
						if (!LOGICAL(na_rm)[0]) {
							INTEGER(ans)[i] = NA_INTEGER;
							break;
						}
					} else if (INTEGER(values)[index] > INTEGER(ans)[i]) {
						INTEGER(ans)[i] = INTEGER(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			} else if (type == 'r') {
				while (lower_run <= upper_bound) {
					if (ISNAN(REAL(values)[index])) {
						if (!LOGICAL(na_rm)[0]) {
							REAL(ans)[i] = NA_REAL;
							break;
						}
					} else if (REAL(values)[index] > REAL(ans)[i]) {
						REAL(ans)[i] = REAL(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			}
		}
	}
	PROTECT(names = duplicate(_get_IRanges_names(ranges)));
	SET_NAMES(ans, names);
	UNPROTECT(2);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP RleViews_viewSums(SEXP x, SEXP na_rm)
{
	char type = '?';
	int i, start, width, ans_length, index,
	    lower_run, upper_run, lower_bound, upper_bound;
	int max_index, *lengths_elt;
	SEXP ans, subject, values, lengths, ranges, names;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	ranges = GET_SLOT(x, install("ranges"));
	cached_ranges = _cache_IRanges(ranges);
	ans_length = _get_cachedIRanges_length(&cached_ranges);

	ans = R_NilValue;
	switch (TYPEOF(values)) {
    case LGLSXP:
    case INTSXP:
		type = 'i';
		PROTECT(ans = NEW_INTEGER(ans_length));
		break;
    case REALSXP:
		type = 'r';
		PROTECT(ans = NEW_NUMERIC(ans_length));
		break;
    case CPLXSXP:
		type = 'c';
		PROTECT(ans = NEW_COMPLEX(ans_length));
		break;
    default:
		error("Rle must contain either 'integer', 'numeric', or 'complex' values");
    }

	if (!IS_LOGICAL(na_rm) || LENGTH(na_rm) != 1 || LOGICAL(na_rm)[0] == NA_LOGICAL)
		error("'na.rm' must be TRUE or FALSE");

	lengths_elt = INTEGER(lengths);
	max_index = LENGTH(lengths) - 1;
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0; i < ans_length; i++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		if (type == 'i') {
			INTEGER(ans)[i] = 0;
		} else if (type == 'r') {
			REAL(ans)[i] = 0;
		} else if (type == 'c') {
			COMPLEX(ans)[i].r = 0;
			COMPLEX(ans)[i].i = 0;
		}
		if (width > 0) {
			while (index > 0 && upper_run > start) {
				upper_run -= *lengths_elt;
				lengths_elt--;
				index--;
			}
			while (upper_run < start) {
				lengths_elt++;
				index++;
				upper_run += *lengths_elt;
			}
			lower_run = upper_run - *lengths_elt + 1;
			lower_bound = start;
			upper_bound = start + width - 1;
			if (type == 'i') {
				while (lower_run <= upper_bound) {
					if (INTEGER(values)[index] == NA_INTEGER) {
						if (!LOGICAL(na_rm)[0]) {
							INTEGER(ans)[i] = NA_INTEGER;
							break;
						}
					} else {
						INTEGER(ans)[i] += INTEGER(values)[index] *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
				if (INTEGER(ans)[i] != NA_INTEGER &&
					(INTEGER(ans)[i] > INT_MAX || INTEGER(ans)[i] < R_INT_MIN))
					error("Integer overflow");
			} else if (type == 'r') {
				while (lower_run <= upper_bound) {
					if (ISNAN(REAL(values)[index])) {
						if (!LOGICAL(na_rm)[0]) {
							REAL(ans)[i] = NA_REAL;
							break;
						}
					} else {
						REAL(ans)[i] += REAL(values)[index] *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			} else if (type == 'c') {
				while (lower_run <= upper_bound) {
					if (ISNAN(COMPLEX(values)[index].r) || ISNAN(COMPLEX(values)[index].i)) {
						if (!LOGICAL(na_rm)[0]) {
							COMPLEX(ans)[i].r = NA_REAL;
							COMPLEX(ans)[i].i = NA_REAL;
							break;
						}
					} else {
						COMPLEX(ans)[i].r += COMPLEX(values)[index].r *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
						COMPLEX(ans)[i].i += COMPLEX(values)[index].i *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			}
		}
	}
	PROTECT(names = duplicate(_get_IRanges_names(ranges)));
	SET_NAMES(ans, names);
	UNPROTECT(2);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP RleViews_viewMeans(SEXP x, SEXP na_rm)
{
	char type = '?';
	int i, n, start, width, ans_length, index,
	    lower_run, upper_run, lower_bound, upper_bound;
	int max_index, *lengths_elt;
	SEXP ans, subject, values, lengths, ranges, names;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	ranges = GET_SLOT(x, install("ranges"));
	cached_ranges = _cache_IRanges(ranges);
	ans_length = _get_cachedIRanges_length(&cached_ranges);

	ans = R_NilValue;
	switch (TYPEOF(values)) {
    case LGLSXP:
    case INTSXP:
		type = 'i';
		PROTECT(ans = NEW_NUMERIC(ans_length));
		break;
    case REALSXP:
		type = 'r';
		PROTECT(ans = NEW_NUMERIC(ans_length));
		break;
    case CPLXSXP:
		type = 'c';
		PROTECT(ans = NEW_COMPLEX(ans_length));
		break;
    default:
		error("Rle must contain either 'integer', 'numeric', or 'complex' values");
    }

	if (!IS_LOGICAL(na_rm) || LENGTH(na_rm) != 1 || LOGICAL(na_rm)[0] == NA_LOGICAL)
		error("'na.rm' must be TRUE or FALSE");

	lengths_elt = INTEGER(lengths);
	max_index = LENGTH(lengths) - 1;
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0; i < ans_length; i++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		if (width <= 0) {
			if (type == 'i') {
				REAL(ans)[i] = R_NaN;
			} else if (type == 'r') {
				REAL(ans)[i] = R_NaN;
			} else if (type == 'c') {
				COMPLEX(ans)[i].r = R_NaN;
				COMPLEX(ans)[i].i = R_NaN;
			}
		} else {
			n = width;
			if (type == 'i') {
				REAL(ans)[i] = 0;
			} else if (type == 'r') {
				REAL(ans)[i] = 0;
			} else if (type == 'c') {
				COMPLEX(ans)[i].r = 0;
				COMPLEX(ans)[i].i = 0;
			}
			while (index > 0 && upper_run > start) {
				upper_run -= *lengths_elt;
				lengths_elt--;
				index--;
			}
			while (upper_run < start) {
				lengths_elt++;
				index++;
				upper_run += *lengths_elt;
			}
			lower_run = upper_run - *lengths_elt + 1;
			lower_bound = start;
			upper_bound = start + width - 1;
			if (type == 'i') {
				while (lower_run <= upper_bound) {
					if (INTEGER(values)[index] == NA_INTEGER) {
						if (!LOGICAL(na_rm)[0]) {
							REAL(ans)[i] = NA_REAL;
							break;
						}
						n -=
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
						
					} else {
						REAL(ans)[i] += INTEGER(values)[index] *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
				if (n == 0) {
					REAL(ans)[i] = R_NaN;
				} else if (REAL(ans)[i] != NA_REAL) {
					REAL(ans)[i] /= n;
				}
			} else if (type == 'r') {
				while (lower_run <= upper_bound) {
					if (ISNAN(REAL(values)[index])) {
						if (!LOGICAL(na_rm)[0]) {
							REAL(ans)[i] = NA_REAL;
							break;
						}
						n -=
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					} else {
						REAL(ans)[i] += REAL(values)[index] *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
				if (n == 0) {
					REAL(ans)[i] = R_NaN;
				} else if (REAL(ans)[i] != NA_REAL) {
					REAL(ans)[i] /= n;
				}
			} else if (type == 'c') {
				while (lower_run <= upper_bound) {
					if (ISNAN(COMPLEX(values)[index].r) || ISNAN(COMPLEX(values)[index].i)) {
						if (!LOGICAL(na_rm)[0]) {
							COMPLEX(ans)[i].r = NA_REAL;
							COMPLEX(ans)[i].i = NA_REAL;
							break;
						}
						n -=
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					} else {
						COMPLEX(ans)[i].r += COMPLEX(values)[index].r *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
						COMPLEX(ans)[i].i += COMPLEX(values)[index].i *
						    (1 + (upper_bound < upper_run ? upper_bound : upper_run) -
						         (lower_bound > lower_run ? lower_bound : lower_run));
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
				if (n == 0) {
					COMPLEX(ans)[i].r = R_NaN;
					COMPLEX(ans)[i].i = R_NaN;
				} else if (COMPLEX(ans)[i].r != NA_REAL) {
					COMPLEX(ans)[i].r /= n;
					COMPLEX(ans)[i].i /= n;
				}
			}
		}
	}
	PROTECT(names = duplicate(_get_IRanges_names(ranges)));
	SET_NAMES(ans, names);
	UNPROTECT(2);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP RleViews_viewWhichMins(SEXP x, SEXP na_rm)
{
	char type = '?';
	int i, start, width, ans_length, index,
	    lower_run, upper_run, lower_bound, upper_bound;
	int max_index, *ans_elt, *lengths_elt;
	SEXP curr, ans, subject, values, lengths, ranges, names;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	ranges = GET_SLOT(x, install("ranges"));
	cached_ranges = _cache_IRanges(ranges);
	ans_length = _get_cachedIRanges_length(&cached_ranges);

	curr = R_NilValue;
	switch (TYPEOF(values)) {
    case LGLSXP:
    case INTSXP:
		type = 'i';
		PROTECT(curr = NEW_INTEGER(1));
		break;
    case REALSXP:
		type = 'r';
		PROTECT(curr = NEW_NUMERIC(1));
		break;
    default:
		error("Rle must contain either 'integer' or 'numeric' values");
    }

	if (!IS_LOGICAL(na_rm) || LENGTH(na_rm) != 1 || LOGICAL(na_rm)[0] == NA_LOGICAL)
		error("'na.rm' must be TRUE or FALSE");

	PROTECT(ans = NEW_INTEGER(ans_length));
	lengths_elt = INTEGER(lengths);
	max_index = LENGTH(lengths) - 1;
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = NA_INTEGER;
		if (width > 0) {
			if (type == 'i') {
				INTEGER(curr)[0] = INT_MAX;
			} else if (type == 'r') {
				REAL(curr)[0] = R_PosInf;
			}
			while (index > 0 && upper_run > start) {
				upper_run -= *lengths_elt;
				lengths_elt--;
				index--;
			}
			while (upper_run < start) {
				lengths_elt++;
				index++;
				upper_run += *lengths_elt;
			}
			lower_run = upper_run - *lengths_elt + 1;
			lower_bound = start;
			upper_bound = start + width - 1;
			if (type == 'i') {
				while (lower_run <= upper_bound) {
					if (INTEGER(values)[index] == NA_INTEGER) {
						if (!LOGICAL(na_rm)[0]) {
							break;
						}
					} else if (INTEGER(values)[index] < INTEGER(curr)[0]) {
						*ans_elt = lower_bound;
						INTEGER(curr)[0] = INTEGER(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			} else if (type == 'r') {
				while (lower_run <= upper_bound) {
					if (ISNAN(REAL(values)[index])) {
						if (!LOGICAL(na_rm)[0]) {
							break;
						}
					} else if (REAL(values)[index] < REAL(curr)[0]) {
						*ans_elt = lower_bound;
						REAL(curr)[0] = REAL(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			}
		}
	}
	PROTECT(names = duplicate(_get_IRanges_names(ranges)));
	SET_NAMES(ans, names);
	UNPROTECT(3);
	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP RleViews_viewWhichMaxs(SEXP x, SEXP na_rm)
{
	char type = '?';
	int i, start, width, ans_length, index,
	    lower_run, upper_run, lower_bound, upper_bound;
	int max_index, *ans_elt, *lengths_elt;
	SEXP curr, ans, subject, values, lengths, ranges, names;
	cachedIRanges cached_ranges;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	ranges = GET_SLOT(x, install("ranges"));
	cached_ranges = _cache_IRanges(ranges);
	ans_length = _get_cachedIRanges_length(&cached_ranges);

	curr = R_NilValue;
	switch (TYPEOF(values)) {
    case LGLSXP:
    case INTSXP:
		type = 'i';
		PROTECT(curr = NEW_INTEGER(1));
		break;
    case REALSXP:
		type = 'r';
		PROTECT(curr = NEW_NUMERIC(1));
		break;
    default:
		error("Rle must contain either 'integer' or 'numeric' values");
    }

	if (!IS_LOGICAL(na_rm) || LENGTH(na_rm) != 1 || LOGICAL(na_rm)[0] == NA_LOGICAL)
		error("'na.rm' must be TRUE or FALSE");

	PROTECT(ans = NEW_INTEGER(ans_length));
	lengths_elt = INTEGER(lengths);
	max_index = LENGTH(lengths) - 1;
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans); i < ans_length; i++, ans_elt++) {
		if (i % 100000 == 99999)
			R_CheckUserInterrupt();
		start = _get_cachedIRanges_elt_start(&cached_ranges, i);
		width = _get_cachedIRanges_elt_width(&cached_ranges, i);
		*ans_elt = NA_INTEGER;
		if (width > 0) {
			if (type == 'i') {
				INTEGER(curr)[0] = R_INT_MIN;
			} else if (type == 'r') {
				REAL(curr)[0] = R_NegInf;
			}
			while (index > 0 && upper_run > start) {
				upper_run -= *lengths_elt;
				lengths_elt--;
				index--;
			}
			while (upper_run < start) {
				lengths_elt++;
				index++;
				upper_run += *lengths_elt;
			}
			lower_run = upper_run - *lengths_elt + 1;
			lower_bound = start;
			upper_bound = start + width - 1;
			if (type == 'i') {
				while (lower_run <= upper_bound) {
					if (INTEGER(values)[index] == NA_INTEGER) {
						if (!LOGICAL(na_rm)[0]) {
							break;
						}
					} else if (INTEGER(values)[index] > INTEGER(curr)[0]) {
						*ans_elt = lower_bound;
						INTEGER(curr)[0] = INTEGER(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			} else if (type == 'r') {
				while (lower_run <= upper_bound) {
					if (ISNAN(REAL(values)[index])) {
						if (!LOGICAL(na_rm)[0]) {
							break;
						}
					} else if (REAL(values)[index] > REAL(curr)[0]) {
						*ans_elt = lower_bound;
						REAL(curr)[0] = REAL(values)[index];
					}
					if (index < max_index) {
						lengths_elt++;
						index++;
						lower_run = upper_run + 1;
						lower_bound = lower_run;
						upper_run += *lengths_elt;
					} else {
						break;
					}
				}
			}
		}
	}
	PROTECT(names = duplicate(_get_IRanges_names(ranges)));
	SET_NAMES(ans, names);
	UNPROTECT(3);
	return ans;
}
