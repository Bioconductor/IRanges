#include "IRanges.h"

#include <limits.h>

#define R_INT_MIN	(1+INT_MIN)

/*
 * --- .Call ENTRY POINT ---
 */
SEXP RleViews_viewMins(SEXP x, SEXP na_rm)
{
	char type = '?';
	int i, ans_length, index, lower_run, upper_run, upper_bound;
	int *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, lengths, start, width, names;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	start = _get_IRanges_start(x);
	width = _get_IRanges_width(x);
	ans_length = _get_IRanges_length(x);

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

	lengths_elt = INTEGER(lengths);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, start_elt++, width_elt++)
	{
		if (type == 'i') {
			INTEGER(ans)[i] = INT_MAX;
		} else if (type == 'r') {
			REAL(ans)[i] = R_PosInf;
		}
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		upper_bound = *start_elt + *width_elt - 1;
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
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				upper_run += *lengths_elt;
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
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				upper_run += *lengths_elt;
			}
		}
	}
	PROTECT(names = duplicate(GET_SLOT(x, install("NAMES"))));
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
	int i, ans_length, index, lower_run, upper_run, upper_bound;
	int *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, lengths, start, width, names;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	start = _get_IRanges_start(x);
	width = _get_IRanges_width(x);
	ans_length = _get_IRanges_length(x);

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

	lengths_elt = INTEGER(lengths);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, start_elt++, width_elt++)
	{
		if (type == 'i') {
			INTEGER(ans)[i] = R_INT_MIN;
		} else if (type == 'r') {
			REAL(ans)[i] = R_NegInf;
		}
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		upper_bound = *start_elt + *width_elt - 1;
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
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				upper_run += *lengths_elt;
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
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				upper_run += *lengths_elt;
			}
		}
	}
	PROTECT(names = duplicate(GET_SLOT(x, install("NAMES"))));
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
	int i, ans_length, index, lower_run, upper_run, lower_bound, upper_bound;
	int *lengths_elt, *start_elt, *width_elt;
	SEXP ans, subject, values, lengths, start, width, names;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	start = _get_IRanges_start(x);
	width = _get_IRanges_width(x);
	ans_length = _get_IRanges_length(x);

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

	lengths_elt = INTEGER(lengths);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, start_elt++, width_elt++)
	{
		if (type == 'i') {
			INTEGER(ans)[i] = 0;
		} else if (type == 'r') {
			REAL(ans)[i] = 0;
		} else if (type == 'c') {
			COMPLEX(ans)[i].r = 0;
			COMPLEX(ans)[i].i = 0;
		}
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		lower_bound = *start_elt;
		upper_bound = *start_elt + *width_elt - 1;
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
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				lower_bound = lower_run;
				upper_run += *lengths_elt;
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
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				lower_bound = lower_run;
				upper_run += *lengths_elt;
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
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				lower_bound = lower_run;
				upper_run += *lengths_elt;
			}
		}
	}
	PROTECT(names = duplicate(GET_SLOT(x, install("NAMES"))));
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
	int i, ans_length, index, lower_run, upper_run, lower_bound, upper_bound;
	int *ans_elt, *lengths_elt, *start_elt, *width_elt;
	SEXP curr, ans, subject, values, lengths, start, width, names;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	start = _get_IRanges_start(x);
	width = _get_IRanges_width(x);
	ans_length = _get_IRanges_length(x);

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

	PROTECT(ans = NEW_INTEGER(ans_length));
	lengths_elt = INTEGER(lengths);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		if (type == 'i') {
			INTEGER(curr)[0] = INT_MAX;
		} else if (type == 'r') {
			REAL(curr)[0] = R_PosInf;
		}
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		lower_bound = *start_elt;
		upper_bound = *start_elt + *width_elt - 1;
		if (type == 'i') {
			while (lower_run <= upper_bound) {
				if (INTEGER(values)[index] == NA_INTEGER) {
					if (!LOGICAL(na_rm)[0]) {
						*ans_elt = NA_INTEGER;
						break;
					}
				} else if (INTEGER(values)[index] < INTEGER(curr)[0]) {
					*ans_elt = lower_bound;
					INTEGER(curr)[0] = INTEGER(values)[index];
				}
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				lower_bound = lower_run;
				upper_run += *lengths_elt;
			}
		} else if (type == 'r') {
			while (lower_run <= upper_bound) {
				if (ISNAN(REAL(values)[index])) {
					if (!LOGICAL(na_rm)[0]) {
						*ans_elt = NA_REAL;
						break;
					}
				} else if (REAL(values)[index] < REAL(curr)[0]) {
					*ans_elt = lower_bound;
					REAL(curr)[0] = REAL(values)[index];
				}
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				lower_bound = lower_run;
				upper_run += *lengths_elt;
			}
		}
	}
	PROTECT(names = duplicate(GET_SLOT(x, install("NAMES"))));
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
	int i, ans_length, index, lower_run, upper_run, lower_bound, upper_bound;
	int *ans_elt, *lengths_elt, *start_elt, *width_elt;
	SEXP curr, ans, subject, values, lengths, start, width, names;

	subject = GET_SLOT(x, install("subject"));
	values = GET_SLOT(subject, install("values"));
	lengths = GET_SLOT(subject, install("lengths"));
	start = _get_IRanges_start(x);
	width = _get_IRanges_width(x);
	ans_length = _get_IRanges_length(x);

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

	PROTECT(ans = NEW_INTEGER(ans_length));
	lengths_elt = INTEGER(lengths);
	index = 0;
	upper_run = *lengths_elt;
	for (i = 0, ans_elt = INTEGER(ans), start_elt = INTEGER(start), width_elt = INTEGER(width);
	     i < ans_length;
	     i++, ans_elt++, start_elt++, width_elt++)
	{
		if (type == 'i') {
			INTEGER(curr)[0] = R_INT_MIN;
		} else if (type == 'r') {
			REAL(curr)[0] = R_NegInf;
		}
		while (index > 0 && upper_run > *start_elt) {
			upper_run -= *lengths_elt;
			lengths_elt--;
			index--;
		}
		while (upper_run < *start_elt) {
			lengths_elt++;
			index++;
			upper_run += *lengths_elt;
		}
		lower_run = upper_run - *lengths_elt + 1;
		lower_bound = *start_elt;
		upper_bound = *start_elt + *width_elt - 1;
		if (type == 'i') {
			while (lower_run <= upper_bound) {
				if (INTEGER(values)[index] == NA_INTEGER) {
					if (!LOGICAL(na_rm)[0]) {
						*ans_elt = NA_INTEGER;
						break;
					}
				} else if (INTEGER(values)[index] > INTEGER(curr)[0]) {
					*ans_elt = lower_bound;
					INTEGER(curr)[0] = INTEGER(values)[index];
				}
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				lower_bound = lower_run;
				upper_run += *lengths_elt;
			}
		} else if (type == 'r') {
			while (lower_run <= upper_bound) {
				if (ISNAN(REAL(values)[index])) {
					if (!LOGICAL(na_rm)[0]) {
						*ans_elt = NA_REAL;
						break;
					}
				} else if (REAL(values)[index] > REAL(curr)[0]) {
					*ans_elt = lower_bound;
					REAL(curr)[0] = REAL(values)[index];
				}
				lengths_elt++;
				index++;
				lower_run = upper_run + 1;
				lower_bound = lower_run;
				upper_run += *lengths_elt;
			}
		}
	}
	PROTECT(names = duplicate(GET_SLOT(x, install("NAMES"))));
	SET_NAMES(ans, names);
	UNPROTECT(3);
	return ans;
}
