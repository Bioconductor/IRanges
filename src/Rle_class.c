#include "IRanges.h"


static SEXP _new_Rle(SEXP values, SEXP lengths)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS("Rle"));
	PROTECT(ans = NEW_OBJECT(classdef));
	SET_SLOT(ans, install("values"), values);
	SET_SLOT(ans, install("lengths"), lengths);
	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 * The compute_<run-type>_runs() low-level helper functions.
 *
 * To compute only the nb of runs without actually computing the runs
 * (degraded mode), set 'run_lengths' to NULL.
 */

static int compute_int_runs(const int *values, int nvalues,
		const int *lengths,
		int *run_values, int *run_lengths)
{
	int i, nrun, lengths_elt;
	int val0;

	for (i = nrun = 0, lengths_elt = 1; i < nvalues; i++, values++) {
		if (lengths != NULL) {
			lengths_elt = lengths[i];
			if (lengths_elt == 0)
				continue;
		}
		if (nrun != 0 && *values == val0) {
			if (run_lengths != NULL)
				run_lengths[nrun - 1] += lengths_elt;
			continue;
		}
		val0 = *values;
		if (run_lengths != NULL) {
			run_lengths[nrun] = lengths_elt;
			run_values[nrun] = val0;
		}
		nrun++;
	}
	return nrun;
}

static int compute_double_runs(const double *values, int nvalues,
		const int *lengths,
		double *run_values, int *run_lengths)
{
	int i, nrun, lengths_elt;
	double val0;

	for (i = nrun = 0, lengths_elt = 1; i < nvalues; i++, values++) {
		if (lengths != NULL) {
			lengths_elt = lengths[i];
			if (lengths_elt == 0)
				continue;
		}
		if (nrun != 0 && ((*values == val0) ||
				  (R_IsNA(*values) && R_IsNA(val0)) ||
				  (R_IsNaN(*values) && R_IsNaN(val0))))
		{
			if (run_lengths != NULL)
				run_lengths[nrun - 1] += lengths_elt;
			continue;
		}
		val0 = *values;
		if (run_lengths != NULL) {
			run_lengths[nrun] = lengths_elt;
			run_values[nrun] = val0;
		}
		nrun++;
	}
	return nrun;
}

static int compute_Rcomplex_runs(const Rcomplex *values, int nvalues,
		const int *lengths,
		Rcomplex *run_values, int *run_lengths)
{
	int i, nrun, lengths_elt;
	Rcomplex val0;

	for (i = nrun = 0, lengths_elt = 1; i < nvalues; i++, values++) {
		if (lengths != NULL) {
			lengths_elt = lengths[i];
			if (lengths_elt == 0)
				continue;
		}
		if (nrun != 0 && ((values->r == val0.r) ||
				  (R_IsNA(values->r) && R_IsNA(val0.r)) ||
				  (R_IsNaN(values->r) && R_IsNaN(val0.r)))
			      && ((values->i == val0.i) ||
				  (R_IsNA(values->i) && R_IsNA(val0.i)) ||
				  (R_IsNaN(values->i) && R_IsNaN(val0.i))))
		{
			if (run_lengths != NULL)
				run_lengths[nrun - 1] += lengths_elt;
			continue;
		}
		val0 = *values;
		if (run_lengths != NULL) {
			run_lengths[nrun] = lengths_elt;
			run_values[nrun] = val0;
		}
		nrun++;
	}
	return nrun;
}

static int compute_CHARSXP_runs(SEXP values,
		const int *lengths,
		SEXP run_values, int *run_lengths)
{
	int nvalues, i, nrun, lengths_elt;
	SEXP values_elt, val0;

	nvalues = LENGTH(values);
	for (i = nrun = 0, lengths_elt = 1; i < nvalues; i++) {
		if (lengths != NULL) {
			lengths_elt = lengths[i];
			if (lengths_elt == 0)
				continue;
		}
		values_elt = STRING_ELT(values, i);
		if (nrun != 0 && values_elt == val0) {
			if (run_lengths != NULL)
				run_lengths[nrun - 1] += lengths_elt;
			continue;
		}
		val0 = values_elt;
		if (run_lengths != NULL) {
			run_lengths[nrun] = lengths_elt;
			SET_STRING_ELT(run_values, nrun, val0);
		}
		nrun++;
	}
	return nrun;
}

static int compute_Rbyte_runs(const Rbyte *values, int nvalues,
		const int *lengths,
		Rbyte *run_values, int *run_lengths)
{
	int i, nrun, lengths_elt;
	Rbyte val0;

	for (i = nrun = 0, lengths_elt = 1; i < nvalues; i++, values++) {
		if (lengths != NULL) {
			lengths_elt = lengths[i];
			if (lengths_elt == 0)
				continue;
		}
		if (nrun != 0 && *values == val0) {
			if (run_lengths != NULL)
				run_lengths[nrun - 1] += lengths_elt;
			continue;
		}
		val0 = *values;
		if (run_lengths != NULL) {
			run_lengths[nrun] = lengths_elt;
			run_values[nrun] = val0;
		}
		nrun++;
	}
	return nrun;
}


/****************************************************************************
 * The C level Rle smart constructors.
 *
 * 'lengths' must be either (a) an int array of length 'nvalues' with no NA
 * or negative values, or (b) NULL. If (b) then it's treated as an array of
 * length 'nvalues' filled with 1's (i.e. each element is set to 1).
 * 'buflength' is the length of the temporary buffers allocated internally by
 * the smart constructor for computing the runs. If set to 0, then a 2-pass
 * algo is used that doesn't use any temporary buffer, typically leading to
 * 20%-30% less memory used (it also seems slightly faster on my machine).
 * Setting 'buflength' to 'nvalues' is safe because the number of runs can
 * only be <= 'nvalues'. If 'buflength' is > 'nvalues', then 'nvalues' is used
 * instead.
 * WARNING: Avoid using a 'buflength' that is > 0 and < 'nvalues' unless you
 * know what you are doing!
 */

SEXP _logical_Rle_constructor(const int *values, int nvalues,
		const int *lengths, int buflength)
{
	int nrun, *buf_lengths;
	int *buf_values;
	SEXP ans_lengths, ans_values, ans;

	if (buflength > nvalues)
		buflength = nvalues;
	if (buflength == 0) {
		/* 1st pass: compute only the nb of runs */
		buf_values = NULL;
		buf_lengths = NULL;
	} else {
		buf_values = (int *) R_alloc(buflength, sizeof(int));
		buf_lengths = (int *) R_alloc(buflength, sizeof(int));
	}
	nrun = compute_int_runs(values, nvalues, lengths,
				buf_values, buf_lengths);
	PROTECT(ans_values = NEW_LOGICAL(nrun));
	PROTECT(ans_lengths = NEW_INTEGER(nrun));
	if (buflength == 0) {
		/* 2nd pass: fill 'ans_values' and 'ans_lengths' */
		compute_int_runs(values, nvalues, lengths,
				LOGICAL(ans_values), INTEGER(ans_lengths));
	} else {
		memcpy(LOGICAL(ans_values), buf_values, nrun * sizeof(int));
		memcpy(INTEGER(ans_lengths), buf_lengths, nrun * sizeof(int));
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _integer_Rle_constructor(const int *values, int nvalues,
		const int *lengths, int buflength)
{
	int nrun, *buf_lengths;
	int *buf_values;
	SEXP ans_lengths, ans_values, ans;

	if (buflength > nvalues)
		buflength = nvalues;
	if (buflength == 0) {
		/* 1st pass: compute only the nb of runs */
		buf_values = NULL;
		buf_lengths = NULL;
	} else {
		buf_values = (int *) R_alloc(buflength, sizeof(int));
		buf_lengths = (int *) R_alloc(buflength, sizeof(int));
	}
	nrun = compute_int_runs(values, nvalues, lengths,
				buf_values, buf_lengths);
	PROTECT(ans_values = NEW_INTEGER(nrun));
	PROTECT(ans_lengths = NEW_INTEGER(nrun));
	if (buflength == 0) {
		/* 2nd pass: fill 'ans_values' and 'ans_lengths' */
		compute_int_runs(values, nvalues, lengths,
				INTEGER(ans_values), INTEGER(ans_lengths));
	} else {
		memcpy(INTEGER(ans_values), buf_values, nrun * sizeof(int));
		memcpy(INTEGER(ans_lengths), buf_lengths, nrun * sizeof(int));
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _numeric_Rle_constructor(const double *values, int nvalues,
		const int *lengths, int buflength)
{
	int nrun, *buf_lengths;
	double *buf_values;
	SEXP ans_lengths, ans_values, ans;

	if (buflength > nvalues)
		buflength = nvalues;
	if (buflength == 0) {
		/* 1st pass: compute only the nb of runs */
		buf_values = NULL;
		buf_lengths = NULL;
	} else {
		buf_values = (double *) R_alloc(buflength, sizeof(double));
		buf_lengths = (int *) R_alloc(buflength, sizeof(int));
	}
	nrun = compute_double_runs(values, nvalues, lengths,
				buf_values, buf_lengths);
	PROTECT(ans_values = NEW_NUMERIC(nrun));
	PROTECT(ans_lengths = NEW_INTEGER(nrun));
	if (buflength == 0) {
		/* 2nd pass: fill 'ans_values' and 'ans_lengths' */
		compute_double_runs(values, nvalues, lengths,
				REAL(ans_values), INTEGER(ans_lengths));
	} else {
		memcpy(REAL(ans_values), buf_values, nrun * sizeof(double));
		memcpy(INTEGER(ans_lengths), buf_lengths, nrun * sizeof(int));
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _complex_Rle_constructor(const Rcomplex *values, int nvalues,
		const int *lengths, int buflength)
{
	int nrun, *buf_lengths;
	Rcomplex *buf_values;
	SEXP ans_lengths, ans_values, ans;

	if (buflength > nvalues)
		buflength = nvalues;
	if (buflength == 0) {
		/* 1st pass: compute only the nb of runs */
		buf_values = NULL;
		buf_lengths = NULL;
	} else {
		buf_values = (Rcomplex *) R_alloc(buflength, sizeof(Rcomplex));
		buf_lengths = (int *) R_alloc(buflength, sizeof(int));
	}
	nrun = compute_Rcomplex_runs(values, nvalues, lengths,
				buf_values, buf_lengths);
	PROTECT(ans_values = NEW_COMPLEX(nrun));
	PROTECT(ans_lengths = NEW_INTEGER(nrun));
	if (buflength == 0) {
		/* 2nd pass: fill 'ans_values' and 'ans_lengths' */
		compute_Rcomplex_runs(values, nvalues, lengths,
				COMPLEX(ans_values), INTEGER(ans_lengths));
	} else {
		memcpy(COMPLEX(ans_values), buf_values,
					    nrun * sizeof(Rcomplex));
		memcpy(INTEGER(ans_lengths), buf_lengths, nrun * sizeof(int));
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}

SEXP _character_Rle_constructor(SEXP values,
		const int *lengths, int buflength)
{
	int nvalues, nrun, *buf_lengths, i;
	SEXP buf_values, ans_lengths, ans_values, ans;

	nvalues = LENGTH(values);
	if (buflength > nvalues)
		buflength = nvalues;
	if (buflength == 0) {
		/* 1st pass: compute only the nb of runs */
		buf_values = NULL;
		buf_lengths = NULL;
	} else {
		PROTECT(buf_values = NEW_CHARACTER(buflength));
		buf_lengths = (int *) R_alloc(buflength, sizeof(int));
	}
	nrun = compute_CHARSXP_runs(values, lengths,
				buf_values, buf_lengths);
	PROTECT(ans_values = NEW_CHARACTER(nrun));
	PROTECT(ans_lengths = NEW_INTEGER(nrun));
	if (buflength == 0) {
		/* 2nd pass: fill 'ans_values' and 'ans_lengths' */
		compute_CHARSXP_runs(values, lengths,
				ans_values, INTEGER(ans_lengths));
	} else {
		for (i = 0; i < nrun; i++)
		    SET_STRING_ELT(ans_values, i, STRING_ELT(buf_values, i));
		memcpy(INTEGER(ans_lengths), buf_lengths, nrun * sizeof(int));
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(buflength == 0 ? 3 : 4);
	return ans;
}

SEXP _raw_Rle_constructor(const Rbyte *values, int nvalues,
		const int *lengths, int buflength)
{
	int nrun, *buf_lengths;
	Rbyte *buf_values;
	SEXP ans_lengths, ans_values, ans;

	if (buflength > nvalues)
		buflength = nvalues;
	if (buflength == 0) {
		/* 1st pass: compute only the nb of runs */
		buf_values = NULL;
		buf_lengths = NULL;
	} else {
		buf_values = (Rbyte *) R_alloc(buflength, sizeof(Rbyte));
		buf_lengths = (int *) R_alloc(buflength, sizeof(int));
	}
	nrun = compute_Rbyte_runs(values, nvalues, lengths,
				buf_values, buf_lengths);
	PROTECT(ans_values = NEW_RAW(nrun));
	PROTECT(ans_lengths = NEW_INTEGER(nrun));
	if (buflength == 0) {
		/* 2nd pass: fill 'ans_values' and 'ans_lengths' */
		compute_Rbyte_runs(values, nvalues, lengths,
				RAW(ans_values), INTEGER(ans_lengths));
	} else {
		memcpy(RAW(ans_values), buf_values, nrun * sizeof(Rbyte));
		memcpy(INTEGER(ans_lengths), buf_lengths, nrun * sizeof(int));
	}
	PROTECT(ans = _new_Rle(ans_values, ans_lengths));
	UNPROTECT(3);
	return ans;
}


/****************************************************************************
 * The Rle constructor (.Call ENTRY POINT).
 */

SEXP Rle_constructor(SEXP values, SEXP lengths, SEXP check, SEXP buflength)
{
	int nvalues, buflength0;
	const int *lengths_p;

	nvalues = LENGTH(values);
	if (LOGICAL(check)[0] && LENGTH(lengths) > 0) {
		if (LENGTH(lengths) != nvalues)
			error("'length(lengths)' != 'length(values)'");
		_sum_non_neg_ints(INTEGER(lengths), LENGTH(lengths),
				  "lengths");
	}
	lengths_p = LENGTH(lengths) > 0 ? INTEGER(lengths) : NULL;
	buflength0 = INTEGER(buflength)[0];
	switch (TYPEOF(values)) {
	    case LGLSXP:
		return _logical_Rle_constructor(LOGICAL(values), nvalues,
						lengths_p, buflength0);
	    case INTSXP:
		return _integer_Rle_constructor(INTEGER(values), nvalues,
						lengths_p, buflength0);
	    case REALSXP:
		return _numeric_Rle_constructor(REAL(values), nvalues,
						lengths_p, buflength0);
	    case CPLXSXP:
		return _complex_Rle_constructor(COMPLEX(values), nvalues,
						lengths_p, buflength0);
	    case STRSXP:
		return _character_Rle_constructor(values,
						lengths_p, buflength0);
	    case RAWSXP:
		return _raw_Rle_constructor(RAW(values), nvalues,
						lengths_p, buflength0);
	}
	error("Rle of type '%s' is not supported",
	      CHAR(type2str(TYPEOF(values))));
	return R_NilValue;
}


/****************************************************************************
 * The Rle start() and end() getters (.Call ENTRY POINTS).
 */

SEXP Rle_start(SEXP x)
{
	int i, nrun, *len_elt, *prev_start, *curr_start;
	SEXP lengths, ans;

	lengths = GET_SLOT(x, install("lengths"));
	nrun = LENGTH(lengths);

	PROTECT(ans = NEW_INTEGER(nrun));

	if (nrun > 0) {
		INTEGER(ans)[0] = 1;
		for(i = 1, len_elt = INTEGER(lengths),
			prev_start = INTEGER(ans), curr_start = INTEGER(ans) + 1;
		    i < nrun; i++, len_elt++, prev_start++, curr_start++) {
			*curr_start = *prev_start + *len_elt;
		}
	}

	UNPROTECT(1);

	return ans;
}

SEXP Rle_end(SEXP x)
{
	int i, nrun, *len_elt, *prev_end, *curr_end;
	SEXP lengths, ans;

	lengths = GET_SLOT(x, install("lengths"));
	nrun = LENGTH(lengths);

	PROTECT(ans = NEW_INTEGER(nrun));

	if (nrun > 0) {
		INTEGER(ans)[0] = INTEGER(lengths)[0];
		for(i = 1, len_elt = INTEGER(lengths) + 1,
			prev_end = INTEGER(ans), curr_end = INTEGER(ans) + 1;
		    i < nrun; i++, len_elt++, prev_end++, curr_end++) {
			*curr_end = *prev_end + *len_elt;
		}
	}

	UNPROTECT(1);

	return ans;
}


/****************************************************************************
 * Rle_getStartEndRunAndOffset()
 */

static SEXP get_StartEndRunAndOffset_from_runLength(
		const int *runlength, int runlength_len,
		const int *start, const int *end, int length)
{
	int i, *soff_elt, *eoff_elt;
	const int *start_elt, *end_elt, *erun_elt;
	SEXP info_start, info_end, ans, ans_names;
	SEXP ans_start, ans_start_names, ans_end, ans_end_names;
	SEXP start_run, start_offset, end_run, end_offset;

	PROTECT(info_start = _find_interv_and_start_from_width(start, length,
					runlength, runlength_len));
	PROTECT(info_end = _find_interv_and_start_from_width(end, length,
					runlength, runlength_len));

	start_run = VECTOR_ELT(info_start, 0);
	start_offset = VECTOR_ELT(info_start, 1);
	end_run = VECTOR_ELT(info_end, 0);
	end_offset = VECTOR_ELT(info_end, 1);
	for (i = 0, start_elt = start, end_elt = end,
		    soff_elt = INTEGER(start_offset),
		    eoff_elt = INTEGER(end_offset),
		    erun_elt = INTEGER(end_run);
	     i < length;
	     i++, start_elt++, end_elt++, soff_elt++, eoff_elt++, erun_elt++)
	{
		*soff_elt = *start_elt - *soff_elt;
		*eoff_elt = *eoff_elt + runlength[*erun_elt - 1] - 1 - *end_elt;
	}

	PROTECT(ans_start = NEW_LIST(2));
	PROTECT(ans_start_names = NEW_CHARACTER(2));
	SET_VECTOR_ELT(ans_start, 0, start_run);
	SET_VECTOR_ELT(ans_start, 1, start_offset);
	SET_STRING_ELT(ans_start_names, 0, mkChar("run"));
	SET_STRING_ELT(ans_start_names, 1, mkChar("offset"));
	SET_NAMES(ans_start, ans_start_names);

	PROTECT(ans_end = NEW_LIST(2));
	PROTECT(ans_end_names = NEW_CHARACTER(2));
	SET_VECTOR_ELT(ans_end, 0, end_run);
	SET_VECTOR_ELT(ans_end, 1, end_offset);
	SET_STRING_ELT(ans_end_names, 0, mkChar("run"));
	SET_STRING_ELT(ans_end_names, 1, mkChar("offset"));
	SET_NAMES(ans_end, ans_end_names);

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));
	SET_VECTOR_ELT(ans, 0, ans_start);
	SET_VECTOR_ELT(ans, 1, ans_end);
	SET_STRING_ELT(ans_names, 0, mkChar("start"));
	SET_STRING_ELT(ans_names, 1, mkChar("end"));
	SET_NAMES(ans, ans_names);

	UNPROTECT(8);

	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_getStartEndRunAndOffset(SEXP x, SEXP start, SEXP end)
{
	int n;
	SEXP lengths;

	n = LENGTH(start);
	if (LENGTH(end) != n)
		error("length of 'start' must equal length of 'end'");
	lengths = GET_SLOT(x, install("lengths"));
	return get_StartEndRunAndOffset_from_runLength(
			INTEGER(lengths), LENGTH(lengths),
			INTEGER(start), INTEGER(end), n);
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

	ans = Rf_duplicate(ans);
	SET_SLOT(ans, install("values"), VECTOR_ELT(ans_list, 0));
	SET_SLOT(ans, install("lengths"), VECTOR_ELT(ans_list, 1));

	UNPROTECT(1);

	return ans;
}


/****************************************************************************
 * Rle_seqselect()
 */

SEXP _seqselect_Rle(SEXP x, const int *start, const int *width, int length)
{
	int i, index, *end_elt, *width_run_elt, *len_elt;
	const int *start_elt, *width_elt, *soff_elt, *eoff_elt;
	SEXP values, lengths, end;
	SEXP info, info_start, info_end;
	SEXP start_run, end_run, width_run, start_offset, end_offset;
	SEXP ans, ans_names, ans_values, ans_lengths;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	PROTECT(end = NEW_INTEGER(length));
	for (i = 0, start_elt = start,
		    end_elt = INTEGER(end),
		    width_elt = width;
	     i < length;
	     i++, start_elt++, end_elt++, width_elt++)
	{
		*end_elt = *start_elt + *width_elt - 1;
	}

	PROTECT(info = get_StartEndRunAndOffset_from_runLength(
					INTEGER(lengths), LENGTH(lengths),
					start, INTEGER(end), length));
	info_start = VECTOR_ELT(info, 0);
	start_run = VECTOR_ELT(info_start, 0);
	start_offset = VECTOR_ELT(info_start, 1);
	info_end = VECTOR_ELT(info, 1);
	end_run = VECTOR_ELT(info_end, 0);
	end_offset = VECTOR_ELT(info_end, 1);

	PROTECT(width_run = NEW_INTEGER(length));
	for (i = 0, start_elt = INTEGER(start_run),
		    end_elt = INTEGER(end_run),
		    width_run_elt = INTEGER(width_run);
	     i < length;
	     i++, start_elt++, end_elt++, width_run_elt++)
	{
		*width_run_elt = *end_elt - *start_elt + 1;
	}

	PROTECT(ans_values = vector_seqselect(values, start_run, width_run));
	PROTECT(ans_lengths = vector_seqselect(lengths, start_run, width_run));

	index = 0;
	len_elt = INTEGER(ans_lengths);
	for (i = 0, soff_elt = INTEGER(start_offset),
		    eoff_elt = INTEGER(end_offset),
		    width_elt = INTEGER(width_run);
	     i < length;
	     i++, soff_elt++, eoff_elt++, width_elt++)
	{
		if (*width_elt > 0) {
			len_elt[index] -= *soff_elt;
			index += *width_elt;
			len_elt[index - 1] -= *eoff_elt;
		}
	}

	PROTECT(ans = NEW_LIST(2));
	PROTECT(ans_names = NEW_CHARACTER(2));

	SET_VECTOR_ELT(ans, 0, ans_values);
	SET_VECTOR_ELT(ans, 1, ans_lengths);
	SET_STRING_ELT(ans_names, 0, mkChar("values"));
	SET_STRING_ELT(ans_names, 1, mkChar("lengths"));
	SET_NAMES(ans, ans_names);

	UNPROTECT(7);

	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP Rle_seqselect(SEXP x, SEXP start, SEXP width)
{
	int n;

	n = LENGTH(start);
	if (LENGTH(width) != n)
		error("length of 'start' must equal length of 'width'");
	return _seqselect_Rle(x, INTEGER(start), INTEGER(width), n);
}

