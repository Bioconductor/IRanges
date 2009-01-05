#include "IRanges.h"

/*
 * --- .Call ENTRY POINT ---
 */
SEXP Rle_run_subseq(SEXP x, SEXP runStart, SEXP runEnd,
		            SEXP offsetStart, SEXP offsetEnd)
{
	SEXP values, lengths, runWidth, ans, ans_values, ans_lengths;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	if (!IS_INTEGER(runStart) || LENGTH(runStart) != 1 ||
		INTEGER(runStart)[0] == NA_INTEGER || INTEGER(runStart)[0] < 1)
		error("invalid 'runStart' argument");

	if (!IS_INTEGER(runEnd) || LENGTH(runEnd) != 1 ||
		INTEGER(runEnd)[0] == NA_INTEGER ||
		INTEGER(runEnd)[0] < INTEGER(runStart)[0] ||
		INTEGER(runEnd)[0] > LENGTH(values))
		error("invalid 'runWidth' argument");

	PROTECT(runWidth = NEW_INTEGER(1));
	INTEGER(runWidth)[0] = INTEGER(runEnd)[0] - INTEGER(runStart)[0] + 1;

	PROTECT(ans_values = vector_subseq(values, runStart, runWidth));
    PROTECT(ans_lengths = vector_subseq(lengths, runStart, runWidth));

    INTEGER(ans_lengths)[0] -= INTEGER(offsetStart)[0];
	INTEGER(ans_lengths)[INTEGER(runWidth)[0] - 1] -= INTEGER(offsetEnd)[0];

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, mkChar("values"), ans_values);
	SET_SLOT(ans, mkChar("lengths"), ans_lengths);
    UNPROTECT(4);

	return ans;
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP Rle_subseq(SEXP x, SEXP start, SEXP width)
{
	int i, x_length, cumlen, more;
	int seq_start, seq_width, seq_end;
	int *lengths_elt;
	SEXP values, lengths, ans, run_start, run_end, offset_start, offset_end;

	if (!IS_INTEGER(start) || LENGTH(start) != 1 ||
		INTEGER(start)[0] == NA_INTEGER || INTEGER(start)[0] < 1)
		error("'start' must be a positive integer");

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER || INTEGER(width)[0] < 0)
		error("'width' must be a non-negative integer");

	seq_start = INTEGER(start)[0];
	seq_width = INTEGER(width)[0];
	seq_end = seq_start + seq_width - 1;

	values = GET_SLOT(x, install("values"));
	lengths = GET_SLOT(x, install("lengths"));

	x_length = 0;
	for (i = 0, lengths_elt = INTEGER(lengths); i < LENGTH(lengths);
	     i++, lengths_elt++) {
		x_length += *lengths_elt;
	}

	if (x_length < seq_end)
		error("subseq exceeds bounds of 'x'");

	PROTECT(run_start = NEW_INTEGER(1));
	PROTECT(run_end = NEW_INTEGER(1));
	PROTECT(offset_start = NEW_INTEGER(1));
	PROTECT(offset_end = NEW_INTEGER(1));

	i = 1;
	more = 1;
	cumlen = 0;
	lengths_elt = INTEGER(lengths);
	while (more) {
		cumlen += *lengths_elt;
		if (seq_start <= cumlen) {
			INTEGER(run_start)[0] = i;
			cumlen -= *lengths_elt;
			INTEGER(offset_start)[0] = seq_start - cumlen - 1;
			more = 0;
		} else {
			i++;
			lengths_elt++;
		}
	}
	more = 1;
	while (more) {
		cumlen += *lengths_elt;
		if (seq_end <= cumlen) {
			INTEGER(run_end)[0] = i;
			INTEGER(offset_end)[0] = cumlen - seq_end;
			more = 0;
		} else {
			i++;
			lengths_elt++;
		}
	}

	PROTECT(ans = Rle_run_subseq(x, run_start, run_end, offset_start, offset_end));
    UNPROTECT(5);

	return ans;
}
