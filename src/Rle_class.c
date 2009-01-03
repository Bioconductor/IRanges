#include "IRanges.h"

SEXP Rle_subseq(SEXP x, SEXP start, SEXP width)
{
	int i, x_length, more;
	int cumlen, diff_start, diff_end;
	int seq_start, seq_width, seq_end;
	int *lengths_elt;
	SEXP values, lengths, ans, ans_start, ans_width, ans_values, ans_lengths;

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

	PROTECT(ans_start = NEW_INTEGER(1));
	PROTECT(ans_width = NEW_INTEGER(1));

	i = 1;
	more = 1;
	cumlen = 0;
	lengths_elt = INTEGER(lengths);
	while (more) {
		cumlen += *lengths_elt;
		if (seq_start <= cumlen) {
			INTEGER(ans_start)[0] = i;
			cumlen -= *lengths_elt;
			diff_start = seq_start - cumlen - 1;
			more = 0;
		} else {
			i++;
			lengths_elt++;
		}
	}
	i = 1;
	more = 1;
	while (more) {
		cumlen += *lengths_elt;
		if (seq_end <= cumlen) {
			INTEGER(ans_width)[0] = i;
			diff_end = cumlen - seq_end;
			more = 0;
		} else {
			i++;
			lengths_elt++;
		}
	}

	PROTECT(ans_values = vector_subseq(values, ans_start, ans_width));
    PROTECT(ans_lengths = vector_subseq(lengths, ans_start, ans_width));
	if (INTEGER(ans_width)[0] == 1) {
		INTEGER(ans_lengths)[0] = seq_width;
	} else {
		INTEGER(ans_lengths)[0] -= diff_start;
		INTEGER(ans_lengths)[INTEGER(ans_width)[0] - 1] -= diff_end;
	}

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Rle")));
	SET_SLOT(ans, mkChar("values"), ans_values);
	SET_SLOT(ans, mkChar("lengths"), ans_lengths);
    UNPROTECT(5);

	return ans;
}
