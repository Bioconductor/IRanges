#include "IRanges.h"

SEXP vector_subseq(SEXP x, SEXP start, SEXP width)
{
	int i, j, ans_start, ans_width;
	SEXP ans, names;

	if (!IS_INTEGER(start) || LENGTH(start) != 1 ||
		INTEGER(start)[0] == NA_INTEGER || INTEGER(start)[0] < 0)
		error("'start' must be a non-negative integer");

	if (!IS_INTEGER(width) || LENGTH(width) != 1 ||
		INTEGER(width)[0] == NA_INTEGER || INTEGER(width)[0] < 0)
		error("'width' must be a non-negative integer");

	ans_start = INTEGER(start)[0] - 1;
	ans_width = INTEGER(width)[0];

	if (LENGTH(x) < ans_start + ans_width)
		error("subseq exceeds bounds of 'x'");

    PROTECT(ans = allocVector(TYPEOF(x), ans_width));
	switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    	memcpy(INTEGER(ans), INTEGER(x) + ans_start, ans_width * sizeof(int));
        break;
    case REALSXP:
    	memcpy(REAL(ans), REAL(x) + ans_start, ans_width * sizeof(double));
        break;
    case CPLXSXP:
    	for (i = 0, j = ans_start; i < ans_width; i++, j++) {
            COMPLEX(ans)[i].r = COMPLEX(x)[j].r;
            COMPLEX(ans)[i].i = COMPLEX(x)[j].i;
    	}
        break;
    case STRSXP:
    	for (i = 0, j = ans_start; i < ans_width; i++, j++)
    		SET_STRING_ELT(ans, i, STRING_ELT(x, j));
        break;
    case VECSXP:
    	for (i = 0, j = ans_start; i < ans_width; i++, j++)
            SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, j));
        break;
    case RAWSXP:
    	memcpy(RAW(ans), RAW(x) + ans_start, ans_width * sizeof(char));
        break;
    default:
        error("unrecognized vector type");
    }

    names = getAttrib(x, R_NamesSymbol);
    if (names != R_NilValue) {
    	setAttrib(ans, R_NamesSymbol, vector_subseq(names, start, width));
    }

    UNPROTECT(1);

	return ans;
}
