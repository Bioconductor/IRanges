/****************************************************************************
 *            Low-level manipulation of Vector and List objects             *
 *          Authors: Patrick Aboyoun, Michael Lawrence, Herve Pages         *
 ****************************************************************************/
#include "IRanges.h"


/****************************************************************************
 * C-level slot getters.
 */

static SEXP elementType_symbol = NULL;

const char *_get_List_elementType(SEXP x)
{
	INIT_STATIC_SYMBOL(elementType)
	return CHAR(STRING_ELT(GET_SLOT(x, elementType_symbol), 0));
}


/****************************************************************************
 * C-level slot setters.
 */

void _set_List_elementType(SEXP x, const char *type)
{
	SEXP value;

	INIT_STATIC_SYMBOL(elementType)
	PROTECT(value = mkString(type));
	SET_SLOT(x, elementType_symbol, value);
	UNPROTECT(1);
	return;
}


/****************************************************************************
 * Other stuff.
 */

/*
 * --- .Call ENTRY POINT ---
 */
SEXP vector_seqselect(SEXP x, SEXP start, SEXP width)
{
	int ans_offset, i, j, s, w;
	SEXP ans, ans_names;

	if (!IS_INTEGER(start))
		error("'start' must be an integer vector");
	if (!IS_INTEGER(width))
		error("'width' must be an integer vector");
	if (LENGTH(start) != LENGTH(width))
		error("length of 'start' must equal length of 'width'");

	for (i = ans_offset = 0; i < LENGTH(start); i++, ans_offset += w) {
		s = INTEGER(start)[i];
		w = INTEGER(width)[i];
		if (s == NA_INTEGER || s < 1)
			error("each element in 'start' must be a positive integer");
		if (w == NA_INTEGER || w < 0)
			error("each element in 'width' must be a non-negative integer");
		if (LENGTH(x) < s + w - 1)
			error("some ranges are out of bounds");
	}

	PROTECT(ans = allocVector(TYPEOF(x), ans_offset));

	for (i = ans_offset = 0; i < LENGTH(start); i++, ans_offset += w) {
		s = INTEGER(start)[i] - 1;
		w = INTEGER(width)[i];
		switch (TYPEOF(x)) {
		    case LGLSXP:
		    case INTSXP:
			memcpy(INTEGER(ans) + ans_offset, INTEGER(x) + s, w * sizeof(int));
			break;
    		    case REALSXP:
			memcpy(REAL(ans) + ans_offset, REAL(x) + s, w * sizeof(double));
			break;
		    case CPLXSXP:
			memcpy(COMPLEX(ans) + ans_offset, COMPLEX(x) + s, w * sizeof(Rcomplex));
			break;
		    case STRSXP:
			for (j = 0; j < w; j++)
				SET_STRING_ELT(ans, ans_offset + j, STRING_ELT(x, s + j));
			break;
		    case VECSXP:
			for (j = 0; j < w; j++)
				SET_VECTOR_ELT(ans, ans_offset + j, VECTOR_ELT(x, s + j));
			break;
		    case RAWSXP:
			memcpy(RAW(ans) + ans_offset, RAW(x) + s, w * sizeof(char));
			break;
		    default:
			UNIMPLEMENTED_TYPE("vector_seqselect", x);
		}
	}
	ans_names = GET_NAMES(x);
	if (ans_names != R_NilValue)
		SET_NAMES(ans, vector_seqselect(ans_names, start, width));
	UNPROTECT(1);
	return ans;
}

