/****************************************************************************
 *            Low-level manipulation of Vector and List objects             *
 *          Authors: Patrick Aboyoun, Michael Lawrence, Herve Pages         *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"


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
 * memcmp()-based comparison of 2 vectors of the same type.
 * NOTE: Doesn't support STRSXP and VECSXP.
 */
int _vector_memcmp(SEXP x1, int x1_offset, SEXP x2, int x2_offset, int nelt)
{
	const void *s1 = NULL, *s2 = NULL; /* gcc -Wall */
	size_t eltsize = 0; /* gcc -Wall */

	if (x1_offset < 0 || x1_offset + nelt > LENGTH(x1)
	 || x2_offset < 0 || x2_offset + nelt > LENGTH(x2))
		error("IRanges internal error in _vector_memcmp(): "
		      "elements to compare are out of vector bounds");
	switch (TYPEOF(x1)) {
	case RAWSXP:
		s1 = (const void *) (RAW(x1) + x1_offset);
		s2 = (const void *) (RAW(x2) + x2_offset);
		eltsize = sizeof(Rbyte);
		break;
	case LGLSXP:
	case INTSXP:
		s1 = (const void *) (INTEGER(x1) + x1_offset);
		s2 = (const void *) (INTEGER(x2) + x2_offset);
		eltsize = sizeof(int);
		break;
	case REALSXP:
		s1 = (const void *) (REAL(x1) + x1_offset);
		s2 = (const void *) (REAL(x2) + x2_offset);
		eltsize = sizeof(double);
		break;
	case CPLXSXP:
		s1 = (const void *) (COMPLEX(x1) + x1_offset);
		s2 = (const void *) (COMPLEX(x2) + x2_offset);
		eltsize = sizeof(Rcomplex);
		break;
	default:
		error("IRanges internal error in _vector_memcmp(): "
		      "%s type not supported", CHAR(type2str(TYPEOF(x1))));
	}
	return s1 == s2 ? 0 : memcmp(s1, s2, nelt * eltsize);
}

/*
 * memcpy()-based copy of data from a vector to a vector of the same type.
 */
void _vector_memcpy(SEXP out, int out_offset, SEXP in, int in_offset, int nelt)
{
	void *dest;
	const void *src;
	size_t eltsize;
	int i;
	SEXP in_elt; // out_elt;

	if (out_offset < 0 || out_offset + nelt > LENGTH(out)
	 || in_offset < 0 || in_offset + nelt > LENGTH(in))
		error("subscripts out of bounds");
	switch (TYPEOF(out)) {
	    case RAWSXP:
		dest = (void *) (RAW(out) + out_offset);
		src = (const void *) (RAW(in) + in_offset);
		eltsize = sizeof(Rbyte);
		break;
	    case LGLSXP:
		dest = (void *) (LOGICAL(out) + out_offset);
		src = (const void *) (LOGICAL(in) + in_offset);
		eltsize = sizeof(int);
		break;
	    case INTSXP:
		dest = (void *) (INTEGER(out) + out_offset);
		src = (const void *) (INTEGER(in) + in_offset);
		eltsize = sizeof(int);
		break;
	    case REALSXP:
		dest = (void *) (REAL(out) + out_offset);
		src = (const void *) (REAL(in) + in_offset);
		eltsize = sizeof(double);
		break;
	    case CPLXSXP:
		dest = (void *) (COMPLEX(out) + out_offset);
		src = (const void *) (COMPLEX(in) + in_offset);
		eltsize = sizeof(Rcomplex);
		break;
	    case STRSXP:
		for (i = 0; i < nelt; i++) {
			in_elt = STRING_ELT(in, in_offset + i);
			SET_STRING_ELT(out, out_offset + i, in_elt);
			//PROTECT(out_elt = duplicate(in_elt));
			//SET_STRING_ELT(out, out_offset + i, out_elt);
			//UNPROTECT(1);
		}
		return;
	    case VECSXP:
		for (i = 0; i < nelt; i++) {
			in_elt = VECTOR_ELT(in, in_offset + i);
			SET_VECTOR_ELT(out, out_offset + i, in_elt);
			//PROTECT(out_elt = duplicate(in_elt));
			//SET_VECTOR_ELT(out, out_offset + i, out_elt);
			//UNPROTECT(1);
		}
		return;
	    default:
		error("IRanges internal error in _vector_memcpy(): "
		      "%s type not supported", CHAR(type2str(TYPEOF(out))));
		return; // gcc -Wall
	}
	memcpy(dest, src, nelt * eltsize);
	return;
}

static void vector_copy_ranges(SEXP out, SEXP in,
		const int *start, const int *width, int nranges)
{
	int i, out_offset, in_offset, nelt;

	out_offset = 0;
	for (i = 0; i < nranges; i++) {
		in_offset = start[i] - 1;
		nelt = width[i];
		if (nelt < 0)
			error("negative widths are not allowed");
		_vector_memcpy(out, out_offset, in, in_offset, nelt);
		out_offset += nelt;
	}
	return;
}

/* --- .Call ENTRY POINT ---
 * 'start' and 'width': integer vectors of the same length. They are assumed
 * to come from a valid Ranges object i.e. no NAs and values in 'width' must
 * be >= 0.
 */
SEXP vector_subsetByRanges(SEXP x, SEXP start, SEXP width)
{
	int x_len, nranges, ans_len, i, offset_i, width_i, end_i;
	const int *start_p, *width_p;
	SEXP ans, x_names, ans_names;

	x_len = LENGTH(x);
	nranges = check_integer_pairs(start, width,
				      &start_p, &width_p,
				      "start", "width");
	for (i = ans_len = 0; i < nranges; i++) {
		width_i = width_p[i];
		if (width_i == NA_INTEGER || width_i < 0)
			error("'width' cannot contain NAs or negative values");
		offset_i = start_p[i] - 1;
		end_i = offset_i + width_i;
		if (offset_i < 0 || end_i > x_len)
			error("some ranges are out of bounds");
		ans_len += width_i;
	}
	PROTECT(ans = allocVector(TYPEOF(x), ans_len));
	vector_copy_ranges(ans, x, start_p, width_p, nranges);
	x_names = GET_NAMES(x);
	if (x_names != R_NilValue) {
		PROTECT(ans_names = NEW_CHARACTER(ans_len));
		vector_copy_ranges(ans_names, x_names,
				   start_p, width_p, nranges);
		SET_NAMES(ans, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT ---
 * TODO: Remove this at some point (use vector_subsetByRanges instead).
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
			error("IRanges internal error in vector_seqselect(): "
			      "%s type not supported",
			      CHAR(type2str(TYPEOF(x))));
		}
	}
	ans_names = GET_NAMES(x);
	if (ans_names != R_NilValue)
		SET_NAMES(ans, vector_seqselect(ans_names, start, width));
	UNPROTECT(1);
	return ans;
}

