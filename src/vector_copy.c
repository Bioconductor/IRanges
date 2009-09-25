/****************************************************************************
 *                   Low-level utilities for copying data                   *
 *                from a vector to a vector of the same type                *
 *                ------------------------------------------                *
 *                                                                          *
 * NOTE: Only "raw" (RAWSXP), "logical" (LGLSXP), "integer" (INTSXP),       *
 *       "double" (REALSXP) and "complex" (CPLXSXP) vectors are supported   *
 *       at the moment.                                                     *
 ****************************************************************************/
#include "IRanges.h"


/****************************************************************************
 * A. memcmp()-BASED COMPARISON
 * ============================
 */

int _vector_memcmp(SEXP x1, int x1_offset, SEXP x2, int x2_offset, int nelt)
{
	const void *s1 = NULL, *s2 = NULL; /* gcc -Wall */
	size_t eltsize = 0; /* gcc -Wall */

	if (x1_offset < 0 || x1_offset + nelt > LENGTH(x1)
	 || x2_offset < 0 || x2_offset + nelt > LENGTH(x2))
		error("IRanges internal error in _vector_memcmp(): "
		      "trying to compare vector elements that are out of bounds");
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
		      "%s type not supported", type2str(TYPEOF(x1)));
	}
	return s1 == s2 ? 0 : memcmp(s1, s2, nelt * eltsize);
}


/****************************************************************************
 * B. memcpy()-BASED COPY (NO RECYCLING)
 * =====================================
 */

void _vector_memcpy(SEXP out, int out_offset, SEXP in, int in_offset, int nelt)
{
	if (out_offset < 0 || out_offset + nelt > LENGTH(out)
	 || in_offset < 0 || in_offset + nelt > LENGTH(in))
		error("subscripts out of bounds");
	switch (TYPEOF(out)) {
	case RAWSXP:
		memcpy(RAW(out) + out_offset, RAW(in) + in_offset,
			nelt * sizeof(Rbyte));
		break;
	case LGLSXP:
	case INTSXP:
		memcpy(INTEGER(out) + out_offset, INTEGER(in) + in_offset,
			nelt * sizeof(int));
		break;
	case REALSXP:
		memcpy(REAL(out) + out_offset, REAL(in) + in_offset,
			nelt * sizeof(double));
		break;
	case CPLXSXP:
		memcpy(COMPLEX(out) + out_offset, COMPLEX(in) + in_offset,
			nelt * sizeof(Rcomplex));
		break;
	default:
		error("IRanges internal error in _vector_memcpy(): "
		      "%s type not supported", type2str(TYPEOF(out)));
	}
	return;
}


/****************************************************************************
 * C. COPY WITH RECYCLING
 * ======================
 *
 * The functions in section B. implement copy with recycling. The user can
 * choose between 2 interfaces for specifying elements in the 'in' or 'out'
 * vectors:
 *
 *   1. The "offset/nelt" interface: the elements to access are specified via
 * 2 integers: 'offset' (the 0-based position of the first element to access)
 * and 'nelt' (the number of elements to access, all immediately following
 * the first element to access).
 *
 *   2. The "subscript" interface: the elements to access are specified by an
 * integer vector containing their 1-based positions in the 'in' or 'out'
 * vectors.
 *
 * The "subscript" interface is intended to be used by the subsetting
 * operator [ defined at the R level for SharedVector objects.
 * Implementing this interface requires to pay some special attention to
 * the following important properties of the subsetting operator [ in R.
 * If x is a vector and i an integer vector of length n with the following
 * properties:
 *   a) i contains no NA values,
 *   b) i can be used to subset x without being "out of bounds" (i.e all
 *      values in i are >= 1 and <= length(x)),
 * then we have the following properties:
 *   1) READING from x: y <- x[i] produces a vector, of the same type than x,
 *      but of the same length than i (length(y) == n).
 *   2) READING from then WRITING to x: x[i] <- x[i] (short for y <- x[i];
 *      x[i] <- y) doesn't modify the values in x.
 *   3) WRITING to then READING from x: if z is a vector of length n and of
 *      the same type than x, then doing x[i] <- z; y <- x[i] guarantees that
 *      y is identical to z only when i contains no repeated value!
 *
 * Functions in this file that implement the "subscript" interface adhere to
 * the above properties.
 */


/*
 * RECYCLING: Cyclic writing to 'out'.
 * INTERFACE: "offset/nelt".
 * In addition, "raw" vectors support fast on-the-fly translation via the
 * 'lkup' table.
 * Reverts the order of the copied elements if 'reverse' is != 0.
 */
void _vector_Ocopy_from_offset(SEXP out, SEXP in, int in_offset, int nelt,
		SEXP lkup, int reverse)
{
	int i1, i2;
	void (*fun)(int, int, char *, size_t, const char *, size_t, size_t);

	i1 = in_offset;
	i2 = in_offset + nelt - 1;
	fun = reverse ? _Orevcopy_byteblocks_from_i1i2
		      : _Ocopy_byteblocks_from_i1i2;
	switch (TYPEOF(out)) {
	case RAWSXP:
		if (lkup == R_NilValue) {
			fun(i1, i2,
				(char *) RAW(out), LENGTH(out),
				(char *) RAW(in), LENGTH(in), sizeof(Rbyte));
		} else {
			if (reverse)
				_Orevcopy_bytes_from_i1i2_with_lkup(
					i1, i2,
					(char *) RAW(out), LENGTH(out),
					(char *) RAW(in), LENGTH(in),
					INTEGER(lkup), LENGTH(lkup));
			else
				_Ocopy_bytes_from_i1i2_with_lkup(i1, i2,
					(char *) RAW(out), LENGTH(out),
					(char *) RAW(in), LENGTH(in),
					INTEGER(lkup), LENGTH(lkup));
		}
		break;
	case LGLSXP:
	case INTSXP:
		fun(i1, i2,
			(char *) INTEGER(out), LENGTH(out),
			(char *) INTEGER(in), LENGTH(in), sizeof(int));
		break;
	case REALSXP:
		fun(i1, i2,
			(char *) REAL(out), LENGTH(out),
			(char *) REAL(in), LENGTH(in), sizeof(double));
		break;
	case CPLXSXP:
		fun(i1, i2,
			(char *) COMPLEX(out), LENGTH(out),
			(char *) COMPLEX(in), LENGTH(in), sizeof(Rcomplex));
		break;
	default:
		error("IRanges internal error in _vector_Ocopy_from_offset(): "
		      "%s type not supported", type2str(TYPEOF(out)));
	}
	return;
}

/*
 * RECYCLING: Cyclic writing to 'out'.
 * INTERFACE: "subscript".
 * In addition, "raw" vectors support fast on-the-fly translation via the
 * 'lkup' table.
 */
void _vector_Ocopy_from_subscript(SEXP out, SEXP in, SEXP subscript, SEXP lkup)
{
	switch (TYPEOF(out)) {
	case RAWSXP:
		if (lkup == R_NilValue)
			_Ocopy_byteblocks_from_subscript(
				INTEGER(subscript), LENGTH(subscript),
				(char *) RAW(out), LENGTH(out),
				(char *) RAW(in), LENGTH(in), sizeof(Rbyte));
		else
			_Ocopy_bytes_from_subscript_with_lkup(
				INTEGER(subscript), LENGTH(subscript),
				(char *) RAW(out), LENGTH(out),
				(char *) RAW(in), LENGTH(in), 
				INTEGER(lkup), LENGTH(lkup));
		break;
	case LGLSXP:
	case INTSXP:
		_Ocopy_byteblocks_from_subscript(
			INTEGER(subscript), LENGTH(subscript),
			(char *) INTEGER(out), LENGTH(out),
			(char *) INTEGER(in), LENGTH(in), sizeof(int));
		break;
	case REALSXP:
		_Ocopy_byteblocks_from_subscript(
			INTEGER(subscript), LENGTH(subscript),
			(char *) REAL(out), LENGTH(out),
			(char *) REAL(in), LENGTH(in), sizeof(double));
		break;
	case CPLXSXP:
		_Ocopy_byteblocks_from_subscript(
			INTEGER(subscript), LENGTH(subscript),
			(char *) COMPLEX(out), LENGTH(out),
			(char *) COMPLEX(in), LENGTH(in), sizeof(Rcomplex));
		break;
	default:
		error("IRanges internal error in _vector_Ocopy_from_subscript(): "
		      "%s type not supported", type2str(TYPEOF(out)));
	}
	return;
}

/*
 * RECYCLING: Cyclic reading from 'in'.
 * INTERFACE: "offset/nelt".
 * In addition, "raw" vectors support fast on-the-fly translation via the
 * 'lkup' table.
 */
void _vector_Ocopy_to_offset(SEXP out, SEXP in, int out_offset, int nelt,
		SEXP lkup)
{
	int i1, i2;

	i1 = out_offset;
	i2 = out_offset + nelt - 1;
	switch (TYPEOF(out)) {
	case RAWSXP:
		if (lkup == R_NilValue) {
			_Ocopy_byteblocks_to_i1i2(i1, i2,
				(char *) RAW(out), LENGTH(out),
				(char *) RAW(in), LENGTH(in), sizeof(Rbyte));
		} else {
			_Ocopy_bytes_to_i1i2_with_lkup(i1, i2,
				(char *) RAW(out), LENGTH(out),
				(char *) RAW(in), LENGTH(in),
				INTEGER(lkup), LENGTH(lkup));
		}
		break;
	case LGLSXP:
	case INTSXP:
		_Ocopy_byteblocks_to_i1i2(i1, i2,
			(char *) INTEGER(out), LENGTH(out),
			(char *) INTEGER(in), LENGTH(in), sizeof(int));
		break;
	case REALSXP:
		_Ocopy_byteblocks_to_i1i2(i1, i2,
			(char *) REAL(out), LENGTH(out),
			(char *) REAL(in), LENGTH(in), sizeof(double));
		break;
	case CPLXSXP:
		_Ocopy_byteblocks_to_i1i2(i1, i2,
			(char *) COMPLEX(out), LENGTH(out),
			(char *) COMPLEX(in), LENGTH(in), sizeof(Rcomplex));
		break;
	default:
		error("IRanges internal error in _vector_Ocopy_to_offset(): "
		      "%s type not supported", type2str(TYPEOF(out)));
	}
	return;
}

/*
 * RECYCLING: Cyclic reading from 'in'.
 * INTERFACE: "subscript".
 * In addition, "raw" vectors support fast on-the-fly translation via the
 * 'lkup' table.
 */
void _vector_Ocopy_to_subscript(SEXP out, SEXP in, SEXP subscript, SEXP lkup)
{
	switch (TYPEOF(out)) {
	case RAWSXP:
		if (lkup == R_NilValue)
			_Ocopy_byteblocks_to_subscript(
				INTEGER(subscript), LENGTH(subscript),
				(char *) RAW(out), LENGTH(out),
				(char *) RAW(in), LENGTH(in), sizeof(Rbyte));
		else
			_Ocopy_bytes_to_subscript_with_lkup(
				INTEGER(subscript), LENGTH(subscript),
				(char *) RAW(out), LENGTH(out),
				(char *) RAW(in), LENGTH(in), 
				INTEGER(lkup), LENGTH(lkup));
		break;
	case LGLSXP:
	case INTSXP:
		_Ocopy_byteblocks_to_subscript(
			INTEGER(subscript), LENGTH(subscript),
			(char *) INTEGER(out), LENGTH(out),
			(char *) INTEGER(in), LENGTH(in), sizeof(int));
		break;
	case REALSXP:
		_Ocopy_byteblocks_to_subscript(
			INTEGER(subscript), LENGTH(subscript),
			(char *) REAL(out), LENGTH(out),
			(char *) REAL(in), LENGTH(in), sizeof(double));
		break;
	case CPLXSXP:
		_Ocopy_byteblocks_to_subscript(
			INTEGER(subscript), LENGTH(subscript),
			(char *) COMPLEX(out), LENGTH(out),
			(char *) COMPLEX(in), LENGTH(in), sizeof(Rcomplex));
		break;
	default:
		error("IRanges internal error in _vector_Ocopy_to_subscript(): "
		      "%s type not supported", type2str(TYPEOF(out)));
	}
	return;
}

