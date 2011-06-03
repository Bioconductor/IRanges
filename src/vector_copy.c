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



/****************************************************************************
 * B. THE DO-IT-ALL _vector_Ocopy() FUNCTION
 * =========================================
 *
 * 'Omode' controls on which side recycling must happen:
 *   - Omode =  0: straight copying (no recycling);
 *   - Omode =  1: cyclic writing to 'out';
 *   - Omode = -1: cyclic reading from 'in' (doesn't support reverse mode).
 */

void _vector_Ocopy(SEXP out, int out_offset, SEXP in, int in_offset,
		int nelt, SEXP lkup, int reverse, int Omode)
{
	void (*Ocopy_bytes)(int, int,
		char *, int, const char *, int, const int *, int);
	void (*Ocopy_byteblocks)(int, int,
		char *, size_t, const char *, size_t, size_t);
	int dest_nelt, src_nelt, i1, i2;
	char *dest = NULL, *src = NULL; /* gcc -Wall */
	size_t blocksize = 0; /* gcc -Wall */

	if (Omode >= 0) {
		if (out_offset < 0)
			error("subscripts out of bounds");
		if (Omode == 0) {
			if (out_offset + nelt > LENGTH(out))
				error("subscripts out of bounds");
			dest_nelt = nelt;
		} else {
			dest_nelt = LENGTH(out) - out_offset;
		}
		if (reverse) {
			Ocopy_bytes = _Orevcopy_bytes_from_i1i2_with_lkup;
			Ocopy_byteblocks = _Orevcopy_byteblocks_from_i1i2;
		} else {
			Ocopy_bytes = _Ocopy_bytes_from_i1i2_with_lkup;
			Ocopy_byteblocks = _Ocopy_byteblocks_from_i1i2;
		}
		i1 = in_offset;
		in_offset = 0;
		src_nelt = LENGTH(in);
	} else {
		if (in_offset < 0)
			error("subscripts out of bounds");
		src_nelt = LENGTH(in) - in_offset;
		if (reverse)
			error("IRanges internal error in _vector_Ocopy(): "
			      "reverse mode not supported when Omode=-1");
		Ocopy_bytes = _Ocopy_bytes_to_i1i2_with_lkup;
		Ocopy_byteblocks = _Ocopy_byteblocks_to_i1i2;
		i1 = out_offset;
		out_offset = 0;
		dest_nelt = LENGTH(out);
	}
	i2 = i1 + nelt - 1;
	switch (TYPEOF(out)) {
	    case RAWSXP:
		dest = (char *) (RAW(out) + out_offset);
		src = (char *) (RAW(in) + in_offset);
		if (lkup != R_NilValue) {
			Ocopy_bytes(i1, i2,
				dest, dest_nelt, src, src_nelt,
				INTEGER(lkup), LENGTH(lkup));
			return;
		}
		blocksize = sizeof(Rbyte);
		break;
	    case LGLSXP:
		dest = (char *) (LOGICAL(out) + out_offset);
		src = (char *) (LOGICAL(in) + in_offset);
		blocksize = sizeof(int);
		break;
	    case INTSXP:
		dest = (char *) (INTEGER(out) + out_offset);
		src = (char *) (INTEGER(in) + in_offset);
		blocksize = sizeof(int);
		break;
	    case REALSXP:
		dest = (char *) (REAL(out) + out_offset);
		src = (char *) (REAL(in) + in_offset);
		blocksize = sizeof(double);
		break;
	    case CPLXSXP:
		dest = (char *) (COMPLEX(out) + out_offset);
		src = (char *) (COMPLEX(in) + in_offset);
		blocksize = sizeof(Rcomplex);
		break;
	    default:
		error("IRanges internal error in _vector_Ocopy(): "
		      "%s type not supported", CHAR(type2str(TYPEOF(out))));
		break; // gcc -Wall
	}
	Ocopy_byteblocks(i1, i2, dest, dest_nelt, src, src_nelt, blocksize);
	return;
}



/****************************************************************************
 * C. CYCLIC COPYING
 * =================
 *
 * The functions in this section implement cyclic copying. The user can
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
 * INTERFACE: "offset/nelt".
 * RECYCLING: Cyclic writing to 'out'.
 * In addition, "raw" vectors support fast on-the-fly translation via the
 * 'lkup' table.
 * Reverts the order of the copied elements if 'reverse' is != 0.
 */
void _vector_Ocopy_from_offset(SEXP out, SEXP in, int in_offset, int nelt,
		SEXP lkup, int reverse)
{
	_vector_Ocopy(out, 0, in, in_offset, nelt, lkup, reverse, 1);
	return;
}

/*
 * INTERFACE: "offset/nelt".
 * RECYCLING: Cyclic reading from 'in'.
 * In addition, "raw" vectors support fast on-the-fly translation via the
 * 'lkup' table.
 */
void _vector_Ocopy_to_offset(SEXP out, SEXP in, int out_offset, int nelt,
		SEXP lkup)
{
	_vector_Ocopy(out, out_offset, in, 0, nelt, lkup, 0, -1);
	return;
}

/*
 * INTERFACE: "subscript".
 * RECYCLING: Cyclic writing to 'out'.
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
		      "%s type not supported", CHAR(type2str(TYPEOF(out))));
	}
	return;
}

/*
 * INTERFACE: "subscript".
 * RECYCLING: Cyclic reading from 'in'.
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
		      "%s type not supported", CHAR(type2str(TYPEOF(out))));
	}
	return;
}



/****************************************************************************
 * D. COPYING MULTIPLE RANGES (NO RECYCLING)
 * =========================================
 *
 * _vector_mcopy() supports:
 *   - fast on-the-fly translation via the 'lkup' table (only on "raw"
 *     vectors);
 *   - reverse copy if 'reverse' is != 0.
 */

void _vector_mcopy(SEXP out, int out_offset,
		SEXP in, SEXP in_start, SEXP in_width,
		SEXP lkup, int reverse)
{
	int i1, i2, j, in_offset, nelt;

	for (i1 = 0, i2 = LENGTH(in_start) - 1;
	     i1 < LENGTH(in_start);
	     i1++, i2--)
	{
		j = reverse ? i2 : i1;
		in_offset = INTEGER(in_start)[j] - 1;
		nelt = INTEGER(in_width)[j];
		if (nelt < 0)
			error("negative widths are not allowed");
		_vector_Ocopy(out, out_offset, in, in_offset, nelt,
			lkup, reverse, 0);
		out_offset += nelt;
	}
	return;
}

