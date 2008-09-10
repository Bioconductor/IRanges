/****************************************************************************
 *                          Fast RawPtr utilities                           *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_RawPtr_utils()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in file %s\n",
		debug ? "on" : "off", __FILE__);
#else
	Rprintf("Debug mode not available in file %s\n", __FILE__);
#endif
	return R_NilValue;
}


/*
 * Memory allocation for an RawPtr object.
 * The data of an RawPtr object are stored in an "external" raw vector
 * (RAWSXP vector).
 * The allocated memory is NOT initialized!
 */
SEXP RawPtr_alloc(SEXP rawptr_xp, SEXP length)
{
	SEXP tag;
	int tag_length;

	tag_length = INTEGER(length)[0];
	PROTECT(tag = NEW_RAW(tag_length));
	R_SetExternalPtrTag(rawptr_xp, tag);
	UNPROTECT(1);
	return rawptr_xp;
}

/*
 * Return the single string printed by the show method for RawPtr objects.
 * 'rawptr_xp' must be the 'xp' slot of an RawPtr object.
 * From R:
 *   xr <- RawPtr(30)
 *   .Call("RawPtr_get_show_string", xr@xp, PACKAGE="IRanges")
 */
SEXP RawPtr_get_show_string(SEXP rawptr_xp)
{
	SEXP tag, ans;
	int tag_length;
	char buf[100]; /* should be enough... */

	tag = R_ExternalPtrTag(rawptr_xp);
	tag_length = LENGTH(tag);
	snprintf(buf, sizeof(buf), "%d-byte RawPtr object (data starting at memory address %p)",
		tag_length, RAW(tag));
	PROTECT(ans = NEW_CHARACTER(1));
	SET_STRING_ELT(ans, 0, mkChar(buf));
	UNPROTECT(1);
	return ans;
}

/*
 * Return length of R string pointed by 'rawptr_xp'.
 * From R:
 *   xr <- RawPtr(30)
 *   .Call("RawPtr_length", xr@xp, PACKAGE="IRanges")
 * Called by method length() for RawPtr objects.
 */
SEXP RawPtr_length(SEXP rawptr_xp)
{
	SEXP tag, ans;
	int tag_length;

	tag = R_ExternalPtrTag(rawptr_xp);
	tag_length = LENGTH(tag);
	PROTECT(ans = NEW_INTEGER(1));
	INTEGER(ans)[0] = tag_length;
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Comparing the data between 2 RawPtr objects.
 *
 * From R:
 *   xr <- RawPtr(30)
 *   .Call("RawPtr_memcmp", xr@xp, 1L, xr@xp, 10L, 21L, PACKAGE="IRanges")
 */

SEXP RawPtr_memcmp(SEXP rawptr1_xp, SEXP start1,
		SEXP rawptr2_xp, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, ans;
	int i1, i2, n;

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] RawPtr_memcmp(): BEGIN\n");
	}
#endif
	tag1 = R_ExternalPtrTag(rawptr1_xp);
	i1 = INTEGER(start1)[0] - 1;
	tag2 = R_ExternalPtrTag(rawptr2_xp);
	i2 = INTEGER(start2)[0] - 1;
	n = INTEGER(width)[0];

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] RawPtr_memcmp(): ");
		Rprintf("RAW(tag1)=%p i1=%d RAW(tag2)=%p i2=%d n=%d\n",
			RAW(tag1), i1, RAW(tag2), i2, n);
	}
#endif
	PROTECT(ans = NEW_INTEGER(1));
	INTEGER(ans)[0] = _IRanges_memcmp((char *) RAW(tag1), i1,
				(char *) RAW(tag2), i2,
				n, sizeof(Rbyte));
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] RawPtr_memcmp(): END\n");
	}
#endif
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * READ/WRITE functions
 * ====================
 *
 * The functions in this section implement the read/write operations to an
 * RawPtr object. The user can choose between 2 interfaces for each
 * read or write operation:
 *
 *   1. The "i1i2" interface: the chars to access are specified by 2
 * integers: 'imin' (the position of the first char to access, the first
 * char in the buffer being at position 1) and 'imax' (the position of the
 * last char to access).
 *
 *   2. The "subset" interface: the chars to access are specified by an
 * integer vector containing their positions in the buffer.
 *
 * The "subset" interface is intended to be used by the subsetting
 * operator [ defined at the R level for RawPtr objects.
 * R subsetting operator [ can be used to read values from, or write values
 * to an object that contains a collection of values, like a character
 * vector, an integer vector or a logical vector.
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
 * Functions in this section that implement the "subset" interface
 * respect the above properties.
 *
 * Here are some arguments to these functions that must always be SEXP of the
 * following types:
 *   src_rawptr_xp, dest_rawptr_xp: externalptr
 *   imin, imax: single integers
 *   subset: integer vector containing the subscripts (with no NAs)
 *   lkup: lookup table for encoding/decoding (integer or complex vector)
 */


/* ==========================================================================
 * Copy bytes from an RawPtr object to another RawPtr object.
 * --------------------------------------------------------------------------
 */

/* Bold version (no recycling) */
SEXP RawPtr_memcpy(SEXP dest_rawptr_xp, SEXP dest_start,
		SEXP src_rawptr_xp, SEXP src_start, SEXP width)
{
	SEXP dest, src;
	int i, j, n;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	i = INTEGER(dest_start)[0] - 1;
	src = R_ExternalPtrTag(src_rawptr_xp);
	j = INTEGER(src_start)[0] - 1;
	n = INTEGER(width)[0];
	if (i < 0 || i + n > LENGTH(dest)
	 || j < 0 || j + n > LENGTH(src))
		error("subscripts out of bounds");
	memcpy(RAW(dest) + i, RAW(src) + j, n * sizeof(Rbyte));
	return dest_rawptr_xp;
}

/* Cyclic writing in 'dest_rawptr_xp' */
SEXP RawPtr_copy_from_i1i2(SEXP dest_rawptr_xp, SEXP src_rawptr_xp, SEXP imin, SEXP imax)
{
	SEXP dest, src;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	src = R_ExternalPtrTag(src_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	_IRanges_memcpy_from_i1i2(i1, i2,
			(char *) RAW(dest), LENGTH(dest),
			(char *) RAW(src), LENGTH(src), sizeof(Rbyte));
	return dest_rawptr_xp;
}

/* Cyclic writing in 'dest_rawptr_xp' */
SEXP RawPtr_copy_from_subset(SEXP dest_rawptr_xp, SEXP src_rawptr_xp, SEXP subset)
{
	SEXP dest, src;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	src = R_ExternalPtrTag(src_rawptr_xp);
	_IRanges_memcpy_from_subset(INTEGER(subset), LENGTH(subset),
			(char *) RAW(dest), LENGTH(dest),
			(char *) RAW(src), LENGTH(src), sizeof(Rbyte));
	return dest_rawptr_xp;
}


/* ==========================================================================
 * Read/write chars from/to an RawPtr object.
 * All the functions in this group assume that sizeof(Rbyte) == sizeof(char).
 * --------------------------------------------------------------------------
 */

/*
 * Return a single string (character vector of length 1).
 * From R:
 *   xr <- RawPtr(15)
 *   xr[] < "Hello"
 *   .Call("RawPtr_read_chars_from_i1i2", xr@xp, 2:2, 4:4, PACKAGE="IRanges")
 */
SEXP RawPtr_read_chars_from_i1i2(SEXP src_rawptr_xp, SEXP imin, SEXP imax)
{
	SEXP src;
	int i1, i2, n;
	CharAE dest;

	src = R_ExternalPtrTag(src_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_from_i1i2(i1, i2,
			dest.elts, n, (char *) RAW(src), LENGTH(src),
			sizeof(char));
	return mkString(dest.elts);
}

SEXP RawPtr_read_chars_from_subset(SEXP src_rawptr_xp, SEXP subset)
{
	SEXP src;
	int n;
	CharAE dest;

	src = R_ExternalPtrTag(src_rawptr_xp);
	n = LENGTH(subset);
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_from_subset(INTEGER(subset), n,
			dest.elts, n, (char *) RAW(src), LENGTH(src),
			sizeof(char));
	return mkString(dest.elts);
}

/*
 * 'string' must be a non-empty single string (character vector of length 1).
 */
SEXP RawPtr_write_chars_to_i1i2(SEXP dest_rawptr_xp, SEXP imin, SEXP imax, SEXP string)
{
	SEXP dest, src;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	src = STRING_ELT(string, 0);
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_to_i1i2(i1, i2,
			(char *) RAW(dest), LENGTH(dest),
			CHAR(src), LENGTH(src), sizeof(char));
	return dest_rawptr_xp;
}

SEXP RawPtr_write_chars_to_subset(SEXP dest_rawptr_xp, SEXP subset, SEXP string)
{
	SEXP dest, src;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	src = STRING_ELT(string, 0);
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_to_subset(INTEGER(subset), LENGTH(subset),
			(char *) RAW(dest), LENGTH(dest),
			CHAR(src), LENGTH(src), sizeof(char));
	return dest_rawptr_xp;
}


/* ==========================================================================
 * Read/write integers from/to an RawPtr object
 * --------------------------------------------------------------------------
 */

/*
 * Return an integer vector of length 'imax' - 'imin' + 1.
 * From R:
 *   xr <- RawPtr(30)
 *   .Call("RawPtr_read_ints_from_i1i2", xr@xp, 20:20, 25:25, PACKAGE="IRanges")
 */
SEXP RawPtr_read_ints_from_i1i2(SEXP src_rawptr_xp, SEXP imin, SEXP imax)
{
	SEXP src, ans;
	int i1, i2, n, j;

	src = R_ExternalPtrTag(src_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	if (i1 < 0 || i2 >= LENGTH(src))
		error("subscript out of bounds");
	n = i2 - i1 + 1;

	PROTECT(ans = NEW_INTEGER(n));
	for (j = 0; i1 <= i2; i1++, j++) {
		INTEGER(ans)[j] = (unsigned char) RAW(src)[i1];
	}
	UNPROTECT(1);
	return ans;
}

/*
 * Return an integer vector of same length than 'subset'.
 * From R:
 *   xr <- RawPtr(30)
 *   .Call("RawPtr_read_ints_from_subset", xr, 25:20, PACKAGE="IRanges")
 */
SEXP RawPtr_read_ints_from_subset(SEXP src_rawptr_xp, SEXP subset)
{
	SEXP src, ans;
	int src_length;
	int n, i, j;

	src = R_ExternalPtrTag(src_rawptr_xp);
	src_length = LENGTH(src);
	n = LENGTH(subset);

	PROTECT(ans = NEW_INTEGER(n));
	for (j = 0; j < n; j++) {
		i = INTEGER(subset)[j] - 1;
		if (i < 0 || i >= src_length)
			error("subscript out of bounds");
		INTEGER(ans)[j] = (unsigned char) RAW(src)[i];
	}
	UNPROTECT(1);
	return ans;
}

/*
 * 'val' must be an integer vector of length > 0.
 */
SEXP RawPtr_write_ints_to_i1i2(SEXP dest_rawptr_xp, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest;
	int val_length;
	int i1, i2, n, j;
	int v;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	if (i1 < 0 || i2 >= LENGTH(dest))
		error("subscript out of bounds");
	n = i2 - i1 + 1;
	val_length = LENGTH(val);
	if (val_length == 0 && n != 0)
		error("no value provided");

	for (j = 0; i1 <= i2; i1++, j++) {
		if (j >= val_length)
			j = 0; /* recycle */
		v = INTEGER(val)[j];
		if (v < 0 || v >= 256)
			error("value out of range");
		RAW(dest)[i1] = (char) v;
	}
	if (j != val_length) {
		warning("number of items to replace is not a multiple "
			"of replacement length");
	}
	return dest_rawptr_xp;
}

SEXP RawPtr_write_ints_to_subset(SEXP dest_rawptr_xp, SEXP subset, SEXP val)
{
	SEXP dest;
	int dest_length, val_length;
	int n, i, j, z;
	int v;

	val_length = LENGTH(val);
	n = LENGTH(subset);
	if (val_length == 0 && n != 0)
		error("no value provided");
	dest = R_ExternalPtrTag(dest_rawptr_xp);
	dest_length = LENGTH(dest);

	for (j = z = 0; z < n; j++, z++) {
		i = INTEGER(subset)[z] - 1;
		if (i < 0 || i >= dest_length)
			error("subscript out of bounds");
		if (j >= val_length)
			j = 0; /* recycle */
		v = INTEGER(val)[j];
		if (v < 0 || v >= 256)
			error("value out of range");
		RAW(dest)[i] = (char) v;
	}
	if (j != val_length) {
		warning("number of items to replace is not a multiple "
			"of replacement length");
	}
	return dest_rawptr_xp;
}


/* ==========================================================================
 * Read/write encoded chars from/to an RawPtr object
 * --------------------------------------------------------------------------
 */

/*
 * Return a single string (character vector of length 1).
 */
SEXP RawPtr_read_enc_chars_from_i1i2(SEXP src_rawptr_xp, SEXP imin, SEXP imax, SEXP lkup)
{
	SEXP src;
	int i1, i2, n;
	CharAE dest;

	src = R_ExternalPtrTag(src_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	_IRanges_charcpy_from_i1i2_with_lkup(i1, i2,
			dest.elts, n, (char *) RAW(src), LENGTH(src),
			INTEGER(lkup), LENGTH(lkup));
	return mkString(dest.elts);
}

SEXP RawPtr_read_enc_chars_from_subset(SEXP src_rawptr_xp, SEXP subset, SEXP lkup)
{
	SEXP src;
	int n;
	CharAE dest;

	src = R_ExternalPtrTag(src_rawptr_xp);
	n = LENGTH(subset);
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	_IRanges_charcpy_from_subset_with_lkup(INTEGER(subset), n,
			dest.elts, n, (char *) RAW(src), LENGTH(src),
			INTEGER(lkup), LENGTH(lkup));
	return mkString(dest.elts);
}

/*
 * The RawPtr_write_enc_chars_to_i1i2() function is used when initializing
 * an XString object to encode and store the source string in the @xdata
 * slot of the object.
 * 'string' must be a non-empty single string (character vector of length 1).
 */
SEXP RawPtr_write_enc_chars_to_i1i2(SEXP dest_rawptr_xp, SEXP imin, SEXP imax,
		SEXP string, SEXP lkup)
{
	SEXP dest, src;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	src = STRING_ELT(string, 0);
	_IRanges_charcpy_to_i1i2_with_lkup(i1, i2,
			(char *) RAW(dest), LENGTH(dest),
			CHAR(src), LENGTH(src),
			INTEGER(lkup), LENGTH(lkup));
	return dest_rawptr_xp;
}

SEXP RawPtr_write_enc_chars_to_subset(SEXP dest_rawptr_xp, SEXP subset,
		SEXP string, SEXP lkup)
{
	SEXP dest, src;
	int n;

	dest = R_ExternalPtrTag(dest_rawptr_xp);
	n = LENGTH(subset);
	src = STRING_ELT(string, 0);
	_IRanges_charcpy_to_subset_with_lkup(INTEGER(subset), n,
		(char *) RAW(dest), LENGTH(dest), CHAR(src), LENGTH(src),
		INTEGER(lkup), LENGTH(lkup));
	return dest_rawptr_xp;
}


/* ==========================================================================
 * Read chars from an RawPtr object and convert them to a vector
 * of complexes.
 * --------------------------------------------------------------------------
 */

SEXP RawPtr_read_complexes_from_i1i2(SEXP src_rawptr_xp, SEXP imin, SEXP imax, SEXP lkup)
{
	SEXP dest, src;
	int i1, i2, n;

	src = R_ExternalPtrTag(src_rawptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;
	PROTECT(dest = NEW_COMPLEX(n));
	_IRanges_memcpy_from_i1i2_to_complex(i1, i2,
		COMPLEX(dest), n, (char *) RAW(src), LENGTH(src),
		COMPLEX(lkup), LENGTH(lkup));
	UNPROTECT(1);
	return dest;
}

SEXP RawPtr_read_complexes_from_subset(SEXP src_rawptr_xp, SEXP subset, SEXP lkup)
{
	SEXP dest, src;
	int n;

	src = R_ExternalPtrTag(src_rawptr_xp);
	n = LENGTH(subset);
	PROTECT(dest = NEW_COMPLEX(n));
	error("not available yet");
	UNPROTECT(1);
	return dest;
}


/* ==========================================================================
 * Copy and reverse copy bytes from an RawPtr object to another RawPtr object
 * with or without translation.
 * --------------------------------------------------------------------------
 */

SEXP RawPtr_translate_copy_from_i1i2(SEXP dest_xp, SEXP src_xp, SEXP imin, SEXP imax, SEXP lkup)
{
	SEXP dest, src;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	src = R_ExternalPtrTag(src_xp);
	_IRanges_charcpy_from_i1i2_with_lkup(i1, i2,
		(char *) RAW(dest), LENGTH(dest),
		(char *) RAW(src), LENGTH(src),
		INTEGER(lkup), LENGTH(lkup));
	return dest_xp;
}

SEXP RawPtr_translate_copy_from_subset(SEXP dest_xp, SEXP src_xp, SEXP subset, SEXP lkup)
{
	SEXP dest, src;

	dest = R_ExternalPtrTag(dest_xp);
	src = R_ExternalPtrTag(src_xp);
	_IRanges_charcpy_from_subset_with_lkup(INTEGER(subset), LENGTH(subset),
		(char *) RAW(dest), LENGTH(dest),
		(char *) RAW(src), LENGTH(src), 
		INTEGER(lkup), LENGTH(lkup));
	return dest_xp;
}

SEXP RawPtr_reverse_copy_from_i1i2(SEXP dest_xp, SEXP src_xp, SEXP imin, SEXP imax)
{
	SEXP dest, src;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	src = R_ExternalPtrTag(src_xp);
	_IRanges_reverse_memcpy_from_i1i2(i1, i2,
		(char *) RAW(dest), LENGTH(dest),
		(char *) RAW(src), LENGTH(src), sizeof(char));
	return dest_xp;
}

SEXP RawPtr_reverse_translate_copy_from_i1i2(SEXP dest_xp, SEXP src_xp, SEXP imin, SEXP imax, SEXP lkup)
{
	SEXP dest, src;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	src = R_ExternalPtrTag(src_xp);
	_IRanges_reverse_charcpy_from_i1i2_with_lkup(i1, i2,
		(char *) RAW(dest), LENGTH(dest),
		(char *) RAW(src), LENGTH(src),
		INTEGER(lkup), LENGTH(lkup));
	return dest_xp;
}

