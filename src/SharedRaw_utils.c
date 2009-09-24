/****************************************************************************
 *                         Fast SharedRaw utilities                         *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_SharedRaw_utils()
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


SEXP SharedRaw_new(SEXP length, SEXP val)
{
	SEXP tag, ans;
	int tag_length, i;
	Rbyte val0;

	tag_length = INTEGER(length)[0];
	if (val == R_NilValue) {
		PROTECT(tag = NEW_RAW(tag_length));
	} else if (LENGTH(val) == 1) {
		PROTECT(tag = NEW_RAW(tag_length));
		val0 = RAW(val)[0];
		for (i = 0; i < tag_length; i++)
			RAW(tag)[i] = val0;
	} else if (LENGTH(val) == tag_length) {
		PROTECT(tag = duplicate(val));
	} else {
		error("when 'val' is not a single value, its length must "
		      "be equal to the value of the 'length' argument");
	}
	PROTECT(ans = _new_SharedVector("SharedRaw", tag));
	UNPROTECT(2);
	return ans;
}

SEXP SharedRaw_address0(SEXP x)
{
	SEXP tag;
	char buf[20]; /* should be enough... */

	tag = _get_SharedVector_tag(x);
	snprintf(buf, sizeof(buf), "%p", RAW(tag));
        return mkString(buf);
}

/*
 * --- .Call ENTRY POINT ---
 * Comparing the data between 2 SharedRaw objects.
 * From R:
 *   x <- SharedRaw(30)
 *   .Call("SharedRaw_memcmp", x, 1L, x, 10L, 21L, PACKAGE="IRanges")
 */

SEXP SharedRaw_memcmp(SEXP x1, SEXP start1, SEXP x2, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, ans;
	int i1, i2, n;

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] SharedRaw_memcmp(): BEGIN\n");
	}
#endif
	tag1 = _get_SharedVector_tag(x1);
	i1 = INTEGER(start1)[0] - 1;
	tag2 = _get_SharedVector_tag(x2);
	i2 = INTEGER(start2)[0] - 1;
	n = INTEGER(width)[0];

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] SharedRaw_memcmp(): ");
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
		Rprintf("[DEBUG] SharedRaw_memcmp(): END\n");
	}
#endif
	UNPROTECT(1);
	return ans;
}


/* ==========================================================================
 * Read/write chars from/to a SharedRaw object.
 * All the functions in this group assume that sizeof(Rbyte) == sizeof(char).
 * --------------------------------------------------------------------------
 */

/*
 * Return a single string (character vector of length 1).
 * From R:
 *   x <- SharedRaw(15)
 *   x[] < "Hello"
 *   .Call("SharedRaw_read_chars_from_i1i2", x, 2:2, 4:4, PACKAGE="IRanges")
 */
SEXP SharedRaw_read_chars_from_i1i2(SEXP src, SEXP imin, SEXP imax)
{
	SEXP src_tag;
	int i1, i2, n;
	CharAE dest;

	src_tag = _get_SharedVector_tag(src);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_from_i1i2(i1, i2,
			dest.elts, n, (char *) RAW(src_tag), LENGTH(src_tag),
			sizeof(char));
	return mkString(dest.elts);
}

SEXP SharedRaw_read_chars_from_subset(SEXP src, SEXP subset)
{
	SEXP src_tag;
	int n;
	CharAE dest;

	src_tag = _get_SharedVector_tag(src);
	n = LENGTH(subset);
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_from_subset(INTEGER(subset), n,
			dest.elts, n, (char *) RAW(src_tag), LENGTH(src_tag),
			sizeof(char));
	return mkString(dest.elts);
}

/*
 * 'string' must be a non-empty single string (character vector of length 1).
 */
SEXP SharedRaw_write_chars_to_i1i2(SEXP dest, SEXP imin, SEXP imax, SEXP string)
{
	SEXP dest_tag, src;
	int i1, i2;

	dest_tag = _get_SharedVector_tag(dest);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	src = STRING_ELT(string, 0);
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_to_i1i2(i1, i2,
			(char *) RAW(dest_tag), LENGTH(dest_tag),
			CHAR(src), LENGTH(src), sizeof(char));
	return dest;
}

SEXP SharedRaw_write_chars_to_subset(SEXP dest, SEXP subset, SEXP string)
{
	SEXP dest_tag, src;

	dest_tag = _get_SharedVector_tag(dest);
	src = STRING_ELT(string, 0);
	/* assumes that sizeof(Rbyte) == sizeof(char) */
	_IRanges_memcpy_to_subset(INTEGER(subset), LENGTH(subset),
			(char *) RAW(dest_tag), LENGTH(dest_tag),
			CHAR(src), LENGTH(src), sizeof(char));
	return dest;
}


/* ==========================================================================
 * Read/write integers from/to a SharedRaw object
 * --------------------------------------------------------------------------
 */

/*
 * Return an integer vector of length 'imax' - 'imin' + 1.
 * From R:
 *   x <- SharedRaw(30)
 *   .Call("SharedRaw_read_ints_from_i1i2", x, 20:20, 25:25, PACKAGE="IRanges")
 */
SEXP SharedRaw_read_ints_from_i1i2(SEXP src, SEXP imin, SEXP imax)
{
	SEXP src_tag, ans;
	int i1, i2, n, j;

	src_tag = _get_SharedVector_tag(src);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	if (i1 < 0 || i2 >= LENGTH(src_tag))
		error("subscript out of bounds");
	n = i2 - i1 + 1;

	PROTECT(ans = NEW_INTEGER(n));
	for (j = 0; i1 <= i2; i1++, j++) {
		INTEGER(ans)[j] = (unsigned char) RAW(src_tag)[i1];
	}
	UNPROTECT(1);
	return ans;
}

/*
 * Return an integer vector of same length as 'subset'.
 * From R:
 *   x <- SharedRaw(30)
 *   .Call("SharedRaw_read_ints_from_subset", x, 25:20, PACKAGE="IRanges")
 */
SEXP SharedRaw_read_ints_from_subset(SEXP src, SEXP subset)
{
	SEXP src_tag, ans;
	int src_length;
	int n, i, j;

	src_tag = _get_SharedVector_tag(src);
	src_length = LENGTH(src_tag);
	n = LENGTH(subset);

	PROTECT(ans = NEW_INTEGER(n));
	for (j = 0; j < n; j++) {
		i = INTEGER(subset)[j] - 1;
		if (i < 0 || i >= src_length)
			error("subscript out of bounds");
		INTEGER(ans)[j] = (unsigned char) RAW(src_tag)[i];
	}
	UNPROTECT(1);
	return ans;
}

/*
 * 'val' must be an integer vector of length > 0.
 */
SEXP SharedRaw_write_ints_to_i1i2(SEXP dest, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest_tag;
	int val_length;
	int i1, i2, n, j;
	int v;

	dest_tag = _get_SharedVector_tag(dest);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	if (i1 < 0 || i2 >= LENGTH(dest_tag))
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
		RAW(dest_tag)[i1] = (char) v;
	}
	if (j != val_length) {
		warning("number of items to replace is not a multiple "
			"of replacement length");
	}
	return dest;
}

SEXP SharedRaw_write_ints_to_subset(SEXP dest, SEXP subset, SEXP val)
{
	SEXP dest_tag;
	int dest_length, val_length;
	int n, i, j, z;
	int v;

	val_length = LENGTH(val);
	n = LENGTH(subset);
	if (val_length == 0 && n != 0)
		error("no value provided");
	dest_tag = _get_SharedVector_tag(dest);
	dest_length = LENGTH(dest_tag);

	for (j = z = 0; z < n; j++, z++) {
		i = INTEGER(subset)[z] - 1;
		if (i < 0 || i >= dest_length)
			error("subscript out of bounds");
		if (j >= val_length)
			j = 0; /* recycle */
		v = INTEGER(val)[j];
		if (v < 0 || v >= 256)
			error("value out of range");
		RAW(dest_tag)[i] = (char) v;
	}
	if (j != val_length) {
		warning("number of items to replace is not a multiple "
			"of replacement length");
	}
	return dest;
}


/* ==========================================================================
 * Read/write encoded chars from/to a SharedRaw object
 * --------------------------------------------------------------------------
 */

/*
 * Return a single string (character vector of length 1).
 */
SEXP SharedRaw_read_enc_chars_from_i1i2(SEXP src, SEXP imin, SEXP imax, SEXP lkup)
{
	SEXP src_tag;
	int i1, i2, n;
	CharAE dest;

	src_tag = _get_SharedVector_tag(src);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	_IRanges_charcpy_from_i1i2_with_lkup(i1, i2,
			dest.elts, n, (char *) RAW(src_tag), LENGTH(src_tag),
			INTEGER(lkup), LENGTH(lkup));
	return mkString(dest.elts);
}

SEXP SharedRaw_read_enc_chars_from_subset(SEXP src, SEXP subset, SEXP lkup)
{
	SEXP src_tag;
	int n;
	CharAE dest;

	src_tag = _get_SharedVector_tag(src);
	n = LENGTH(subset);
	dest = _new_CharAE(n + 1);
	dest.elts[n] = '\0';
	_IRanges_charcpy_from_subset_with_lkup(INTEGER(subset), n,
			dest.elts, n, (char *) RAW(src_tag), LENGTH(src_tag),
			INTEGER(lkup), LENGTH(lkup));
	return mkString(dest.elts);
}

/*
 * The SharedRaw_write_enc_chars_to_i1i2() function is used when initializing
 * an XString object to encode and store the source string in the @shared
 * slot of the object.
 * 'string' must be a non-empty single string (character vector of length 1).
 */
SEXP SharedRaw_write_enc_chars_to_i1i2(SEXP dest, SEXP imin, SEXP imax,
		SEXP string, SEXP lkup)
{
	SEXP dest_tag, src;
	int i1, i2;

	dest_tag = _get_SharedVector_tag(dest);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	src = STRING_ELT(string, 0);
	_IRanges_charcpy_to_i1i2_with_lkup(i1, i2,
			(char *) RAW(dest_tag), LENGTH(dest_tag),
			CHAR(src), LENGTH(src),
			INTEGER(lkup), LENGTH(lkup));
	return dest;
}

SEXP SharedRaw_write_enc_chars_to_subset(SEXP dest, SEXP subset,
		SEXP string, SEXP lkup)
{
	SEXP dest_tag, src;
	int n;

	dest_tag = _get_SharedVector_tag(dest);
	n = LENGTH(subset);
	src = STRING_ELT(string, 0);
	_IRanges_charcpy_to_subset_with_lkup(INTEGER(subset), n,
		(char *) RAW(dest_tag), LENGTH(dest_tag), CHAR(src), LENGTH(src),
		INTEGER(lkup), LENGTH(lkup));
	return dest;
}


/* ==========================================================================
 * Read chars from a SharedRaw object and convert them to a vector
 * of complexes.
 * --------------------------------------------------------------------------
 */

SEXP SharedRaw_read_complexes_from_i1i2(SEXP src, SEXP imin, SEXP imax, SEXP lkup)
{
	SEXP dest, src_tag;
	int i1, i2, n;

	src_tag = _get_SharedVector_tag(src);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;
	PROTECT(dest = NEW_COMPLEX(n));
	_IRanges_memcpy_from_i1i2_to_complex(i1, i2,
		COMPLEX(dest), n, (char *) RAW(src_tag), LENGTH(src_tag),
		COMPLEX(lkup), LENGTH(lkup));
	UNPROTECT(1);
	return dest;
}

SEXP SharedRaw_read_complexes_from_subset(SEXP src, SEXP subset, SEXP lkup)
{
	SEXP dest, src_tag;
	int n;

	src_tag = _get_SharedVector_tag(src);
	n = LENGTH(subset);
	PROTECT(dest = NEW_COMPLEX(n));
	error("not available yet");
	UNPROTECT(1);
	return dest;
}

