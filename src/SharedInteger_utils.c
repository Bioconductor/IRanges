/****************************************************************************
 *                       Fast SharedInteger utilities                       *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_SharedInteger_utils()
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


SEXP SharedInteger_new(SEXP length, SEXP val)
{
	SEXP tag, ans;
	int tag_length, i, val0;

	tag_length = INTEGER(length)[0];
	if (val == R_NilValue) {
		PROTECT(tag = NEW_INTEGER(tag_length));
	} else if (LENGTH(val) == 1) {
		PROTECT(tag = NEW_INTEGER(tag_length));
		val0 = INTEGER(val)[0];
		for (i = 0; i < tag_length; i++)
			INTEGER(tag)[i] = val0;
	} else if (LENGTH(val) == tag_length) {
		PROTECT(tag = duplicate(val));
	} else {
		error("when 'val' is not a single value, its length must "
		      "be equal to the value of the 'length' argument");
	}
	PROTECT(ans = _new_SharedVector("SharedInteger", tag));
	UNPROTECT(2);
	return ans;
}

SEXP SharedInteger_get_show_string(SEXP x)
{
	SEXP tag;
	int tag_length;
	char buf[100]; /* should be enough... */

	tag = _get_SharedVector_tag(x);
	tag_length = LENGTH(tag);
	snprintf(buf, sizeof(buf),
		"%d-integer SharedInteger object (data starting at memory address %p)",
		tag_length, INTEGER(tag));
	return mkString(buf);
}


/* ==========================================================================
 * Read/write integers to a SharedInteger object.
 * --------------------------------------------------------------------------
 */

SEXP SharedInteger_read_ints_from_i1i2(SEXP src, SEXP imin, SEXP imax)
{
	SEXP src_tag, tag;
	int i1, i2, n;

	src_tag = _get_SharedVector_tag(src);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;

	PROTECT(tag = NEW_INTEGER(n));
	_Ocopy_byteblocks_from_i1i2(i1, i2,
			(char *) INTEGER(tag), LENGTH(tag),
			(char *) INTEGER(src_tag), LENGTH(src_tag), sizeof(int));
	UNPROTECT(1);
	return tag;
}

SEXP SharedInteger_read_ints_from_subscript(SEXP src, SEXP subscript)
{
	SEXP src_tag, tag;
	int n;

	src_tag = _get_SharedVector_tag(src);
	n = LENGTH(subscript);
	PROTECT(tag = NEW_INTEGER(n));
	_Ocopy_byteblocks_from_subscript(INTEGER(subscript), n,
			(char *) INTEGER(tag), n,
			(char *) INTEGER(src_tag), LENGTH(src_tag), sizeof(int));
	UNPROTECT(1);
	return tag;
}

/*
 * 'val' must be an integer vector.
 */
SEXP SharedInteger_write_ints_to_i1i2(SEXP dest, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest_tag;
	int i1, i2;

	dest_tag = _get_SharedVector_tag(dest);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	_Ocopy_byteblocks_to_i1i2(i1, i2,
			(char *) INTEGER(dest_tag), LENGTH(dest_tag),
			(char *) INTEGER(val), LENGTH(val), sizeof(int));
	return dest;
}

SEXP SharedInteger_write_ints_to_subscript(SEXP dest, SEXP subscript, SEXP val)
{
	SEXP dest_tag;

	dest_tag = _get_SharedVector_tag(dest);
	_Ocopy_byteblocks_to_subscript(INTEGER(subscript), LENGTH(subscript),
			(char *) INTEGER(dest_tag), LENGTH(dest_tag),
			(char *) INTEGER(val), LENGTH(val), sizeof(int));
	return dest;
}

