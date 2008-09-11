/****************************************************************************
 *                        Fast IntegerPtr utilities                         *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_IntegerPtr_utils()
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
 * Memory allocation for an IntegerPtr object.
 * The data of an IntegerPtr object are stored in an "external" integer vector
 * (INTSXP vector).
 * The allocated memory is NOT initialized!
 */
SEXP IntegerPtr_alloc(SEXP x_xp, SEXP length)
{
	SEXP tag;
	int tag_length;

	tag_length = INTEGER(length)[0];
	PROTECT(tag = NEW_INTEGER(tag_length));
	R_SetExternalPtrTag(x_xp, tag);
	UNPROTECT(1);
	return x_xp;
}

SEXP IntegerPtr_alloc_initialize(SEXP x_xp, SEXP length)
{
	SEXP tag;
	int tag_length;

	tag_length = INTEGER(length)[0];

	PROTECT(tag = NEW_INTEGER(tag_length));
	memset(INTEGER(tag), 0, tag_length * sizeof(int));
	R_SetExternalPtrTag(x_xp, tag);
	UNPROTECT(1);
	return x_xp;
}

SEXP IntegerPtr_get_show_string(SEXP x)
{
	SEXP tag;
	int tag_length;
	char buf[100]; /* should be enough... */

	tag = _get_SequencePtr_tag(x);
	tag_length = LENGTH(tag);
	snprintf(buf, sizeof(buf), "%d-integer IntegerPtr object (data starting at memory address %p)",
		tag_length, INTEGER(tag));
	return mkString(buf);
}

/*
 * From R:
 *   x <- IntegerPtr(30)
 *   .Call("IntegerPtr_memcmp", x, 1L, x, 10L, 21L, PACKAGE="IRanges")
 */
SEXP IntegerPtr_memcmp(SEXP x1, SEXP start1, SEXP x2, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, tag;
	int i1, i2, n;

	tag1 = _get_SequencePtr_tag(x1);
	i1 = INTEGER(start1)[0] - 1;
	tag2 = _get_SequencePtr_tag(x2);
	i2 = INTEGER(start2)[0] - 1;
	n = INTEGER(width)[0];

	PROTECT(tag = NEW_INTEGER(1));
	INTEGER(tag)[0] = _IRanges_memcmp((char *) INTEGER(tag1), i1,
				(char *) INTEGER(tag2), i2,
				n, sizeof(int));
	UNPROTECT(1);
	return tag;
}

/* ==========================================================================
 * Read/write integers to an IntegerPtr object
 * --------------------------------------------------------------------------
 */

SEXP IntegerPtr_read_ints_from_i1i2(SEXP src, SEXP imin, SEXP imax)
{
	SEXP src_tag, tag;
	int i1, i2, n;

	src_tag = _get_SequencePtr_tag(src);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;

	PROTECT(tag = NEW_INTEGER(n));
	_IRanges_memcpy_from_i1i2(i1, i2,
			(char *) INTEGER(tag), LENGTH(tag),
			(char *) INTEGER(src_tag), LENGTH(src_tag), sizeof(int));
	UNPROTECT(1);
	return tag;
}

SEXP IntegerPtr_read_ints_from_subset(SEXP src, SEXP subset)
{
	SEXP src_tag, tag;
	int n;

	src_tag = _get_SequencePtr_tag(src);
	n = LENGTH(subset);
	PROTECT(tag = NEW_INTEGER(n));
	_IRanges_memcpy_from_subset(INTEGER(subset), n,
			(char *) INTEGER(tag), n,
			(char *) INTEGER(src_tag), LENGTH(src_tag), sizeof(int));
	UNPROTECT(1);
	return tag;
}

/*
 * 'val' must be an integer vector.
 */
SEXP IntegerPtr_write_ints_to_i1i2(SEXP dest, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest_tag;
	int i1, i2;

	dest_tag = _get_SequencePtr_tag(dest);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	_IRanges_memcpy_to_i1i2(i1, i2,
			(char *) INTEGER(dest_tag), LENGTH(dest_tag),
			(char *) INTEGER(val), LENGTH(val), sizeof(int));
	return dest;
}

SEXP IntegerPtr_write_ints_to_subset(SEXP dest, SEXP subset, SEXP val)
{
	SEXP dest_tag;

	dest_tag = _get_SequencePtr_tag(dest);
	_IRanges_memcpy_to_subset(INTEGER(subset), LENGTH(subset),
			(char *) INTEGER(dest_tag), LENGTH(dest_tag),
			(char *) INTEGER(val), LENGTH(val), sizeof(int));
	return dest;
}

