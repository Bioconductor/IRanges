/****************************************************************************
 *                        Fast NumericPtr utilities                         *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_NumericPtr_utils()
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
 * Memory allocation for a NumericPtr object.
 * The data of a NumericPtr object are stored in an "external" numeric vector
 * (REALSXP vector).
 * The allocated memory is NOT initialized!
 */
SEXP NumericPtr_alloc(SEXP x_xp, SEXP length)
{
	SEXP tag;
	int tag_length;

	tag_length = INTEGER(length)[0];
	PROTECT(tag = NEW_NUMERIC(tag_length));
	R_SetExternalPtrTag(x_xp, tag);
	UNPROTECT(1);
	return x_xp;
}

SEXP NumericPtr_get_show_string(SEXP x)
{
	SEXP tag;
	int tag_length;
	char buf[100]; /* should be enough... */

	tag = _get_SequencePtr_tag(x);
	tag_length = LENGTH(tag);
	snprintf(buf, sizeof(buf), "%d-number NumericPtr object (data starting at memory address %p)",
		tag_length, REAL(tag));
	return mkString(buf);
}

/*
 * From R:
 *   x <- NumericPtr(30)
 *   .Call("NumericPtr_memcmp", x, 1L, x, 10L, 21L, PACKAGE="IRanges")
 */
SEXP NumericPtr_memcmp(SEXP x1, SEXP start1, SEXP x2, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, ans;
	int i1, i2, n;

	tag1 = _get_SequencePtr_tag(x1);
	i1 = INTEGER(start1)[0] - 1;
	tag2 = _get_SequencePtr_tag(x2);
	i2 = INTEGER(start2)[0] - 1;
	n = INTEGER(width)[0];

	PROTECT(ans = NEW_NUMERIC(1));
	REAL(ans)[0] = _IRanges_memcmp((char *) REAL(tag1), i1,
					(char *) REAL(tag2), i2,
					n, sizeof(double));
	UNPROTECT(1);
	return ans;
}

/* ==========================================================================
 * Read/write numerics to a NumericPtr object
 * --------------------------------------------------------------------------
 */

SEXP NumericPtr_read_nums_from_i1i2(SEXP src, SEXP imin, SEXP imax)
{
	SEXP src_tag, ans;
	int i1, i2, n;

	src_tag = _get_SequencePtr_tag(src);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;

	PROTECT(ans = NEW_NUMERIC(n));
	_IRanges_memcpy_from_i1i2(i1, i2,
			(char *) REAL(ans), LENGTH(ans),
			(char *) REAL(src_tag), LENGTH(src_tag), sizeof(double));
	UNPROTECT(1);
	return ans;
}

SEXP NumericPtr_read_nums_from_subset(SEXP src, SEXP subset)
{
	SEXP src_tag, ans;
	int n;

	src_tag = _get_SequencePtr_tag(src);
	n = LENGTH(subset);
	PROTECT(ans = NEW_NUMERIC(n));
	_IRanges_memcpy_from_subset(INTEGER(subset), n,
			(char *) REAL(ans), n,
			(char *) REAL(src_tag), LENGTH(src_tag), sizeof(double));
	UNPROTECT(1);
	return ans;
}

/*
 * 'val' must be a numeric vector.
 */
SEXP NumericPtr_write_nums_to_i1i2(SEXP dest, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest_tag;
	int i1, i2;

	dest_tag = _get_SequencePtr_tag(dest);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	_IRanges_memcpy_to_i1i2(i1, i2,
			(char *) REAL(dest_tag), LENGTH(dest_tag),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest;
}

SEXP NumericPtr_write_nums_to_subset(SEXP dest, SEXP subset, SEXP val)
{
	SEXP dest_tag;

	dest_tag = _get_SequencePtr_tag(dest);
	_IRanges_memcpy_to_subset(INTEGER(subset), LENGTH(subset),
			(char *) REAL(dest_tag), LENGTH(dest_tag),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest;
}
