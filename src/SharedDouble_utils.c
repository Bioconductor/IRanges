/****************************************************************************
 *                       Fast SharedDouble utilities                        *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_SharedDouble_utils()
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


SEXP SharedDouble_new(SEXP length, SEXP val)
{
	SEXP tag, ans;
	int tag_length, i;
	double val0;

	tag_length = INTEGER(length)[0];
	if (val == R_NilValue) {
		PROTECT(tag = NEW_NUMERIC(tag_length));
	} else if (LENGTH(val) == 1) {
		PROTECT(tag = NEW_NUMERIC(tag_length));
		val0 = REAL(val)[0];
		for (i = 0; i < tag_length; i++)
			REAL(tag)[i] = val0;
	} else if (LENGTH(val) == tag_length) {
		PROTECT(tag = duplicate(val));
	} else {
		error("when 'val' is not a single value, its length must "
		      "be equal to the value of the 'length' argument");
	}
	PROTECT(ans = _new_SharedVector("SharedDouble", tag));
	UNPROTECT(2);
	return ans;
}

SEXP SharedDouble_get_show_string(SEXP x)
{
	SEXP tag;
	int tag_length;
	char buf[100]; /* should be enough... */

	tag = _get_SharedVector_tag(x);
	tag_length = LENGTH(tag);
	snprintf(buf, sizeof(buf),
		"%d-number SharedDouble object (data starting at memory address %p)",
		tag_length, REAL(tag));
	return mkString(buf);
}

/*
 * From R:
 *   x <- SharedDouble(30)
 *   .Call("SharedDouble_memcmp", x, 1L, x, 10L, 21L, PACKAGE="IRanges")
 */
SEXP SharedDouble_memcmp(SEXP x1, SEXP start1, SEXP x2, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, ans;
	int i1, i2, n;

	tag1 = _get_SharedVector_tag(x1);
	i1 = INTEGER(start1)[0] - 1;
	tag2 = _get_SharedVector_tag(x2);
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
 * Read/write numerics to a SharedDouble object
 * --------------------------------------------------------------------------
 */

SEXP SharedDouble_read_nums_from_i1i2(SEXP src, SEXP imin, SEXP imax)
{
	SEXP src_tag, ans;
	int i1, i2, n;

	src_tag = _get_SharedVector_tag(src);
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

SEXP SharedDouble_read_nums_from_subset(SEXP src, SEXP subset)
{
	SEXP src_tag, ans;
	int n;

	src_tag = _get_SharedVector_tag(src);
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
SEXP SharedDouble_write_nums_to_i1i2(SEXP dest, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest_tag;
	int i1, i2;

	dest_tag = _get_SharedVector_tag(dest);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	_IRanges_memcpy_to_i1i2(i1, i2,
			(char *) REAL(dest_tag), LENGTH(dest_tag),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest;
}

SEXP SharedDouble_write_nums_to_subset(SEXP dest, SEXP subset, SEXP val)
{
	SEXP dest_tag;

	dest_tag = _get_SharedVector_tag(dest);
	_IRanges_memcpy_to_subset(INTEGER(subset), LENGTH(subset),
			(char *) REAL(dest_tag), LENGTH(dest_tag),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest;
}

/* ==========================================================================
 * Copy values from a SharedDouble object to another SharedDouble object.
 * --------------------------------------------------------------------------
 */

/* Cyclic writing in 'dest' */
SEXP SharedDouble_copy_from_i1i2(SEXP dest, SEXP src, SEXP imin, SEXP imax)
{
  SEXP dest_tag, src_tag;
  int i1, i2;

  dest_tag = _get_SharedVector_tag(dest);
  src_tag = _get_SharedVector_tag(src);
  i1 = INTEGER(imin)[0] - 1;
  i2 = INTEGER(imax)[0] - 1;
  _IRanges_memcpy_from_i1i2(i1, i2,
                            (char *) REAL(dest_tag), LENGTH(dest_tag),
                            (char *) REAL(src_tag), LENGTH(src_tag), sizeof(double));
  return dest;
}

/* Cyclic writing in 'dest' */
SEXP SharedDouble_copy_from_subset(SEXP dest, SEXP src, SEXP subset)
{
  SEXP dest_tag, src_tag;

  dest_tag = _get_SharedVector_tag(dest);
  src_tag = _get_SharedVector_tag(src);
  _IRanges_memcpy_from_subset(INTEGER(subset), LENGTH(subset),
                              (char *) REAL(dest_tag), LENGTH(dest_tag),
                              (char *) REAL(src_tag), LENGTH(src_tag), sizeof(double));
  return dest;
}
