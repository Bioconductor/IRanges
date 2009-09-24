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
	_Ocopy_byteblocks_from_i1i2(i1, i2,
			(char *) REAL(ans), LENGTH(ans),
			(char *) REAL(src_tag), LENGTH(src_tag), sizeof(double));
	UNPROTECT(1);
	return ans;
}

SEXP SharedDouble_read_nums_from_subscript(SEXP src, SEXP subscript)
{
	SEXP src_tag, ans;
	int n;

	src_tag = _get_SharedVector_tag(src);
	n = LENGTH(subscript);
	PROTECT(ans = NEW_NUMERIC(n));
	_Ocopy_byteblocks_from_subscript(INTEGER(subscript), n,
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
	_Ocopy_byteblocks_to_i1i2(i1, i2,
			(char *) REAL(dest_tag), LENGTH(dest_tag),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest;
}

SEXP SharedDouble_write_nums_to_subscript(SEXP dest, SEXP subscript, SEXP val)
{
	SEXP dest_tag;

	dest_tag = _get_SharedVector_tag(dest);
	_Ocopy_byteblocks_to_subscript(INTEGER(subscript), LENGTH(subscript),
			(char *) REAL(dest_tag), LENGTH(dest_tag),
			(char *) REAL(val), LENGTH(val), sizeof(double));
	return dest;
}

