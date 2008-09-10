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
SEXP IntegerPtr_alloc(SEXP intptr_xp, SEXP length)
{
	SEXP tag;
	int tag_length;

	tag_length = INTEGER(length)[0];
	PROTECT(tag = NEW_INTEGER(tag_length));
	R_SetExternalPtrTag(intptr_xp, tag);
	UNPROTECT(1);
	return intptr_xp;
}

SEXP IntegerPtr_alloc_initialize(SEXP intptr_xp, SEXP length)
{
	SEXP tag;
	int tag_length;

	tag_length = INTEGER(length)[0];

	PROTECT(tag = NEW_INTEGER(tag_length));
	memset(INTEGER(tag), 0, tag_length * sizeof(int));
	R_SetExternalPtrTag(intptr_xp, tag);
	UNPROTECT(1);
	return intptr_xp;
}

/*
 * Return the single string printed by the show method for IntegerPtr objects.
 * 'intptr_xp' must be the 'xp' slot of an IntegerPtr object.
 * From R:
 *   intptr <- IntegerPtr(30)
 *   .Call("IntegerPtr_get_show_string", intptr@xp, PACKAGE="IRanges")
 */
SEXP IntegerPtr_get_show_string(SEXP intptr_xp)
{
	SEXP tag, ans;
	int tag_length;
	char buf[100]; /* should be enough... */

	tag = R_ExternalPtrTag(intptr_xp);
	tag_length = LENGTH(tag);
	snprintf(buf, sizeof(buf), "%d-integer IntegerPtr object (data starting at memory address %p)",
		tag_length, INTEGER(tag));
	PROTECT(ans = NEW_CHARACTER(1));
	SET_STRING_ELT(ans, 0, mkChar(buf));
	UNPROTECT(1);
	return ans;
}

SEXP IntegerPtr_length(SEXP intptr_xp)
{
	SEXP tag, ans;
	int tag_length;

	tag = R_ExternalPtrTag(intptr_xp);
	tag_length = LENGTH(tag);

	PROTECT(ans = NEW_INTEGER(1));
	INTEGER(ans)[0] = tag_length;
	UNPROTECT(1);
	return ans;
}

/*
 * From R:
 *   xi <- IntegerPtr(30)
 *   .Call("IntegerPtr_memcmp", xi@xp, 1:1, xi@xp, 10:10, 21:21, PACKAGE="IRanges")
 */
SEXP IntegerPtr_memcmp(SEXP intptr1_xp, SEXP start1,
		 SEXP intptr2_xp, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, ans;
	int i1, i2, n;

	tag1 = R_ExternalPtrTag(intptr1_xp);
	i1 = INTEGER(start1)[0] - 1;
	tag2 = R_ExternalPtrTag(intptr2_xp);
	i2 = INTEGER(start2)[0] - 1;
	n = INTEGER(width)[0];

	PROTECT(ans = NEW_INTEGER(1));
	INTEGER(ans)[0] = _IRanges_memcmp((char *) INTEGER(tag1), i1,
					(char *) INTEGER(tag2), i2,
					n, sizeof(int));
	UNPROTECT(1);
	return ans;
}

/* ==========================================================================
 * Read/write integers to an IntegerPtr object
 * --------------------------------------------------------------------------
 */

SEXP IntegerPtr_read_ints_from_i1i2(SEXP src_intptr_xp, SEXP imin, SEXP imax)
{
	SEXP src, ans;
	int i1, i2, n;

	src = R_ExternalPtrTag(src_intptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	n = i2 - i1 + 1;

	PROTECT(ans = NEW_INTEGER(n));
	_IRanges_memcpy_from_i1i2(i1, i2,
			(char *) INTEGER(ans), LENGTH(ans),
			(char *) INTEGER(src), LENGTH(src), sizeof(int));
	UNPROTECT(1);
	return ans;
}

SEXP IntegerPtr_read_ints_from_subset(SEXP src_intptr_xp, SEXP subset)
{
	SEXP src, ans;
	int n;

	src = R_ExternalPtrTag(src_intptr_xp);
	n = LENGTH(subset);
	PROTECT(ans = NEW_INTEGER(n));
	_IRanges_memcpy_from_subset(INTEGER(subset), n,
			(char *) INTEGER(ans), n,
			(char *) INTEGER(src), LENGTH(src), sizeof(int));
	UNPROTECT(1);
	return ans;
}

/*
 * 'val' must be an integer vector.
 */
SEXP IntegerPtr_write_ints_to_i1i2(SEXP dest_intptr_xp, SEXP imin, SEXP imax, SEXP val)
{
	SEXP dest;
	int i1, i2;

	dest = R_ExternalPtrTag(dest_intptr_xp);
	i1 = INTEGER(imin)[0] - 1;
	i2 = INTEGER(imax)[0] - 1;
	_IRanges_memcpy_to_i1i2(i1, i2,
			(char *) INTEGER(dest), LENGTH(dest),
			(char *) INTEGER(val), LENGTH(val), sizeof(int));
	return dest_intptr_xp;
}

SEXP IntegerPtr_write_ints_to_subset(SEXP dest_intptr_xp, SEXP subset, SEXP val)
{
	SEXP dest;

	dest = R_ExternalPtrTag(dest_intptr_xp);
	_IRanges_memcpy_to_subset(INTEGER(subset), LENGTH(subset),
			(char *) INTEGER(dest), LENGTH(dest),
			(char *) INTEGER(val), LENGTH(val), sizeof(int));
	return dest_intptr_xp;
}

/*
 * --- .Call ENTRY POINT ---
 */
void IntegerPtr_coverage(SEXP x, SEXP weight, SEXP ans_xp)
{
	int x_len, ans_len, *ans_elt, i1, i2, j;
	const int *x_start, *x_width, *weight_elt;
	SEXP ans;

	ans = R_ExternalPtrTag(ans_xp);
	ans_len = LENGTH(ans);
	x_len = _get_IRanges_length(x);
	for (i1 = 0, x_start = _get_IRanges_start0(x),
	             x_width = _get_IRanges_width0(x),
	     i2 = 0, weight_elt = INTEGER(weight);
	     i1 < x_len;
	     i1++, x_start++, x_width++, i2++, weight_elt++)
	{
		if (i2 >= LENGTH(weight)) {
			/* recycle */
			i2 = 0;
			weight_elt = INTEGER(weight);
		}
		for (j = 0, ans_elt = INTEGER(ans) + *x_start - 1;
		     j < *x_width;
		     j++, ans_elt++)
		{
			*ans_elt += *weight_elt;
		}
	}
}

