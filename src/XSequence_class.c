/****************************************************************************
 *               Low-level manipulation of XSequence objects                *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_XSequence_class()
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


/****************************************************************************
 * C-level slot getters.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call() entry points!
 */

static SEXP
	xdata_symbol = NULL,
	offset_symbol = NULL,
	length_symbol = NULL;

SEXP _get_XSequence_xdata(SEXP x)
{
	INIT_STATIC_SYMBOL(xdata);
	return GET_SLOT(x, xdata_symbol);
}

SEXP _get_XSequence_tag(SEXP x)
{
	return _get_SequencePtr_tag(_get_XSequence_xdata(x));
}

SEXP _get_XSequence_offset(SEXP x)
{
	INIT_STATIC_SYMBOL(offset);
	return GET_SLOT(x, offset_symbol);
}

SEXP _get_XSequence_length(SEXP x)
{
	INIT_STATIC_SYMBOL(length);
	return GET_SLOT(x, length_symbol);
}


/****************************************************************************
 * Caching.
 */

cachedCharSeq _cache_XRaw(SEXP x)
{
	cachedCharSeq cached_x;
	SEXP tag;
	int offset;

	tag = _get_XSequence_tag(x);
	offset = INTEGER(_get_XSequence_offset(x))[0];
	cached_x.seq = (const char *) (RAW(tag) + offset);
	cached_x.length = INTEGER(_get_XSequence_length(x))[0];
	return cached_x;
}


/****************************************************************************
 * C-level slot setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_XSequence_xdata(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(xdata);
	SET_SLOT(x, xdata_symbol, value);
	return;
}

static void set_XSequence_offset(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(offset);
	SET_SLOT(x, offset_symbol, value);
	return;
}

static void set_XSequence_length(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(length);
	SET_SLOT(x, length_symbol, value);
	return;
}

static void set_XSequence_slots(SEXP x, SEXP xdata, SEXP offset, SEXP length)
{
	set_XSequence_xdata(x, xdata);
	set_XSequence_offset(x, offset);
	set_XSequence_length(x, length);
}


/****************************************************************************
 * C-level constructors.
 *
 * Be careful that these functions do NOT duplicate their arguments before
 * putting them in the slots of the returned object.
 * Thus they cannot be made .Call() entry points!
 */

SEXP _new_XSequence(const char *classname, SEXP xdata, int offset, int length)
{
	SEXP classdef, ans, ans_offset, ans_length;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	PROTECT(ans_offset = ScalarInteger(offset));
	PROTECT(ans_length = ScalarInteger(length));
	set_XSequence_slots(ans, xdata, ans_offset, ans_length);
	UNPROTECT(4);
	return ans;
}

SEXP _new_XRaw_from_tag(const char *classname, SEXP tag)
{
	SEXP xdata, ans;

	if (!IS_RAW(tag))
		error("IRanges internal error in _new_XRaw_from_tag(): "
		      "'tag' is not RAW");
	PROTECT(xdata = _new_SequencePtr("RawPtr", tag));
	PROTECT(ans = _new_XSequence(classname, xdata, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

SEXP _new_XInteger_from_tag(const char *classname, SEXP tag)
{
	SEXP xdata, ans;

	if (!IS_INTEGER(tag))
		error("IRanges internal error in _new_XInteger_from_tag(): "
		      "'tag' is not INTEGER");
	PROTECT(xdata = _new_SequencePtr("IntegerPtr", tag));
	PROTECT(ans = _new_XSequence(classname, xdata, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

SEXP _new_XNumeric_from_tag(const char *classname, SEXP tag)
{
	SEXP xdata, ans;

	if (!IS_NUMERIC(tag))
		error("IRanges internal error in _new_XNumeric_from_tag(): "
		      "'tag' is not NUMERIC");
	PROTECT(xdata = _new_SequencePtr("NumericPtr", tag));
	PROTECT(ans = _new_XSequence(classname, xdata, 0, LENGTH(tag)));
	UNPROTECT(2);
	return ans;
}

