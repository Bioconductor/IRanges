/****************************************************************************
 *                Low-level manipulation of IRanges objects                 *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_IRanges_class()
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
 * C-level accessor functions for IRanges objects.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call() entry points!
 */

SEXP _get_IRanges_start(SEXP x)
{
	return GET_SLOT(x, install("start"));
}

SEXP _get_IRanges_width(SEXP x)
{
	return GET_SLOT(x, install("width"));
}

SEXP _get_IRanges_names(SEXP x)
{
	return GET_SLOT(x, install("NAMES"));
}


/****************************************************************************
 * Other functions.
 */

int _get_IRanges_length(SEXP x)
{
	return LENGTH(_get_IRanges_start(x));
}

const int *_get_IRanges_start0(SEXP x)
{
	return INTEGER(_get_IRanges_start(x));
}

const int *_get_IRanges_width0(SEXP x)
{
	return INTEGER(_get_IRanges_width(x));
}

/*
 * Does NOT duplicate 'x'. The @NAMES slot is modified in place!
 */
void _set_IRanges_names(SEXP x, SEXP names)
{
	if (names == R_NilValue || names == NULL) {
		SET_SLOT(x, mkChar("NAMES"), R_NilValue);
	} else if (LENGTH(names) == _get_IRanges_length(x)) {
		SET_SLOT(x, mkChar("NAMES"), names);
	} else {
		error("number of names and number of elements differ");
	}
	return;
}

/*
 * Note that 'start' and 'width' must NOT contain NAs.
 * set_IRanges_slots() trusts the caller and does NOT check this!
 */
static void set_IRanges_slots(SEXP x, SEXP start, SEXP width, SEXP names)
{
	if (LENGTH(width) != LENGTH(start))
		error("number of starts and number of widths differ");
	SET_SLOT(x, mkChar("start"), start);
	SET_SLOT(x, mkChar("width"), width);
	_set_IRanges_names(x, names);
	return;
}

void _copy_IRanges_slots(SEXP x, SEXP x0)
{
	SET_SLOT(x, mkChar("start"), duplicate(GET_SLOT(x0, install("start"))));
	SET_SLOT(x, mkChar("width"), duplicate(GET_SLOT(x0, install("width"))));
	SET_SLOT(x, mkChar("NAMES"), duplicate(GET_SLOT(x0, install("NAMES"))));
	return;
}


/****************************************************************************
 * C-level constructor functions for IRanges objects.
 *
 * Be careful that these functions do NOT duplicate their arguments before
 * they put them in the slots of the returned objects.
 * Thus they cannot be made .Call() entry points!
 */

SEXP _new_IRanges(const char *classname, SEXP start, SEXP width, SEXP names)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_IRanges_slots(ans, start, width, names);
	UNPROTECT(2);
	return ans;
}

/*
 * Allocation WITHOUT initialization.
 * The 'start' and 'width' slots are not initialized (they contain junk).
 */
SEXP _alloc_IRanges(const char *classname, int length)
{
        SEXP start, width, ans;

        PROTECT(start = NEW_INTEGER(length));
        PROTECT(width = NEW_INTEGER(length));
        PROTECT(ans = _new_IRanges(classname, start, width, R_NilValue));
        UNPROTECT(3);
        return ans;
}

