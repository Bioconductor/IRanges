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
 * C-level slot getters.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call() entry points!
 */

static SEXP
	start_symbol = NULL,
	width_symbol = NULL,
	NAMES_symbol = NULL;

SEXP _get_IRanges_start(SEXP x)
{
	INIT_STATIC_SYMBOL(start);
	return GET_SLOT(x, start_symbol);
}

int _get_IRanges_length(SEXP x)
{
	return LENGTH(_get_IRanges_start(x));
}

SEXP _get_IRanges_width(SEXP x)
{
	INIT_STATIC_SYMBOL(width);
	return GET_SLOT(x, width_symbol);
}

SEXP _get_IRanges_names(SEXP x)
{
	INIT_STATIC_SYMBOL(NAMES);
	return GET_SLOT(x, NAMES_symbol);
}


/****************************************************************************
 * C-level abstract getters.
 */

cachedIRanges _cache_IRanges(SEXP x)
{
	cachedIRanges cached_x;

	cached_x.classname = _get_classname(x);
	cached_x.is_constant_width = 0;
	cached_x.offset = 0;
	cached_x.length = _get_IRanges_length(x);
	cached_x.width = INTEGER(_get_IRanges_width(x));
	cached_x.start = INTEGER(_get_IRanges_start(x));
	cached_x.end = NULL;
	cached_x.names = _get_IRanges_names(x);
	return cached_x;
}

int _get_cachedIRanges_length(const cachedIRanges *cached_x)
{
	return cached_x->length;
}

int _get_cachedIRanges_elt_width(const cachedIRanges *cached_x, int i)
{
	return cached_x->is_constant_width ?
	       cached_x->width[0] : cached_x->width[i];
}

int _get_cachedIRanges_elt_start(const cachedIRanges *cached_x, int i)
{
	if (cached_x->start)
		return cached_x->start[i];
	return cached_x->end[i] - _get_cachedIRanges_elt_width(cached_x, i) + 1;
}

int _get_cachedIRanges_elt_end(const cachedIRanges *cached_x, int i)
{
	if (cached_x->end)
		return cached_x->end[i];
	return cached_x->start[i] + _get_cachedIRanges_elt_width(cached_x, i) - 1;
}

SEXP _get_cachedIRanges_elt_name(const cachedIRanges *cached_x, int i)
{
	return STRING_ELT(cached_x->names, cached_x->offset + i);
}

cachedIRanges _sub_cachedIRanges(const cachedIRanges *cached_x, int offset, int length)
{
	cachedIRanges cached_y;

	cached_y = *cached_x;
	cached_y.offset += offset;
	cached_y.length = length;
	cached_y.start += offset;
	if (!cached_y.is_constant_width)
		cached_y.width += offset;
	return cached_y;
}


/****************************************************************************
 * C-level slot setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_IRanges_start(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(start);
	SET_SLOT(x, start_symbol, value);
	return;
}

static void set_IRanges_width(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(width);
	SET_SLOT(x, width_symbol, value);
	return;
}

static void set_IRanges_names(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(NAMES);
	SET_SLOT(x, NAMES_symbol, value);
	return;
}

void _set_IRanges_names(SEXP x, SEXP names)
{
	if (names == NULL)
		names = R_NilValue;
	else if (LENGTH(names) != _get_IRanges_length(x))
		error("_set_IRanges_names(): "
		      "number of names and number of elements differ");
	set_IRanges_names(x, names);
	return;
}

/*
 * Note that 'start' and 'width' must NOT contain NAs.
 * set_IRanges_slots() trusts the caller and does NOT check this!
 */
static void set_IRanges_slots(SEXP x, SEXP start, SEXP width, SEXP names)
{
	if (LENGTH(width) != LENGTH(start))
		error("set_IRanges_slots(): "
		      "number of starts and number of widths differ");
	set_IRanges_start(x, start);
	set_IRanges_width(x, width);
	_set_IRanges_names(x, names);
	return;
}

void _copy_IRanges_slots(SEXP x, SEXP x0)
{
	set_IRanges_start(x, duplicate(_get_IRanges_start(x0)));
	set_IRanges_width(x, duplicate(_get_IRanges_width(x0)));
	set_IRanges_names(x, duplicate(_get_IRanges_names(x0)));
	return;
}


/****************************************************************************
 * C-level constructors.
 *
 * Be careful that these functions do NOT duplicate their arguments before
 * putting them in the slots of the returned object.
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


/*
 * --- .Call ENTRY POINT ---
 */
SEXP IRanges_from_integer(SEXP x)
{
	SEXP ans, ans_start, ans_width;
	int i, x_length, ans_length;
	int *start_buf, *width_buf;
	int *x_elt, *start_elt, *width_elt, prev_elt_plus1;

	x_length = LENGTH(x);
	if (x_length == 0) {
		PROTECT(ans_start = NEW_INTEGER(0));
		PROTECT(ans_width = NEW_INTEGER(0));
	} else {
		ans_length = 1;
		start_buf = (int *) R_alloc((long) x_length, sizeof(int));
		width_buf = (int *) R_alloc((long) x_length, sizeof(int));
		start_buf[0] = INTEGER(x)[0];
		width_buf[0] = 1;
		prev_elt_plus1 = start_buf[0] + 1;
		start_elt = start_buf;
		width_elt = width_buf;
		for (i = 1, x_elt = (INTEGER(x)+1); i < x_length; i++, x_elt++) {
			if (*x_elt == NA_INTEGER)
				error("cannot create an IRanges object from an integer vector with missing values");
			if (*x_elt == prev_elt_plus1) {
				*width_elt += 1;
			} else {
				ans_length++;
				start_elt++;
				width_elt++;
				*start_elt = *x_elt;
				*width_elt = 1;
				prev_elt_plus1 = *x_elt;
			}
			prev_elt_plus1++;
		}
		PROTECT(ans_start = NEW_INTEGER(ans_length));
		PROTECT(ans_width = NEW_INTEGER(ans_length));
		memcpy(INTEGER(ans_start), start_buf, sizeof(int) * ans_length);
		memcpy(INTEGER(ans_width), width_buf, sizeof(int) * ans_length);
	}

	PROTECT(ans = _new_IRanges("IRanges", ans_start, ans_width, R_NilValue));
	UNPROTECT(3);
	return ans;
}


/*
 * --- .Call ENTRY POINT ---
 */
SEXP NormalIRanges_from_logical(SEXP x)
{
	SEXP ans, ans_start, ans_width;
	int i, x_length, ans_length;
	int *start_buf, *width_buf;
	int *x_elt, *start_elt, *width_elt, prev_elt;

	x_length = LENGTH(x);
	if (x_length == 0) {
		PROTECT(ans_start = NEW_INTEGER(0));
		PROTECT(ans_width = NEW_INTEGER(0));
	} else {
		ans_length = 0;
		start_buf = (int *) R_alloc((long) x_length, sizeof(int));
		width_buf = (int *) R_alloc((long) x_length, sizeof(int));
		prev_elt = 0;
		start_elt = start_buf - 1;
		width_elt = width_buf - 1;
		for (i = 1, x_elt = LOGICAL(x); i <= x_length; i++, x_elt++) {
			if (*x_elt == NA_LOGICAL)
				error("cannot create an IRanges object from a logical vector with missing values");
			if (*x_elt == 1) {
				if (prev_elt) {
					*width_elt += 1;
				} else {
					ans_length++;
					start_elt++;
					width_elt++;
					*start_elt = i;
					*width_elt = 1;
				}
			}
			prev_elt = *x_elt;
		}
		PROTECT(ans_start = NEW_INTEGER(ans_length));
		PROTECT(ans_width = NEW_INTEGER(ans_length));
		memcpy(INTEGER(ans_start), start_buf, sizeof(int) * ans_length);
		memcpy(INTEGER(ans_width), width_buf, sizeof(int) * ans_length);
	}

	PROTECT(ans = _new_IRanges("NormalIRanges", ans_start, ans_width, R_NilValue));
	UNPROTECT(3);
	return ans;
}
