/****************************************************************************
 *                Low-level manipulation of IRanges objects                 *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"


/****************************************************************************
 * C-level slot getters.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call entry points!
 */

static SEXP
	start_symbol = NULL,
	width_symbol = NULL,
	NAMES_symbol = NULL;

SEXP _get_IRanges_start(SEXP x)
{
	INIT_STATIC_SYMBOL(start)
	return GET_SLOT(x, start_symbol);
}

SEXP _get_IRanges_width(SEXP x)
{
	INIT_STATIC_SYMBOL(width)
	return GET_SLOT(x, width_symbol);
}

SEXP _get_IRanges_names(SEXP x)
{
	INIT_STATIC_SYMBOL(NAMES)
	return GET_SLOT(x, NAMES_symbol);
}

/* Not a strict "slot getter" but very much like. */
int _get_IRanges_length(SEXP x)
{
	return LENGTH(_get_IRanges_start(x));
}


/****************************************************************************
 * C-level abstract getters.
 */

IRanges_holder _hold_IRanges(SEXP x)
{
	IRanges_holder x_holder;

	x_holder.classname = get_classname(x);
	x_holder.is_constant_width = 0;
	x_holder.length = _get_IRanges_length(x);
	x_holder.width = INTEGER(_get_IRanges_width(x));
	x_holder.start = INTEGER(_get_IRanges_start(x));
	x_holder.end = NULL;
	x_holder.SEXP_offset = 0;
	x_holder.names = _get_IRanges_names(x);
	return x_holder;
}

int _get_length_from_IRanges_holder(const IRanges_holder *x_holder)
{
	return x_holder->length;
}

int _get_width_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i)
{
	return x_holder->is_constant_width ?
	       x_holder->width[0] : x_holder->width[i];
}

int _get_start_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i)
{
	if (x_holder->start)
		return x_holder->start[i];
	return x_holder->end[i] -
	       _get_width_elt_from_IRanges_holder(x_holder, i) + 1;
}

int _get_end_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i)
{
	if (x_holder->end)
		return x_holder->end[i];
	return x_holder->start[i] +
	       _get_width_elt_from_IRanges_holder(x_holder, i) - 1;
}

SEXP _get_names_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i)
{
	return STRING_ELT(x_holder->names, x_holder->SEXP_offset + i);
}

IRanges_holder _get_linear_subset_from_IRanges_holder(
		const IRanges_holder *x_holder, int offset, int length)
{
	IRanges_holder y_holder;

	y_holder = *x_holder;
	y_holder.length = length;
	y_holder.start += offset;
	if (!y_holder.is_constant_width)
		y_holder.width += offset;
	y_holder.SEXP_offset += offset;
	return y_holder;
}


/****************************************************************************
 * C-level slot setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_IRanges_start(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(start)
	SET_SLOT(x, start_symbol, value);
	return;
}

static void set_IRanges_width(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(width)
	SET_SLOT(x, width_symbol, value);
/*
        Rprintf("set_IRanges_width(): value=%p _get_IRanges_width(x)=%p\n",
                value, _get_IRanges_width(x));
*/
	return;
}

static void set_IRanges_names(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(NAMES)
	SET_SLOT(x, NAMES_symbol, value);
	return;
}

/* WARNING: Use only AFTER 'x@start' has been set! Because this setter
   is trying to figure out what the length of 'x' is. */
void _set_IRanges_names(SEXP x, SEXP names)
{
	if (names == NULL)
		names = R_NilValue;
	else if (names != R_NilValue
	      && LENGTH(names) != _get_IRanges_length(x))
		error("_set_IRanges_names(): "
		      "number of names and number of elements differ");
	set_IRanges_names(x, names);
	return;
}

/* Note that 'start' and 'width' must NOT contain NAs.
   set_IRanges_slots() trusts the caller and does NOT check this! */
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
	SEXP slot;

	PROTECT(slot = duplicate(_get_IRanges_start(x0)));
	set_IRanges_start(x, slot);
	UNPROTECT(1);

	PROTECT(slot = duplicate(_get_IRanges_width(x0)));
	set_IRanges_width(x, slot);
	UNPROTECT(1);

	PROTECT(slot = duplicate(_get_IRanges_names(x0)));
	set_IRanges_names(x, slot);
	UNPROTECT(1);
	return;
}


/****************************************************************************
 * C-level constructors.
 */

/* Be careful that this constructor does NOT duplicate its arguments before
   putting them in the slots of the returned object.
   So don't try to make it a .Call entry point! */
SEXP _new_IRanges(const char *classname, SEXP start, SEXP width, SEXP names)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_IRanges_slots(ans, start, width, names);
	UNPROTECT(2);
	return ans;
}

SEXP _new_IRanges_from_IntPairAE(const char *classname,
				 const IntPairAE *intpair_ae)
{
	SEXP ans, start, width;

	PROTECT(start = new_INTEGER_from_IntAE(intpair_ae->a));
	PROTECT(width = new_INTEGER_from_IntAE(intpair_ae->b));
	PROTECT(ans = _new_IRanges(classname, start, width, R_NilValue));
	UNPROTECT(3);
	return ans;
}

/* TODO: Try to make this faster by making only 1 call to _new_IRanges() (or
   _alloc_IRanges()) and cloning and modifying this initial object inside the
   for loop. */
SEXP _new_list_of_IRanges_from_IntPairAEAE(const char *element_type,
		const IntPairAEAE *intpair_aeae)
{
	SEXP ans, ans_elt;
	int nelt, i;
	const IntPairAE *ae;

	nelt = IntPairAEAE_get_nelt(intpair_aeae);
	PROTECT(ans = NEW_LIST(nelt));
	for (i = 0; i < nelt; i++) {
		ae = intpair_aeae->elts[i];
		PROTECT(ans_elt = _new_IRanges_from_IntPairAE(element_type,
							      ae));
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

/* Allocation WITHOUT initialization.
   The 'start' and 'width' slots are not initialized (they contain junk). */
SEXP _alloc_IRanges(const char *classname, int length)
{
	SEXP start, width, ans;

	PROTECT(start = NEW_INTEGER(length));
	PROTECT(width = NEW_INTEGER(length));
	PROTECT(ans = _new_IRanges(classname, start, width, R_NilValue));
	UNPROTECT(3);
	return ans;
}


/****************************************************************************
 * Validity functions.
 */

int _is_normal_IRanges_holder(const IRanges_holder *x_holder)
{
	int x_len, i;

	x_len = _get_length_from_IRanges_holder(x_holder);
	if (x_len == 0)
		return 1;
	if (_get_width_elt_from_IRanges_holder(x_holder, 0) <= 0)
		return 0;
	for (i = 1; i < x_len; i++) {
		if (_get_width_elt_from_IRanges_holder(x_holder, i) <= 0)
			return 0;
		if (_get_start_elt_from_IRanges_holder(x_holder, i)
		 <= _get_end_elt_from_IRanges_holder(x_holder, i - 1) + 1)
			return 0;
	}
	return 1;
}

/* --- .Call ENTRY POINT --- */
SEXP IRanges_isNormal(SEXP x)
{
	IRanges_holder ir_holder;

	ir_holder = _hold_IRanges(x);
	return ScalarLogical(_is_normal_IRanges_holder(&ir_holder));
}


/****************************************************************************
 * Coercion functions.
 */

/* --- .Call ENTRY POINT --- */
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

/* --- .Call ENTRY POINT --- */
SEXP NormalIRanges_from_logical(SEXP x)
{
	SEXP ans, ans_start, ans_width;
	int i, x_length, buf_length, ans_length;
	int *start_buf, *width_buf;
	int *x_elt, *start_elt, *width_elt, prev_elt;

	x_length = LENGTH(x);
	if (x_length == 0) {
		PROTECT(ans_start = NEW_INTEGER(0));
		PROTECT(ans_width = NEW_INTEGER(0));
	} else {
		buf_length = x_length / 2 + 1;
		ans_length = 0;
		start_buf = (int *) R_alloc((long) buf_length, sizeof(int));
		width_buf = (int *) R_alloc((long) buf_length, sizeof(int));
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

