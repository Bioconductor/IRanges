/****************************************************************************
 *             Low-level manipulation of CompressedList objects             *
 ****************************************************************************/

#include "IRanges.h"


/****************************************************************************
 * C-level slot getters.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call entry points!
 */

static SEXP
	unlistData_symbol = NULL,
	partitioning_symbol = NULL;

SEXP _get_CompressedList_unlistData(SEXP x)
{
	INIT_STATIC_SYMBOL(unlistData)
	return GET_SLOT(x, unlistData_symbol);
}

SEXP _get_CompressedList_partitioning(SEXP x)
{
	INIT_STATIC_SYMBOL(partitioning)
	return GET_SLOT(x, partitioning_symbol);
}

/* Not strict "slot getters" but very much like. */

int _get_CompressedList_length(SEXP x)
{
	return LENGTH(_get_PartitioningByEnd_end(
			_get_CompressedList_partitioning(x)));
}

SEXP _get_CompressedList_names(SEXP x)
{
	return _get_Partitioning_names(
			_get_CompressedList_partitioning(x));
}


/****************************************************************************
 * C-level slot setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_CompressedList_unlistData(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(unlistData)
	SET_SLOT(x, unlistData_symbol, value);
	return;
}

static void set_CompressedList_partitioning(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(partitioning)
	SET_SLOT(x, partitioning_symbol, value);
	return;
}


/****************************************************************************
 * C-level constructor.
 */

/* Be careful that this constructor does NOT duplicate its arguments before
   putting them in the slots of the returned object.
   So don't try to make it a .Call entry point! */
SEXP _new_CompressedList(const char *classname,
		SEXP unlistData, SEXP partitioning)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_CompressedList_unlistData(ans, unlistData);
	set_CompressedList_partitioning(ans, partitioning);
	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 * C-level abstract getters for CompressedIntegerList objects.
 */

CompressedIntsList_holder _hold_CompressedIntegerList(SEXP x)
{
	SEXP partitioning_end;
	CompressedIntsList_holder x_holder;

	partitioning_end = _get_PartitioningByEnd_end(
				_get_CompressedList_partitioning(x));
	x_holder.length = LENGTH(partitioning_end);
	x_holder.breakpoints = INTEGER(partitioning_end);
	x_holder.unlisted = INTEGER(_get_CompressedList_unlistData(x));
	return x_holder;
}

int _get_length_from_CompressedIntsList_holder(
		const CompressedIntsList_holder *x_holder)
{
	return x_holder->length;
}

Ints_holder _get_elt_from_CompressedIntsList_holder(
		const CompressedIntsList_holder *x_holder,
		int i)
{
	Ints_holder x_elt_holder;
	int offset;

	if (i == 0) {
		offset = 0;
	} else {
		offset = x_holder->breakpoints[i - 1];
	}
	x_elt_holder.ptr = x_holder->unlisted + offset;
	x_elt_holder.length = x_holder->breakpoints[i] - offset;
        return x_elt_holder;
}

