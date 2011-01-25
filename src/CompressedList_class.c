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

