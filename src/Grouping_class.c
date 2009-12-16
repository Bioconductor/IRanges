/****************************************************************************
 *                Low-level manipulation of Grouping objects                *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_Grouping_class()
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
 * Thus they cannot be made .Call entry points!
 */

static SEXP
	high2low_symbol = NULL,
	low2high_symbol = NULL,
	end_symbol = NULL,
	NAMES_symbol = NULL;

SEXP _get_H2LGrouping_high2low(SEXP x)
{
	INIT_STATIC_SYMBOL(high2low)
	return GET_SLOT(x, high2low_symbol);
}

SEXP _get_H2LGrouping_low2high(SEXP x)
{
	INIT_STATIC_SYMBOL(low2high)
	return GET_SLOT(x, low2high_symbol);
}

SEXP _get_Partitioning_names(SEXP x)
{
	INIT_STATIC_SYMBOL(NAMES)
	return GET_SLOT(x, NAMES_symbol);
}

SEXP _get_PartitioningByEnd_end(SEXP x)
{
	INIT_STATIC_SYMBOL(end)
	return GET_SLOT(x, end_symbol);
}


/****************************************************************************
 * C-level slot setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_Partitioning_names(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(NAMES)
	SET_SLOT(x, NAMES_symbol, value);
	return;
}

static void set_PartitioningByEnd_end(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(end)
	SET_SLOT(x, end_symbol, value);
	return;
}


/****************************************************************************
 * C-level constructor.
 */

/* Be careful that this constructor does NOT duplicate its arguments before
   putting them in the slots of the returned object.
   So don't try to make it a .Call entry point! */
SEXP _new_PartitioningByEnd(const char *classname, SEXP end, SEXP names)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_PartitioningByEnd_end(ans, end);
	if (names == NULL)
		names = R_NilValue;
	set_Partitioning_names(ans, names);
	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 *                        --- .Call ENTRY POINTS ---                        *
 ****************************************************************************/

SEXP H2LGrouping_members(SEXP x, SEXP group_ids)
{
	SEXP ans, x_high2low, x_low2high, x_low2high_elt;
	int x_length, nids, ans_length, i, j, group_id, *ans_elt;

	if (TYPEOF(group_ids) != INTSXP)
		error("the group ids must be integers");
	x_high2low = _get_H2LGrouping_high2low(x);
	x_low2high = _get_H2LGrouping_low2high(x);
	x_length = LENGTH(x_low2high);  /* same as LENGTH(x_high2low) */
	nids = LENGTH(group_ids);

	/* 1st pass: determine 'ans_length' */
	ans_length = 0;
	for (j = 0; j < nids; j++) {
		group_id = INTEGER(group_ids)[j];
		if (group_id == NA_INTEGER)
			error("some group ids are NAs");
		i = group_id - 1;
		if (i < 0 || i >= x_length)
			error("subscript out of bounds");
		if (INTEGER(x_high2low)[i] != NA_INTEGER)
			continue;
		ans_length++;
		x_low2high_elt = VECTOR_ELT(x_low2high, i);
		if (x_low2high_elt == R_NilValue)
			continue;
		ans_length += LENGTH(x_low2high_elt);
	}
        PROTECT(ans = NEW_INTEGER(ans_length));

        /* 2nd pass: fill 'ans' */
	ans_elt = INTEGER(ans);
	for (j = 0; j < nids; j++)
	{
		group_id = INTEGER(group_ids)[j];
		i = group_id - 1;
		if (INTEGER(x_high2low)[i] != NA_INTEGER)
			continue;
		*(ans_elt++) = i + 1;
		x_low2high_elt = VECTOR_ELT(x_low2high, i);
		if (x_low2high_elt == R_NilValue)
			continue;
		memcpy(ans_elt, INTEGER(x_low2high_elt),
			sizeof(int) * LENGTH(x_low2high_elt));
		ans_elt += LENGTH(x_low2high_elt);
	}

	_sort_int_array(INTEGER(ans), ans_length, 0);
	UNPROTECT(1);
	return ans;
}

SEXP H2LGrouping_vmembers(SEXP x, SEXP group_ids_list)
{
	SEXP ans, group_ids;
	int ans_length, i;

	ans_length = LENGTH(group_ids_list);
	PROTECT(ans = NEW_LIST(ans_length));
	for (i = 0; i < ans_length; i++) {
		group_ids = VECTOR_ELT(group_ids_list, i);
		if (TYPEOF(group_ids) != INTSXP)
			error("'L' must be a list of integer vectors");
		SET_VECTOR_ELT(ans, i, H2LGrouping_members(x, group_ids));
	}
	UNPROTECT(1);
	return ans;
}

