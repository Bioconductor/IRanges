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
 * C-level accessor functions for H2LGrouping objects.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call() entry points!
 */

SEXP _get_H2LGrouping_high2low(SEXP x)
{
	return GET_SLOT(x, install("high2low"));
}

SEXP _get_H2LGrouping_low2high(SEXP x)
{
	return GET_SLOT(x, install("low2high"));
}

