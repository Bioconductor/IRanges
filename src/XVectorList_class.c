/****************************************************************************
 *              Low-level manipulation of XVectorList objects               *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_XVectorList_class()
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
 * C-level slot getters for XVectorList objects.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call entry points!
 */

static SEXP
	pool_symbol = NULL,
	ranges_symbol = NULL;

SEXP _get_XVectorList_pool(SEXP x)
{
	INIT_STATIC_SYMBOL(pool)
	return GET_SLOT(x, pool_symbol);
}

SEXP _get_XVectorList_ranges(SEXP x)
{
	INIT_STATIC_SYMBOL(ranges)
	return GET_SLOT(x, ranges_symbol);
}

/* Not a strict "slot getter" but very much like. */
int _get_XVectorList_length(SEXP x)
{
	return _get_IRanges_length(_get_XVectorList_ranges(x));
}


/****************************************************************************
 * One C-level slot getter for GroupedIRanges objects.
 */

static SEXP group_symbol = NULL;

static SEXP get_GroupedIRanges_group(SEXP x)
{
	INIT_STATIC_SYMBOL(group)
	return GET_SLOT(x, group_symbol);
}


/****************************************************************************
 * C-level abstract getters.
 */

cachedXVectorList _cache_XVectorList(SEXP x)
{
	cachedXVectorList cached_x;
	SEXP ranges;

	cached_x.classname = _get_classname(x);
	cached_x.element_type = _get_Sequence_elementType(x);
	cached_x.xp_list = _get_SharedVector_Pool_xp_list(
				_get_XVectorList_pool(x));
	ranges = _get_XVectorList_ranges(x);
	cached_x.length = _get_IRanges_length(ranges);
	cached_x.start = INTEGER(_get_IRanges_start(ranges));
	cached_x.width = INTEGER(_get_IRanges_width(ranges));
	cached_x.group = INTEGER(get_GroupedIRanges_group(ranges));
	return cached_x;
}

int _get_cachedXVectorList_length(const cachedXVectorList *cached_x)
{
	return cached_x->length;
}

cachedCharSeq _get_cachedXRawList_elt(const cachedXVectorList *cached_x,
		int i)
{
	SEXP tag;
	cachedCharSeq charseq;

	tag = R_ExternalPtrTag(VECTOR_ELT(cached_x->xp_list,
					  cached_x->group[i] - 1));
	charseq.seq = (const char *) RAW(tag) + cached_x->start[i] - 1;
	charseq.length = cached_x->width[i];
        return charseq;
}

cachedIntSeq _get_cachedXIntegerList_elt(const cachedXVectorList *cached_x,
		int i)
{
	SEXP tag;
	cachedIntSeq intseq;

	tag = R_ExternalPtrTag(VECTOR_ELT(cached_x->xp_list,
					  cached_x->group[i] - 1));
	intseq.seq = INTEGER(tag) + cached_x->start[i] - 1;
	intseq.length = cached_x->width[i];
        return intseq;
}

cachedDoubleSeq _get_cachedXDoubleList_elt(const cachedXVectorList *cached_x,
		int i)
{
	SEXP tag;
	cachedDoubleSeq doubleseq;

	tag = R_ExternalPtrTag(VECTOR_ELT(cached_x->xp_list,
					  cached_x->group[i] - 1));
	doubleseq.seq = REAL(tag) + cached_x->start[i] - 1;
	doubleseq.length = cached_x->width[i];
        return doubleseq;
}

