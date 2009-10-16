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

/* Not strict "slot getters" but very much like. */

int _get_XVectorList_length(SEXP x)
{
	return _get_IRanges_length(_get_XVectorList_ranges(x));
}

SEXP _get_XVectorList_width(SEXP x)
{
	return _get_IRanges_width(_get_XVectorList_ranges(x));
}

SEXP _get_XVectorList_names(SEXP x)
{
	return _get_IRanges_names(_get_XVectorList_ranges(x));
}


/****************************************************************************
 * C-level slot getter, slot setter, and constructor for GroupedIRanges
 * objects.
 */

static SEXP group_symbol = NULL;

static SEXP get_GroupedIRanges_group(SEXP x)
{
	INIT_STATIC_SYMBOL(group)
	return GET_SLOT(x, group_symbol);
}

static void set_GroupedIRanges_group(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(group)
	SET_SLOT(x, group_symbol, value);
	return;
}

static SEXP new_GroupedIRanges(SEXP ranges, SEXP group)
{
	SEXP ans;

	PROTECT(ans = _new_IRanges("GroupedIRanges",
				_get_IRanges_start(ranges),
				_get_IRanges_width(ranges),
				_get_IRanges_names(ranges)));
	set_GroupedIRanges_group(ans, group);
	UNPROTECT(1);
	return ans;
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


/****************************************************************************
 * C-level slot setters.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_XVectorList_pool(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(pool)
	SET_SLOT(x, pool_symbol, value);
	return;
}

static void set_XVectorList_ranges(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(ranges)
	SET_SLOT(x, ranges_symbol, value);
	return;
}

/*
 * Not strict a "slot getter" but very much like.
 * WARNING: x@ranges@NAMES is modified in-place!
 */
void _set_XVectorList_names(SEXP x, SEXP names)
{
	_set_IRanges_names(_get_XVectorList_ranges(x), names);
	return;
}


/****************************************************************************
 * C-level constructors.
 */

/* Be careful that this constructor does NOT duplicate its arguments before
   putting them in the slots of the returned object.
   So don't try to make it a .Call entry point! */
SEXP _new_XVectorList1(const char *classname, SEXP xvector, SEXP ranges)
{
	const char *element_type;
	char classname_buf[80];
	SEXP classdef, ans, xvector_shared, ans_pool,
	     shifted_ranges, shifted_ranges_start, ranges_group, ans_ranges;
	int ans_length, offset, i;

	element_type = _get_classname(xvector);
	if (classname == NULL) {
		if (snprintf(classname_buf, sizeof(classname_buf),
			     "%sList", element_type) >= sizeof(classname_buf))
			error("IRanges internal error in _new_XVectorList1(): "
			      "'element_type' too long");
		classname = classname_buf;
	}

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));

	/* set "elementType" slot */
	_set_Sequence_elementType(ans, element_type);

	/* set "pool" slot */
	xvector_shared = _get_XVector_shared(xvector);
	PROTECT(ans_pool = _new_SharedVector_Pool1(xvector_shared));
	set_XVectorList_pool(ans, ans_pool);
	UNPROTECT(1);

	/* set "ranges" slot */
	PROTECT(shifted_ranges = duplicate(ranges));
	shifted_ranges_start = _get_IRanges_start(shifted_ranges);
	ans_length = LENGTH(shifted_ranges_start);
	PROTECT(ranges_group = NEW_INTEGER(ans_length));
	offset = _get_XVector_offset(xvector);
	for (i = 0; i < ans_length; i++) {
		INTEGER(shifted_ranges_start)[i] += offset;
		INTEGER(ranges_group)[i] = 1;
	}
	PROTECT(ans_ranges = new_GroupedIRanges(shifted_ranges, ranges_group));
	set_XVectorList_ranges(ans, ans_ranges);
	UNPROTECT(3);

	UNPROTECT(2);
	return ans;
}

/* Allocation WITHOUT initialization. */

static SEXP alloc_XVectorList(const char *classname, const char *element_type,
		SEXP (*alloc_XVector)(const char *, int), SEXP width)
{
	int ans_length, xvector_length, i;
	SEXP start, xvector, ranges, ans;

	ans_length = LENGTH(width);
	PROTECT(start = NEW_INTEGER(ans_length));
	for (i = xvector_length = 0; i < ans_length; i++) {
		INTEGER(start)[i] = xvector_length + 1;
		xvector_length += INTEGER(width)[i];
	}
	PROTECT(xvector = alloc_XVector(element_type, xvector_length));
	PROTECT(ranges = _new_IRanges("IRanges", start, width, NULL));
	PROTECT(ans = _new_XVectorList1(classname, xvector, ranges));
	UNPROTECT(4);
	return ans;
}

SEXP _alloc_XRawList(const char *classname, const char *element_type, SEXP width)
{
	return alloc_XVectorList(classname, element_type, _alloc_XRaw, width);
}

SEXP _alloc_XIntegerList(const char *classname, const char *element_type, SEXP width)
{
	return alloc_XVectorList(classname, element_type, _alloc_XInteger, width);
}

SEXP _alloc_XDoubleList(const char *classname, const char *element_type, SEXP width)
{
	return alloc_XVectorList(classname, element_type, _alloc_XDouble, width);
}

