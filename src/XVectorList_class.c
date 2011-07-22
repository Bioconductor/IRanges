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
	cached_x.element_type = _get_List_elementType(x);
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
 *
 * Please be aware that these functions do NOT duplicate their arguments
 * before putting them in the slots of the returned object.
 * Thus they cannot be made .Call entry points!
 */

/* Constructing an XVectorList object from a list of tags.  */

static SEXP new_XVectorList_from_tags(const char *classname,
		const char *element_type,
		SEXP (*new_SharedVector_Pool)(SEXP),
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	SEXP classdef, ans, ans_pool, ans_ranges;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));

	/* set "elementType" slot */
	_set_List_elementType(ans, element_type);

	/* set "pool" slot */
	PROTECT(ans_pool = new_SharedVector_Pool(tags));
	set_XVectorList_pool(ans, ans_pool);
	UNPROTECT(1);

	/* set "ranges" slot */
	PROTECT(ans_ranges = new_GroupedIRanges(ranges, ranges_group));
	set_XVectorList_ranges(ans, ans_ranges);
	UNPROTECT(1);

	UNPROTECT(2);
	return ans;
}

SEXP _new_XRawList_from_tags(const char *classname,
		const char *element_type,
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	return new_XVectorList_from_tags(classname, element_type,
					 _new_SharedRaw_Pool,
					 tags, ranges, ranges_group);
}

SEXP _new_XIntegerList_from_tags(const char *classname,
		const char *element_type,
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	return new_XVectorList_from_tags(classname, element_type,
					 _new_SharedInteger_Pool,
					 tags, ranges, ranges_group);
}

SEXP _new_XDoubleList_from_tags(const char *classname,
		const char *element_type,
		SEXP tags, SEXP ranges, SEXP ranges_group)
{
	return new_XVectorList_from_tags(classname, element_type,
					 _new_SharedDouble_Pool,
					 tags, ranges, ranges_group);
}

/*
 * Constructing an XVectorList object from a single tag.
 * For convenience, 'ranges' can be NULL as a way to indicate that the
 * returned XVectorList object has only 1 element that spans the entire tag.
 */

static SEXP new_XVectorList_from_tag(const char *classname,
		const char *element_type,
		SEXP (*new_SharedVector_Pool)(SEXP),
		SEXP tag, SEXP ranges)
{
	SEXP tags, ans_start, ans_width, ranges_group, ans;
	int nprotect = 0, ans_length, i;

	/* prepare 'tags' */
	PROTECT(tags = NEW_LIST(1));
	nprotect++;
	SET_VECTOR_ELT(tags, 0, tag);

	/* prepare 'ranges' */
	if (ranges == NULL) {
		PROTECT(ans_start = ScalarInteger(1));
		PROTECT(ans_width = ScalarInteger(LENGTH(tag)));
		PROTECT(ranges = _new_IRanges("IRanges",
					ans_start, ans_width, R_NilValue));
		nprotect += 3;
	}

	/* prepare 'ranges_group' */
	ans_length = _get_IRanges_length(ranges);
	PROTECT(ranges_group = NEW_INTEGER(ans_length));
	nprotect++;
	for (i = 0; i < ans_length; i++)
		INTEGER(ranges_group)[i] = 1;

	/* make the XVectorList object */
	PROTECT(ans = new_XVectorList_from_tags(classname, element_type,
				new_SharedVector_Pool,
				tags, ranges, ranges_group));
	nprotect++;
	UNPROTECT(nprotect);
	return ans;
}

SEXP _new_XRawList_from_tag(const char *classname,
		const char *element_type,
		SEXP tag, SEXP ranges)
{
	return new_XVectorList_from_tag(classname, element_type,
					_new_SharedRaw_Pool,
					tag, ranges);
}

SEXP _new_XIntegerList_from_tag(const char *classname,
		const char *element_type,
		SEXP tag, SEXP ranges)
{
	return new_XVectorList_from_tag(classname, element_type,
					_new_SharedInteger_Pool,
					tag, ranges);
}

SEXP _new_XDoubleList_from_tag(const char *classname,
		const char *element_type,
		SEXP tag, SEXP ranges)
{
	return new_XVectorList_from_tag(classname, element_type,
					_new_SharedDouble_Pool,
					tag, ranges);
}

/* Allocation WITHOUT initialization.
 * This is a soft limit. Some tags could be longer than this limit if the
 * XVectorList object to allocate contains elements that are also longer
 * than this limit. */
#define	MAX_TAG_LENGTH 1073741824

static SEXP alloc_XVectorList(const char *classname,
		const char *element_type, const char *tag_type,
		SEXP width)
{
	int ans_length, tag_length, new_tag_length, i, nelt;
	SEXP start, group, ranges, tags, tag, ans;
	IntAE tag_lengths;

	ans_length = LENGTH(width);
	PROTECT(start = NEW_INTEGER(ans_length));
	PROTECT(group = NEW_INTEGER(ans_length));
	tag_lengths = _new_IntAE(0, 0, 0);
	if (ans_length != 0) {
		tag_length = 0;
		for (i = 0; i < ans_length; i++) {
			new_tag_length = tag_length + INTEGER(width)[i];
			if (new_tag_length > MAX_TAG_LENGTH
			 || new_tag_length < tag_length) {
				_IntAE_insert_at(&tag_lengths,
					_IntAE_get_nelt(&tag_lengths),
					tag_length);
				tag_length = 0;
			}
			INTEGER(start)[i] = tag_length + 1;
			INTEGER(group)[i] = _IntAE_get_nelt(&tag_lengths) + 1;
			tag_length += INTEGER(width)[i];
		}
		_IntAE_insert_at(&tag_lengths,
			_IntAE_get_nelt(&tag_lengths), tag_length);
	}
	PROTECT(ranges = _new_IRanges("IRanges", start, width, NULL));
	nelt = _IntAE_get_nelt(&tag_lengths);
	PROTECT(tags = NEW_LIST(nelt));
	if (strcmp(tag_type, "raw") == 0) {
		for (i = 0; i < nelt; i++) {
			PROTECT(tag = NEW_RAW(tag_lengths.elts[i]));
			SET_VECTOR_ELT(tags, i, tag);
			UNPROTECT(1);
		}
		PROTECT(ans = _new_XRawList_from_tags(classname,
					element_type, tags, ranges, group));
	} else if (strcmp(tag_type, "integer") == 0) {
		for (i = 0; i < nelt; i++) {
			PROTECT(tag = NEW_INTEGER(tag_lengths.elts[i]));
			SET_VECTOR_ELT(tags, i, tag);
			UNPROTECT(1);
		}
		PROTECT(ans = _new_XIntegerList_from_tags(classname,
					element_type, tags, ranges, group));
	} else if (strcmp(tag_type, "double") == 0) {
		for (i = 0; i < nelt; i++) {
			PROTECT(tag = NEW_NUMERIC(tag_lengths.elts[i]));
			SET_VECTOR_ELT(tags, i, tag);
			UNPROTECT(1);
		}
		PROTECT(ans = _new_XDoubleList_from_tags(classname,
					element_type, tags, ranges, group));
	} else {
		UNPROTECT(4);
		error("IRanges internal error in alloc_XVectorList(): "
		      "%s: invalid 'tag_type'", tag_type);
	}
	UNPROTECT(5);
	return ans;
}

SEXP _alloc_XRawList(const char *classname, const char *element_type,
		SEXP width)
{
	return alloc_XVectorList(classname, element_type, "raw", width);
}

SEXP _alloc_XIntegerList(const char *classname, const char *element_type,
		SEXP width)
{
	return alloc_XVectorList(classname, element_type, "integer", width);
}

SEXP _alloc_XDoubleList(const char *classname, const char *element_type,
		SEXP width)
{
	return alloc_XVectorList(classname, element_type, "double", width);
}

/* More constructors. */

SEXP _new_XRawList_from_CharAEAE(const char *classname,
		const char *element_type,
		const CharAEAE *char_aeae, SEXP lkup)
{
	const int *lkup0;
	int lkup_length, nelt, i;
	SEXP ans_width, ans;
	const CharAE *src;
	cachedXVectorList cached_ans;
	cachedCharSeq dest;

	if (lkup == R_NilValue) {
		lkup0 = NULL;
	} else {
		lkup0 = INTEGER(lkup);
		lkup_length = LENGTH(lkup);
	}
	nelt = _CharAEAE_get_nelt(char_aeae);
	PROTECT(ans_width = NEW_INTEGER(nelt));
	for (i = 0; i < nelt; i++) {
		src = char_aeae->elts + i;
		INTEGER(ans_width)[i] = _CharAE_get_nelt(src);
	}
	PROTECT(ans = _alloc_XRawList(classname, element_type, ans_width));
	cached_ans = _cache_XVectorList(ans);
	for (i = 0; i < nelt; i++) {
		src = char_aeae->elts + i;
		dest = _get_cachedXRawList_elt(&cached_ans, i);
		/* dest.seq is a const char * so we need to cast it to
		   char * before we can write to it */
		_Ocopy_bytes_to_i1i2_with_lkup(0, dest.length - 1,
			(char *) dest.seq, dest.length,
			src->elts, _CharAE_get_nelt(src),
			lkup0, lkup_length);
	}
	UNPROTECT(2);
	return ans;
}

SEXP _new_XIntegerList_from_IntAEAE(const char *classname,
		const char *element_type,
		const IntAEAE *int_aeae)
{
	int nelt, i;
	SEXP ans_width, ans;
	const IntAE *src;
	cachedXVectorList cached_ans;
	cachedIntSeq dest;

	nelt = _IntAEAE_get_nelt(int_aeae);
	PROTECT(ans_width = NEW_INTEGER(nelt));
	for (i = 0; i < nelt; i++) {
		src = int_aeae->elts + i;
		INTEGER(ans_width)[i] = _IntAE_get_nelt(src);
	}
	PROTECT(ans = _alloc_XIntegerList(classname, element_type, ans_width));
	cached_ans = _cache_XVectorList(ans);
	for (i = 0; i < nelt; i++) {
		src = int_aeae->elts + i;
		dest = _get_cachedXIntegerList_elt(&cached_ans, i);
		/* dest.seq is a const int * so we need to cast it to
		   char * before we can write to it */
		_Ocopy_byteblocks_to_i1i2(0, dest.length - 1,
			(char *) dest.seq, dest.length,
			(const char *) src->elts, _IntAE_get_nelt(src),
			sizeof(int));
	}
	UNPROTECT(2);
	return ans;
}

