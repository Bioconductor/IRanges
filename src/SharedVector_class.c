/****************************************************************************
 *   Low-level manipulation of SharedVector and SharedVector_Pool objects   *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"

static int debug = 0;

SEXP debug_SharedVector_class()
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
 * Some .Call entry points for manipulation externalptr objects.
 */

/*
 * From R:
 *   .Call("externalptr_tagtype", new("externalptr"), PACKAGE="IRanges")
 */
SEXP externalptr_tagtype(SEXP x)
{
	return ScalarString(type2str(TYPEOF(R_ExternalPtrTag(x))));
}

SEXP externalptr_taglength(SEXP x)
{
	return ScalarInteger(LENGTH(R_ExternalPtrTag(x)));
}

/*
 * Print some info about an externalptr object.
 * From R:
 *   .Call("externalptr_show", new("externalptr"), PACKAGE="IRanges")
 */
SEXP externalptr_show(SEXP x)
{
	void *addr;
	SEXP s;

	Rprintf("Object of class 'externalptr':\n");
	Rprintf("  x adress: %p\n", x);
	addr = R_ExternalPtrAddr(x);
	Rprintf("  R_ExternalPtrAddr(x): %p\n", addr);
	s = R_ExternalPtrTag(x);
	Rprintf("  R_ExternalPtrTag(x): %p\n", s);
	Rprintf("  typeof(R_ExternalPtrTag(x)): %s\n",
				CHAR(type2str(TYPEOF(s))));
	s = R_ExternalPtrProtected(x);
	Rprintf("  R_ExternalPtrProtected(x): %p\n", s);
	Rprintf("  typeof(R_ExternalPtrProtected(x)): %s\n",
				CHAR(type2str(TYPEOF(s))));
	return R_NilValue;
}

/*
 * new("externalptr") will always return the same instance of an external
 * pointer object! If you need a new instance, use this function instead.
 * From R:
 *   x <- .Call("externalptr_new", PACKAGE="IRanges")
 */
SEXP externalptr_new()
{
	return R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
}


/****************************************************************************
 * C-level getters for SharedVector objects.
 *
 * Be careful that these functions do NOT duplicate the returned SEXP.
 * Thus they cannot be made .Call entry points!
 */

static SEXP
	xp_symbol = NULL,
	link_symbol = NULL;

static SEXP get_SharedVector_xp(SEXP x)
{
	INIT_STATIC_SYMBOL(xp)
	return GET_SLOT(x, xp_symbol);
}

SEXP _get_SharedVector_tag(SEXP x)
{
	return R_ExternalPtrTag(get_SharedVector_xp(x));
}

/* Not a strict "slot getter" but very much like. */
int _get_SharedVector_length(SEXP x)
{
	return LENGTH(_get_SharedVector_tag(x));
}

static SEXP get_SharedVector_link(SEXP x)
{
	if (link_symbol == NULL)
		link_symbol = install(".link_to_cached_object");
	return GET_SLOT(x, link_symbol);
}


/****************************************************************************
 * C-level setters for SharedVector objects.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_SharedVector_tag(SEXP x, SEXP value)
{
	SEXP xp;

	PROTECT(xp = R_MakeExternalPtr(NULL, value, R_NilValue));
	INIT_STATIC_SYMBOL(xp)
	SET_SLOT(x, xp_symbol, xp);
	UNPROTECT(1);
	return;
}


/****************************************************************************
 * C-level constructors for SharedVector objects.
 *
 * Be careful that these functions do NOT duplicate their arguments before
 * putting them in the slots of the returned object.
 * Thus they cannot be made .Call entry points!
 */

/* 'classname' can be "SharedRaw", "SharedInteger" or "SharedDouble" */
SEXP _new_SharedVector(const char *classname, SEXP tag)
{
	SEXP classdef, ans;

	PROTECT(classdef = MAKE_CLASS(classname));
	PROTECT(ans = NEW_OBJECT(classdef));
	set_SharedVector_tag(ans, tag);
	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 * Some .Call entry points for copying data from a SharedVector object to a
 * SharedVector object of the same subtype.
 */

SEXP SharedVector_memcmp(SEXP x1, SEXP start1, SEXP x2, SEXP start2, SEXP width)
{
	SEXP tag1, tag2, ans;
	int offset1, offset2, nelt;

	tag1 = _get_SharedVector_tag(x1);
	offset1 = INTEGER(start1)[0] - 1;
	tag2 = _get_SharedVector_tag(x2);
	offset2 = INTEGER(start2)[0] - 1;
	nelt = INTEGER(width)[0];

	PROTECT(ans = NEW_INTEGER(1));
	INTEGER(ans)[0] = _vector_memcmp(tag1, offset1, tag2, offset2, nelt);
	UNPROTECT(1);
	return ans;
}

SEXP SharedVector_memcpy(SEXP out, SEXP out_start, SEXP in, SEXP in_start,
		SEXP width)
{
	SEXP out_tag, in_tag;
	int out_offset, in_offset, nelt;

	out_tag = _get_SharedVector_tag(out);
	out_offset = INTEGER(out_start)[0] - 1;
	in_tag = _get_SharedVector_tag(in);
	in_offset = INTEGER(in_start)[0] - 1;
	nelt = INTEGER(width)[0];
	_vector_memcpy(out_tag, out_offset, in_tag, in_offset, nelt);
	return out;
}

SEXP SharedVector_Ocopy_from_start(SEXP out, SEXP in, SEXP in_start, SEXP width,
		SEXP lkup, SEXP reverse)
{
	SEXP out_tag, in_tag;
	int in_offset, nelt, reverse0;

	out_tag = _get_SharedVector_tag(out);
	in_tag = _get_SharedVector_tag(in);
	in_offset = INTEGER(in_start)[0] - 1;
	nelt = INTEGER(width)[0];
	reverse0 = LOGICAL(reverse)[0];
	_vector_Ocopy_from_offset(out_tag, in_tag, in_offset, nelt,
				 lkup, reverse0);
	return out;
}

SEXP SharedVector_Ocopy_from_subscript(SEXP out, SEXP in, SEXP subscript,
		SEXP lkup)
{
	SEXP out_tag, in_tag;

	out_tag = _get_SharedVector_tag(out);
	in_tag = _get_SharedVector_tag(in);
	_vector_Ocopy_from_subscript(out_tag, in_tag, subscript, lkup);
	return out;
}


/****************************************************************************
 * C-level getters for SharedVector_Pool objects.
 *
 * Be careful that this function does NOT duplicate the returned slot.
 * Thus it cannot be made a .Call entry point!
 */

static SEXP
	xp_list_symbol = NULL,
	link_list_symbol = NULL;

SEXP _get_SharedVector_Pool_xp_list(SEXP x)
{
	INIT_STATIC_SYMBOL(xp_list)
	return GET_SLOT(x, xp_list_symbol);
}


/****************************************************************************
 * C-level setters for SharedVector_Pool objects.
 *
 * Be careful that these functions do NOT duplicate the assigned value!
 */

static void set_SharedVector_Pool_xp_list(SEXP x, SEXP value)
{
	INIT_STATIC_SYMBOL(xp_list)
	SET_SLOT(x, xp_list_symbol, value);
	return;
}

static void set_SharedVector_Pool_link_list(SEXP x, SEXP value)
{
	if (link_list_symbol == NULL)
		link_list_symbol = install(".link_to_cached_object_list");
	SET_SLOT(x, link_list_symbol, value);
	return;
}


/****************************************************************************
 * C-level constructors for SharedVector_Pool objects.
 *
 * Be careful that these functions do NOT duplicate their arguments before
 * putting them in the slots of the returned object.
 * Thus they cannot be made .Call entry points!
 */

SEXP _new_SharedVector_Pool1(SEXP shared)
{
	const char *shared_classname;
	char classname_buf[80];
	SEXP classdef, ans, ans_xp_list, ans_link_list, tmp;

	shared_classname = _get_classname(shared);
	if (snprintf(classname_buf, sizeof(classname_buf),
		     "%s_Pool", shared_classname) >= sizeof(classname_buf))
		error("IRanges internal error in _new_SharedVector_Pool1(): "
		      "'shared_classname' too long");

	PROTECT(classdef = MAKE_CLASS(classname_buf));
	PROTECT(ans = NEW_OBJECT(classdef));

	/* set "xp_list" slot */
	PROTECT(ans_xp_list = NEW_LIST(1));
	PROTECT(tmp = duplicate(get_SharedVector_xp(shared)));
	SET_VECTOR_ELT(ans_xp_list, 0, tmp);
	set_SharedVector_Pool_xp_list(ans, ans_xp_list);
	UNPROTECT(2);

	/* set ".link_to_cached_object_list" slot */
	PROTECT(ans_link_list = NEW_LIST(1));
	PROTECT(tmp = duplicate(get_SharedVector_link(shared)));
	SET_VECTOR_ELT(ans_link_list, 0, tmp);
	set_SharedVector_Pool_link_list(ans, ans_link_list);
	UNPROTECT(2);

	UNPROTECT(2);
	return ans;
}

