/****************************************************************************
 *            Low-level manipulation of Vector and List objects             *
 *          Authors: Patrick Aboyoun, Michael Lawrence, Herve Pages         *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"


/****************************************************************************
 * C-level slot getters.
 */

static SEXP elementType_symbol = NULL;

const char *_get_List_elementType(SEXP x)
{
	INIT_STATIC_SYMBOL(elementType)
	return CHAR(STRING_ELT(GET_SLOT(x, elementType_symbol), 0));
}


/****************************************************************************
 * C-level slot setters.
 */

void _set_List_elementType(SEXP x, const char *type)
{
	SEXP value;

	INIT_STATIC_SYMBOL(elementType)
	PROTECT(value = mkString(type));
	SET_SLOT(x, elementType_symbol, value);
	UNPROTECT(1);
	return;
}

