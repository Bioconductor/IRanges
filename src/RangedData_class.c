/****************************************************************************
 *            Low-level manipulation of RangedData objects            
 ****************************************************************************/

#include "IRanges.h"

static SEXP ranges_symbol = NULL, values_symbol = NULL;

static void set_RangedData_ranges(SEXP x, SEXP value) {
  INIT_STATIC_SYMBOL(ranges)
  SET_SLOT(x, ranges_symbol, value);
}

static void set_RangedData_values(SEXP x, SEXP value) {
  INIT_STATIC_SYMBOL(values)
  SET_SLOT(x, values_symbol, value);
}

SEXP _new_RangedData(const char *classname, SEXP ranges, SEXP values)
{
  SEXP rdClass, rd;
  PROTECT(rdClass = MAKE_CLASS(classname));
  PROTECT(rd = NEW_OBJECT(rdClass));
  set_RangedData_ranges(rd, ranges);
  set_RangedData_values(rd, values);
  UNPROTECT(2);
  return rd;
}

