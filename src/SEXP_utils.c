#include "IRanges.h"


const char *_get_classname(SEXP x)
{
	return CHAR(STRING_ELT(GET_CLASS(x), 0));
}

static int get_NROW(SEXP x)
{
	SEXP x_dim, x_rownames;

	if (x == R_NilValue)
		return 0;
	if (!IS_VECTOR(x))
		error("get_NROW() defined only on a vector (or NULL)");
	/* A data.frame doesn't have a "dim" attribute but the dimensions can
	   be inferred from the "names" and "row.names" attributes. */
	x_rownames = getAttrib(x, R_RowNamesSymbol);
	if (x_rownames != R_NilValue)
		return LENGTH(x_rownames);
	x_dim = GET_DIM(x);
	if (x_dim == R_NilValue || LENGTH(x_dim) == 0)
		return LENGTH(x);
	return INTEGER(x_dim)[0];
}

/*
 * --- .Call ENTRY POINT ---
 * A C implementation of 'sapply(x, NROW)' that works only on a list of
 * vectors (or NULLs).
 */
SEXP sapply_NROW(SEXP x)
{
	SEXP ans, x_elt;
	int x_len, i, *ans_elt;

	x_len = LENGTH(x);
	PROTECT(ans = NEW_INTEGER(x_len));
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
		x_elt = VECTOR_ELT(x, i);
		if (x_elt != R_NilValue && !IS_VECTOR(x_elt)) {
			UNPROTECT(1);
			error("element %d not a vector (or NULL)", i + 1);
		}
		*ans_elt = get_NROW(x_elt);
	}
	UNPROTECT(1);
	return ans;
}

static SEXP _top_prenv(SEXP promise, SEXP env) {
  while(TYPEOF(promise) == PROMSXP) {
    env = PRENV(promise);
    promise = PREXPR(promise);
  }
  return env;
}

/*
 * --- .Call ENTRY POINT ---
 * Gets the top environment associated with a (nested) promise.
 */
SEXP top_prenv(SEXP nm, SEXP env)
{
  SEXP promise = findVar(nm, env);
  return _top_prenv(promise, env);
}

/*
 * --- .Call ENTRY POINT ---
 * Gets the top environment associated with each promise in '...'
 */
SEXP top_prenv_dots(SEXP env)
{
  SEXP dots = findVar(R_DotsSymbol, env);
  SEXP ans = allocVector(VECSXP, length(dots));
  int i = 0;
  for (SEXP p = dots; p != R_NilValue; p = CDR(p)) {
    SET_VECTOR_ELT(ans, i++, _top_prenv(CAR(p), env));
  }
  return ans;
}
