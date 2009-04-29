#include "IRanges.h"

/*
 * --- .Call ENTRY POINT ---
 */

SEXP vector_subseq(SEXP x, SEXP start, SEXP width)
{
  int i, j, ans_width = 0, ans_off = 0;
  SEXP ans, names;
  
  if (!IS_INTEGER(start))
    error("'start' must be an integer vector");
  if (!IS_INTEGER(width))
    error("'width' must be an integer vector");
  if (LENGTH(start) != LENGTH(width))
    error("length of 'start' must equal 'length' of width");
  
  for (i = 0; i < LENGTH(start); i++) {
    int s = INTEGER(start)[i];
    int w = INTEGER(width)[i];
    if (s == NA_INTEGER || s < 1)
      error("each element in 'start' must be a positive integer");
    if (w == NA_INTEGER || w < 0)
      error("each element in 'width' must be a non-negative integer");
    ans_width += w;
    if (LENGTH(x) < s + w - 1)
      error("subseq exceeds bounds of 'x'");
  }
        
  PROTECT(ans = allocVector(TYPEOF(x), ans_width));
  
  for (i = 0; i < LENGTH(start); i++) {
    int s = INTEGER(start)[i] - 1;
    int w = INTEGER(width)[i];
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
      memcpy(INTEGER(ans) + ans_off, INTEGER(x) + s, w * sizeof(int));
      break;
    case REALSXP:
      memcpy(REAL(ans) + ans_off, REAL(x) + s, w * sizeof(double));
      break;
    case CPLXSXP:
      memcpy(COMPLEX(ans) + ans_off, COMPLEX(x) + s, w * sizeof(Rcomplex));
      /*
    	for (i = 0, j = s; i < w; i++, j++) {
        COMPLEX(ans)[i].r = COMPLEX(x)[j].r;
        COMPLEX(ans)[i].i = COMPLEX(x)[j].i;
    	}
      */
      break;
    case STRSXP:
      for (i = ans_off, j = s; i < w + ans_off; i++, j++)
        SET_STRING_ELT(ans, i, STRING_ELT(x, j));
      break;
    case VECSXP:
      for (i = ans_off, j = s; i < w + ans_off; i++, j++)
        SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, j));
      break;
    case RAWSXP:
      memcpy(RAW(ans) + ans_off, RAW(x) + s, w * sizeof(char));
      break;
    default:
      error("unrecognized vector type");
    }
    ans_off += w;
  }
  names = getAttrib(x, R_NamesSymbol);
  if (names != R_NilValue) {
    setAttrib(ans, R_NamesSymbol, vector_subseq(names, start, width));
  }
  
  UNPROTECT(1);
  
  return ans;
}
