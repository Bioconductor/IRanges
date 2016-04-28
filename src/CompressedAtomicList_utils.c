/****************************************************************************
 *             Utilities for CompressedAtomicList objects                   *
 ****************************************************************************/

#include "IRanges.h"

#define R_INT_MIN	(1+INT_MIN)

#define PARTITIONED_SUM(C_TYPE, ACCESSOR, ANS_TYPE, ANS_ACCESSOR, NA_CHECK) { \
    PARTITIONED_AGG(C_TYPE, ACCESSOR, ANS_TYPE, ANS_ACCESSOR, NA_CHECK, 0,    \
                    summary += val,);					\
}

#define PARTITIONED_PROD(ACCESSOR, NA_CHECK) {                            \
        PARTITIONED_AGG(double, ACCESSOR, REALSXP, REAL, NA_CHECK, 1,     \
			summary *= val,);				\
}

#define PARTITIONED_EX(C_TYPE, ACCESSOR, ANS_TYPE, NA_CHECK, INIT, RELOP) { \
    PARTITIONED_AGG(C_TYPE, ACCESSOR, ANS_TYPE, ACCESSOR, NA_CHECK, INIT,   \
                    if (val RELOP summary) summary = val,);          	    \
  }

#define PARTITIONED_MIN(C_TYPE, ACCESSOR, ANS_TYPE, NA_CHECK, INIT) {         \
    PARTITIONED_EX(C_TYPE, ACCESSOR, ANS_TYPE, NA_CHECK, INIT, <);  \
}

#define PARTITIONED_MAX(C_TYPE, ACCESSOR, ANS_TYPE, NA_CHECK, INIT) {	     \
    PARTITIONED_EX(C_TYPE, ACCESSOR, ANS_TYPE, NA_CHECK, INIT, >); \
  }

#define PARTITIONED_WHICH_AGG(C_TYPE, ACCESSOR, NA_CHECK, INIT, RELOP) {  \
    SEXP na_rm = ScalarLogical(TRUE);					  \
    PARTITIONED_AGG(int, ACCESSOR, INTSXP, INTEGER, NA_CHECK, NA_INTEGER, \
                    if (val RELOP summary_val)				  \
		      (summary_val = val, summary = j - prev_end + 1),	  \
		    C_TYPE summary_val = INIT)				  \
  }

#define PARTITIONED_WHICH_MAX(C_TYPE, ACCESSOR, NA_CHECK, INIT) {       \
    PARTITIONED_WHICH_AGG(C_TYPE, ACCESSOR, NA_CHECK, INIT, >)		\
  }

#define PARTITIONED_WHICH_MIN(C_TYPE, ACCESSOR, NA_CHECK, INIT) {       \
    PARTITIONED_WHICH_AGG(C_TYPE, ACCESSOR, NA_CHECK, INIT, <)		\
  }

#define PARTITIONED_AGG(C_TYPE, ACCESSOR, ANS_TYPE, ANS_ACCESSOR, NA_CHECK, \
                        INIT, UPDATE, EXTRA_INIT) {			\
    SEXP unlistData = _get_CompressedList_unlistData(x);                \
    SEXP ends =                                                         \
      _get_PartitioningByEnd_end(_get_CompressedList_partitioning(x));  \
    Rboolean _na_rm = asLogical(na_rm);                                 \
    int prev_end = 0;                                                   \
    SEXP ans = allocVector(ANS_TYPE, length(ends));                     \
    for (int i = 0; i < length(ends); i++) {                            \
      int end = INTEGER(ends)[i];                                       \
      C_TYPE summary = INIT;                                            \
      EXTRA_INIT;							\
      for (int j = prev_end; j < end; j++) {                            \
        C_TYPE val = ACCESSOR(unlistData)[j];                           \
        if (NA_CHECK) {                                                 \
          if (_na_rm) {                                                 \
            continue;                                                   \
          } else {                                                      \
            summary = NA_ ## ANS_ACCESSOR;                              \
            break;                                                      \
          }                                                             \
        }                                                               \
        UPDATE;                                                         \
      }                                                                 \
      ANS_ACCESSOR(ans)[i] = summary;                                   \
      prev_end = end;                                                   \
    }                                                                   \
    setAttrib(ans, R_NamesSymbol, _get_CompressedList_names(x));        \
    return ans;                                                         \
  }


/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedLogicalList_sum(SEXP x, SEXP na_rm)
{
  PARTITIONED_SUM(Rboolean, LOGICAL, INTSXP, INTEGER, val == NA_LOGICAL);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIntegerList_sum(SEXP x, SEXP na_rm)
{
  PARTITIONED_SUM(int, INTEGER, INTSXP, INTEGER, val == NA_INTEGER);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedNumericList_sum(SEXP x, SEXP na_rm)
{
  PARTITIONED_SUM(double, REAL, REALSXP, REAL, ISNA(val));
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedLogicalList_prod(SEXP x, SEXP na_rm)
{
  PARTITIONED_PROD(LOGICAL, val == NA_LOGICAL);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIntegerList_prod(SEXP x, SEXP na_rm)
{
  PARTITIONED_PROD(INTEGER, val == NA_INTEGER);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedNumericList_prod(SEXP x, SEXP na_rm)
{
  PARTITIONED_PROD(REAL, ISNA(val));
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedLogicalList_min(SEXP x, SEXP na_rm)
{
  PARTITIONED_MIN(Rboolean, LOGICAL, LGLSXP, val == NA_LOGICAL, TRUE);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedLogicalList_which_min(SEXP x)
{
  PARTITIONED_WHICH_MIN(Rboolean, LOGICAL, val == NA_LOGICAL, TRUE);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIntegerList_min(SEXP x, SEXP na_rm)
{
  PARTITIONED_MIN(int, INTEGER, INTSXP, val == NA_INTEGER, INT_MAX);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIntegerList_which_min(SEXP x)
{
  PARTITIONED_WHICH_MIN(int, INTEGER, val == NA_INTEGER, INT_MAX);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedNumericList_min(SEXP x, SEXP na_rm)
{
  PARTITIONED_MIN(double, REAL, REALSXP, ISNA(val), R_PosInf);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedNumericList_which_min(SEXP x)
{
  PARTITIONED_WHICH_MIN(double, REAL, ISNA(val), R_PosInf);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedLogicalList_max(SEXP x, SEXP na_rm)
{
  PARTITIONED_MAX(Rboolean, LOGICAL, LGLSXP, val == NA_LOGICAL, TRUE);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedLogicalList_which_max(SEXP x)
{
  PARTITIONED_WHICH_MAX(Rboolean, LOGICAL, val == NA_LOGICAL, TRUE);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIntegerList_max(SEXP x, SEXP na_rm)
{
  PARTITIONED_MAX(int, INTEGER, INTSXP, val == NA_INTEGER, R_INT_MIN);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIntegerList_which_max(SEXP x)
{
  PARTITIONED_WHICH_MAX(int, INTEGER, val == NA_INTEGER, R_INT_MIN);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedNumericList_max(SEXP x, SEXP na_rm)
{
  PARTITIONED_MAX(double, REAL, REALSXP, ISNA(val), R_NegInf);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedNumericList_which_max(SEXP x)
{
  PARTITIONED_WHICH_MAX(double, REAL, ISNA(val), R_NegInf);
}

#define PARTITIONED_IS_UNSORTED(C_TYPE, ACCESSOR, NA_CHECK) {		\
        if (asLogical(strictly)) {                                      \
            PARTITIONED_BREAK(C_TYPE, ACCESSOR, NA_CHECK,               \
                              val <= ACCESSOR(unlistData)[j-1], 1);     \
        } else {                                                        \
            PARTITIONED_BREAK(C_TYPE, ACCESSOR, NA_CHECK,               \
                              val < ACCESSOR(unlistData)[j-1], 1);      \
        }                                                               \
}

#define PARTITIONED_BREAK(C_TYPE, ACCESSOR, NA_CHECK, BREAK_CHECK, OFFSET) { \
    SEXP unlistData = _get_CompressedList_unlistData(x);                \
    SEXP ends =                                                         \
      _get_PartitioningByEnd_end(_get_CompressedList_partitioning(x));  \
    Rboolean _na_rm = asLogical(na_rm);                                 \
    int prev_end = 0;                                                   \
    SEXP ans = allocVector(LGLSXP, length(ends));			\
    for (int i = 0; i < length(ends); i++) {                            \
      int end = INTEGER(ends)[i];                                       \
      Rboolean summary = FALSE;						\
      for (int j = prev_end + OFFSET; j < end; j++) {                   \
        C_TYPE val = ACCESSOR(unlistData)[j];                           \
        if (NA_CHECK) {                                                 \
          if (_na_rm) {                                                 \
            continue;                                                   \
          } else {                                                      \
            summary = NA_LOGICAL;					\
            break;                                                      \
          }								\
        }								\
	if (BREAK_CHECK) {						\
	  summary = TRUE;						\
	  break;							\
	}								\
      }                                                                 \
      LOGICAL(ans)[i] = summary;					\
      prev_end = end;                                                   \
    }                                                                   \
    setAttrib(ans, R_NamesSymbol, _get_CompressedList_names(x));        \
    return ans;                                                         \
  }

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedLogicalList_is_unsorted(SEXP x, SEXP na_rm, SEXP strictly)
{
  PARTITIONED_IS_UNSORTED(Rboolean, LOGICAL, val == NA_LOGICAL);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIntegerList_is_unsorted(SEXP x, SEXP na_rm, SEXP strictly)
{
  PARTITIONED_IS_UNSORTED(int, INTEGER, val == NA_INTEGER);
}

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedNumericList_is_unsorted(SEXP x, SEXP na_rm, SEXP strictly)
{
  PARTITIONED_IS_UNSORTED(double, REAL, ISNA(val));
}
