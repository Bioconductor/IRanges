/****************************************************************************
 *         Low-level manipulation of CompressedIRangesList objects          *
 ****************************************************************************/

#include "IRanges.h"


/****************************************************************************
 * C-level slot getters for CompressedIRangesList objects.
 *
 * Be careful that these functions do NOT duplicate the returned slot.
 * Thus they cannot be made .Call entry points!
 */

static SEXP
	unlistData_symbol = NULL,
	partitioning_symbol = NULL;

SEXP _get_CompressedIRangesList_unlistData(SEXP x)
{
	INIT_STATIC_SYMBOL(unlistData)
	return GET_SLOT(x, unlistData_symbol);
}

SEXP _get_CompressedIRangesList_partitioning(SEXP x)
{
	INIT_STATIC_SYMBOL(partitioning)
	return GET_SLOT(x, partitioning_symbol);
}

/* Not strict "slot getters" but very much like. */

int _get_CompressedIRangesList_length(SEXP x)
{
	return LENGTH(_get_PartitioningByEnd_end(
			_get_CompressedIRangesList_partitioning(x)));
}


/****************************************************************************
 * C-level abstract getters.
 */

cachedCompressedIRangesList _cache_CompressedIRangesList(SEXP x)
{
	cachedCompressedIRangesList cached_x;
	SEXP x_end;

	cached_x.classname = _get_classname(x);
	x_end = _get_PartitioningByEnd_end(_get_CompressedIRangesList_partitioning(x));
	cached_x.length = LENGTH(x_end);
	cached_x.end = INTEGER(x_end);
	cached_x.cached_unlistData = _cache_IRanges(
				_get_CompressedIRangesList_unlistData(x));
	return cached_x;
}

int _get_cachedCompressedIRangesList_length(const cachedCompressedIRangesList *cached_x)
{
	return cached_x->length;
}

cachedIRanges _get_cachedCompressedIRangesList_elt(
		const cachedCompressedIRangesList *cached_x, int i)
{
	int offset, length;

	offset = i == 0 ? 0 : cached_x->end[i - 1];
	length = cached_x->end[i] - offset;
	return _sub_cachedIRanges(&(cached_x->cached_unlistData), offset, length);
}


/****************************************************************************
 * Other utilities.
 */

/*
 * --- .Call ENTRY POINT ---
 */
SEXP CompressedIRangesList_summary(SEXP object)
{
	int ans_len;
	SEXP part_end;
	SEXP ans, ans_names, col_names;

	part_end = GET_SLOT(GET_SLOT(object, install("partitioning")),
			                     install("end"));
	ans_len = LENGTH(part_end);
	PROTECT(ans = allocMatrix(INTSXP, ans_len, 2));
	memset(INTEGER(ans), 0, 2 * ans_len * sizeof(int));
	if (ans_len > 0) {
		int i, j, prev_end = 0;
		int *ans1_elt, *ans2_elt;
		const int *part_end_elt, *ranges_width;
		SEXP unlistData = GET_SLOT(object, install("unlistData"));
		ranges_width = INTEGER(_get_IRanges_width(unlistData));
		for (i = 0, ans1_elt = INTEGER(ans), ans2_elt = (INTEGER(ans) + ans_len),
			 part_end_elt = INTEGER(part_end);
		     i < ans_len; i++, ans1_elt++, ans2_elt++, part_end_elt++)
		{
			*ans1_elt = *part_end_elt - prev_end;
			for (j = 0; j < *ans1_elt; j++) {
				*ans2_elt += *ranges_width;
				ranges_width++;
			}
			prev_end = *part_end_elt;
		}
	}
	PROTECT(ans_names = NEW_LIST(2));
	PROTECT(col_names = NEW_CHARACTER(2));
	SET_STRING_ELT(col_names, 0, mkChar("Length"));
	SET_STRING_ELT(col_names, 1, mkChar("WidthSum"));
	SET_VECTOR_ELT(ans_names, 0,
			       duplicate(GET_SLOT(GET_SLOT(object, install("partitioning")),
			    		              install("NAMES"))));
	SET_VECTOR_ELT(ans_names, 1, col_names);
	SET_DIMNAMES(ans, ans_names);
	UNPROTECT(3);
	return ans;
}
