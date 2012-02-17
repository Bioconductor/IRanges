/****************************************************************************
 *                  Low-level manipulation of Hits objects                  *
 ****************************************************************************/
#include "IRanges.h"


/*
 * --- .Call ENTRY POINT ---
 * 'hit_type' must be 0, -1 or 1 (single integer).
 */
SEXP make_all_group_inner_hits(SEXP group_sizes, SEXP hit_type)
{
	int ngroup, htype, ans_length, i, j, k, gs, nhit,
	    iofeig, *left, *right;
	const int *group_sizes_elt;
	SEXP ans_query_hits, ans_subject_hits,
	     ans_query_length, ans_subject_length, ans;

	ngroup = LENGTH(group_sizes);
	htype = INTEGER(hit_type)[0];
	for (i = ans_length = 0, group_sizes_elt = INTEGER(group_sizes);
	     i < ngroup;
	     i++, group_sizes_elt++)
	{
		gs = *group_sizes_elt;
		if (gs == NA_INTEGER || gs < 0)
			error("'group_sizes' contains NAs or negative values");
		nhit = htype == 0 ? gs * gs : (gs * (gs - 1)) / 2;
		ans_length += nhit;
		
	}
	PROTECT(ans_query_hits = NEW_INTEGER(ans_length));
	PROTECT(ans_subject_hits = NEW_INTEGER(ans_length));
	left = INTEGER(ans_query_hits);
	right = INTEGER(ans_subject_hits);
	iofeig = 0; /* 0-based Index Of First Element In Group */
	for (i = 0, group_sizes_elt = INTEGER(group_sizes);
	     i < ngroup;
	     i++, group_sizes_elt++)
	{
		gs = *group_sizes_elt;
		if (htype > 0) {
			for (j = 1; j < gs; j++) {
				for (k = j + 1; k <= gs; k++) {
					*(left++) = j + iofeig;
					*(right++) = k + iofeig;
				}
			}
		} else if (htype < 0) {
			for (j = 2; j <= gs; j++) {
				for (k = 1; k < j; k++) {
					*(left++) = j + iofeig;
					*(right++) = k + iofeig;
				}
			}
		} else {
			for (j = 1; j <= gs; j++) {
				for (k = 1; k <= gs; k++) {
					*(left++) = j + iofeig;
					*(right++) = k + iofeig;
				}
			}
		}
		iofeig += gs;
	}
	PROTECT(ans_query_length = ScalarInteger(iofeig));
	PROTECT(ans_subject_length = ScalarInteger(iofeig));

	PROTECT(ans = NEW_OBJECT(MAKE_CLASS("Hits")));
	SET_SLOT(ans, install("queryHits"), ans_query_hits);
	SET_SLOT(ans, install("subjectHits"), ans_subject_hits);
	SET_SLOT(ans, install("queryLength"), ans_query_length);
	SET_SLOT(ans, install("subjectLength"), ans_subject_length);
	UNPROTECT(5);
	return ans;
}

