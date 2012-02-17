/****************************************************************************
 *                  Low-level manipulation of Hits objects                  *
 ****************************************************************************/
#include "IRanges.h"


SEXP make_all_group_inner_hits(SEXP group_sizes)
{
	int ngroup, ans_length, i, j, k, gs, iofeig, *left, *right;
	const int *group_sizes_elt;
	SEXP ans_query_hits, ans_subject_hits,
	     ans_query_length, ans_subject_length, ans;

	ngroup = LENGTH(group_sizes);
	for (i = ans_length = 0, group_sizes_elt = INTEGER(group_sizes);
	     i < ngroup;
	     i++, group_sizes_elt++)
	{
		gs = *group_sizes_elt;
		if (gs == NA_INTEGER || gs < 0)
			error("'group_sizes' contains NAs or negative values");
		ans_length += gs * gs;
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
		for (j = 1; j <= gs; j++) {
			for (k = 1; k <= gs; k++) {
				*(left++) = j + iofeig;
				*(right++) = k + iofeig;
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

