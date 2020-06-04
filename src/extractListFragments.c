/****************************************************************************
 *                 A Nested Containment List implementation                 *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

/*
 * --- .Call ENTRY POINT ---
 * Args:
 *   q_end, s_end:          Integer vectors containing the breakpoints of 2
 *                          compatible Partitioning objects.
 *   with_split_partitions: TRUE or FALSE.
 * Find the overlaps between 2 Partitioning objects in linear time. Note that,
 * more generally speaking, the overlaps between 2 IntegerRanges derivatives
 * that are both disjoint and sorted can be found in linear time. However, the
 * algorithm implemented below is only for Partitioning objects (which are
 * a particular type of such objects). Also note that, although findOverlaps()
 * could be used for this, it isn't as efficient as the algorithm below
 * because of the cost of building a Nested Containment List object and using
 * a binary search on it.
 * If 'with_split_partitions' is FALSE, return a list of 2 sorted integer
 * vectors of the same length, the 1st one for the query hits and the 2nd one
 * for the subject hits. If 'with_split_partitions' is TRUE, a 3rd list
 * element that is also a sorted integer vector parallel to the first 2
 * vectors is added. This vector contains the breakpoints of the Partitioning
 * object obtained by splitting the query by the subject (or vice-versa, this
 * split is commutative).
 */
SEXP C_find_partition_overlaps(SEXP q_end, SEXP s_end,
			       SEXP with_split_partitions)
{
	int q_len, s_len, q_prev_end, s_prev_end, i, j;
	IntPairAE *hits_buf;
	IntAE *split_partitions_buf;
	const int *q_end_p, *s_end_p;
	SEXP ans, ans_elt;

	q_len = LENGTH(q_end);
	s_len = LENGTH(s_end);
	hits_buf = new_IntPairAE(0, 0);
	if (LOGICAL(with_split_partitions)[0])
		split_partitions_buf = new_IntAE(0, 0, 0);
	q_end_p = INTEGER(q_end);
	s_end_p = INTEGER(s_end);
	q_prev_end = s_prev_end = 0;
	i = j = 1;
	while (i <= q_len && j <= s_len) {
		if (q_prev_end == s_prev_end) {
			if (*q_end_p == q_prev_end) {
				i++;
				q_end_p++;
				continue;
			}
			if (*s_end_p == s_prev_end) {
				j++;
				s_end_p++;
				continue;
			}
		}
		IntPairAE_insert_at(hits_buf,
				    IntPairAE_get_nelt(hits_buf), i, j);
		if (*q_end_p < *s_end_p) {
			q_prev_end = *q_end_p;
			if (LOGICAL(with_split_partitions)[0])
				IntAE_insert_at(split_partitions_buf,
					IntAE_get_nelt(split_partitions_buf),
					q_prev_end);
			i++;
			q_end_p++;
			continue;
		}
		if (*s_end_p < *q_end_p) {
			s_prev_end = *s_end_p;
			if (LOGICAL(with_split_partitions)[0])
				IntAE_insert_at(split_partitions_buf,
					IntAE_get_nelt(split_partitions_buf),
					s_prev_end);
			j++;
			s_end_p++;
			continue;
		}
		q_prev_end = *q_end_p;
		if (LOGICAL(with_split_partitions)[0])
			IntAE_insert_at(split_partitions_buf,
				IntAE_get_nelt(split_partitions_buf),
				q_prev_end);
		i++;
		q_end_p++;
		s_prev_end = *s_end_p;
		j++;
		s_end_p++;
	}

	ans = PROTECT(NEW_LIST(LOGICAL(with_split_partitions)[0] ? 3 : 2));

	ans_elt = PROTECT(new_INTEGER_from_IntAE(hits_buf->a));
	SET_VECTOR_ELT(ans, 0, ans_elt);
	UNPROTECT(1);

	ans_elt = PROTECT(new_INTEGER_from_IntAE(hits_buf->b));
	SET_VECTOR_ELT(ans, 1, ans_elt);
	UNPROTECT(1);

	if (LOGICAL(with_split_partitions)[0]) {
		ans_elt = PROTECT(new_INTEGER_from_IntAE(split_partitions_buf));
		SET_VECTOR_ELT(ans, 2, ans_elt);
		UNPROTECT(1);
	}

	UNPROTECT(1);
	return ans;
}

