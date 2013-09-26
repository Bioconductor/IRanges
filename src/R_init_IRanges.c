#include "IRanges.h"

#define CALLMETHOD_DEF(fun, numArgs) {#fun, (DL_FUNC) &fun, numArgs}

#define REGISTER_CCALLABLE(fun) \
	R_RegisterCCallable("IRanges", #fun, (DL_FUNC) &fun)

static const R_CallMethodDef callMethods[] = {

/* AEbufs.c */
	CALLMETHOD_DEF(debug_AEbufs, 0),
	CALLMETHOD_DEF(AEbufs_use_malloc, 1),
	CALLMETHOD_DEF(AEbufs_free, 0),

/* anyMissing.c */
	CALLMETHOD_DEF(anyMissing, 1),

/* SEXP_utils.c */
	CALLMETHOD_DEF(sapply_NROW, 1),

/* int_utils.c */
	CALLMETHOD_DEF(Integer_any_missing_or_outside, 3),
	CALLMETHOD_DEF(Integer_sum_non_neg_vals, 1),
	CALLMETHOD_DEF(Integer_diff_with_0, 1),
        CALLMETHOD_DEF(Integer_diff_with_last, 2),
	CALLMETHOD_DEF(Integer_order, 2),
	CALLMETHOD_DEF(Integer_order2, 3),
	CALLMETHOD_DEF(Integer_match2_quick, 5),
	CALLMETHOD_DEF(Integer_selfmatch2_quick, 2),
	CALLMETHOD_DEF(Integer_match2_hash, 5),
	CALLMETHOD_DEF(Integer_selfmatch2_hash, 2),
	CALLMETHOD_DEF(Integer_order4, 5),
	CALLMETHOD_DEF(Integer_match4_quick, 9),
	CALLMETHOD_DEF(Integer_selfmatch4_quick, 4),
	CALLMETHOD_DEF(Integer_match4_hash, 9),
	CALLMETHOD_DEF(Integer_selfmatch4_hash, 4),
	CALLMETHOD_DEF(Integer_tabulate2, 4),
	CALLMETHOD_DEF(Integer_explode_bits, 2),
	CALLMETHOD_DEF(Integer_sorted_merge, 2),
	CALLMETHOD_DEF(Integer_mseq, 2),
	CALLMETHOD_DEF(Integer_fancy_mseq, 3),
	CALLMETHOD_DEF(findIntervalAndStartFromWidth, 2),

/* str_utils.c */
	CALLMETHOD_DEF(safe_strexplode, 1),
	CALLMETHOD_DEF(strsplit_as_list_of_ints, 2),
	CALLMETHOD_DEF(svn_time, 0),

/* compact_bitvector.c */
	CALLMETHOD_DEF(logical_as_compact_bitvector, 1),
	CALLMETHOD_DEF(compact_bitvector_as_logical, 2),
	CALLMETHOD_DEF(subset_compact_bitvector, 2),
	CALLMETHOD_DEF(compact_bitvector_bit_count, 1),
	CALLMETHOD_DEF(compact_bitvector_last_bit, 1),
	CALLMETHOD_DEF(compact_bitvector_set_op, 3),

/* Vector_class.c */
	CALLMETHOD_DEF(vector_subsetByRanges, 3),
	CALLMETHOD_DEF(vector_seqselect, 3),

/* Ranges_comparison.c */
	CALLMETHOD_DEF(Ranges_compare, 4),

/* IRanges_class.c */
	CALLMETHOD_DEF(debug_IRanges_class, 0),
	CALLMETHOD_DEF(IRanges_isNormal, 1),
	CALLMETHOD_DEF(IRanges_from_integer, 1),
	CALLMETHOD_DEF(NormalIRanges_from_logical, 1),

/* IRanges_constructor.c */
	CALLMETHOD_DEF(solve_user_SEW0, 3),
	CALLMETHOD_DEF(solve_user_SEW, 6),

/* Grouping_class.c */
	CALLMETHOD_DEF(debug_Grouping_class, 0),

	CALLMETHOD_DEF(H2LGrouping_members, 2),
	CALLMETHOD_DEF(H2LGrouping_vmembers, 2),

/* Rle_class.c */
	CALLMETHOD_DEF(Rle_constructor, 4),
	CALLMETHOD_DEF(Rle_start, 1),
	CALLMETHOD_DEF(Rle_end, 1),
	CALLMETHOD_DEF(Rle_getStartEndRunAndOffset, 3),
	CALLMETHOD_DEF(Rle_window_aslist, 5),
	CALLMETHOD_DEF(Rle_window, 6),
	CALLMETHOD_DEF(Rle_seqselect, 3),

/* Rle_utils.c */
	CALLMETHOD_DEF(Rle_runsum, 3),
	CALLMETHOD_DEF(Rle_runwtsum, 4),
	CALLMETHOD_DEF(Rle_runq, 4),

/* RleViews_utils.c */
	CALLMETHOD_DEF(RleViews_viewMins, 2),
	CALLMETHOD_DEF(RleViews_viewMaxs, 2),
	CALLMETHOD_DEF(RleViews_viewSums, 2),
	CALLMETHOD_DEF(RleViews_viewMeans, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMins, 2),
	CALLMETHOD_DEF(RleViews_viewWhichMaxs, 2),

/* SimpleIRangesList_class.c */
	CALLMETHOD_DEF(SimpleIRangesList_isNormal, 1),
	CALLMETHOD_DEF(SimpleNormalIRangesList_min, 1),
	CALLMETHOD_DEF(SimpleNormalIRangesList_max, 1),

/* CompressedIRangesList_class.c */
	CALLMETHOD_DEF(CompressedIRangesList_isNormal, 2),
	CALLMETHOD_DEF(CompressedIRangesList_summary, 1),
	CALLMETHOD_DEF(CompressedNormalIRangesList_min, 2),
	CALLMETHOD_DEF(CompressedNormalIRangesList_max, 2),

/* GappedRanges_class.c */
	CALLMETHOD_DEF(valid_GappedRanges, 2),

/* Hits_class.c */
	CALLMETHOD_DEF(make_all_group_inner_hits, 2),

/* inter_range_methods.c */
	CALLMETHOD_DEF(debug_inter_range_methods, 0),
	CALLMETHOD_DEF(IRanges_range, 1),
	CALLMETHOD_DEF(Ranges_reduce, 6),
	CALLMETHOD_DEF(CompressedIRangesList_reduce, 4),
	CALLMETHOD_DEF(IRanges_gaps, 4),
	CALLMETHOD_DEF(CompressedIRangesList_gaps, 3),
	CALLMETHOD_DEF(Ranges_disjointBins, 2),

/* encodeOverlaps_methods.c */
	CALLMETHOD_DEF(encode_overlaps1, 10),
	CALLMETHOD_DEF(RangesList_encode_overlaps, 7),
	CALLMETHOD_DEF(Hits_encode_overlaps, 10),

/* coverage_methods.c */
	CALLMETHOD_DEF(Ranges_integer_coverage, 5),
	CALLMETHOD_DEF(Ranges_numeric_coverage, 5),

	{NULL, NULL, 0}
};


void R_init_IRanges(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);

/* sort_utils.c */
	REGISTER_CCALLABLE(_sort_int_array);
	REGISTER_CCALLABLE(_get_order_of_int_array);
	REGISTER_CCALLABLE(_get_order_of_int_pairs);
	REGISTER_CCALLABLE(_get_order_of_int_quads);

/* hash_utils.c */
	REGISTER_CCALLABLE(_new_htab);
	REGISTER_CCALLABLE(_get_hbucket_val);
	REGISTER_CCALLABLE(_set_hbucket_val);

/* AEbufs.c */
	REGISTER_CCALLABLE(_get_new_buflength);
	REGISTER_CCALLABLE(_IntAE_get_nelt);
	REGISTER_CCALLABLE(_IntAE_set_nelt);
	REGISTER_CCALLABLE(_IntAE_set_val);
	REGISTER_CCALLABLE(_new_IntAE);
	REGISTER_CCALLABLE(_IntAE_insert_at);
	REGISTER_CCALLABLE(_IntAE_append);
	REGISTER_CCALLABLE(_IntAE_delete_at);
	REGISTER_CCALLABLE(_IntAE_shift);
	REGISTER_CCALLABLE(_IntAE_sum_and_shift);
	REGISTER_CCALLABLE(_IntAE_append_shifted_vals);
	REGISTER_CCALLABLE(_IntAE_qsort);
	REGISTER_CCALLABLE(_IntAE_delete_adjdups);
	REGISTER_CCALLABLE(_new_INTEGER_from_IntAE);
	REGISTER_CCALLABLE(_new_IntAE_from_INTEGER);
	REGISTER_CCALLABLE(_new_IntAE_from_CHARACTER);
	REGISTER_CCALLABLE(_IntAEAE_get_nelt);
	REGISTER_CCALLABLE(_IntAEAE_set_nelt);
	REGISTER_CCALLABLE(_new_IntAEAE);
	REGISTER_CCALLABLE(_IntAEAE_insert_at);
	REGISTER_CCALLABLE(_IntAEAE_eltwise_append);
	REGISTER_CCALLABLE(_IntAEAE_shift);
	REGISTER_CCALLABLE(_IntAEAE_sum_and_shift);
	REGISTER_CCALLABLE(_new_LIST_from_IntAEAE);
	REGISTER_CCALLABLE(_new_IntAEAE_from_LIST);
	REGISTER_CCALLABLE(_IntAEAE_toEnvir);
	REGISTER_CCALLABLE(_RangeAE_get_nelt);
	REGISTER_CCALLABLE(_RangeAE_set_nelt);
	REGISTER_CCALLABLE(_new_RangeAE);
	REGISTER_CCALLABLE(_RangeAE_insert_at);
	REGISTER_CCALLABLE(_RangeAEAE_get_nelt);
	REGISTER_CCALLABLE(_RangeAEAE_set_nelt);
	REGISTER_CCALLABLE(_new_RangeAEAE);
	REGISTER_CCALLABLE(_RangeAEAE_insert_at);
	REGISTER_CCALLABLE(_CharAE_get_nelt);
	REGISTER_CCALLABLE(_CharAE_set_nelt);
	REGISTER_CCALLABLE(_new_CharAE);
	REGISTER_CCALLABLE(_new_CharAE_from_string);
	REGISTER_CCALLABLE(_CharAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAE);
	REGISTER_CCALLABLE(_CharAE_delete_at);
	REGISTER_CCALLABLE(_new_RAW_from_CharAE);
	REGISTER_CCALLABLE(_new_LOGICAL_from_CharAE);
	REGISTER_CCALLABLE(_CharAEAE_get_nelt);
	REGISTER_CCALLABLE(_CharAEAE_set_nelt);
	REGISTER_CCALLABLE(_new_CharAEAE);
	REGISTER_CCALLABLE(_CharAEAE_insert_at);
	REGISTER_CCALLABLE(_append_string_to_CharAEAE);
	REGISTER_CCALLABLE(_new_CHARACTER_from_CharAEAE);

/* SEXP_utils.c */
	REGISTER_CCALLABLE(_get_classname);

/* int_utils.c */
	REGISTER_CCALLABLE(_check_integer_pairs);

/* Vector_class.c */
	REGISTER_CCALLABLE(_get_List_elementType);
	REGISTER_CCALLABLE(_set_List_elementType);
	REGISTER_CCALLABLE(_vector_memcmp);
	REGISTER_CCALLABLE(_vector_memcpy);

/* IRanges_class.c */
	REGISTER_CCALLABLE(_get_IRanges_start);
	REGISTER_CCALLABLE(_get_IRanges_width);
	REGISTER_CCALLABLE(_get_IRanges_names);
	REGISTER_CCALLABLE(_get_IRanges_length);
	REGISTER_CCALLABLE(_cache_IRanges);
	REGISTER_CCALLABLE(_get_cachedIRanges_length);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_width);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_start);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_end);
	REGISTER_CCALLABLE(_get_cachedIRanges_elt_name);
	REGISTER_CCALLABLE(_sub_cachedIRanges);
	REGISTER_CCALLABLE(_set_IRanges_names);
	REGISTER_CCALLABLE(_copy_IRanges_slots);
	REGISTER_CCALLABLE(_new_IRanges);
	REGISTER_CCALLABLE(_new_IRanges_from_RangeAE);
	REGISTER_CCALLABLE(_new_list_of_IRanges_from_RangeAEAE);
	REGISTER_CCALLABLE(_alloc_IRanges);

/* Grouping_class.c */
	REGISTER_CCALLABLE(_get_H2LGrouping_high2low);
	REGISTER_CCALLABLE(_get_H2LGrouping_low2high);
	REGISTER_CCALLABLE(_get_Partitioning_names);
	REGISTER_CCALLABLE(_get_PartitioningByEnd_end);
	REGISTER_CCALLABLE(_new_PartitioningByEnd);

/* SimpleList_class.c */
	REGISTER_CCALLABLE(_new_SimpleList);

/* DataFrame_class.c */
	REGISTER_CCALLABLE(_new_DataFrame);

/* CompressedList_class.c */
	REGISTER_CCALLABLE(_new_CompressedList);

/* CompressedIRangesList_class.c */
	REGISTER_CCALLABLE(_cache_CompressedIRangesList);
	REGISTER_CCALLABLE(_get_cachedCompressedIRangesList_elt);

/* RangedData_class.c */
	REGISTER_CCALLABLE(_new_RangedData);

/* Rle_class.c */
	REGISTER_CCALLABLE(_seqselect_Rle);
	return;
}

