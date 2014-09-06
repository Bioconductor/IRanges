/*****************************************************************************
 IRanges C interface: prototypes
 -------------------------------

   The IRanges C interface is split in 2 files:
     1. IRanges_defines.h (in this directory): contains the typedefs and
        defines of the interface.
     2. IRanges_interface.h (this file): contains the prototypes of the
        IRanges C routines that are part of the interface.

 *****************************************************************************/
#include "IRanges_defines.h"


/*
 * Comparing ranges.
 * (see Ranges_comparison.c)
 */

int overlap_code(
	int x_start,
	int x_width,
	int y_start,
	int y_width
);

int invert_overlap_code(int code);

/*
 * Low-level manipulation of IRanges objects.
 * (see IRanges_class.c)
 */

SEXP get_IRanges_start(SEXP x);

SEXP get_IRanges_width(SEXP x);

SEXP get_IRanges_names(SEXP x);

int get_IRanges_length(SEXP x);

IRanges_holder hold_IRanges(SEXP x);

int get_length_from_IRanges_holder(const IRanges_holder *x_holder);

int get_width_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i);

int get_start_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i);

int get_end_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i);

SEXP get_names_elt_from_IRanges_holder(const IRanges_holder *x_holder, int i);

IRanges_holder get_linear_subset_from_IRanges_holder(const IRanges_holder *x_holder, int offset, int length);

void set_IRanges_names(SEXP x, SEXP names);

void copy_IRanges_slots(SEXP x, SEXP x0);

SEXP new_IRanges(const char *classname, SEXP start, SEXP width, SEXP names);

SEXP new_IRanges_from_RangeAE(const char *classname, const RangeAE *range_ae);

SEXP new_list_of_IRanges_from_RangeAEAE(const char *element_type, const RangeAEAE *range_aeae);

SEXP alloc_IRanges(const char *classname, int length);

/*
 * Low-level manipulation of Grouping objects.
 * (see Grouping_class.c)
 */

SEXP get_H2LGrouping_high2low(SEXP x);

SEXP get_H2LGrouping_low2high(SEXP x);

SEXP get_Partitioning_names(SEXP x);

SEXP get_PartitioningByEnd_end(SEXP x);

SEXP new_PartitioningByEnd(const char *classname, SEXP end, SEXP names);

/*
 * Low-level manipulation of DataFrame objects.
 * (see DataFrame_class.c)
 */

SEXP new_DataFrame(const char *classname, SEXP vars, SEXP rownames, SEXP nrows);

/*
 * Low-level manipulation of CompressedList objects.
 * (see CompressedList_class.c)
 */

SEXP get_CompressedList_unlistData(SEXP x);

SEXP get_CompressedList_partitioning(SEXP x);

int get_CompressedList_length(SEXP x);

SEXP get_CompressedList_names(SEXP x);

SEXP new_CompressedList(const char *classname, SEXP unlistData, SEXP partitioning);

/*
 * Low-level manipulation of CompressedIRangesList objects.
 * (see CompressedIRangesList_class.c)
 */

CompressedIRangesList_holder hold_CompressedIRangesList(SEXP x);

IRanges_holder get_elt_from_CompressedIRangesList_holder(const CompressedIRangesList_holder *x_holder, int i);

/*
 * Low-level manipulation of RangedData objects.
 * (see RangedData_class.c)
 */

SEXP new_RangedData(const char *classname, SEXP ranges, SEXP values);

