/*****************************************************************************
 IRanges C interface: typedefs and defines
 -----------------------------------------

   The IRanges C interface is split in 2 files:
     1. IRanges_defines.h (this file): contains the typedefs and defines
        of the interface.
     2. IRanges_interface.h (in this directory): contains the prototypes
        of the IRanges C routines that are part of the interface.

   Please consult IRanges_interface.h for how to use this interface in your
   package.

 *****************************************************************************/
#ifndef IRANGES_DEFINES_H
#define IRANGES_DEFINES_H

#include <Rdefines.h>
#include <R_ext/Rdynload.h>


/* Hash table -- modified from R_HOME/src/main/unique.c */
struct htab {
	int K, M;
	unsigned int Mminus1;
	int *buckets;
};


/*
 * Auto-Extending buffers used for temporary storage of incoming data whose
 * size is not known in advance:
 *
 *   o IntAE:     Auto-Extending buffer of ints;
 *   o IntAEAE:   Auto-Extending buffer of Auto-Extending buffers of ints;
 *   o RangeAE:   Auto-Extending buffer of integer ranges;
 *   o RangeAEAE: Auto-Extending buffer of Auto-Extending buffers of integer
 *                ranges;
 *   o CharAE:    Auto-Extending buffer of chars;
 *   o CharAEAE:  Auto-Extending buffer of Auto-Extending buffers of chars.
 *
 * Some differences between AE buffers and SEXP: (a) AE buffers auto-extend
 * i.e. they automatically reallocate when more room is needed to add a new
 * element, (b) they are faster, and (c) they don't require any
 * PROTECT/UNPROTECT mechanism.
 */

typedef struct int_ae {
	int buflength;
	int *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} IntAE;

typedef struct int_aeae {
	int buflength;
	IntAE *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} IntAEAE; 

typedef struct range_ae {
	IntAE start;
	IntAE width;
	int _AE_malloc_stack_idx;
} RangeAE;

typedef struct range_aeae {
	int buflength;
	RangeAE *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} RangeAEAE;

typedef struct char_ae {
	int buflength;
	char *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} CharAE; 

typedef struct char_aeae {
	int buflength;
	CharAE *elts;
	int _nelt;
	int _AE_malloc_stack_idx;
} CharAEAE; 


/*
 * *_holder structs.
 */

typedef struct iranges_holder {
	const char *classname;
	int is_constant_width;
	int length;
	const int *width;
	const int *start;
	const int *end;
	int SEXP_offset;  /* offset in 'names' member below */
	SEXP names;
} IRanges_holder;

typedef struct compressed_iranges_list_holder {
	const char *classname;
	int length;
	const int *end;
	IRanges_holder unlistData_holder;
} CompressedIRangesList_holder;

#endif
