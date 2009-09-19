/*****************************************************************************
 IRanges C interface: typedefs and defines
 -----------------------------------------

   The IRanges C interface is splitted in 2 files:
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


/*
 * Auto-Extending buffers used for temporary storage of incoming data whose size
 * is not known in advance:
 *
 *   o IntAE:    Auto-Extending buffer of ints;
 *   o IntAEAE:  Auto-Extending buffer of Auto-Extending buffers of ints;
 *   o RangeAE:  Auto-Extending buffer of integer ranges;
 *   o CharAE:   Auto-Extending buffer of chars;
 *   o CharAEAE: Auto-Extending buffer of Auto-Extending buffers of chars.
 *
 * Some differences between AE buffers and SEXP: (a) AE buffers auto-extend
 * i.e. they automatically reallocate when more room is needed to add a new
 * element, (b) they are faster, and (c) they don't require any
 * PROTECT/UNPROTECT mechanism.
 */

typedef struct int_ae {
	int buflength;
	int *elts;
	int nelt;
} IntAE;

typedef struct int_aeae {
	int buflength;
	IntAE *elts;
	int nelt;
} IntAEAE; 

typedef struct range_ae {
	IntAE start;
	IntAE width;
} RangeAE;

typedef struct char_ae {
	int buflength;
	char *elts;
	int nelt;
} CharAE; 

typedef struct char_aeae {
        int buflength;
        CharAE *elts;
        int nelt;
} CharAEAE; 


/*
 * cached_* structs.
 */

typedef struct cached_iranges {
	const char *classname;
	int is_constant_width;
	int offset;
	int length;
	const int *width;
	const int *start;
	const int *end;
	SEXP names;
} cachedIRanges;

typedef struct cached_compressedirangeslist {
	const char *classname;
	int length;
	const int *end;
	cachedIRanges cached_unlistData;
} cachedCompressedIRangesList;

typedef struct cached_charseq {
	const char *seq;
	int length;
} cachedCharSeq;

typedef struct cached_xvectorlist {
	const char *classname;
	const char *element_type;
	SEXP xp_list;
	int length;
	const int *start;
	const int *width;
	const int *group;
} cachedXVectorList;

#endif
