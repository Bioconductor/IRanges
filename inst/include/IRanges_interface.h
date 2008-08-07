/*****************************************************************************
 IRanges C interface: prototypes
 -------------------------------

   The IRanges C interface is splitted in 2 files:
     1. IRanges_defines.h (in this directory): contains the typedefs and
        defines of the interface.
     2. IRanges_interface.h (this file): contains the prototypes of the
        IRanges C routines that are part of the interface.

 *****************************************************************************/
#include "IRanges_defines.h"


/*
 * Low-level manipulation of the extendable buffers.
 */

CharBuf new_CharBuf_from_string(
	const char *string
);

CharBBuf new_CharBBuf(
	int buflength,
	int nelt
);

void append_string_to_CharBBuf(
	CharBBuf *cbbuf,
	const char *string
);

