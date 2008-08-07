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
#include <Rdefines.h>
#include <R_ext/Rdynload.h>


/*
 * Extendable buffers used for temporary storage of incoming data whose size
 * is not known in advance:
 *
 *   o IntBuf:   extendable buffer of ints;
 *   o IntBBuf:  extendable buffer of extendable buffers of ints;
 *   o RangeBuf: extendable buffer of integer ranges;
 *   o CharBuf:  extendable buffer of chars;
 *   o CharBBuf: extendable buffer of extendable buffers of chars.
 *
 * They are NOT an attempt to reinvent an SEXP subsystem. Some notable
 * differences are: (a) they are extendable (i.e. they are automatically
 * reallocated when more room is needed to add a new element), (b) they are
 * much faster, and (c) they don't require any PROTECT/UNPROTECT mechanism.
 */

typedef struct ibuf {
	int buflength;
	int *elts;
	int nelt;
} IntBuf;

typedef struct ibbuf {
	int buflength;
	IntBuf *elts;
	int nelt;
} IntBBuf; 

typedef struct rangebuf {
	IntBuf start;
	IntBuf width;
} RangeBuf;

typedef struct cbuf {
	int buflength;
	char *elts;
	int nelt;
} CharBuf; 

typedef struct cbbuf {
        int buflength;
        CharBuf *elts;
        int nelt;
} CharBBuf; 


/*
 * Three additional types:
 *
 *   o RoSeq:  array of const chars (think of this as a pointer to a non
 *               null-terminated sequence of chars);
 *   o RoSeqs: array of arrays of const chars;
 *   o CachedXStringSet: use for fast extraction of the elements of
 *             an XStringSet object in a loop.
 */
typedef struct roseq {
	const char *elts;
	int nelt;
} RoSeq;

typedef struct roseqs {
	RoSeq *elts;
	int nelt;
} RoSeqs;

typedef struct cachedxstringset {
	int *start;
	int *width;
	char *super_elts;
	int super_nelt;
	const char *baseClass;
	const int *enc_chrtrtable;
	const int *dec_chrtrtable;
} CachedXStringSet;


/*
 * Match reporting modes (more modes will be added soon...)
 */
#define COUNT_MRMODE	1
#define START_MRMODE	2

