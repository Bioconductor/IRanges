#include "IRanges.h"


static int debug = 0;

SEXP debug_X_utils()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in 'X_utils.c'\n", debug ? "on" : "off");
#else
	Rprintf("Debug mode not available in 'X_utils.c'\n");
#endif
	return R_NilValue;
}


/*
 * From R:
 *   .Call("IRanges_sexp_address", 6:4, PACKAGE="IRanges")
 *   .Call("IRanges_sexp_address", new("externalptr"), PACKAGE="IRanges")
 */
SEXP IRanges_sexp_address(SEXP s)
{
	SEXP ans;
	char buf[40]; /* should be enough, even for 128-bit addresses */

	snprintf(buf, sizeof(buf), "%p", s);
	PROTECT(ans = NEW_CHARACTER(1));
	SET_STRING_ELT(ans, 0, mkChar(buf));
	UNPROTECT(1);
	return ans;
}

/*
 * Print some obscure info about an "externalptr" object.
 * From R:
 *   .Call("IRanges_xp_show", new("externalptr"), PACKAGE="IRanges")
 */
SEXP IRanges_xp_show(SEXP xp)
{
	SEXP s;
	void *p;

	Rprintf("Object of class 'externalptr':\n");
	Rprintf("  xp adress: %p\n", xp);
	p = R_ExternalPtrAddr(xp);
	Rprintf("  R_ExternalPtrAddr(xp): %p\n", p);
	s = R_ExternalPtrTag(xp);
	Rprintf("  R_ExternalPtrTag(xp): %p", s);
	Rprintf("%s\n", TYPEOF(s) == NILSXP ? " (NILSXP)" : "");
	s = R_ExternalPtrProtected(xp);
	Rprintf("  R_ExternalPtrProtected(xp): %p", s);
	Rprintf("%s\n", TYPEOF(s) == NILSXP ? " (NILSXP)" : "");
	return R_NilValue;
}

/*
 * new("externalptr") will always return the same instance of an external
 * pointer object! If you need a new instance, use this function instead.
 * From R:
 *   xp <- .Call("IRanges_xp_new", PACKAGE="IRanges")
 */
SEXP IRanges_xp_new()
{
	return R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
}


/****************************************************************************
 Functions defined below are NOT .Call methods: they are low level routines
 used by .Call methods. They are almost "R independent" (i.e. except for the
 use of R_alloc() or the error()/warning() macros, they don't use/know
 anything about R internals).
 DON'T REGISTER THEM IN R_init_IRanges.c!
 They are all prefixed with "_IRanges_" to minimize the risk of clash with
 symbols found in libc (before "_memcmp" was renamed "_IRanges_memcmp" it
 was clashing with "_memcmp" from libc on churchill).
 ****************************************************************************/


/* ==========================================================================
 * Memory comparison
 * --------------------------------------------------------------------------
 */

int _IRanges_memcmp(const char *a, int ia, const char *b, int ib, int n, size_t size)
{
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _IRanges_memcmp(): ");
		Rprintf("a=%p ia=%d b=%p ib=%d n=%d size=%d\n",
			a, ia, b, ib, n, size);
	}
#endif
	a += ia * size;
	b += ib * size;
	/* memcmp() doesn't try to be smart by checking if a == b */
	return a == b ? 0 : memcmp(a, b, n * size);
}


/* ==========================================================================
 * Memory copy:
 *   dest[(i-i1) % dest_nmemb] <- src[i] for i1 <= i <= i2
 * --------------------------------------------------------------------------
 * Reads a linear subset from 'src' defined by 'i1', 'i2'.
 * Writing is recycled in 'dest': it starts at its first member
 * and comes back to it after it reaches its last member.
 * Don't do anything if i1 > i2.
 */
void _IRanges_memcpy_from_i1i2(int i1, int i2,
		char *dest, size_t dest_nmemb,
		const char *src, size_t src_nmemb, size_t size)
{
	const char *b;
	int i2next, i1max, q;
	size_t dest_size;

	if (i1 > i2)
		return;
	if (i1 < 0 || i2 >= src_nmemb)
		error("subscript out of bounds");
	if (dest_nmemb == 0)
		error("no destination to copy to");
	i2next = i2 + 1;
	i1max = i2next - dest_nmemb;
	b = src + i1 * size;
	dest_size = dest_nmemb * size;
	while (i1 <= i1max) {
		memcpy(dest, b, dest_size);
		b += dest_size;
		i1 += dest_nmemb;
	}
	q = i2next - i1;
	if (q > 0) {
		/* Safe because q is always < dest_nmemb */
		memcpy(dest, b, q * size);
		warning("number of items to replace is not a multiple "
			"of replacement length");
	}
	return;
}


/* ==========================================================================
 * Memory copy:
 *   dest[k % dest_nmemb] <- src[subset[k] - 1] for 0 <= k <= n
 * --------------------------------------------------------------------------
 * Reads from the members of 'src' that have the offsets passed in 'subset'.
 * Writing is recycled in 'dest': it starts at its first member
 * and comes back to it after it reaches its last member.
 */
void _IRanges_memcpy_from_subset(const int *subset, int n,
		char *dest, size_t dest_nmemb,
		const char *src, size_t src_nmemb, size_t size)
{
	char *a;
        const char *b;
	int i, j, k, z;

	if (dest_nmemb == 0 && n != 0)
		error("no destination to copy to");
	a = dest;
	for (i = k = 0; k < n; i++, k++) {
		j = subset[k] - 1;
		if (j < 0 || j >= src_nmemb)
			error("subscript out of bounds");
		if (i >= dest_nmemb) {
			i = 0; /* recycle */
			a = dest;
		}
		b = src + j * size;
		for (z = 0; z < size; z++) {
			*(a++) = *(b++);
		}
	}
	if (i != dest_nmemb)
		warning("number of items to replace is not a multiple "
			"of replacement length");
	return;
}


/* ==========================================================================
 * Memory copy:
 *   dest[i] <- src[(i-i1) % src_nmemb] for i1 <= i <= i2
 * --------------------------------------------------------------------------
 * Writes to a linear subset of 'dest' defined by 'i1', 'i2'.
 * Reading is recycled in 'src': it starts at its first member
 * and comes back to it after it reaches its last member.
 * Don't do anything if i1 > i2.
 */
void _IRanges_memcpy_to_i1i2(int i1, int i2,
		char *dest, size_t dest_nmemb,
		const char *src, size_t src_nmemb, size_t size)
{
	char *a;
	int i2next, i1max, q;
	size_t src_size;

	if (i1 > i2)
		return;
	if (i1 < 0 || i2 >= dest_nmemb)
		error("subscript out of bounds");
	if (src_nmemb == 0)
		error("no value provided");
	i2next = i2 + 1;
	i1max = i2next - src_nmemb;
	a = dest + i1 * size;
	src_size = src_nmemb * size;
	while (i1 <= i1max) {
		memcpy(a, src, src_size);
		a += src_size;
		i1 += src_nmemb;
	}
	q = i2next - i1;
	if (q > 0) {
		/* Safe because q is always < src_nmemb */
		memcpy(a, src, q * size);
		warning("number of items to replace is not a multiple "
			"of replacement length");
	}
	return;
}


/* ==========================================================================
 * Memory copy:
 *   dest[subset[k] - 1] <- src[k % src_nmemb] for 0 <= k <= n
 * --------------------------------------------------------------------------
 * Writes to the members of 'dest' that have the offsets passed in 'subset'.
 * Reading is recycled in 'src': it starts at its first member
 * and comes back to it after it reaches its last member.
 */
void _IRanges_memcpy_to_subset(const int *subset, int n,
		char *dest, size_t dest_nmemb,
		const char *src, size_t src_nmemb, size_t size)
{
	char *a;
        const char *b;
	int i, j, k, z;

	if (src_nmemb == 0 && n != 0)
		error("no value provided");
	b = src;
	for (j = k = 0; k < n; j++, k++) {
		i = subset[k] - 1;
		if (i < 0 || i >= dest_nmemb)
			error("subscript out of bounds");
		if (j >= src_nmemb) {
			j = 0; /* recycle */
			b = src;
		}
		a = dest + i * size;
		for (z = 0; z < size; z++) {
			*(a++) = *(b++);
		}
	}
	if (j != src_nmemb)
		warning("number of items to replace is not a multiple "
			"of replacement length");
	return;
}
