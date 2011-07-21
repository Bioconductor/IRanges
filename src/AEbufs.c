/****************************************************************************
 *           Low-level manipulation of the Auto-Extending buffers           *
 *           ----------------------------------------------------           *
 ****************************************************************************/
#include "IRanges.h"
#include <stdlib.h>  /* for malloc, free, realloc */

#define MAX_BUFLENGTH_INC (32 * 1024 * 1024)
#define MAX_BUFLENGTH (32 * MAX_BUFLENGTH_INC)


static int debug = 0;

SEXP debug_AEbufs()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in file %s\n",
		debug ? "on" : "off", __FILE__);
#else
	Rprintf("Debug mode not available in file %s\n", __FILE__);
#endif
	return R_NilValue;
}


/****************************************************************************
 * Management of the malloc-based AEbufs.
 */

static int use_malloc = 0;

SEXP AEbufs_use_malloc()
{
	return ScalarLogical(use_malloc = !use_malloc);
}

/* Types of AEbufs */
#define	INTAE		1
#define	INTAEAE		2
#define	RANGEAE		3
#define	RANGEAEAE	4
#define	CHARAE		5
#define	CHARAEAE	6

/*
 * The "global AEbuf stack" stores pointers to all the malloc-based AEbufs
 * that are created during the execution of a .Call entry point. Every .Call()
 * should start with an empty stack. After the .Call() has returned, the stack
 * must be emptied with '.Call("AEbufs_free", PACKAGE="IRanges")'.
 */
#define AEBUF_STACK_MAXNELT 2048

typedef struct aebuf_ptr {
	int type;
	void *ptr;
} AEbufPtr;

static AEbufPtr AEbuf_stack[AEBUF_STACK_MAXNELT];

static int AEbuf_stack_nelt = 0;

static void push_AEbuf(int type, void *ptr)
{
	AEbufPtr *aebuf_ptr;

	if (AEbuf_stack_nelt >= AEBUF_STACK_MAXNELT)
		error("IRanges internal error in push_AEbuf(): "
		      "AEbuf stack is full");
	aebuf_ptr = AEbuf_stack + AEbuf_stack_nelt;
	aebuf_ptr->type = type;
	aebuf_ptr->ptr = ptr;
	AEbuf_stack_nelt++;
	return;
}


/****************************************************************************
 * Core helper functions used for allocation/reallocation of the AEbufs.
 */

/* Guaranteed to return a new buflength > 'buflength', or to raise an error. */
int _get_new_buflength(int buflength)
{
	if (buflength >= MAX_BUFLENGTH)
		error("_get_new_buflength(): MAX_BUFLENGTH reached");
	if (buflength == 0)
		return 128;
	if (buflength <= MAX_BUFLENGTH_INC)
		return 2 * buflength;
	buflength += MAX_BUFLENGTH_INC;
	if (buflength <= MAX_BUFLENGTH)
		return buflength;
	return MAX_BUFLENGTH;
}

static void *malloc_AEbuf(int buflength, size_t size)
{
	void *elts;

	if (buflength == 0)
		return NULL;
	elts = malloc((size_t) buflength * size);
	if (elts == NULL)
		error("IRanges internal error in malloc_AEbuf(): "
		      "cannot allocate memory");
	return elts;
}

static void *alloc_AEbuf(int buflength, size_t size)
{
	if (use_malloc)
		return malloc_AEbuf(buflength, size);
	if (buflength == 0)
		return NULL;
	return (void *) R_alloc(buflength, size);
}

static void *realloc_AEbuf(void *elts, int new_buflength,
		int buflength, size_t size)
{
	void *new_elts;

	if (use_malloc) {
		new_elts = realloc(elts, (size_t) new_buflength * size);
		if (new_elts == NULL)
			error("IRanges internal error in realloc_AEbuf(): "
			      "cannot reallocate memory");
		return new_elts;
	}
	new_elts = (void *) R_alloc(new_buflength, size);
	return memcpy(new_elts, elts, (size_t) buflength * size);
}


/****************************************************************************
 * IntAE buffers
 */

static void IntAE_alloc(IntAE *int_ae, int buflength)
{
	int_ae->elts = (int *) alloc_AEbuf(buflength, sizeof(int));
	int_ae->buflength = buflength;
	return;
}

/* Must be used on a malloc-based IntAE */
static void IntAE_free(IntAE *int_ae)
{
	if (int_ae->elts != NULL)
		free(int_ae->elts);
	return;
}

void _IntAE_set_val(const IntAE *int_ae, int val)
{
	int i, *elt;

	for (i = 0, elt = int_ae->elts; i < int_ae->nelt; i++, elt++)
		*elt = val;
	return;
}

IntAE _new_IntAE(int buflength, int nelt, int val)
{
	IntAE int_ae;

	/* Allocation */
	IntAE_alloc(&int_ae, buflength);
	if (use_malloc)
		push_AEbuf(INTAE, &int_ae);
	/* Initialization */
	int_ae.nelt = nelt;
	_IntAE_set_val(&int_ae, val);
	return int_ae;
}

static void IntAE_extend(IntAE *int_ae)
{
	int new_buflength;

	new_buflength = _get_new_buflength(int_ae->buflength);
	int_ae->elts = (int *) realloc_AEbuf(int_ae->elts, new_buflength,
					int_ae->buflength, sizeof(int));
	int_ae->buflength = new_buflength;
	return;
}

void _IntAE_insert_at(IntAE *int_ae, int at, int val)
{
	const int *elt1;
	int *elt2;
	int i1;

	if (int_ae->nelt >= int_ae->buflength)
		IntAE_extend(int_ae);
	elt2 = int_ae->elts + int_ae->nelt;
	elt1 = elt2 - 1;
	for (i1 = int_ae->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = val;
	return;
}

void _IntAE_append(IntAE *int_ae, const int *newvals, int nnewval)
{
	int new_nelt, *dest;

	new_nelt = int_ae->nelt + nnewval;
	while (int_ae->buflength < new_nelt)
		IntAE_extend(int_ae);
	dest = int_ae->elts + int_ae->nelt;
	memcpy(dest, newvals, nnewval * sizeof(int));
	int_ae->nelt = new_nelt;
	return;
}

void _IntAE_delete_at(IntAE *int_ae, int at)
{
	int *elt1;
	const int *elt2;
	int i2;

	elt1 = int_ae->elts + at;
	elt2 = elt1 + 1;
	for (i2 = at + 1; i2 < int_ae->nelt; i2++)
		*(elt1++) = *(elt2++);
	int_ae->nelt--;
	return;
}

void _IntAE_shift(const IntAE *int_ae, int shift)
{
	int i, *elt;

	for (i = 0, elt = int_ae->elts; i < int_ae->nelt; i++, elt++)
		*elt += shift;
	return;
}

/*
 * Left and right IntAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAE_sum_and_shift(const IntAE *int_ae1, const IntAE *int_ae2, int shift)
{
	int i, *elt1, *elt2;

	for (i = 0, elt1 = int_ae1->elts, elt2 = int_ae2->elts;
	     i < int_ae1->nelt;
	     i++, elt1++, elt2++)
		*elt1 += *elt2 + shift;
	return;
}

void _IntAE_append_shifted_vals(IntAE *int_ae, const int *newvals, int nnewval, int shift)
{
	int new_nelt, i, *elt1;
	const int *elt2;

	new_nelt = int_ae->nelt + nnewval;
	while (int_ae->buflength < new_nelt)
		IntAE_extend(int_ae);
	for (i = 0, elt1 = int_ae->elts + int_ae->nelt, elt2 = newvals;
	     i < nnewval;
	     i++, elt1++, elt2++)
		*elt1 = *elt2 + shift;
	int_ae->nelt = new_nelt;
	return;
}

void _IntAE_qsort(IntAE *int_ae, int desc)
{
	_sort_int_array(int_ae->elts, int_ae->nelt, desc);
	return;
}

void _IntAE_delete_adjdups(IntAE *int_ae)
{
	int *elt1;
	const int *elt2;
	int i2;

	if (int_ae->nelt <= 1)
		return;
	elt1 = int_ae->elts;
	elt2 = elt1 + 1;
	for (i2 = 1; i2 < int_ae->nelt; i2++) {
		if (*elt2 != *elt1) {
			elt1++;
			*elt1 = *elt2;
		}
		elt2++;
	}
	int_ae->nelt = elt1 - int_ae->elts + 1;
	return;
}

SEXP _new_INTEGER_from_IntAE(const IntAE *int_ae)
{
	SEXP ans;

	PROTECT(ans = NEW_INTEGER(int_ae->nelt));
	memcpy(INTEGER(ans), int_ae->elts, sizeof(int) * int_ae->nelt);
	UNPROTECT(1);
	return ans;
}

static void copy_INTEGER_to_IntAE(SEXP x, IntAE *int_ae)
{
	int_ae->nelt = LENGTH(x);
	memcpy(int_ae->elts, INTEGER(x), sizeof(int) * LENGTH(x));
	return;
}

IntAE _new_IntAE_from_INTEGER(SEXP x)
{
	IntAE int_ae;

	int_ae = _new_IntAE(LENGTH(x), 0, 0);
	copy_INTEGER_to_IntAE(x, &int_ae);
	return int_ae;
}

IntAE _new_IntAE_from_CHARACTER(SEXP x, int keyshift)
{
	IntAE int_ae;
	int *elt;

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _new_IntAE_from_CHARACTER(): BEGIN ... "
			"LENGTH(x)=%d keyshift=%d\n",
			LENGTH(x), keyshift);
	}
#endif
	int_ae = _new_IntAE(LENGTH(x), 0, 0);
	for (int_ae.nelt = 0, elt = int_ae.elts;
	     int_ae.nelt < int_ae.buflength;
	     int_ae.nelt++, elt++) {
		sscanf(CHAR(STRING_ELT(x, int_ae.nelt)), "%d", elt);
		*elt += keyshift;
#ifdef DEBUG_IRANGES
		if (debug) {
			if (int_ae.nelt < 100
			 || int_ae.nelt >= int_ae.buflength - 100)
				Rprintf("[DEBUG] _new_IntAE_from_CHARACTER(): "
					"int_ae.nelt=%d key=%s *elt=%d\n",
					int_ae.nelt,
					CHAR(STRING_ELT(x, int_ae.nelt)), *elt);
		}
#endif
	}
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _new_IntAE_from_CHARACTER(): END\n");
	}
#endif
	return int_ae;
}


/****************************************************************************
 * IntAEAE buffers
 */

static void IntAEAE_alloc(IntAEAE *int_aeae, int buflength)
{
	int_aeae->elts = (IntAE *) alloc_AEbuf(buflength, sizeof(IntAE));
	int_aeae->buflength = buflength;
	return;
}

/* Must be used on a malloc-based IntAEAE */
static void IntAEAE_free(IntAEAE *int_aeae)
{
	int i;
	IntAE *elt;

	for (i = 0, elt = int_aeae->elts; i < int_aeae->nelt; i++, elt++)
		IntAE_free(elt);
	if (int_aeae->elts != NULL)
		free(int_aeae->elts);
	return;
}

IntAEAE _new_IntAEAE(int buflength, int nelt)
{
	IntAEAE int_aeae;
	int i;
	IntAE *elt;

	/* Allocation */
	IntAEAE_alloc(&int_aeae, buflength);
	if (use_malloc)
		push_AEbuf(INTAEAE, &int_aeae);
	/* Initialization */
	int_aeae.nelt = nelt;
	for (i = 0, elt = int_aeae.elts; i < nelt; i++, elt++) {
		IntAE_alloc(elt, 0);
		elt->nelt = 0;
	}
	return int_aeae;
}

static void IntAEAE_extend(IntAEAE *int_aeae)
{
	int new_buflength;

	new_buflength = _get_new_buflength(int_aeae->buflength);
	int_aeae->elts = (IntAE *) realloc_AEbuf(int_aeae->elts, new_buflength,
					int_aeae->buflength, sizeof(IntAE));
	int_aeae->buflength = new_buflength;
	return;
}

void _IntAEAE_insert_at(IntAEAE *int_aeae, int at, const IntAE *int_ae)
{
	IntAE *elt1, *elt2;
	int i1;

	if (int_aeae->nelt >= int_aeae->buflength)
		IntAEAE_extend(int_aeae);
	elt2 = int_aeae->elts + int_aeae->nelt;
	elt1 = elt2 - 1;
	for (i1 = int_aeae->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = *int_ae;
	return;
}

/*
 * Left and right IntAEAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAEAE_eltwise_append(const IntAEAE *int_aeae1, const IntAEAE *int_aeae2)
{
	int i;
	IntAE *elt1, *elt2;

	for (i = 0, elt1 = int_aeae1->elts, elt2 = int_aeae2->elts;
	     i < int_aeae1->nelt;
	     i++, elt1++, elt2++)
		_IntAE_append(elt1, elt2->elts, elt2->nelt);
	return;
}

void _IntAEAE_shift(const IntAEAE *int_aeae, int shift)
{
	int i;
	IntAE *elt;

	for (i = 0, elt = int_aeae->elts; i < int_aeae->nelt; i++, elt++)
		_IntAE_shift(elt, shift);
	return;
}

/*
 * Left and right IntAEAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAEAE_sum_and_shift(const IntAEAE *int_aeae1, const IntAEAE *int_aeae2, int shift)
{
	int i;
	IntAE *elt1, *elt2;

	for (i = 0, elt1 = int_aeae1->elts, elt2 = int_aeae2->elts;
	     i < int_aeae1->nelt;
	     i++, elt1++, elt2++)
		_IntAE_sum_and_shift(elt1, elt2, shift);
	return;
}

/*
 * 'mode' controls how empty list elements should be represented:
 *   0 -> integer(0); 1 -> NULL; 2 -> NA
 */
SEXP _new_LIST_from_IntAEAE(const IntAEAE *int_aeae, int mode)
{
	SEXP ans, ans_elt;
	int i;
	const IntAE *elt;

	PROTECT(ans = NEW_LIST(int_aeae->nelt));
	for (i = 0, elt = int_aeae->elts; i < int_aeae->nelt; i++, elt++) {
		if (elt->nelt != 0 || mode == 0) {
			PROTECT(ans_elt = _new_INTEGER_from_IntAE(elt));
		} else if (mode == 1) {
			continue;
		} else {
			// Not sure new LOGICALs are initialized with NAs,
			// need to check! If not, then LOGICAL(ans_elt)[0] must
			// be set to NA but I don't know how to do this :-/
			PROTECT(ans_elt = NEW_LOGICAL(1));
		}
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

IntAEAE _new_IntAEAE_from_LIST(SEXP x)
{
	IntAEAE int_aeae;
	int i;
	IntAE *elt;
	SEXP x_elt;

	int_aeae = _new_IntAEAE(LENGTH(x), 0);
	int_aeae.nelt = int_aeae.buflength;
	for (i = 0, elt = int_aeae.elts; i < int_aeae.nelt; i++, elt++) {
		x_elt = VECTOR_ELT(x, i);
		if (TYPEOF(x_elt) != INTSXP)
			error("IRanges internal error in "
			      "_new_IntAEAE_from_LIST(): "
			      "not all elements in the list "
			      "are integer vectors");
		IntAE_alloc(elt, LENGTH(x_elt));
		copy_INTEGER_to_IntAE(x_elt, elt);
	}
	return int_aeae;
}

SEXP _IntAEAE_toEnvir(const IntAEAE *int_aeae, SEXP envir, int keyshift)
{
	int i;
	IntAE *elt;
	char key[11];
	SEXP value;

#ifdef DEBUG_IRANGES
	int nkey = 0, cum_length = 0;
	if (debug) {
		Rprintf("[DEBUG] _IntAEAE_toEnvir(): BEGIN ... "
			"int_aeae->nelt=%d keyshift=%d\n",
			int_aeae->nelt, keyshift);
	}
#endif
	for (i = 0, elt = int_aeae->elts; i < int_aeae->nelt; i++, elt++) {
#ifdef DEBUG_IRANGES
		if (debug) {
			if (i < 100 || i >= int_aeae->nelt - 100)
				Rprintf("[DEBUG] _IntAEAE_toEnvir(): "
					"nkey=%d int_aeae->elts[%d].nelt=%d\n",
					nkey, i, elt->nelt);
		}
#endif
		if (elt->nelt == 0)
			continue;
		//snprintf(key, sizeof(key), "%d", i + keyshift);
		snprintf(key, sizeof(key), "%010d", i + keyshift);
#ifdef DEBUG_IRANGES
		if (debug) {
			if (i < 100 || i >= int_aeae->nelt - 100)
				Rprintf("[DEBUG] _IntAEAE_toEnvir(): "
					"installing key=%s ... ", key);
		}
#endif
		PROTECT(value = _new_INTEGER_from_IntAE(elt));
		defineVar(install(key), value, envir);
		UNPROTECT(1);
#ifdef DEBUG_IRANGES
		if (debug) {
			nkey++;
			cum_length += elt->nelt;
			if (i < 100 || i >= int_aeae->nelt - 100)
				Rprintf("OK (nkey=%d cum_length=%d)\n",
					nkey, cum_length);
		}
#endif
	}
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _IntAEAE_toEnvir(): END "
			"(nkey=%d cum_length=%d)\n", nkey, cum_length);
	}
#endif
	return envir;
}


/****************************************************************************
 * RangeAE buffers
 */

static void RangeAE_alloc(RangeAE *range_ae, int buflength)
{
	IntAE_alloc(&(range_ae->start), buflength);
	IntAE_alloc(&(range_ae->width), buflength);
	return;
}

/* Must be used on a malloc-based RangeAE */
static void RangeAE_free(RangeAE *range_ae)
{
	IntAE_free(&(range_ae->start));
	IntAE_free(&(range_ae->width));
	return;
}

RangeAE _new_RangeAE(int buflength, int nelt)
{
	RangeAE range_ae;

	/* Allocation */
	RangeAE_alloc(&range_ae, buflength);
	if (use_malloc)
		push_AEbuf(RANGEAE, &range_ae);
	/* There is NO initialization */
	range_ae.start.nelt = range_ae.width.nelt = nelt;
	return range_ae;
}

void _RangeAE_insert_at(RangeAE *range_ae, int at, int start, int width)
{
	_IntAE_insert_at(&(range_ae->start), at, start);
	_IntAE_insert_at(&(range_ae->width), at, width);
	return;
}


/****************************************************************************
 * RangeAEAE buffers
 */

static void RangeAEAE_alloc(RangeAEAE *range_aeae, int buflength)
{
	range_aeae->elts = (RangeAE *) alloc_AEbuf(buflength, sizeof(RangeAE));
	range_aeae->buflength = buflength;
	return;
}

/* Must be used on a malloc-based RangeAEAE */
static void RangeAEAE_free(RangeAEAE *range_aeae)
{
	int i;
	RangeAE *elt;

	for (i = 0, elt = range_aeae->elts; i < range_aeae->nelt; i++, elt++)
		RangeAE_free(elt);
	if (range_aeae->elts != NULL)
		free(range_aeae->elts);
	return;
}

RangeAEAE _new_RangeAEAE(int buflength, int nelt)
{
	RangeAEAE range_aeae;
	int i;
	RangeAE *elt;

	/* Allocation */
	RangeAEAE_alloc(&range_aeae, buflength);
	if (use_malloc)
		push_AEbuf(RANGEAEAE, &range_aeae);
	/* Initialization */
	range_aeae.nelt = nelt;
	for (i = 0, elt = range_aeae.elts; i < nelt; i++, elt++) {
		RangeAE_alloc(elt, 0);
		elt->start.nelt = elt->width.nelt = 0;
	}
	return range_aeae;
}

static void RangeAEAE_extend(RangeAEAE *range_aeae)
{
	int new_buflength;

	new_buflength = _get_new_buflength(range_aeae->buflength);
	range_aeae->elts = (RangeAE *) realloc_AEbuf(range_aeae->elts,
					new_buflength, range_aeae->buflength,
					sizeof(RangeAE));
	range_aeae->buflength = new_buflength;
	return;
}

void _RangeAEAE_insert_at(RangeAEAE *range_aeae, int at,
		const RangeAE *range_ae)
{
	RangeAE *elt1, *elt2;
	int i1;

	if (range_aeae->nelt >= range_aeae->buflength)
		RangeAEAE_extend(range_aeae);
	elt2 = range_aeae->elts + range_aeae->nelt;
	elt1 = elt2 - 1;
	for (i1 = range_aeae->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = *range_ae;
	return;
}


/****************************************************************************
 * CharAE buffers
 */

static void CharAE_alloc(CharAE *char_ae, int buflength)
{
	char_ae->elts = (char *) alloc_AEbuf(buflength, sizeof(char));
	char_ae->buflength = buflength;
	return;
}

/* Must be used on a malloc-based CharAE */
static void CharAE_free(CharAE *char_ae)
{
	if (char_ae->elts != NULL)
		free(char_ae->elts);
	return;
}

CharAE _new_CharAE(int buflength)
{
	CharAE char_ae;

	/* Allocation */
	CharAE_alloc(&char_ae, buflength);
	if (use_malloc)
		push_AEbuf(CHARAE, &char_ae);
	/* Initialization */
	char_ae.nelt = 0;
	return char_ae;
}

CharAE _new_CharAE_from_string(const char *string)
{
	CharAE char_ae;

	char_ae = _new_CharAE(strlen(string));
	char_ae.nelt = char_ae.buflength;
	memcpy(char_ae.elts, string, char_ae.nelt);
	return char_ae;
}

static void CharAE_extend(CharAE *char_ae)
{
	int new_buflength;

	new_buflength = _get_new_buflength(char_ae->buflength);
	char_ae->elts = (char *) realloc_AEbuf(char_ae->elts, new_buflength,
					char_ae->buflength, sizeof(char));
	char_ae->buflength = new_buflength;
	return;
}

void _CharAE_insert_at(CharAE *char_ae, int at, char c)
{
	char *elt1, *elt2;
	int i1;

	if (char_ae->nelt >= char_ae->buflength)
		CharAE_extend(char_ae);
	elt2 = char_ae->elts + char_ae->nelt;
	elt1 = elt2 - 1;
	for (i1 = char_ae->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = c;
	return;
}

void _append_string_to_CharAE(CharAE *char_ae, const char *string)
{
	int nnewval, new_nelt;
	char *dest;

	nnewval = strlen(string);
	new_nelt = char_ae->nelt + nnewval;
	while (char_ae->buflength < new_nelt)
		CharAE_extend(char_ae);
	dest = char_ae->elts + char_ae->nelt;
	memcpy(dest, string, nnewval * sizeof(char));
	char_ae->nelt = new_nelt;
	return;
}

SEXP _new_RAW_from_CharAE(const CharAE *char_ae)
{
	SEXP ans;

	if (sizeof(Rbyte) != sizeof(char)) // should never happen!
		error("_new_RAW_from_CharAE(): sizeof(Rbyte) != sizeof(char)");
	PROTECT(ans = NEW_RAW(char_ae->nelt));
	memcpy(RAW(ans), char_ae->elts, sizeof(char) * char_ae->nelt);
	UNPROTECT(1);
	return ans;
}

/* only until we have a bitset or something smaller than char */
SEXP _new_LOGICAL_from_CharAE(const CharAE *char_ae)
{
	SEXP ans;
	int i;

	PROTECT(ans = NEW_LOGICAL(char_ae->nelt));
	for (i = 0; i < char_ae->nelt; i++)
		LOGICAL(ans)[i] = char_ae->elts[i];
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * CharAEAE buffers
 */

static void CharAEAE_alloc(CharAEAE *char_aeae, int buflength)
{
	char_aeae->elts = (CharAE *) alloc_AEbuf(buflength, sizeof(CharAE));
	char_aeae->buflength = buflength;
	return;
}

/* Must be used on a malloc-based CharAEAE */
static void CharAEAE_free(CharAEAE *char_aeae)
{
	int i;
	CharAE *elt;

	for (i = 0, elt = char_aeae->elts; i < char_aeae->nelt; i++, elt++)
		CharAE_free(elt);
	if (char_aeae->elts != NULL)
		free(char_aeae->elts);
	return;
}

CharAEAE _new_CharAEAE(int buflength, int nelt)
{
	CharAEAE char_aeae;
	int i;
	CharAE *elt;

	/* Allocation */
	CharAEAE_alloc(&char_aeae, buflength);
	if (use_malloc)
		push_AEbuf(CHARAEAE, &char_aeae);
	/* Initialization */
	char_aeae.nelt = nelt;
	for (i = 0, elt = char_aeae.elts; i < nelt; i++, elt++) {
		CharAE_alloc(elt, 0);
		elt->nelt = 0;
	}
	return char_aeae;
}

static void CharAEAE_extend(CharAEAE *char_aeae)
{
	int new_buflength;

	new_buflength = _get_new_buflength(char_aeae->buflength);
	char_aeae->elts = (CharAE *) realloc_AEbuf(char_aeae->elts,
					new_buflength,
					char_aeae->buflength, sizeof(CharAE));
	char_aeae->buflength = new_buflength;
	return;
}

void _CharAEAE_insert_at(CharAEAE *char_aeae, int at, const CharAE *char_ae)
{
	CharAE *elt1, *elt2;
	int i1;

	if (char_aeae->nelt >= char_aeae->buflength)
		CharAEAE_extend(char_aeae);
	elt2 = char_aeae->elts + char_aeae->nelt;
	elt1 = elt2 - 1;
	for (i1 = char_aeae->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = *char_ae;
	return;
}

void _append_string_to_CharAEAE(CharAEAE *char_aeae, const char *string)
{
	CharAE char_ae;

	CharAE_alloc(&char_ae, strlen(string));
	char_ae.nelt = char_ae.buflength;
	memcpy(char_ae.elts, string, char_ae.nelt);
	_CharAEAE_insert_at(char_aeae, char_aeae->nelt, &char_ae);
	return;
}

SEXP _new_CHARACTER_from_CharAEAE(const CharAEAE *char_aeae)
{
	SEXP ans, ans_elt;
	int i;
	CharAE *elt;

	PROTECT(ans = NEW_CHARACTER(char_aeae->nelt));
	for (i = 0, elt = char_aeae->elts; i < char_aeae->nelt; i++, elt++) {
		PROTECT(ans_elt = mkCharLen(elt->elts, elt->nelt));
		SET_STRING_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Freeing the malloc-based AEbufs.
 */

static void pop_AEbuf()
{
	AEbufPtr *aebuf_ptr;

	if (AEbuf_stack_nelt <= 0)
		error("IRanges internal error in pop_AEbuf(): "
		      "cannot pop AEbuf from empty stack. "
		      "This should NEVER happen! Please report.");
	aebuf_ptr = AEbuf_stack + AEbuf_stack_nelt;
	switch (aebuf_ptr->type) {
	case INTAE:
		IntAE_free((IntAE *) aebuf_ptr->ptr);
		break;
	case INTAEAE:
		IntAEAE_free((IntAEAE *) aebuf_ptr->ptr);
		break;
	case RANGEAE:
		RangeAE_free((RangeAE *) aebuf_ptr->ptr);
		break;
	case RANGEAEAE:
		RangeAEAE_free((RangeAEAE *) aebuf_ptr->ptr);
		break;
	case CHARAE:
		CharAE_free((CharAE *) aebuf_ptr->ptr);
		break;
	case CHARAEAE:
		CharAEAE_free((CharAEAE *) aebuf_ptr->ptr);
		break;
	default: 
		error("IRanges internal error in pop_AEbuf(): "
		      "type of AEbuf not supported: %d. "
		      "This should NEVER happen! Please report.",
		      aebuf_ptr->type);
	}
	AEbuf_stack_nelt--;
	return;
}

SEXP AEbufs_free()
{
	while (AEbuf_stack_nelt > 0)
		pop_AEbuf();
	return R_NilValue;
}

