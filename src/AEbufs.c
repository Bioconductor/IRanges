/*
 * Low-level manipulation of the Auto-Extending buffers.
 *
 * Except for debug_AEbufs(), the functions defined in this file are
 * NOT .Call methods (but they are used by .Call methods defined in other .c
 * files). They are prefixed with a "_" (underscore) to emphasize the fact that
 * they are used internally within the IRanges shared lib.
 */
#include "IRanges.h"
#include <stdlib.h> /* for qsort() */
#include <S.h> /* for Salloc() and Srealloc() */

#define MAX_BUFLENGTH_INC (128 * 1024 * 1024)
#define MAX_BUFLENGTH (8 * MAX_BUFLENGTH_INC)


static int debug = 0;

SEXP debug_AEbufs()
{
#ifdef DEBUG_IRANGES
	debug = !debug;
	Rprintf("Debug mode turned %s in 'AEbufs.c'\n", debug ? "on" : "off");
#else
	Rprintf("Debug mode not available in 'AEbufs.c'\n");
#endif
	return R_NilValue;
}

/*
 * Get the order of an array of ints.
 */
static int cmpintpp(const void *p1, const void *p2)
{
	int *i1, *i2;

	i1 = *((int **) p1);
	i2 = *((int **) p2);
	if (*i1 < *i2)
		return -1;
	if (*i1 > *i2)
		return 1;
	return 0;
}
void _get_intorder(int len, const int *in, int *out)
{
	const int **inp, *tmp0, **tmp1;
	int k, *tmp2;

	inp = (const int **) malloc(len * sizeof(const int *));
	if (inp == NULL)
		error("Biostrings internal error in intorder(): malloc failed");
	for (k = 0, tmp0 = in, tmp1 = inp; k < len; k++, tmp0++, tmp1++)
		*tmp1 = tmp0;
	qsort(inp, len, sizeof(int *), cmpintpp);
	for (k = 0, tmp1 = inp, tmp2 = out; k < len; k++, tmp1++, tmp2++)
		*tmp2 = *tmp1 - in;
	free(inp);
	return;
}

static int get_new_buflength(int buflength)
{
	if (buflength >= MAX_BUFLENGTH)
		error("get_new_buflength(): MAX_BUFLENGTH reached");
	if (buflength == 0)
		return 256;
	if (buflength <= 256 * 1024)
		return 4 * buflength;
	if (buflength <= MAX_BUFLENGTH_INC)
		return 2 * buflength;
	buflength += MAX_BUFLENGTH_INC;
	if (buflength <= MAX_BUFLENGTH)
		return buflength;
	return MAX_BUFLENGTH;
}


/****************************************************************************
 * IntAE functions
 */

void _IntAE_set_val(const IntAE *ibuf, int val)
{
	int i, *elt;

	for (i = 0, elt = ibuf->elts; i < ibuf->nelt; i++, elt++)
		*elt = val;
	return;
}

IntAE _new_IntAE(int buflength, int nelt, int val)
{
	IntAE ibuf;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		ibuf.elts = NULL;
	else
		ibuf.elts = Salloc((long) buflength, int);
	ibuf.buflength = buflength;
	ibuf.nelt = nelt;
	_IntAE_set_val(&ibuf, val);
	return ibuf;
}

static void IntAE_extend(IntAE *ibuf)
{
	long new_buflength;

	new_buflength = get_new_buflength(ibuf->buflength);
	ibuf->elts = Srealloc((char *) ibuf->elts, new_buflength,
					(long) ibuf->buflength, int);
	ibuf->buflength = new_buflength;
	return;
}

void _IntAE_insert_at(IntAE *ibuf, int at, int val)
{
	const int *elt1;
	int *elt2;
	int i1;

	if (ibuf->nelt >= ibuf->buflength)
		IntAE_extend(ibuf);
	elt2 = ibuf->elts + ibuf->nelt;
	elt1 = elt2 - 1;
	for (i1 = ibuf->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = val;
	return;
}

void _IntAE_append(IntAE *ibuf, const int *newvals, int nnewval)
{
	int new_nelt, *dest;

	new_nelt = ibuf->nelt + nnewval;
	while (ibuf->buflength < new_nelt)
		IntAE_extend(ibuf);
	dest = ibuf->elts + ibuf->nelt;
	memcpy(dest, newvals, nnewval * sizeof(int));
	ibuf->nelt = new_nelt;
	return;
}

void _IntAE_delete_at(IntAE *ibuf, int at)
{
	int *elt1;
	const int *elt2;
	int i2;

	elt1 = ibuf->elts + at;
	elt2 = elt1 + 1;
	for (i2 = at + 1; i2 < ibuf->nelt; i2++)
		*(elt1++) = *(elt2++);
	ibuf->nelt--;
	return;
}

void _IntAE_sum_val(const IntAE *ibuf, int val)
{
	int i, *elt;

	for (i = 0, elt = ibuf->elts; i < ibuf->nelt; i++, elt++)
		*elt += val;
	return;
}

void _IntAE_append_shifted_vals(IntAE *ibuf, const int *newvals, int nnewval, int shift)
{
	int new_nelt, i, *elt1;
	const int *elt2;

	new_nelt = ibuf->nelt + nnewval;
	while (ibuf->buflength < new_nelt)
		IntAE_extend(ibuf);
	for (i = 0, elt1 = ibuf->elts + ibuf->nelt, elt2 = newvals;
	     i < nnewval;
	     i++, elt1++, elt2++)
		*elt1 = *elt2 + shift;
	ibuf->nelt = new_nelt;
	return;
}

/*
 * Left and right IntAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAE_sum_IntAE(const IntAE *ibuf1, const IntAE *ibuf2)
{
	int i, *elt1, *elt2;

	for (i = 0, elt1 = ibuf1->elts, elt2 = ibuf2->elts;
	     i < ibuf1->nelt;
	     i++, elt1++, elt2++)
		*elt1 += *elt2;
	return;
}

static int cmpintp(const void *p1, const void *p2)
{
	return *((const int *) p1) - *((const int *) p2);
}

void _IntAE_qsort(IntAE *ibuf)
{
	qsort(ibuf->elts, ibuf->nelt, sizeof(int), cmpintp);
	return;
}

void _IntAE_delete_consecutiverepeats(IntAE *ibuf)
{
	int *elt1;
	const int *elt2;
	int i2;

	if (ibuf->nelt <= 1)
		return;
	elt1 = ibuf->elts;
	elt2 = elt1 + 1;
	for (i2 = 1; i2 < ibuf->nelt; i2++) {
		if (*elt2 != *elt1) {
			elt1++;
			*elt1 = *elt2;
		}
		elt2++;
	}
	ibuf->nelt = elt1 - ibuf->elts + 1;
	return;
}

SEXP _IntAE_asINTEGER(const IntAE *ibuf)
{
	SEXP ans;

	PROTECT(ans = NEW_INTEGER(ibuf->nelt));
	memcpy(INTEGER(ans), ibuf->elts, sizeof(int) * ibuf->nelt);
	UNPROTECT(1);
	return ans;
}

IntAE _INTEGER_asIntAE(SEXP x)
{
	IntAE ibuf;

	ibuf = _new_IntAE(LENGTH(x), 0, 0);
	memcpy(ibuf.elts, INTEGER(x), sizeof(int) * LENGTH(x));
	ibuf.nelt = ibuf.buflength;
	return ibuf;
}

IntAE _CHARACTER_asIntAE(SEXP x, int keyshift)
{
	IntAE ibuf;
	int *elt;

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CHARACTER_asIntAE(): BEGIN ... "
			"LENGTH(x)=%d keyshift=%d\n",
			LENGTH(x), keyshift);
	}
#endif
	ibuf = _new_IntAE(LENGTH(x), 0, 0);
	for (ibuf.nelt = 0, elt = ibuf.elts;
	     ibuf.nelt < ibuf.buflength;
	     ibuf.nelt++, elt++) {
		sscanf(CHAR(STRING_ELT(x, ibuf.nelt)), "%d", elt);
		*elt += keyshift;
#ifdef DEBUG_IRANGES
		if (debug) {
			if (ibuf.nelt < 100
			 || ibuf.nelt >= ibuf.buflength - 100)
				Rprintf("[DEBUG] _CHARACTER_asIntAE(): "
					"ibuf.nelt=%d key=%s *elt=%d\n",
					ibuf.nelt,
					CHAR(STRING_ELT(x, ibuf.nelt)), *elt);
		}
#endif
	}
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CHARACTER_asIntAE(): END\n");
	}
#endif
	return ibuf;
}


/****************************************************************************
 * IntAEAE functions
 */

IntAEAE _new_IntAEAE(int buflength, int nelt)
{
	IntAEAE ibbuf;
	IntAE *elt;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		ibbuf.elts = NULL;
	else
		ibbuf.elts = Salloc((long) buflength, IntAE);
	ibbuf.buflength = buflength;
	for (ibbuf.nelt = 0, elt = ibbuf.elts;
	     ibbuf.nelt < nelt;
	     ibbuf.nelt++, elt++)
		*elt = _new_IntAE(0, 0, 0);
	return ibbuf;
}

static void IntAEAE_extend(IntAEAE *ibbuf)
{
	long new_buflength;

	new_buflength = get_new_buflength(ibbuf->buflength);
	ibbuf->elts = Srealloc((char *) ibbuf->elts, new_buflength,
					(long) ibbuf->buflength, IntAE);
	ibbuf->buflength = new_buflength;
	return;
}

void _IntAEAE_insert_at(IntAEAE *ibbuf, int at, const IntAE *ibuf)
{
	IntAE *elt1, *elt2;
	int i1;

	if (ibbuf->nelt >= ibbuf->buflength)
		IntAEAE_extend(ibbuf);
	elt2 = ibbuf->elts + ibbuf->nelt;
	elt1 = elt2 - 1;
	for (i1 = ibbuf->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = *ibuf;
	return;
}

/*
 * Left and right IntAEAE objects must have the same length. This is
 * NOT checked!
 */
void _IntAEAE_eltwise_append(const IntAEAE *ibbuf1, const IntAEAE *ibbuf2)
{
	int i;
	IntAE *elt1, *elt2;

	for (i = 0, elt1 = ibbuf1->elts, elt2 = ibbuf2->elts;
	     i < ibbuf1->nelt;
	     i++, elt1++, elt2++)
		_IntAE_append(elt1, elt2->elts, elt2->nelt);
	return;
}

void _IntAEAE_sum_val(const IntAEAE *ibbuf, int val)
{
	int i;
	IntAE *elt;

	for (i = 0, elt = ibbuf->elts; i < ibbuf->nelt; i++, elt++)
		_IntAE_sum_val(elt, val);
	return;
}

/*
 * mode: 0 -> integer(0), 1-> NULL, 2 -> NA
 */
SEXP _IntAEAE_asLIST(const IntAEAE *ibbuf, int mode)
{
	SEXP ans, ans_elt;
	int i;
	IntAE *elt;

	PROTECT(ans = NEW_LIST(ibbuf->nelt));
	for (i = 0, elt = ibbuf->elts; i < ibbuf->nelt; i++, elt++) {
		if (elt->nelt == 0 && mode != 0) {
			if (mode == 1) {
				PROTECT(ans_elt = R_NilValue);
			} else {
				// Not sure new LOGICALs are initialized with NAs,
				// need to check! If not, then LOGICAL(ans_elt)[0]
				// must be set to NA but I don't know how to do this :-/
				PROTECT(ans_elt = NEW_LOGICAL(1));
			}
		} else {
			PROTECT(ans_elt = _IntAE_asINTEGER(elt));
		}
		SET_ELEMENT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

IntAEAE _LIST_asIntAEAE(SEXP x)
{
	IntAEAE ibbuf;
	IntAE *elt;

	ibbuf = _new_IntAEAE(LENGTH(x), 0);
	for (ibbuf.nelt = 0, elt = ibbuf.elts;
	     ibbuf.nelt < ibbuf.buflength;
	     ibbuf.nelt++, elt++) {
		*elt = _INTEGER_asIntAE(VECTOR_ELT(x, ibbuf.nelt));
	}
	return ibbuf;
}

SEXP _IntAEAE_toEnvir(const IntAEAE *ibbuf, SEXP envir, int keyshift)
{
	int i;
	IntAE *elt;
	char key[11];
	SEXP value;

#ifdef DEBUG_IRANGES
	int nkey = 0, cum_length = 0;
	if (debug) {
		Rprintf("[DEBUG] _IntAEAE_toEnvir(): BEGIN ... "
			"ibbuf->nelt=%d keyshift=%d\n",
			ibbuf->nelt, keyshift);
	}
#endif
	for (i = 0, elt = ibbuf->elts; i < ibbuf->nelt; i++, elt++) {
#ifdef DEBUG_IRANGES
		if (debug) {
			if (i < 100 || i >= ibbuf->nelt - 100)
				Rprintf("[DEBUG] _IntAEAE_toEnvir(): "
					"nkey=%d ibbuf->elts[%d].nelt=%d\n",
					nkey, i, elt->nelt);
		}
#endif
		if (elt->nelt == 0)
			continue;
		//snprintf(key, sizeof(key), "%d", i + keyshift);
		snprintf(key, sizeof(key), "%010d", i + keyshift);
#ifdef DEBUG_IRANGES
		if (debug) {
			if (i < 100 || i >= ibbuf->nelt - 100)
				Rprintf("[DEBUG] _IntAEAE_toEnvir(): "
					"installing key=%s ... ", key);
		}
#endif
		PROTECT(value = _IntAE_asINTEGER(elt));
		defineVar(install(key), value, envir);
		UNPROTECT(1);
#ifdef DEBUG_IRANGES
		if (debug) {
			nkey++;
			cum_length += elt->nelt;
			if (i < 100 || i >= ibbuf->nelt - 100)
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
 * RangeAE functions
 */

RangeAE _new_RangeAE(int buflength, int nelt)
{
	RangeAE rangebuf;

	rangebuf.start = _new_IntAE(buflength, nelt, 0);
	rangebuf.width = _new_IntAE(buflength, nelt, 0);
	return rangebuf;
}

void _RangeAE_insert_at(RangeAE *rangebuf, int at, int start, int width)
{
	_IntAE_insert_at(&(rangebuf->start), at, start);
	_IntAE_insert_at(&(rangebuf->width), at, width);
	return;
}


/****************************************************************************
 * CharAE functions
 */

CharAE _new_CharAE(int buflength)
{
	CharAE cbuf;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		cbuf.elts = NULL;
	else
		cbuf.elts = Salloc((long) buflength, char);
	cbuf.buflength = buflength;
	cbuf.nelt = 0;
	return cbuf;
}

CharAE _new_CharAE_from_string(const char *string)
{
	CharAE cbuf;
	int buflength;

	buflength = strlen(string);
	cbuf = _new_CharAE(buflength);
	memcpy(cbuf.elts, string, buflength);
	cbuf.nelt = buflength;
	return cbuf;
}

static void CharAE_extend(CharAE *cbuf)
{
	long new_buflength;

	new_buflength = get_new_buflength(cbuf->buflength);
	cbuf->elts = Srealloc((char *) cbuf->elts, new_buflength,
					(long) cbuf->buflength, char);
	cbuf->buflength = new_buflength;
	return;
}

void _CharAE_insert_at(CharAE *cbuf, int at, char c)
{
	char *elt1, *elt2;
	int i1;

	if (cbuf->nelt >= cbuf->buflength)
		CharAE_extend(cbuf);
	elt2 = cbuf->elts + cbuf->nelt;
	elt1 = elt2 - 1;
	for (i1 = cbuf->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = c;
	return;
}

SEXP _CharAE_asRAW(const CharAE *cbuf)
{
	SEXP ans;

	if (sizeof(Rbyte) != sizeof(char)) // should never happen!
		error("_CharAE_asRAW(): sizeof(Rbyte) != sizeof(char)");
	PROTECT(ans = NEW_RAW(cbuf->nelt));
	memcpy(RAW(ans), cbuf->elts, sizeof(char) * cbuf->nelt);
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * CharAEAE functions
 */

CharAEAE _new_CharAEAE(int buflength, int nelt)
{
	CharAEAE cbbuf;
	CharAE *elt;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		cbbuf.elts = NULL;
	else
		cbbuf.elts = Salloc((long) buflength, CharAE);
	cbbuf.buflength = buflength;
	for (cbbuf.nelt = 0, elt = cbbuf.elts;
	     cbbuf.nelt < nelt;
	     cbbuf.nelt++, elt++)
		*elt = _new_CharAE(0);
	return cbbuf;
}

static void CharAEAE_extend(CharAEAE *cbbuf)
{
	long new_buflength;

	new_buflength = get_new_buflength(cbbuf->buflength);
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] CharAEAE_extend(): BEGIN\n");
		Rprintf("[DEBUG] CharAEAE_extend(): "
			"cbbuf->elts=%p buflength=%d new_buflength=%d\n",
			cbbuf->elts, cbbuf->buflength, new_buflength);
	}
#endif
	cbbuf->elts = Srealloc((char *) cbbuf->elts, new_buflength,
				(long) cbbuf->buflength, CharAE);
	cbbuf->buflength = new_buflength;
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] CharAEAE_extend(): END (cbbuf->elts=%p)\n",
			cbbuf->elts);
	}
#endif
	return;
}

void _CharAEAE_insert_at(CharAEAE *cbbuf, int at, const CharAE *cbuf)
{
	CharAE *elt1, *elt2;
	int i1;

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CharAEAE_insert_at(): BEGIN\n");
	}
#endif
	if (cbbuf->nelt >= cbbuf->buflength)
		CharAEAE_extend(cbbuf);
	elt2 = cbbuf->elts + cbbuf->nelt;
	elt1 = elt2 - 1;
	for (i1 = cbbuf->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = *cbuf;
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CharAEAE_insert_at(): END\n");
	}
#endif
	return;
}

void _append_string_to_CharAEAE(CharAEAE *cbbuf, const char *string)
{
	CharAE cbuf;

	cbuf = _new_CharAE_from_string(string);
	_CharAEAE_insert_at(cbbuf, cbbuf->nelt, &cbuf);
	return;
}

