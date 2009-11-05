/*****************************************************************************
 * Low-level manipulation of the Auto-Extending buffers
 * ----------------------------------------------------
 * Except for debug_AEbufs(), the functions defined in this file are
 * NOT .Call methods (but they are used by .Call methods defined in other .c
 * files). They are prefixed with a "_" (underscore) to emphasize the fact that
 * they are used internally within the IRanges shared lib.
 */
#include "IRanges.h"
#include <S.h> /* for Salloc() and Srealloc() */

#define MAX_BUFLENGTH_INC (128 * 1024 * 1024)
#define MAX_BUFLENGTH (8 * MAX_BUFLENGTH_INC)


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

/* Guaranteed to return a new buflength > 'buflength', or to raise an error. */
int _get_new_buflength(int buflength)
{
	if (buflength >= MAX_BUFLENGTH)
		error("_get_new_buflength(): MAX_BUFLENGTH reached");
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

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		int_ae.elts = NULL;
	else
		int_ae.elts = Salloc((long) buflength, int);
	int_ae.buflength = buflength;
	int_ae.nelt = nelt;
	_IntAE_set_val(&int_ae, val);
	return int_ae;
}

static void IntAE_extend(IntAE *int_ae)
{
	long new_buflength;

	new_buflength = _get_new_buflength(int_ae->buflength);
	int_ae->elts = Srealloc((char *) int_ae->elts, new_buflength,
					(long) int_ae->buflength, int);
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

void _IntAE_qsort(IntAE *int_ae)
{
	_sort_int_array(int_ae->elts, int_ae->nelt);
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

SEXP _IntAE_asINTEGER(const IntAE *int_ae)
{
	SEXP ans;

	PROTECT(ans = NEW_INTEGER(int_ae->nelt));
	memcpy(INTEGER(ans), int_ae->elts, sizeof(int) * int_ae->nelt);
	UNPROTECT(1);
	return ans;
}

IntAE _INTEGER_asIntAE(SEXP x)
{
	IntAE int_ae;

	int_ae = _new_IntAE(LENGTH(x), 0, 0);
	memcpy(int_ae.elts, INTEGER(x), sizeof(int) * LENGTH(x));
	int_ae.nelt = int_ae.buflength;
	return int_ae;
}

IntAE _CHARACTER_asIntAE(SEXP x, int keyshift)
{
	IntAE int_ae;
	int *elt;

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CHARACTER_asIntAE(): BEGIN ... "
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
				Rprintf("[DEBUG] _CHARACTER_asIntAE(): "
					"int_ae.nelt=%d key=%s *elt=%d\n",
					int_ae.nelt,
					CHAR(STRING_ELT(x, int_ae.nelt)), *elt);
		}
#endif
	}
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CHARACTER_asIntAE(): END\n");
	}
#endif
	return int_ae;
}


/****************************************************************************
 * IntAEAE functions
 */

IntAEAE _new_IntAEAE(int buflength, int nelt)
{
	IntAEAE int_aeae;
	IntAE *elt;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		int_aeae.elts = NULL;
	else
		int_aeae.elts = Salloc((long) buflength, IntAE);
	int_aeae.buflength = buflength;
	for (int_aeae.nelt = 0, elt = int_aeae.elts;
	     int_aeae.nelt < nelt;
	     int_aeae.nelt++, elt++)
		*elt = _new_IntAE(0, 0, 0);
	return int_aeae;
}

static void IntAEAE_extend(IntAEAE *int_aeae)
{
	long new_buflength;

	new_buflength = _get_new_buflength(int_aeae->buflength);
	int_aeae->elts = Srealloc((char *) int_aeae->elts, new_buflength,
					(long) int_aeae->buflength, IntAE);
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
 * mode: 0 -> integer(0), 1-> NULL, 2 -> NA
 */
SEXP _IntAEAE_asLIST(const IntAEAE *int_aeae, int mode)
{
	SEXP ans, ans_elt;
	int i;
	const IntAE *elt;

	PROTECT(ans = NEW_LIST(int_aeae->nelt));
	for (i = 0, elt = int_aeae->elts; i < int_aeae->nelt; i++, elt++) {
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
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}

IntAEAE _LIST_asIntAEAE(SEXP x)
{
	IntAEAE int_aeae;
	IntAE *elt;

	int_aeae = _new_IntAEAE(LENGTH(x), 0);
	for (int_aeae.nelt = 0, elt = int_aeae.elts;
	     int_aeae.nelt < int_aeae.buflength;
	     int_aeae.nelt++, elt++) {
		*elt = _INTEGER_asIntAE(VECTOR_ELT(x, int_aeae.nelt));
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
		PROTECT(value = _IntAE_asINTEGER(elt));
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
 * RangeAE functions
 */

RangeAE _new_RangeAE(int buflength, int nelt)
{
	RangeAE range_ae;

	range_ae.start = _new_IntAE(buflength, nelt, 0);
	range_ae.width = _new_IntAE(buflength, nelt, 0);
	return range_ae;
}

void _RangeAE_insert_at(RangeAE *range_ae, int at, int start, int width)
{
	_IntAE_insert_at(&(range_ae->start), at, start);
	_IntAE_insert_at(&(range_ae->width), at, width);
	return;
}


SEXP _RangeAE_asIRanges(const RangeAE *range_ae)
{
  SEXP ans, start, width;

  PROTECT(start = _IntAE_asINTEGER(&(range_ae->start)));
  PROTECT(width = _IntAE_asINTEGER(&(range_ae->width)));
  ans = _new_IRanges("IRanges", start, width, R_NilValue);
  UNPROTECT(2);
  return ans;
}


/****************************************************************************
 * RangeAEAE functions
 */

RangeAEAE _new_RangeAEAE(int buflength, int nelt)
{
	RangeAEAE range_aeae;
	RangeAE *elt;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		range_aeae.elts = NULL;
	else
		range_aeae.elts = Salloc((long) buflength, RangeAE);
	range_aeae.buflength = buflength;
	for (range_aeae.nelt = 0, elt = range_aeae.elts;
	     range_aeae.nelt < nelt;
	     range_aeae.nelt++, elt++)
		*elt = _new_RangeAE(0, 0);
	return range_aeae;
}

static void RangeAEAE_extend(RangeAEAE *range_aeae)
{
	long new_buflength;

	new_buflength = _get_new_buflength(range_aeae->buflength);
	range_aeae->elts = Srealloc((char *) range_aeae->elts, new_buflength,
					(long) range_aeae->buflength, RangeAE);
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

SEXP _RangeAEAE_asLIST(const RangeAEAE *range_aeae)
{
	SEXP ans, ans_elt;
	int i;
	const RangeAE *elt;

	PROTECT(ans = NEW_LIST(range_aeae->nelt));
	for (i = 0, elt = range_aeae->elts; i < range_aeae->nelt; i++, elt++) {
		PROTECT(ans_elt = _RangeAE_asIRanges(elt));
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * CharAE functions
 */

CharAE _new_CharAE(int buflength)
{
	CharAE char_ae;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		char_ae.elts = NULL;
	else
		char_ae.elts = Salloc((long) buflength, char);
	char_ae.buflength = buflength;
	char_ae.nelt = 0;
	return char_ae;
}

CharAE _new_CharAE_from_string(const char *string)
{
	CharAE char_ae;
	int buflength;

	buflength = strlen(string);
	char_ae = _new_CharAE(buflength);
	memcpy(char_ae.elts, string, buflength);
	char_ae.nelt = buflength;
	return char_ae;
}

static void CharAE_extend(CharAE *char_ae)
{
	long new_buflength;

	new_buflength = _get_new_buflength(char_ae->buflength);
	char_ae->elts = Srealloc((char *) char_ae->elts, new_buflength,
					(long) char_ae->buflength, char);
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

SEXP _CharAE_asRAW(const CharAE *char_ae)
{
	SEXP ans;

	if (sizeof(Rbyte) != sizeof(char)) // should never happen!
		error("_CharAE_asRAW(): sizeof(Rbyte) != sizeof(char)");
	PROTECT(ans = NEW_RAW(char_ae->nelt));
	memcpy(RAW(ans), char_ae->elts, sizeof(char) * char_ae->nelt);
	UNPROTECT(1);
	return ans;
}

/* only until we have a bitset or something smaller than char */
SEXP _CharAE_asLOGICAL(const CharAE *char_ae)
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
 * CharAEAE functions
 */

CharAEAE _new_CharAEAE(int buflength, int nelt)
{
	CharAEAE char_aeae;
	CharAE *elt;

	/* No memory leak here, because we use transient storage allocation */
	if (buflength == 0)
		char_aeae.elts = NULL;
	else
		char_aeae.elts = Salloc((long) buflength, CharAE);
	char_aeae.buflength = buflength;
	for (char_aeae.nelt = 0, elt = char_aeae.elts;
	     char_aeae.nelt < nelt;
	     char_aeae.nelt++, elt++)
		*elt = _new_CharAE(0);
	return char_aeae;
}

static void CharAEAE_extend(CharAEAE *char_aeae)
{
	long new_buflength;

	new_buflength = _get_new_buflength(char_aeae->buflength);
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] CharAEAE_extend(): BEGIN\n");
		Rprintf("[DEBUG] CharAEAE_extend(): "
			"char_aeae->elts=%p buflength=%d new_buflength=%d\n",
			char_aeae->elts, char_aeae->buflength, new_buflength);
	}
#endif
	char_aeae->elts = Srealloc((char *) char_aeae->elts, new_buflength,
				(long) char_aeae->buflength, CharAE);
	char_aeae->buflength = new_buflength;
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] CharAEAE_extend(): END (char_aeae->elts=%p)\n",
			char_aeae->elts);
	}
#endif
	return;
}

void _CharAEAE_insert_at(CharAEAE *char_aeae, int at, const CharAE *char_ae)
{
	CharAE *elt1, *elt2;
	int i1;

#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CharAEAE_insert_at(): BEGIN\n");
	}
#endif
	if (char_aeae->nelt >= char_aeae->buflength)
		CharAEAE_extend(char_aeae);
	elt2 = char_aeae->elts + char_aeae->nelt;
	elt1 = elt2 - 1;
	for (i1 = char_aeae->nelt++; i1 > at; i1--)
		*(elt2--) = *(elt1--);
	*elt2 = *char_ae;
#ifdef DEBUG_IRANGES
	if (debug) {
		Rprintf("[DEBUG] _CharAEAE_insert_at(): END\n");
	}
#endif
	return;
}

void _append_string_to_CharAEAE(CharAEAE *char_aeae, const char *string)
{
	CharAE char_ae;

	char_ae = _new_CharAE_from_string(string);
	_CharAEAE_insert_at(char_aeae, char_aeae->nelt, &char_ae);
	return;
}

SEXP _CharAEAE_asCHARACTER(const CharAEAE *char_aeae)
{
  int i;
  SEXP ans;

  PROTECT(ans = NEW_CHARACTER(char_aeae->nelt));
  for (i = 0; i < char_aeae->nelt; i++)
    SET_STRING_ELT(ans, i,
                   mkCharLen(char_aeae->elts[i].elts, char_aeae->elts[i].nelt));
  UNPROTECT(1);
  return ans;
}
