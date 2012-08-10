/****************************************************************************
 *   Comparing and ordering the elements in one or more XRawList objects    *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"
#include <stdlib.h> /* for qsort() */


/****************************************************************************
 * Comparison of 2 cachedCharSeq structs.
 */

static int compar_cachedCharSeqs(const cachedCharSeq *x1,
				 const cachedCharSeq *x2)
{
	int n, ret;

	n = x1->length < x2->length ? x1->length : x2->length;
	ret = memcmp(x1->seq, x2->seq, n);
	if (ret != 0)
		return ret;
	ret = x1->length - x2->length;
	return ret;
}

/* Fast version of 'compar_cachedCharSeqs(x1, x2) == 0' */
static int equal_cachedCharSeqs(const cachedCharSeq *x1,
				const cachedCharSeq *x2)
{
	return x1->length == x2->length &&
	       memcmp(x1->seq, x2->seq, x1->length) == 0;
}


/****************************************************************************
 * "Parallel" comparison of 2 XRawList objects.
 */

static void cachedXRawList_pcompar(const cachedXVectorList *x,
				   const cachedXVectorList *y,
				   int *out, int out_len, int with_warning)
{
	int x_len, y_len, i, j, k;
	cachedCharSeq x_elt, y_elt;

	x_len = _get_cachedXVectorList_length(x);
	y_len = _get_cachedXVectorList_length(y);
	for (i = j = k = 0; k < out_len; i++, j++, k++) {
		if (i >= x_len)
			i = 0; /* recycle i */
		if (j >= y_len)
			j = 0; /* recycle j */
		x_elt = _get_cachedXRawList_elt(x, i);
		y_elt = _get_cachedXRawList_elt(y, j);
		out[k] = compar_cachedCharSeqs(&x_elt, &y_elt);
	}
	/* Warning message appropriate only when 'out_len' is
	   'max(x_len, y_len)' */
	if (with_warning && out_len != 0 && (i != x_len || j != y_len))
		warning("longer object length is not a multiple "
			"of shorter object length");
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP XRawList_compare(SEXP x, SEXP y)
{
	cachedXVectorList cached_x, cached_y;
	int x_len, y_len, ans_len;
	SEXP ans;

	cached_x = _cache_XVectorList(x);
	cached_y = _cache_XVectorList(y);
	x_len = _get_cachedXVectorList_length(&cached_x);
	y_len = _get_cachedXVectorList_length(&cached_y);
	if (x_len == 0 || y_len == 0)
		ans_len = 0;
	else
		ans_len = x_len >= y_len ? x_len : y_len;
	PROTECT(ans = NEW_INTEGER(ans_len));
	cachedXRawList_pcompar(&cached_x, &cached_y, INTEGER(ans), ans_len, 1);
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Order and rank of the elements in an XRawList object.
 */

static cachedCharSeq *XX;

static int compar_XX(int i1, int i2)
{
	return compar_cachedCharSeqs(XX + i1, XX + i2);
}

static int compar_XX_for_stable_asc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = compar_XX(i1, i2);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/* We cannot just define compar_XX_for_stable_desc_order(p1, p2) to be
 * compar_XX_for_stable_asc_order(p2, p1) because of the tie-break
 * by position. */
static int compar_XX_for_stable_desc_order(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = compar_XX(i2, i1);
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

static void get_order_of_cachedXRawList(const cachedXVectorList *cached_x,
		int desc, int *out, int out_shift)
{
	int nelt, i, (*compar)(const void *, const void *);

	nelt = _get_cachedXVectorList_length(cached_x);
	XX = (cachedCharSeq *) R_alloc(sizeof(cachedCharSeq), nelt);
	XX -= out_shift;
	for (i = 0; i < nelt; i++, out_shift++) {
		XX[out_shift] = _get_cachedXRawList_elt(cached_x, i);
		out[i] = out_shift;
	}
	compar = desc ? compar_XX_for_stable_desc_order :
			compar_XX_for_stable_asc_order;
	qsort(out, nelt, sizeof(int), compar);
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP XRawList_is_unsorted(SEXP x, SEXP strictly)
{
	cachedXVectorList cached_x;
	int x_length, is_unsorted, i, ret0, ret;
	cachedCharSeq x_elt1, x_elt2;
	SEXP ans;

	cached_x = _cache_XVectorList(x);
	x_length = _get_cachedXVectorList_length(&cached_x);
	ret0 = LOGICAL(strictly)[0] ? 0 : 1;
	is_unsorted = 0;
	if (x_length >= 2) {
		x_elt2 = _get_cachedXRawList_elt(&cached_x, 0);
		for (i = 1; i < x_length; i++) {
			x_elt1 = x_elt2;
			x_elt2 = _get_cachedXRawList_elt(&cached_x, i);
			ret = compar_cachedCharSeqs(&x_elt1, &x_elt2);
			if (ret >= ret0) {
				is_unsorted = 1;
				break;
			}
		}
	}
	PROTECT(ans = NEW_LOGICAL(1));
	LOGICAL(ans)[0] = is_unsorted;
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP XRawList_order(SEXP x, SEXP decreasing)
{
	cachedXVectorList cached_x;
	int ans_length;
	SEXP ans;

	cached_x = _cache_XVectorList(x);
	ans_length = _get_cachedXVectorList_length(&cached_x);
	PROTECT(ans = NEW_INTEGER(ans_length));
	get_order_of_cachedXRawList(&cached_x,
				LOGICAL(decreasing)[0], INTEGER(ans), 1);
	UNPROTECT(1);
	return ans;
}

static void get_first_rank_from_order(const int *oo, int nelt, int *out)
{
	int i;

	for (i = 1; i <= nelt; i++)
		out[*(oo++)] = i;
	return;
}

static void get_min_rank_from_order(const int *oo, int nelt, int *out,
		const cachedXVectorList *cached_x)
{
	const int *oo1, *oo2;
	cachedCharSeq x_elt1, x_elt2;
	int i;

	oo1 = oo2 = oo;
	x_elt2 = _get_cachedXRawList_elt(cached_x, *oo2);
	out[*oo2] = 1;
	oo2++;
	for (i = 2; i <= nelt; i++)
	{
		x_elt1 = x_elt2;
		x_elt2 = _get_cachedXRawList_elt(cached_x, *oo2);
		out[*oo2] = equal_cachedCharSeqs(&x_elt1, &x_elt2) ?
				out[*oo1] : i;
		oo2++;
		oo1++;
	}
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP XRawList_rank(SEXP x, SEXP ties_method)
{
	cachedXVectorList cached_x;
	int ans_length, *oo;
	const char *method;
	SEXP ans;

	cached_x = _cache_XVectorList(x);
	ans_length = _get_cachedXVectorList_length(&cached_x);
	method = CHAR(STRING_ELT(ties_method, 0));
	oo = (int *) R_alloc(ans_length, sizeof(int));
	get_order_of_cachedXRawList(&cached_x, 0, oo, 0);
	PROTECT(ans = NEW_INTEGER(ans_length));
	if (ans_length <= 1 || strcmp(method, "first") == 0) {
		get_first_rank_from_order(oo, ans_length, INTEGER(ans));
	} else if (strcmp(method, "min") == 0) {
		get_min_rank_from_order(oo, ans_length, INTEGER(ans),
					&cached_x);
	} else {
		error("ties_method \"%s\" is not supported", ties_method);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Matches between 2 XRawList objects, and self-matches within an XRawList
 * object.
 */

/*
 * We use the Bernstein's function to hash arbitrary arrays of bytes.
 * See http://www.strchr.com/hash_functions for an empirical comparison of hash
 * functions and http://www.cse.yorku.ca/~oz/hash.html for an implementation of
 * the Bernstein's function. Note that this implementation is NOT the same as
 * the hash function used for the Global CHARSXP cache in base R
 * (see char_hash() function in R_HOME/src/main/envir.c) which was taken from
 * the same place but slightly modified by replacing the use of unsigned char
 * with (plain) char. As a consequence, the hash values returned by djb2_hash()
 * and char_hash() will be different on a platform where (plain) char is
 * equivalent to signed char (e.g. gcc on Intel 32- or 64-bit Linux).
 *
 * TODO: Some people recommend to use XOR operation instead of addition in
 * 'hval * 33 + *s'. Try and see if that makes any difference.
 */
static unsigned int djb2_hash(const unsigned char *s, int len)
{
	unsigned int hval = 5381;
	int i;

	for (i = 0; i < len; i++, s++)
		hval += (hval << 5) + *s; /* hval = hval * 33 + *s */
	return hval;
}

static int get_bucket_idx_for_cachedCharSeq(const struct htab *htab,
		const cachedCharSeq *charseq1,
		const cachedXVectorList *charseqs2)
{
	unsigned int hval;
	int bucket_idx, i2;
	const int *buckets;
	cachedCharSeq charseq2;

	hval = djb2_hash((unsigned char *) charseq1->seq, charseq1->length);
	bucket_idx = hval & htab->Mminus1;
	buckets = htab->buckets;
	while ((i2 = buckets[bucket_idx]) != NA_INTEGER) {
		charseq2 = _get_cachedXRawList_elt(charseqs2, i2);
		if (equal_cachedCharSeqs(charseq1, &charseq2))
			break;
		bucket_idx = (bucket_idx + 1) % htab->M;
	}
	return bucket_idx;
}

/* --- .Call ENTRY POINT --- */
SEXP XRawList_match_hash(SEXP x1, SEXP x2, SEXP nomatch)
{
	int len1, len2, nomatch0, *ans0, i, bucket_idx, i2;
	cachedXVectorList cached_x1, cached_x2;
	cachedCharSeq charseq;
	struct htab htab;
	SEXP ans;

	cached_x1 = _cache_XVectorList(x1);
	cached_x2 = _cache_XVectorList(x2);
	len1 = _get_cachedXVectorList_length(&cached_x1);
	len2 = _get_cachedXVectorList_length(&cached_x2);
	nomatch0 = INTEGER(nomatch)[0];
	htab = _new_htab(len2);
	for (i = 0; i < len2; i++) {
		charseq = _get_cachedXRawList_elt(&cached_x2, i);
		bucket_idx = get_bucket_idx_for_cachedCharSeq(&htab,
					&charseq, &cached_x2);
		if (_get_hbucket_val(&htab, bucket_idx) == NA_INTEGER)
			_set_hbucket_val(&htab, bucket_idx, i);
	}
	PROTECT(ans = NEW_INTEGER(len1));
	ans0 = INTEGER(ans);
	for (i = 0; i < len1; i++) {
		charseq = _get_cachedXRawList_elt(&cached_x1, i);
		bucket_idx = get_bucket_idx_for_cachedCharSeq(&htab,
					&charseq, &cached_x2);
		i2 = _get_hbucket_val(&htab, bucket_idx);
		if (i2 == NA_INTEGER)
			ans0[i] = nomatch0;
		else
			ans0[i] = i2 + 1;
	}
	UNPROTECT(1);
	return ans;
}

/* --- .Call ENTRY POINT --- */
SEXP XRawList_selfmatch_hash(SEXP x)
{
	int ans_length, *ans0, i, bucket_idx, i2;
	cachedXVectorList cached_x;
	cachedCharSeq charseq;
	struct htab htab;
	SEXP ans;

	cached_x = _cache_XVectorList(x);
	ans_length = _get_cachedXVectorList_length(&cached_x);
	htab = _new_htab(ans_length);
	PROTECT(ans = NEW_INTEGER(ans_length));
	ans0 = INTEGER(ans);
	for (i = 0; i < ans_length; i++) {
		charseq = _get_cachedXRawList_elt(&cached_x, i);
		bucket_idx = get_bucket_idx_for_cachedCharSeq(&htab,
					&charseq, &cached_x);
		i2 = _get_hbucket_val(&htab, bucket_idx);
		if (i2 == NA_INTEGER) {
			_set_hbucket_val(&htab, bucket_idx, i);
			ans0[i] = i + 1;
		} else {
			ans0[i] = i2 + 1;
		}
	}
	UNPROTECT(1);
	return ans;
}

