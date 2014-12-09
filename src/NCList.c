/****************************************************************************
 *                 A Nested Containment List implementation                 *
 *                                                                          *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h>  /* for malloc, realloc, free, qsort */
#include <math.h>    /* for log10 */

/*
#include <time.h>
static clock_t clock0;
static void init_clock(const char *msg)
{
	printf("%s", msg);
	clock0 = clock();
}
static void print_elapsed_time()
{
	printf("%8.6f s\n", ((double) clock() - clock0) / CLOCKS_PER_SEC);
}
*/

typedef struct nclist_t {
	int buflength;  /* >= 0 */
	int nelt;       /* >= 0 and <= buflength */
	int *revmap;    /* Of length 'nelt'. Reverse mapping into Ranges
			   object 'x'. Contains 0-based indices */
	struct nclist_t *contained_list;  /* Of length 'nelt' */
} NCList;

static void init_NCList(NCList *nclist)
{
	nclist->buflength = nclist->nelt = 0;
	return;
}

static void free_NCList(const NCList *nclist)
{
	int n;
	const NCList *contained_list_p;

	if (nclist->buflength == 0)
		return;
	for (n = 0, contained_list_p = nclist->contained_list;
	     n < nclist->nelt;
	     n++, contained_list_p++)
		free_NCList(contained_list_p);
	free(nclist->revmap);
	free(nclist->contained_list);
	return;
}


/****************************************************************************
 * NCList_new() and NCList_free()
 */

/* --- .Call ENTRY POINT --- */
SEXP NCList_new()
{
	NCList *top_nclist;

	//init_clock("preprocessing: T1 = ");
	top_nclist = (NCList *) malloc(sizeof(NCList));
	if (top_nclist == NULL)
		error("NCList_new: memory allocation failed");
	init_NCList(top_nclist);
	return R_MakeExternalPtr(top_nclist, R_NilValue, R_NilValue);
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_free(SEXP nclist_xp)
{
	NCList *top_nclist;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist_xp);
	if (top_nclist == NULL)
		error("NCList_free: pointer to NCList struct is NULL");
	free_NCList(top_nclist);
	free(top_nclist);
	R_SetExternalPtrAddr(nclist_xp, NULL);
	return R_NilValue;
}


/****************************************************************************
 * NCList_build()
 */

static void extend_NCList(NCList *nclist)
{
	int old_buflength, new_buflength;
	int *new_revmap;
	NCList *new_contained_list;

	old_buflength = nclist->buflength;
	if (old_buflength == 0) {
		new_buflength = 4;
		new_revmap = (int *) malloc(sizeof(int) * new_buflength);
		new_contained_list = (NCList *) malloc(sizeof(NCList) *
							  new_buflength);
	} else {
		if (old_buflength < 16384)
			new_buflength = 8 * old_buflength;
		else if (old_buflength < 4194304)
			new_buflength = 4 * old_buflength;
		else if (old_buflength < 67108864)
			new_buflength = 2 * old_buflength;
		else
			new_buflength = old_buflength + 33554432;
		new_revmap = (int *) realloc(nclist->revmap,
					     sizeof(int) * new_buflength);
		new_contained_list = (NCList *)
				     realloc(nclist->contained_list,
					     sizeof(NCList) * new_buflength);
	}
	if (new_revmap == NULL || new_contained_list == NULL)
		error("extend_NCList: memory allocation failed");
	nclist->buflength = new_buflength;
	nclist->revmap = new_revmap;
	nclist->contained_list = new_contained_list;
	return;
}

static const int *aa, *bb;

static int qsort_compar(const void *p1, const void *p2)
{
	int i1, i2, ret;

	i1 = *((const int *) p1);
	i2 = *((const int *) p2);
	ret = aa[i1] - aa[i2];
	if (ret != 0)
		return ret;
	ret = bb[i2] - bb[i1];
	return ret;
}

/*
 * Setting a hard limit on the max depth of NCList objects to prevent C stack
 * overflows when running recursive code like NCList_get_y_overlaps().
 * A better solution would be to not use recursive code at all when traversing
 * an NCList object. Then NCList objects of arbitrary depth could be supported
 * and it wouldn't be necessary to set the limit below.
 */
#define NCLIST_MAX_DEPTH 25000
typedef struct stack_elt_t {
	int revidx;
	NCList *contained_list;
} StackElt;

static StackElt *stack = NULL;
static int stack_length = 0;

static StackElt append_NCList_elt(NCList *host, int revidx)
{
	StackElt stack_elt;

	if (host->nelt == host->buflength)
		extend_NCList(host);
	stack_elt.revidx = host->revmap[host->nelt] = revidx;
	stack_elt.contained_list = host->contained_list + host->nelt;
	init_NCList(stack_elt.contained_list);
	host->nelt++;
	return stack_elt;
}

static void extend_stack()
{
	int new_length;
	StackElt *new_stack;

	if (stack_length == 0) {
		new_length = 1000;
		new_stack = (StackElt *) malloc(sizeof(StackElt) *
						new_length);
	} else {
		if (stack_length == NCLIST_MAX_DEPTH)
			error("extend_stack: cannot build an NCList object "
			      "of depth >= %d", NCLIST_MAX_DEPTH);
		if (stack_length <= NCLIST_MAX_DEPTH / 2)
			new_length = 2 * stack_length;
		else
			new_length = NCLIST_MAX_DEPTH;
		new_stack = (StackElt *) realloc(stack, sizeof(StackElt) *
							new_length);
	}
	if (new_stack == NULL)
		error("extend_stack: memory allocation failed");
	stack_length = new_length;
	stack = new_stack;
	return;
}

static void build_NCList(NCList *top_nclist,
			 const int *x_start_p, const int *x_end_p,
			 const int *x_subset_p, int x_len)
{
	int *oo, k, d, i, current_end;
	NCList *host;
	StackElt stack_elt;

	/* Determine order of 'x'. 'oo' will be such that 'x[oo]' is sorted
	   first by ascending start then by descending end. */
	oo = (int *) R_alloc(sizeof(int), x_len);
	if (x_subset_p == NULL)
		for (i = 0; i < x_len; i++)
			oo[i] = i;
	else
		for (i = 0; i < x_len; i++)
			oo[i] = x_subset_p[i];
	aa = x_start_p;
	bb = x_end_p;
	qsort(oo, x_len, sizeof(int), qsort_compar);

	init_NCList(top_nclist);
	for (k = 0, d = -1; k < x_len; k++) {
		i = oo[k];
		current_end = x_end_p[i];
		while (d >= 0 && x_end_p[stack[d].revidx] < current_end)
			d--;  // unstack
		host = d == -1 ? top_nclist: stack[d].contained_list;
		// append range i to host
		stack_elt = append_NCList_elt(host, i);
		if (++d == stack_length)
			extend_stack();
		stack[d] = stack_elt;  // stack
	}
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_build(SEXP nclist_xp, SEXP x_start, SEXP x_end, SEXP x_subset)
{
	NCList *top_nclist;
	int x_len;
	const int *x_start_p, *x_end_p, *x_subset_p;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist_xp);
	if (top_nclist == NULL)
		error("NCList_build: pointer to NCList struct is NULL");
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	if (x_subset == R_NilValue) {
		x_subset_p = NULL;
	} else {
		x_subset_p = INTEGER(x_subset);
		x_len = LENGTH(x_subset);
	}
	build_NCList(top_nclist, x_start_p, x_end_p, x_subset_p, x_len);
	return nclist_xp;
}


/****************************************************************************
 * new_NCListSXP_from_NCList()
 */

#define NCListSXP_NELT(nclist) ((nclist)[0])
#define NCListSXP_REVMAP(nclist) ((nclist)+1)
#define NCListSXP_OFFSETS(nclist) ((nclist)+1+NCListSXP_NELT(nclist))

static int compute_length_of_NCListSXP(const NCList *nclist)
{
	int nelt, n;
	unsigned int ans_len, dump_len;
	const NCList *contained_list_p;

	nelt = nclist->nelt;
	if (nelt == 0)
		return 0;
	ans_len = 1U + 2U * (unsigned int) nelt;
	for (n = 0, contained_list_p = nclist->contained_list;
	     n < nelt;
	     n++, contained_list_p++)
	{
		dump_len = compute_length_of_NCListSXP(contained_list_p);
		ans_len += dump_len;
		if (dump_len > ans_len)
			goto too_big;
	}
	if (ans_len <= INT_MAX)
		return (int) ans_len;
too_big:
	error("compute_length_of_NCListSXP: "
	      "NCList object is too big to fit in an integer vector");
}

static int dump_NCList_to_int_array(const NCList *nclist, int *out)
{
	int nelt, offset, dump_len, n;
	const int *revmap_p;
	const NCList *contained_list_p;

	nelt = nclist->nelt;
	if (nelt == 0)
		return 0;
	offset = 1 + 2 * nelt;
	NCListSXP_NELT(out) = nelt;
	for (n = 0, revmap_p = nclist->revmap,
		    contained_list_p = nclist->contained_list;
	     n < nelt;
	     n++, revmap_p++, contained_list_p++)
	{
		NCListSXP_REVMAP(out)[n] = *revmap_p;
		dump_len = dump_NCList_to_int_array(contained_list_p,
						    out + offset);
		NCListSXP_OFFSETS(out)[n] = dump_len != 0 ? offset : -1;
		offset += dump_len;
	}
	return offset;
}

/* --- .Call ENTRY POINT --- */
SEXP new_NCListSXP_from_NCList(SEXP nclist_xp)
{
	SEXP ans;
	const NCList *top_nclist;
	int ans_len;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist_xp);
	if (top_nclist == NULL)
		error("new_NCListSXP_from_NCList: "
		      "pointer to NCList struct is NULL");
	ans_len = compute_length_of_NCListSXP(top_nclist);
	PROTECT(ans = NEW_INTEGER(ans_len));
	dump_NCList_to_int_array(top_nclist, INTEGER(ans));
	UNPROTECT(1);
	//print_elapsed_time();
	return ans;
}


/****************************************************************************
 * NCListSXP_print()
 */

/* Print 1 line per range in 'nclist'. Return max depth. */
static int print_NCListSXP(const int *nclist,
			   const int *x_start_p, const int *x_end_p,
			   int depth, const char *format)
{
	int max_depth, nelt, n, d, revidx, offset, tmp;

	max_depth = depth;
	nelt = NCListSXP_NELT(nclist);
	for (n = 0; n < nelt; n++) {
		for (d = 1; d < depth; d++)
			Rprintf("|");
		revidx = NCListSXP_REVMAP(nclist)[n];
		Rprintf(format, revidx + 1);
		Rprintf(": [%d, %d]\n", x_start_p[revidx], x_end_p[revidx]);
		offset = NCListSXP_OFFSETS(nclist)[n];
		if (offset != -1) {
			tmp = print_NCListSXP(nclist + offset,
					      x_start_p, x_end_p, depth + 1,
					      format);
			if (tmp > max_depth)
				max_depth = tmp;
		}
	}
	return max_depth;
}

/* --- .Call ENTRY POINT --- */
SEXP NCListSXP_print(SEXP x_nclist, SEXP x_start, SEXP x_end)
{
	const int *top_nclist;
	int x_len, max_digits, max_depth;
	const int *x_start_p, *x_end_p;
	char format[10];

	top_nclist = INTEGER(x_nclist);
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	if (x_len == 0) {
		max_depth = 0;
	} else {
		max_digits = (int) log10((double) x_len) + 1;
		sprintf(format, "%c0%d%c", '%', max_digits, 'd');
		max_depth = print_NCListSXP(top_nclist, x_start_p, x_end_p,
					    1, format);
	}
	Rprintf("max depth = %d\n", max_depth);
	return R_NilValue;
}


/****************************************************************************
 * pp_find_overlaps()
 */

/* 6 supported types of overlap. */
#define TYPE_ANY		1
#define TYPE_START		2
#define TYPE_END		3
#define TYPE_WITHIN		4
#define TYPE_EXTEND		5
#define TYPE_EQUAL		6

typedef struct backpack_t {
	/* Members set by prepare_backpack(). */
	const int *x_start_p;
	const int *x_end_p;
	const int *x_space_p;
	int min_overlap_score0;
	int overlap_type;
	int select_mode;
	int circle_len;
	int pp_is_q;
	IntAE *hits;
	int *direct_out;

	/* Members set by update_backpack(). */
	int y_start;
	int y_end;
	int y_space;
	int ext_y_start;
	int ext_y_end;
	int y_idx;
} Backpack;

static int is_hit(int revidx, const Backpack *backpack)
{
	static int x_space, x_start, x_end, d;

	/* Check the space */
	if (backpack->x_space_p != NULL && backpack->y_space != 0) {
		x_space = backpack->x_space_p[revidx];
		if (x_space != 0 && x_space != backpack->y_space)
			return 0;
	}
	/* Check the score */
	x_start = backpack->x_start_p[revidx];
	x_end = backpack->x_end_p[revidx];
	if (x_end - x_start < backpack->min_overlap_score0)
		return 0;
	/* Check the type */
	if (backpack->overlap_type == TYPE_ANY
	 || backpack->overlap_type == TYPE_WITHIN)
		return 1;
	if (backpack->overlap_type == TYPE_EXTEND)
		return backpack->y_start <= x_start &&
		       backpack->y_end >= x_end;
	if (backpack->overlap_type == TYPE_START)
		return backpack->y_start == x_start;
	if (backpack->overlap_type == TYPE_END) {
		d = backpack->y_end - x_end;
		if (backpack->circle_len != NA_INTEGER)
			d %= backpack->circle_len;
		return d == 0;
	}
	/* TYPE_EQUAL */
	return backpack->y_start == x_start &&
	       backpack->y_end == x_end;
}

static void report_hit(int revidx, const Backpack *backpack)
{
	int i1, q_idx, s_idx1, *selection_p;

	i1 = revidx + 1;  /* 1-based */
	if (backpack->select_mode == ALL_HITS) {
		/* Report the hit. */
		IntAE_insert_at(backpack->hits,
				IntAE_get_nelt(backpack->hits), i1);
		return;
	}
	/* Update current selection if necessary. */
	if (backpack->pp_is_q) {
		q_idx = revidx;
		s_idx1 = backpack->y_idx + 1;
	} else {
		q_idx = backpack->y_idx;
		s_idx1 = i1;
	}
	selection_p = backpack->direct_out + q_idx;
	if (backpack->select_mode == COUNT_HITS) {
		(*selection_p)++;
		return;
	}
	if (*selection_p == NA_INTEGER
	 || (backpack->select_mode == FIRST_HIT) ==
	    (s_idx1 < *selection_p))
		*selection_p = s_idx1;
	return;
}

static Backpack prepare_backpack(const int *x_start_p, const int *x_end_p,
				 const int *x_space_p, 
				 int min_overlap_score, int overlap_type,
				 int select_mode, int circle_len, int pp_is_q,
				 IntAE *hits, int *direct_out)
{
	Backpack backpack;

	backpack.x_start_p = x_start_p;
	backpack.x_end_p = x_end_p;
	backpack.x_space_p = x_space_p;
	backpack.min_overlap_score0 = min_overlap_score - 1;
	backpack.overlap_type = overlap_type;
	backpack.select_mode = select_mode;
	backpack.circle_len = circle_len;
	backpack.pp_is_q = pp_is_q;
	backpack.hits = hits;
	backpack.direct_out = direct_out;
	return backpack;
}

static void update_backpack(Backpack *backpack, int y_start, int y_end,
						int y_space, int y_idx)
{
	backpack->y_start = y_start;
	backpack->y_end = y_end;
	backpack->y_space = y_space;
	backpack->ext_y_start = y_start + backpack->min_overlap_score0;
	backpack->ext_y_end = y_end - backpack->min_overlap_score0;
	backpack->y_idx = y_idx;
	return;
}

static void shift_y(Backpack *backpack, int shift)
{
	backpack->y_start += shift;
	backpack->y_end += shift;
	backpack->ext_y_start += shift;
	backpack->ext_y_end += shift;
	return;
}

/* TODO: Maybe move this to S4Vectors/src/AEbufs.c */
static void IntAE_delete_duplicates(IntAE *int_ae, int at1, int at2)
{
	int d, k0, k, val;

	d = at2 - at1;
	if (d <= 1)
		return;
	if (d >= 3)
		sort_int_array(int_ae->elts + at1, d, 0);
	k0 = at1;
	for (k = k0 + 1; k < at2; k++) {
		val = int_ae->elts[k];
		if (val == int_ae->elts[k0])
			continue;
		k0++;
		int_ae->elts[k0] = val;
	}
	IntAE_set_nelt(int_ae, k0 + 1);
	return;
}

typedef void (*GetYOverlapsFunType)(const void *, const Backpack *);

static void pp_find_overlaps(
		const int *q_start_p, const int *q_end_p,
		const int *q_space_p, const int *q_subset_p, int q_len,
		const int *s_start_p, const int *s_end_p,
		const int *s_space_p, const int *s_subset_p, int s_len,
		int min_overlap_score, int overlap_type,
		int select_mode, int circle_len,
		const void *pp, int pp_is_q,
		GetYOverlapsFunType get_y_overlaps,
		IntAE *qh_buf, IntAE *sh_buf, int *direct_out)
{
	const int *x_start_p, *x_end_p, *x_space_p,
		  *y_start_p, *y_end_p, *y_space_p, *y_subset_p;
	int y_len, backpack_select_mode,
	    i, j, y_start, y_end, old_nhit, new_nhit, k;
	IntAE *xh_buf, *yh_buf;
	Backpack backpack;

	if (q_len == 0 || s_len == 0)
		return;
	if (pp_is_q) {
		x_start_p = q_start_p;
		x_end_p = q_end_p;
		x_space_p = q_space_p;
		xh_buf = qh_buf;
		y_start_p = s_start_p;
		y_end_p = s_end_p;
		y_space_p = s_space_p;
		y_subset_p = s_subset_p;
		y_len = s_len;
		yh_buf = sh_buf;
		if (overlap_type == TYPE_WITHIN)
			overlap_type = TYPE_EXTEND;
		else if (overlap_type == TYPE_EXTEND)
			overlap_type = TYPE_WITHIN;
	} else {
		x_start_p = s_start_p;
		x_end_p = s_end_p;
		x_space_p = s_space_p;
		xh_buf = sh_buf;
		y_start_p = q_start_p;
		y_end_p = q_end_p;
		y_space_p = q_space_p;
		y_subset_p = q_subset_p;
		y_len = q_len;
		yh_buf = qh_buf;
	}
	if (circle_len != NA_INTEGER && select_mode == COUNT_HITS)
		backpack_select_mode = ALL_HITS;
	else
		backpack_select_mode = select_mode;
	backpack = prepare_backpack(x_start_p, x_end_p, x_space_p,
				    min_overlap_score, overlap_type,
				    backpack_select_mode, circle_len, pp_is_q,
				    xh_buf, direct_out);
	for (i = 0; i < y_len; i++) {
		j = y_subset_p == NULL ? i : y_subset_p[i];
		y_start = y_start_p[j];
		y_end = y_end_p[j];
		if (y_end - y_start < backpack.min_overlap_score0)
			continue;
		update_backpack(&backpack, y_start, y_end,
				y_space_p == NULL ? 0 : y_space_p[j], j);
		/* pass 0 */
		get_y_overlaps(pp, &backpack);
		if (circle_len == NA_INTEGER)
			goto life_is_good;
		if (select_mode == ARBITRARY_HIT
		 && !pp_is_q && direct_out[j] != NA_INTEGER)
			goto life_is_good;
		/* pass 1 */
		shift_y(&backpack, - circle_len);
		get_y_overlaps(pp, &backpack);
		if (select_mode == ARBITRARY_HIT
		 && !pp_is_q && direct_out[j] != NA_INTEGER)
			goto life_is_good;
		/* pass 2 */
		shift_y(&backpack, 2 * circle_len);
		get_y_overlaps(pp, &backpack);

		life_is_good:
		if (backpack_select_mode != ALL_HITS)
			continue;
		old_nhit = IntAE_get_nelt(yh_buf);
		new_nhit = IntAE_get_nelt(xh_buf);
		if (circle_len != NA_INTEGER) {
			IntAE_delete_duplicates(xh_buf, old_nhit, new_nhit);
			new_nhit = IntAE_get_nelt(xh_buf);
		}
		if (select_mode != COUNT_HITS) {
			j++;  /* 1-based */
			for (k = old_nhit; k < new_nhit; k++)
				IntAE_insert_at(yh_buf, k, j);
			continue;
		}
		if (pp_is_q) {
			for (k = old_nhit; k < new_nhit; k++)
				direct_out[xh_buf->elts[k] - 1]++;
		} else {
			direct_out[j] += new_nhit - old_nhit;
		}
		IntAE_set_nelt(xh_buf, old_nhit);
	}
	return;
}


/****************************************************************************
 * bsearch_revmap()
 */

/*
 * 'subset_len' is assumed to be > 0.
 * Return the first index 'n' for which 'base[subset[n]] >= min', or
 * 'subset_len' if there is no such index.
 * TODO: Maybe move this to int_utils.c or sort_utils.c in S4Vectors/src/
 */
static int int_bsearch(const int *subset, int subset_len,
		       const int *base, int min)
{
	int n1, n2, n, b;

	/* Check first element. */
	n1 = 0;
	b = base[subset[n1]];
	if (b >= min)
		return n1;

	/* Check last element. */
	n2 = subset_len - 1;
	b = base[subset[n2]];
	if (b < min)
		return subset_len;
	if (b == min)
		return n2;

	/* Binary search.
	   Seems that using >> 1 instead of / 2 is faster, even when compiling
	   with 'gcc -O2' (one would hope that the optimizer is able to do that
	   kind of optimization). */
	while ((n = (n1 + n2) >> 1) != n1) {
		b = base[subset[n]];
		if (b == min)
			return n;
		if (b < min)
			n1 = n;
		else
			n2 = n;
	}
	return n2;
}

static int bsearch_revmap(const int *revmap, int revmap_len,
			  const Backpack *backpack)
{
	const int *base;
	int min;

	if (backpack->overlap_type == TYPE_WITHIN) {
		base = backpack->x_end_p;
		min = backpack->y_end;
	} else {
		base = backpack->x_end_p;
		min = backpack->ext_y_start;
	}
	return int_bsearch(revmap, revmap_len, base, min);
}


/****************************************************************************
 * NCList_get_y_overlaps()
 */

/* Recursive! */
static void NCList_get_y_overlaps(const NCList *x_nclist,
				  const Backpack *backpack)
{
	const int *revmap_p, *base;
	int nelt, n, revidx, max;
	const NCList *contained_list_p;

	revmap_p = x_nclist->revmap;
	nelt = x_nclist->nelt;
	n = bsearch_revmap(revmap_p, nelt, backpack);
	for (revmap_p = revmap_p + n,
	     contained_list_p = x_nclist->contained_list + n;
	     n < nelt;
	     n++, revmap_p++, contained_list_p++)
	{
		revidx = *revmap_p;
		if (backpack->overlap_type == TYPE_WITHIN:) {
			base = backpack->x_start_p;
			max = backpack->y_start;
		} else {
			base = backpack->x_start_p;
			max = backpack->ext_y_end;
		}
		if (base[revidx] > max)
			break;
		if (is_hit(revidx, backpack)) {
			report_hit(revidx, backpack);
			if (backpack->select_mode == ARBITRARY_HIT
			 && !backpack->pp_is_q)
				break;
		}
		if (contained_list_p->nelt != 0)
			NCList_get_y_overlaps(contained_list_p, backpack);
	}
	return;
}


/****************************************************************************
 * NCListSXP_get_y_overlaps()
 */

/* Recursive! */
static void NCListSXP_get_y_overlaps(const int *x_nclist,
				     const Backpack *backpack)
{
	const int *revmap_p, *base, *offset_p;
	int nelt, n, revidx, max, offset;

	revmap_p = NCListSXP_REVMAP(x_nclist);
	nelt = NCListSXP_NELT(x_nclist);
	n = bsearch_revmap(revmap_p, nelt, backpack);
	for (revmap_p = revmap_p + n,
	     offset_p = NCListSXP_OFFSETS(x_nclist) + n;
	     n < nelt;
	     n++, revmap_p++, offset_p++)
	{
		revidx = *revmap_p;
		if (backpack->overlap_type == TYPE_WITHIN:) {
			base = backpack->x_start_p;
			max = backpack->y_start;
		} else {
			base = backpack->x_start_p;
			max = backpack->ext_y_end;
		}
		if (base[revidx] > max)
			break;
		if (is_hit(revidx, backpack)) {
			report_hit(revidx, backpack);
			if (backpack->select_mode == ARBITRARY_HIT
			 && !backpack->pp_is_q)
				break;
		}
		offset = *offset_p;
		if (offset != -1)
			NCListSXP_get_y_overlaps(x_nclist + offset, backpack);
	}
	return;
}


/****************************************************************************
 * find_overlaps()
 */

static int find_overlaps(
		const int *q_start_p, const int *q_end_p,
		const int *q_space_p, const int *q_subset_p, int q_len,
		const int *s_start_p, const int *s_end_p,
		const int *s_space_p, const int *s_subset_p, int s_len,
		int min_overlap_score, int overlap_type,
		int select_mode, int circle_len,
		SEXP nclist_sxp, int pp_is_q,
		IntAE *qh_buf, IntAE *sh_buf, int *direct_out)
{
	NCList nclist;
	const void *pp;
	GetYOverlapsFunType get_y_overlaps;

	if (q_len == 0 || s_len == 0)
		return 0;
	if (nclist_sxp == R_NilValue) {
		/* On-the-fly preprocessing. */
		pp_is_q = q_len < s_len;
		if (pp_is_q)
			build_NCList(&nclist, q_start_p, q_end_p,
					      q_subset_p, q_len);
		else 
			build_NCList(&nclist, s_start_p, s_end_p,
					      s_subset_p, s_len);
		pp = &nclist;
		get_y_overlaps = (GetYOverlapsFunType) NCList_get_y_overlaps;
	} else {
		pp = INTEGER(nclist_sxp);
		get_y_overlaps = (GetYOverlapsFunType) NCListSXP_get_y_overlaps;
	}
	pp_find_overlaps(
		q_start_p, q_end_p, q_space_p, q_subset_p, q_len,
		s_start_p, s_end_p, s_space_p, s_subset_p, s_len,
		min_overlap_score, overlap_type,
		select_mode, circle_len,
		pp, pp_is_q, get_y_overlaps,
		qh_buf, sh_buf, direct_out);
	if (nclist_sxp == R_NilValue)
		free_NCList(&nclist);
	return pp_is_q;
}


/****************************************************************************
 * Helper functions shared by NCList_find_overlaps() and
 * NCList_find_overlaps_in_groups()
 */

static int get_min_overlap_score(SEXP min_score)
{
	int min_overlap_score;

	if (!IS_INTEGER(min_score) || LENGTH(min_score) != 1)
		error("'min_score' must be a single integer");
	min_overlap_score = INTEGER(min_score)[0];
	if (min_overlap_score == NA_INTEGER)
		error("'min_score' cannot be NA");
	return min_overlap_score;
}

static int get_overlap_type(SEXP type)
{
	const char *type0;

	if (!IS_CHARACTER(type) || LENGTH(type) != 1)
		error("'type' must be a single string");
	type = STRING_ELT(type, 0);
	if (type == NA_STRING)
		error("'type' cannot be NA");
	type0 = CHAR(type);
	if (strcmp(type0, "any") == 0)
		return TYPE_ANY;
	if (strcmp(type0, "start") == 0)
		return TYPE_START;
	if (strcmp(type0, "end") == 0)
		return TYPE_END;
	if (strcmp(type0, "within") == 0)
		return TYPE_WITHIN;
	if (strcmp(type0, "extend") == 0)
		return TYPE_EXTEND;
	if (strcmp(type0, "equal") == 0)
		return TYPE_EQUAL;
	error("'type' must be \"any\", \"start\", \"end\", "
	      "\"within\", \"extend\", or \"equal\"");
	return 0;
}

static int get_circle_length(SEXP circle_length)
{
	int circle_len;

	if (!IS_INTEGER(circle_length) || LENGTH(circle_length) != 1)
		error("'circle_length' must be a single integer");
	circle_len = INTEGER(circle_length)[0];
	if (circle_len != NA_INTEGER && circle_len <= 0)
		error("'circle_length' must be a single "
                      "positive integer or NA");
	return circle_len;
}

static SEXP new_direct_out(int q_len, int select_mode)
{
	SEXP ans;
	int init_val, i, *ans_elt;

	PROTECT(ans = NEW_INTEGER(q_len));
	init_val = select_mode == COUNT_HITS ? 0 : NA_INTEGER;
	for (i = 0, ans_elt = INTEGER(ans); i < q_len; i++, ans_elt++)
		*ans_elt = init_val;
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * NCList_find_overlaps()
 *
 * --- .Call ENTRY POINT ---
 * Args:
 *   q_start, q_end: Integer vectors of same length.
 *   s_start, s_end: Integer vectors of same length.
 *   nclist:         An integer vector representing the Nested Containment
 *                   List for 'y'.
 *   nclist_is_q:    TRUE or FALSE.
 *   min_score:      See get_min_overlap_score() C function.
 *   type:           See get_overlap_type() C function.
 *   select:         See _get_select_mode() C function in S4Vectors.
 *   circle_length:  A single positive integer or NA_INTEGER.
 */
SEXP NCList_find_overlaps(SEXP q_start, SEXP q_end,
			  SEXP s_start, SEXP s_end,
			  SEXP nclist, SEXP nclist_is_q,
			  SEXP min_score, SEXP type, SEXP select,
			  SEXP circle_length)
{
	int q_len, s_len,
	    min_overlap_score, overlap_type, select_mode, circle_len,
	    *direct_out, pp_is_q;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	IntAE qh_buf, sh_buf;
	SEXP ans;

	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(q)", "end(q)");
	s_len = check_integer_pairs(s_start, s_end,
				    &s_start_p, &s_end_p,
				    "start(s)", "end(s)");
	min_overlap_score = get_min_overlap_score(min_score);
	overlap_type = get_overlap_type(type);
	select_mode = get_select_mode(select);
	circle_len = get_circle_length(circle_length);

	qh_buf = new_IntAE(0, 0, 0);
	sh_buf = new_IntAE(0, 0, 0);
	direct_out = NULL;
	if (select_mode != ALL_HITS) {
		PROTECT(ans = new_direct_out(q_len, select_mode));
		direct_out = INTEGER(ans);
	}
	//init_clock("find_overlaps: T2 = ");
	pp_is_q = find_overlaps(
		q_start_p, q_end_p, NULL, NULL, q_len,
		s_start_p, s_end_p, NULL, NULL, s_len,
		min_overlap_score, overlap_type,
		select_mode, circle_len,
		nclist, LOGICAL(nclist_is_q)[0],
		&qh_buf, &sh_buf, direct_out);
	//print_elapsed_time();
	if (select_mode != ALL_HITS) {
		UNPROTECT(1);
		return ans;
	}
	return new_Hits(qh_buf.elts, sh_buf.elts, IntAE_get_nelt(&qh_buf),
			q_len, s_len, !pp_is_q);
}


/****************************************************************************
 * NCList_find_overlaps_in_groups()
 *
 * --- .Call ENTRY POINT ---
 * Args:
 *   q_start, q_end, q_space: Integer vectors of same length (or NULL for
 *                   'q_space').
 *   q_groups:       A CompressedIntegerList object of length NG1. Each list
 *                   element (integer vector) represents a group of 0-based
 *                   indices into 'q_start', 'q_end', and 'q_space'.
 *   s_start, s_end, s_space: Integer vectors of same length (or NULL for
 *                   's_space').
 *   s_groups:       A CompressedIntegerList object of length NG2. Each list
 *                   element (integer vector) represents a group of 0-based
 *                   indices into 's_start', 's_end', and 's_space'.
 *   nclists:        A list of length >= min(NG1, NG2). Each list element must
 *                   be NULL or an integer vector representing a Nested
 *                   Containment List.
 *   nclist_is_q:    A logical vector parallel to 'nclists'.
 *   min_score:      See get_min_overlap_score() C function.
 *   type:           See get_overlap_type() C function.
 *   select:         See _get_select_mode() C function in S4Vectors.
 *   circle_length:  An integer vector of length >= min(NG1, NG2) with positive
 *                   or NA values.
 */
SEXP NCList_find_overlaps_in_groups(
		SEXP q_start, SEXP q_end, SEXP q_space, SEXP q_groups,
		SEXP s_start, SEXP s_end, SEXP s_space, SEXP s_groups,
		SEXP nclists, SEXP nclist_is_q,
		SEXP min_score, SEXP type, SEXP select,
		SEXP circle_length)
{
	int q_len, s_len, NG1, NG2,
	    min_overlap_score, overlap_type, select_mode,
	    NG, i, qi_len, si_len, *direct_out;
	const int *q_start_p, *q_end_p, *q_space_p,
		  *s_start_p, *s_end_p, *s_space_p;
	CompressedIntsList_holder q_groups_holder, s_groups_holder;
	Ints_holder qi_group_holder, si_group_holder;
	IntAE qh_buf, sh_buf;
	SEXP ans;

	/* Check query. */
	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "q_start", "q_end");
	if (q_space == R_NilValue) {
		q_space_p = NULL;
	} else {
		if (LENGTH(q_space) != q_len)
			error("'q_space' must have the length of 'q_start'");
		q_space_p = INTEGER(q_space);
	}
	q_groups_holder = _hold_CompressedIntegerList(q_groups);
	NG1 = _get_length_from_CompressedIntsList_holder(&q_groups_holder);

	/* Check subject. */
	s_len = check_integer_pairs(s_start, s_end,
				    &s_start_p, &s_end_p,
				    "s_start", "s_end");
	if (s_space == R_NilValue) {
		s_space_p = NULL;
	} else {
		if (LENGTH(s_space) != s_len)
			error("'s_space' must have the length of 's_start'");
		s_space_p = INTEGER(s_space);
	}
	s_groups_holder = _hold_CompressedIntegerList(s_groups);
	NG2 = _get_length_from_CompressedIntsList_holder(&s_groups_holder);

	min_overlap_score = get_min_overlap_score(min_score);
	overlap_type = get_overlap_type(type);
	select_mode = get_select_mode(select);

	qh_buf = new_IntAE(0, 0, 0);
	sh_buf = new_IntAE(0, 0, 0);
	direct_out = NULL;
	if (select_mode != ALL_HITS) {
		PROTECT(ans = new_direct_out(q_len, select_mode));
		direct_out = INTEGER(ans);
	}
	NG = NG1 <= NG2 ? NG1 : NG2;
	for (i = 0; i < NG; i++) {
		qi_group_holder = _get_elt_from_CompressedIntsList_holder(
					&q_groups_holder, i);
		qi_len = qi_group_holder.length;
		si_group_holder = _get_elt_from_CompressedIntsList_holder(
					&s_groups_holder, i);
		si_len = si_group_holder.length;
		find_overlaps(
			q_start_p, q_end_p, q_space_p,
			qi_group_holder.ptr, qi_len,
			s_start_p, s_end_p, s_space_p,
			si_group_holder.ptr, si_len,
			min_overlap_score, overlap_type,
			select_mode, INTEGER(circle_length)[i],
			VECTOR_ELT(nclists, i), LOGICAL(nclist_is_q)[i],
			&qh_buf, &sh_buf, direct_out);
	}
	if (select_mode != ALL_HITS) {
		UNPROTECT(1);
		return ans;
	}
	return new_Hits(qh_buf.elts, sh_buf.elts, IntAE_get_nelt(&qh_buf),
			q_len, s_len, 0);
}


/****************************************************************************
 Algorithm complexity
 ====================

   X: length of object to preprocess
   Y: length of other object
   H: nb of hits (upper bound is X * Y)

   Time of preprocessing:

     T1 = a * X * log(X)

   Time of find_overlaps(..., select="all"):

     T2 = b * Y * log(X) + c * H

   Total time T is T1 + T2.
 ****************************************************************************/

