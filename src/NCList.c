/****************************************************************************
 *                 A Nested Containment List implementation                 *
 *                                                                          *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h>  /* for malloc, realloc, free, qsort */
#include <math.h>    /* for log10 */


typedef struct pnclist {
	int buflength;           /* always >= 0 */
	int nelt;                /* always >= 0 and <= buflength */
	struct pnclistelt *elts;
} preNCList;

typedef struct pnclistelt {
	int i;
	struct pnclist *sublist;
} preNCListElt;


/****************************************************************************
 * preNCList_new()
 */

static void init_preNCList(preNCList *pnclist)
{
	pnclist->buflength = pnclist->nelt = 0;
	pnclist->elts = NULL;
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP preNCList_new()
{
	preNCList *top_pnclist;

	top_pnclist = (preNCList *) malloc(sizeof(preNCList));
	if (top_pnclist == NULL)
		error("preNCList_new: memory allocation failed");
	init_preNCList(top_pnclist);
	return R_MakeExternalPtr(top_pnclist, R_NilValue, R_NilValue);
}


/****************************************************************************
 * preNCList_free()
 */

static void free_preNCList(preNCList *pnclist)
{
	int n;
	const preNCListElt *elt;

	if (pnclist->buflength != 0) {
		for (n = 0, elt = pnclist->elts; n < pnclist->nelt; n++, elt++)
			free_preNCList(elt->sublist);
		free(pnclist->elts);
	}
	free(pnclist);
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP preNCList_free(SEXP pnclist)
{
	preNCList *top_pnclist;

	top_pnclist = (preNCList *) R_ExternalPtrAddr(pnclist);
	if (top_pnclist == NULL)
		error("preNCList_free: pointer to preNCList struct is NULL");
	free_preNCList(top_pnclist);
	R_SetExternalPtrAddr(pnclist, NULL);
	return R_NilValue;
}


/****************************************************************************
 * preNCList_build()
 */

static void init_preNCListElt(preNCListElt *elt, int i)
{
	elt->sublist = (preNCList *) malloc(sizeof(preNCList));
	if (elt->sublist == NULL)
		error("init_preNCListElt: memory allocation failed");
	elt->i = i;
	init_preNCList(elt->sublist);
	return;
}

static void extend_preNCList(preNCList *pnclist)
{
	int old_buflength, new_buflength;
	size_t elt_size;
	preNCListElt *new_elts;

	old_buflength = pnclist->buflength;
	elt_size = sizeof(preNCListElt);
	if (old_buflength == 0) {
		new_buflength = 4;
		new_elts = (preNCListElt *) malloc(elt_size * new_buflength);
	} else {
		if (old_buflength < 16384)
			new_buflength = 8 * old_buflength;
		else if (old_buflength < 4194304)
			new_buflength = 4 * old_buflength;
		else if (old_buflength < 67108864)
			new_buflength = 2 * old_buflength;
		else
			new_buflength = old_buflength + 33554432;
		new_elts = (preNCListElt *) realloc(pnclist->elts,
						    new_buflength * elt_size);
	}
	if (new_elts == NULL)
		error("extend_preNCList: memory allocation failed");
	pnclist->buflength = new_buflength;
	pnclist->elts = new_elts;
	return;
}

static preNCListElt *add_preNCList_elt(preNCList *pnclist, int i)
{
	preNCListElt *new_elt;

	if (pnclist->nelt == pnclist->buflength)
		extend_preNCList(pnclist);
	new_elt = pnclist->elts + pnclist->nelt;
	init_preNCListElt(new_elt, i);
	pnclist->nelt++;
	return new_elt;
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
 * overflows when running recursive code like get_overlaps(). A better
 * solution would be to not use recursive code at all when traversing an
 * NCList object. Then NCList objects of arbitrary depth could be supported
 * and it wouldn't be necessary to set the limit below.
 */
#define NCLIST_MAX_DEPTH 25000
static preNCListElt **stack = NULL;
static int stack_length = 0;

static void extend_stack()
{
	int new_length;
	preNCListElt **new_stack;

	if (stack_length == 0) {
		new_length = 1000;
		new_stack = (preNCListElt **) malloc(sizeof(preNCListElt *) *
						     new_length);
	} else {
		if (stack_length == NCLIST_MAX_DEPTH)
			error("extend_stack: cannot build an NCList object "
			      "of depth >= %d", NCLIST_MAX_DEPTH);
		if (stack_length <= NCLIST_MAX_DEPTH / 2)
			new_length = 2 * stack_length;
		else
			new_length = NCLIST_MAX_DEPTH;
		new_stack = (preNCListElt **) realloc(stack,
						      sizeof(preNCListElt *) *
						      new_length);
	}
	if (new_stack == NULL)
		error("extend_stack: memory allocation failed");
	stack_length = new_length;
	stack = new_stack;
	return;
}

static void build_preNCList(preNCList *top_pnclist,
			    const int *x_start, const int *x_end, int x_len)
{
	preNCListElt *new_elt;

	int *oo, k, d, i, current_end;

	// Determine order of 'x'. 'oo' will be such that 'x[oo]' is sorted
	// first by ascending start then by descending end.
	oo = (int *) R_alloc(sizeof(int), x_len);
	for (i = 0; i < x_len; i++)
		oo[i] = i;
	aa = x_start;
	bb = x_end;
	qsort(oo, x_len, sizeof(int), qsort_compar);

	init_preNCList(top_pnclist);
	for (k = 0, d = -1; k < x_len; k++) {
		i = oo[k];
		current_end = x_end[i];
		while (d >= 0 && x_end[stack[d]->i] < current_end)
			d--;  // unstack
		if (d == -1) {
			// append range i to top-level
			new_elt = add_preNCList_elt(top_pnclist, i);
		} else {
			// append range i to sublist of stack[d]
			new_elt = add_preNCList_elt(stack[d]->sublist, i);
		}
		if (++d == stack_length)
			extend_stack();
		stack[d] = new_elt;  // stack
	}
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP preNCList_build(SEXP pnclist, SEXP x_start, SEXP x_end)
{
	preNCList *top_pnclist;
	int x_len;
	const int *x_start_p, *x_end_p;

	top_pnclist = (preNCList *) R_ExternalPtrAddr(pnclist);
	if (top_pnclist == NULL)
		error("preNCList_build: pointer to preNCList struct is NULL");
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	build_preNCList(top_pnclist, x_start_p, x_end_p, x_len);
	return pnclist;
}


/****************************************************************************
 * new_NCList_from_preNCList()
 */

#define	NCLIST_NELT(nclist) ((nclist)[0])
#define	NCLIST_I(nclist, n) ((nclist)[((n)<<1)+1])
#define	NCSUBLIST_OFFSET(nclist, n) ((nclist)[((n)<<1)+2])

static int compute_length_of_preNCList_as_INTEGER(const preNCList *pnclist)
{
	int nelt, n;
	unsigned int ans_len, dump_len;
	const preNCListElt *elt;

	nelt = pnclist->nelt;
	if (nelt == 0)
		return 0;
	ans_len = 1U + 2U * (unsigned int) nelt;
	for (n = 0, elt = pnclist->elts; n < nelt; n++, elt++) {
		dump_len = compute_length_of_preNCList_as_INTEGER(elt->sublist);
		ans_len += dump_len;
		if (ans_len < dump_len)
			goto too_big;
	}
	if (ans_len <= INT_MAX)
		return (int) ans_len;
too_big:
	error("compute_length_of_preNCList_as_INTEGER: "
	      "preNCList object is too big to fit in an integer vector");
}

static int dump_preNCList_as_int_array(const preNCList *pnclist, int *out)
{
	int nelt, offset, dump_len, n;
	const preNCListElt *elt;

	nelt = pnclist->nelt;
	if (nelt == 0)
		return 0;
	offset = 1 + 2 * nelt;
	NCLIST_NELT(out) = nelt;
	for (n = 0, elt = pnclist->elts; n < nelt; n++, elt++) {
		NCLIST_I(out, n) = elt->i;
		dump_len = dump_preNCList_as_int_array(elt->sublist,
						       out + offset);
		NCSUBLIST_OFFSET(out, n) = dump_len != 0 ? offset : -1;
		offset += dump_len;
	}
	return offset;
}

/* --- .Call ENTRY POINT --- */
SEXP new_NCList_from_preNCList(SEXP pnclist)
{
	SEXP ans;
	const preNCList *top_pnclist;
	int ans_len;

	top_pnclist = (preNCList *) R_ExternalPtrAddr(pnclist);
	if (top_pnclist == NULL)
		error("new_NCList_from_preNCList: "
		      "pointer to preNCList struct is NULL");
	ans_len = compute_length_of_preNCList_as_INTEGER(top_pnclist);
	PROTECT(ans = NEW_INTEGER(ans_len));
	dump_preNCList_as_int_array(top_pnclist, INTEGER(ans));
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * NCList_print()
 */

/* Print 1 line per range in 'nclist'. Return max depth. */
static int print_NCList(const int *nclist,
			const int *x_start, const int *x_end,
			int depth, const char *format)
{
	int max_depth, nelt, n, d, i, offset, tmp;

	max_depth = depth;
	nelt = NCLIST_NELT(nclist);
	for (n = 0; n < nelt; n++) {
		for (d = 1; d < depth; d++)
			Rprintf("|");
		i = NCLIST_I(nclist, n);
		Rprintf(format, i + 1);
		Rprintf(": [%d, %d]\n", x_start[i], x_end[i]);
		offset = NCSUBLIST_OFFSET(nclist, n);
		if (offset != -1) {
			tmp = print_NCList(nclist + offset,
					   x_start, x_end, depth + 1,
					   format);
			if (tmp > max_depth)
				max_depth = tmp;
		}
	}
	return max_depth;
}

/* --- .Call ENTRY POINT --- */
SEXP NCList_print(SEXP x_nclist, SEXP x_start, SEXP x_end)
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
		sprintf(format, "%c%d%c", '%', max_digits, 'd');
		max_depth = print_NCList(top_nclist, x_start_p, x_end_p,
					 1, format);
	}
	Rprintf("max depth = %d\n", max_depth);
	return R_NilValue;
}


/****************************************************************************
 * find_overlaps()
 */

/* The 6 supported types of overlap. */
#define TYPE_ANY		1
#define TYPE_START		2
#define TYPE_END		3
#define TYPE_WITHIN		4
#define TYPE_EXTEND		5
#define TYPE_EQUAL		6

/* The 4 supported select modes. */
#define SELECT_ALL		1
#define SELECT_FIRST		2
#define SELECT_LAST		3
#define SELECT_ARBITRARY	4

typedef struct backpack {
	/* Members set by prepare_backpack(). */
	const int *y_start_p;
	const int *y_end_p;
	const int *y_space_p;
	int min_overlap_score;
	int x_extension;
	int overlap_type;
	int select_mode;
	int circle_len;
	IntAE *hits;

	/* Members set by update_backpack(). */
	int x_start;
	int x_end;
	int x_space;
	int ext_x_start;
	int ext_x_end;
	int *hit;
} Backpack;

static Backpack prepare_backpack(const int *y_start_p, const int *y_end_p,
				 const int *y_space_p, 
				 int min_overlap_score, int overlap_type,
				 int backpack_select_mode, int circle_len,
				 IntAE *hits)
{
	Backpack backpack;
	int x_extension;

	if (min_overlap_score >= 1) {
		x_extension = 0;
	} else {
		x_extension = 1 - min_overlap_score;
		min_overlap_score = 1;
	}
	backpack.y_start_p = y_start_p;
	backpack.y_end_p = y_end_p;
	backpack.y_space_p = y_space_p;
	backpack.min_overlap_score = min_overlap_score;
	backpack.x_extension = x_extension;
	backpack.overlap_type = overlap_type;
	backpack.select_mode = backpack_select_mode;
	backpack.circle_len = circle_len;
	backpack.hits = hits;
	return backpack;
}

static void update_backpack(Backpack *backpack, int x_start, int x_end,
			    int x_space, int *hit)
{
	int x_start0;

	if (backpack->circle_len != NA_INTEGER) {
		x_start0 = x_start;
		x_start %= backpack->circle_len;
		if (x_start <= 0)
			x_start += backpack->circle_len;
		x_end += x_start - x_start0;
	}
	backpack->x_start = x_start;
	backpack->x_end = x_end;
	backpack->x_space = x_space;
	backpack->ext_x_start = x_start - backpack->x_extension;
	backpack->ext_x_end = x_end + backpack->x_extension;
	backpack->hit = hit;
	return;
}

static void shift_x(Backpack *backpack, int shift)
{
	backpack->x_start += shift;
	backpack->x_end += shift;
	backpack->ext_x_start += shift;
	backpack->ext_x_end += shift;
	return;
}

static int bsearch_n1(int x_start, const int *nclist, const int *y_end_p)
{
	int n1, n2, nelt, n, end;

	/* Check first element. */
	n1 = 0;
	end = y_end_p[NCLIST_I(nclist, n1)];
	if (end >= x_start)
		return n1;

	/* Check last element. */
	nelt = NCLIST_NELT(nclist);
	n2 = nelt - 1;
	end = y_end_p[NCLIST_I(nclist, n2)];
	if (end < x_start)
		return nelt;
	if (end == x_start)
		return n2;

	/* Binary search. */
	while ((n = (n1 + n2) / 2) != n1) {
		end = y_end_p[NCLIST_I(nclist, n)];
		if (end == x_start)
			return n;
		if (end < x_start)
			n1 = n;
		else
			n2 = n;
	}
	return n2;
}

/* Recursive! */
static void get_overlaps(const int *nclist, Backpack *backpack)
{
	int nelt, n;
	/* Unlike the 2 above variables, the variables below don't need to go
	   on the stack. Shouldn't we make them static? That would reduce the
	   number of stuff that goes on the stack and could reduce the overhead
	   due to stacking. Would that make a difference when the recursive
	   calls go deep? TODO: Investigate this. */
	int i, y_start,
	    ok, y_space, y_end, ov_start, ov_end, score, d,
	    i1, current_sel, update_sel,
	    offset;

	nelt = NCLIST_NELT(nclist);
	n = bsearch_n1(backpack->ext_x_start, nclist, backpack->y_end_p);
	for ( ; n < nelt; n++) {
		i = NCLIST_I(nclist, n);
		y_start = backpack->y_start_p[i];
		if (backpack->ext_x_end < y_start)
			break;
		/* Check the space */
		if (backpack->y_space_p != NULL) {
			y_space = backpack->y_space_p[i];
			ok = y_space == 0 ||
			     backpack->x_space == 0 ||
			     backpack->x_space == y_space;
			if (!ok) goto sorry;
		}
		y_end = backpack->y_end_p[i];
		/* Check the score */
		if (backpack->min_overlap_score != 1) {
			ov_start = backpack->x_start > y_start ?
				   backpack->x_start : y_start;
			ov_end   = backpack->x_end < y_end ?
				   backpack->x_end : y_end;
			score = ov_end - ov_start + 1;
			ok = score >= backpack->min_overlap_score;
			if (!ok) goto sorry;
		}
		/* Check the type */
		if (backpack->overlap_type != TYPE_ANY) {
			switch (backpack->overlap_type) {
			    case TYPE_START:
				ok = backpack->x_start == y_start;
				break;
			    case TYPE_END:
				d = backpack->x_end - y_end;
				if (backpack->circle_len != NA_INTEGER)
					d %= backpack->circle_len;
				ok = d == 0;
				break;
			    case TYPE_WITHIN:
				ok = backpack->x_start >= y_start &&
				     backpack->x_end <= y_end;
				break;
			    case TYPE_EXTEND:
				ok = backpack->x_start <= y_start &&
				     backpack->x_end >= y_end;
				break;
			    case TYPE_EQUAL:
				ok = backpack->x_start == y_start &&
				     backpack->x_end == y_end;
				break;
			    default:
				ok = 1;
			}
			if (!ok) goto sorry;
		}
		i1 = i + 1;
		if (backpack->select_mode == SELECT_ALL) {
			/* Report the hit. */
			IntAE_insert_at(backpack->hits,
					IntAE_get_nelt(backpack->hits), i1);
		} else {
			/* Update current selection if necessary. */
			current_sel = *backpack->hit;
			update_sel = current_sel == NA_INTEGER ||
				(backpack->select_mode == SELECT_FIRST) ==
				(i1 < current_sel);
			if (update_sel)
				*backpack->hit = i1;
			if (backpack->select_mode == SELECT_ARBITRARY)
				break;
		}
		sorry:
		offset = NCSUBLIST_OFFSET(nclist, n);
		if (offset != -1)
			get_overlaps(nclist + offset, backpack);
	}
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

static void select_hits(int *sh, const IntAE *qh_buf, const IntAE *sh_buf,
			int select_mode)
{
	int nelt, i, i0, i1, current_sel, update_sel;

	nelt = IntAE_get_nelt(qh_buf);
	for (i = 0; i < nelt; i++) {
		i0 = qh_buf->elts[i] - 1L;
		current_sel = sh[i0];
		i1 = sh_buf->elts[i];
		update_sel = current_sel == NA_INTEGER ||
			select_mode != SELECT_ARBITRARY &&
			(select_mode == SELECT_FIRST) == (i1 < current_sel);
		if (update_sel)
			sh[i0] = i1;
	}
	return;
}

static void find_overlaps(const int *q_start_p, const int *q_end_p,
			  const int *q_space_p, int q_len,
			  const int *s_start_p, const int *s_end_p,
			  const int *s_space_p, int s_len,
			  const int *nclist, int nclist_is_q,
			  int min_overlap_score, int overlap_type,
			  int select_mode, int circle_len,
			  IntAE *qh_buf, IntAE *sh_buf, int *sh)
{
	const int *x_start_p, *x_end_p, *x_space_p,
		  *y_start_p, *y_end_p, *y_space_p;
	int x_len, backpack_select_mode, i, *yh, old_nhit, new_nhit, k;
	IntAE *xh_buf, *yh_buf;
	Backpack backpack;

	if (s_len == 0 || q_len == 0)
		return;
	if (nclist_is_q) {
		x_start_p = s_start_p;
		x_end_p = s_end_p;
		x_space_p = s_space_p;
		x_len = s_len;
		xh_buf = sh_buf;
		y_start_p = q_start_p;
		y_end_p = q_end_p;
		y_space_p = q_space_p;
		yh_buf = qh_buf;
		if (overlap_type == TYPE_WITHIN)
			overlap_type = TYPE_EXTEND;
		else if (overlap_type == TYPE_EXTEND)
			overlap_type = TYPE_WITHIN;
		backpack_select_mode = SELECT_ALL;
	} else {
		x_start_p = q_start_p;
		x_end_p = q_end_p;
		x_space_p = q_space_p;
		x_len = q_len;
		xh_buf = qh_buf;
		y_start_p = s_start_p;
		y_end_p = s_end_p;
		y_space_p = s_space_p;
		yh_buf = sh_buf;
		backpack_select_mode = select_mode;
	}
	backpack = prepare_backpack(y_start_p, y_end_p, y_space_p,
				    min_overlap_score, overlap_type,
				    backpack_select_mode, circle_len,
				    yh_buf);
	for (i = 1, yh = sh;
	     i <= x_len;
	     i++, x_start_p++, x_end_p++, x_space_p++, yh++)
	{
		update_backpack(&backpack, *x_start_p, *x_end_p,
				y_space_p == NULL ? 0 : *x_space_p,
				yh);
		/* pass 0 */
		get_overlaps(nclist, &backpack);
		if (circle_len == NA_INTEGER)
			goto life_is_good;
		if (backpack_select_mode == SELECT_ARBITRARY
		 && *yh != NA_INTEGER)
			goto life_is_good;
		/* pass 1 */
		shift_x(&backpack, - circle_len);
		get_overlaps(nclist, &backpack);
		if (backpack_select_mode == SELECT_ARBITRARY
		 && *yh != NA_INTEGER)
			goto life_is_good;
		/* pass 2 */
		shift_x(&backpack, 2 * circle_len);
		get_overlaps(nclist, &backpack);

		life_is_good:
		if (backpack_select_mode == SELECT_ALL) {
			old_nhit = IntAE_get_nelt(xh_buf);
			new_nhit = IntAE_get_nelt(yh_buf);
			if (circle_len != NA_INTEGER) {
				IntAE_delete_duplicates(yh_buf,
							old_nhit, new_nhit);
				new_nhit = IntAE_get_nelt(yh_buf);
			}
			for (k = old_nhit; k < new_nhit; k++)
				IntAE_insert_at(xh_buf, k, i);
		}
	}
	if (nclist_is_q && select_mode != SELECT_ALL)
		select_hits(sh, qh_buf, sh_buf, select_mode);
	return;
}


/****************************************************************************
 * Check user-supplied input.
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

static int get_select_mode(SEXP select)
{
	const char *select0;

	if (!IS_CHARACTER(select) || LENGTH(select) != 1)
		error("'select' must be a single string");
	select = STRING_ELT(select, 0);
	if (select == NA_STRING)
		error("'select' cannot be NA");
	select0 = CHAR(select);
	if (strcmp(select0, "all") == 0)
		return SELECT_ALL;
	if (strcmp(select0, "first") == 0)
		return SELECT_FIRST;
	if (strcmp(select0, "last") == 0)
		return SELECT_LAST;
	if (strcmp(select0, "arbitrary") == 0)
		return SELECT_ARBITRARY;
	error("'select' must be \"all\", \"first\", "
	      "\"last\", or \"arbitrary\"");
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


/****************************************************************************
 * Move raw hits from buffers to Hits object or integer vector
 */

static void fill_with_val(int *x, int x_len, int val)
{
	int i;

	for (i = 0; i < x_len; i++)
		*(x++) = val;
	return;
}

static void sort_hits(int *q_hits, int *s_hits, int n)
{
	int *out, *tmp, i;

	out = (int *) malloc(sizeof(int) * n * 2);
	if (out == NULL)
		error("sort_hits: memory allocation failed");
	get_order_of_int_array(q_hits, n, 0, out, 0);
	tmp = out + n;
	for (i = 0; i < n; i++)
		tmp[i] = q_hits[out[i]];
	memcpy(q_hits, tmp, sizeof(int) * n);
	for (i = 0; i < n; i++)
		tmp[i] = s_hits[out[i]];
	memcpy(s_hits, tmp, sizeof(int) * n);
	free(out);
	return;
}

static SEXP new_Hits_from_IntAEs(const IntAE *qh_buf, const IntAE *sh_buf,
				 int q_len, int s_len)
{
	SEXP classdef, ans,
	     ans_queryHits, ans_subjectHits,
	     ans_queryLength, ans_subjectLength;

	PROTECT(classdef = MAKE_CLASS("Hits"));
	PROTECT(ans = NEW_OBJECT(classdef));

	PROTECT(ans_queryHits = new_INTEGER_from_IntAE(qh_buf));
	SET_SLOT(ans, install("queryHits"), ans_queryHits);
	UNPROTECT(1);

	PROTECT(ans_subjectHits = new_INTEGER_from_IntAE(sh_buf));
	SET_SLOT(ans, install("subjectHits"), ans_subjectHits);
	UNPROTECT(1);

	PROTECT(ans_queryLength = ScalarInteger(q_len));
	SET_SLOT(ans, install("queryLength"), ans_queryLength);
	UNPROTECT(1);

	PROTECT(ans_subjectLength = ScalarInteger(s_len));
	SET_SLOT(ans, install("subjectLength"), ans_subjectLength);
	UNPROTECT(1);

	UNPROTECT(2);
	return ans;
}


/****************************************************************************
 * NCList_find_overlaps()
 *
 * --- .Call ENTRY POINT ---
 * Args:
 *   q_start, q_end: Integer vectors of length M.
 *   s_start, s_end: Integer vectors of length N.
 *   nclist:         An integer vector representing the Nested Containment
 *                   List for 'y'.
 *   nclist_is_q:    TRUE or FALSE.
 *   min_score:      See get_min_overlap_score() C function.
 *   type:           See get_overlap_type() C function.
 *   select:         See get_select_mode() C function.
 *   circle_length:  A single positive integer or NA_INTEGER.
 */
SEXP NCList_find_overlaps(SEXP q_start, SEXP q_end,
			  SEXP s_start, SEXP s_end,
			  SEXP nclist, SEXP nclist_is_q,
			  SEXP min_score, SEXP type, SEXP select,
			  SEXP circle_length)
{
	int q_len, s_len, nclist_is_q0,
	    min_overlap_score, overlap_type, select_mode, circle_len, *sh;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	IntAE qh_buf, sh_buf;
	SEXP ans;

	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(q)", "end(q)");
	s_len = check_integer_pairs(s_start, s_end,
				    &s_start_p, &s_end_p,
				    "start(s)", "end(s)");
	nclist_is_q0 = LOGICAL(nclist_is_q)[0];
	min_overlap_score = get_min_overlap_score(min_score);
	overlap_type = get_overlap_type(type);
	select_mode = get_select_mode(select);
	circle_len = get_circle_length(circle_length);
	qh_buf = new_IntAE(0, 0, 0);
	sh_buf = new_IntAE(0, 0, 0);
	if (select_mode != SELECT_ALL) {
		PROTECT(ans = NEW_INTEGER(q_len));
		sh = INTEGER(ans);
		fill_with_val(sh, q_len, NA_INTEGER);
	}
	find_overlaps(q_start_p, q_end_p, NULL, q_len,
		      s_start_p, s_end_p, NULL, s_len,
		      INTEGER(nclist), nclist_is_q0,
		      min_overlap_score, overlap_type,
		      select_mode, circle_len,
		      &qh_buf, &sh_buf, sh);
	if (select_mode != SELECT_ALL) {
		UNPROTECT(1);
		return ans;
	}
	if (nclist_is_q0)
		sort_hits(qh_buf.elts, sh_buf.elts, IntAE_get_nelt(&sh_buf));
	return new_Hits_from_IntAEs(&qh_buf, &sh_buf, q_len, s_len);
}


/****************************************************************************
 * NCLists_find_overlaps()
 */

static void set_end_buf(IntAE *end_buf,
		const int *start_p, const int *width_p, int len)
{
	int i;

	IntAE_set_nelt(end_buf, 0);
	for (i = 0; i < len; i++, start_p++, width_p++)
		IntAE_insert_at(end_buf, i, *start_p + *width_p - 1);
	return;
}

static SEXP make_ans_elt(int i,
		const CompressedIRangesList_holder *q_holder, 
		const CompressedIRangesList_holder *s_holder,
		SEXP nclists, int nclists_is_q,
		int min_overlap_score, int overlap_type,
		int select_mode, SEXP circle_length,
		IntAE *qi_end_buf, IntAE *si_end_buf,
		IntAE *qh_buf, IntAE *sh_buf)
{
	int q_len, s_len, qi_len, si_len, circle_len, *sh;
	IRanges_holder qi_holder, si_holder;
	SEXP ans, nclist;
	const int *qi_start_p, *qi_width_p, *si_start_p, *si_width_p;

	q_len = _get_length_from_CompressedIRangesList_holder(q_holder);
	s_len = _get_length_from_CompressedIRangesList_holder(s_holder);
	if (i < q_len) {
		qi_holder = _get_elt_from_CompressedIRangesList_holder(
					q_holder, i);
		qi_len = _get_length_from_IRanges_holder(&qi_holder);
		/* UGLY HACK! IRanges_holder should always be handled as an
		   opaque struct so we should not access its members. We do it
		   here because we know it's safe. However, future changes to
		   the IRanges_holder struct and/or to the behavior of
		   _get_elt_from_CompressedIRangesList_holder() could break
		   our ugly hack. */
		qi_start_p = qi_holder.start;
		qi_width_p = qi_holder.width;
		set_end_buf(qi_end_buf, qi_start_p, qi_width_p, qi_len);
	} else {
		qi_len = 0;
	}
	if (i < s_len) {
		si_holder = _get_elt_from_CompressedIRangesList_holder(
					s_holder, i);
		si_len = _get_length_from_IRanges_holder(&si_holder);
		si_start_p = si_holder.start;
		si_width_p = si_holder.width;
		set_end_buf(si_end_buf, si_start_p, si_width_p, si_len);
	} else {
		si_len = 0;
	}
	nclist = VECTOR_ELT(nclists, i);
	circle_len = INTEGER(circle_length)[i];
	if (circle_len != NA_INTEGER && circle_len <= 0)
		error("'circle_length' must be NA or "
		      "a single positive integer");
	IntAE_set_nelt(qh_buf, 0);
	IntAE_set_nelt(sh_buf, 0);
	if (select_mode != SELECT_ALL) {
		PROTECT(ans = NEW_INTEGER(qi_len));
		sh = INTEGER(ans);
		fill_with_val(sh, qi_len, NA_INTEGER);
	}
	find_overlaps(qi_start_p, qi_end_buf->elts, NULL, qi_len,
		      si_start_p, si_end_buf->elts, NULL, si_len,
		      INTEGER(nclist), nclists_is_q,
		      min_overlap_score, overlap_type,
		      select_mode, circle_len,
		      qh_buf, sh_buf, sh);
	if (select_mode != SELECT_ALL) {
		UNPROTECT(1);
		return ans;
	}
	if (nclists_is_q)
		sort_hits(qh_buf->elts, sh_buf->elts, IntAE_get_nelt(sh_buf));
	return new_Hits_from_IntAEs(qh_buf, sh_buf, qi_len, si_len);
}

/* --- .Call ENTRY POINT ---
 * Args:
 *   q, s:           2 parallel CompressedIRangesList objects (one possibly
 *                   longer than the other).
 *   nclists:        A list of integer vectors. Each integer vector represents
 *                   a Nested Containment List.
 *   nclists_is_q:   TRUE or FALSE.
 *   min_score:      See get_min_overlap_score() C function.
 *   type:           See get_overlap_type() C function.
 *   select:         See get_select_mode() C function.
 *   circle_length:  An integer vector parallel to 'x' and 'y' with positive
 *                   or NA values.
 */
SEXP NCLists_find_overlaps(SEXP q, SEXP s,
		SEXP nclists, SEXP nclists_is_q,
		SEXP min_score, SEXP type, SEXP select,
		SEXP circle_length)
{
	CompressedIRangesList_holder q_holder, s_holder;
	int q_len, s_len, nclists_is_q0,
	    min_overlap_score, overlap_type, select_mode,
	    min_len, ans_len, i;
	IntAE qi_end_buf, si_end_buf, qh_buf, sh_buf;
	SEXP ans, ans_elt;

	q_holder = _hold_CompressedIRangesList(q);
	s_holder = _hold_CompressedIRangesList(s);
	q_len = _get_length_from_CompressedIRangesList_holder(&q_holder);
	s_len = _get_length_from_CompressedIRangesList_holder(&s_holder);
	nclists_is_q0 = LOGICAL(nclists_is_q)[0];
	min_overlap_score = get_min_overlap_score(min_score);
	overlap_type = get_overlap_type(type);
	select_mode = get_select_mode(select);
	qi_end_buf = new_IntAE(0, 0, 0);
	si_end_buf = new_IntAE(0, 0, 0);
	if (select_mode == SELECT_ALL) {
		qh_buf = new_IntAE(0, 0, 0);
		sh_buf = new_IntAE(0, 0, 0);
	}
	min_len = q_len <= s_len ? q_len : s_len;
	ans_len = select_mode == SELECT_ALL ? min_len : q_len;
	PROTECT(ans = NEW_LIST(ans_len));
	for (i = 0; i < ans_len; i++) {
		PROTECT(ans_elt = make_ans_elt(i, &q_holder, &s_holder,
					nclists, nclists_is_q0,
					min_overlap_score, overlap_type,
					select_mode, circle_length,
					&qi_end_buf, &si_end_buf,
					&qh_buf, &sh_buf));
		SET_VECTOR_ELT(ans, i, ans_elt);
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * NCList_find_overlaps_by_group_and_combine()
 */

static void subset1(const int *in, const int *idx, int idx_len, int *out)
{
	int i;

	in--;
	for (i = 0; i < idx_len; i++, idx++, out++)
		*out = in[*idx];
	return;
}

static void subset2(const int *in, const int *idx, int idx_len, IntAE *out)
{
	int i;

	in--;
	for (i = 0; i < idx_len; i++, idx++)
		IntAE_insert_at(out, i, in[*idx]);
	return;
}

static void extract_group(const Ints_holder *group_holder,
			  const int *start_p, IntAE *start_buf,
			  const int *width_p, IntAE *end_buf,
			  const int *space_p, IntAE *space_buf)
{
	int i;

	IntAE_set_nelt(start_buf, 0);
	subset2(start_p, group_holder->ptr, group_holder->length, start_buf);
	IntAE_set_nelt(end_buf, 0);
	subset2(width_p, group_holder->ptr, group_holder->length, end_buf);
	for (i = 0; i < group_holder->length; i++)
		end_buf->elts[i] += start_buf->elts[i] - 1L;
	if (space_p != NULL) {
		IntAE_set_nelt(space_buf, 0);
		subset2(space_p, group_holder->ptr,
				 group_holder->length,
				 space_buf);
	}
	return;
}

/* --- .Call ENTRY POINT ---
 * Args:
 *   q_start, q_width, q_space: Integer vectors of length M (or NULL for
 *                   'q_space').
 *   q_groups:       A CompressedIntegerList object of length NG1. Each list
 *                   element (integer vector) represents a group of indices in
 *                   'q_start'.
 *   s_start, s_width, s_space: Integer vectors of length N (or NULL for
 *                   's_space').
 *   s_groups:       A CompressedIntegerList object of length NG2. Each list
 *                   element (integer vector) represents a group of indices in
 *                   's_start'.
 *   nclists:        A list of integer vectors of length >= min(NG1, NG2).
 *                   Each integer vector represents a Nested Containment List.
 *   nclist_is_q:    A logical vector parallel to 'nclists'.
 *   min_score:      See get_min_overlap_score() C function.
 *   type:           See get_overlap_type() C function.
 *   select:         See get_select_mode() C function.
 *   circle_length:  An integer vector of length >= min(NG1, NG2) with positive
 *                   or NA values.
 */
SEXP NCList_find_overlaps_by_group_and_combine(
		SEXP q_start, SEXP q_width, SEXP q_space, SEXP q_groups,
		SEXP s_start, SEXP s_width, SEXP s_space, SEXP s_groups,
		SEXP nclists, SEXP nclist_is_q,
		SEXP min_score, SEXP type, SEXP select,
		SEXP circle_length)
{
	int q_len, s_len, NG1, NG2,
	    min_overlap_score, overlap_type, select_mode,
	    NG, i, old_nhit, nhit;
	const int *q_start_p, *q_width_p, *q_space_p,
		  *s_start_p, *s_width_p, *s_space_p;
	CompressedIntsList_holder q_groups_holder, s_groups_holder;
	Ints_holder qi_group_holder, si_group_holder;
	IntAE qi_start_buf, qi_end_buf, qi_space_buf,
	      si_start_buf, si_end_buf, si_space_buf,
	      qh_buf, sh_buf;
	SEXP ans, nclist;

	/* Check query. */
	q_len = check_integer_pairs(q_start, q_width,
				    &q_start_p, &q_width_p,
				    "start(query)", "width(query)");
	if (q_space == R_NilValue) {
		q_space_p = NULL;
		qi_space_buf.elts = NULL;
	} else {
		if (LENGTH(q_space) != q_len)
			error("'query.space' must have the length "
			      "of 'query'");
		q_space_p = INTEGER(q_space);
		qi_space_buf = new_IntAE(0, 0, 0);
	}
	q_groups_holder = _hold_CompressedIntegerList(q_groups);
	NG1 = _get_length_from_CompressedIntsList_holder(&q_groups_holder);

	/* Check subject. */
	s_len = check_integer_pairs(s_start, s_width,
				    &s_start_p, &s_width_p,
				    "start(s)", "width(s)");
	if (s_space == R_NilValue) {
		s_space_p = NULL;
		si_space_buf.elts = NULL;
	} else {
		if (LENGTH(s_space) != s_len)
			error("'subject.space' must have the length "
			      "of 'subject'");
		s_space_p = INTEGER(s_space);
		si_space_buf = new_IntAE(0, 0, 0);
	}
	s_groups_holder = _hold_CompressedIntegerList(s_groups);
	NG2 = _get_length_from_CompressedIntsList_holder(&s_groups_holder);

	min_overlap_score = get_min_overlap_score(min_score);
	overlap_type = get_overlap_type(type);
	select_mode = get_select_mode(select);

	qi_start_buf = new_IntAE(0, 0, 0);
	qi_end_buf = new_IntAE(0, 0, 0);
	si_start_buf = new_IntAE(0, 0, 0);
	si_end_buf = new_IntAE(0, 0, 0);
	qh_buf = new_IntAE(0, 0, 0);
	sh_buf = new_IntAE(0, 0, 0);

	NG = NG1 <= NG2 ? NG1 : NG2;
	for (i = 0; i < NG; i++) {
		/* Extract i-th group from query. */
		qi_group_holder = _get_elt_from_CompressedIntsList_holder(
					&q_groups_holder, i);
		extract_group(&qi_group_holder,
					q_start_p, &qi_start_buf,
					q_width_p, &qi_end_buf,
					q_space_p, &qi_space_buf);

		/* Extract i-th group from subject. */
		si_group_holder = _get_elt_from_CompressedIntsList_holder(
					&s_groups_holder, i);
		extract_group(&si_group_holder,
					s_start_p, &si_start_buf,
					s_width_p, &si_end_buf,
					s_space_p, &si_space_buf);

		nclist = VECTOR_ELT(nclists, i);

		old_nhit = IntAE_get_nelt(&qh_buf);
		find_overlaps(qi_start_buf.elts, qi_end_buf.elts,
			      qi_space_buf.elts, qi_group_holder.length,
			      si_start_buf.elts, si_end_buf.elts,
			      si_space_buf.elts, si_group_holder.length,
			      INTEGER(nclist), LOGICAL(nclist_is_q)[i],
			      min_overlap_score, overlap_type,
			      SELECT_ALL, INTEGER(circle_length)[i],
			      &qh_buf, &sh_buf, NULL);
		nhit = IntAE_get_nelt(&qh_buf) - old_nhit;

		/* Remap new hits. */
		subset1(qi_group_holder.ptr, qh_buf.elts + old_nhit, nhit,
					qh_buf.elts + old_nhit);
		subset1(si_group_holder.ptr, sh_buf.elts + old_nhit, nhit,
					sh_buf.elts + old_nhit);
	}
	if (select_mode != SELECT_ALL) {
		PROTECT(ans = NEW_INTEGER(q_len));
		fill_with_val(INTEGER(ans), q_len, NA_INTEGER);
		select_hits(INTEGER(ans), &qh_buf, &sh_buf, select_mode);
		UNPROTECT(1);
		return ans;
		
	}
	sort_hits(qh_buf.elts, sh_buf.elts, IntAE_get_nelt(&qh_buf));
	return new_Hits_from_IntAEs(&qh_buf, &sh_buf, q_len, s_len);
}

