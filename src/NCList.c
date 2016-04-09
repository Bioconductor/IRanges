/****************************************************************************
 *                 A Nested Containment List implementation                 *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h>  /* for malloc, realloc, free, qsort, abs */
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


/****************************************************************************
 * A simple wrapper to realloc()
 */

/* 'new_nmemb' must be > 'old_nmemb'. */
static void *realloc2(void *ptr, int new_nmemb, int old_nmemb, size_t size)
{
	void *new_ptr;

	if (new_nmemb <= old_nmemb)
		error("IRanges internal error in realloc2(): "
		      "'new_nmemb' <= 'old_nmemb'");
	size *= new_nmemb;
	if (old_nmemb == 0) {
		new_ptr = malloc(size);
	} else {
		new_ptr = realloc(ptr, size);
	}
	if (new_ptr == NULL)
		error("IRanges internal error in realloc2(): "
		      "memory (re)allocation failed");
	return new_ptr;
}

static int get_new_maxdepth(int maxdepth)
{
	return maxdepth == 0 ? 1000 : 2 * maxdepth;
}


/****************************************************************************
 * NCList structure
 */

/* sizeof(NCList) is 24 bytes (0x18 bytes) */
typedef struct nclist_t {
	int buflength;	/* >= 0 */
	int nchildren;	/* >= 0 and <= buflength */
	int *revmap;	/* Of length 'nchildren'. Reverse mapping into Ranges
			   object 'x'. Contains 0-based indices */
	struct nclist_t *childrenbuf;  /* Of length 'buflength'. */
} NCList;

/*
static void print_NCList_node(const NCList *nclist, int depth)
{
	int d, n;

	for (d = 0; d < depth; d++) printf("-"); printf(" ");
	printf("NCList node at address %p:\n", nclist);

	for (d = 0; d < depth; d++) printf("-"); printf(" ");
	printf("  buflength=%d; nchildren=%d\n",
	       nclist->buflength, nclist->nchildren);

	for (d = 0; d < depth; d++) printf("-"); printf(" ");
	printf("  revmap:");
	for (n = 0; n < nclist->nchildren; n++)
		printf(" %d", nclist->revmap[n]);
	printf("\n");
	return;
}

static void print_NCList_rec(const NCList *nclist, int depth)
{
	int n;

	print_NCList_node(nclist, depth);
	for (n = 0; n < nclist->nchildren; n++)
		print_NCList_rec(nclist->childrenbuf + n, depth + 1);
	return;
}
*/

static void init_NCList(NCList *nclist)
{
	nclist->buflength = nclist->nchildren = 0;
	return;
}


/****************************************************************************
 * Utilities to walk on an NCList structure non-recursively.
 *
 */

typedef struct NCList_stack_elt_t {
	const NCList *parent_nclist;
	int n;  /* point to n-th child of 'parent_nclist' */
} NCListStackElt;

#define	GET_REVIDX(stack_elt) \
	((stack_elt)->parent_nclist->revmap[(stack_elt)->n])
#define	GET_NCLIST(stack_elt) \
	((stack_elt)->parent_nclist->childrenbuf + (stack_elt)->n)

static NCListStackElt *NCList_stack = NULL;
static int NCList_stack_maxdepth = 0;
static int NCList_stack_depth = 0;

#define	RESET_NCLIST_STACK() NCList_stack_depth = 0

/*
static void print_NCList_stack()
{
	int d;

	printf("NCList_stack:");
	for (d = 0; d < NCList_stack_depth; d++)
		printf(" %d", NCList_stack[d].n);
	printf("\n");
	return;
}
*/

/* Must NOT be called when 'NCList_stack_depth' is 0 (i.e. empty stack). */
static NCListStackElt *pop_NCListStackElt()
{
	NCList_stack_depth--;
	return NCList_stack + NCList_stack_depth;
}

/* Must NOT be called when 'NCList_stack_depth' is 0 (i.e. empty stack). */
static NCListStackElt *peek_NCListStackElt()
{
	return NCList_stack + NCList_stack_depth - 1;
}

/* Must NOT be called when 'NCList_stack_depth' is 0 (i.e. empty stack). */
static int get_current_revidx()
{
	NCListStackElt *stack_elt;

	stack_elt = peek_NCListStackElt();
	return GET_REVIDX(stack_elt);
}

static void extend_NCList_stack()
{
	int new_maxdepth;

	new_maxdepth = get_new_maxdepth(NCList_stack_maxdepth);
	NCList_stack = (NCListStackElt *) realloc2(NCList_stack,
						   new_maxdepth,
						   NCList_stack_maxdepth,
						   sizeof(NCListStackElt));
	NCList_stack_maxdepth = new_maxdepth;
	return;
}

/* Return a pointer to n-th child. */
static const NCList *move_to_child(const NCList *parent_nclist, int n)
{
	NCListStackElt *stack_elt;

	if (NCList_stack_depth == NCList_stack_maxdepth)
		extend_NCList_stack();
	stack_elt = NCList_stack + NCList_stack_depth++;
	stack_elt->parent_nclist = parent_nclist;
	stack_elt->n = n;
	return GET_NCLIST(stack_elt);
}

static const NCList *move_right()
{
	NCListStackElt *stack_elt;
	const NCList *parent_nclist;

	do {
		if (NCList_stack_depth == 0)
			return NULL;
		stack_elt = pop_NCListStackElt();
		parent_nclist = stack_elt->parent_nclist;
	} while (++(stack_elt->n) == parent_nclist->nchildren);
	NCList_stack_depth++;
	return GET_NCLIST(stack_elt);
}

static const NCList *move_down(const NCList *nclist)
{
	while (nclist->nchildren != 0)
		nclist = move_to_child(nclist, 0);
	return nclist;
}

/*
   Top-down walk: parent is processed before children.
   For a full (i.e. on all nodes) top-down walk, do:

	RESET_NCLIST_STACK();
	for (nclist = top_nclist;
	     nclist != NULL;
	     nclist = next_top_down(nclist))
	{
		process nclist
	}
 */
static const NCList *next_top_down(const NCList *nclist)
{
	/* Try to move to first child, if any. */
	if (nclist->nchildren != 0)
		return move_to_child(nclist, 0);
	/* Move up (i.e. pop stack elements). */
	return move_right();
}

/*
   Bottom-up walk: children are processed before parent.
   For a full (i.e. on all nodes) bottom-up walk, do:

	RESET_NCLIST_STACK();
	for (nclist = move_down(top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up())
	{
		process nclist
	}
*/
static const NCList *next_bottom_up()
{
	NCListStackElt *stack_elt;
	const NCList *parent_nclist;

	if (NCList_stack_depth == 0)
		return NULL;
	stack_elt = peek_NCListStackElt();
	parent_nclist = stack_elt->parent_nclist;
	if (++(stack_elt->n) < parent_nclist->nchildren) {
		/* Move down thru the next children. */
		return move_down(GET_NCLIST(stack_elt));
	}
	/* All children have been processed --> move 1 level up. */
	NCList_stack_depth--;
	return parent_nclist;
}

/*
static void test_top_down_walk(const NCList *top_nclist)
{
	const NCList *nclist;

	printf("======= START top-down walk ========\n");
	RESET_NCLIST_STACK();
	for (nclist = top_nclist;
	     nclist != NULL;
	     nclist = next_top_down(nclist))
	{
		print_NCList_stack();
		print_NCList_node(nclist, NCList_stack_depth);
		printf("\n"); fflush(stdout);
	}
	printf("======== END top-down walk =========\n");
	return;
}

static void test_bottom_up_walk(const NCList *top_nclist)
{
	const NCList *nclist;

	printf("======= START bottom-up walk =======\n");
	RESET_NCLIST_STACK();
	for (nclist = move_down(top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up())
	{
		print_NCList_stack();
		print_NCList_node(nclist, NCList_stack_depth);
		printf("\n"); fflush(stdout);
	}
	printf("======== END bottom-up walk ========\n");
	return;
}
*/


/****************************************************************************
 * free_NCList()
 */

/* Recursive!
static void free_NCList_rec(const NCList *nclist)
{
	int n;
	const NCList *child_nclist;

	if (nclist->buflength == 0)
		return;
	for (n = 0, child_nclist = nclist->childrenbuf;
	     n < nclist->nchildren;
	     n++, child_nclist++)
		free_NCList_rec(child_nclist);
	free(nclist->revmap);
	free(nclist->childrenbuf);
	return;
}
*/

/* Non-recursive version of free_NCList_rec(). */
static void free_NCList(const NCList *top_nclist)
{
	const NCList *nclist;

	RESET_NCLIST_STACK();
	for (nclist = move_down(top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up())
	{
		if (nclist->buflength != 0) {
			free(nclist->revmap);
			free(nclist->childrenbuf);
		}
	}
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
	NCList *new_childrenbuf;

	old_buflength = nclist->buflength;
	if (old_buflength == 0) {
		new_buflength = 1;
	} else {
		if (old_buflength < 256)
			new_buflength = 16 * old_buflength;
		else if (old_buflength < 131072)
			new_buflength = 8 * old_buflength;
		else if (old_buflength < 8388608)
			new_buflength = 4 * old_buflength;
		else if (old_buflength < 134217728)
			new_buflength = 2 * old_buflength;
		else
			new_buflength = old_buflength + 67108864;
	}
	new_revmap = (int *) realloc2(nclist->revmap,
				      new_buflength,
				      old_buflength,
				      sizeof(int));
	new_childrenbuf = (NCList *) realloc2(nclist->childrenbuf,
					      new_buflength,
					      old_buflength,
					      sizeof(NCList));
	nclist->buflength = new_buflength;
	nclist->revmap = new_revmap;
	nclist->childrenbuf = new_childrenbuf;
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
	if (ret != 0)
		return ret;
	/* Break tie by position so the ordering is "stable". */
	return i1 - i2;
}

/*
 * Setting a hard limit on the max depth of NCList objects to prevent C stack
 * overflows when running recursive code like NCList_get_y_overlaps_rec().
 * A better solution would be to not use recursive code at all when traversing
 * an NCList object. Then NCList objects of arbitrary depth could be supported
 * and it wouldn't be necessary to set the limit below.
 */
#define NCLIST_MAX_DEPTH 100000
typedef struct stack_elt_t {
	int revidx;
	NCList *nclist;
} StackElt;

static StackElt *stack = NULL;
static int stack_maxdepth = 0;

static StackElt append_NCList_elt(NCList *landing_nclist, int revidx)
{
	int nchildren;
	StackElt stack_elt;

	nchildren = landing_nclist->nchildren;
	if (nchildren == landing_nclist->buflength)
		extend_NCList(landing_nclist);
	stack_elt.revidx = landing_nclist->revmap[nchildren] = revidx;
	stack_elt.nclist = landing_nclist->childrenbuf + nchildren;
	init_NCList(stack_elt.nclist);
	landing_nclist->nchildren++;
	return stack_elt;
}

static void extend_stack()
{
	int new_maxdepth;

	if (stack_maxdepth == NCLIST_MAX_DEPTH)
		error("extend_stack: cannot build an NCList object "
		      "of depth > %d", NCLIST_MAX_DEPTH);
	new_maxdepth = get_new_maxdepth(stack_maxdepth);
	if (new_maxdepth > NCLIST_MAX_DEPTH)
		new_maxdepth = NCLIST_MAX_DEPTH;
	stack = (StackElt *) realloc2(stack, new_maxdepth, stack_maxdepth,
				      sizeof(StackElt));
	stack_maxdepth = new_maxdepth;
	return;
}

static void build_NCList(NCList *top_nclist,
			 const int *x_start_p, const int *x_end_p,
			 const int *x_subset_p, int x_len)
{
	int *oo, k, d, i, current_end;
	NCList *landing_nclist;
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
		landing_nclist = d == -1 ? top_nclist: stack[d].nclist;
		// append range i to landing_nclist
		stack_elt = append_NCList_elt(landing_nclist, i);
		// put stack_elt on stack
		if (++d == stack_maxdepth)
			extend_stack();
		stack[d] = stack_elt;
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
 * new_NCListAsINTSXP_from_NCList()
 */

#define NCListAsINTSXP_NCHILDREN(nclist) ((nclist)[0])
#define NCListAsINTSXP_REVMAP(nclist) ((nclist) + 1)
#define NCListAsINTSXP_OFFSETS(nclist) \
	((nclist) + 1 + NCListAsINTSXP_NCHILDREN(nclist))

/* Recursive!
static int compute_length_of_NCListAsINTSXP_rec(const NCList *nclist)
{
	int nchildren, n;
	unsigned int ans_len, dump_len;
	const NCList *child_nclist;

	nchildren = nclist->nchildren;
	if (nchildren == 0)
		return 0;
	ans_len = 1U + 2U * (unsigned int) nchildren;
	for (n = 0, child_nclist = nclist->childrenbuf;
	     n < nchildren;
	     n++, child_nclist++)
	{
		dump_len = compute_length_of_NCListAsINTSXP_rec(child_nclist);
		ans_len += dump_len;
		if (dump_len > ans_len)
			goto too_big;
	}
	if (ans_len <= INT_MAX)
		return (int) ans_len;
too_big:
	error("compute_length_of_NCListAsINTSXP_rec: "
	      "NCList object is too big to fit in an integer vector");
}
*/

/* Non-recursive version of compute_length_of_NCListAsINTSXP_rec(). */
static int compute_length_of_NCListAsINTSXP(const NCList *top_nclist)
{
	unsigned int ans_len;
	const NCList *nclist;
	int nchildren;

	ans_len = 0U;
	RESET_NCLIST_STACK();
	for (nclist = move_down(top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up())
	{
		nchildren = nclist->nchildren;
		if (nchildren == 0)
			continue;
		ans_len += 1U + 2U * (unsigned int) nchildren;
		if (ans_len > INT_MAX)
			error("compute_length_of_NCListAsINTSXP: "
			      "NCList object is too big to fit in "
			      "an integer vector");
	}
	return (int) ans_len;
}

/* Recursive! */
static int dump_NCList_to_int_array_rec(const NCList *nclist, int *out)
{
	int nchildren, offset, dump_len, n;
	const int *revidx_p;
	const NCList *child_nclist;

	nchildren = nclist->nchildren;
	if (nchildren == 0)
		return 0;
	offset = 1 + 2 * nchildren;
	NCListAsINTSXP_NCHILDREN(out) = nchildren;
	for (n = 0, revidx_p = nclist->revmap,
		    child_nclist = nclist->childrenbuf;
	     n < nchildren;
	     n++, revidx_p++, child_nclist++)
	{
		NCListAsINTSXP_REVMAP(out)[n] = *revidx_p;
		dump_len = dump_NCList_to_int_array_rec(child_nclist,
							out + offset);
		NCListAsINTSXP_OFFSETS(out)[n] = dump_len != 0 ? offset : -1;
		offset += dump_len;
	}
	return offset;
}

/* --- .Call ENTRY POINT --- */
SEXP new_NCListAsINTSXP_from_NCList(SEXP nclist_xp)
{
	SEXP ans;
	const NCList *top_nclist;
	int ans_len;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist_xp);
	if (top_nclist == NULL)
		error("new_NCListAsINTSXP_from_NCList: "
		      "pointer to NCList struct is NULL");
	ans_len = compute_length_of_NCListAsINTSXP(top_nclist);
	PROTECT(ans = NEW_INTEGER(ans_len));
	dump_NCList_to_int_array_rec(top_nclist, INTEGER(ans));
	UNPROTECT(1);
	//print_elapsed_time();
	return ans;
}


/****************************************************************************
 * NCListAsINTSXP_print()
 */

/* Recursive! 
   Print 1 line per range in 'nclist'. Return max depth. */
static int print_NCListAsINTSXP_rec(const int *nclist,
				    const int *x_start_p, const int *x_end_p,
				    int depth, const char *format)
{
	int maxdepth, nchildren, n, d, revidx, offset, tmp;

	maxdepth = depth;
	nchildren = NCListAsINTSXP_NCHILDREN(nclist);
	for (n = 0; n < nchildren; n++) {
		for (d = 1; d < depth; d++)
			Rprintf("|");
		revidx = NCListAsINTSXP_REVMAP(nclist)[n];
		Rprintf(format, revidx + 1);
		Rprintf(": [%d, %d]\n", x_start_p[revidx], x_end_p[revidx]);
		offset = NCListAsINTSXP_OFFSETS(nclist)[n];
		if (offset != -1) {
			tmp = print_NCListAsINTSXP_rec(nclist + offset,
						       x_start_p, x_end_p,
						       depth + 1, format);
			if (tmp > maxdepth)
				maxdepth = tmp;
		}
	}
	return maxdepth;
}

/* --- .Call ENTRY POINT --- */
SEXP NCListAsINTSXP_print(SEXP x_nclist, SEXP x_start, SEXP x_end)
{
	const int *top_nclist;
	int x_len, max_digits, maxdepth;
	const int *x_start_p, *x_end_p;
	char format[10];

	top_nclist = INTEGER(x_nclist);
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	if (x_len == 0) {
		maxdepth = 0;
	} else {
		max_digits = (int) log10((double) x_len) + 1;
		sprintf(format, "%c0%d%c", '%', max_digits, 'd');
		maxdepth = print_NCListAsINTSXP_rec(top_nclist,
						    x_start_p, x_end_p,
						    1, format);
	}
	Rprintf("max depth = %d\n", maxdepth);
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

	int maxgap;
	int minoverlap;
	int overlap_type;
	int min_overlap_score0;
	int (*is_hit_fun)(int revidx, const struct backpack_t *backpack);

	int select_mode;
	int circle_len;
	int pp_is_q;
	IntAE *hits;
	int *direct_out;

	/* Members set by update_backpack(). */
	int y_idx;
	int y_start;
	int y_end;
	int y_space;
	int min_x_end;
	int max_x_start;
} Backpack;

static int overlap_score0(int x_start, int x_end, int y_start, int y_end)
{
	return (x_end <= y_end ? x_end : y_end) -
	       (x_start >= y_start ? x_start : y_start);
}

static int is_TYPE_ANY_hit(int revidx, const Backpack *backpack)
{
	int x_start, x_end;

	/* Check the score */
	x_start = backpack->x_start_p[revidx];
	x_end = backpack->x_end_p[revidx];
	return x_end - x_start >= backpack->min_overlap_score0;
}

static int is_TYPE_START_hit(int revidx, const Backpack *backpack)
{
	int x_start, x_end, d, score0;

	/* Check the distance between the starts. */
	x_start = backpack->x_start_p[revidx];
	d = abs(backpack->y_start - x_start);
	if (d > backpack->maxgap)
		return 0;
	/* Check the score, but only if minoverlap != 0. */
	if (backpack->minoverlap == 0)
		return 1;
	x_end = backpack->x_end_p[revidx];
	score0 = overlap_score0(x_start, x_end,
				backpack->y_start, backpack->y_end);
	return score0 >= backpack->min_overlap_score0;
}

static int is_TYPE_END_hit(int revidx, const Backpack *backpack)
{
	int x_start, x_end, d, score0;

	/* Check the distance between the ends. */
	x_end = backpack->x_end_p[revidx];
	d = abs(backpack->y_end - x_end);
	if (backpack->circle_len != NA_INTEGER)
		d %= backpack->circle_len;
	if (d > backpack->maxgap)
		return 0;
	/* Check the score, but only if minoverlap != 0. */
	if (backpack->minoverlap == 0)
		return 1;
	x_start = backpack->x_start_p[revidx];
	score0 = overlap_score0(x_start, x_end,
				backpack->y_start, backpack->y_end);
	return score0 >= backpack->min_overlap_score0;
}

static int is_TYPE_WITHIN_hit(int revidx, const Backpack *backpack)
{
	int x_start, x_end, d;

	if (backpack->maxgap == 0)
		return 1;
	x_start = backpack->x_start_p[revidx];
	x_end = backpack->x_end_p[revidx];
	d = backpack->y_start - x_start + x_end - backpack->y_end;
	return d <= backpack->maxgap;
}

static int is_TYPE_EXTEND_hit(int revidx, const Backpack *backpack)
{
	int x_start, x_end, d1, d2;

	x_start = backpack->x_start_p[revidx];
	d1 = x_start - backpack->y_start;
	if (d1 < 0)
		return 0;
	x_end = backpack->x_end_p[revidx];
	d2 = backpack->y_end - x_end;
	if (d2 < 0)
		return 0;
	if (x_end - x_start < backpack->min_overlap_score0)
		return 0;
	if (backpack->maxgap == 0)
		return 1;
	return d1 + d2 <= backpack->maxgap;
}

static int is_TYPE_EQUAL_hit(int revidx, const Backpack *backpack)
{
	int x_start, x_end, d, score0;

	/* Check the distance between the starts. */
	x_start = backpack->x_start_p[revidx];
	d = abs(backpack->y_start - x_start);
	if (d > backpack->maxgap)
		return 0;
	/* Check the distance between the ends. */
	x_end = backpack->x_end_p[revidx];
	d = abs(backpack->y_end - x_end);
	if (backpack->circle_len != NA_INTEGER)
		d %= backpack->circle_len;
	if (d > backpack->maxgap)
		return 0;
	/* Check the score, but only if minoverlap != 0. */
	if (backpack->minoverlap == 0)
		return 1;
	score0 = overlap_score0(x_start, x_end,
				backpack->y_start, backpack->y_end);
	return score0 >= backpack->min_overlap_score0;
}

static int is_hit(int revidx, const Backpack *backpack)
{
	int x_space;

	/* 1st: perform checks common to all types of overlaps */
	if (backpack->x_space_p != NULL && backpack->y_space != 0) {
		x_space = backpack->x_space_p[revidx];
		if (x_space != 0 && x_space != backpack->y_space)
			return 0;
	}
	/* 2nd: perform checks specific to the current type of overlaps
	   (by calling the callback function for this type) */
	return backpack->is_hit_fun(revidx, backpack);
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
				 int maxgap, int minoverlap,
				 int overlap_type, int select_mode,
				 int circle_len,
				 int pp_is_q,
				 IntAE *hits, int *direct_out)
{
	Backpack backpack;

	backpack.x_start_p = x_start_p;
	backpack.x_end_p = x_end_p;
	backpack.x_space_p = x_space_p;

	backpack.maxgap = maxgap;
	backpack.minoverlap = minoverlap;
	backpack.overlap_type = overlap_type;
	if (overlap_type == TYPE_ANY)
		backpack.min_overlap_score0 = minoverlap - maxgap - 1;
	else
		backpack.min_overlap_score0 = minoverlap - 1;

	/* set callback function for the current type of overlaps */
	switch (overlap_type) {
		case TYPE_ANY:
			backpack.is_hit_fun = is_TYPE_ANY_hit;
			break;
		case TYPE_START:
			backpack.is_hit_fun = is_TYPE_START_hit;
			break;
		case TYPE_END:
			backpack.is_hit_fun = is_TYPE_END_hit;
			break;
		case TYPE_WITHIN:
			backpack.is_hit_fun = is_TYPE_WITHIN_hit;
			break;
		case TYPE_EXTEND:
			backpack.is_hit_fun = is_TYPE_EXTEND_hit;
			break;
		case TYPE_EQUAL:
			backpack.is_hit_fun = is_TYPE_EQUAL_hit;
			break;
	}

	backpack.select_mode = select_mode;
	backpack.circle_len = circle_len;
	backpack.pp_is_q = pp_is_q;
	backpack.hits = hits;
	backpack.direct_out = direct_out;
	return backpack;
}

static void update_backpack(Backpack *backpack, int y_idx,
			    int y_start, int y_end, int y_space)
{
	int min_x_end, max_x_start, min_overlap_score0;

	backpack->y_idx = y_idx;
	backpack->y_start = y_start;
	backpack->y_end = y_end;
	backpack->y_space = y_space;

	/* set 'min_x_end' and 'max_x_start' */
	if (backpack->overlap_type == TYPE_WITHIN) {
		backpack->min_x_end = backpack->y_end;
		backpack->max_x_start = backpack->y_start;
		return;
	}
	if (backpack->overlap_type == TYPE_ANY
	 || backpack->overlap_type == TYPE_EXTEND
	 || backpack->minoverlap != 0
	 || backpack->circle_len != NA_INTEGER)
	{
		min_overlap_score0 = backpack->min_overlap_score0;
		backpack->min_x_end = y_start + min_overlap_score0;
		backpack->max_x_start = y_end - min_overlap_score0;
	}
	if (backpack->overlap_type == TYPE_ANY
	 || backpack->overlap_type == TYPE_EXTEND)
		return;

	/* TYPE_START, TYPE_END, or TYPE_EQUAL */
	/* min_x_end */
	if (backpack->overlap_type == TYPE_START) {
		/* TYPE_START */
		if (backpack->minoverlap == 0)
			backpack->min_x_end = y_start - backpack->maxgap - 1;
	} else if (backpack->circle_len == NA_INTEGER) {
		/* TYPE_END or TYPE_EQUAL */
		min_x_end = y_end - backpack->maxgap;
		if (backpack->minoverlap == 0
		 || min_x_end > backpack->min_x_end)
			backpack->min_x_end = min_x_end;
	}
	/* max_x_start */
	if (backpack->overlap_type == TYPE_END) {
		/* TYPE_END */
		if (backpack->minoverlap == 0)
			backpack->max_x_start = y_end + backpack->maxgap + 1;
	//} else if (backpack->circle_len == NA_INTEGER) {
	} else {
		/* TYPE_START or TYPE_EQUAL */
		max_x_start = y_start + backpack->maxgap;
		if (backpack->minoverlap == 0
		 || max_x_start < backpack->max_x_start)
			backpack->max_x_start = max_x_start;
	}
	//printf("y_start=%d y_end=%d min_x_end=%d max_x_start=%d\n",
	//       y_start, y_end, backpack->min_x_end, backpack->max_x_start);
	return;
}

static void shift_y(Backpack *backpack, int shift)
{
	backpack->y_start += shift;
	backpack->y_end += shift;
	backpack->min_x_end += shift;
	backpack->max_x_start += shift;
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

typedef void (*GetYOverlapsFunType)(const void *x_nclist,
				    const Backpack *backpack);

static void pp_find_overlaps(
		const int *q_start_p, const int *q_end_p,
		const int *q_space_p, const int *q_subset_p, int q_len,
		const int *s_start_p, const int *s_end_p,
		const int *s_space_p, const int *s_subset_p, int s_len,
		int maxgap, int minoverlap,
		int overlap_type, int select_mode,
		int circle_len,
		const void *pp, int pp_is_q,
		GetYOverlapsFunType get_y_overlaps_fun,
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
				    maxgap, minoverlap,
				    overlap_type, backpack_select_mode,
				    circle_len, pp_is_q,
				    xh_buf, direct_out);
	for (i = 0; i < y_len; i++) {
		j = y_subset_p == NULL ? i : y_subset_p[i];
		y_start = y_start_p[j];
		y_end = y_end_p[j];
		if (y_end - y_start < backpack.min_overlap_score0)
			continue;
		update_backpack(&backpack, j, y_start, y_end,
				y_space_p == NULL ? 0 : y_space_p[j]);
		/* pass 0 */
		get_y_overlaps_fun(pp, &backpack);
		if (circle_len == NA_INTEGER)
			goto life_is_good;
		if (select_mode == ARBITRARY_HIT
		 && !pp_is_q && direct_out[j] != NA_INTEGER)
			goto life_is_good;
		/* pass 1 */
		shift_y(&backpack, - circle_len);
		get_y_overlaps_fun(pp, &backpack);
		if (select_mode == ARBITRARY_HIT
		 && !pp_is_q && direct_out[j] != NA_INTEGER)
			goto life_is_good;
		/* pass 2 */
		shift_y(&backpack, 2 * circle_len);
		get_y_overlaps_fun(pp, &backpack);

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
 * int_bsearch()
 */

/*
 * 'subset_len' is assumed to be > 0.
 * Return the first index 'n' for which 'base[subset[n]] >= min', or
 * 'subset_len' if there is no such index.
 * TODO: Maybe move this to int_utils.c or sort_utils.c in S4Vectors/src/
 */
static int int_bsearch(const int *subset, int subset_len, const int *base,
		       int min)
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


/****************************************************************************
 * NCList_get_y_overlaps()
 */

/* Recursive! */
static void NCList_get_y_overlaps_rec(const NCList *x_nclist,
				      const Backpack *backpack)
{
	const int *revmap;
	int nchildren, n, revidx;
	const NCList *child_nclist;

	revmap = x_nclist->revmap;
	nchildren = x_nclist->nchildren;
	n = int_bsearch(revmap, nchildren, backpack->x_end_p,
			backpack->min_x_end);
	for (revmap = revmap + n, child_nclist = x_nclist->childrenbuf + n;
	     n < nchildren;
	     n++, revmap++, child_nclist++)
	{
		revidx = *revmap;
		if (backpack->x_start_p[revidx] > backpack->max_x_start)
			break;
		if (is_hit(revidx, backpack)) {
			report_hit(revidx, backpack);
			if (backpack->select_mode == ARBITRARY_HIT
			 && !backpack->pp_is_q)
				break;
		}
		if (child_nclist->nchildren != 0)
			NCList_get_y_overlaps_rec(child_nclist, backpack);
	}
	return;
}

/* Non-recursive version of NCList_get_y_overlaps_rec(). */
static void NCList_get_y_overlaps(const NCList *top_nclist,
				  const Backpack *backpack)
{
	const NCList *nclist;
	const int *revmap;
	int revidx, nchildren, n;

	RESET_NCLIST_STACK();
	nclist = top_nclist;
	do {
		if (nclist != top_nclist) {
			revidx = get_current_revidx();
			if (backpack->x_start_p[revidx] >
			    backpack->max_x_start)
			{
				pop_NCListStackElt();
				nclist = move_right();
				continue;
			}
			if (is_hit(revidx, backpack)) {
				report_hit(revidx, backpack);
				if (backpack->select_mode == ARBITRARY_HIT
				 && !backpack->pp_is_q)
					break;
			}
		}
		nchildren = nclist->nchildren;
		if (nchildren == 0) {
			nclist = move_right();
			continue;
		}
		revmap = nclist->revmap;
		n = int_bsearch(revmap, nchildren, backpack->x_end_p,
				backpack->min_x_end);
		if (n >= nchildren) {
			nclist = move_right();
			continue;
		}
		nclist = move_to_child(nclist, n);
	} while (nclist != NULL);
	return;
}


/****************************************************************************
 * NCListAsINTSXP_get_y_overlaps()
 */

/* Recursive! */
static void NCListAsINTSXP_get_y_overlaps_rec(const int *x_nclist,
					      const Backpack *backpack)
{
	const int *revmap, *offset_p;
	int nchildren, n, revidx, offset;

	revmap = NCListAsINTSXP_REVMAP(x_nclist);
	nchildren = NCListAsINTSXP_NCHILDREN(x_nclist);
	n = int_bsearch(revmap, nchildren, backpack->x_end_p,
			backpack->min_x_end);
	for (revmap = revmap + n,
	     offset_p = NCListAsINTSXP_OFFSETS(x_nclist) + n;
	     n < nchildren;
	     n++, revmap++, offset_p++)
	{
		revidx = *revmap;
		if (backpack->x_start_p[revidx] > backpack->max_x_start)
			break;
		if (is_hit(revidx, backpack)) {
			report_hit(revidx, backpack);
			if (backpack->select_mode == ARBITRARY_HIT
			 && !backpack->pp_is_q)
				break;
		}
		offset = *offset_p;
		if (offset != -1)
			NCListAsINTSXP_get_y_overlaps_rec(x_nclist + offset,
							  backpack);
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
		int maxgap, int minoverlap,
		int overlap_type, int select_mode,
		int circle_len,
		SEXP nclist_sxp, int pp_is_q,
		IntAE *qh_buf, IntAE *sh_buf, int *direct_out)
{
	NCList nclist;
	const void *pp;
	GetYOverlapsFunType get_y_overlaps_fun;

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
		get_y_overlaps_fun =
		    (GetYOverlapsFunType) NCList_get_y_overlaps_rec;
	} else {
		pp = INTEGER(nclist_sxp);
		get_y_overlaps_fun =
		    (GetYOverlapsFunType) NCListAsINTSXP_get_y_overlaps_rec;
	}
	pp_find_overlaps(
		q_start_p, q_end_p, q_space_p, q_subset_p, q_len,
		s_start_p, s_end_p, s_space_p, s_subset_p, s_len,
		maxgap, minoverlap,
		overlap_type, select_mode,
		circle_len,
		pp, pp_is_q, get_y_overlaps_fun,
		qh_buf, sh_buf, direct_out);
	if (nclist_sxp == R_NilValue)
		free_NCList(&nclist);
	return pp_is_q;
}


/****************************************************************************
 * Helper functions shared by NCList_find_overlaps() and
 * NCList_find_overlaps_in_groups()
 */

static int get_maxgap0(SEXP maxgap)
{
	int maxgap0;

	if (!IS_INTEGER(maxgap) || LENGTH(maxgap) != 1)
		error("'maxgap' must be a single integer");
	maxgap0 = INTEGER(maxgap)[0];
	if (maxgap0 == NA_INTEGER)
		error("'maxgap' cannot be NA");
	if (maxgap0 < 0)
		error("'maxgap' cannot be negative");
	return maxgap0;
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

static int get_minoverlap0(SEXP minoverlap, int maxgap, int overlap_type)
{
	int minoverlap0;

	if (!IS_INTEGER(minoverlap) || LENGTH(minoverlap) != 1)
		error("'minoverlap' must be a single integer");
	minoverlap0 = INTEGER(minoverlap)[0];
	if (minoverlap0 == NA_INTEGER)
		error("'minoverlap' cannot be NA");
	if (minoverlap0 < 0)
		error("'minoverlap' cannot be negative");
	if (overlap_type == TYPE_ANY && maxgap != 0 && minoverlap0 > 1)
		error("'minoverlap' must be <= 1 when 'maxgap' is not 0");
	return minoverlap0;
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
 *   maxgap:         See get_maxgap0() C function.
 *   minoverlap:     See get_minoverlap0() C function.
 *   type:           See get_overlap_type() C function.
 *   select:         See _get_select_mode() C function in S4Vectors.
 *   circle_length:  A single positive integer or NA_INTEGER.
 */
SEXP NCList_find_overlaps(
		SEXP q_start, SEXP q_end,
		SEXP s_start, SEXP s_end,
		SEXP nclist, SEXP nclist_is_q,
		SEXP maxgap, SEXP minoverlap, SEXP type, SEXP select,
		SEXP circle_length)
{
	int q_len, s_len,
	    maxgap0, minoverlap0, overlap_type, select_mode, circle_len,
	    *direct_out, pp_is_q;
	const int *q_start_p, *q_end_p, *s_start_p, *s_end_p;
	IntAE *qh_buf, *sh_buf;
	SEXP ans;

	q_len = check_integer_pairs(q_start, q_end,
				    &q_start_p, &q_end_p,
				    "start(q)", "end(q)");
	s_len = check_integer_pairs(s_start, s_end,
				    &s_start_p, &s_end_p,
				    "start(s)", "end(s)");
	maxgap0 = get_maxgap0(maxgap);
	overlap_type = get_overlap_type(type);
	minoverlap0 = get_minoverlap0(minoverlap, maxgap0, overlap_type);
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
		maxgap0, minoverlap0, overlap_type,
		select_mode, circle_len,
		nclist, LOGICAL(nclist_is_q)[0],
		qh_buf, sh_buf, direct_out);
	//print_elapsed_time();
	if (select_mode != ALL_HITS) {
		UNPROTECT(1);
		return ans;
	}
	return new_Hits(qh_buf->elts, sh_buf->elts, IntAE_get_nelt(qh_buf),
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
 *   maxgap:         See get_maxgap0() C function.
 *   minoverlap:     See get_minoverlap0() C function.
 *   type:           See get_overlap_type() C function.
 *   select:         See _get_select_mode() C function in S4Vectors.
 *   circle_length:  An integer vector of length >= min(NG1, NG2) with positive
 *                   or NA values.
 */
SEXP NCList_find_overlaps_in_groups(
		SEXP q_start, SEXP q_end, SEXP q_space, SEXP q_groups,
		SEXP s_start, SEXP s_end, SEXP s_space, SEXP s_groups,
		SEXP nclists, SEXP nclist_is_q,
		SEXP maxgap, SEXP minoverlap, SEXP type, SEXP select,
		SEXP circle_length)
{
	int q_len, s_len, NG1, NG2,
	    maxgap0, minoverlap0, overlap_type, select_mode,
	    NG, i, qi_len, si_len, *direct_out;
	const int *q_start_p, *q_end_p, *q_space_p,
		  *s_start_p, *s_end_p, *s_space_p;
	CompressedIntsList_holder q_groups_holder, s_groups_holder;
	Ints_holder qi_group_holder, si_group_holder;
	IntAE *qh_buf, *sh_buf;
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

	maxgap0 = get_maxgap0(maxgap);
	overlap_type = get_overlap_type(type);
	minoverlap0 = get_minoverlap0(minoverlap, maxgap0, overlap_type);
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
			maxgap0, minoverlap0, overlap_type,
			select_mode, INTEGER(circle_length)[i],
			VECTOR_ELT(nclists, i), LOGICAL(nclist_is_q)[i],
			qh_buf, sh_buf, direct_out);
	}
	if (select_mode != ALL_HITS) {
		UNPROTECT(1);
		return ans;
	}
	return new_Hits(qh_buf->elts, sh_buf->elts, IntAE_get_nelt(qh_buf),
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

