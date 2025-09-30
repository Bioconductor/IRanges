/****************************************************************************
 *                 A Nested Containment List implementation                 *
 *                            Author: H. Pag\`es                            *
 ****************************************************************************/
#include "IRanges.h"
#include "S4Vectors_interface.h"

#include <stdlib.h>  /* for malloc, realloc, free, abs */
#include <string.h>  /* for memcpy */
#include <math.h>    /* for log10 */

#ifdef _OPENMP
/* <Rinternals.h> defines macro match that seems to break <omp.h> with
   some versions of Clang.
   See https://github.com/Bioconductor/SparseArray/issues/9 */
#undef match
#include <omp.h>
#endif

/*
#include <time.h>
static double cumulated_time = 0.0;
static clock_t clock0;

static void start_clock(void)
{
	clock0 = clock();
}
static void stop_clock(void)
{
	cumulated_time += ((double) clock() - clock0) / CLOCKS_PER_SEC;
}
static void init_clock(const char *msg)
{
	printf("%s", msg);
	cumulated_time = 0.0;
	clock0 = clock();
}
static void print_elapsed_time(void)
{
	stop_clock();
	printf("%8.6f s\n", cumulated_time);
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
	return maxdepth == 0 ? 16384 : 4 * maxdepth;
}


/****************************************************************************
 * NCList structure
 */

/* sizeof(NCList) is 24 bytes (0x18 bytes) */
typedef struct nclist_t {
	int buflength;	/* >= 0 */
	int nchildren;	/* >= 0 and <= buflength */
	struct nclist_t *childrenbuf;  /* Of length 'buflength'. */
	int *rgidbuf;	/* Of length 'nchildren'. The IDs of the ranges asso-
			   ciated with the children. The ID of a range is just
			   its 0-based position in original IntegerRanges ob-
			   ject 'x'. Allows reverse mapping of the children in-
			   to 'x' (e.g. to find their start, end, or width). */
} NCList;

static void init_NCList(NCList *nclist)
{
	nclist->buflength = nclist->nchildren = 0;
	return;
}


/****************************************************************************
 * NCList walking stack: used to walk on an NCList structure non-recursively
 */

typedef struct NCList_walking_stack_elt_t {
	const NCList *parent_nclist;
	int n;  /* point to n-th child of 'parent_nclist' */
} NCListWalkingStackElt;

#define	GET_NCLIST(stack_elt) \
	((stack_elt)->parent_nclist->childrenbuf + (stack_elt)->n)

#define	GET_RGID(stack_elt) \
	((stack_elt)->parent_nclist->rgidbuf[(stack_elt)->n])

typedef struct NCList_walking_stack {
	NCListWalkingStackElt *elts;
	int maxdepth;
	int depth;
} NCListWalkingStack;

/* Must NOT be called when 'stack->depth' is 0 (i.e. when stack is empty). */
static NCListWalkingStackElt *
		pop_NCListWalkingStackElt(NCListWalkingStack *stack)
{
	stack->depth--;
	return stack->elts + stack->depth;
}

/* Must NOT be called when 'stack->depth' is 0 (i.e. when stack is empty). */
static NCListWalkingStackElt *
		peek_NCListWalkingStackElt(NCListWalkingStack *stack)
{
	return stack->elts + stack->depth - 1;
}

static void extend_NCListWalkingStack(NCListWalkingStack *stack)
{
	int new_maxdepth = get_new_maxdepth(stack->maxdepth);
	stack->elts = (NCListWalkingStackElt *)
		realloc2(stack->elts, new_maxdepth, stack->maxdepth,
			 sizeof(NCListWalkingStackElt));
	stack->maxdepth = new_maxdepth;
	return;
}

/* Returns a pointer to n-th child. */
static const NCList *move_to_child(NCListWalkingStack *stack,
				   const NCList *parent_nclist, int n)
{
	if (stack->depth == stack->maxdepth)
		extend_NCListWalkingStack(stack);
	NCListWalkingStackElt *stack_elt = stack->elts + stack->depth++;
	stack_elt->parent_nclist = parent_nclist;
	stack_elt->n = n;
	return GET_NCLIST(stack_elt);
}

/* Must NOT be called when 'stack->depth' is 0 (i.e. when stack is empty). */
static const NCList *move_to_right_sibling_or_uncle(NCListWalkingStack *stack,
						    const NCList *nclist)
{
	NCListWalkingStackElt *stack_elt = stack->elts + stack->depth;
	do {
		stack_elt--;
		if (++(stack_elt->n) < stack_elt->parent_nclist->nchildren)
			return ++nclist;
		nclist = stack_elt->parent_nclist;
	} while (--stack->depth != 0);
	return NULL;
}

/* Must NOT be called when 'stack->depth' is 0 (i.e. when stack is empty). */
static const NCList *move_to_right_uncle(NCListWalkingStack *stack)
{
	const NCList *parent_nclist =
			pop_NCListWalkingStackElt(stack)->parent_nclist;
	if (stack->depth == 0)
		return NULL;
	return move_to_right_sibling_or_uncle(stack, parent_nclist);
}

static const NCList *move_down(NCListWalkingStack *stack, const NCList *nclist)
{
	while (nclist->nchildren != 0)
		nclist = move_to_child(stack, nclist, 0);
	return nclist;
}

/*
   Top-down walk: parent is treated before children and children are treated
   from left to right. For a top-down walk that visits the entire tree (i.e.
   "complete walk") do:

	NCListWalkingStack stack;
	stack.depth = 0;
	for (nclist = top_nclist;
	     nclist != NULL;
	     nclist = next_top_down(&stack, nclist))
	{
		treat nclist
	}
 */
static const NCList *next_top_down(NCListWalkingStack *stack,
				   const NCList *nclist)
{
	/* Try to move to first child, if any. */
	if (nclist->nchildren != 0)
		return move_to_child(stack, nclist, 0);
	if (stack->depth == 0)
		return NULL;
	return move_to_right_sibling_or_uncle(stack, nclist);
}

/*
   Bottom-up walk: children are treated from left to right and before parent.
   For a bottom-up walk that visits the entire tree (i.e. "complete walk"),
   do:

	NCListWalkingStack stack;
	stack.depth = 0;
	for (nclist = move_down(&stack, top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up(&stack))
	{
		treat nclist
	}
*/
static const NCList *next_bottom_up(NCListWalkingStack *stack)
{
	if (stack->depth == 0)
		return NULL;
	NCListWalkingStackElt *stack_elt = peek_NCListWalkingStackElt(stack);
	stack_elt->n++;
	const NCList *parent_nclist = stack_elt->parent_nclist;
	if (stack_elt->n < parent_nclist->nchildren) {
		/* Move down thru the next children. */
		return move_down(stack, GET_NCLIST(stack_elt));
	}
	/* All children have been treated --> move 1 level up. */
	stack->depth--;
	return parent_nclist;
}


/****************************************************************************
 * get_empty_walking_stack()
 */

static NCListWalkingStack* get_empty_walking_stack(void)
{
	NCListWalkingStack *stack;
#ifdef _OPENMP
#define	STACK_POOL_SIZE 20
	/* 1 stack per max number of threads. */
	static NCListWalkingStack stack_pool[STACK_POOL_SIZE];
	int thread_num = omp_get_thread_num();
	if (thread_num >= STACK_POOL_SIZE)
		error("IRanges internal error in get_empty_walking_stack(): "
		      "thread_num >= STACK_POOL_SIZE");
	stack = stack_pool + thread_num;
#else
	static NCListWalkingStack global_stack;
	stack = &global_stack;
#endif
	stack->depth = 0;
	return stack;
}


/****************************************************************************
 * Test the top-down and bottom-up non-recursive walks on an NCList structure
 */

/*
static void print_NCListWalkingStack(NCListWalkingStack *stack)
{
	printf("NCListWalkingStack:");
	for (int d = 0; d < stack->depth; d++)
		printf(" %d", stack->elts[d].n);
	printf("\n");
	return;
}

static void print_NCList_node(const NCList *nclist, int depth)
{
	for (int d = 0; d < depth; d++) printf("-"); printf(" ");
	printf("NCList node at address %p:\n", nclist);

	for (int d = 0; d < depth; d++) printf("-"); printf(" ");
	printf("  buflength=%d; nchildren=%d\n",
	       nclist->buflength, nclist->nchildren);

	for (int d = 0; d < depth; d++) printf("-"); printf(" ");
	printf("  rgidbuf:");
	for (int n = 0; n < nclist->nchildren; n++)
		printf(" %d", nclist->rgidbuf[n]);
	printf("\n");
	return;
}

static void print_NCList_rec(const NCList *nclist, int depth)
{
	print_NCList_node(nclist, depth);
	for (int n = 0; n < nclist->nchildren; n++)
		print_NCList_rec(nclist->childrenbuf + n, depth + 1);
	return;
}

static void test_complete_top_down_walk(const NCList *top_nclist)
{
	NCListWalkingStack *stack = get_empty_walking_stack();
	printf("======= START complete top-down walk ========\n");
	for (const NCList *nclist = top_nclist;
	     nclist != NULL;
	     nclist = next_top_down(stack, nclist))
	{
		print_NCListWalkingStack(stack);
		print_NCList_node(nclist, stack->depth);
		printf("\n"); fflush(stdout);
	}
	printf("======== END complete top-down walk =========\n");
	return;
}

static void test_complete_bottom_up_walk(const NCList *top_nclist)
{
	NCListWalkingStack *stack = get_empty_walking_stack();
	printf("======= START complete bottom-up walk =======\n");
	for (const NCList *nclist = move_down(stack, top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up(stack))
	{
		print_NCListWalkingStack(stack);
		print_NCList_node(nclist, stack->depth);
		printf("\n"); fflush(stdout);
	}
	printf("======== END complete bottom-up walk ========\n");
	return;
}
*/


/****************************************************************************
 * free_NCList()
 */

static void free_NCList(const NCList *top_nclist)
{
	/* Complete bottom-up walk. */
	NCListWalkingStack *stack = get_empty_walking_stack();
	for (const NCList *nclist = move_down(stack, top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up(stack))
	{
		if (nclist->buflength != 0) {
			free(nclist->childrenbuf);
			free(nclist->rgidbuf);
		}
	}
	return;
}


/****************************************************************************
 * C_new_NCList() and C_free_NCList()
 */

/* --- .Call ENTRY POINT --- */
SEXP C_new_NCList()
{
	NCList *top_nclist;

	//init_clock("preprocessing: T1 = ");
	top_nclist = (NCList *) malloc(sizeof(NCList));
	if (top_nclist == NULL)
		error("C_new_NCList: memory allocation failed");
	init_NCList(top_nclist);
	return R_MakeExternalPtr(top_nclist, R_NilValue, R_NilValue);
}

/* --- .Call ENTRY POINT --- */
SEXP C_free_NCList(SEXP nclist_xp)
{
	NCList *top_nclist;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist_xp);
	if (top_nclist == NULL)
		error("C_free_NCList: pointer to NCList struct is NULL");
	free_NCList(top_nclist);
	free(top_nclist);
	R_SetExternalPtrAddr(nclist_xp, NULL);
	return R_NilValue;
}


/****************************************************************************
 * C_build_NCList()
 */

static void extend_NCList(NCList *nclist)
{
	int old_buflength, new_buflength;
	NCList *new_childrenbuf;
	int *new_rgidbuf;

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
	new_childrenbuf = (NCList *) realloc2(nclist->childrenbuf,
					      new_buflength,
					      old_buflength,
					      sizeof(NCList));
	new_rgidbuf = (int *) realloc2(nclist->rgidbuf,
				       new_buflength,
				       old_buflength,
				       sizeof(int));
	nclist->buflength = new_buflength;
	nclist->childrenbuf = new_childrenbuf;
	nclist->rgidbuf = new_rgidbuf;
	return;
}

typedef struct NCList_building_stack_elt_t {
	NCList *nclist;
	int rgid;  /* range ID */
} NCListBuildingStackElt;

static NCListBuildingStackElt *NCList_building_stack = NULL;
static int NCList_building_stack_maxdepth = 0;

static NCListBuildingStackElt append_NCList_elt(NCList *landing_nclist,
						int rgid)
{
	int nchildren;
	NCListBuildingStackElt stack_elt;

	nchildren = landing_nclist->nchildren;
	if (nchildren == landing_nclist->buflength)
		extend_NCList(landing_nclist);
	stack_elt.nclist = landing_nclist->childrenbuf + nchildren;
	stack_elt.rgid = landing_nclist->rgidbuf[nchildren] = rgid;
	init_NCList(stack_elt.nclist);
	landing_nclist->nchildren++;
	return stack_elt;
}

static void extend_NCList_building_stack(void)
{
	int new_maxdepth;

	new_maxdepth = get_new_maxdepth(NCList_building_stack_maxdepth);
	NCList_building_stack = (NCListBuildingStackElt *)
			realloc2(NCList_building_stack,
				 new_maxdepth,
				 NCList_building_stack_maxdepth,
				 sizeof(NCListBuildingStackElt));
	NCList_building_stack_maxdepth = new_maxdepth;
	return;
}

static void build_NCList(NCList *top_nclist,
			 const int *x_start_p, const int *x_end_p,
			 const int *x_subset_p, int x_len)
{
	int *base, rgid, retcode, i, d, current_end;
	NCList *landing_nclist;
	NCListBuildingStackElt stack_elt;

	/* Compute the order of 'x' (or its subset) in 'base'.
	   The sorting is first by ascending start then by descending end. */
	base = (int *) malloc(sizeof(int) * x_len);
	if (base == NULL)
		error("build_NCList: memory allocation failed");
	if (x_subset_p == NULL) {
		for (rgid = 0; rgid < x_len; rgid++)
			base[rgid] = rgid;
	} else {
		memcpy(base, x_subset_p, sizeof(int) * x_len);
	}
	retcode = sort_int_pairs(base, x_len,
				 x_start_p, x_end_p,
				 0, 1,
				 1, NULL, NULL);
	if (retcode < 0) {
		free(base);
		error("build_NCList: memory allocation failed");
	}
	init_NCList(top_nclist);
	for (i = 0, d = -1; i < x_len; i++) {
		rgid = base[i];
		current_end = x_end_p[rgid];
		while (d >= 0 &&
		       x_end_p[NCList_building_stack[d].rgid] < current_end)
			d--;  // unstack
		landing_nclist = d == -1 ? top_nclist :
					   NCList_building_stack[d].nclist;
		// append 'rgid' to landing_nclist
		stack_elt = append_NCList_elt(landing_nclist, rgid);
		// put stack_elt on stack
		if (++d == NCList_building_stack_maxdepth)
			extend_NCList_building_stack();
		NCList_building_stack[d] = stack_elt;
	}
	free(base);
	return;
}

/* --- .Call ENTRY POINT --- */
SEXP C_build_NCList(SEXP nclist_xp, SEXP x_start, SEXP x_end, SEXP x_subset)
{
	NCList *top_nclist;
	int x_len;
	const int *x_start_p, *x_end_p, *x_subset_p;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist_xp);
	if (top_nclist == NULL)
		error("C_build_NCList: pointer to NCList struct is NULL");
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
 * C_new_NCListAsINTSXP_from_NCList()
 */

/*
 * Setting an arbitrary hard limit on the max depth of NCListAsINTSXP objects
 * to prevent C stack overflows when walking on them recursively (e.g. with
 * print_NCListAsINTSXP_rec() or NCListAsINTSXP_get_y_overlaps_rec()).
 * A better solution would be to not use recursive code at all when traversing
 * an NCListAsINTSXP object. Then NCListAsINTSXP objects of arbitrary depth
 * could be supported and it wouldn't be necessary to set the limit below.
 */
#define NCListAsINTSXP_MAX_DEPTH 100000

#define NCListAsINTSXP_NCHILDREN(nclist) ((nclist)[0])
#define NCListAsINTSXP_RGIDS(nclist) ((nclist) + 1)
#define NCListAsINTSXP_OFFSETS(nclist) \
	((nclist) + 1 + NCListAsINTSXP_NCHILDREN(nclist))

static int compute_NCListAsINTSXP_length(const NCList *top_nclist)
{
	unsigned int ans_len = 0U;
	/* Complete bottom-up walk (top-down walk would also work). */
	NCListWalkingStack *stack = get_empty_walking_stack();
	for (const NCList *nclist = move_down(stack, top_nclist);
	     nclist != NULL;
	     nclist = next_bottom_up(stack))
	{
		if (stack->depth > NCListAsINTSXP_MAX_DEPTH)
			error("compute_NCListAsINTSXP_length: "
			      "NCList object is too deep (has more "
			      "than\n  %d levels of nested ranges)",
			      NCListAsINTSXP_MAX_DEPTH);
		int nchildren = nclist->nchildren;
		if (nchildren == 0)
			continue;
		ans_len += 1U + 2U * (unsigned int) nchildren;
		if (ans_len > INT_MAX)
			error("compute_NCListAsINTSXP_length: "
			      "NCList object is too big to fit in "
			      "an integer vector");
	}
	return (int) ans_len;
}

/* Recursive! */
static int dump_NCList_to_int_array_rec(const NCList *nclist, int *out)
{
	int nchildren, offset, dump_len, n;
	const NCList *child_nclist;
	const int *rgid_p;

	nchildren = nclist->nchildren;
	if (nchildren == 0)
		return 0;
	offset = 1 + 2 * nchildren;
	NCListAsINTSXP_NCHILDREN(out) = nchildren;
	for (n = 0, child_nclist = nclist->childrenbuf,
		    rgid_p = nclist->rgidbuf;
	     n < nchildren;
	     n++, child_nclist++, rgid_p++)
	{
		NCListAsINTSXP_RGIDS(out)[n] = *rgid_p;
		dump_len = dump_NCList_to_int_array_rec(child_nclist,
							out + offset);
		NCListAsINTSXP_OFFSETS(out)[n] = dump_len != 0 ? offset : -1;
		offset += dump_len;
	}
	return offset;
}

/* --- .Call ENTRY POINT --- */
SEXP C_new_NCListAsINTSXP_from_NCList(SEXP nclist_xp)
{
	SEXP ans;
	const NCList *top_nclist;
	int ans_len;

	top_nclist = (NCList *) R_ExternalPtrAddr(nclist_xp);
	if (top_nclist == NULL)
		error("C_new_NCListAsINTSXP_from_NCList: "
		      "pointer to NCList struct is NULL");
	ans_len = compute_NCListAsINTSXP_length(top_nclist);
	PROTECT(ans = NEW_INTEGER(ans_len));
	dump_NCList_to_int_array_rec(top_nclist, INTEGER(ans));
	UNPROTECT(1);
	//print_elapsed_time();
	return ans;
}


/****************************************************************************
 * C_print_NCListAsINTSXP()
 */

/* Recursive! 
   Print 1 line per range in 'nclist'. Returns max depth. */
static int print_NCListAsINTSXP_rec(const int *nclist,
				    const int *x_start_p, const int *x_end_p,
				    int depth, const char *format)
{
	int maxdepth, nchildren, n, d, rgid, offset, tmp;

	maxdepth = depth;
	nchildren = NCListAsINTSXP_NCHILDREN(nclist);
	for (n = 0; n < nchildren; n++) {
		for (d = 1; d < depth; d++)
			Rprintf("|");
		rgid = NCListAsINTSXP_RGIDS(nclist)[n];
		Rprintf(format, rgid + 1);
		Rprintf(": [%d, %d]\n", x_start_p[rgid], x_end_p[rgid]);
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
SEXP C_print_NCListAsINTSXP(SEXP x_nclist, SEXP x_start, SEXP x_end)
{
	const int *top_nclist;
	int x_len, max_digits, maxdepth;
	const int *x_start_p, *x_end_p;
	char format[15];

	top_nclist = INTEGER(x_nclist);
	x_len = check_integer_pairs(x_start, x_end,
				    &x_start_p, &x_end_p,
				    "start(x)", "end(x)");
	if (x_len == 0) {
		maxdepth = 0;
	} else {
		max_digits = (int) log10((double) x_len) + 1;
		snprintf(format, sizeof(format),
			 "%c0%d%c", '%', max_digits, 'd');
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
	int (*is_hit_fun)(int rgid, const struct backpack_t *backpack);

	int select_mode;
	int circle_len;
	int pp_is_q;
	int *direct_out;

	/* Members set by update_backpack(). */
	int y_rgid;
	int y_start;
	int y_end;
	int y_space;
	IntAE *xh_buf;
	int min_x_end;
	int max_x_start;
} Backpack;

static int overlap_score0(int x_start, int x_end, int y_start, int y_end)
{
	return (x_end <= y_end ? x_end : y_end) -
	       (x_start >= y_start ? x_start : y_start);
}

static int is_TYPE_ANY_hit(int rgid, const Backpack *backpack)
{
	int x_start, x_end;

	if (backpack->minoverlap == 0)
		return 1;
	/* Check the score */
	x_start = backpack->x_start_p[rgid];
	x_end = backpack->x_end_p[rgid];
	return x_end - x_start >= backpack->min_overlap_score0;
}

static int is_TYPE_START_hit(int rgid, const Backpack *backpack)
{
	int x_start, x_end, d, score0;

	/* Check the distance between the starts. */
	x_start = backpack->x_start_p[rgid];
	d = abs(backpack->y_start - x_start);
	if (d > backpack->maxgap)
		return 0;
	/* Check the score, but only if minoverlap != 0. */
	if (backpack->minoverlap == 0)
		return 1;
	x_end = backpack->x_end_p[rgid];
	score0 = overlap_score0(x_start, x_end,
				backpack->y_start, backpack->y_end);
	return score0 >= backpack->min_overlap_score0;
}

static int is_TYPE_END_hit(int rgid, const Backpack *backpack)
{
	int x_start, x_end, d, score0;

	/* Check the distance between the ends. */
	x_end = backpack->x_end_p[rgid];
	d = abs(backpack->y_end - x_end);
	if (backpack->circle_len != NA_INTEGER)
		d %= backpack->circle_len;
	if (d > backpack->maxgap)
		return 0;
	/* Check the score, but only if minoverlap != 0. */
	if (backpack->minoverlap == 0)
		return 1;
	x_start = backpack->x_start_p[rgid];
	score0 = overlap_score0(x_start, x_end,
				backpack->y_start, backpack->y_end);
	return score0 >= backpack->min_overlap_score0;
}

static int is_TYPE_WITHIN_hit(int rgid, const Backpack *backpack)
{
	int x_start, x_end, d;

	if (backpack->maxgap == 0)
		return 1;
	x_start = backpack->x_start_p[rgid];
	x_end = backpack->x_end_p[rgid];
	d = backpack->y_start - x_start + x_end - backpack->y_end;
	return d <= backpack->maxgap;
}

static int is_TYPE_EXTEND_hit(int rgid, const Backpack *backpack)
{
	int x_start, x_end, d1, d2;

	x_start = backpack->x_start_p[rgid];
	d1 = x_start - backpack->y_start;
	if (d1 < 0)
		return 0;
	x_end = backpack->x_end_p[rgid];
	d2 = backpack->y_end - x_end;
	if (d2 < 0)
		return 0;
	if (x_end - x_start < backpack->min_overlap_score0)
		return 0;
	if (backpack->maxgap == 0)
		return 1;
	return d1 + d2 <= backpack->maxgap;
}

static int is_TYPE_EQUAL_hit(int rgid, const Backpack *backpack)
{
	int x_start, x_end, d, score0;

	/* Check the distance between the starts. */
	x_start = backpack->x_start_p[rgid];
	d = abs(backpack->y_start - x_start);
	if (d > backpack->maxgap)
		return 0;
	/* Check the distance between the ends. */
	x_end = backpack->x_end_p[rgid];
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

static int is_hit(int rgid, const Backpack *backpack)
{
	int x_space;

	/* 1st: perform checks common to all types of overlaps */
	if (backpack->x_space_p != NULL && backpack->y_space != 0) {
		x_space = backpack->x_space_p[rgid];
		if (x_space != 0 && x_space != backpack->y_space)
			return 0;
	}
	/* 2nd: perform checks specific to the current type of overlaps
	   (by calling the callback function for this type) */
	return backpack->is_hit_fun(rgid, backpack);
}

static Backpack prepare_backpack(const int *x_start_p, const int *x_end_p,
				 const int *x_space_p, 
				 int maxgap, int minoverlap,
				 int overlap_type, int select_mode,
				 int circle_len, int pp_is_q, int *direct_out)
{
	Backpack backpack;

	backpack.x_start_p = x_start_p;
	backpack.x_end_p = x_end_p;
	backpack.x_space_p = x_space_p;

	backpack.maxgap = maxgap;
	backpack.minoverlap = minoverlap;
	backpack.overlap_type = overlap_type;
	if (overlap_type == TYPE_ANY)
		backpack.min_overlap_score0 = minoverlap - maxgap - 2;
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
	backpack.direct_out = direct_out;
	return backpack;
}

static void update_backpack(Backpack *backpack, int y_rgid,
			    int y_start, int y_end, int y_space,
			    IntAE *xh_buf)
{
	int slack, min_x_end, max_x_start, min_overlap_score0;

	backpack->y_rgid = y_rgid;
	backpack->y_start = y_start;
	backpack->y_end = y_end;
	backpack->y_space = y_space;
	backpack->xh_buf = xh_buf;

	/* set 'min_x_end' and 'max_x_start' */

	if (backpack->overlap_type == TYPE_ANY) {
		if (backpack->minoverlap == 0) {
			slack = backpack->maxgap + 1;
		} else {
			slack = 1 - backpack->minoverlap;
		}
		backpack->min_x_end = y_start - slack;
		backpack->max_x_start = y_end + slack;
		return;
	}
	if (backpack->overlap_type == TYPE_WITHIN) {
		backpack->min_x_end = backpack->y_end;
		backpack->max_x_start = backpack->y_start;
		return;
	}
	if (backpack->overlap_type == TYPE_EXTEND
	 || backpack->minoverlap != 0
	 || backpack->circle_len != NA_INTEGER)
	{
		min_overlap_score0 = backpack->min_overlap_score0;
		backpack->min_x_end = y_start + min_overlap_score0;
		backpack->max_x_start = y_end - min_overlap_score0;
		if (backpack->overlap_type == TYPE_EXTEND)
			return;
	}

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

/* Thread-safe only if one of the two following conditions is satisfied:
     (A1) backpack->select_mode == ALL_HITS && hits_buf != NULL
     (A2) backpack->select_mode != ALL_HITS && !backpack->pp_is_q */
static void report_hit(int rgid, const Backpack *backpack)
{
	int rgid1 = rgid + 1;  /* 1-based */
	if (backpack->select_mode == ALL_HITS) {
		/* Report the hit. */
		IntAE_insert_at(backpack->xh_buf,
				IntAE_get_nelt(backpack->xh_buf), rgid1);
		return;
	}
	/* Update current selection if necessary. */
	int q_rgid, s_rgid1;
	if (backpack->pp_is_q) {
		q_rgid = rgid;
		s_rgid1 = backpack->y_rgid + 1;
	} else {
		q_rgid = backpack->y_rgid;
		s_rgid1 = rgid1;
	}
	int *selection_p = backpack->direct_out + q_rgid;
	if (backpack->select_mode == COUNT_HITS) {
		(*selection_p)++;
		return;
	}
	if (*selection_p == NA_INTEGER ||
	    (backpack->select_mode == FIRST_HIT) == (s_rgid1 < *selection_p))
		*selection_p = s_rgid1;
	return;
}

typedef void (*GetYOverlapsFunType)(const void *x_nclist,
				    const Backpack *backpack);

/* Returns 1 if range y can be skipped, and 0 otherwise. */
static int find_y_pp_overlaps(int j,
		const int *y_start_p, const int *y_end_p, const int *y_space_p,
		int select_mode,
		const void *pp, GetYOverlapsFunType get_y_overlaps_fun,
		Backpack *backpack, IntAE *xh_buf)
{
	int y_start = y_start_p[j];
	int y_end = y_end_p[j];
	if (y_end - y_start < backpack->min_overlap_score0)
		return 1;
	update_backpack(backpack, j,
			y_start, y_end, y_space_p == NULL ? 0 : y_space_p[j],
			xh_buf);
	/* pass 0 */
	get_y_overlaps_fun(pp, backpack);
	if (backpack->circle_len == NA_INTEGER)
		return 0;
	if (select_mode == ARBITRARY_HIT && !backpack->pp_is_q &&
	    backpack->direct_out[j] != NA_INTEGER)
		return 0;
	/* pass 1 */
	shift_y(backpack, -backpack->circle_len);
	get_y_overlaps_fun(pp, backpack);
	if (select_mode == ARBITRARY_HIT && !backpack->pp_is_q &&
	    backpack->direct_out[j] != NA_INTEGER)
		return 0;
	/* pass 2 */
	shift_y(backpack, 2 * backpack->circle_len);
	get_y_overlaps_fun(pp, backpack);
	return 0;
}

/* 'select_mode' should only be ALL_HITS or COUNT_HITS. If the latter,
   then 'circle_len' is expect to be != NA_INTEGER.
   Thread-safe only if one of the three following conditions is satisfied:
     (B1) hits_buf != NULL && select_mode == ALL_HITS
     (B2) hits_buf != NULL && circle_len == NA_INTEGER
     (B3) hits_buf != NULL && select_mode == COUNT_HITS && !pp_is_q */
static void postprocess_y_hits(int j,
		int select_mode, int circle_len, int pp_is_q,
		IntAE *xh_buf, IntAE *yh_buf, IntAEAE *hits_buf,
		int *direct_out)
{
	if (select_mode != ALL_HITS) {
		if (select_mode != COUNT_HITS)
			error("IRanges internal error in postprocess_hits(): "
			      "'select_mode' must be ALL_HITS or COUNT_HITS");
		if (circle_len == NA_INTEGER)
			error("IRanges internal error in postprocess_hits(): "
			      "'circle_len' cannot be NA when 'select_mode' "
			      "is COUNT_HITS");
	}
	size_t buf_offset = hits_buf == NULL ? IntAE_get_nelt(yh_buf) : 0;
	if (circle_len != NA_INTEGER) {
		/* delete duplicates */
		IntAE_qsort(xh_buf, buf_offset, 0);
		IntAE_uniq(xh_buf, buf_offset);
	}
	size_t xh_buf_nelt = IntAE_get_nelt(xh_buf);
	if (select_mode == ALL_HITS) {
		if (hits_buf != NULL)
			return;
		j++;  /* 1-based */
		for (size_t k = buf_offset; k < xh_buf_nelt; k++)
			IntAE_insert_at(yh_buf, k, j);
		return;
	}
	/* At this point 'select_mode' == COUNT_HITS and 'circle_len' != NA. */
	if (pp_is_q) {
		for (size_t k = buf_offset; k < xh_buf_nelt; k++)
			direct_out[xh_buf->elts[k] - 1]++;
	} else {
		direct_out[j] += xh_buf_nelt - buf_offset;
	}
	if (hits_buf == NULL)
		IntAE_set_nelt(xh_buf, buf_offset);
	return;
}

#ifdef _OPENMP
static int thread_safe(int select_mode, int circle_len, int pp_is_q,
		       IntAEAE *hits_buf)
{
	int A1 = select_mode == ALL_HITS && hits_buf != NULL;
	int A2 = select_mode != ALL_HITS && !pp_is_q;
	int B123 = hits_buf != NULL &&
			(select_mode == ALL_HITS ||
			 circle_len == NA_INTEGER ||
			 (select_mode == COUNT_HITS && !pp_is_q));
	return (A1 || A2) && B123;
}
#endif

static void pp_find_overlaps(
		const int *q_start_p, const int *q_end_p,
		const int *q_space_p, const int *q_subset_p, int q_len,
		const int *s_start_p, const int *s_end_p,
		const int *s_space_p, const int *s_subset_p, int s_len,
		int maxgap, int minoverlap,
		int overlap_type, int select_mode,
		int circle_len, int nthread,
		const void *pp, int pp_is_q,
		GetYOverlapsFunType get_y_overlaps_fun,
		IntAE *qh_buf, IntAE *sh_buf, IntAEAE *hits_buf,
		int *direct_out)
{
	if (q_len == 0 || s_len == 0)
		return;
	const int *x_start_p, *x_end_p, *x_space_p,
		  *y_start_p, *y_end_p, *y_space_p, *y_subset_p;
	int y_len;
	IntAE *xh_buf = NULL, *yh_buf = NULL;
	if (pp_is_q) {
		x_start_p = q_start_p;
		x_end_p = q_end_p;
		x_space_p = q_space_p;
		y_start_p = s_start_p;
		y_end_p = s_end_p;
		y_space_p = s_space_p;
		y_subset_p = s_subset_p;
		y_len = s_len;
		if (hits_buf == NULL) {
			xh_buf = qh_buf;
			yh_buf = sh_buf;
		}
		if (overlap_type == TYPE_WITHIN)
			overlap_type = TYPE_EXTEND;
		else if (overlap_type == TYPE_EXTEND)
			overlap_type = TYPE_WITHIN;
	} else {
		x_start_p = s_start_p;
		x_end_p = s_end_p;
		x_space_p = s_space_p;
		y_start_p = q_start_p;
		y_end_p = q_end_p;
		y_space_p = q_space_p;
		y_subset_p = q_subset_p;
		y_len = q_len;
		if (hits_buf == NULL) {
			xh_buf = sh_buf;
			yh_buf = qh_buf;
		}
	}
	int backpack_select_mode;
	if (select_mode == COUNT_HITS && circle_len != NA_INTEGER) {
		backpack_select_mode = ALL_HITS;
	} else {
		backpack_select_mode = select_mode;
	}
#ifdef _OPENMP
	int safe = thread_safe(select_mode, circle_len, pp_is_q, hits_buf);
	if (!safe)
		nthread = 1;
	//Rprintf("using %d thread%s\n", nthread, nthread == 1 ? "" : "s");
	if (nthread == 1) {
#endif
		Backpack backpack = prepare_backpack(
					x_start_p, x_end_p, x_space_p,
					maxgap, minoverlap,
					overlap_type, backpack_select_mode,
					circle_len, pp_is_q, direct_out);
		for (int i = 0; i < y_len; i++) {
			int j = y_subset_p == NULL ? i : y_subset_p[i];
			if (hits_buf != NULL)
				xh_buf = hits_buf->elts[i];
			int miss = find_y_pp_overlaps(j,
					y_start_p, y_end_p, y_space_p,
					select_mode,
					pp, get_y_overlaps_fun,
					&backpack, xh_buf);
			if (miss || backpack_select_mode != ALL_HITS)
				continue;
			postprocess_y_hits(j,
					select_mode, circle_len, pp_is_q,
					xh_buf, yh_buf, hits_buf, direct_out);
		}
#ifdef _OPENMP
	} else {
		/* Let's use multiple threads. */
		int old_max_threads = omp_get_max_threads();
		omp_set_num_threads(nthread);
		#pragma omp parallel for schedule(static)
		for (int i = 0; i < y_len; i++) {
			Backpack backpack = prepare_backpack(
					x_start_p, x_end_p, x_space_p,
					maxgap, minoverlap,
					overlap_type, backpack_select_mode,
					circle_len, pp_is_q, direct_out);
			int j = y_subset_p == NULL ? i : y_subset_p[i];
			IntAE *xh_buf2 = hits_buf->elts[i];
			int miss = find_y_pp_overlaps(j,
					y_start_p, y_end_p, y_space_p,
					select_mode,
					pp, get_y_overlaps_fun,
					&backpack, xh_buf2);
			if (miss || backpack_select_mode != ALL_HITS)
				continue;
			postprocess_y_hits(j,
					select_mode, circle_len, pp_is_q,
					xh_buf2, yh_buf, hits_buf, direct_out);
		}
		omp_set_num_threads(old_max_threads);
	}
#endif
	return;
}


/****************************************************************************
 * int_bsearch()
 */

/* 'subset_len' is assumed to be > 0.
   Returns the first index 'n' for which 'base[subset[n]] >= min', or
   'subset_len' if there is no such index.
   TODO: Maybe move this to src/int_utils.c or src/sort_utils.c in S4Vectors
   package. */
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
	const int *rgidbuf;
	int nchildren, n, rgid;
	const NCList *child_nclist;

	rgidbuf = x_nclist->rgidbuf;
	nchildren = x_nclist->nchildren;
	n = int_bsearch(rgidbuf, nchildren, backpack->x_end_p,
			backpack->min_x_end);
	for (child_nclist = x_nclist->childrenbuf + n, rgidbuf = rgidbuf + n;
	     n < nchildren;
	     n++, child_nclist++, rgidbuf++)
	{
		rgid = *rgidbuf;
		if (backpack->x_start_p[rgid] > backpack->max_x_start)
			break;
		if (is_hit(rgid, backpack)) {
			report_hit(rgid, backpack);
			if (backpack->select_mode == ARBITRARY_HIT
			 && !backpack->pp_is_q)
				break;
		}
		if (child_nclist->nchildren != 0)
			NCList_get_y_overlaps_rec(child_nclist, backpack);
	}
	return;
}

static int find_landing_child(const NCList *nclist, const Backpack *backpack)
{
	int nchildren, n;

	nchildren = nclist->nchildren;
	if (nchildren == 0)
		return -1;
	n = int_bsearch(nclist->rgidbuf, nchildren, backpack->x_end_p,
			backpack->min_x_end);
	if (n >= nchildren)
		return -1;
	return n;
}

/* Non-recursive version of NCList_get_y_overlaps_rec(). */
static void NCList_get_y_overlaps(const NCList *top_nclist,
				  const Backpack *backpack)
{
	/* Incomplete top-down walk: only a pruned version of the full tree
	   (i.e. a subtree starting at the same top node) will be visited. */
	NCListWalkingStack *stack = get_empty_walking_stack();
	int n = find_landing_child(top_nclist, backpack);
	if (n < 0)
		return;
	const NCList *nclist = move_to_child(stack, top_nclist, n);
	while (nclist != NULL) {
		NCListWalkingStackElt *stack_elt =
			peek_NCListWalkingStackElt(stack);
		int rgid = GET_RGID(stack_elt);
		if (backpack->x_start_p[rgid] > backpack->max_x_start) {
			/* Skip all further siblings of 'nclist'. */
			nclist = move_to_right_uncle(stack);
			continue;
		}
		if (is_hit(rgid, backpack)) {
			report_hit(rgid, backpack);
			if (backpack->select_mode == ARBITRARY_HIT
			 && !backpack->pp_is_q)
				return;  /* we're done! */
		}
		n = find_landing_child(nclist, backpack);
		/* Skip first 'n' or all children of 'nclist'. */
		nclist = n >= 0 ?
			move_to_child(stack, nclist, n) :
			move_to_right_sibling_or_uncle(stack, nclist);
	}
	return;
}


/****************************************************************************
 * NCListAsINTSXP_get_y_overlaps()
 */

/* Recursive! */
static void NCListAsINTSXP_get_y_overlaps_rec(const int *x_nclist,
					      const Backpack *backpack)
{
	const int *rgid_p, *offset_p;
	int nchildren, n, rgid, offset;

	rgid_p = NCListAsINTSXP_RGIDS(x_nclist);
	nchildren = NCListAsINTSXP_NCHILDREN(x_nclist);
	n = int_bsearch(rgid_p, nchildren, backpack->x_end_p,
			backpack->min_x_end);
	for (rgid_p = rgid_p + n,
	     offset_p = NCListAsINTSXP_OFFSETS(x_nclist) + n;
	     n < nchildren;
	     n++, rgid_p++, offset_p++)
	{
		rgid = *rgid_p;
		if (backpack->x_start_p[rgid] > backpack->max_x_start)
			break;
		if (is_hit(rgid, backpack)) {
			report_hit(rgid, backpack);
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

static void find_overlaps(
		const int *q_start_p, const int *q_end_p,
		const int *q_space_p, const int *q_subset_p, int q_len,
		const int *s_start_p, const int *s_end_p,
		const int *s_space_p, const int *s_subset_p, int s_len,
		int maxgap, int minoverlap,
		int overlap_type, int select_mode,
		int circle_len, int nthread,
		SEXP nclist_sxp, int pp_is_q,
		IntAE *qh_buf, IntAE *sh_buf, IntAEAE *hits_buf,
		int *direct_out)
{
	NCList nclist;
	const void *pp;
	GetYOverlapsFunType get_y_overlaps_fun;

	if (q_len == 0 || s_len == 0)
		return;
	if (nclist_sxp == R_NilValue) {
		/* On-the-fly preprocessing. */
		if (pp_is_q)
			build_NCList(&nclist, q_start_p, q_end_p,
					      q_subset_p, q_len);
		else 
			build_NCList(&nclist, s_start_p, s_end_p,
					      s_subset_p, s_len);
		pp = &nclist;
		get_y_overlaps_fun =
		    (GetYOverlapsFunType) NCList_get_y_overlaps;
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
		circle_len, nthread,
		pp, pp_is_q, get_y_overlaps_fun,
		qh_buf, sh_buf, hits_buf, direct_out);
	if (nclist_sxp == R_NilValue)
		free_NCList(&nclist);
	return;
}


/****************************************************************************
 * Helper functions shared by C_find_overlaps_NCList() and
 * C_find_overlaps_in_groups_NCList()
 */

static int get_overlap_type(SEXP type)
{
	if (!IS_CHARACTER(type) || LENGTH(type) != 1)
		error("'type' must be a single string");
	type = STRING_ELT(type, 0);
	if (type == NA_STRING)
		error("'type' cannot be NA");
	const char *type0 = CHAR(type);
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

static int get_maxgap0(SEXP maxgap, int overlap_type)
{
	if (!IS_INTEGER(maxgap) || LENGTH(maxgap) != 1)
		error("'maxgap' must be a single integer");
	int maxgap0 = INTEGER(maxgap)[0];
	if (maxgap0 == NA_INTEGER)
		error("'maxgap' cannot be NA");
	if (maxgap0 < -1)
		error("'maxgap' must be >= -1");
	if (maxgap0 == -1 && overlap_type != TYPE_ANY)
		maxgap0 = 0;
	return maxgap0;
}

static int get_minoverlap0(SEXP minoverlap, int maxgap, int overlap_type)
{
	if (!IS_INTEGER(minoverlap) || LENGTH(minoverlap) != 1)
		error("'minoverlap' must be a single integer");
	int minoverlap0 = INTEGER(minoverlap)[0];
	if (minoverlap0 == NA_INTEGER)
		error("'minoverlap' cannot be NA");
	if (minoverlap0 < 0)
		error("'minoverlap' cannot be negative");
	if (overlap_type == TYPE_ANY && maxgap != -1 && minoverlap0 != 0)
		error("when 'type' is \"any\", at least one of 'maxgap' "
		      "and 'minoverlap' must be set to its default value");
	return minoverlap0;
}

static int get_circle_length(SEXP circle_length)
{
	if (!IS_INTEGER(circle_length) || LENGTH(circle_length) != 1)
		error("'circle_length' must be a single integer");
	int circle_len = INTEGER(circle_length)[0];
	if (circle_len != NA_INTEGER && circle_len <= 0)
		error("'circle_length' must be a single "
		      "positive integer or NA");
	return circle_len;
}

static int get_pp_is_q(int nclist_is_q, SEXP nclist, int q_len, int s_len)
{
	if (nclist_is_q != NA_INTEGER)
		return nclist_is_q;
	if (nclist != R_NilValue)
		error("'nclist_is_q' cannot be NA when 'nclist' is not NULL");
	return q_len < s_len;
}

/* Returned value is never greater than parallel::detectCores() or
   omp_get_max_threads() or STACK_POOL_SIZE. */
static int get_nthread0(SEXP nthread)
{
#ifdef _OPENMP
	int nthread0 = INTEGER(nthread)[0];
	/* Note that omp_get_num_procs() returns the same as
	   parallel::detectCores(). */
	int num_procs = omp_get_num_procs();
	if (nthread0 == NA_INTEGER) {
		/* No multithreading by default for now! See 'nthread' arg
		   in man/findOverlaps-methods.Rd for more info. */
		return 1;  // temporary!
		nthread0 = num_procs / 3;
	} else if (nthread0 > num_procs) {
		nthread0 = num_procs;
	}
	if (nthread0 < 1)
		return 1;
	/* Note that the "initial" value returned by omp_get_max_threads(),
	   that is, the value it returns **before** omp_set_num_threads() gets
	   called, is controlled by environment variable OMP_NUM_THREADS. */
	int max_threads = omp_get_max_threads();
	if (nthread0 > max_threads)
		nthread0 = max_threads;
	if (nthread0 >= STACK_POOL_SIZE)
		nthread0 = STACK_POOL_SIZE;
	return nthread0;
#else
	return 1;
#endif
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
 * C_find_overlaps_NCList()
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
SEXP C_find_overlaps_NCList(
		SEXP q_start, SEXP q_end,
		SEXP s_start, SEXP s_end,
		SEXP nclist, SEXP nclist_is_q,
		SEXP maxgap, SEXP minoverlap, SEXP type, SEXP select,
		SEXP circle_length, SEXP nthread)
{
	SEXP ans;

	const int *q_start_p, *q_end_p;
	int q_len = check_integer_pairs(q_start, q_end,
					&q_start_p, &q_end_p,
					"start(q)", "end(q)");
	const int *s_start_p, *s_end_p;
	int s_len = check_integer_pairs(s_start, s_end,
					&s_start_p, &s_end_p,
					"start(s)", "end(s)");
	int overlap_type = get_overlap_type(type);
	int maxgap0 = get_maxgap0(maxgap, overlap_type);
	int minoverlap0 = get_minoverlap0(minoverlap, maxgap0, overlap_type);
	int select_mode = get_select_mode(select);
	int circle_len = get_circle_length(circle_length);
	int pp_is_q = get_pp_is_q(LOGICAL(nclist_is_q)[0],
				  nclist, q_len, s_len);
	int nthread0 = get_nthread0(nthread);

	IntAE *qh_buf = NULL, *sh_buf = NULL;
	IntAEAE *hits_buf = NULL;
	if (select_mode == ALL_HITS || circle_len != NA_INTEGER) {
		if (nthread0 == 1) {
			qh_buf = new_IntAE(0, 0, 0);
			sh_buf = new_IntAE(0, 0, 0);
		} else {
			size_t buflen = pp_is_q ? s_len : q_len;
			hits_buf = new_IntAEAE(buflen, buflen);
		}
	}
	int *direct_out = NULL;
	if (select_mode != ALL_HITS) {
		ans = PROTECT(new_direct_out(q_len, select_mode));
		direct_out = INTEGER(ans);
	}
	//init_clock("find_overlaps: T2 = ");
	find_overlaps(
		q_start_p, q_end_p, NULL, NULL, q_len,
		s_start_p, s_end_p, NULL, NULL, s_len,
		maxgap0, minoverlap0, overlap_type, select_mode,
		circle_len, nthread0,
		nclist, pp_is_q,
		qh_buf, sh_buf, hits_buf, direct_out);
	//print_elapsed_time();
	if (select_mode != ALL_HITS) {
		UNPROTECT(1);
		return ans;
	}
	return hits_buf == NULL ?
		new_Hits("SortedByQueryHits",
			 qh_buf->elts, sh_buf->elts, IntAE_get_nelt(qh_buf),
			 q_len, s_len, !pp_is_q) :
		new_SortedByQueryHits_from_IntAEAE(hits_buf,
						   pp_is_q ? q_len : s_len,
						   pp_is_q);
}


/****************************************************************************
 * C_find_overlaps_in_groups_NCList()
 */

static const int *get_space_p(SEXP space, int expected_len, const char *prefix)
{
	if (space == R_NilValue)
		return NULL;
	if (LENGTH(space) != expected_len)
		error("'%s_space' must have the length of '%s_start'",
		      prefix, prefix);
	return INTEGER(space);
}

/* --- .Call ENTRY POINT ---
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
SEXP C_find_overlaps_in_groups_NCList(
		SEXP q_start, SEXP q_end, SEXP q_space, SEXP q_groups,
		SEXP s_start, SEXP s_end, SEXP s_space, SEXP s_groups,
		SEXP nclists, SEXP nclist_is_q,
		SEXP maxgap, SEXP minoverlap, SEXP type, SEXP select,
		SEXP circle_length)
{
	/* Check query. */
	const int *q_start_p, *q_end_p;
	int q_len = check_integer_pairs(q_start, q_end,
					&q_start_p, &q_end_p,
					"q_start", "q_end");
	const int *q_space_p = get_space_p(q_space, q_len, "q");
	CompressedIntsList_holder q_groups_holder =
			_hold_CompressedIntegerList(q_groups);
	int NG1 = _get_length_from_CompressedIntsList_holder(&q_groups_holder);

	/* Check subject. */
	const int *s_start_p, *s_end_p;
	int s_len = check_integer_pairs(s_start, s_end,
					&s_start_p, &s_end_p,
					"s_start", "s_end");
	const int *s_space_p = get_space_p(s_space, s_len, "s");
	CompressedIntsList_holder s_groups_holder =
			_hold_CompressedIntegerList(s_groups);
	int NG2 = _get_length_from_CompressedIntsList_holder(&s_groups_holder);

	int overlap_type = get_overlap_type(type);
	int maxgap0 = get_maxgap0(maxgap, overlap_type);
	int minoverlap0 = get_minoverlap0(minoverlap, maxgap0, overlap_type);
	int select_mode = get_select_mode(select);

	IntAE *qh_buf = new_IntAE(0, 0, 0);
	IntAE *sh_buf = new_IntAE(0, 0, 0);
	SEXP ans;
	int *direct_out = NULL;
	if (select_mode != ALL_HITS) {
		PROTECT(ans = new_direct_out(q_len, select_mode));
		direct_out = INTEGER(ans);
	}
	int NG = NG1 <= NG2 ? NG1 : NG2;
	for (int i = 0; i < NG; i++) {
		Ints_holder qi_group_holder =
			_get_elt_from_CompressedIntsList_holder(
						&q_groups_holder, i);
		int qi_len = qi_group_holder.length;
		Ints_holder si_group_holder =
			_get_elt_from_CompressedIntsList_holder(
						&s_groups_holder, i);
		int si_len = si_group_holder.length;
		SEXP nclist = VECTOR_ELT(nclists, i);
		int pp_is_q = get_pp_is_q(LOGICAL(nclist_is_q)[i],
					  nclist, qi_len, si_len);
		find_overlaps(
			q_start_p, q_end_p, q_space_p,
			qi_group_holder.ptr, qi_len,
			s_start_p, s_end_p, s_space_p,
			si_group_holder.ptr, si_len,
			maxgap0, minoverlap0, overlap_type, select_mode,
			INTEGER(circle_length)[i], 1,
			nclist, pp_is_q,
			qh_buf, sh_buf, NULL, direct_out);
	}
	if (select_mode != ALL_HITS) {
		UNPROTECT(1);
		return ans;
	}
	return new_Hits("SortedByQueryHits",
			qh_buf->elts, sh_buf->elts, IntAE_get_nelt(qh_buf),
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

