#include "IRanges.h"

#include "common.h"
#include "rbTree.h"
#include "localmem.h"

#define MAX(a, b)  (((a) > (b)) ? (a) : (b))

typedef struct _IntegerInterval {
  int start;
  int end;
} IntegerInterval;

typedef struct _IntegerIntervalNode {
  IntegerInterval interval;
  unsigned int index; /* identifies the interval */
  int maxEnd; /* maximum end value in children */
} IntegerIntervalNode;

static
int compare_interval(void *a, void *b) {
  IntegerInterval *ra = (IntegerInterval *)a;
  IntegerInterval *rb = (IntegerInterval *)b;
  if (ra->start < rb->start)
    return -1;
  if (rb->start < ra->start)
    return 1;
  if (rb->start == ra->start && ra->end != rb->end)
    return -1;
  return 0;
}

static
void _IntegerIntervalTree_free(SEXP r_tree) {
  struct rbTree *tree = (struct rbTree *)R_ExternalPtrAddr(r_tree);
  if (tree) {
    rbTreeFree(&tree);
    R_ClearExternalPtr(r_tree);
  }
}


void _IntegerIntervalTree_add(struct rbTree *tree, int start, int end,
                       unsigned int index) {
  IntegerIntervalNode tmpInterval = { start, end, index, 0 };
  IntegerIntervalNode *interval =
    lmCloneMem(tree->lm, &tmpInterval, sizeof(tmpInterval));
  rbTreeAdd(tree, interval);
}

/* need non-recursive implementation */
static void _IntegerIntervalNode_calc_max_end(struct rbTreeNode *node) {
  int maxEnd;
  maxEnd = ((IntegerInterval *)node->item)->end;
  if (node->left)
    _IntegerIntervalNode_calc_max_end(node->left);
  if (node->right)
    _IntegerIntervalNode_calc_max_end(node->right);
  if (node->left && node->right) {
    int childMax = MAX(((IntegerIntervalNode *)node->left->item)->maxEnd,
                       ((IntegerIntervalNode *)node->right->item)->maxEnd);
    maxEnd = MAX(childMax, maxEnd);
  } else if (node->left)
    maxEnd = MAX(((IntegerIntervalNode *)node->left->item)->maxEnd, maxEnd);
  else if (node->right)
    maxEnd = MAX(((IntegerIntervalNode *)node->right->item)->maxEnd, maxEnd);
  ((IntegerIntervalNode *)node->item)->maxEnd = maxEnd;
}

static void _IntegerIntervalTree_calc_max_end(struct rbTree *tree) {
  if (tree->root)
    _IntegerIntervalNode_calc_max_end(tree->root);
}

struct rbTree *_IntegerIntervalTree_new(void) {
  return rbTreeNew(compare_interval);
}

SEXP IntegerIntervalTree_new(SEXP r_ranges) {
  struct rbTree *tree = _IntegerIntervalTree_new();
  int i;
  SEXP r_tree;
  SEXP start = _get_IRanges_start(r_ranges);
  SEXP width = _get_IRanges_width(r_ranges);
  
  for (i = 0; i < nrows(r_ranges); i++) {
    _IntegerIntervalTree_add(tree, INTEGER(start)[i],
                      INTEGER(start)[i] + INTEGER(width)[i] - 1, i+1);
  }
  
  _IntegerIntervalTree_calc_max_end(tree);
  r_tree = R_MakeExternalPtr(tree, R_NilValue, R_NilValue);
  R_RegisterCFinalizer(r_tree, _IntegerIntervalTree_free);
  
  return r_tree;
}

/* if results is NULL, returns index of first overlap, otherwise
   adds every hit to results and returns hit count */ 
int _IntegerIntervalTree_overlap(struct rbTree *tree, IntegerInterval *query,
                                 struct slInt **results)
{
  struct rbTreeNode *p, *nextP;
  int count = 0;
  
  for (p = tree->root; p != NULL; p = nextP) {
    IntegerInterval *interval = (IntegerInterval *)p->item;
    /* first, check for overlap */
    /*Rprintf("subject: %d,%d,%d / query: %d,%d\n", interval->start,
      interval->end, ((IntegerIntervalNode *)interval)->maxEnd,
      query->start, query->end);*/
    if (interval->start <= query->end && interval->end >= query->start) {
      int result = ((IntegerIntervalNode *)interval)->index;
      if (results) {
        slAddHead(results, slIntNew(result));
        count++;
      } else return result;
    }
    /* otherwise keep searching */
    /* only go left if overlap is possible */
    if(p->left &&
       ((IntegerIntervalNode *)p->left->item)->maxEnd >= query->start) {
      nextP = p->left;
    } else nextP = p->right; /* otherwise, we can only go right */
  }
  
  return count;
}

SEXP IntegerIntervalTree_overlap(SEXP r_tree, SEXP r_ranges) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  SEXP r_results = allocVector(INTSXP, nrows(r_ranges));
  int i;

  SEXP start = _get_IRanges_start(r_ranges);
  SEXP width = _get_IRanges_width(r_ranges);
  
  for (i = 0; i < LENGTH(r_results); i++) {
    IntegerInterval query;
    int result;
    query.start = INTEGER(start)[i];
    query.end = INTEGER(start)[i] + INTEGER(width)[i] - 1;
    result = _IntegerIntervalTree_overlap(tree, &query, NULL);
    if (result == 0)
      result = NA_INTEGER;
    INTEGER(r_results)[i] = result;
  }
  return r_results;
}

SEXP IntegerIntervalTree_overlap_multiple(SEXP r_tree, SEXP r_ranges) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  struct slInt *results = NULL, *result;
  SEXP r_query_start = allocVector(INTSXP, _get_IRanges_length(r_ranges));
  SEXP r_subject, r_results, r_dims;
  int i, next_start = 0;

  SEXP start = _get_IRanges_start(r_ranges);
  SEXP width = _get_IRanges_width(r_ranges);

  PROTECT(r_results = NEW_OBJECT(MAKE_CLASS("RangesMatching")));
  r_dims = allocVector(INTSXP, 2);
  INTEGER(r_dims)[0] = tree->n;
  INTEGER(r_dims)[1] = _get_IRanges_length(r_ranges);
  SET_SLOT(r_results, install("Dims"), r_dims);
  SET_SLOT(r_results, install("p"), r_query_start);
  
  for (i = 0; i < LENGTH(r_results); i++) {
    IntegerInterval query;
    struct slList *result;
    int count = 0;
    INTEGER(r_query_start)[i] = next_start;
    query.start = INTEGER(start)[i];
    query.end = INTEGER(start)[i] + INTEGER(width)[i] - 1;
    count = _IntegerIntervalTree_overlap(tree, &query, &results);
    next_start += count;
  }

  slReverse(&results);
  r_subject = allocVector(INTSXP, next_start);
  SET_SLOT(r_results, install("j"), r_subject);
  for (result = results, i = 0; result != NULL; result = result->next, i++) {
    INTEGER(r_subject)[i] = result->val;
  }
  
  slFreeList(&results);
  UNPROTECT(1);
  
  return r_results;
}

static void _IntegerIntervalTreeNode_dump(void *item, FILE *file) {
  IntegerInterval *node = (IntegerInterval *)item;
  fprintf(file, "%d:%d / %d", node->start, node->end,
          ((IntegerIntervalNode *)node)->maxEnd);
}

SEXP IntegerIntervalTree_dump(SEXP r_tree) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  rbTreeDump(tree, stdout, _IntegerIntervalTreeNode_dump);
  return(R_NilValue);
}
