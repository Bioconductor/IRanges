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
  if (rb->start < ra->start)
    return 1;
  /*
  if (ra->start < rb->start)
    return -1;
  if (rb->start == ra->start && ra->end != rb->end)
    return -1;
  return 0;
  */
  return -1;
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
  int i, nranges = _get_IRanges_length(r_ranges);
  SEXP r_tree;
  SEXP start = _get_IRanges_start(r_ranges);
  SEXP width = _get_IRanges_width(r_ranges);
  
  
  for (i = 0; i < nranges; i++) {
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
/* Running time: O(min(n,k*lg(n))), for tree of size 'n' with 'k' intervals
   overlapping the query */
/* A O(lg(n) + k) algorithm exists, but it's a lot more complicated */
/* and it may be that we achieve that anyway, in our use cases */
int _IntegerIntervalTree_overlap(struct rbTree *tree, IntegerInterval *query,
                                 struct slInt **results)
{
  struct rbTreeNode *p, *nextP;
  int count = 0, /* stack height */ height = 0;
  
  for (p = tree->root; p != NULL; p = nextP) {
    IntegerInterval *interval = (IntegerInterval *)p->item;
    /* is node on top of stack? */
    Rboolean visited = height && p == tree->stack[height-1];
    /* first, check for overlap */
    /*Rprintf("subject: %d,%d,%d / query: %d,%d, stack: %d\n", interval->start,
            interval->end, ((IntegerIntervalNode *)interval)->maxEnd,
            query->start, query->end, height);*/
    if (!visited &&
        interval->start <= query->end && interval->end >= query->start) {
      int result = ((IntegerIntervalNode *)interval)->index;
      if (results) {
        /*Rprintf("hit: %d\n", result);*/
        struct slInt *resultNode = slIntNew(result);
        slAddHead(results, resultNode);
        count++;
      } else return result;
    }
    /* otherwise keep searching */

    /* go left if node not yet visited and max end satisfied */
    if(p->left && !visited && 
       ((IntegerIntervalNode *)p->left->item)->maxEnd >= query->start) {
      if (results) /* if tracking multiple results, add to stack */
        tree->stack[height++] = p;
      nextP = p->left; 
    } else {
      if (visited) /* pop already visited node */ 
        height--; 
      /* go right if  sensible */
      if (p->right && interval->start < query->end &&
          ((IntegerIntervalNode *)p->right->item)->maxEnd >= query->start)
        nextP = p->right; 
      else if (height) /* return to ancestor if possible */
        nextP = tree->stack[height-1];
      else break;
    }
  }
  
  return count;
}

SEXP IntegerIntervalTree_overlap(SEXP r_tree, SEXP r_ranges) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  int i, nranges = _get_IRanges_length(r_ranges);
  SEXP r_results = allocVector(INTSXP, nranges);
  
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
  SEXP r_query_start, r_results, r_dims, r_matrix;
  int i, next_start = 0, nranges = _get_IRanges_length(r_ranges);

  SEXP start = _get_IRanges_start(r_ranges);
  SEXP width = _get_IRanges_width(r_ranges);

  PROTECT(r_query_start = allocVector(INTSXP, nranges + 1));
  for (i = 0; i < nranges; i++) {
    IntegerInterval query;
    INTEGER(r_query_start)[i] = next_start;
    query.start = INTEGER(start)[i];
    query.end = INTEGER(start)[i] + INTEGER(width)[i] - 1;
    next_start += _IntegerIntervalTree_overlap(tree, &query, &results);
  }
  INTEGER(r_query_start)[i] = next_start;
  slReverse(&results);
  
  if ((next_start+nranges+1) < ((double)tree->n*nranges)) {
    SEXP r_subject;
    PROTECT(r_matrix = NEW_OBJECT(MAKE_CLASS("ngCMatrix")));
    SET_SLOT(r_matrix, install("p"), r_query_start);
    r_subject = allocVector(INTSXP, next_start);
    SET_SLOT(r_matrix, install("i"), r_subject);
    for (result = results, i = 0; result != NULL; result = result->next, i++) {
      INTEGER(r_subject)[i] = result->val-1;
    }
  } else {
    SEXP r_elements;
    int j = 0;
    PROTECT(r_matrix = NEW_OBJECT(MAKE_CLASS("lgeMatrix")));
    r_elements = allocVector(LGLSXP, tree->n*nranges);
    for (i = 0; i < LENGTH(r_elements); i++)
      LOGICAL(r_elements)[i] = FALSE;
    SET_SLOT(r_matrix, install("x"), r_elements);
    result = results;
    for (i = 0; i < nranges; i++) {
      int offset = i * tree->n;
      while(j < INTEGER(r_query_start)[i+1]) {
        LOGICAL(r_elements)[offset + result->val-1] = TRUE;
        result = result->next;
        j++;
      }
    }
  }

  r_dims = allocVector(INTSXP, 2);
  INTEGER(r_dims)[0] = tree->n;
  INTEGER(r_dims)[1] = nranges;
  SET_SLOT(r_matrix, install("Dim"), r_dims);
  
  r_results = NEW_OBJECT(MAKE_CLASS("RangesMatching"));
  SET_SLOT(r_results, install("matchmatrix"), r_matrix);
  
  slFreeList(&results);
  
  UNPROTECT(2);
  
  return r_results;
}

SEXP IntegerIntervalTree_asRanges(SEXP r_tree) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  struct rbTreeNode *p = tree->root;
  int count = 0, height = 0;
  SEXP r_start, r_width, r_ranges;

  PROTECT(r_start = allocVector(INTSXP, tree->n));
  PROTECT(r_width = allocVector(INTSXP, tree->n));

  if (tree->n)
    while(1) {
      /* is node on top of stack? */
      Rboolean visited = height && p == tree->stack[height-1];
      /* first, check for overlap */
      if (!visited && p->left) {
        /* push current node onto stack */
        tree->stack[height++] = p;
        /* go left */
        p = p->left;
      } else {       /* can't go left, handle this node */
        IntegerInterval *interval = (IntegerInterval *)p->item;
        INTEGER(r_start)[count] = interval->start;
        INTEGER(r_width)[count] = interval->end - interval->start + 1;
        count++;
        if (visited)
          height--; /* pop handled node if on stack */
        if (p->right) /* go right if possible */
          p = p->right;
        else if (height) /* more on stack */
          p = tree->stack[height-1];
        else break; /* nothing left on stack, we're finished */
      }
    }

  r_ranges = _new_IRanges("IRanges", r_start, r_width, R_NilValue);
  
  UNPROTECT(2);
  return(r_ranges);
}

SEXP IntegerIntervalTree_length(SEXP r_tree) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  return(ScalarInteger(tree->n));
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
