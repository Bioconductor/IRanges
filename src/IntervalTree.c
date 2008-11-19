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
    if (INTEGER(width)[i] > 0) /* only add non-empty ranges */
      _IntegerIntervalTree_add(tree, INTEGER(start)[i],
                               INTEGER(start)[i] + INTEGER(width)[i] - 1, i+1);
  }
  tree->n = nranges; /* kind of a hack - includes empty ranges */
  
  _IntegerIntervalTree_calc_max_end(tree);
  r_tree = R_MakeExternalPtr(tree, R_NilValue, R_NilValue);
  R_RegisterCFinalizer(r_tree, _IntegerIntervalTree_free);
  
  return r_tree;
}

/* if result_ints is NULL, returns indices of first overlaps, otherwise
   adds every hit to result_ints and returns index into it for each query */
/* running time: less than O(mk + mlogn) */
SEXP _IntegerIntervalTree_overlap(struct rbTree *tree, SEXP r_ranges,
                                  struct slRef **result_ints)
{
  struct rbTreeNode *p = tree->root;
  struct slRef *active = NULL, *active_head = NULL;
  int /* stack height */ height = 0, /* query counter */ m;
  Rboolean hit = FALSE; /* already hit current node? */
  
  int nranges = _get_IRanges_length(r_ranges);
  SEXP result_inds = allocVector(INTSXP, nranges + (result_ints != NULL));
  SEXP starts = _get_IRanges_start(r_ranges);
  SEXP widths = _get_IRanges_width(r_ranges);

  if (!p) { /* tree is empty */
    int fill = result_ints ? 0 : NA_INTEGER;
    for (m = 0; m < LENGTH(result_inds); m++) {
      INTEGER(result_inds)[m] = fill;
    }
    return(result_inds);
  }
  
  if (nranges)
    INTEGER(result_inds)[0] = 0;
  
  for (m = 0; m < nranges; m++) {
    int start = INTEGER(starts)[m];
    int end = INTEGER(widths)[m] + start - 1;
    if (end < start) { /* empty query range */
      if (result_inds)
        INTEGER(result_inds)[m+1] = INTEGER(result_inds)[m];
      else INTEGER(result_inds)[m] = NA_INTEGER;
      continue;
    }
    int count = 0;
    /* add hits from previous query, if still valid */
    /* this trick lets us avoid restarting the search for every query */
    if (result_ints) { 
      struct slRef *prev = NULL;
      for (struct slRef *a = active_head; a != NULL;) {
        IntegerInterval *interval = a->val;
        if (interval->end < start) { /* never see this again */
          /*Rprintf("goodbye: %d\n", ((IntegerIntervalNode*)interval)->index);*/
          struct slRef *next = a->next;
          if (prev)
            prev->next = next;
          else active_head = next;
          freeMem(a);
          a = next;
        } else {
          if (interval->start > end) /* no more hits here */
            break;
          struct slRef *resultNode = slRefNew(interval);
          /*Rprintf("p hit: %d\n", ((IntegerIntervalNode*)interval)->index);*/
          slAddHead(result_ints, resultNode); /* owns Node */
          count++;
          prev = a;
          a = a->next;
        }
      }
      active = prev; /* active is the tail of the list (when it matters) */
    }
    while(1) {
      IntegerInterval *interval = (IntegerInterval *)p->item;
      /* is node on top of stack? */
      Rboolean visited = height && p == tree->stack[height-1];
      /* have to retry nodes on stack after query switch */
      /*Rprintf("subject: %d,%d,%d / query: %d,%d, stack: %d\n",
              interval->start, interval->end,
              ((IntegerIntervalNode *)interval)->maxEnd, start, end, height);*/
      
      /* in-order traversal of tree */
      
      /* go left if node not yet visited and max end satisfied */
      if(p->left && !visited && 
         ((IntegerIntervalNode *)p->left->item)->maxEnd >= start) {
        tree->stack[height++] = p;
        p = p->left;
        /*Rprintf("left\n");*/
      } else {
        /* consider current node if not already checked */
        if (interval->start <= end && interval->end >= start) {
          /* Rprintf("hit: %d\n", ((IntegerIntervalNode *)interval)->index); */
          if (result_ints) {
            if (!hit) {
              struct slRef *resultNode = slRefNew(interval);
              slAddHead(result_ints, resultNode);
              resultNode = slRefNew(interval);
              if (active == NULL)
                active_head = resultNode;
              else active->next = resultNode;
              active = resultNode;
              count++;
            }
          } else {
            int result = ((IntegerIntervalNode *)interval)->index;
            INTEGER(result_inds)[m] = result;
            break;
          }
          hit = TRUE;
        }
        if (visited) { /* pop already visited node */ 
          height--;
          /*if (dirty) *//* retried this one */
          /*  dirty_level--;*/
        }
        
        /* go right if sensible */
        if (p->right && interval->start <= end &&
            ((IntegerIntervalNode *)p->right->item)->maxEnd >= start) {
          /* Rprintf("right\n"); */
          p = p->right;
          hit = FALSE;
        }
        else if (interval->start > end || !height) { /* no more hits */
          if (visited)
            height++; /* repush -- don't go left again */
          if (result_ints)
            INTEGER(result_inds)[m+1] = INTEGER(result_inds)[m] + count;
          else INTEGER(result_inds)[m] = NA_INTEGER;
          break;
        } else {
          p = tree->stack[height-1]; /* return to ancestor */
          hit = FALSE;
          /* Rprintf("up\n"); */
        }
      }
    }
  }
  
  return result_inds;
}

SEXP IntegerIntervalTree_overlap(SEXP r_tree, SEXP r_ranges) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  return _IntegerIntervalTree_overlap(tree, r_ranges, NULL);
}

SEXP IntegerIntervalTree_overlap_multiple(SEXP r_tree, SEXP r_ranges) {
  struct rbTree *tree = R_ExternalPtrAddr(r_tree);
  struct slRef *results = NULL, *result;
  SEXP r_query_start, r_results, r_dims, r_matrix;
  int i, nhits, nranges = _get_IRanges_length(r_ranges);
  
  r_query_start = _IntegerIntervalTree_overlap(tree, r_ranges, &results);
  PROTECT(r_query_start);
  nhits = INTEGER(r_query_start)[nranges];
  slReverse(&results);
  
  if (2*nhits < ((double)tree->n*nranges)) {
    SEXP r_subject, r_query;
    PROTECT(r_matrix = NEW_OBJECT(MAKE_CLASS("ngCMatrix")));
    /* thinking about going to a doublet matrix, rather than sparse */
    /*
    r_query = allocVector(INTSXP, nhits);
    for (i = 1; i < LENGTH(r_query_start); i++) {
      int prev = INTEGER(r_query_start)[i-1];
      int cur = INTEGER(r_query_start)[i] + prev;
      for (int j = prev; j < nj; j++) {
        INTEGER(r_query)[j] = i-1;
      }
    }
    SET_SLOT(r_matrix, install("j"), r_query);
    */
    SET_SLOT(r_matrix, install("p"), r_query_start);
    r_subject = allocVector(INTSXP, nhits);
    SET_SLOT(r_matrix, install("i"), r_subject);
    for (result = results, i = 0; result != NULL; result = result->next, i++) {
      INTEGER(r_subject)[i] = ((IntegerIntervalNode *)result->val)->index-1;
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
        int index = ((IntegerIntervalNode *)result->val)->index;
        LOGICAL(r_elements)[offset + index - 1] = TRUE;
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
  SET_SLOT(r_results, install("matchMatrix"), r_matrix);
  
  slFreeList(&results);
  
  UNPROTECT(2);
  
  return r_results;
}

SEXP IntegerIntervalTree_asIRanges(SEXP r_tree) {
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
