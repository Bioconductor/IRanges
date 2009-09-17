\name{IntervalTree-class}
\docType{class}
\alias{IntervalTree-class}

% constructor
\alias{IntervalTree}

% coercion
\alias{coerce,IRanges,IntervalTree-method}
\alias{coerce,Ranges,IntervalTree-method}
\alias{coerce,IntervalTree,IRanges-method}

% accessors
\alias{length,IntervalTree-method}
\alias{start,IntervalTree-method}
\alias{end,IntervalTree-method}

% overlap computation
\alias{findOverlaps}
\alias{findOverlaps,Ranges,IntervalTree-method}
\alias{findOverlaps,Ranges,Ranges-method}
\alias{findOverlaps,Ranges,missing-method}
\alias{findOverlaps,integer,Ranges-method}
\alias{findOverlaps,RangesList,RangesList-method}
\alias{findOverlaps,RangedData,RangedData-method}
\alias{findOverlaps,RangesList,RangedData-method}
\alias{findOverlaps,RangedData,RangesList-method}
\alias{\%in\%,Ranges,Ranges-method}
\alias{\%in\%,RangesList,RangesList-method}
\alias{\%in\%,RangedData,RangedData-method}
\alias{\%in\%,RangedData,RangesList-method}
\alias{\%in\%,RangesList,RangedData-method}
\alias{match,Ranges,Ranges-method}
\alias{match,RangesList,RangesList-method}
\alias{countOverlaps}
\alias{countOverlaps,Ranges,Ranges-method}

\title{Interval Search Trees}
\description{
  Efficiently perform overlap queries with an interval tree.
}
\details{
  A common type of query that arises when working with intervals is
  finding which intervals in one set overlap those in another. An
  efficient family of algorithms for answering such queries is known as
  the Interval Tree. This implementation makes use of the augmented tree
  algorithm from the reference below, but heavily adapts it for the use
  case of large, sorted query sets.

  The simplest approach is to call the \code{findOverlaps} function on a
  \code{Ranges} or other object with range information, as described in
  the following section.

  An \code{IntervalTree} object is a derivative of
  \code{\linkS4class{Ranges}} and stores its ranges as a tree that is
  optimized for overlap queries. Thus, for repeated queries against the
  same subject, it is more efficient to create an \code{IntervalTree}
  once for the subject using the constructor described below and then
  perform the queries against the \code{IntervalTree} instance.
}

\section{Finding Overlaps}{
  This main purpose of the interval tree is to optimize the search for
  ranges overlapping those in a query set. The interface for this
  operation is the \code{\link{findOverlaps}} function.

  \describe{
    \item{}{
      \code{findOverlaps(query, subject = query, maxgap = 0, multiple = TRUE,
        drop = FALSE)}:

      Find the intervals in \code{query}, a \code{Ranges},
      \code{RangesList}, \code{RangedData} or \code{integer} vector (to
      be converted to length-one ranges), that overlap with the
      intervals \code{object}, a \code{Ranges}, \code{RangesList}, or
      \code{RangedData}. If \code{query} is omitted, \code{object} is
      queried against itself. if \code{query} is unsorted, it is sorted
      first, so it is usually better to sort up-front, to avoid a sort
      with each \code{findOverlaps} call. Intervals with a separation of
      \code{maxgap} or less are considered to be overlapping.
      \code{maxgap} should be a scalar, non-negative, non-NA number.
      When \code{multiple} (a scalar non-NA logical) is \code{TRUE}, 
      the results are returned as a \code{\linkS4class{RangesMatching}}
      object.

      If \code{multiple} is \code{FALSE}, at most one overlapping
      interval in \code{object} is returned for each interval in
      \code{query}. The matchings are returned as an integer vector of
      length \code{length(query)}, with \code{NA} indicating intervals
      that did not overlap any intervals in \code{object}. This is
      analogous to the default behavior of the \code{\link{match}}
      function.

      If \code{query} is a \code{\linkS4class{RangesList}} or
      \code{\linkS4class{RangedData}}, \code{object} must be a
      \code{RangesList} or \code{RangedData}.  If both lists have names,
      each element from the subject is paired with the element from the
      query with the matching name, if any. Otherwise, elements are
      paired by position. The overlap is then computed between the pairs
      as described above. If \code{multiple} is \code{TRUE}, a
      \code{\linkS4class{RangesMatchingList}} is returned, otherwise a
      list of integer vectors or, if \code{drop} is \code{TRUE}, an
      integer vector with indices offset to align with the unlisted
      \code{query}. When \code{drop} is \code{FALSE}, an
      \code{\linkS4class{IntegerList}} is returned, where each element
      of the result corresponds to a space in \code{query}. For spaces
      that did not exist in \code{object}, the overlap is nil.
    }
    \item{}{
      \code{x \%in\% table}:
      Shortcut for finding the ranges in \code{x} that overlap any of
      the ranges in \code{table}. Both \code{x} and \code{table} should
      be \code{Ranges}, \code{RangesList} or \code{RangedData}
      objects. For \code{Ranges} objects, the result is a \code{logical}
      vector of length equal to the number of ranges in \code{x}. For
      \code{RangesList} and \code{RangedData} objects, the result is a
      \code{\linkS4class{LogicalList}} object, where each element of the
      result corresponds to a space in \code{x}.
    }
    \item{}{
      \code{match(x, table, nomatch = NA_integer_, incomparables =
        NULL)}:
      Returns an integer vector of length \code{length(x)}, containing
      the index of the first overlapping range in \code{table} for each
      range in \code{x}. If a range in \code{x} does not overlap any
      ranges in \code{table}, its value is \code{nomatch}. The \code{x}
      and \code{table} arguments should either be both \code{Ranges}
      objects or both \code{RangesList} objects, in which case the
      indices are into the unlisted \code{table}. The
      \code{incomparables} argument is currently ignored.
    }
    \item{}{
      \code{countOverlaps(query, subject)}: Return the count of the
      number of ranges in \code{query} that overlap a range in \code{subject}.
    }
  }
  
}

\section{Constructor}{
  \describe{
    \item{}{IntervalTree(ranges): Creates an \code{IntervalTree} from the
      ranges in \code{ranges}, an object coercible to
      \code{IntervalTree}, such as an \code{\linkS4class{IRanges}} object.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as(from, "IRanges")}: Imports the ranges in
      \code{from}, an \code{IntervalTree}, to an
      \code{\linkS4class{IRanges}}.}
    \item{}{\code{as(from, "IntervalTree")}: Constructs an
      \code{IntervalTree} representing \code{from}, a \code{Ranges}
      object that is coercible to \code{IRanges}.
    }
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{length(x)}: Gets the number of ranges stored in the
      tree. This is a fast operation that does not bring the ranges into
      R.}
    \item{}{\code{start(x)}: Get the starts of the ranges.}
    \item{}{\code{end(x)}: Get the ends of the ranges.}
  }
}

\section{Notes on Time Complexity}{
  The cost of constructing an instance of the interval tree is a
  \code{O(n*lg(n))}, which makes it about as fast as other types of
  overlap query algorithms based on sorting. The good news is that the
  tree need only be built once per subject; this is useful in situations
  of frequent querying. Also, in this implementation the data is stored
  outside of R, avoiding needless copying. Of course, external storage
  is not always convenient, so it is possible to coerce the tree to an
  instance of \code{\linkS4class{IRanges}} (see the Coercion section).

  For the query operation, the running time is based on the query size
  \code{m} and the average number of hits per query \code{k}. The output
  size is then \code{max(mk,m)}, but we abbreviate this as
  \code{mk}. Note that when the \code{multiple} parameter is set to
  \code{FALSE}, \code{k} is fixed to 1 and drops out of this
  analysis. We also assume here that the query is sorted by start
  position (the \code{findOverlaps} function sorts the query if it is unsorted).
  
  An upper bound for finding overlaps is
  \code{O(min(mk*lg(n),n+mk))}. The fastest interval tree algorithm
  known is bounded by \code{O(min(m*lg(n),n)+mk)} but is a lot more
  complicated and involves two auxillary trees. The lower bound is
  \code{Omega(lg(n)+mk)}, which is almost the same as for returning
  the answer, \code{Omega(mk)}. The average is of course somewhere in
  between.

  This analysis informs the choice of which set of ranges to process
  into a tree, i.e. assigning one to be the subject and the other to be
  the query. Note that if \code{m > n}, then the running time is
  \code{O(m)}, and the total operation of complexity \code{O(n*lg(n) +
  m)} is better than if \code{m} and \code{n} were exchanged. Thus, for
  once-off operations, it is often most efficient to choose the smaller
  set to become the tree (but \code{k} also affects this). This is
  reinforced by the realization that if \code{mk} is about the same in
  either direction, the running time depends only on \code{n}, which
  should be minimized. Even in cases where a tree has already been
  constructed for one of the sets, it can be more efficient to build a
  new tree when the existing tree of size \code{n} is much larger than
  the query set of size \code{m}, roughly when \code{n > m*lg(n)}.
}

\references{Interval tree algorithm from: Cormen, Thomas H.; Leiserson,
  Charles E.; Rivest, Ronald L.; Stein, Clifford. Introduction to
  Algorithms, second edition, MIT Press and McGraw-Hill. ISBN
  0-262-53196-8}

\author{Michael Lawrence}
\seealso{
  \code{\linkS4class{Ranges}}, the parent of this class,
  \code{\linkS4class{RangesMatching}}, the result of an overlap query.
}
\examples{
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)

  ## at most one hit per query
  findOverlaps(query, tree, multiple = FALSE) # c(2, NA, 3)

  ## allow multiple hits
  findOverlaps(query, tree)

  ## overlap as long as distance <= 1
  findOverlaps(query, tree, maxgap = 1)

  ## shortcut
  findOverlaps(query, subject)

  ## query and subject are easily interchangeable
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)
  t(findOverlaps(query, tree))
  # the same as:
  findOverlaps(subject, query)

  ## one Ranges with itself
  findOverlaps(query)

  ## single points as query
  subject <- IRanges(c(1, 6, 13), c(4, 9, 14))
  findOverlaps(c(3L, 7L, 10L), subject, multiple=FALSE)
}

\keyword{classes}
\keyword{methods}
