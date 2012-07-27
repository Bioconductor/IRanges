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

  The simplest approach for finding overlaps is to call the
  \code{\link{findOverlaps}} function on a \link{Ranges} or other object
  with range information. See the man page of \code{\link{findOverlaps}}
  for how to use this and other related functions.

  An \code{IntervalTree} object is a derivative of \link{Ranges} and
  stores its ranges as a tree that is optimized for overlap queries.
  Thus, for repeated queries against the same subject, it is more
  efficient to create an \code{IntervalTree} once for the subject using
  the constructor described below and then perform the queries against
  the \code{IntervalTree} instance.
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

\references{
  Interval tree algorithm from: Cormen, Thomas H.; Leiserson,
  Charles E.; Rivest, Ronald L.; Stein, Clifford. Introduction to
  Algorithms, second edition, MIT Press and McGraw-Hill. ISBN
  0-262-53196-8
}

\author{Michael Lawrence}
\seealso{
  \code{\link{findOverlaps}} for finding/counting interval overlaps between
  two "range-based" objects,
  \code{\linkS4class{Ranges}}, the parent of this class,
  \code{\linkS4class{Hits}}, set of hits between 2 vector-like objects.
}
\examples{
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)

  findOverlaps(query, tree)

  ## query and subject are easily interchangeable
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)

  t(findOverlaps(query, tree))
  # the same as:
  findOverlaps(subject, query)
}

\keyword{classes}
\keyword{methods}
