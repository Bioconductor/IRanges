\name{IntervalForest-class}
\docType{class}
\alias{IntervalForest-class}

% constructor
\alias{IntervalForest}

% coercion
\alias{coerce,CompressedIRangesList,IntervalForest-method}
\alias{coerce,IntervalForest,CompressedIRangesList-method}
\alias{coerce,IntervalForest,IRanges-method}
\alias{coerce,RangesList,IntervalForest-method}

% accessors
\alias{length,IntervalForest-method}
\alias{start,IntervalForest-method}
\alias{end,IntervalForest-method}
\alias{width,IntervalForest-method}
\alias{elementLengths,IntervalForest-method}
\alias{names,IntervalForest-method}

% methods
\alias{[,IntervalForest-method}
\alias{show,IntervalForest-method}

\title{Interval Search Forests}
\description{
  Efficiently perform overlap queries with a set of interval trees.
}
\details{
  A common type of query that arises when working with intervals is
  finding which intervals in one set overlap those in another. An
  efficient family of algorithms for answering such queries is known as
  the Interval Tree. The \code{IntervalForest} class stores a set of Interval Trees
  corresponding to intervals that are partitioned into disjoint sets. The most
  efficient way to construct \code{IntervalForest} objects is to call the constructor below
  on a \link{CompressedIRangesList} object. See the \link{IntervalTree} class for the 
  underlying Interval Tree data structure.

  A canonical example of a compressed ranges list are \code{\link[GenomicRanges]{GenomicRanges}} 
  objects, where intervals are partitioned by their \code{seqnames}. See the 
  \link[GenomicRanges]{GIntervalTree} class to see the use of \code{IntervalForest} objects 
  in this case.

  The simplest approach for finding overlaps is to call the
  \code{\link{findOverlaps}} function on a \link{RangesList} object. 
  See the man page of \code{\link{findOverlaps-methods}}
  for how to use this and other related functions.
}

\section{Constructor}{
  \describe{
    \item{}{IntervalForest(rangesList): Creates an \code{IntervalForest} from the
      ranges list in \code{rangesList}, an object coercible to
      \code{CompressedIRangesList}.
    }
  }
}


\section{Accessors}{
  \describe{
    \item{}{\code{length(x)}: Gets the number of ranges stored in the
      forest. This is a fast operation that does not bring the ranges into
      R.}
    \item{}{\code{start(x)}: Get the starts of the ranges as a \code{CompressedIntegerList}.}
    \item{}{\code{end(x)}: Get the ends of the ranges as \code{CompressedIntegerList}.}
    \item{}{\code{x@partitioning}: The range partitioning of class \code{PartitioningByEnd}.}
    \item{}{\code{names(x)}: Get the names of the range partitioning.}
    \item{}{\code{elementLengths(x)}: The number of ranges in each partition.}
  }
}

\author{Hector Corrada Bravo, Michael Lawrence}
\seealso{
  \code{\link{findOverlaps-methods}} for finding/counting interval overlaps between
  two compressed lists of "range-based" objects,
  \code{\linkS4class{RangesList}}, the parent of this class,
  \code{\linkS4class{CompressedHitsList}}, set of hits between 2 list-like objects,
  \code{\link[GenomicRanges]{GIntervalTree}}, which uses \code{IntervalForest} objects.
}
\examples{
  query <- IRangesList(a=IRanges(c(1,4),c(5,7)),b=IRanges(9,10))
  subject <- IRangesList(a=IRanges(c(2,2),c(2,3)),b=IRanges(10,12))
  forest <- IntervalForest(subject)

  findOverlaps(query, forest)
}

\keyword{classes}
\keyword{methods}
