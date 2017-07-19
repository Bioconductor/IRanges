\name{range-squeezers}

\alias{range-squeezers}

\alias{ranges}
\alias{rglist}

\alias{rglist,Pairs-method}

\title{Squeeze the ranges out of a range-based object}

\description{
  S4 generic functions for squeezing the ranges out of a range-based object.

  These are analog to range squeezers \code{\link[GenomicRanges]{granges}}
  and \code{\link[GenomicRanges]{grglist}} defined in the \pkg{GenomicRanges}
  package, except that \code{ranges} returns the ranges in an \link{IRanges}
  object (instead of a \link[GenomicRanges]{GRanges} object for
  \code{\link[GenomicRanges]{granges}}), and \code{rglist} returns them in an
  \link{IRangesList} object (instead of a \link[GenomicRanges]{GRangesList}
  object for \code{\link[GenomicRanges]{grglist}}).
}

\usage{
ranges(x, use.names=TRUE, use.mcols=FALSE, ...)
rglist(x, use.names=TRUE, use.mcols=FALSE, ...)
}

\arguments{
  \item{x}{
    An object containing ranges e.g. a
    \link{Ranges}, \link[GenomicRanges]{GenomicRanges},
    \link[SummarizedExperiment]{RangedSummarizedExperiment},
    \link[GenomicAlignments]{GAlignments},
    \link[GenomicAlignments]{GAlignmentPairs}, or
    \link[GenomicAlignments]{GAlignmentsList} object,
    or a \link[S4Vectors]{Pairs} object containing ranges.
  }
  \item{use.names}{
    \code{TRUE} (the default) or \code{FALSE}.
    Whether or not the names on \code{x} (accessible with \code{names(x)})
    should be propagated to the returned object.
  }
  \item{use.mcols}{
    \code{TRUE} or \code{FALSE} (the default).
    Whether or not the metadata columns on \code{x} (accessible with
    \code{mcols(x)}) should be propagated to the returned object.
  }
  \item{...}{
    Additional arguments, for use in specific methods.
  }
}

\details{
  Various packages (e.g. \pkg{IRanges}, \pkg{GenomicRanges},
  \pkg{SummarizedExperiment}, \pkg{GenomicAlignments}, etc...)
  define and document various range squeezing methods for various types
  of objects.

  Note that these functions can be seen as \emph{object getters} or as
  functions performing coercion.

  For some objects (e.g. \link[GenomicAlignments]{GAlignments} and
  \link[GenomicAlignments]{GAlignmentPairs} objects defined in the
  \pkg{GenomicAlignments} package), \code{as(x, "IRanges")} and
  \code{as(x, "IRangesList")}, are equivalent to
  \code{ranges(x, use.names=TRUE, use.mcols=TRUE)} and
  \code{rglist(x, use.names=TRUE, use.mcols=TRUE)}, respectively.
}

\value{
  An \link{IRanges} object for \code{ranges}.

  An \link{IRangesList} object for \code{rglist}.

  If \code{x} is a vector-like object (e.g.
  \link[GenomicAlignments]{GAlignments}), the returned object is expected
  to be \emph{parallel} to \code{x}, that is, the i-th element in the output
  corresponds to the i-th element in the input.

  If \code{use.names} is TRUE, then the names on \code{x}
  (if any) are propagated to the returned object.
  If \code{use.mcols} is TRUE, then the metadata columns on \code{x}
  (if any) are propagated to the returned object.
}

\author{H. Pagès}

\seealso{
  \itemize{
    \item \link{IRanges} and \link{IRangesList} objects.

    \item \link[SummarizedExperiment]{RangedSummarizedExperiment} objects
          in the \pkg{SummarizedExperiment} packages.

    \item \link[GenomicAlignments]{GAlignments},
          \link[GenomicAlignments]{GAlignmentPairs},
          and \link[GenomicAlignments]{GAlignmentsList} objects in the
          \pkg{GenomicAlignments} package.
  }
}

\examples{
## See ?GAlignments in the GenomicAlignments package for examples of
## "ranges" and "rglist" methods.
}

\keyword{methods}
