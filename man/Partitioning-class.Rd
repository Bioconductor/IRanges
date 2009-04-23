\name{Partitioning-class}
\docType{class}

\alias{class:Partitioning}
\alias{Partitioning-class}
\alias{Partitioning}

\alias{class:IPartitioning}
\alias{IPartitioning-class}
\alias{IPartitioning}

\alias{end,IPartitioning-method}
\alias{length,IPartitioning-method}
\alias{start,IPartitioning-method}
\alias{width,IPartitioning-method}
\alias{names,IPartitioning-method}
\alias{names<-,IPartitioning-method}

\alias{IPartitioning}

\alias{coerce,Ranges,IPartitioning-method}


\title{Partitioning and IPartitioning objects}

\description{
  A Partitioning object is a \link{Ranges} object where the ranges are
  adjacent starting at 1. Hence it covers an interval of the form 1:N
  with no overlap between the ranges.

  The IPartitioning container is a concrete implementation of the
  Partitioning VIRTUAL class that uses a compact internal representation.
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{IPartitioning(end=integer(), names=NULL)}:
      Return the IPartitioning object made of the partitions ending
      at the values specified by \code{end}. \code{end} must contain
      sorted non-negative integer values.
      Note that the \code{names} argument is never recycled (to remain
      consistent with what \code{`names<-`} does on standard vectors).
    }
  }
}

\details{
  An IPartitioning object is a particular type of \link{Ranges} object.
  Therefore all the methods that are defined for \link{Ranges} objects
  can also be used on an IPartitioning object.
  See \code{?Ranges} for a description of these methods.
}

\author{H. Pages}

\seealso{
  \link{Ranges-class},
  \link{IRanges-class},
  \link{successiveIRanges},
  \link[base]{cumsum},
  \link[base]{diff}
}

\examples{
  showClass("IPartitioning")  # shows (some of) the known subclasses

  ## ---------------------------------------------------------------------
  ## A. USING THE IPartitioning() CONSTRUCTOR
  ## ---------------------------------------------------------------------

  x <- IPartitioning(end=c(4, 7, 7, 8, 15))
  x  # the 3rd partition is empty
  names(x)[3] <- "empty partition" 
  as(x, "IRanges")


  IPartitioning(end=c(0, 0, 19), names=LETTERS[1:3])
  IPartitioning()  # no partition
  IPartitioning(end=integer(9))  # all partitions are empty

  ## ---------------------------------------------------------------------
  ## B. RELATIONSHIP WITH IRanges OBJECTS AND successiveIRanges()
  ## ---------------------------------------------------------------------

  mywidths <- c(4, 3, 0, 1, 7)

  ## The two following calls produce the same ranges:
  x1 <- successiveIRanges(mywidths)  # IRanges instance.
  x2 <- IPartitioning(end=cumsum(mywidths))  # IPartitioning instance.
  stopifnot(identical(as(x1, "IPartitioning"), x2))
}

\keyword{methods}
\keyword{classes}
