\name{IRanges-class}
\docType{class}

% IRanges objects:
\alias{class:IRanges}
\alias{IRanges-class}
\alias{IRanges}

\alias{start,IRanges-method}
\alias{width,IRanges-method}
\alias{names,IRanges-method}
\alias{start<-,IRanges-method}
\alias{width<-,IRanges-method}
\alias{end<-,IRanges-method}
\alias{names<-,IRanges-method}
\alias{update,IRanges-method}
\alias{[,IRanges-method}
\alias{c,IRanges-method}

% NormalIRanges objects:
\alias{class:NormalIRanges}
\alias{NormalIRanges-class}
\alias{NormalIRanges}

\alias{isEmpty,NormalIRanges-method}
\alias{max,NormalIRanges-method}
\alias{min,NormalIRanges-method}


\title{IRanges and NormalIRanges objects}

\description{
  The IRanges class is a simple implementation of the \link{Ranges}
  container where 2 integer vectors of the same length are used to
  store the start and width values.
  See the \link{Ranges} virtual class for a formal definition of
  \link{Ranges} objects and for their methods (all of them should
  work for IRanges objects).

  A NormalIRanges object is just an IRanges object that is guaranteed
  to be "normal". See the Normality section in the man page for
  \link{Ranges} objects for the definition and properties
  of "normal" \link{Ranges} objects.
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{IRanges(start=NULL, end=NULL, width=NULL)}:
      Return the IRanges object containing the ranges specified by \code{start},
      \code{end} and \code{width}.
      Exactly two out of the \code{start}, \code{end} and \code{width}
      arguments must be supplied as integer vectors (with no \code{NA}s)
      and the other argument must be \code{NULL}.
      If \code{start} and \code{end} are supplied, then they must be
      vectors of the same length.
      If \code{start} and \code{width} (or \code{end} and \code{width})
      are supplied, then the length of \code{width} must be <= to the
      length of \code{start} and, if it is <, then \code{width} is expanded
      cyclically to the length of \code{start}.
    }
  }
}

\section{Methods for NormalIRanges objects}{
  \describe{
    \item{}{
      \code{max(x)}:
      The maximum value in the finite set of integers represented by \code{x}.
    }
    \item{}{
      \code{min(x)}:
      The minimum value in the finite set of integers represented by \code{x}.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{Ranges-class},
  \link{IRanges-utils},
  \link{IRanges-setops}
}

\examples{
  ## Using an IRanges object for storing a big set of ranges is more
  ## efficient than using a standard R data frame:
  N <- 2000000L  # nb of ranges
  W <- 180L      # width of each range
  start <- 1L
  end <- 50000000L
  set.seed(777)
  range_starts <- sort(sample(end-W+1L, N))
  range_widths <- rep.int(W, N)
  ## Instantiation is faster
  system.time(x <- IRanges(start=range_starts, width=range_widths))
  system.time(y <- data.frame(start=range_starts, width=range_widths))
  ## Subsetting is faster
  system.time(x16 <- x[c(TRUE, rep.int(FALSE, 15))])
  system.time(y16 <- y[c(TRUE, rep.int(FALSE, 15)), ])
  ## Internal representation is more compact
  object.size(x16)
  object.size(y16)
}

\keyword{methods}
\keyword{classes}
