\name{IRanges-class}
\docType{class}

% IRanges objects:
\alias{class:IRanges}
\alias{IRanges-class}
\alias{IRanges}

\alias{length,IRanges-method}
\alias{start,IRanges-method}
\alias{width,IRanges-method}
\alias{names,IRanges-method}
\alias{isNormal}
\alias{isNormal,IRanges-method}
\alias{whichFirstNotNormal}
\alias{whichFirstNotNormal,IRanges-method}
\alias{start<-,IRanges-method}
\alias{width<-,IRanges-method}
\alias{end<-,IRanges-method}
\alias{names<-,IRanges-method}
\alias{update,IRanges-method}
\alias{[,IRanges,ANY,ANY,ANY-method}
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

  A NormalIRanges object is an IRanges object that is "normal".
  See the Normality section below for the definition and properties
  of normal IRanges objects.
}

\details{
  See the \link{Ranges} virtual class for a formal definition of
  \link{Ranges} objects.
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{IRanges(start=NULL, end=NULL, width=NULL)}:
      Return the IRanges object containing the ranges specified by \code{start},
      \code{end} and \code{width}.
      Exactly two of the \code{start}, \code{end} and \code{width}
      arguments must be specified as integer vectors (with no \code{NA}s)
      and the other argument must be \code{NULL}.
      If \code{start} and \code{end} are specified, then they must be
      vectors of the same length.
      If \code{start} and \code{width} (or \code{end} and \code{width})
      are specified, then the length of \code{width} must be <= to the
      length of \code{start} and, if it is <, then \code{width} is expanded
      cyclically to the length of \code{start}.
    }
  }
}

\section{Normality}{
  A NormalIRanges object is an IRanges object that is "normal".

  An IRanges object is said to be "normal" when its ranges are:
    (a) not empty (i.e. they have a non-null width);
    (b) not overlapping;
    (c) ordered from left to right;
    (d) not even adjacent (i.e. there must be a non empty gap between 2
        consecutive ranges).
  If \code{x} is an IRanges object with more than one element (i.e.
  \code{length(x) >= 2}), then \code{x} is normal iff:
  \preformatted{  start(x)[i] <= end(x)[i] < start(x)[i+1] <= end(x)[i+1]}
  for every 1 <= \code{i} < \code{length(x)}.
  If \code{length(x) == 1}, then \code{x} is normal iff \code{width(x) >= 1}.
  If \code{length(x) == 0}, then \code{x} is normal.

  An IRanges object can be used to represent an arbitrary finite set of
  integers (that are not necessarily consecutive).
  Now the 2 most interesting properties of normal IRanges objects are that:
  (1) they are the "best" (in terms of storage space) IRanges objects for
  representing arbitrary finite sets of integers and (2) the mapping between
  finite sets of integers and normal IRanges objects is one-to-one.
  More precisely, if \code{x} is an IRanges object, then it can be seen
  as representing the set of integers obtained by taking the union of
  all its ranges.
  Inversely, since any finite set of integers can be obtained by a finite
  union of ranges, then it can be represented by an IRanges object, but this
  representation is clearly not unique.
  However, among all the IRanges objects that represent (or map) the
  same finite set of integers, only one is normal, and this normal
  representation is minimal in terms of length (and therefore in terms
  of storage space).

  Use the \code{isNormal} method to check whether an IRanges object is
  normal or not. In the code snippet below, \code{x} is an IRanges object.

  \describe{
    \item{}{
      \code{isNormal(x)}:
      Return a logical value indicating whether \code{x} is normal or not.
    }
    \item{}{
      \code{whichFirstNotNormal(x)}:
      Return \code{NA} if \code{x} is normal, or the smallest valid indice
      \code{i} in \code{x} for which \code{x[1:i]} is not normal.
    }
    \item{}{
      \code{max(x)}:
      (Defined for NormalIRanges objects only.)
      The maximum value in the finite set of integers represented by \code{x}.
    }
    \item{}{
      \code{min(x)}:
      (Defined for NormalIRanges objects only.)
      The minimum value in the finite set of integers represented by \code{x}.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{Ranges-class},
  \link{IRanges-utils}
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
