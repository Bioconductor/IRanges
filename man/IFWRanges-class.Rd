\name{IFWRanges-class}
\docType{class}

% IFWRanges objects:
\alias{class:IFWRanges}
\alias{IFWRanges-class}
\alias{IFWRanges}

\alias{parallelSlotNames,IFWRanges-method}

% Accessors
\alias{start,IFWRanges-method}
\alias{width,IFWRanges-method}
\alias{names,IFWRanges-method}
\alias{width<-,IFWRanges-method}
\alias{names<-,IFWRanges-method}

% Coercion:
\alias{coerce,IntegerRanges,IFWRanges-method}
\alias{coerce,ANY,IFWRanges-method}

\alias{bindROWS,IFWRanges-method}

\title{Memory-efficient representation of fixed-width ranges on the space of integers.}

\description{
  The IFWRanges class is a container for storing a set of \emph{fixed-width}
  ranges. The IFWRanges class extends the \link{IntegerRanges} virtual class.
  Note that even though an \link{IRanges} object can be used for storing
  fixed-width ranges, using an IFWRanges object will be much more
  memory-efficient (roughly half the size in-memory).
}

\usage{
## IFWRanges constructor:
IFWRanges(start=NULL, end=NULL, width=NULL, names=NULL)
}

\arguments{
  \item{start, end, width}{
    \code{NULL} or vector of integers (eventually with NAs).
  }
  \item{names}{
    A character vector or \code{NULL}.
  }
}

\section{IFWRanges constructor}{
  Returns the IFWRanges object containing the fixed-width ranges specified
  by \code{start}, \code{end} and \code{width}. It is most efficient to
  supply the \code{IFWRanges()} constructor with an integer vector of start
  positions (\code{start}) and the fixed-width as an \code{integer(1)}
  vector (\code{width}) (see examples).

  If necessary \code{start}, \code{end} and \code{width} are recycled to
  the length of the longest (NULL arguments are filled with NAs). After
  this recycling, each row in the 3-column matrix obtained by binding
  those 3 vectors together is "solved" i.e. NAs are treated as
  unknown in the equation \code{end = start + width - 1}. Finally, the
  solved matrix is returned as an IFWRanges instance. An error is thrown
  if this solution results in more than one unique 'width' value (i.e. if
  the ranges are not fixed-width).

  Note that the \code{names} argument is never recycled (to remain
  consistent with what \code{`names<-`} does on standard vectors).
}

\value{
  An IFWRanges object.
}

\section{Coercion}{
  From \link{IntegerRanges} to IFWRanges:
  An \link{IntegerRanges} derivative \code{x} in which all the ranges have
  a fixed-width can be coerced to an IFWRanges object with
  \code{as(x, "IFWRanges")}.

  From IFWRanges to \link{IRanges}:
  An IFWRanges object \code{x} can be coerced to an \link{IRanges} object
  with \code{as(x, "IRanges")}. However, be aware that the resulting object
  will use twice as much memory as \code{x}!
  See "MEMORY USAGE" in the Examples section below.

  From IFWRanges to ordinary R objects:
  Like with any other \link{IntegerRanges} derivative, \code{as.character()},
  \code{as.factor()}, and \code{as.data.frame()} work on an IFWRanges object
  \code{x}.
}

\section{Subsetting}{
  An IFWRanges object can be subsetted exactly like an \link{IRanges} object.
}

\section{Concatenation}{
  IFWRanges objects can be concatenated with \code{c()} or \code{append()}
  \strong{provided that all objects have the same fixed-width}.
  See \code{?\link[S4Vectors]{c}} in the \pkg{S4Vectors} package for
  more information about concatenating Vector derivatives.
}

\section{Splitting and Relisting}{
  Like with an \link{IRanges} object, \code{split()} and \code{relist()} work
  on an IFWRanges object.
}

\note{
  Like for any \link[S4Vectors]{Vector} derivative, the length of an
  IFWRanges object cannot exceed \code{.Machine$integer.max} (i.e. 2^31 on
  most platforms).
}

\author{
  Peter Hickey
}

\seealso{
  \itemize{
    \item The \link[GenomicRanges]{GFWRanges} class in the
          \pkg{GenomicRanges} package for a memory-efficient representation
          of \emph{genomic fixed-width ranges} (i.e. genomic ranges of
          fixed-width).

    \item \link{IntegerRanges} and \link{IRanges} objects.

    \item \link{IPosRanges-comparison} for comparing and ordering integer
          ranges and/or positions.

    \item \link{findOverlaps-methods} for finding overlapping
          integer ranges and/or positions.

    \item \link{nearest-methods} for finding the nearest integer range
          and/or position.
  }
}

\examples{
## ---------------------------------------------------------------------
## A. USING THE IFWRanges() CONSTRUCTOR
## ---------------------------------------------------------------------

## Three ways to construct the same IFWRanges object.
IFWRanges(start=c(1L, 3L, 7L, 9L), width=1L) # Most efficient
IFWRanges(width=1, end=c(2, 4, 8, 10))
IFWRanges(start=c(1, 3, 7, 9), end=c(2, 4, 8, 10))

IFWRanges(-2, 20)  # only one range
IFWRanges(start=c(2, 0, NA), end=c(NA, NA, 14), width=12)
IFWRanges()  # IFWRanges instance of length zero
IFWRanges(names=character())

## ---------------------------------------------------------------------
## A. MANIPULATING IFWRanges OBJECTS
## ---------------------------------------------------------------------
## All the methods defined for IntegerRanges objects work on IRanges
## objects.
## See ?IntegerRanges for some examples.

## Concatenating IRanges objects
ifwr1 <- IFWRanges(c(1, 10, 20), width=5)
mcols(ifwr1) <- DataFrame(score=runif(3))
ifwr2 <- IFWRanges(c(101, 110, 120), width=5)
ifwr3 <- IRanges(c(1001, 1010, 1020), width=10)
c(ifwr1, ifwr2) ## Works because same width
\dontrun{
c(ifwr1, ifwr3) ## This will raise an error because different widths
}

## ---------------------------------------------------------------------
## MEMORY USAGE
## ---------------------------------------------------------------------

## Coercion to IRanges works...
ifwr4 <- IFWRanges(sample(10^6, 10^4), width=1)
ir4 <- as(ifwr4, "IRanges")
ir4
## ... but is generally not a good idea:
object.size(ifwr4)
object.size(ir4)  # 2-times bigger than the IFWRanges object.
}
\keyword{methods}
\keyword{classes}
