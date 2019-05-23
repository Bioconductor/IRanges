\name{IRanges-class}
\docType{class}

% IRanges objects:
\alias{class:IRanges}
\alias{IRanges-class}

\alias{parallelSlotNames,IRanges-method}

% Accessors
\alias{start,IRanges-method}
\alias{width,IRanges-method}
\alias{names,IRanges-method}
\alias{start<-,IRanges-method}
\alias{width<-,IRanges-method}
\alias{end<-,IRanges-method}
\alias{names<-,IRanges-method}
\alias{ranges,IntegerRanges-method}

\alias{isNormal,IRanges-method}

% NormalIRanges objects:
\alias{class:NormalIRanges}
\alias{NormalIRanges-class}
\alias{NormalIRanges}

\alias{isEmpty,NormalIRanges-method}
\alias{isNormal,NormalIRanges-method}
\alias{max,NormalIRanges-method}
\alias{min,NormalIRanges-method}

% Coercion:
\alias{coerce,IntegerRanges,IRanges-method}
\alias{coerce,logical,IRanges-method}
\alias{coerce,logical,NormalIRanges-method}
\alias{coerce,integer,IRanges-method}
\alias{coerce,integer,NormalIRanges-method}
\alias{coerce,numeric,IRanges-method}
\alias{coerce,numeric,NormalIRanges-method}
\alias{coerce,character,IRanges-method}
\alias{coerce,factor,IRanges-method}
\alias{coerce,ANY,IntegerRanges-method}


\title{IRanges and NormalIRanges objects}

\description{
  The IRanges class is a simple implementation of the \link{IntegerRanges}
  container where 2 integer vectors of the same length are used to
  store the start and width values.
  See the \link{IntegerRanges} virtual class for a formal definition of
  \link{IntegerRanges} objects and for their methods (all of them should
  work for IRanges objects).

  Some subclasses of the IRanges class are: NormalIRanges,
  \link{Views}, etc...

  A NormalIRanges object is just an IRanges object that is guaranteed
  to be "normal". See the Normality section in the man page for
  \link{IntegerRanges} objects for the definition and properties
  of "normal" \link{IntegerRanges} objects.
}

\section{Constructor}{
  See \code{?`\link{IRanges-constructor}`}.
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{ranges(x, use.names=FALSE, use.mcols=FALSE)}: Squeeze the ranges
      out of \link{IntegerRanges} object \code{x} and return them in an IRanges
      object \emph{parallel} to \code{x} (i.e. same length as \code{x}).
    }
    \item{}{
      \code{as(from, "IRanges")}: Creates an IRanges instance from an
      \link{IntegerRanges} derivative, or from a logical or integer vector.
      When \code{from} is a logical vector, the resulting IRanges object
      contains the indices for the runs of \code{TRUE} values.
      When \code{from} is an integer vector, the elements are either
      singletons or "increase by 1" sequences.
    }
    \item{}{
      \code{as(from, "NormalIRanges")}: Creates a NormalIRanges instance
      from a logical or integer vector. When \code{from} is an integer vector,
      the elements must be strictly increasing.
    }
  }
}

\section{Concatenation}{
  \describe{
    \item{}{
      \code{c(x, ..., ignore.mcols=FALSE)}:
      Concatenate IRanges object \code{x} and the IRanges objects in
      \code{...} together.
      See \code{?\link[S4Vectors]{c}} in the \pkg{S4Vectors} package for
      more information about concatenating Vector derivatives.
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

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item The \link[GenomicRanges]{GRanges} class in the \pkg{GenomicRanges}
          package for storing a set of \emph{genomic ranges}.

    \item The \link{IPos} class for representing a set of \emph{integer
          positions} (i.e. \emph{integer ranges} of width 1).

    \item \link{IPosRanges-comparison} for comparing and ordering integer
          ranges and/or positions.

    \item \link{IRanges-utils} for some utility functions for creating
          or modifying IRanges objects.

    \item \link{findOverlaps-methods} for finding overlapping
          integer ranges and/or positions.

    \item \link{intra-range-methods} and \link{inter-range-methods}
          for \emph{intra range} and \emph{inter range} transformations.

    \item \link{coverage-methods} for computing the coverage
          of a set of ranges and/or positions.

    \item \link{setops-methods} for set operations on IRanges objects.

    \item \link{nearest-methods} for finding the nearest integer
          range/position neighbor.
  }
}

\examples{
showClass("IRanges")  # shows the known subclasses

## ---------------------------------------------------------------------
## A. MANIPULATING IRanges OBJECTS
## ---------------------------------------------------------------------
## All the methods defined for IntegerRanges objects work on IRanges
## objects.
## See ?IntegerRanges for some examples.
## Also see ?`IRanges-utils` and ?`setops-methods` for additional
## operations on IRanges objects.
  
## Concatenating IRanges objects
ir1 <- IRanges(c(1, 10, 20), width=5)
mcols(ir1) <- DataFrame(score=runif(3))
ir2 <- IRanges(c(101, 110, 120), width=10)
mcols(ir2) <- DataFrame(score=runif(3))
ir3 <- IRanges(c(1001, 1010, 1020), width=20)
mcols(ir3) <- DataFrame(value=runif(3))
some.iranges <- c(ir1, ir2)
## all.iranges <- c(ir1, ir2, ir3) ## This will raise an error
all.iranges <- c(ir1, ir2, ir3, ignore.mcols=TRUE)
stopifnot(is.null(mcols(all.iranges)))

## ---------------------------------------------------------------------
## B. A NOTE ABOUT PERFORMANCE
## ---------------------------------------------------------------------
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
