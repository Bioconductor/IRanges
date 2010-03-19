\name{IRanges-class}
\docType{class}

% IRanges objects:
\alias{class:IRanges}
\alias{IRanges-class}

\alias{start,IRanges-method}
\alias{width,IRanges-method}
\alias{names,IRanges-method}
\alias{start<-,IRanges-method}
\alias{width<-,IRanges-method}
\alias{end<-,IRanges-method}
\alias{names<-,IRanges-method}
\alias{isNormal,IRanges-method}
\alias{update,IRanges-method}
\alias{[,IRanges-method}
\alias{[<-,IRanges-method}
\alias{seqselect,IRanges-method}
\alias{seqselect<-,IRanges-method}
\alias{window,IRanges-method}
\alias{c,IRanges-method}

% NormalIRanges objects:
\alias{class:NormalIRanges}
\alias{NormalIRanges-class}
\alias{NormalIRanges}

\alias{isEmpty,NormalIRanges-method}
\alias{max,NormalIRanges-method}
\alias{min,NormalIRanges-method}

% Coercion:
\alias{coerce,Ranges,IRanges-method}
\alias{coerce,logical,IRanges-method}
\alias{coerce,logical,NormalIRanges-method}
\alias{coerce,integer,IRanges-method}
\alias{coerce,integer,NormalIRanges-method}
\alias{coerce,numeric,IRanges-method}
\alias{coerce,numeric,NormalIRanges-method}


\title{IRanges and NormalIRanges objects}

\description{
  The IRanges class is a simple implementation of the \link{Ranges}
  container where 2 integer vectors of the same length are used to
  store the start and width values.
  See the \link{Ranges} virtual class for a formal definition of
  \link{Ranges} objects and for their methods (all of them should
  work for IRanges objects).

  Some subclasses of the IRanges class are: NormalIRanges,
  \link{Views}, etc...

  A NormalIRanges object is just an IRanges object that is guaranteed
  to be "normal". See the Normality section in the man page for
  \link{Ranges} objects for the definition and properties
  of "normal" \link{Ranges} objects.
}

\section{Constructor}{
  See \code{?`\link{IRanges-constructor}`}.
}

\section{Coercion}{
  \describe{
    \item{}{
      \code{as(from, "IRanges")}: Creates an IRanges instance from a Ranges
      object, logical vector, or integer vector. When \code{from} is a logical
      vector, the resulting IRanges object contains the indices for the runs
      of \code{TRUE} values. When \code{from} is an integer vector, the
      elements are either singletons or "increase by 1" sequences.
    }
    \item{}{
      \code{as(from, "NormalIRanges")}: Creates a NormalIRanges instance
      from a logical or integer vector. When \code{from} is an integer vector,
      the elements must be strictly increasing.
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
  \link{Ranges-utils},
  \link{IRanges-constructor},
  \link{IRanges-utils},
  \link{IRanges-setops}.

  Some direct subclasses of the IRanges class (other than NormalIRanges):
  \link{Views-class}.
}

\examples{
  showClass("IRanges")  # shows (some of) the known subclasses

  ## ---------------------------------------------------------------------
  ## A. MANIPULATING IRanges OBJECTS
  ## ---------------------------------------------------------------------
  ## All the methods defined for Ranges objects work on IRanges objects.
  ## See ?Ranges for some examples.
  ## Also see ?`IRanges-utils` and ?`IRanges-setops` for additional
  ## operations on IRanges objects.

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
