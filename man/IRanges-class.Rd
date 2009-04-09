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

% Coercion:
\alias{coerce,logical,IRanges-method}
\alias{coerce,logical,NormalIRanges-method}


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
  \describe{
    \item{}{
      \code{IRanges(start=NULL, end=NULL, width=NULL, names=NULL)}:
      Return the IRanges object containing the ranges specified by \code{start},
      \code{end} and \code{width}.
      Input falls into one of two categories:
      \describe{
        \item{Category 1}{
          \code{start}, \code{end} and \code{width} are numeric vectors
          (or NULLs). If necessary they are expanded cyclically to the
          length of the longest (NULL arguments are filled with NAs).
          After this expansion, each row in the 3-column matrix obtained
          by binding those 3 vectors together is "solved" i.e. NAs are
          treated as unknown in the equation \code{end = start + width - 1}.
          Finally, the solved matrix is returned as an \link{IRanges}
          instance.
        }
        \item{Category 2}{
          The \code{start} argument is a logical vector or logical Rle object
          and \code{IRanges(start)} produces the same result as
          \code{as(start, "IRanges")}.
          Note that, in that case, the returned IRanges instance is guaranteed
          to be normal.
        }
      }
      Note that the \code{names} argument is never recycled (to remain
      consistent with what \code{`names<-`} does on standard vectors).
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
  \link{IRanges-setops}.

  Some direct subclasses of the IRanges class (other than NormalIRanges):
  \link{Views-class}.
}

\examples{
  showClass("IRanges")  # shows (some of) the known subclasses

  ## ---------------------------------------------------------------------
  ## A. USING THE IRanges() CONSTRUCTOR
  ## ---------------------------------------------------------------------
  IRanges(start=11, end=rep.int(20, 5))
  IRanges(start=11, width=rep.int(20, 5))
  IRanges(-2, 20)  # only one range
  IRanges(start=c(2, 0, NA), end=c(NA, NA, 14), width=11:0)
  IRanges()  # IRanges instance of length zero
  IRanges(names=character())

  ## With logical input:
  x <- IRanges(c(FALSE, TRUE, TRUE, FALSE, TRUE))  # logical vector input
  isNormal(x)  # TRUE
  x <- IRanges(Rle(1:30) \%\% 5 <= 2)  # logical Rle input
  isNormal(x)  # TRUE

  ## ---------------------------------------------------------------------
  ## B. MANIPULATING IRanges OBJECTS
  ## ---------------------------------------------------------------------
  ## All the methods defined for Ranges objects work on IRanges objects.
  ## See ?Ranges for some examples.
  ## Also see ?`IRanges-utils` and ?`IRanges-setops` for additional
  ## operations on IRanges objects.

  ## ---------------------------------------------------------------------
  ## C. A NOTE ABOUT PERFORMANCE
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
