\name{RangedSelection-class}
\docType{class}
\alias{RangedSelection-class}

% accessors
\alias{ranges,RangedSelection-method}
\alias{colnames,RangedSelection-method}
\alias{ranges<-,RangedSelection-method}
\alias{colnames<-,RangedSelection-method}

% coercion
\alias{coerce,RangesList,RangedSelection-method}

% constructor
\alias{RangedSelection}

\title{Selection of ranges and columns}

\description{A \code{RangedSelection} represents a query against a table
  of interval data in terms of ranges and column names. The ranges
  select any table row with an overlapping interval. Note that the
  intervals are always returned, even if no columns are selected.}

\details{
  Traditionally, tabular data structures have supported the
  \code{\link{subset}} function, which allows one to select a subset of
  the rows and columns from the table. In that case, the rows and
  columns are specified by two separate arguments. As querying interval
  data sources, especially those external to R, such as binary indexed
  files and databases, is increasingly common, there is a need to
  encapsulate the row and column specifications into a single data
  structure, mostly for the sake of interface
  cleanliness. The \code{RangedSelection} class fills that role.
}

\section{Constructor}{
  \describe{
    \item{}{\code{RangedSelection(ranges = RangesList(),
        colnames = character())}: Constructors a \code{RangedSelection}
        with the given \code{ranges} and \code{colnames}.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as(from, "RangedSelection")}: Coerces \code{from} to a
      \code{RangedSelection} object. Typically, \code{from} is a
      \code{\linkS4class{RangesList}}, the ranges of which become the
      ranges in the new \code{RangedSelection}.
    }
  }
}

\section{Accessors}{
  In the code snippets below, \code{x} is always a \code{RangedSelection}.
  \describe{
    \item{}{\code{ranges(x), ranges(x) <- value}: Gets or sets the
      ranges, a \code{\linkS4class{RangesList}}, that select rows with
      overlapping intervals.
    }
    \item{}{\code{colnames(x), colnames(x) <- value}: Gets the 
      names, a \code{character} vector, indicating the columns.
    }
  }
}

\author{ Michael Lawrence }

\examples{
  rl <- RangesList(chr1 = IRanges(c(1, 5), c(3, 6)))

  RangedSelection(rl)
  as(rl, "RangedSelection") # same as above

  RangedSelection(rl, "score")
}

\keyword{methods}
\keyword{classes}
