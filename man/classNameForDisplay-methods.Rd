\name{classNameForDisplay}

\alias{classNameForDisplay}
\alias{classNameForDisplay,ANY-method}
\alias{classNameForDisplay,CompressedList-method}
\alias{classNameForDisplay,CompressedNormalIRangesList-method}
\alias{classNameForDisplay,SimpleList-method}
\alias{classNameForDisplay,SimpleNormalIRangesList-method}

\title{Provide a class name for displaying to users}

\description{
  Generic function to create a class name suitable for display to users.
  Current methods remove "Compressed" or "Simple" from the formal names
  of classes defined in IRanges.
}

\usage{
classNameForDisplay(x)
}

\arguments{
  \item{x}{
    An instance of any class.
  }
}

\value{
  A character vector of length 1, as returned by \code{class}.
}

\author{Martin Morgan}

\examples{
  classNameForDisplay(IntegerList())
  class(IntegerList())
}

\keyword{utilities}
