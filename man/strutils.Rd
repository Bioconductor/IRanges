\name{strutils}

\alias{safeExplode}
\alias{strsplitAsListOfIntegerVectors}
\alias{svn.time}

\title{Low-level string utilities}

\description{
  Some low-level string utilities that operate on ordinary
  character vectors. For more advanced string manipulations,
  see the Biostrings package.
}

\usage{
strsplitAsListOfIntegerVectors(x, sep=",")
}

\arguments{
  \item{x}{
    A character vector where each element is a string containing
    comma-separated decimal integer values.
  }
  \item{sep}{
    The value separator character.
  }
}

\value{
  A list of integer vectors. The list is of the same length as the input.
}

\note{
  \code{strsplitAsListOfIntegerVectors} is similar to the
  \code{strsplitAsListOfIntegerVectors2} function shown in the
  Examples section below, except that the former generally raises
  an error where the latter would have inserted an \code{NA} in
  the returned object. More precisely:
  \itemize{
    \item The latter accepts NAs in the input,
          the former doesn't (raises an error).
    \item The latter introduces NAs by coercion (with a warning),
          the former doesn't (raises an error).
    \item The latter supports "inaccurate integer conversion in coercion"
          when the value to coerce is > INT_MAX (then it's coerced to INT_MAX),
          the former doesn't (raises an error).
    \item The latter coerces non-integer values (e.g. 10.3) to an
          int by truncating them, the former doesn't (raises an error).
  }
  When it fails, \code{strsplitAsListOfIntegerVectors} will print
  an informative error message.
  Finally, \code{strsplitAsListOfIntegerVectors} is faster and uses
  much less memory than \code{strsplitAsListOfIntegerVectors2}.
}

\author{H. Pages}

\seealso{
  \code{\link{strsplit}}
}

\examples{
  x <- c("1116,0,-19",
         " +55291 , 2476,",
         "19184,4269,5659,6470,6721,7469,14601",
         "7778889, 426900, -4833,5659,6470,6721,7096",
         "19184 , -99999")
  y <- strsplitAsListOfIntegerVectors(x)
  y

  ## In normal situations (i.e. when the input is well-formed),
  ## strsplitAsListOfIntegerVectors() does actually the same as the
  ## function below but is more efficient (both in speed and memory
  ## footprint):
  strsplitAsListOfIntegerVectors2 <- function(x, sep=",")
  {
    tmp <- strsplit(x, sep, fixed = TRUE)
    lapply(tmp, as.integer)
  }
  y2 <- strsplitAsListOfIntegerVectors2(x)
  stopifnot(identical(y, y2))
}

\keyword{utilities}
