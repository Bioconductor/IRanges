\name{RangedData-utils}
\alias{RangedData-utils}
\alias{eval,expressionORlanguage,RangedData-method}
\alias{range,RangedData-method}

\title{RangedData utility functions}

\description{
  Utility functions for manipulating \code{\linkS4class{RangedData}} objects.
}

\usage{
\S4method{eval}{expressionORlanguage,RangedData}(expr, envir, enclos =
parent.frame())
\S4method{range}{RangedData}(x, ..., na.rm = FALSE)
}

\arguments{
  \item{expr}{The \code{expression}, \code{call}, or \code{name} to be
    evaluated.}
  \item{envir}{The \code{RangedData} object in which to evaluate
    \code{expr}.}
  \item{enclos}{The \code{environment} in which to look for symbols that
    do not exist in the environment formed from
    \code{RangedData}.}
  \item{x}{A \code{RangedData} object}
  \item{...}{Additional \code{RangedData} objects}
  \item{na.rm}{Ignored}
}

\value{The result of expression evaluation.}

\details{  
  The \code{eval} method converts the \code{RangedData} object
  specified in \code{envir} to an \code{environmnent}, with
  \code{enclos} as its parent, and then evaluates
  \code{expr} within that environment. The \code{RangedData} environment
  contains the following objects:
  \describe{
    \item{\code{ranges}}{The result of \code{unlist(ranges(envir))},
      i.e. all of the ranges in a single \code{\linkS4class{Ranges}} object.}
    \item{\code{colnames(envir)}}{The data columns in \code{envir} are stored
      individually by their column names.}
  }
  The objects are not actually copied into the
  environment. Rather, they are dynamically bound using
  \code{\link{makeActiveBinding}}. This prevents unnecessary copying of
  the data from the external vectors into R vectors. The values are
  cached, so that the data is not copied every time the symbol is
  accessed.

  \code{range} returns a \code{RangesList} resulting from calling
  \code{\link[=range,RangesList-method]{range}(ranges(x))}, i.e. the
  bounds of the ranges in each space.
}

\author{Michael Lawrence}

\seealso{
  \code{\linkS4class{FilterRules}} objects, which can be evaluated on a
  \code{RangedData}, and the base \code{\link{eval}} function.
}

\examples{
  ranges <- IRanges(c(1,2,3),c(4,5,6))
  score <- c(10L, 2L, NA)
  rd <- RangedData(ranges, score)
  eval(quote(score > 3), rd)

  range(rd)
  rd2 <- RangedData(IRanges(c(5,2,0), c(6,3,1)))
  range(rd, rd2)
}

\keyword{utilities}
