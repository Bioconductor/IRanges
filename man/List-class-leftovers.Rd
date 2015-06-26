\name{List-class-leftovers}
\docType{class}

\alias{stack,List-method}

\title{List objects (old man page)}

\description{
  IMPORTANT NOTE - 9/4/2014: This man page is being refactored. Most of
  the things that used to be documented here have been moved to the man
  page for \link[S4Vectors]{List} objects located in the \pkg{S4Vectors}
  package.
}

\details{
  The only thing left here is the documentation of the \code{stack}
  method for \link{List} objects. In the code snippets below, \code{x}
  is a List object.

  \describe{
    \item{}{
      \code{stack(x, index.var = "name", value.var = "value")}:
      As with \code{\link[utils:stack]{stack}} on a \code{list},
      constructs a \code{DataFrame} with two columns: one for the
      unlisted values, the other indicating the name of the element from
      which each value was obtained. \code{index.var} specifies the column
      name for the index (source name) column and \code{value.var}
      specifies the column name for the values.
    }
  }
}

\seealso{
  \itemize{
    \item The \link[S4Vectors]{List} class defined and documented in the
          \pkg{S4Vectors} package.
  }
}

\examples{
starts <- IntegerList(c(1, 5), c(2, 8))
ends <- IntegerList(c(3, 8), c(5, 9))
rgl <- IRangesList(start=starts, end=ends)
rangeDataFrame <- stack(rgl, "space", "ranges")
}

\keyword{methods}
\keyword{classes}
