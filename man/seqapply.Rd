\name{seqapply}

\alias{unsplit,List-method}
\alias{split<-,Vector-method}

\title{
2 methods that should be documented somewhere else
}
\description{
  \code{unsplit} method for \link{List} object and \code{split<-}
  method for \link{Vector} object.
}

\usage{
\S4method{unsplit}{List}(value, f, drop = FALSE)
\S4method{split}{Vector}(x, f, drop = FALSE, ...) <- value
}

\arguments{
  \item{value}{
    The \link{List} object to unsplit.
  }
  \item{f}{
    A \code{factor} or \code{list} of factors
  }
  \item{drop}{
    Whether to drop empty elements from the returned list
  }
  \item{x}{
    Like \code{X}
  }
  \item{\dots}{
    Extra arguments to pass to \code{FUN}
  }
}

\details{
  \code{unsplit} unlists \code{value}, where the order of the returned
  vector is as if \code{value} were originally created by splitting that
  vector on the factor \code{f}.

  \code{split(x, f, drop = FALSE) <- value}: Virtually splits \code{x} by
  the factor \code{f}, replaces the elements of the resulting list with the
  elements from the list \code{value}, and restores \code{x} to its original
  form. Note that this works for any \code{Vector}, even though \code{split}
  itself is not universally supported.
}

\author{
  Michael Lawrence
}

\keyword{manip}
