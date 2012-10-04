\name{funprog-methods}

\alias{funprog-methods}

\alias{Reduce,List-method}
\alias{Filter,List-method}
\alias{Find,List-method}
\alias{Map,List-method}
\alias{Position,List-method}


\title{Functional programming methods for List objects}

\description{
  The R base package defines some higher-order functions that are commonly
  found in Functional Programming Languages. See \code{?\link[base]{Reduce}}
  for the details, and, in particular, for a description of their arguments.
  The IRanges package provides methods for \link{List} objects, so, in addition
  to be an ordinary vector or list, the \code{x} argument can also be a
  \link{List} object.
}

\usage{
\S4method{Reduce}{List}(f, x, init, right=FALSE, accumulate=FALSE)
\S4method{Filter}{List}(f, x)
\S4method{Find}{List}(f, x, right=FALSE, nomatch=NULL)
\S4method{Map}{List}(f, ...)
\S4method{Position}{List}(f, x, right=FALSE, nomatch=NA_integer_)
}

\arguments{
  \item{f, init, right, accumulate, nomatch}{
    See \code{?base::\link[base]{Reduce}} for a description of
    these arguments.
  }
  \item{x}{
    A \link{List} object.
  }
  \item{...}{
    One or more \link{List} objects. (FIXME: Mixing \link{List} objects with
    ordinary lists doesn't seem to work properly at the moment.)
  }
}

\author{P. Aboyoun}

\seealso{
  \itemize{
    \item The \link{List} class.
    \item The \link{IntegerList} class and constructor for an example of
          a \link{List} subclass.
    \item \code{\link[base]{Reduce}} for a full description of what these
          functions do and what they return.
  }
}

\examples{
x <- IntegerList(a=1:3, b=16:11, c=22:21, d=31:36)
x

Reduce("+", x)

Filter(is.unsorted, x)

pos1 <- Position(is.unsorted, x)
stopifnot(identical(Find(is.unsorted, x), x[[pos1]]))

pos2 <- Position(is.unsorted, x, right=TRUE)
stopifnot(identical(Find(is.unsorted, x, right=TRUE), x[[pos2]]))

y <- x * 1000L
Map("c", x, y)
}

\keyword{methods}
