\name{Ranges-comparison}

\alias{Ranges-comparison}

\alias{==}
\alias{==,Ranges,Ranges-method}
\alias{!=}
\alias{!=,Ranges,Ranges-method}
\alias{duplicated}
\alias{duplicated,Ranges-method}
\alias{unique}
\alias{unique,Ranges-method}

\alias{<=}
\alias{<=,Ranges,Ranges-method}
\alias{>=}
\alias{>=,Ranges,Ranges-method}
\alias{<}
\alias{<,Ranges,Ranges-method}
\alias{>}
\alias{>,Ranges,Ranges-method}
\alias{order}
\alias{order,Ranges-method}
\alias{sort}
\alias{sort,Ranges-method}
\alias{rank}
\alias{rank,Ranges-method}


\title{Ranges comparison}

\description{
  Equality and ordering of ranges, and related methods.
}

\usage{
  ## ==== Equality and related methods ====
  ## --------------------------------------

  x == y
  x != y

  \S4method{duplicated}{Ranges}(x, incomparables=FALSE, fromLast=FALSE, ...)

  \S4method{unique}{Ranges}(x, incomparables=FALSE, fromLast=FALSE, ...)

  ## ==== Ordering and related methods ====
  ## --------------------------------------

  x <= y
  x >= y
  x < y
  x > y

  \S4method{order}{Ranges}(..., na.last=TRUE, decreasing=FALSE)

  \S4method{sort}{Ranges}(x, decreasing=FALSE, ...)

  \S4method{rank}{Ranges}(x, na.last=TRUE, ties.method=c("average", "first", "random", "max", "min"))
}

\arguments{
  \item{x,y}{
    A \link{Ranges} object.
  }
  \item{incomparables}{
    Must be \code{FALSE}.
  }
  \item{fromLast}{
    \code{TRUE} or \code{FALSE}.
  }
  \item{...}{
    \link{Ranges} objects for \code{order}.
  }
  \item{na.last}{
    Ignored.
  }
  \item{decreasing}{
    \code{TRUE} or \code{FALSE}.
  }
  \item{ties.method}{
    A character string specifying how ties are treated. Only \code{"first"}
    is supported for now.
  }
}

\details{
  Two ranges are considered equal iff they share the same start and width.
  Note that with this definition, 2 empty ranges are generally
  not equal (they need to share the same start to be considered equal).

  Ranges are ordered by starting position first, and then by width.
  This way, the space of ranges is totally ordered.
  The \code{order}, \code{sort} and \code{rank} methods for \link{Ranges}
  objects are consistent with this order.

  \describe{
    \item{}{
      \code{duplicated(x)}:
      Determines which elements of \code{x} are equal to elements
      with smaller subscripts, and returns a logical vector indicating
      which elements are duplicates.
      It is semantically equivalent to \code{duplicated(as.data.frame(x))}.
      See \code{\link[base]{duplicated}} in the base package for more details.
    }
    \item{}{
      \code{unique(x)}:
      Removes duplicate ranges from \code{x}.
      See \code{\link[base]{unique}} in the base package for more details.
    }
    \item{}{
      \code{order(...)}:
      Returns a permutation which rearranges its first argument (a \link{Ranges}
      object) into ascending order, breaking ties by further arguments (also
      \link{Ranges} objects).
      See \code{\link[base]{order}} in the base package for more details.
    }
    \item{}{
      \code{sort(x)}:
      Sorts \code{x}.
      See \code{\link[base]{sort}} in the base package for more details.
    }
    \item{}{
      \code{rank(x, na.last=TRUE, ties.method=c("average", "first", "random", "max", "min"))}:
      Returns the sample ranks of the ranges in \code{x}.
      See \code{\link[base]{rank}} in the base package for more details.
    }
  }
}

\seealso{
  \link{Ranges-class},
  \link{IRanges-class},
  \code{\link[base]{duplicated}},
  \code{\link[base]{unique}},
  \code{\link[base]{order}},
  \code{\link[base]{sort}},
  \code{\link[base]{rank}}
}

\examples{
  x <- IRanges(start=c(20L, 8L, 20L, 22L, 25L, 20L, 22L, 22L),
               width=c( 4L, 0L, 11L,  5L,  0L,  9L,  5L,  0L))
  x
  which(width(x) == 0)  # 3 empty ranges
  x[2] == x[5]  # FALSE
  x == x[4]
  duplicated(x)
  unique(x)
  x >= x[3]
  order(x)
  sort(x)
  rank(x, ties.method="first")
}

\keyword{methods}
