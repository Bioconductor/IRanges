\name{multisplit}
\alias{multisplit}

\title{
  Split elements belonging to multiple groups
}

\description{
  This is like \code{\link{split}}, except elements can belong to
  multiple groups, in which case they are repeated to appear in multiple
  elements of the return value.
}

\usage{
multisplit(x, f)
}

\arguments{
  \item{x}{
    The object to split, like a vector.
  }
  \item{f}{
    A list-like object of vectors, the same length as \code{x}, where
    each element indicates the groups to which each element of \code{x}
    belongs.
  }
}

\value{
  A list-like object, with an element for each unique value in the
  unlisted \code{f}, containing the elements in \code{x} where the
  corresponding element in \code{f} contained that value. Just try it.
}

\author{
  Michael Lawrence
}

\examples{
multisplit(1:3, list(letters[1:2], letters[2:3], letters[2:4]))
}

\keyword{ manip }
