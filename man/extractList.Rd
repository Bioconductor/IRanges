\name{extractList}

\alias{relist}
\alias{relist,ANY,PartitioningByEnd-method}
\alias{relist,ANY,List-method}
\alias{relist,Vector,list-method}

\alias{extractList}
\alias{extractList,ANY,ANY-method}
\alias{extractList,ANY-method}

\alias{regroup}

\title{Group elements of a vector-like object into a list-like object}

\description{
  \code{relist} and \code{split} are 2 common ways of grouping the elements
  of a vector-like object into a list-like object. The \pkg{IRanges} and
  \pkg{S4Vectors} packages define \code{relist} and \code{split} methods
  that operate on a \link{Vector} object and return a \link{List} object.

  Because \code{relist} and \code{split} both impose restrictions on
  the kind of grouping that they support (e.g. every element in the input
  object needs to go in a group and can only go in one group), the
  \pkg{IRanges} package introduces the \code{extractList} generic function
  for performing \emph{arbitrary} groupings.
}

\usage{
## relist()
## --------

\S4method{relist}{ANY,List}(flesh, skeleton)

\S4method{relist}{Vector,list}(flesh, skeleton)

## extractList()
## -------------

extractList(x, i)

## regroup()
## ---------

regroup(x, g)
}

\arguments{
  \item{flesh, x}{
    A vector-like object.
  }
  \item{skeleton}{
    A list-like object. Only the "shape" (i.e. element lengths) of
    \code{skeleton} matters. Its exact content is ignored.
  }
  \item{i}{
    A list-like object. Unlike for \code{skeleton}, the content here matters
    (see Details section below).
    Note that \code{i} can be a \link{IntegerRanges} object (a particular
    type of list-like object), and, in that case, \code{extractList} is
    particularly fast (this is a common use case).
  }
  \item{g}{
    A \linkS4class{Grouping} or an object coercible to one. For
    \code{regroup}, \code{g} groups the elements of \code{x}.
  }
}

\details{
  Like \code{split}, \code{relist} and \code{extractList} have in common
  that they return a list-like object where all the list elements have the
  same class as the original vector-like object.

  Methods that return a \link{List} derivative return an object of class
  \code{\link[S4Vectors]{relistToClass}(x)}.

  By default, \code{extractList(x, i)} is equivalent to:
\preformatted{    relist(x[unlist(i)], i)
}
  An exception is made when \code{x} is a data-frame-like object. In that
  case \code{x} is subsetted along the rows, that is, \code{extractList(x, i)}
  is equivalent to:
\preformatted{    relist(x[unlist(i), ], i)
}
  This is more or less how the default method is implemented, except for
  some optimizations when \code{i} is a \link{IntegerRanges} object.

  \code{relist} and \code{split} can be seen as special cases of
  \code{extractList}:
\preformatted{    relist(flesh, skeleton) is equivalent to
    extractList(flesh, PartitioningByEnd(skeleton))

    split(x, f) is equivalent to
    extractList(x, split(seq_along(f), f))
}
  It is good practise to use \code{extractList} only for cases not covered
  by \code{relist} or \code{split}. Whenever possible, using \code{relist}
  or \code{split} is preferred as they will always perform more efficiently.
  In addition their names carry meaning and are familiar to most R
  users/developers so they'll make your code easier to read/understand.

  Note that the transformation performed by \code{relist} or \code{split}
  is always reversible (via \code{unlist} and \code{unsplit}, respectively),
  but not the transformation performed by \code{extractList} (in general).

  The \code{regroup} function splits the elements of \code{unlist(x)}
  into a list according to the grouping \code{g}. Each element of
  \code{unlist(x)} inherits its group from its parent element of
  \code{x}. \code{regroup} is different from \code{relist} and
  \code{split}, because \code{x} is already grouped, and the goal is to
  combine groups.

}

\value{
  The \code{relist} methods behave like \code{utils::relist} except that they
  return a \link{List} object. If \code{skeleton} has names, then they are
  propagated to the returned value.

  \code{extractList} returns a list-like object parallel to \code{i} and with
  the same "shape" as \code{i} (i.e. same element lengths).
  If \code{i} has names, then they are propagated to the returned value.

  All these functions return a list-like object where the list elements have
  the same class as \code{x}. \code{\link[S4Vectors]{relistToClass}} gives
  the exact class of the returned object.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item The \code{\link[S4Vectors]{relistToClass}} function and
          \code{\link[S4Vectors]{split}} methods defined in the
          \pkg{S4Vectors} package.

    \item The \code{\link[base]{unlist}} and \code{\link[utils]{relist}}
          functions in the \pkg{base} and \pkg{utils} packages, respectively.

    \item The \code{\link[base]{split}} and \code{\link[base]{unsplit}}
          functions in the \pkg{base} package.

    \item \link{PartitioningByEnd} objects. These objects are used inside
          \link{CompressedList} derivatives to keep track of the
          \emph{partitioning} of the single vector-like object made of
          all the list elements concatenated together.

    \item \link[S4Vectors]{Vector}, \link[S4Vectors]{List},
          \link[S4Vectors]{Rle}, and \link[S4Vectors]{DataFrame} objects
          implemented in the \pkg{S4Vectors} package.

    \item \link{IntegerRanges} objects.
  }
}

\examples{
## On an Rle object:
x <- Rle(101:105, 6:2)
i <- IRanges(6:10, 16:12, names=letters[1:5])
extractList(x, i)

## On a DataFrame object:
df <- DataFrame(X=x, Y=LETTERS[1:20])
extractList(df, i)
}

\keyword{manip}
