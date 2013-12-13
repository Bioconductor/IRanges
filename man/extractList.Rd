\name{extractList}

\alias{splitAsListReturnedClass}
\alias{splitAsListReturnedClass,ANY-method}

\alias{relist,ANY,PartitioningByEnd-method}
\alias{relist,ANY,List-method}
\alias{relist,Vector,list-method}

\alias{splitAsList}
\alias{split,Vector,ANY-method}
\alias{split,ANY,Vector-method}
\alias{split,Vector,Vector-method}
\alias{split,list,Vector-method}

\alias{extractList}
\alias{extractList,ANY,ANY-method}

\title{Group elements of a vector-like object into a list-like object}

\description{
  \code{relist} and \code{split} are 2 common ways of grouping the elements
  of a vector-like object into a list-like object. The \pkg{IRanges} package
  defines \code{relist} and \code{split} methods that operate on a
  \link{Vector} object and return a \link{List} object.

  Because \code{relist} and \code{split} both impose severe restrictions on
  the kind of grouping that they support (e.g. every element in the input
  object needs to go in a group and can only go in one group), the
  \pkg{IRanges} package introduces the \code{extractList} generic function
  for performing \emph{arbitrary} groupings.

  \code{relist}, \code{split}, and \code{extractList} have in common that
  they return a list-like value where each list element has the same class
  as the original vector-like object. Thus they need to be able to select
  the appropriate \link{List} concrete subclass to use for this returned
  value. This selection is performed by \code{splitAsListReturnedClass} and
  is based only on the class of the original object.
}

\usage{
## relist()
## --------

\S4method{relist}{ANY,List}(flesh, skeleton)
\S4method{relist}{Vector,list}(flesh, skeleton)

## splitAsList() and split()
## -------------------------

splitAsList(x, f, drop=FALSE)

\S4method{split}{Vector,ANY}(x, f, drop=FALSE)

## extractList()
## -------------

extractList(x, i)

## splitAsListReturnedClass()
## --------------------------

splitAsListReturnedClass(x)
}

\arguments{
  \item{flesh, x}{
    A vector-like object.
  }
  \item{skeleton}{
    A list-like object. Only the "shape" (i.e. element lengths) of
    \code{skeleton} matters. Its exact content is ignored.
  }
  \item{f}{
    An atomic vector or a factor (possibly in \link{Rle} form).
  }
  \item{drop}{
    Logical indicating if levels that do not occur should be dropped (if
    \code{f} is a factor).
  }
  \item{i}{
    A list-like object. Unlike for \code{skeleton}, the content here matters
    (see Details section below).
    Note that \code{i} can be a \link{Ranges} object (a particular type of
    list-like object), and, in that case, \code{extractList} is particularly
    fast (this is a common use case).
  }
}

\details{
\code{extractList(x, i)} is equivalent to \code{relist(x[unlist(i)], i)}.
This is more or less how the default method is implemented except for
some optimizations when \code{i} is a \link{Ranges} object.

\code{relist} and \code{split} can be seen as specialized versions of
\code{extractList}:
\preformatted{
    relist(x, skeleton) is equivalent to
    extractList(x, PartitioningByEnd(skeleton))

    split(x, f) is equivalent to
    extractList(x, split(seq_along(f), f))
}
However, whenever possible, \code{relist} or \code{split} should be used
instead of \code{extractList} as they will always perform more efficiently.
Also their names carry meaning and are familiar to most R users/developpers
so they'll make your code easier to read.

\code{extractList} should only be used for cases not covered by
\code{relist} or \code{split}.

Note that, unlike \code{relist} and \code{split} which are reversible
(with \code{unlist} and \code{unsplit}, respectively), \code{extractList}
is not (in general).
}

\value{
  The \code{relist} method behaves like \code{utils::relist} except that it
  returns a \link{List} object. If \code{skeleton} has names, then they are
  propagated to the returned value.

  \code{splitAsList} and the \code{split} method behave like
  \code{base::split} except that they return a \link{List} object.
  The difference between \code{splitAsList} and \code{split} is that the
  former will always return a \link{List} object while the latter can return
  an ordinary list (e.g. when \code{x} and \code{f} are ordinary vectors
  and/or factors).

  \code{extractList} returns a list-like object parallel to \code{i} and with
  the same "shape" as \code{i} (i.e. same element lengths).
  If \code{i} has names, then they are propagated to the returned value.

  All these functions (except \code{splitAsListReturnedClass}) return a
  list-like object where the list elements have the same class as \code{x}.
  \code{splitAsListReturnedClass} gives the exact class of the returned
  object.
}

\author{
  H. Pages
}

\seealso{
  \itemize{
    \item The \code{\link[base]{unlist}} and \code{\link[utils]{relist}}
          functions in the \pkg{base} and \pkg{utils} packages, respectively.

    \item The \code{\link[base]{split}} and \code{\link[base]{unsplit}}
          functions in the \pkg{base} package.

    \item \link{Vector} and \link{List} objects.
  }
}

\examples{
extractList(Rle(101:105, 6:2), IRanges(6, 16:12))
}

\keyword{manip}
