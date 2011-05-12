\name{compact}

\alias{xvcopy}
\alias{xvcopy,SharedVector-method}
\alias{xvcopy,XVector-method}
\alias{xvcopy,SharedVector_Pool-method}
\alias{xvcopy,XRawList-method}

\alias{compact}
\alias{compact,ANY-method}
\alias{compact,XVector-method}
\alias{compact,XVectorList-method}

\title{Object compaction}

\description{
  Compacting an object is modifying its internal representation in order
  to reduce its size in memory.
}

\usage{
compact(x, check=TRUE, ...)

## Internal compact() support function. Not intended to be called
## directly:
xvcopy(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
}

\arguments{
  \item{x}{
    For \code{compact}, the object to be compacted. \code{compact} accepts
    any R object. However, on most of them, \code{compact} won't do anything
    and will just return an object identical to \code{x}.
    See the Details section below.

    For \code{xvcopy}, a \link{SharedVector}, \link{XVector},
    \link{SharedVector_Pool}, or \link{XRawList} vector.
  }
  \item{check}{
    After compacting the individual slots of an S4 object, this argument
    is passed to \code{`slot<-`} when replacing the original value of a
    slot with the compacted value.
  }
  \item{...}{
    Arguments to be passed to or from other methods.
  }
  \item{start, end, width, lkup, reverse}{
    For internal use.
  }
}

\details{
  The internal reorganization of the object should be transparent to the
  user i.e. \code{compact(x)} should "look" the same as \code{x}, or,
  more precisely, \code{x} and \code{compact(x)} should be interchangeable
  anywhere in the user's code. However, because they have different internal
  representations, we generally don't expect \code{identical(x, compact(x))}
  to be TRUE, even though most of the times they will, because there are
  only very few types of objects that \code{compact} actually knows how to
  reorganize internally.

  \code{compact} is a generic function.

  Here is how the default method works. By default \code{compact(x)} is
  obtained by compacting all the "components" in \code{x}. Only 2 kinds
  of objects are considered to have "components": lists (the components
  are the list elements), and S4 objects (the components are the slots).
  The other objects are not considered to have components, so, by default,
  \code{compact} does nothing on them. In particular, it does nothing on
  environments. Also the attributes of an object (other than the slots of
  an S4 object) are not considered to be "components" and therefore are
  not compacted.

  Note that, in the absence of specialized \code{compact} methods that
  actually know how to reorganize an object internally, the default method
  would visit the tree of all the components, sub-components,
  sub-sub-components etc of object \code{x} without actually modifying
  anything in \code{x}. So of course, specialized \code{compact} methods
  need to be defined for the objects that can *effectively* be compacted.
  Otherwise the \code{compact} function would be equivalent to the
  \code{identity} function!

  At the moment, 2 specialized \code{compact} methods are defined (in
  addition to the default method): one for \link{XVector} objects, and
  one for \link{XVectorList} objects.
}

\value{
  An object equivalent to \code{x} but eventually smaller in memory.
}

\author{H. Pages}

\seealso{
  \link{XVector-class},
  \link{XVectorList-class},
  \code{\link{subseq}},
  \code{\link[utils]{object.size}},
  \code{\link[base]{save}}
}

\examples{
## We illustrate the use of compact() on an XInteger vector (XInteger
## is one of the 3 concrete subclasses of the XVector virtual class):
x <- XInteger(500000, sample(500000))

## subseq() does NOT copy the data stored in an XVector object:
y <- subseq(x, start=41, end=60)
x@shared
y@shared  # same address
object.size(x)
object.size(y)  # same size

## compact() copies the data, but only the data actually "used" by 'y':
y0 <- compact(y)
y0@shared  # new address
object.size(y0)  # much smaller now!

## Compaction is particularly relevant when saving an object with
## external references like 'y':
yfile <- file.path(tempdir(), "y.rda")
save(y, file=yfile)
file.info(yfile)$size

y0file <- file.path(tempdir(), "y0.rda")
save(y0, file=y0file)
file.info(y0file)$size
}

\keyword{methods}
