\name{Vector-comparison}

\alias{Vector-comparison}

\alias{compare}

\alias{==,Vector,Vector-method}
\alias{==,Vector,ANY-method}
\alias{==,ANY,Vector-method}

\alias{<=,Vector,Vector-method}
\alias{<=,Vector,ANY-method}
\alias{<=,ANY,Vector-method}

\alias{!=,Vector,Vector-method}
\alias{!=,Vector,ANY-method}
\alias{!=,ANY,Vector-method}

\alias{>=,Vector,Vector-method}
\alias{>=,Vector,ANY-method}
\alias{>=,ANY,Vector-method}

\alias{<,Vector,Vector-method}
\alias{<,Vector,ANY-method}
\alias{<,ANY,Vector-method}

\alias{>,Vector,Vector-method}
\alias{>,Vector,ANY-method}
\alias{>,ANY,Vector-method}

\alias{selfmatch}
\alias{selfmatch,ANY-method}

\alias{duplicated,Vector-method}
\alias{unique,Vector-method}

\alias{\%in\%,Vector,Vector-method}
\alias{\%in\%,Vector,ANY-method}
\alias{\%in\%,ANY,Vector-method}

\alias{findMatches}
\alias{findMatches,ANY,ANY-method}
\alias{countMatches}
\alias{countMatches,ANY,ANY-method}

\alias{sort,Vector-method}

\alias{table,Vector-method}


\title{Compare, order, tabulate vector-like objects}

\description{
  Generic functions and methods for comparing, ordering, and tabulating
  vector-like objects.
}

\usage{
## Element-wise (aka "parallel") comparison of 2 Vector objects
## ------------------------------------------------------------

compare(x, y)

\S4method{==}{Vector,Vector}(e1, e2)
\S4method{==}{Vector,ANY}(e1, e2)
\S4method{==}{ANY,Vector}(e1, e2)

\S4method{<=}{Vector,Vector}(e1, e2)
\S4method{<=}{Vector,ANY}(e1, e2)
\S4method{<=}{ANY,Vector}(e1, e2)

\S4method{!=}{Vector,Vector}(e1, e2)
\S4method{!=}{Vector,ANY}(e1, e2)
\S4method{!=}{ANY,Vector}(e1, e2)

\S4method{>=}{Vector,Vector}(e1, e2)
\S4method{>=}{Vector,ANY}(e1, e2)
\S4method{>=}{ANY,Vector}(e1, e2)

\S4method{<}{Vector,Vector}(e1, e2)
\S4method{<}{Vector,ANY}(e1, e2)
\S4method{<}{ANY,Vector}(e1, e2)

\S4method{>}{Vector,Vector}(e1, e2)
\S4method{>}{Vector,ANY}(e1, e2)
\S4method{>}{ANY,Vector}(e1, e2)

## selfmatch()
## -----------

selfmatch(x, ...)

## duplicated() & unique()
## -----------------------

\S4method{duplicated}{Vector}(x, incomparables=FALSE, ...)

\S4method{unique}{Vector}(x, incomparables=FALSE, ...)

## %in%
## ----

\S4method{\%in\%}{Vector,Vector}(x, table)
\S4method{\%in\%}{Vector,ANY}(x, table)
\S4method{\%in\%}{ANY,Vector}(x, table)

## findMatches() & countMatches()
## ------------------------------

findMatches(x, table, select=c("all", "first", "last"), ...)
countMatches(x, table, ...)

## sort()
## ------

\S4method{sort}{Vector}(x, decreasing=FALSE, ...)

## table()
## -------

\S4method{table}{Vector}(...)
}

\arguments{
  \item{x, y, e1, e2, table}{
    Vector-like objects.
  }
  \item{incomparables}{
    The \code{duplicated} method for \link{Vector} objects does NOT support
    this argument.

    The \code{unique} method for \link{Vector} objects, which is implemented
    on top of \code{duplicated}, propagates this argument to its call to
    \code{duplicated}.

    See \code{?\link[base]{base::duplicated}} and
    \code{?\link[base]{base::unique}} for more information about this
    argument.
  }
  \item{select}{
    Only \code{select="all"} is supported at the moment.
    Note that you can use \code{match} if you want to do \code{select="first"}.
    Otherwise you're welcome to request this on the Bioconductor mailing list.
  }
  \item{decreasing}{
    See \code{?\link[base]{base::sort}}.
  }
  \item{...}{
    A \link{Vector} object for \code{table} (the \code{table} method
    for \link{Vector} objects currently only supports one argument).

    Otherwise, extra arguments supported by specific methods.
    In particular:
    \itemize{
      \item The default \code{selfmatch} method, which is implemented on top
            of \code{match}, propagates the extra arguments to its call to
            \code{match}.

      \item The \code{duplicated} method for \link{Vector} objects, which is
            implemented on top of \code{selfmatch}, accepts extra argument
            \code{fromLast} and propagates the other extra arguments to its
            call to \code{selfmatch}.
            See \code{?\link[base]{base::duplicated}} for more information
            about this argument.

      \item The \code{unique} method for \link{Vector} objects, which is
            implemented on top of \code{duplicated}, propagates the extra
            arguments to its call to \code{duplicated}.

      \item The default \code{findMatches} and \code{countMatches} methods,
            which are implemented on top of \code{match} and \code{selfmatch},
            propagate the extra arguments to their calls to \code{match} and
            \code{selfmatch}.

      \item The \code{sort} method for \link{Vector} objects, which is
            implemented on top of \code{order}, only accepts extra argument
            \code{na.last} and propagates it to its call to \code{order}.
    }
  }
}

\details{
  Doing \code{compare(x, y)} on 2 vector-like objects \code{x} and \code{y}
  of length 1 must return an integer less than, equal to, or greater than zero
  if the single element in \code{x} is considered to be respectively less than,
  equal to, or greater than the single element in \code{y}.
  If \code{x} or \code{y} have a length != 1, then they are typically expected
  to have the same length so \code{compare(x, y)} can operate element-wise,
  that is, in that case it returns an integer vector of the same length
  as \code{x} and \code{y} where the i-th element is the result of compairing
  \code{x[i]} and \code{y[i]}. If \code{x} and \code{y} don't have the same
  length and are not zero-length vectors, then the shortest is first
  recycled to the length of the longest. If one of them is a zero-length
  vector then \code{compare(x, y)} returns a zero-length integer vector.

  \code{selfmatch(x, ...)} is equivalent to \code{match(x, x, ...)}. This
  is actually how the default method is implemented. However note that
  \code{selfmatch(x, ...)} will typically be more efficient than
  \code{match(x, x, ...)} on vector-like objects for which a specific
  \code{selfmatch} method is implemented.

  \code{findMatches} is an enhanced version of \code{match} which, by default
  (i.e. if \code{select="all"}), returns all the matches in a \link{Hits}
  object.

  \code{countMatches} returns an integer vector of the length of \code{x}
  containing the number of matches in \code{table} for each element
  in \code{x}.
}

\value{
  For \code{compare}: see Details section above.

  For \code{selfmatch}: an integer vector of the same length as \code{x}.

  For \code{duplicated}, \code{unique}, and \code{\%in\%}: see
  \code{?\link[BiocGenerics]{BiocGenerics::duplicated}},
  \code{?\link[BiocGenerics]{BiocGenerics::unique}},
  and \code{?\link{`\%in\%`}}.

  For \code{findMatches}: a \link{Hits} object by default (i.e. if
  \code{select="all"}).

  For \code{countMatches}: an integer vector of the length of \code{x}
  containing the number of matches in \code{table} for each element
  in \code{x}.

  For \code{sort}: see \code{?\link[BiocGenerics]{BiocGenerics::sort}}.

  For \code{table}: a 1D array of integer values promoted to the
  \code{"table"} class. See \code{?BiocGeneric::table} for more information.
}

\note{
  The 6 traditional binary comparison operators are: \code{==}, \code{!=},
  \code{<=}, \code{>=}, \code{<}, and \code{>}. The \pkg{IRanges} package
  defines methods for each of these operators and for \link{Vector} objects
  as follow:
  \preformatted{
setMethod("==", c("Vector", "Vector"),
    function(e1, e2) { compare(e1, e2) == 0L }
)
setMethod("<=", c("Vector", "Vector"),
    function(e1, e2) { compare(e1, e2) <= 0L }
)
setMethod("!=", c("Vector", "Vector"),
    function(e1, e2) { !(e1 == e2) }
)
setMethod(">=", c("Vector", "Vector"),
    function(e1, e2) { e2 <= e1 }
)
setMethod("<", c("Vector", "Vector"),
    function(e1, e2) { !(e2 <= e1) }
)
setMethod(">", c("Vector", "Vector"),
    function(e1, e2) { !(e1 <= e2) }
)
  }
  With these definitions, the 6 binary operators work out-of-the-box
  on \link{Vector} objects for which \code{compare} works the expected way.
  If \code{compare} is not implemented, then it's enough to implement
  \code{==} and \code{<=} methods to have the 4 remaining operators
  (\code{!=}, \code{>=}, \code{<}, and \code{>}) work out-of-the-box.

  Note that no \code{compare} method is actually implemented for the
  \link{Vector} class. Specific \code{compare} methods need to be implemented
  for specific subclasses (e.g. for \link{Ranges} objects). These specific
  methods must obey the rules described in the Details section above.

  The \code{duplicated}, \code{unique}, and \code{\%in\%} methods for
  \link{Vector} objects are implemented on top of \code{selfmatch},
  \code{duplicated}, and \code{match}, respectively, so they work
  out-of-the-box on \link{Vector} objects for which \code{selfmatch},
  \code{duplicated}, and \code{match} work the expected way.
  Also the default \code{findMatches} and \code{countMatches} methods are
  implemented on top of \code{match} and \code{selfmatch} so they work 
  out-of-the-box on \link{Vector} objects for which \code{match} and
  \code{selfmatch} work the expected way.

  However, since \code{selfmatch} itself is also implemented on top of
  \code{match}, then having \code{match} work the expected way is enough
  to get all the others work out-of-the-box on \link{Vector} objects.

  No \code{match}, or \code{order} method is actually implemented for the
  \link{Vector} class. Specific methods need to be implemented for specific
  subclasses (e.g. for \link{Ranges} objects).

  The \code{sort} method for \link{Vector} objects is implemented on top
  of \code{order}, so it works out-of-the-box on \link{Vector} objects
  for which \code{order} works the expected way.

  The \code{table} method for \link{Vector} objects is implemented on top
  of \code{selfmatch}, \code{order}, and \code{as.character}, so it works 
  out-of-the-box on a \link{Vector} object for which \code{selfmatch},
  \code{order}, and \code{as.character} work the expected way.
}

\author{H. Pages}

\seealso{
  \itemize{
    \item The \link{Vector} class.

    \item \link{Ranges-comparison} for comparing and ordering ranges.

    \item \code{\link{==}},
          \code{\link[BiocGenerics]{duplicated}},
          \code{\link[BiocGenerics]{unique}},
          \code{\link[BiocGenerics]{match}},
          \code{\link{\%in\%}},
          \code{\link[BiocGenerics]{order}},
          \code{\link[BiocGenerics]{sort}},
          \code{\link[BiocGenerics]{rank}} for general information about
          the comparison/ordering operators and functions.

    \item The \link{Hits} class.
  }
}

\examples{
## ---------------------------------------------------------------------
## A. SIMPLE EXAMPLES
## ---------------------------------------------------------------------

y <- c(16L, -3L, -2L, 15L, 15L, 0L, 8L, 15L, -2L)
selfmatch(y)

x <- c(unique(y), 999L)
findMatches(x, y)
countMatches(x, y)

## See ?`Ranges-comparison` for more examples (using Ranges objects).

## ---------------------------------------------------------------------
## B. FOR DEVELOPPERS: HOW TO IMPLEMENT THE BINARY COMPARISON OPERATORS
##    FOR YOUR Vector SUBCLASS
## ---------------------------------------------------------------------

## The answer is: don't implement them. Just implement compare() and the
## binary comparison operators will work out-of-the-box. Here is an
## example:

## (1) Implement a simple Vector subclass.

setClass("Raw", contains="Vector", representation(data="raw"))

setMethod("length", "Raw", function(x) length(x@data))

setMethod("[", "Raw",
    function(x, i, j, ..., drop) { x@data <- x@data[i]; x }
)

x <- new("Raw", data=charToRaw("AB.x0a-BAA+C"))
stopifnot(identical(length(x), 12L))
stopifnot(identical(x[7:3], new("Raw", data=charToRaw("-a0x."))))

## (2) Implement a "compare" method for Raw objects.

setMethod("compare", c("Raw", "Raw"),
    function(x, y) {as.integer(x@data) - as.integer(y@data)}
)

stopifnot(identical(which(x == x[1]), c(1L, 9L, 10L)))
stopifnot(identical(x[x < x[5]], new("Raw", data=charToRaw(".-+"))))
}

\keyword{methods}
