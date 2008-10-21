\name{Ranges-class}
\docType{class}

% Classes:
\alias{class:Ranges}
\alias{Ranges-class}
\alias{Ranges}

% Generics and methods:
\alias{width}
\alias{start,Ranges-method}
\alias{width,Ranges-method}
\alias{end,Ranges-method}
\alias{start<-}
\alias{width<-}
\alias{end<-}
\alias{isEmpty}
\alias{isEmpty,Ranges-method}
\alias{as.matrix,Ranges-method}
\alias{as.data.frame,Ranges-method}
\alias{duplicated,Ranges-method}
\alias{show,Ranges-method}
\alias{[<-,Ranges,ANY,ANY,ANY-method}
\alias{rep,Ranges-method}

% Old stuff:
\alias{first}
\alias{first,Ranges-method}
\alias{last}
\alias{last,Ranges-method}


\title{Ranges objects}

\description{
  The Ranges virtual class is a general container for storing
  a set of integer ranges.
}

\details{
  A Ranges object is a vector-like object where each element describes
  a "range of integer values".

  A "range of integer values" is a finite set of consecutive integer
  values. Each range can be fully described with exactly 2 integer
  values which can be arbitrarily picked up among the 3 following values:
  its "start" i.e. its smallest (or first, or leftmost) value;
  its "end" i.e. its greatest (or last, or rightmost) value;
  and its "width" i.e. the number of integer values in the range.
  For example the set of integer values that are greater than or equal
  to -20 and less than or equal to 400 is the range that starts at -20
  and has a width of 421.
  In other words, a range is a closed, one-dimensional interval
  with integer end points and on the domain of integers.

  The starting point (or "start") of a range can be any integer (see
  \code{start} below) but its "width" must be a non-negative integer
  (see \code{width} below). The ending point (or "end") of a range is
  equal to its "start" plus its "width" minus one (see \code{end} below).
  An "empty" range is a range that contains no value i.e. a range that
  has a null width.
  Note that for an empty range, the end is smaller than the start.

  Two ranges are considered equal iff they share the same start and width.
  Note that with this definition, 2 empty ranges are generally
  not equal (they need to share the same start to be considered equal).

  The length of a Ranges object is the number of ranges in it, not the
  number of integer values in its ranges.

  A Ranges object is considered empty iff all its ranges are empty.

  Ranges objects have a vector-like semantic i.e. they only support single
  subscript subsetting (unlike, for example, standard R data frames which
  can be subsetted by row and by column).

  The Ranges class itself is a virtual class. The following classes derive
  directly from the Ranges class: \link{IRanges} and \link{XRanges}.
}

\section{Methods}{
  In the code snippets below, \code{x}, \code{y} and \code{object} are
  Ranges objects. Not all the functions described below will necessarily
  work with all kinds of Ranges objects but they should work at least
  for \link{IRanges} objects.
  Also more operations on Ranges objects are described in the man page
  for \link{IRanges-utils} (\code{\link{shift}}, \code{\link{restrict}},
  \code{\link{narrow}}, \code{\link{reduce}}, \code{\link{gaps}})
  and for \link{IntervalTree-class} (\code{\link{overlap}}).

  \describe{
    \item{}{
      \code{length(x)}:
      The number of ranges in \code{x}.
    }
    \item{}{
      \code{start(x)}:
      The start values of the ranges.
      This is an integer vector of the same length as \code{x}.
    }
    \item{}{
      \code{width(x)}:
      The number of integer values in each range.
      This is a vector of non-negative integers of the same length as \code{x}.
    }
    \item{}{
      \code{end(x)}:
      \code{start(x) + width(x) - 1L}
    }
    \item{}{
      \code{names(x)}:
      \code{NULL} or a character vector of the same length as \code{x}.
    }
    \item{}{
      \code{update(object, ...)}:
      Convenience method for combining multiple modifications of
      \code{object} in one single call. For example
      \code{object <- update(object, start=start(object)-2L,
                                     end=end(object)+2L)}
      is equivalent to
      \code{start(object) <- start(object)-2L; end(object) <- end(object)+2L}.
    }
    \item{}{
      \code{isEmpty(x)}:
      Return a logical value indicating whether \code{x} is empty or not.
    }
    \item{}{
      \code{as.matrix(x, ...)}:
      Convert \code{x} into a 2-column integer matrix
      containing \code{start(x)} and \code{width(x)}.
      Extra arguments (\code{...}) are ignored.
    }
    \item{}{
      \code{as.data.frame(x, row.names=NULL, optional=FALSE, ...)}:
      Convert \code{x} into a standard R data frame object.
      \code{row.names} must be \code{NULL} or a character vector giving
      the row names for the data frame, and \code{optional} and any
      additional argument (\code{...}) is ignored.
      See \code{?\link{as.data.frame}} for more information about these
      arguments.
    }
    \item{}{
      \code{duplicated(x)}:
      Determine which elements of \code{x} are equal to elements
      with smaller subscripts, and returns a logical vector indicating
      which elements are duplicates.
      It is semantically equivalent to \code{duplicated(as.data.frame(x))}
      (see \code{?\link{duplicated}} for more information).
    }
    \item{}{
      \code{x[i]}:
      Return a new Ranges object (of the same type as \code{x}) made of
      the selected ranges.
      \code{i} can be a numeric vector, a logical vector, \code{NULL}
      or missing. If \code{x} is a \link{NormalIRanges} object and
      \code{i} a positive numeric subscript (i.e. a numeric vector of
      positive values), then \code{i} must be strictly increasing.
    }
    \item{}{
      \code{rep(x, times)}:
      Return a new Ranges object made of the repeated elements.
    }
    \item{}{
      \code{union(x, y)}:
    }
    \item{}{
      \code{intersect(x, y)}:
    }
    \item{}{
      \code{setdiff(x, y)}:
    }
    \item{}{
      \code{c(x, ...)}:
    }
  }
}

\seealso{
  \link{IRanges-class},
  \link{XRanges-class},
  \link{IRanges-utils},
  \link{RangedData-class},
  \link{IntervalTree-class},
  \code{\link{as.data.frame}},
  \code{\link{duplicated}},
  \code{\link{as.matrix}}
}

\examples{
  x <- IRanges(start=c(2:-1, 13:15), width=c(0:3, 2:0))
  x
  length(x)
  start(x)
  width(x)
  end(x)
  isEmpty(x)
  as.matrix(x)
  as.data.frame(x)

  ## Subsetting:
  x[4:2]                  # 3 ranges
  x[-1]                   # 6 ranges
  x[FALSE]                # 0 range
  x0 <- x[width(x) == 0]  # 2 ranges
  isEmpty(x0)

  ## Use the replacement methods to resize the ranges:
  width(x) <- width(x) * 2 + 1
  x
  end(x) <- start(x)            # equivalent to width(x) <- 0
  x
  width(x) <- c(2, 0, 4) 
  x
  start(x)[3] <- end(x)[3] - 2  # resize the 3rd range
  x
  duplicated(x)

  ## Name the elements:
  names(x)
  names(x) <- c("range1", "range2")
  x
  x[is.na(names(x))]  # 5 ranges
  x[!is.na(names(x))]  # 2 ranges
}

\keyword{methods}
\keyword{classes}
