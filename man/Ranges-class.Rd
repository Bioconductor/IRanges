\name{Ranges-class}
\docType{class}

% Classes:
\alias{class:Ranges}
\alias{Ranges-class}
\alias{Ranges}

% Generics and methods:
\alias{length,Ranges-method}
\alias{elementLengths,Ranges-method}
\alias{width}
\alias{start,Ranges-method}
\alias{width,Ranges-method}
\alias{end,Ranges-method}
\alias{mid}
\alias{mid,Ranges-method}
\alias{start<-}
\alias{width<-}
\alias{end<-}
\alias{isEmpty,Ranges-method}
\alias{as.matrix,Ranges-method}
\alias{as.data.frame,Ranges-method}
\alias{as.integer,Ranges-method}
\alias{unlist,Ranges-method}
\alias{[[,Ranges-method}
\alias{show,Ranges-method}
\alias{[,Ranges-method}
\alias{isNormal}
\alias{isNormal,Ranges-method}
\alias{whichFirstNotNormal}
\alias{whichFirstNotNormal,Ranges-method}
\alias{update,Ranges-method}

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
  has a null width. Depending on the context, it can be interpreted
  either as just the empty \emph{set} of integers or, more precisely,
  as the position \emph{between} its "end" and its "start" (note that
  for an empty range, the "end" equals the "start" minus one).

  The length of a Ranges object is the number of ranges in it, not the
  number of integer values in its ranges.

  A Ranges object is considered empty iff all its ranges are empty.

  Ranges objects have a vector-like semantic i.e. they only support single
  subscript subsetting (unlike, for example, standard R data frames which
  can be subsetted by row and by column).

  The Ranges class itself is a virtual class. The following classes derive
  directly from the Ranges class: \link{IRanges} and \link{IntervalTree}.
}

\section{Methods}{
  In the code snippets below, \code{x}, \code{y} and \code{object} are
  Ranges objects. Not all the functions described below will necessarily
  work with all kinds of Ranges objects but they should work at least
  for \link{IRanges} objects.

  Note that many more operations on Ranges objects are described in other
  man pages of the IRanges package. See for example the man page for intra
  range transformations (e.g. \code{shift()}, see
  \code{?`\link{intra-range-methods}`}), or the man page for inter range
  transformations (e.g. \code{reduce()}, see
  \code{?`\link{inter-range-methods}`}), or the man page for
  \code{findOverlaps} methods (see \code{?`\link{findOverlaps-methods}`}),
  or the man page for \link{RangesList} objects where the \code{split}
  method for Ranges objects is documented.

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
      \code{mid(x)}: returns the midpoint of the range,
      \code{start(x) + floor((width(x) - 1)/2)}.
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
      \code{as.integer(x)}:
      Convert \code{x} into an integer vector, by converting each range
      into the integer sequence formed by \code{from:to} and concatenating
      them together.
    }
    \item{}{
      \code{unlist(x, recursive = TRUE, use.names = TRUE)}:
      Similar to \code{as.integer(x)} except can add names to elements.
    }
    \item{}{
      \code{x[[i]]}:
      Return integer vector \code{start(x[i]):end(x[i])} denoted by \code{i}.
      Subscript \code{i} can be a single integer or a character string.
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
      \code{rep(x, times, length.out, each)}:
      Repeats the values in \code{x} through one of the following conventions:
      \describe{
        \item{\code{times}}{Vector giving the number of times to repeat each
          element if of length \code{length(x)}, or to repeat the Ranges
          elements if of length 1.}
        \item{\code{length.out}}{Non-negative integer. The desired length of
          the output vector.}
        \item{\code{each}}{Non-negative integer.  Each element of \code{x} is
          repeated \code{each} times.}
      }
    }
    \item{}{
      \code{c(x, ...)}:
      Combine \code{x} and the Ranges objects in \code{...} together.
      Any object in \code{...} must belong to the same class as \code{x},
      or to one of its subclasses, or must be \code{NULL}.
      The result is an object of the same class as \code{x}.
      NOTE: Only works for \link{IRanges} (and derived) objects for now.
    }
    \item{}{
      \code{x * y}:
      The arithmetic operation \code{x * y} is for centered zooming. It
      symmetrically scales the width of \code{x} by \code{1/y}, where
      \code{y} is a numeric vector that is recycled as necessary. For
      example, \code{x * 2} results in ranges with half their previous width
      but with approximately the same midpoint. The ranges have been
      \dQuote{zoomed in}. If \code{y} is negative, it is equivalent to
      \code{x * (1/abs(y))}. Thus, \code{x * -2} would double the widths in
      \code{x}. In other words, \code{x} has been \dQuote{zoomed out}.
    }
    \item{}{
      \code{x + y}: Expands the ranges in \code{x} on either side by the
      corresponding value in the numeric vector \code{y}.
    }
  }
}

\section{Normality}{
  A Ranges object \code{x} is implicitly representing an arbitrary finite
  set of integers (that are not necessarily consecutive). This set is the
  set obtained by taking the union of all the values in all the ranges in
  \code{x}. This representation is clearly not unique: many different
  Ranges objects can be used to represent the same set of integers.
  However one and only one of them is guaranteed to be "normal".

  By definition a Ranges object is said to be "normal" when its ranges are:
    (a) not empty (i.e. they have a non-null width);
    (b) not overlapping;
    (c) ordered from left to right;
    (d) not even adjacent (i.e. there must be a non empty gap between 2
        consecutive ranges).

  Here is a simple algorithm to determine whether \code{x} is "normal":
    (1) if \code{length(x) == 0}, then \code{x} is normal;
    (2) if \code{length(x) == 1}, then \code{x} is normal iff \code{width(x) >= 1};
    (3) if \code{length(x) >= 2}, then \code{x} is normal iff:
        \preformatted{  start(x)[i] <= end(x)[i] < start(x)[i+1] <= end(x)[i+1]}
        for every 1 <= \code{i} < \code{length(x)}.

  The obvious advantage of using a "normal" Ranges object to represent
  a given finite set of integers is that it is the smallest in terms of
  of number of ranges and therefore in terms of storage space.
  Also the fact that we impose its ranges to be ordered from left to
  right makes it unique for this representation.

  A special container (\link{NormalIRanges}) is provided for holding
  a "normal" \link{IRanges} object: a \link{NormalIRanges} object is
  just an \link{IRanges} object that is guaranteed to be "normal".

  Here are some methods related to the notion of "normal" Ranges:

  \describe{
    \item{}{
      \code{isNormal(x)}:
      Return a logical value indicating whether \code{x} is "normal" or not.
    }
    \item{}{
      \code{whichFirstNotNormal(x)}:
      Return \code{NA} if \code{x} is normal, or the smallest valid indice
      \code{i} in \code{x} for which \code{x[1:i]} is not "normal".
    }
  }
}

\author{H. Pages and M. Lawrence}

\seealso{
  \link{Ranges-comparison},
  \link{intra-range-methods},
  \link{inter-range-methods},
  \link{IRanges-class},
  \link{IRanges-utils},
  \link{IRanges-setops},
  \link{RangedData-class},
  \link{IntervalTree-class},
  \code{\link{update}},
  \code{\link{as.matrix}},
  \code{\link{as.data.frame}},
  \code{\link{rep}}
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

  ## Name the elements:
  names(x)
  names(x) <- c("range1", "range2")
  x
  x[is.na(names(x))]  # 5 ranges
  x[!is.na(names(x))]  # 2 ranges

  ir <- IRanges(c(1,5), c(3,10))
  ir*1 # no change
  ir*c(1,2) # zoom second range by 2X
  ir*-2 # zoom out 2X  
}

\keyword{methods}
\keyword{classes}
