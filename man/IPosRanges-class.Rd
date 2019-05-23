\name{IPosRanges-class}
\docType{class}

% Classes:
\alias{class:IPosRanges}
\alias{IPosRanges-class}
\alias{IPosRanges}

% Generics and methods:
\alias{width}
\alias{start,Ranges-method}
\alias{end,Ranges-method}
\alias{width,Ranges-method}
\alias{length,Ranges-method}
\alias{start,Pos-method}
\alias{end,Pos-method}
\alias{width,Pos-method}
\alias{names,Pos-method}
\alias{names<-,Pos-method}
\alias{elementNROWS,Ranges-method}
\alias{mid}
\alias{mid,Ranges-method}
\alias{isEmpty,Ranges-method}
\alias{isNormal}
\alias{isNormal,Ranges-method}
\alias{whichFirstNotNormal}
\alias{whichFirstNotNormal,Ranges-method}
\alias{start<-}
\alias{width<-}
\alias{end<-}
\alias{as.character,IPosRanges-method}
\alias{as.factor,IPosRanges-method}
\alias{as.matrix,IPosRanges-method}
\alias{as.data.frame,IPosRanges-method}
\alias{show,IPosRanges-method}
\alias{getListElement,IPosRanges-method}
\alias{tile}
\alias{tile,IPosRanges-method}
\alias{slidingWindows}
\alias{slidingWindows,IPosRanges-method}


\title{IPosRanges objects}

\description{
  The IPosRanges \emph{virtual} class is a general container for storing
  a vector of ranges of integer positions.
}

\details{
  An IPosRanges object is a vector-like object where each element
  describes a "range of integer positions".

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

  The length of an IPosRanges object is the number of ranges in it, not
  the number of integer values in its ranges.

  An IPosRanges object is considered empty iff all its ranges are empty.

  IPosRanges objects have a vector-like semantic i.e. they only support
  single subscript subsetting (unlike, for example, standard R data frames
  which can be subsetted by row and by column).

  The IPosRanges class itself is a virtual class. The following classes
  derive directly from it: \link{IRanges}, \link{IPos}, \link{NCList},
  and \link{GroupingRanges}.
}

\section{Methods}{
  In the code snippets below, \code{x}, \code{y} and \code{object} are
  IPosRanges objects. Not all the functions described below will necessarily
  work with all kinds of IPosRanges derivatives but they should work at
  least for \link{IRanges} objects.

  Note that many more operations on IPosRanges objects are described in
  other man pages of the \pkg{IRanges} package. See for example the man page
  for \emph{intra range transformations} (e.g. \code{shift()}, see
  \code{?`\link{intra-range-methods}`}), or the man page for inter range
  transformations (e.g. \code{reduce()}, see
  \code{?`\link{inter-range-methods}`}), or the man page for
  \code{findOverlaps} methods (see \code{?`\link{findOverlaps-methods}`}),
  or the man page for \link{IntegerRangesList} objects where the \code{split}
  method for \link{IntegerRanges} derivatives is documented.

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
      \code{tile(x, n, width, ...)}:
      Splits each range in \code{x} into subranges as specified by \code{n}
      (number of ranges) or \code{width}. Only one of \code{n} or \code{width} 
      can be specified. The return value is a \code{IRangesList} the same 
      length as \code{x}. IPosRanges with a width less than the \code{width}
      argument are returned unchanged.
    }
    \item{}{
      \code{slidingWindows(x, width, step=1L)}: Generates sliding
      windows within each range of \code{x}, of width \code{width}, and
      starting every \code{step} positions. The return value is a
      \code{IRangesList} the same length as \code{x}. IPosRanges
      with a width less than the \code{width} argument are returned
      unchanged. If the sliding windows do not exactly cover \code{x},
      the last window is partial.
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
      \code{x[[i]]}:
      Return integer vector \code{start(x[i]):end(x[i])} denoted by \code{i}.
      Subscript \code{i} can be a single integer or a character string.
    }
    \item{}{
      \code{x[i]}:
      Return a new IPosRanges object (of the same type as \code{x}) made
      of the selected ranges.
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
          element if of length \code{length(x)}, or to repeat the
          IPosRanges elements if of length 1.}
        \item{\code{length.out}}{Non-negative integer. The desired length of
          the output vector.}
        \item{\code{each}}{Non-negative integer.  Each element of \code{x} is
          repeated \code{each} times.}
      }
    }
    \item{}{
      \code{c(x, ..., ignore.mcols=FALSE)}:
      Concatenate IPosRanges object \code{x} and the IPosRanges objects
      in \code{...} together.
      See \code{?\link[S4Vectors]{c}} in the \pkg{S4Vectors} package
      for more information about concatenating Vector derivatives.
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
    \item{}{
      \code{show(x)}:
      By default the \code{show} method displays 5 head and 5 tail
      lines. The number of lines can be altered by setting the global
      options \code{showHeadLines} and \code{showTailLines}. If the 
      object length is less than the sum of the options, the full object 
      is displayed. These options affect display of \link{IRanges},
      \link{IPos}, \link[S4Vectors]{Hits}, \link[GenomicRanges]{GRanges},
      \link[GenomicRanges]{GPos}, \link[GenomicAlignments]{GAlignments},
      \link[Biostrings]{XStringSet} objects, and more...
    }
  }
}

\section{Normality}{
  An IPosRanges object \code{x} is implicitly representing an arbitrary
  finite set of integers (that are not necessarily consecutive). This set is
  the set obtained by taking the union of all the values in all the ranges in
  \code{x}. This representation is clearly not unique: many different
  IPosRanges objects can be used to represent the same set of integers.
  However one and only one of them is guaranteed to be \emph{normal}.

  By definition an IPosRanges object is said to be \emph{normal} when its
  ranges are:
    (a) not empty (i.e. they have a non-null width);
    (b) not overlapping;
    (c) ordered from left to right;
    (d) not even adjacent (i.e. there must be a non empty gap between 2
        consecutive ranges).

  Here is a simple algorithm to determine whether \code{x} is \emph{normal}:
    (1) if \code{length(x) == 0}, then \code{x} is normal;
    (2) if \code{length(x) == 1}, then \code{x} is normal iff
        \code{width(x) >= 1};
    (3) if \code{length(x) >= 2}, then \code{x} is normal iff:
        \preformatted{  start(x)[i] <= end(x)[i] < start(x)[i+1] <= end(x)[i+1]}
        for every 1 <= \code{i} < \code{length(x)}.

  The obvious advantage of using a \emph{normal} IPosRanges object to
  represent a given finite set of integers is that it is the smallest in
  terms of number of ranges and therefore in terms of storage space.
  Also the fact that we impose its ranges to be ordered from left to
  right makes it unique for this representation.

  A special container (\link{NormalIRanges}) is provided for holding
  a \emph{normal} \link{IRanges} object: a \link{NormalIRanges} object is
  just an \link{IRanges} object that is guaranteed to be \emph{normal}.

  Here are some methods related to the notion of \emph{normal} IPosRanges:

  \describe{
    \item{}{
      \code{isNormal(x)}:
      Return TRUE or FALSE indicating whether \code{x} is \emph{normal} or not.
    }
    \item{}{
      \code{whichFirstNotNormal(x)}:
      Return \code{NA} if \code{x} is \emph{normal}, or the smallest valid
      indice \code{i} in \code{x} for which \code{x[1:i]} is not \emph{normal}.
    }
  }
}

\author{H. Pagès and M. Lawrence}

\seealso{
  \itemize{
    \item The \link{IRanges} class, a concrete IPosRanges direct subclass for
          storing a set of \emph{integer ranges}.

    \item The \link{IPos} class, an IPosRanges direct subclass for representing
          a set of \emph{integer positions} (i.e. \emph{integer ranges} of
          width 1).

    \item \link{IPosRanges-comparison} for comparing and ordering ranges.

    \item \link{findOverlaps-methods} for finding/counting overlapping ranges.

    \item \link{intra-range-methods} and \link{inter-range-methods}
          for \emph{intra range} and \emph{inter range} transformations
          of \link{IntegerRanges} derivatives.

    \item \link{coverage-methods} for computing the coverage
          of a set of ranges.

    \item \link{setops-methods} for set operations on ranges.

    \item \link{nearest-methods} for finding the nearest range neighbor.
  }
}

\examples{
## ---------------------------------------------------------------------
## Basic manipulation
## ---------------------------------------------------------------------
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
