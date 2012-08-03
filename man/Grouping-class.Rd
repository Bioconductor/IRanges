\name{Grouping-class}
\docType{class}

% Grouping objects:
\alias{class:Grouping}
\alias{Grouping-class}
\alias{Grouping}

\alias{nobj}
\alias{grouplength}
\alias{grouplength,Grouping-method}
\alias{members}
\alias{members,Grouping-method}
\alias{vmembers}
\alias{vmembers,Grouping-method}
\alias{togroup}
\alias{togroup,ANY-method}
\alias{tofactor}
\alias{togrouplength}
\alias{togrouplength,Grouping-method}
\alias{show,Grouping-method}

% H2LGrouping and Dups objects:
\alias{class:H2LGrouping}
\alias{H2LGrouping-class}
\alias{H2LGrouping}

\alias{high2low}
\alias{high2low,H2LGrouping-method}
\alias{high2low,vector-method}
\alias{high2low,Vector-method}
\alias{low2high}
\alias{low2high,H2LGrouping-method}
\alias{length,H2LGrouping-method}
\alias{nobj,H2LGrouping-method}
\alias{[[,H2LGrouping-method}
\alias{grouplength,H2LGrouping-method}
\alias{members,H2LGrouping-method}
\alias{vmembers,H2LGrouping-method}
\alias{togroup,H2LGrouping-method}
\alias{grouprank}
\alias{grouprank,H2LGrouping-method}
\alias{togrouprank}
\alias{togrouprank,H2LGrouping-method}
\alias{length<-,H2LGrouping-method}

\alias{class:Dups}
\alias{Dups-class}
\alias{Dups}

\alias{duplicated,Dups-method}
\alias{show,Dups-method}

% Partitioning objects:
\alias{class:Partitioning}
\alias{Partitioning-class}
\alias{Partitioning}

\alias{[[,Partitioning-method}
\alias{grouplength,Partitioning-method}
\alias{names,Partitioning-method}
\alias{names<-,Partitioning-method}

\alias{class:PartitioningByEnd}
\alias{PartitioningByEnd-class}
\alias{PartitioningByEnd}

\alias{end,PartitioningByEnd-method}
\alias{length,PartitioningByEnd-method}
\alias{nobj,PartitioningByEnd-method}
\alias{start,PartitioningByEnd-method}
\alias{width,PartitioningByEnd-method}
\alias{coerce,Ranges,PartitioningByEnd-method}

\alias{class:PartitioningByWidth}
\alias{PartitioningByWidth-class}
\alias{PartitioningByWidth}

\alias{width,PartitioningByWidth-method}
\alias{length,PartitioningByWidth-method}
\alias{end,PartitioningByWidth-method}
\alias{nobj,PartitioningByWidth-method}
\alias{start,PartitioningByWidth-method}
\alias{coerce,Ranges,PartitioningByWidth-method}


\title{Grouping objects}

\description{
  In this man page, we call "grouping" the action of dividing a collection of
  NO objects into NG groups (some of which may be empty). The Grouping class
  and subclasses are containers for representing groupings.
}

\section{The Grouping core API}{
  Let's give a formal description of the Grouping core API:

  Groups G_i are indexed from 1 to NG (1 <= i <= NG).

  Objects O_j are indexed from 1 to NO (1 <= j <= NO).

  Every object must belong to one group and only one.

  Given that empty groups are allowed, NG can be greater than NO.

  Grouping an empty collection of objects (NO = 0) is supported. In that
  case, all the groups are empty. And only in that case, NG can be zero
  too (meaning there are no groups).

  If \code{x} is a Grouping object:
  \describe{
    \item{}{
      \code{length(x)}:
      Returns the number of groups (NG).
    }
    \item{}{
      \code{names(x)}:
      Returns the names of the groups.
    }
    \item{}{
      \code{nobj(x)}:
      Returns the number of objects (NO). Equivalent to \code{length(togroup(x))}.
    }
  }
  
  Going from groups to objects:
  \describe{
    \item{}{
      \code{x[[i]]}:
      Returns the indices of the objects (the j's) that belong to G_i.
      The j's are returned in ascending order.
      This provides the mapping from groups to objects (one-to-many mapping).
    }
    \item{}{
      \code{grouplength(x, i=NULL)}:
      Returns the number of objects in G_i.
      Works in a vectorized fashion (unlike \code{x[[i]]}).
      \code{grouplength(x)} is equivalent to
      \code{grouplength(x, seq_len(length(x)))}.
      If \code{i} is not NULL, \code{grouplength(x, i)} is equivalent to
      \code{sapply(i, function(ii) length(x[[ii]]))}.
    }
    \item{}{
      \code{members(x, i)}:
      Equivalent to \code{x[[i]]} if \code{i} is a single integer.
      Otherwise, if \code{i} is an integer vector of arbitrary length, it's
      equivalent to \code{sort(unlist(sapply(i, function(ii) x[[ii]])))}.
    }
    \item{}{
      \code{vmembers(x, L)}:
      A version of \code{members} that works in a vectorized fashion with
      respect to the \code{L} argument (\code{L} must be a list of integer
      vectors). Returns \code{lapply(L, function(i) members(x, i))}.
    }
  }

  Going from objects to groups:
  \describe{
    \item{}{
      \code{togroup(x, j=NULL)}:
      Returns the index i of the group that O_j belongs to.
      This provides the mapping from objects to groups (many-to-one mapping).
      Works in a vectorized fashion. \code{togroup(x)} is equivalent to
      \code{togroup(x, seq_len(nobj(x)))}: both return the entire mapping in
      an integer vector of length NO.
      If \code{j} is not NULL, \code{togroup(x, j)} is equivalent to
      \code{y <- togroup(x); y[j]}.
    }
    \item{}{
      \code{togrouplength(x, j=NULL)}:
      Returns the number of objects that belong to the same group as O_j
      (including O_j itself).
      Equivalent to \code{grouplength(x, togroup(x, j))}.
    }
  }

  Given that \code{length}, \code{names} and \code{[[} are defined
  for Grouping objects, those objects can be considered \link{List}
  objects. In particular, \code{as.list} works out-of-the-box on them.

  One important property of any Grouping object \code{x} is
  that \code{unlist(as.list(x))} is always a permutation of 
  \code{seq_len(nobj(x))}. This is a direct consequence of the fact
  that every object in the grouping belongs to one group and only
  one.
}

\section{The H2LGrouping and Dups subclasses}{
  [DOCUMENT ME]
}

\section{The Partitioning subclass}{
  A Partitioning container represents a block-grouping, i.e. a grouping
  where each group contains objects that are neighbors in the original
  collection of objects. More formally, a grouping \code{x} is a
  block-grouping iff \code{togroup(x)} is sorted in increasing order
  (not necessarily strictly increasing).

  A block-grouping object can also be seen (and manipulated) as a
  \link{Ranges} object where all the ranges are adjacent starting at 1
  (i.e. it covers the 1:NO interval with no overlap between the ranges).

  Note that a Partitioning object is both: a particular type of Grouping
  object and a particular type of \link{Ranges} object. Therefore all the
  methods that are defined for Grouping and \link{Ranges} objects can also
  be used on a Partitioning object. See \code{?Ranges} for a description of
  the \link{Ranges} API.

  The Partitioning class is virtual with 2 concrete subclasses:
  PartitioningByEnd (only stores the end of the groups, allowing fast
  mapping from groups to objects), and PartitioningByWidth (only stores
  the width of the groups).
}

\section{Constructors}{
  \describe{
    \item{}{
      \code{H2LGrouping(high2low=integer())}:
      [DOCUMENT ME]
    }
    \item{}{
      \code{Dups(high2low=integer())}:
      [DOCUMENT ME]
    }
    \item{}{
      \code{PartitioningByEnd(x=integer(), NG=NULL, names=NULL)}:
      \code{x} must be either a list-like object or a sorted integer vector.
      \code{NG} must be either \code{NULL} or a single integer.
      \code{names} must be either \code{NULL} or a character vector of
      length \code{NG} (if supplied) or \code{length(x)} (if \code{NG}
      is not supplied).

      Returns the following PartitioningByEnd object \code{y}:
      \itemize{
        \item If \code{x} is a list-like object, then the returned object
              \code{y} has the same length as \code{x} and is such that
              \code{width(y)} is identical to \code{elementLengths(x)}.
        \item If \code{x} is an integer vector and \code{NG} is not supplied,
              then \code{x} must be sorted (checked) and contain non-NA
              non-negative values (NOT checked).
              The returned object \code{y} has the same length as \code{x}
              and is such that \code{end(y)} is identical to \code{x}.
        \item If \code{x} is an integer vector and \code{NG} is supplied,
              then \code{x} must be sorted (checked) and contain values
              >= 1 and <= \code{NG} (checked).
              The returned object \code{y} is of length \code{NG} and is
              such that \code{togroup(y)} is identical to \code{x}.
      }
      If the \code{names} argument is supplied, it is used to name the
      partitions.
    }
    \item{}{
      \code{PartitioningByWidth(x=integer(), NG=NULL, names=NULL)}:
      \code{x} must be either a list-like object or an integer vector.
      \code{NG} must be either \code{NULL} or a single integer.
      \code{names} must be either \code{NULL} or a character vector of
      length \code{NG} (if supplied) or \code{length(x)} (if \code{NG}
      is not supplied).

      Returns the following PartitioningByWidth object \code{y}:
      \itemize{
        \item If \code{x} is a list-like object, then the returned object
              \code{y} has the same length as \code{x} and is such that
              \code{width(y)} is identical to \code{elementLengths(x)}.
        \item If \code{x} is an integer vector and \code{NG} is not supplied,
              then \code{x} must contain non-NA non-negative values (NOT
              checked).
              The returned object \code{y} has the same length as \code{x}
              and is such that \code{width(y)} is identical to \code{x}.
        \item If \code{x} is an integer vector and \code{NG} is supplied,
              then \code{x} must be sorted (checked) and contain values
              >= 1 and <= \code{NG} (checked).
              The returned object \code{y} is of length \code{NG} and is
              such that \code{togroup(y)} is identical to \code{x}.
      }
      If the \code{names} argument is supplied, it is used to name the
      partitions.
    }
  }
  Note that these constructors don't recycle their \code{names} argument
  (to remain consistent with what \code{`names<-`} does on standard
  vectors).
}

\author{H. Pages}

\seealso{
  \link{List-class},
  \link{Ranges-class},
  \link{IRanges-class},
  \link{successiveIRanges},
  \link[base]{cumsum},
  \link[base]{diff}
}

\examples{
showClass("Grouping")  # shows (some of) the known subclasses

## ---------------------------------------------------------------------
## A. H2LGrouping OBJECTS
## ---------------------------------------------------------------------
high2low <- c(NA, NA, 2, 2, NA, NA, NA, 6, NA, 1, 2, NA, 6, NA, NA, 2)
h2l <- H2LGrouping(high2low)
h2l

## The Grouping core API:
length(h2l)
nobj(h2l)  # same as 'length(h2l)' for H2LGrouping objects
h2l[[1]]
h2l[[2]]
h2l[[3]]
h2l[[4]]
h2l[[5]]
grouplength(h2l)  # same as 'unname(sapply(h2l, length))'
grouplength(h2l, 5:2)
members(h2l, 5:2)  # all the members are put together and sorted
togroup(h2l)
togroup(h2l, 5:2)
togrouplength(h2l)  # same as 'grouplength(h2l, togroup(h2l))'
togrouplength(h2l, 5:2)

## The List API:
as.list(h2l)
sapply(h2l, length)

## ---------------------------------------------------------------------
## B. Dups OBJECTS
## ---------------------------------------------------------------------
dups1 <- as(h2l, "Dups")
dups1
duplicated(dups1)  # same as 'duplicated(togroup(dups1))'

### The purpose of a Dups object is to describe the groups of duplicated
### elements in a vector-like object:
x <- c(2, 77, 4, 4, 7, 2, 8, 8, 4, 99)
x_high2low <- high2low(x)
x_high2low  # same length as 'x'
dups2 <- Dups(x_high2low)
dups2
togroup(dups2)
duplicated(dups2)
togrouplength(dups2)  # frequency for each element
table(x)

## ---------------------------------------------------------------------
## C. Partitioning OBJECTS
## ---------------------------------------------------------------------
pbe1 <- PartitioningByEnd(c(4, 7, 7, 8, 15), names=LETTERS[1:5])
pbe1  # the 3rd partition is empty

## The Grouping core API:
length(pbe1)
nobj(pbe1)
pbe1[[1]]
pbe1[[2]]
pbe1[[3]]
grouplength(pbe1)  # same as 'unname(sapply(pbe1, length))' and 'width(pbe1)'
togroup(pbe1)
togrouplength(pbe1)  # same as 'grouplength(pbe1, togroup(pbe1))'
names(pbe1)

## The Ranges core API:
start(pbe1)
end(pbe1)
width(pbe1)

## The List API:
as.list(pbe1)
sapply(pbe1, length)

## Replacing the names:
names(pbe1)[3] <- "empty partition"
pbe1

## Coercion to an IRanges object:
as(pbe1, "IRanges")

## Other examples:
PartitioningByEnd(c(0, 0, 19), names=LETTERS[1:3])
PartitioningByEnd()  # no partition
PartitioningByEnd(integer(9))  # all partitions are empty
x <- c(1L, 5L, 5L, 6L, 8L)
pbe2 <- PartitioningByEnd(x, NG=10L)
stopifnot(identical(togroup(pbe2), x))
pbw2 <- PartitioningByWidth(x, NG=10L)
stopifnot(identical(togroup(pbw2), x))

## ---------------------------------------------------------------------
## D. RELATIONSHIP BETWEEN Partitioning OBJECTS AND successiveIRanges()
## ---------------------------------------------------------------------
mywidths <- c(4, 3, 0, 1, 7)

## The 3 following calls produce the same ranges:
ir <- successiveIRanges(mywidths)  # IRanges instance.
pbe <- PartitioningByEnd(cumsum(mywidths))  # PartitioningByEnd instance.
pbw <- PartitioningByWidth(mywidths)  # PartitioningByWidth instance.
stopifnot(identical(as(ir, "PartitioningByEnd"), pbe))
stopifnot(identical(as(ir, "PartitioningByWidth"), pbw))
}

\keyword{methods}
\keyword{classes}
