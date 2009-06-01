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
\alias{togrouplength}
\alias{togrouplength,Grouping-method}
\alias{[[<-,Grouping-method}
\alias{show,Grouping-method}

% H2LGrouping and Dups objects:
\alias{class:H2LGrouping}
\alias{H2LGrouping-class}
\alias{H2LGrouping}

\alias{high2low}
\alias{high2low,H2LGrouping-method}
\alias{low2high}
\alias{low2high,H2LGrouping-method}
\alias{length,H2LGrouping-method}
\alias{nobj,H2LGrouping-method}
\alias{[[,H2LGrouping-method}
\alias{grouplength,H2LGrouping-method}
\alias{members,H2LGrouping-method}
\alias{vmembers,H2LGrouping-method}
\alias{togroup,H2LGrouping-method}

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
\alias{togroup,Partitioning-method}
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
      \code{grouplength(x)} is equivalent to \code{grouplength(x, seq_len(length(x)))}.
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
  for Grouping objects, those objects can be considered \link{Sequence}
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
  A Partitioning container represents a block-grouping i.e. a grouping
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
      \code{PartitioningByEnd(end=integer(), names=NULL)}:
      Return the PartitioningByEnd object made of the partitions ending
      at the values specified by \code{end}. \code{end} must contain
      sorted non-negative integer values.
    }
    \item{}{
      \code{PartitioningByWidth(width=integer(), names=NULL)}:
      Return the PartitioningByWidth object made of the partitions with
      the widths specified by \code{width}. \code{width} must contain
      non-negative integer values.
    }
  }
  Note that these constructors don't recycle their \code{names} argument
  (to remain consistent with what \code{`names<-`} does on standard
  vectors).
}

\author{H. Pages and P. Aboyoun}

\seealso{
  \link{Sequence-class},
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
  x <- H2LGrouping(high2low)
  x

  ## The Grouping core API:
  length(x)
  nobj(x)  # same as 'length(x)' for H2LGrouping objects
  x[[1]]
  x[[2]]
  x[[3]]
  x[[4]]
  x[[5]]
  grouplength(x)  # same as 'unname(sapply(x, length))'
  grouplength(x, 5:2)
  members(x, 5:2)  # all the members are put together and sorted
  togroup(x)
  togroup(x, 5:2)
  togrouplength(x)  # same as 'grouplength(x, togroup(x))'
  togrouplength(x, 5:2)

  ## The Sequence API:
  as.list(x)
  sapply(x, length)

  ## ---------------------------------------------------------------------
  ## B. Dups OBJECTS
  ## ---------------------------------------------------------------------
  dups <- as(x, "Dups")
  dups
  duplicated(dups)  # same as 'duplicated(togroup(dups))'

  ## ---------------------------------------------------------------------
  ## C. Partitioning OBJECTS
  ## ---------------------------------------------------------------------
  x <- PartitioningByEnd(end=c(4, 7, 7, 8, 15), names=LETTERS[1:5])
  x  # the 3rd partition is empty

  ## The Grouping core API:
  length(x)
  nobj(x)
  x[[1]]
  x[[2]]
  x[[3]]
  grouplength(x)  # same as 'unname(sapply(x, length))' and 'width(x)'
  togroup(x)
  togrouplength(x)  # same as 'grouplength(x, togroup(x))'
  names(x)

  ## The Ranges core API:
  start(x)
  end(x)
  width(x)

  ## The Sequence API:
  as.list(x)
  sapply(x, length)

  ## Replacing the names:
  names(x)[3] <- "empty partition"
  x

  ## Coercion to an IRanges object:
  as(x, "IRanges")

  ## Other examples:
  PartitioningByEnd(end=c(0, 0, 19), names=LETTERS[1:3])
  PartitioningByEnd()  # no partition
  PartitioningByEnd(end=integer(9))  # all partitions are empty

  ## ---------------------------------------------------------------------
  ## D. RELATIONSHIP BETWEEN Partitioning OBJECTS AND successiveIRanges()
  ## ---------------------------------------------------------------------
  mywidths <- c(4, 3, 0, 1, 7)

  ## The 3 following calls produce the same ranges:
  x1 <- successiveIRanges(mywidths)  # IRanges instance.
  x2 <- PartitioningByEnd(end=cumsum(mywidths))  # PartitioningByEnd instance.
  x3 <- PartitioningByWidth(width=mywidths)  # PartitioningByWidth instance.
  stopifnot(identical(as(x1, "PartitioningByEnd"), x2))
  stopifnot(identical(as(x1, "PartitioningByWidth"), x3))
}

\keyword{methods}
\keyword{classes}
