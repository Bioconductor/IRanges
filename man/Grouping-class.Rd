\name{Grouping-class}
\docType{class}

% Grouping objects
\alias{class:Grouping}
\alias{Grouping-class}
\alias{Grouping}

\alias{nobj}
\alias{grouplengths}
\alias{grouplengths,Grouping-method}
\alias{grouplengths,CompressedGrouping-method}
\alias{show,Grouping-method}

% ManyToOneGrouping objects
\alias{class:ManyToOneGrouping}
\alias{ManyToOneGrouping-class}
\alias{ManyToOneGrouping}

\alias{nobj,ManyToOneGrouping-method}
\alias{nobj,CompressedManyToOneGrouping-method}
\alias{members}
\alias{members,ManyToOneGrouping-method}
\alias{vmembers}
\alias{vmembers,ManyToOneGrouping-method}
\alias{togroup}
\alias{togroup,ManyToOneGrouping-method}
\alias{togrouplength}
\alias{togrouplength,ManyToOneGrouping-method}

\alias{coerce,grouping,Grouping-method}
\alias{coerce,grouping,ManyToOneGrouping-method}
\alias{coerce,vector,Grouping-method}
\alias{coerce,vector,ManyToOneGrouping-method}
\alias{coerce,ManyToOneGrouping,factor-method}
\alias{coerce,DataFrame,Grouping-method}
\alias{coerce,FactorList,Grouping-method}
\alias{coerce,Hits,Grouping-method}

% ManyToManyGrouping objects
\alias{nobj,BaseManyToManyGrouping-method}

\alias{coerce,vector,ManyToManyGrouping-method}

% H2LGrouping and Dups objects
\alias{class:H2LGrouping}
\alias{H2LGrouping-class}
\alias{H2LGrouping}

\alias{high2low}
\alias{high2low,H2LGrouping-method}
\alias{high2low,ANY-method}
\alias{low2high}
\alias{low2high,H2LGrouping-method}
\alias{length,H2LGrouping-method}
\alias{nobj,H2LGrouping-method}
\alias{grouplengths,H2LGrouping-method}
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

% ManyToManyGrouping objects
\alias{class:ManyToManyGrouping}
\alias{ManyToManyGrouping-class}
\alias{ManyToManyGrouping}
\alias{nobj,ManyToManyGrouping-method}

% GroupingRanges objects
\alias{class:GroupingRanges}
\alias{GroupingRanges-class}
\alias{GroupingRanges}

\alias{grouplengths,GroupingRanges-method}

\alias{class:GroupingIRanges}
\alias{GroupingIRanges-class}
\alias{GroupingIRanges}

% Partitioning objects
\alias{class:Partitioning}
\alias{Partitioning-class}
\alias{Partitioning}

\alias{togroup,Partitioning-method}
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
\alias{end,PartitioningByWidth-method}
\alias{start,PartitioningByWidth-method}
\alias{coerce,Ranges,PartitioningByWidth-method}

% PartitioningMap objects
\alias{class:PartitioningMap}
\alias{PartitioningMap-class}
\alias{PartitioningMap}

\alias{mapOrder}
\alias{mapOrder,PartitioningMap-method}
\alias{show,PartitioningMap-method}

% SimpleGrouping & CompressedGrouping objects
\alias{class:SimpleGrouping}
\alias{SimpleGrouping-class}

\alias{class:CompressedGrouping}
\alias{CompressedGrouping-class}

\alias{class:SimpleManyToOneGrouping}
\alias{SimpleManyToOneGrouping-class}

\alias{class:CompressedManyToOneGrouping}
\alias{CompressedManyToOneGrouping-class}

\alias{class:SimpleManyToManyGrouping}
\alias{SimpleManyToManyGrouping-class}

\alias{class:CompressedManyToManyGrouping}
\alias{CompressedManyToManyGrouping-class}

% old stuff (deprecated & defunct)
\alias{togroup,ANY-method}
\alias{grouplength}

\title{Grouping objects}

\description{
  We call \emph{grouping} an arbitrary mapping from a collection of NO objects
  to a collection of NG groups, or, more formally, a bipartite graph
  between integer sets [1, NO] and [1, NG]. Objects mapped to a given group
  are said to belong to, or to be assigned to, or to be in that group.
  Additionally, the objects in each group are ordered. So for example the
  2 following groupings are considered different:
\preformatted{
  Grouping 1: NG = 3, NO = 5
              group   objects
                  1 : 4, 2
                  2 :
                  3 : 4

  Grouping 2: NG = 3, NO = 5
              group   objects
                  1 : 2, 4
                  2 :
                  3 : 4
}
  There are no restriction on the mapping e.g. any object can be mapped
  to 0, 1, or more groups, and can be mapped twice to the same group. Also
  some or all the groups can be empty.

  The Grouping class is a virtual class that formalizes the most general
  kind of grouping. More specific groupings (e.g. \emph{many-to-one groupings}
  or \emph{block-groupings}) are formalized via specific Grouping subclasses.

  This man page documents the core Grouping API, and 3 important Grouping
  subclasses: ManyToOneGrouping, GroupingRanges, and Partitioning (the last
  one deriving from the 2 first).
}

\section{The core Grouping API}{
  Let's give a formal description of the core Grouping API:

  Groups G_i are indexed from 1 to NG (1 <= i <= NG).

  Objects O_j are indexed from 1 to NO (1 <= j <= NO).

  Given that empty groups are allowed, NG can be greater than NO.

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
      Returns the number of objects (NO).
    }
  }
  
  Going from groups to objects:
  \describe{
    \item{}{
      \code{x[[i]]}:
      Returns the indices of the objects (the j's) that belong to G_i.
      This provides the mapping from groups to objects.
    }
    \item{}{
      \code{grouplengths(x, i=NULL)}:
      Returns the number of objects in G_i.
      Works in a vectorized fashion (unlike \code{x[[i]]}).
      \code{grouplengths(x)} is equivalent to
      \code{grouplengths(x, seq_len(length(x)))}.
      If \code{i} is not NULL, \code{grouplengths(x, i)} is equivalent to
      \code{sapply(i, function(ii) length(x[[ii]]))}.
    }
  }

  Note to developers: Given that \code{length}, \code{names} and \code{[[}
  are expected to work on any Grouping object, those objects can be seen as
  \link{List} objects. More precisely, the Grouping class actually extends
  the \link{IntegerList} class. In particular, many other "list" operations
  like \code{as.list}, \code{elementNROWS}, and \code{unlist}, etc...
  should work out-of-the-box on any Grouping object.
}

\section{ManyToOneGrouping objects}{
  The ManyToOneGrouping class is a virtual subclass of Grouping for
  representing \emph{many-to-one groupings}, that is, groupings where each
  object in the original collection of objects belongs to exactly one group.

  The grouping of an empty collection of objects in an arbitrary number of
  (necessarily empty) groups is a valid ManyToOneGrouping object.

  Note that, for a ManyToOneGrouping object, if NG is 0 then NO must also
  be 0.

  The ManyToOneGrouping API extends the core Grouping API by adding a couple
  more operations for going from groups to objects:
  \describe{
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

  And also by adding operations for going from objects to groups:
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
      Equivalent to \code{grouplengths(x, togroup(x, j))}.
    }
  }

  One important property of any ManyToOneGrouping object \code{x} is
  that \code{unlist(as.list(x))} is always a permutation of 
  \code{seq_len(nobj(x))}. This is a direct consequence of the fact
  that every object in the grouping belongs to one group and only
  one.
}

\section{2 ManyToOneGrouping concrete subclasses: H2LGrouping, Dups and
  SimpleManyToOneGrouping}{
  [DOCUMENT ME]

  Constructors:
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
      \code{ManyToOneGrouping(..., compress=TRUE)}: Collect \code{\dots}
      into a \code{ManyToOneGrouping}. The arguments will be coerced to
      integer vectors and combined into a list, unless there is a single
      list argument, which is taken to be an integer list. The resulting
      integer list should have a structure analogous to that of
      \code{Grouping} itself: each element represents a group in terms
      of the subscripts of the members. If \code{compress} is
      \code{TRUE}, the representation uses a \code{CompressedList},
      otherwise a \code{SimpleList}.
    }
  }
}

\section{ManyToManyGrouping objects}{
  The ManyToManyGrouping class is a virtual subclass of Grouping for
  representing \emph{many-to-many groupings}, that is, groupings where
  each object in the original collection of objects belongs to any
  number of groups.

  Constructors:
  \describe{
    \item{}{
      \code{ManyToManyGrouping(x, compress=TRUE)}: Collect \code{\dots}
      into a \code{ManyToManyGrouping}. The arguments will be coerced to
      integer vectors and combined into a list, unless there is a single
      list argument, which is taken to be an integer list. The resulting
      integer list should have a structure analogous to that of
      \code{Grouping} itself: each element represents a group in terms
      of the subscripts of the members. If \code{compress} is
      \code{TRUE}, the representation uses a \code{CompressedList},
      otherwise a \code{SimpleList}.
    }
  }
}

\section{GroupingRanges objects}{
  The GroupingRanges class is a virtual subclass of Grouping for representing
  \emph{block-groupings}, that is, groupings where each group is a block of
  adjacent elements in the original collection of objects. GroupingRanges
  objects support the Ranges API (e.g. \code{\link{start}}, \code{\link{end}},
  \code{\link{width}}, etc...) in addition to the Grouping API.
  See \code{?\link{Ranges}} for a description of the \link{Ranges} API.
}

\section{Partitioning objects}{
  The Partitioning class is a virtual subclass of GroupingRanges for
  representing \emph{block-groupings} where the blocks fully cover the
  original collection of objects and don't overlap. Since this makes them
  \emph{many-to-one groupings}, the Partitioning class is also a subclass
  of ManyToOneGrouping. An additional constraint of Partitioning objects
  is that the blocks must be ordered by ascending position with respect to
  the original collection of objects.

  The Partitioning virtual class itself has 3 concrete subclasses:
  PartitioningByEnd (only stores the end of the groups, allowing fast
  mapping from groups to objects), and PartitioningByWidth (only stores
  the width of the groups), and PartitioningMap which contains
  PartitioningByEnd and two additional slots to re-order and re-list
  the object to a related mapping.

  Constructors:
  \describe{
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
              \code{width(y)} is identical to \code{elementNROWS(x)}.
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
              \code{width(y)} is identical to \code{elementNROWS(x)}.
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
    \item{}{
      \code{PartitioningMap(x=integer(), mapOrder=integer())}:
      \code{x} is a list-like object or a sorted integer vector used to
               construct a PartitioningByEnd object.
      \code{mapOrder} numeric vector of the mapped order.

      Returns a PartitioningMap object.
    }
  }
  Note that these constructors don't recycle their \code{names} argument
  (to remain consistent with what \code{`names<-`} does on standard
  vectors).
}

\section{Coercions to Grouping objects}{
  These types can be coerced to different derivatives of Grouping objects:
  \describe{
    \item{factor}{
      Analogous to calling \code{split} with the factor. Returns a
      ManyToOneGrouping if there are no NAs, otherwise a
      ManyToManyGrouping. If a factor is explicitly converted to
      a ManytoOneGrouping, then any NAs are placed in the last group.
    }
    \item{vector}{
      A vector is effectively treated as a factor, but more
      efficiently. The order of the groups is not defined.
    }
    \item{FactorList}{
      Same as the factor coercion, except using the interaction of every
      factor in the list. The interaction has an NA wherever any of the
      elements has one. Every element must have the same length.
    }
    \item{DataFrame}{
      Effectively converted via a FactorList by coercing each column to a
      factor.
    }
    \item{grouping}{
      Equivalent Grouping representation of the base R
      \code{\link{grouping}} object.
    }
    \item{Hits}{
      Returns roughly the same object as \code{as(x, "List")}, except it
      is a ManyToManyGrouping, i.e., it knows the number of right nodes.
    }
  }
}

\author{Hervé Pagès, Michael Lawrence}

\seealso{
  \link{IntegerList-class},
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

## The core Grouping API:
length(h2l)
nobj(h2l)  # same as 'length(h2l)' for H2LGrouping objects
h2l[[1]]
h2l[[2]]
h2l[[3]]
h2l[[4]]
h2l[[5]]
grouplengths(h2l)  # same as 'unname(sapply(h2l, length))'
grouplengths(h2l, 5:2)
members(h2l, 5:2)  # all the members are put together and sorted
togroup(h2l)
togroup(h2l, 5:2)
togrouplength(h2l)  # same as 'grouplengths(h2l, togroup(h2l))'
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

## The core Grouping API:
length(pbe1)
nobj(pbe1)
pbe1[[1]]
pbe1[[2]]
pbe1[[3]]
grouplengths(pbe1)  # same as 'unname(sapply(pbe1, length))'
                    # and 'width(pbe1)'
togroup(pbe1)
togrouplength(pbe1)  # same as 'grouplengths(pbe1, togroup(pbe1))'
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
