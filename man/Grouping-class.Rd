\name{Grouping-class}
\docType{class}

% Grouping objects:
\alias{class:Grouping}
\alias{Grouping-class}
\alias{Grouping}

\alias{nobj}
\alias{togroup}
\alias{grouplength}
\alias{grouplength,Grouping-method}
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
\alias{togroup,H2LGrouping-method}
\alias{grouplength,H2LGrouping-method}

\alias{class:Dups}
\alias{Dups-class}
\alias{Dups}

\alias{duplicated,Dups-method}
\alias{show,Dups-method}

% Partitioning objects:
\alias{class:Partitioning}
\alias{Partitioning-class}
\alias{Partitioning}

\alias{end,Partitioning-method}
\alias{length,Partitioning-method}
\alias{nobj,Partitioning-method}
\alias{start,Partitioning-method}
\alias{width,Partitioning-method}
\alias{[[,Partitioning-method}
\alias{togroup,Partitioning-method}
\alias{grouplength,Partitioning-method}
\alias{names,Partitioning-method}
\alias{names<-,Partitioning-method}

\alias{coerce,Ranges,Partitioning-method}


\title{Grouping objects}

\description{
  In this man page, we call "grouping" the action of dividing a collection of
  NO objects into NG groups (some of them eventually empty). The Grouping class
  and subclasses are containers for representing groupings.
}

\section{The Grouping core API}{
  Let's give a formal description of the Grouping core API:

  Groups G_i are indexed from 1 to NG (1 <= i <= NG).

  Object O_j are indexed from 1 to NO (1 <= i <= NO).

  Every object must belong to one group and only one.

  Given that empty groups are allowed, NG can be greater than NO.

  Grouping an empty collection of objects (NO = 0) is supported. In that
  case, all the groups are empty. And only in that case, NG can be zero
  too (no group).

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
    \item{}{
      \code{x[[i]]}:
      Returns the indices of the objects (the j's) that belong to G_i.
      The j's are returned in ascending order. This provides the mapping from
      groups to objects (one-to-many mapping).
    }
    \item{}{
      \code{togroup(x, j=NULL)}:
      Returns the index i of the group that O_j belongs to. This provides the
      mapping from objects to groups (many-to-one mapping). Expected to work
      in a vectorized fashion. \code{togroup(x)} is equivalent to
      \code{togroup(x, seq_len(nobj(x)))}: both return the entire mapping in
      an integer vector of length NO. In fact \code{togroup(x, j)} is
      equivalent to \code{y <- togroup(x); y[j]}.
    }
    \item{}{
      \code{grouplength(x, i=NULL)}:
      Returns the number of objects in G_i. Expected to work in a vectorized
      fashion (unlike \code{x[[i]]}). \code{grouplength(x)} is equivalent to
      \code{grouplength(x, seq_len(length(x)))}. If \code{i} is not NULL,
      \code{grouplength(x, i)} is equivalent to
      \code{sapply(i, function(ii) length(x[[i]]))}.
    }
    \item{}{
      \code{togrouplength(x, j=NULL)}:
      Returns the nb of objects that belong to the same group as O_j
      (including O_j itself). Equivalent to \code{grouplength(x, togroup(x, j))}.
    }
  }
}

\section{The H2LGrouping and Dups subclasses}{
  [DOCUMENT ME]
}

\section{The Partitioning subclass}{
  The Partitioning container represents block-groupings i.e. groupings
  where each group contains objects that are contiguous in the original
  collection of objects. In other words, \code{x} is a Partitioning object
  iff \code{togroup(x)} is sorted. In addition, a Partitioning object can
  be seen (and manipulated) as a \link{Ranges} object where all the ranges
  are adjacent starting at 1 (i.e. it covers the 1:NO interval with no
  overlap between the ranges).

  Note that a Partitioning object is both: a particular type of Grouping
  object and a particular type of \link{Ranges} object. Therefore all the
  methods that are defined for Grouping and \link{Ranges} objects can also
  be used on a Partitioning object. See \code{?Ranges} for a description of
  the \link{Ranges} API.
}

\section{Constructors}{
  \describe{
    \item{}{
      \code{H2LGrouping(high2low)}:
      [DOCUMENT ME]
    }
    \item{}{
      \code{Partitioning(end=integer(), names=NULL)}:
      Return the Partitioning object made of the partitions ending
      at the values specified by \code{end}. \code{end} must contain
      sorted non-negative integer values.
      Note that the \code{names} argument is never recycled (to remain
      consistent with what \code{`names<-`} does on standard vectors).
    }
  }
}

\author{H. Pages}

\seealso{
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
  ## TODO: Put some examples here.

  ## ---------------------------------------------------------------------
  ## B. Partitioning OBJECTS
  ## ---------------------------------------------------------------------
  x <- Partitioning(end=c(4, 7, 7, 8, 15))
  x  # the 3rd partition is empty
  names(x)[3] <- "empty partition" 
  as(x, "IRanges")


  Partitioning(end=c(0, 0, 19), names=LETTERS[1:3])
  Partitioning()  # no partition
  Partitioning(end=integer(9))  # all partitions are empty

  ## ---------------------------------------------------------------------
  ## C. RELATIONSHIP BETWEEN Partitioning OBJECTS AND successiveIRanges()
  ## ---------------------------------------------------------------------
  mywidths <- c(4, 3, 0, 1, 7)

  ## The two following calls produce the same ranges:
  x1 <- successiveIRanges(mywidths)  # IRanges instance.
  x2 <- Partitioning(end=cumsum(mywidths))  # Partitioning instance.
  stopifnot(identical(as(x1, "Partitioning"), x2))
}

\keyword{methods}
\keyword{classes}
