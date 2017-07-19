\name{IPos-class}
\docType{class}

\alias{class:IPos}
\alias{IPos-class}
\alias{IPos}

\alias{length,IPos-method}
\alias{names,IPos-method}
\alias{names<-,IPos-method}
\alias{pos}
\alias{pos,IPos-method}
\alias{start,IPos-method}
\alias{end,IPos-method}
\alias{width,IPos-method}

\alias{coerce,Ranges,IPos-method}
\alias{as.data.frame,IPos-method}
\alias{extractROWS,IPos-method}
\alias{show,IPos-method}
\alias{c,IPos-method}

\title{IPos objects}

\description{
  The IPos class is a container for storing a set of \emph{integer positions}
  where most of the positions are typically (but not necessarily) adjacent.
  Because integer positions can be seen as ranges of width 1, the IPos
  class extends the \link{Ranges} virtual class. Note that even though an
  \link{IRanges} object can be used for storing integer positions, using an
  IPos object will be much more memory-efficient, especially when the object
  contains long runs of adjacent positions in \emph{ascending order}.
}

\usage{
IPos(pos_runs)  # constructor function
}

\arguments{
  \item{pos_runs}{
    An \link{IRanges} object (or any other \link{Ranges} derivative) where
    each range is interpreted as a run of adjacent ascending positions.
    If \code{pos_runs} is not a \link{Ranges} derivative, \code{IPos()} first
    tries to coerce it to one with \code{as(pos_runs, "Ranges", strict=FALSE)}.
  }
}

\value{
  An IPos object.
}

\section{Accessors}{
  \subsection{Getters}{
    IPos objects support the same set of getters as other \link{Ranges}
    derivatives (i.e. \code{start()}, \code{end()}, \code{mcols()}, etc...),
    plus the \code{pos()} getter which is equivalent to \code{start()}
    or \code{end()}. See \code{?Ranges} for the list of getters supported
    by \link{Ranges} derivatives.

    Note that an IPos object cannot hold names i.e. \code{names()}
    always returns \code{NULL} on it.
  }
  \subsection{Setters}{
    IPos objects support the \code{mcols()} and \code{metadata()} setters
    only.
  }
}

\section{Coercion}{
  From \link{Ranges} to IPos:
  A \link{Ranges} derivative \code{x} in which all the ranges have a
  width of 1 can be coerced to an IPos object with \code{as(x, "IPos")}.
  The names on \code{x} are not propagated (a warning is issued if \code{x}
  has names on it).

  From IPos to \link{IRanges}:
  An IPos object \code{x} can be coerced to an \link{IRanges} object
  with \code{as(x, "IRanges")}. However be aware that the resulting object
  can use thousands times (or more) memory than \code{x}!
  See "MEMORY USAGE" in the Examples section below.

  From IPos to ordinary R objects:
  Like with any other \link{Ranges} derivative, \code{as.character()},
  \code{as.factor()}, and \code{as.data.frame()} work on an IPos object
  \code{x}. Note howver that \code{as.data.frame(x)} returns a data frame
  with a \code{pos} column (containing \code{pos(x)}) instead of the
  \code{start}, \code{end}, and \code{width} columns that one gets with other
  \link{Ranges} derivatives.
}

\section{Subsetting}{
  An IPos object can be subsetted exactly like an \link{IRanges} object.
}

\section{Combining}{
  IPos objects can be combined (a.k.a. appended) with \code{c()} or
  \code{append()}.
}

\section{Splitting and Relisting}{
  Like with an \link{IRanges} object, \code{split()} and \code{relist()} work
  with an IPos object \code{x}.
}

\note{
  Like for any \link[S4Vectors]{Vector} derivative, the length of an
  IPos object cannot exceed \code{.Machine$integer.max} (i.e. 2^31 on
  most platforms). \code{IPos()} will return an error if \code{pos_runs}
  contains too many integer positions.
}

\author{
  Hervé Pagès; based on ideas borrowed from Georg Stricker
  \email{georg.stricker@in.tum.de} and Julien Gagneur
  \email{gagneur@in.tum.de}
}

\seealso{
  \itemize{
    \item The \link[GenomicRanges]{GPos} container in the \pkg{GenomicRanges}
          package for representing a set of \emph{genomic positions}.

    \item The \link{Ranges} class.

    \item \link{Ranges-comparison} for comparing and ordering integer ranges
          and/or positions.

    \item \link{findOverlaps-methods} for finding overlapping
          integer ranges and/or positions.

    \item \link{nearest-methods} for finding the nearest integer range
          and/or position.
  }
}

\examples{
## ---------------------------------------------------------------------
## BASIC EXAMPLES
## ---------------------------------------------------------------------

## Example 1:
ipos1 <- IPos(c("44-53", "5-10", "2-5"))
ipos1

length(ipos1)
pos(ipos1)  # same as 'start(ipos1)' and 'end(ipos1)'
as.character(ipos1)
as.data.frame(ipos1)
as(ipos1, "IRanges")
as.data.frame(as(ipos1, "IRanges"))
ipos1[9:17]

## Example 2:
pos_runs <- IRanges(c(1, 6, 12, 17), c(5, 10, 16, 20))
ipos2 <- IPos(pos_runs)
ipos2

## Example 3:
ipos3A <- ipos3B <- IPos(c("1-15000", "15400-88700"))
npos <- length(ipos3A)

mcols(ipos3A)$sample <- Rle("sA")
sA_counts <- sample(10, npos, replace=TRUE)
mcols(ipos3A)$counts <- sA_counts

mcols(ipos3B)$sample <- Rle("sB")
sB_counts <- sample(10, npos, replace=TRUE)
mcols(ipos3B)$counts <- sB_counts

ipos3 <- c(ipos3A, ipos3B)
ipos3

## ---------------------------------------------------------------------
## MEMORY USAGE
## ---------------------------------------------------------------------

## Coercion to IRanges works...
ipos4 <- IPos(c("1-125000", "135000-575000"))
ir4 <- as(ipos4, "IRanges")
ir4
## ... but is generally not a good idea:
object.size(ipos4)
object.size(ir4)  # 1739 times bigger than the IPos object!

## Shuffling the order of the positions impacts memory usage:
ipos4s <- sample(ipos4)
object.size(ipos4s)

## AN IMPORTANT NOTE: In the worst situations, IPos still performs as
## good as an IRanges object.
object.size(as(ipos4s, "IRanges"))  # same size as 'ipos4s'

## Best case scenario is when the object is strictly sorted (i.e.
## positions are in strict ascending order).
## This can be checked with:
is.unsorted(ipos4, strict=TRUE)  # 'ipos4' is strictly sorted

## ---------------------------------------------------------------------
## USING MEMORY-EFFICIENT METADATA COLUMNS
## ---------------------------------------------------------------------
## In order to keep memory usage as low as possible, it is recommended
## to use a memory-efficient representation of the metadata columns that
## we want to set on the object. Rle's are particularly well suited for
## this, especially if the metadata columns contain long runs of
## identical values. This is the case for example if we want to use an
## IPos object to represent the coverage of sequencing reads along a
## chromosome.

## Example 5:
library(pasillaBamSubset)
library(Rsamtools)  # for the BamFile() constructor function
bamfile1 <- BamFile(untreated1_chr4())
bamfile2 <- BamFile(untreated3_chr4())
ipos5 <- IPos(IRanges(1, seqlengths(bamfile1)[["chr4"]]))
library(GenomicAlignments)  # for "coverage" method for BamFile objects
cov1 <- coverage(bamfile1)$chr4
cov2 <- coverage(bamfile2)$chr4
mcols(ipos5) <- DataFrame(cov1, cov2)
ipos5

object.size(ipos5)  # lightweight

## Keep only the positions where coverage is at least 10 in one of the
## 2 samples:
ipos5[mcols(ipos5)$cov1 >= 10 | mcols(ipos5)$cov2 >= 10]
}
\keyword{methods}
\keyword{classes}
