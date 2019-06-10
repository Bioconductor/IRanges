\name{IPos-class}
\docType{class}

% Classes

\alias{class:IPos}
\alias{IPos-class}
\alias{IPos}

\alias{parallelSlotNames,IPos-method}

\alias{class:UnstitchedIPos}
\alias{UnstitchedIPos-class}
\alias{UnstitchedIPos}

\alias{parallelSlotNames,UnstitchedIPos-method}

\alias{class:StitchedIPos}
\alias{StitchedIPos-class}
\alias{StitchedIPos}

\alias{updateObject,IPos-method}

% Accessors

\alias{pos}
\alias{pos,UnstitchedIPos-method}
\alias{pos,IPos-method}
\alias{length,UnstitchedIPos-method}
\alias{length,IPos-method}
\alias{names,IPos-method}
\alias{names<-,IPos-method}

% Coercion

\alias{coerce,UnstitchedIPos,StitchedIPos-method}
\alias{coerce,StitchedIPos,UnstitchedIPos-method}
\alias{coerce,IntegerRanges,UnstitchedIPos-method}
\alias{coerce,IntegerRanges,StitchedIPos-method}
\alias{coerce,IntegerRanges,IPos-method}
\alias{coerce,ANY,UnstitchedIPos-method}
\alias{coerce,ANY,StitchedIPos-method}
\alias{coerce,ANY,IPos-method}
\alias{as.data.frame.IPos}
\alias{as.data.frame,IPos-method}

% Other

\alias{extractROWS,StitchedIPos-method}
\alias{show,IPos-method}
\alias{bindROWS,StitchedIPos-method}

\title{IPos objects}

\description{
  The IPos class is a container for storing a set of \emph{integer positions}.
  It exists in 2 flavors: UnstitchedIPos and StitchedIPos. Each flavor uses
  a particular internal representation:
  \itemize{
    \item In an UnstitchedIPos instance the positions are stored as an
          integer vector.
    \item In a StitchedIPos instance the positions are stored as an
          \link{IRanges} object where each range represents a run of
          \emph{consecutive positions} (i.e. a run of positions that
          are adjacent and in \emph{ascending order}). This storage is
          particularly memory-efficient when the vector of positions
          contains long runs of consecutive positions.
  }
  Because integer positions can be seen as integer ranges of width 1, the
  IPos class extends the \link{IntegerRanges} virtual class.
}

\usage{
IPos(pos=integer(0), stitch=NA)  # constructor function
}

\arguments{
  \item{pos}{
    An integer or numeric vector, or an \link{IRanges} object (or other
    \link{IntegerRanges} derivative). If \code{pos} is anything else,
    \code{IPos()} will first try to coerce it to an IRanges object with
    \code{as(pos, "IRanges")}.

    When \code{pos} is an \link{IRanges} object (or other \link{IntegerRanges}
    derivative), each range in it is interpreted as a run of
    consecutive positions.
  }
  \item{stitch}{
    \code{TRUE}, \code{FALSE}, or \code{NA} (the default).

    Controls which internal representation should be used: StitchedIPos
    (when \code{stitch} is \code{TRUE}) or UnstitchedIPos (when \code{stitch}
    is \code{FALSE}).

    When \code{stitch} is \code{NA} (the default), which internal
    representation will be used depends on the type of \code{pos}:
    UnstitchedIPos if \code{pos} is an integer or numeric vector,
    and StitchedIPos otherwise.
  }
}

\details{
  Even though an \link{IRanges} object can be used for storing integer
  positions, using an IPos object is more efficient. In particular the
  memory footprint of an UnstitchedIPos object is half that of an
  \link{IRanges} object.

  OTOH the memory footprint of a StitchedIPos object can vary a lot but
  will never be worse than that of an \link{IRanges} object. However it
  will reduce dramatically if the vector of positions contains long runs
  of consecutive positions. In the worst case scenario (i.e. when the
  object contains no consecutive positions) its memory footprint will
  be the same as that of an \link{IRanges} object.

  Like for any \link[S4Vectors]{Vector} derivative, the length of an
  IPos object cannot exceed \code{.Machine$integer.max} (i.e. 2^31 on
  most platforms). \code{IPos()} will return an error if \code{pos}
  contains too many positions.
}

\value{
  An UnstitchedIPos or StitchedIPos object. If the input object \code{pos}
  is itself an IPos derivative, its metadata columns are propagated.
}

\section{Accessors}{

  \subsection{Getters}{
    IPos objects support the same set of getters as other \link{IntegerRanges}
    derivatives (i.e. \code{length()}, \code{start()}, \code{end()},
    \code{names()}, \code{mcols()}, etc...), plus the \code{pos()} getter
    which is equivalent to \code{start()} and \code{end()}.
    See \code{?\link{IntegerRanges}} for the list of getters supported by
    \link{IntegerRanges} derivatives.
  }

  \subsection{Setters}{
    IPos derivatives support the \code{names()}, \code{mcols()} and
    \code{metadata()} setters only.

    In particular there is no \code{pos()} setter for IPos derivatives
    at the moment (although one might be added in the future).
  }
}

\section{Coercion}{
  From UnstitchedIPos to StitchedIPos and vice-versa: coercion back and
  forth between UnstitchedIPos and StitchedIPos is supported via
  \code{as(x, "StitchedIPos")} and \code{as(x, "UnstitchedIPos")}.
  This is the most efficient and recommended way to switch between the
  2 internal representations. Note that this switch can have dramatic
  consequences on memory usage so is for advanced users only.
  End users should almost never need to do this switch when following
  a typical workflow.

  From \link{IntegerRanges} to UnstitchedIPos, StitchedIPos, or IPos:
  An \link{IntegerRanges} derivative \code{x} in which all the ranges have
  a width of 1 can be coerced to an UnstitchedIPos or StitchedIPos object
  with \code{as(x, "UnstitchedIPos")} or \code{as(x, "StitchedIPos")},
  respectively. For convenience \code{as(x, "IPos")} is supported and is
  equivalent to \code{as(x, "UnstitchedIPos")}.

  From IPos to \link{IRanges}:
  An IPos derivative \code{x} can be coerced to an \link{IRanges} object
  with \code{as(x, "IRanges")}. However be aware that if \code{x} is a
  StitchedIPos instance, the memory footprint of the resulting object
  can be thousands times (or more) than that of \code{x}!
  See "MEMORY USAGE" in the Examples section below.

  From IPos to ordinary R objects:
  Like with any other \link{IntegerRanges} derivative, \code{as.character()},
  \code{as.factor()}, and \code{as.data.frame()} work on an IPos derivative
  \code{x}. Note however that \code{as.data.frame(x)} returns a data frame
  with a \code{pos} column (containing \code{pos(x)}) instead of the
  \code{start}, \code{end}, and \code{width} columns that one gets with other
  \link{IntegerRanges} derivatives.
}

\section{Subsetting}{
  An IPos derivative can be subsetted exactly like an \link{IRanges} object.
}

\section{Concatenation}{
  IPos derivatives can be concatenated with \code{c()} or \code{append()}.
  See \code{?\link[S4Vectors]{c}} in the \pkg{S4Vectors} package for
  more information about concatenating Vector derivatives.
}

\section{Splitting and Relisting}{
  Like with an \link{IRanges} object, \code{split()} and \code{relist()} work
  on an IPos derivative.
}

\author{
  Hervé Pagès; based on ideas borrowed from Georg Stricker
  \email{georg.stricker@in.tum.de} and Julien Gagneur
  \email{gagneur@in.tum.de}
}

\seealso{
  \itemize{
    \item The \link[GenomicRanges]{GPos} class in the \pkg{GenomicRanges}
          package for representing a set of \emph{genomic positions} (i.e.
          genomic ranges of width 1, a.k.a. \emph{genomic loci}).

    \item The \link{IRanges} class for storing a set of \emph{integer ranges}
          of arbitrary width.

    \item \link{IPosRanges-comparison} for comparing and ordering integer
          ranges and/or positions.

    \item \link{findOverlaps-methods} for finding overlapping
          integer ranges and/or positions.

    \item \link{intra-range-methods} and \link{inter-range-methods}
          for \emph{intra range} and \emph{inter range} transformations.

    \item \link{coverage-methods} for computing the coverage
          of a set of ranges and/or positions.

    \item \link{nearest-methods} for finding the nearest integer
          range/position neighbor.
  }
}

\examples{
showClass("IPos")  # shows the known subclasses

## ---------------------------------------------------------------------
## BASIC EXAMPLES
## ---------------------------------------------------------------------

## Example 1:
ipos1a <- IPos(c(44:53, 5:10, 2:5))
ipos1a  # unstitched

length(ipos1a)
pos(ipos1a)  # same as 'start(ipos1a)' and 'end(ipos1a)'
as.character(ipos1a)
as.data.frame(ipos1a)
as(ipos1a, "IRanges")
as.data.frame(as(ipos1a, "IRanges"))
ipos1a[9:17]

ipos1b <- IPos(c(44:53, 5:10, 2:5), stitch=TRUE)
ipos1b  # stitched

## 'ipos1a' and 'ipos1b' are semantically equivalent, only their
## internal representations differ:
all(ipos1a == ipos1b)

ipos1c <- IPos(c("44-53", "5-10", "2-5"))
ipos1c  # stitched

identical(ipos1b, ipos1c)

## Example 2:
my_pos <- IRanges(c(1, 6, 12, 17), c(5, 10, 16, 20))
ipos2 <- IPos(my_pos)
ipos2  # stitched

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

## Coercion to IRanges works on a StitchedIPos object...
ipos4 <- IPos(c("1-125000", "135000-575000"))
ir4 <- as(ipos4, "IRanges")
ir4
## ... but is generally not a good idea:
object.size(ipos4)
object.size(ir4)  # 1652 times bigger than the StitchedIPos object!

## Shuffling the order of the positions impacts memory usage:
ipos4r <- rev(ipos4)
object.size(ipos4r)
ipos4s <- sample(ipos4)
object.size(ipos4s)

## If one anticipates a lot of shuffling of the positions,
## then an UnstitchedIPos object should be used instead:
ipos4b <- as(ipos4, "UnstitchedIPos")
object.size(ipos4b)  # initial size is bigger than stitched version
object.size(rev(ipos4b))  # size didn't change
object.size(sample(ipos4b))  # size didn't change

## AN IMPORTANT NOTE: In the worst situations, IPos still performs
## as good as an IRanges object.
object.size(as(ipos4r, "IRanges"))  # same size as 'ipos4r'
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
cvg1 <- coverage(bamfile1)$chr4
cvg2 <- coverage(bamfile2)$chr4
mcols(ipos5) <- DataFrame(cvg1, cvg2)
ipos5

object.size(ipos5)  # lightweight

## Keep only the positions where coverage is at least 10 in one of the
## 2 samples:
ipos5[mcols(ipos5)$cvg1 >= 10 | mcols(ipos5)$cvg2 >= 10]
}
\keyword{methods}
\keyword{classes}
