\name{extractListFragments}

\alias{INCOMPATIBLE_ARANGES_MSG}
\alias{extractListFragments}
\alias{equisplit}

\title{Extract list fragments from a list-like object}

\description{
  Utilities for extracting \emph{list fragments} from a list-like object.
}

\usage{
extractListFragments(x, aranges, use.mcols=FALSE,
                     msg.if.incompatible=INCOMPATIBLE_ARANGES_MSG)

equisplit(x, nchunk, chunksize, use.mcols=FALSE)
}

\arguments{
  \item{x}{
    The list-like object from which to extract the list fragments.

    Can be any \link{List} derivative for \code{extractListFragments}.
    Can also be an ordinary list if \code{extractListFragments} is
    called with \code{use.mcols=TRUE}.

    Can be any \link{List} derivative that supports \code{relist()}
    for \code{equisplit}.
  }
  \item{aranges}{
    An \link{IntegerRanges} derivative containing the \emph{absolute ranges}
    (i.e. the ranges \emph{along \code{unlist(x)}}) of the list fragments
    to extract.

    The ranges in \code{aranges} must be compatible with the
    \emph{cumulated length} of all the list elements in \code{x},
    that is, \code{start(aranges)} and \code{end(aranges)} must
    be >= 1 and <= \code{sum(elementNROWS(x))}, respectively.

    Also please note that only \link{IntegerRanges} objects that are
    disjoint and sorted are supported at the moment.
  }
  \item{use.mcols}{
    Whether to propagate the metadata columns on \code{x} (if any) or not.

    Must be \code{TRUE} or \code{FALSE} (the default).
    If set to \code{FALSE}, instead of having the metadata columns propagated
    from \code{x}, the object returned by \code{extractListFragments} has
    metadata columns \code{revmap} and \code{revmap2}, and the object
    returned by \code{equisplit} has metadata column \code{revmap}. Note that
    this is the default.
  }
  \item{msg.if.incompatible}{
    The error message to use if \code{aranges} is not compatible with
    the \emph{cumulated length} of all the list elements in \code{x}.
  }
  \item{nchunk}{
    The number of chunks. Must be a single positive integer.
  }
  \item{chunksize}{
    The size of the chunks (last chunk might be smaller). Must be a single
    positive integer.
  }
}

\details{
  A \emph{list fragment} of list-like object \code{x} is a window in one of
  its list elements.

  \code{extractListFragments} is a low-level utility that extracts list
  fragments from list-like object \code{x} according to the absolute ranges
  in \code{aranges}.

  \code{equisplit} fragments and splits list-like object \code{x} into a
  specified number of partitions with equal (total) width. This is useful
  for instance to ensure balanced loading of workers in parallel evaluation.
  For example, if \code{x} is a \link[GenomicRanges]{GRanges} object,
  each partition is also a \link[GenomicRanges]{GRanges} object and the
  set of all partitions is returned as a \link[GenomicRanges]{GRangesList}
  object.
}

\value{
  An object of the same class as \code{x} for \code{extractListFragments}.

  An object of class \code{\link[S4Vectors]{relistToClass}(x)} for
  \code{equisplit}.
}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item \link{IRanges} and \link{IRangesList} objects.

    \item \link{Partitioning} objects.

    \item \link{IntegerList} objects.

    \item \code{\link{breakInChunks}} from breaking a vector-like object
          in chunks.

    \item \link[GenomicRanges]{GRanges} and \link[GenomicRanges]{GRangesList}
          objects defined in the \pkg{GenomicRanges} package.

    \item \link[S4Vectors]{List} objects defined in the \pkg{S4Vectors}
          package.

    \item \link{intra-range-methods} and \link{inter-range-methods}
          for intra range and inter range transformations.
  }
}

\examples{
## ---------------------------------------------------------------------
## A. extractListFragments()
## ---------------------------------------------------------------------

x <- IntegerList(a=101:109, b=5:-5)
x

aranges <- IRanges(start=c(2, 4, 8, 17, 17), end=c(3, 6, 14, 16, 19))
aranges
extractListFragments(x, aranges)

x2 <- IRanges(c(1, 101, 1001, 10001), width=c(10, 5, 0, 12),
              names=letters[1:4])
mcols(x2)$label <- LETTERS[1:4]
x2

aranges <- IRanges(start=13, end=20)
extractListFragments(x2, aranges)
extractListFragments(x2, aranges, use.mcols=TRUE)

aranges2 <- PartitioningByWidth(c(3, 9, 13, 0, 2))
extractListFragments(x2, aranges2)
extractListFragments(x2, aranges2, use.mcols=TRUE)

x2b <- as(x2, "IntegerList")
extractListFragments(x2b, aranges2)

x2c <- as.list(x2b)
extractListFragments(x2c, aranges2, use.mcols=TRUE)

## ---------------------------------------------------------------------
## B. equisplit()
## ---------------------------------------------------------------------

## equisplit() first calls breakInChunks() internally to create a
## PartitioningByWidth object that contains the absolute ranges of the
## chunks, then calls extractListFragments() on it 'x' to extract the
## fragments of 'x' that correspond to these absolute ranges. Finally
## the IRanges object returned by extractListFragments() is split into
## an IRangesList object where each list element corresponds to a chunk.
equisplit(x2, nchunk=2)
equisplit(x2, nchunk=2, use.mcols=TRUE)

equisplit(x2, chunksize=5)

library(GenomicRanges)
gr <- GRanges(c("chr1", "chr2"), IRanges(1, c(100, 1e5)))
equisplit(gr, nchunk=2)
equisplit(gr, nchunk=1000)

## ---------------------------------------------------------------------
## C. ADVANCED extractListFragments() EXAMPLES
## ---------------------------------------------------------------------

## === D1. Fragment list-like object into length 1 fragments ===

## First we construct a Partitioning object where all the partitions
## have a width of 1:
x2_cumlen <- nobj(PartitioningByWidth(x2))  # Equivalent to
                                            # length(unlist(x2)) except
                                            # that it doesn't unlist 'x2'
                                            # so is much more efficient.
aranges1 <- PartitioningByEnd(seq_len(x2_cumlen))
aranges1

## Then we use it to fragment 'x2':
extractListFragments(x2, aranges1)
extractListFragments(x2b, aranges1)
extractListFragments(x2c, aranges1, use.mcols=TRUE)

## === D2. Fragment a Partitioning object ===

partitioning2 <- PartitioningByEnd(x2b)  # same as PartitioningByEnd(x2)
extractListFragments(partitioning2, aranges2)

## Note that when the 1st arg is a Partitioning derivative, then
## swapping the 1st and 2nd elements in the call to extractListFragments()
## doesn't change the returned partitioning:
extractListFragments(aranges2, partitioning2)

## ---------------------------------------------------------------------
## D. SANITY CHECKS
## ---------------------------------------------------------------------

## If 'aranges' is 'PartitioningByEnd(x)' or 'PartitioningByWidth(x)'
## and 'x' has no zero-length list elements, then
## 'extractListFragments(x, aranges, use.mcols=TRUE)' is a no-op.
check_no_ops <- function(x) {
  aranges <- PartitioningByEnd(x)
  stopifnot(identical(
    extractListFragments(x, aranges, use.mcols=TRUE), x
  ))
  aranges <- PartitioningByWidth(x)
  stopifnot(identical(
    extractListFragments(x, aranges, use.mcols=TRUE), x
  ))
}

check_no_ops(x2[lengths(x2) != 0])
check_no_ops(x2b[lengths(x2b) != 0])
check_no_ops(x2c[lengths(x2c) != 0])
check_no_ops(gr)
}

\keyword{utilities}
