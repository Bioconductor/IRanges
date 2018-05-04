\name{splitListElements}

\alias{splitListElements}
\alias{equisplit}

\title{Split the list elements of a list-like object}

\description{
  2 utilities for splitting the list elements of a list-like object.
}

\usage{
splitListElements(x, partitioning, use.mcols=FALSE)
equisplit(x, nchunk, chunksize, use.mcols=FALSE)
}

\arguments{
  \item{x}{
    Any \link{List} derivative for \code{splitListElements}.

    Any \link{List} derivative that supports \code{relist()}
    for \code{equisplit}.

    Note that \code{splitListElements} can also be used on an ordinary
    list if called with \code{use.mcols=TRUE}.
  }
  \item{partitioning}{
    A \link{Partitioning} derivative describing how to split the list
    elements in \code{x}. It must be compatible with the \emph{cumulated
    length} of all the list elements in \code{x}.
  }
  \item{use.mcols}{
    \code{TRUE} or \code{FALSE}.
    Whether to propagate the metadata columns on \code{x} (if any) or not.
    If set to \code{FALSE}, instead of having the metadata columns propagated
    from \code{x}, the object returned by \code{splitListElements} has
    metadata columns \code{revmap} and \code{partition} and the object
    returned by \code{equisplit} has metadata column \code{revmap}.
  }
  \item{nchunk}{
    A single positive integer. The number of chunks.
  }
  \item{chunksize}{
    A single positive integer. The size of the chunks (last chunk might be
    smaller).
  }
}

\details{
  \code{splitListElements} is a low-level utility that splits the list
  elements of list-like object \code{x} according to \code{partitioning}.

  \code{equisplit} splits list-like object \code{x} into a specified
  number of partitions with equal (total) width. This is useful for
  instance to ensure balanced loading of workers in parallel evaluation.
  For example, if \code{x} is a \link[GenomicRanges]{GRanges} object,
  each partition is also a \link[GenomicRanges]{GRanges} object and the
  set of all partitions is returned as a \link[GenomicRanges]{GRangesList}
  object.
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
## SIMPLE EXAMPLES
## ---------------------------------------------------------------------

x <- IRanges(c(1, 101, 1001, 10001), width=c(10, 5, 0, 12),
             names=letters[1:4])
mcols(x)$label <- LETTERS[1:4]
x

partitioning <- PartitioningByWidth(c(3, 9, 13, 0, 2))
splitListElements(x, partitioning)
splitListElements(x, partitioning, use.mcols=TRUE)

## equisplit() first calls breakInChunks() internally to generate the
## geometry of the chunks (returned in a PartitioningByWidth object),
## then calls splitListElements() to split the ranges. Finally the
## IRanges object returned by splitListElements() is split into an
## IRangesList object where each list element corresponds to a chunk.
equisplit(x, nchunk=2)
equisplit(x, nchunk=2, use.mcols=TRUE)

equisplit(x, chunksize=5)

x2 <- as(x, "IntegerList")
splitListElements(x2, partitioning)

x3 <- as.list(x2)
splitListElements(x3, partitioning, use.mcols=TRUE)

library(GenomicRanges)
gr <- GRanges(c("chr1", "chr2"), IRanges(1, c(100, 1e5)))
equisplit(gr, nchunk=2)
equisplit(gr, nchunk=1000)

## ---------------------------------------------------------------------
## SPLIT ALL LIST ELEMENTS INTO LENGTH-1 LIST ELEMENTS
## ---------------------------------------------------------------------

## First we need to construct a Partitioning object where all the
## partitions have a width of 1:
x_nobj <- nobj(PartitioningByWidth(x))  # Equivalent to length(unlist(x))
                                        # except that it doesn't unlist
                                        # 'x' so is much more efficient.
partitioning1 <- PartitioningByEnd(seq_len(x_nobj))
partitioning1

## Then we use it to split 'x':
splitListElements(x, partitioning1)
splitListElements(x2, partitioning1)
splitListElements(x3, partitioning1, use.mcols=TRUE)

## ---------------------------------------------------------------------
## SANITY CHECKS
## ---------------------------------------------------------------------

## If 'partitioning' is PartitioningByEnd(x) or PartitioningByWidth(x)
## then 'splitListElements(x, partitioning, use.mcols=TRUE)' is a no-op.
check_no_ops <- function(x) {
  partitioning <- PartitioningByEnd(x)
  stopifnot(identical(
    splitListElements(x, partitioning, use.mcols=TRUE),
    x
  ))
  partitioning <- PartitioningByWidth(x)
  stopifnot(identical(
    splitListElements(x, partitioning, use.mcols=TRUE),
    x
  ))
}
check_no_ops(x)
check_no_ops(x2)
check_no_ops(x3)
check_no_ops(gr)
}

\keyword{utilities}
