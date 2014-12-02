\name{Hits-class-leftovers}
\docType{class}

\alias{Hits-examples}

% coercion
\alias{as.data.frame.Hits}
\alias{as.data.frame,Hits-method}
\alias{as.list,Hits-method}
\alias{as.list.Hits}
\alias{coerce,Hits,list-method}
\alias{coerce,Hits,List-method}
\alias{coerce,Hits,DataFrame-method}


\title{Examples of basic manipulation of Hits objects}

\description{
  IMPORTANT NOTE - 4/29/2014: This man page is being refactored. Most of
  the things that used to be documented here have been moved to the man
  page for \link[S4Vectors]{Hits} objects located in the \pkg{S4Vectors}
  package.
}

\details{
  The \code{as.data.frame} method coerces a \code{Hits} object to a two column
  \code{data.frame} with one row for each hit, where the value in the first
  column is the index of an element in the query and the value in the second
  column is the index of an element in the subject.
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{Hits}
  object.
  
  \describe{
    \item{}{
      \code{as(from, "DataFrame")}: Creates a \code{DataFrame} by
      combining the result of \code{as.matrix(from)}
      with \code{mcols(from)}.
    }
    \item{}{\code{as.data.frame(x)}: Attempts to coerce the result of
      \code{as(from, "DataFrame")} to a \code{data.frame}.
    }
    \item{}{\code{as(x, "List")}: Like \code{as.list}, above.
    }
  }
}

\seealso{
  The \link[S4Vectors]{Hits} class defined and documented in the
  \pkg{S4Vectors} package.
}
\examples{
query <- IRanges(c(1, 4, 9), c(5, 7, 10))
subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
tree <- IntervalTree(subject)
overlaps <- findOverlaps(query, tree)

as.matrix(overlaps)
as.data.frame(overlaps)

as.table(overlaps) # hits per query
as.table(t(overlaps)) # hits per subject

## Turn a Hits object into an IntegerList object with one list element
## per element in the original query.
as(overlaps, "IntegerList")
as(overlaps, "List")  # same as as(overlaps, "IntegerList")

## Turn a Hits object into a PartitioningByEnd object that describes
## the grouping of hits by query.
as(overlaps, "PartitioningByEnd")
as(overlaps, "Partitioning")  # same as as(overlaps, "PartitioningByEnd")

## ---------------------------------------------------------------------
## remapHits()
## ---------------------------------------------------------------------

hits1 <- remapHits(overlaps, subject.map=factor(c("e", "e", "d"), letters[1:5]))
hits1
hits2 <- remapHits(overlaps, subject.map=c(5, 5, 4), new.subjectLength=5)
hits2
stopifnot(identical(hits1, hits2))
}
\keyword{methods}
\keyword{classes}
