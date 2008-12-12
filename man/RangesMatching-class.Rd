\name{RangesMatching-class}
\docType{class}
\alias{RangesMatching-class}

% accessors
\alias{matchMatrix}
\alias{matchMatrix,RangesMatching-method}
\alias{dim,RangesMatching-method}
\alias{ranges,RangesMatching-method}

% coercion
\alias{as.matrix,RangesMatching-method}
\alias{as.table,RangesMatching-method}
\alias{t,RangesMatching-method}

\title{Matchings between Ranges}
\description{The \code{RangesMatching} class stores a set of matchings
  between the ranges in one \code{\linkS4class{Ranges}} instance and the
  ranges in another. Currently, \code{RangesMatching} are used to represent the
  result of an \code{\link{overlap}} query, though other matching
  operations are imaginable.}

\details{
  The matchings between the ranges are stored as a
  \code{\link[Matrix]{Matrix-class}} instance. While that structure is
  accessible, it is usually more convenient to coerce the
  \code{RangesMatching} instance to a more amenable representation.

  The \code{as.matrix} method coerces a \code{RangesMatching} to a two
  column \code{matrix} with one row for each matching, where the value
  in the first column is the index of a range in the first (query)
  \code{Ranges} and the index of the matched subject range is in the
  second column.

  The \code{as.table} method counts the number of matchings for each
  query range and outputs the counts as a \code{table}.

  To transpose a \code{RangesMatching} \code{x}, so that the subject and query
  are interchanged, call \code{t(x)}. This allows, for example, counting
  the number of subjects that matched using \code{as.table}.

  To get the actual regions of intersection between the overlapping
  ranges, use the \code{ranges} accessor.
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{RangesMatching}
  object.
  
  \describe{
    \item{}{\code{as.matrix(x)}: Coerces \code{x} to a two
      column integer matrix, with each row representing a matching
      between a query index (first column) and subject index (second
      column).
    }
    \item{}{\code{as.table(x)}: counts the number of matchings for each
      query range in \code{x} and outputs the counts as a \code{table}.
    }
    \item{}{\code{t(x)}: Interchange the query and subject in \code{x},
      returns a transposed \code{RangesMatching}.}
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{matchMatrix(x)}: Get the \code{\link[Matrix]{Matrix-class}},
      which may be a dense logical matrix
      (\code{\link[Matrix]{lgeMatrix-class}}) or sparse non-zero pattern matrix
      (\code{\link[Matrix]{ngCMatrix-class}}), that encodes the matchings, with
      columns corresponding to query ranges and rows corresponding to
      subject ranges. It is not recommended to work with this matrix
      directly, unless the coercion methods above are inadequate.
    }
    \item{}{\code{ranges(x, query, subject)}: returns a \code{Ranges}
      holding the intersection of the ranges in the
      \code{Ranges} instances \code{query} and \code{subject}, which
      should be the same subject and query used to generate
      \code{x}. Eventually, we might store the query and subject inside
      \code{x}, in which case the arguments would be redundant.
    }
    \item{}{\code{nrow(x)}: get the number of subjects in the matching}
    \item{}{\code{ncol(x)}: get the number of queries in the matching}
    \item{}{
      \code{dim(x)}: get a two-element integer vector, essentially
      \code{c(nrow(x), ncol(x))}.
    }
  }
}

\author{ Michael Lawrence }
\seealso{
\code{\link{overlap}}, which generates an instance of this class.
}
\examples{
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  matchings <- overlap(tree, query)

  as.matrix(matchings)

  as.table(matchings) # hits per query
  as.table(t(matchings)) # hits per subject
}
\keyword{methods}
\keyword{classes}
