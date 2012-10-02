\name{Hits-class}
\docType{class}

\alias{Hits-class}
\alias{Hits}

% accessors
\alias{matchMatrix}
\alias{dim,Hits-method}
\alias{ranges,Hits-method}
\alias{length,Hits-method}
\alias{subjectHits}
\alias{subjectHits,Hits-method}
\alias{queryHits}
\alias{queryHits,Hits-method}
\alias{queryLength}
\alias{subjectLength}
\alias{queryLength,Hits-method}
\alias{subjectLength,Hits-method}
\alias{countQueryHits}
\alias{countQueryHits,Hits-method}
\alias{countSubjectHits}
\alias{countSubjectHits,Hits-method}
\alias{[,Hits-method}

% coercion
\alias{as.matrix,Hits-method}
\alias{as.data.frame,Hits-method}
\alias{as.table,Hits-method}
\alias{t,Hits-method}
\alias{as.list,Hits-method}
\alias{coerce,Hits,list-method}
\alias{coerce,Hits,List-method}
\alias{coerce,Hits,DataFrame-method}

% setops
\alias{match,Hits,Hits-method}
\alias{intersect,Hits,Hits-method}
\alias{setdiff,Hits,Hits-method}
\alias{union,Hits,Hits-method}

% show
\alias{show,Hits-method}

\alias{remapHits}

\title{Set of hits between 2 vector-like objects}
\description{The \code{Hits} class stores a set of "hits"
  between the elements in one vector-like object (called the "query")
  and the elements in another (called the "subject"). Currently,
  \code{Hits} are used to represent the result of a call to
  \code{\link{findOverlaps}}, though other operations producing "hits"
  are imaginable.}

\details{
  The \code{as.matrix} and \code{as.data.frame} methods coerce a \code{Hits}
  object to a two column \code{matrix} or \code{data.frame} with one row for
  each hit, where the value in the first column is the index of an element in
  the query and the value in the second column is the index of an element in
  the subject.

  The \code{as.table} method counts the number of hits for each
  query element and outputs the counts as a \code{table}.

  To transpose a \code{Hits} \code{x}, so that the subject and query
  are interchanged, call \code{t(x)}. This allows, for example, counting
  the number of hits for each subject element using \code{as.table}.

  When the Hits object is the result of a call to \code{\link{findOverlaps}},
  the actual regions of intersection between the overlapping ranges
  can be obtained with the \code{ranges} accessor.
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{Hits}
  object.
  
  \describe{
    \item{}{\code{as.matrix(x)}: Coerces \code{x} to a two
      column integer matrix, with each row representing a hit
      between a query index (first column) and subject index (second
      column).
    }
    \item{}{
      \code{as(from, "DataFrame")}: Creates a \code{DataFrame} by
      combining the result of \code{as.matrix(from)}
      with \code{mcols(from)}.
    }
    \item{}{\code{as.data.frame(x)}: Attempts to coerce the result of
      \code{as(from, "DataFrame")} to a \code{data.frame}.
    }
    \item{}{\code{as.table(x)}: counts the number of hits for each
      query element in \code{x} and outputs the counts as a \code{table}.
    }
    \item{}{\code{t(x)}: Interchange the query and subject in \code{x},
      returns a transposed \code{Hits}.}
    \item{}{\code{as.list(x)}: Returns a list with an element for each
      query, where each element contains the indices of the subjects
      that have a hit with the corresponding query.
    }
    \item{}{\code{as(x, "List")}: Like \code{as.list}, above.
    }
  }
}

\section{Extraction}{
  \describe{
    \item{}{\code{x[i]}: Extracts a subset of the hits. The index
    argument \code{i} may be \code{logical} or \code{numeric}. If
    numeric, be sure that \code{i} does not contain any duplicates,
    which would violate the set property of \code{Hits}.}
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{queryHits(x)}: Equivalent to \code{as.data.frame(x)[[1]]}.}
    \item{}{\code{subjectHits(x)}: Equivalent
      to \code{as.data.frame(x)[[2]]}.}
    \item{}{\code{countQueryHits(x)}: Counts the number of hits for
      each query, returning an integer vector.
    }
    \item{}{\code{countSubjectHits(x)}: Counts the number of hits for
      each subject, returning an integer vector.
    }
    \item{}{\code{ranges(x, query, subject)}: returns a \code{Ranges}
      holding the intersection of the ranges in the
      \code{Ranges} objects \code{query} and \code{subject}, which
      should be the same subject and query used in the call to
      \code{findOverlaps} that generated \code{x}. Eventually, we might
      store the query and subject inside \code{x}, in which case the
      arguments would be redundant.
    }
    \item{}{\code{length(x)}: get the number of hits}
    \item{}{\code{queryLength(x)}, \code{nrow(x)}: get the number of
      elements in the query}
    \item{}{\code{subjectLength(x)}, \code{ncol(x)}: get the number of
      elements in the subject}
    \item{}{
      \code{dim(x)}: get a two-element integer vector, essentially
      \code{c(nrow(x), ncol(x))}.
    }
  }
}

\section{Set Operations}{
  A \code{Hits} object is a set of hits, each from a \code{query}
  index to a \code{subject} index. The basic set operation API has been
  implemented to treat \code{Hits} in this manner. This includes
  \code{\%in\%}, \code{intersect}, \code{union}, and \code{setdiff}.
}

\section{Other operations}{
  \describe{
    \item{}{\code{queryHits(x, query.map=NULL, new.queryLength=NA,
                               subject.map=NULL, new.subjectLength=NA)}:
      Remaps the hits in \code{x} thru a "query map" and/or a "subject map"
      map. The query hits are remapped thru the "query map", which is specified
      via the \code{query.map} and \code{new.queryLength} arguments. The
      subject hits are remapped thru the "subject map", which is specified via
      the \code{subject.map} and \code{new.subjectLength} arguments.

      The "query map" is conceptually a function (in the mathematical sense)
      and is also known as the "mapping function". It must be defined on the
      1..M interval and take values in the 1..N interval, where N is
      \code{queryLength(x)} and M is the value specified by the user via the
      \code{new.queryLength} argument. Note that this mapping function doesn't
      need to be injective or surjective. Also it is not represented by an R
      function but by an integer vector of length M with no NAs. More precisely
      \code{query.map} can be NULL (identity map), or a vector of
      \code{queryLength(x)} non-NA integers that are >= 1 and
      <= \code{new.queryLength}, or a factor of length \code{queryLength(x)}
      with no NAs (a factor is treated as an integer vector, and, if missing,
      \code{new.queryLength} is taken to be its number of levels). Note that
      a factor will typically be used to represent a mapping function that is
      not injective.

      The same apply to the "subject map".

      \code{remapHits} returns a Hits object where all the query and subject
      hits (accessed with \code{queryHits} and \code{subjectHits},
      respectively) have been remapped thru the 2 specified maps. This
      remapping is actually only the 1st step of the transformation, and is
      followed by 2 additional steps: (2) the removal of duplicated hits,
      and (3) the reordering of the hits (first by query hits, then by subject
      hits). Note that if the 2 maps are injective then the remapping won't
      introduce duplicated hits, so, in that case, step (2) is a no-op (but
      is still performed). Also if the "query map" is strictly ascending and
      the "subject map" ascending then the remapping will preserve the order
      of the hits, so, in that case, step (3) is also a no-op (but is still
      performed).
    }
  }
}

\author{ Michael Lawrence }
\seealso{
\code{\link{findOverlaps}}, which generates an instance of this class.
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

hits1 <- remapHits(overlaps, subject.map=factor(c("e", "e", "d"), letters[1:5]))
hits1
hits2 <- remapHits(overlaps, subject.map=c(5, 5, 4), new.subjectLength=5)
hits2
stopifnot(identical(hits1, hits2))
}
\keyword{methods}
\keyword{classes}
