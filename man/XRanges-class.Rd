\name{XRanges-class}
\docType{class}
\alias{XRanges-class}

% accessors
\alias{end,XRanges-method}
\alias{start,XRanges-method}
\alias{width,XRanges-method}

% utilities
\alias{gaps,XRanges-method}
\alias{reduce,XRanges-method}

\title{External Ranges}
\description{The \code{XRanges} class is meant to be the virtual parent
  for all \code{\linkS4class{Ranges}} derivatives that exist externally
  from R, such as search trees, databases, etc. It is the external
  analog of the internal \code{\linkS4class{IRanges}}.}

\details{
  The primary requirement for a \code{XRanges} implementation is that it
  is coercible to \code{\linkS4class{IRanges}}, so that the data may be
  imported into R. Several of the most important accessors
  (\code{start}, \code{end}, \code{width}) and utilities (\code{reduce},
  \code{gaps}) have default implementations for \code{XRanges} instances
  that simply coerce the \code{XRanges} to an \code{IRanges} and
  delegate. Subclasses are responsible for optimized implementations of
  those methods and should generally attempt to implement as much of the
  \code{Ranges} API as is feasible.
}

\author{ Michael Lawrence }
\seealso{
  The internal \code{\linkS4class{IRanges}};
  \code{\linkS4class{IntervalTree}} for an implementation.
}

\keyword{methods}
\keyword{classes}
