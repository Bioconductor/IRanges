\name{RangesList-class}
\docType{class}
\alias{RangesList-class}

% constructor
\alias{RangesList}

% accessors
\alias{end,RangesList-method}
\alias{isEmpty,RangesList-method}
\alias{width,RangesList-method}
\alias{start,RangesList-method}

% coercion
\alias{as.data.frame,RangesList-method}
\alias{coerce,RangesList,IRangesList-method}

% show
\alias{show,RangesList-method}
\alias{summary,RangesList-method}

\title{List of Ranges}
\description{An extension of \code{\linkS4class{TypedList}} that holds
  only \code{\linkS4class{Ranges}} instances. Useful for storing ranges
  over a set of spaces (e.g. chromosomes), each of which requires a separate
  \code{Ranges} instance.
}

\section{Accessors}{
  In the code snippets below, \code{x} is a \code{RangesList}
  object. All accessors collapse over the spaces.

  \describe{
    \item{}{\code{start(x)}: Get the starts of the ranges.}
    \item{}{\code{end(x)}: Get the ends of the ranges.}
    \item{}{\code{width(x)}: Get the widths of the ranges.}
    \item{}{\code{isEmpty(x)}: Gets a logical vector indicating
      which elements are empty (length zero).}
   }
}

\section{Constructor}{
  \describe{
    \item{}{\code{RangesList(...)}:
      Each \code{Ranges} in \code{...}
      becomes an element in the new \code{RangesList}, in the same
      order. This is analogous to the \code{\link{list}} constructor,
      except every argument in \code{...} must be derived from \code{Ranges}. 
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is a \code{RangesList} object.

  \describe{
    \item{}{\code{as.data.frame(x, row.names = NULL, optional = FALSE)}:
      Coerces \code{x} to a \code{data.frame}. Essentially the same as
      calling \code{as.data.frame(unlist(x))}.
    }
    \item{}{\code{as(from, "IRangesList")}: Coerces \code{from}, a
      \code{RangesList}, to an \code{\linkS4class{IRangesList}}, requiring
      that all \code{Ranges} elements are coerced to internal
      \code{IRanges} elements. This is a convenient way to ensure that all
      \code{Ranges} have been imported into R (and that there is no
      unwanted overhead when accessing them).
    }
  }
}

\author{ Michael Lawrence }
\examples{
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  named <- RangesList(one = range1, two = range2)
  length(named) # 2
  start(named) # same as start(c(range1, range2))
  names(named) # "one" and "two"
  named[[1]] # range1
  unnamed <- RangesList(range1, range2)
  names(unnamed) # NULL

  # same as list(range1, range2)
  as.list(RangesList(range1, range2))

  # coerce to data.frame
  as.data.frame(named)
}
\keyword{methods}
\keyword{classes}
