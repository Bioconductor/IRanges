\name{RangesList-class}
\docType{class}
\alias{RangesList-class}

% constructor
\alias{RangesList}

% accessors
\alias{end,RangesList-method}
\alias{width,RangesList-method}
\alias{start,RangesList-method}
\alias{space}
\alias{space,RangesList-method}
\alias{universe,RangesList-method}
\alias{universe<-,RangesList-method}
\alias{universe}
\alias{universe<-}

% subset
\alias{[,RangesList,ANY,ANY,ANY-method}

% coercion
\alias{as.data.frame,RangesList-method}
\alias{coerce,RangesList,IRangesList-method}

% operations
\alias{Ops,RangesList,ANY-method}

% show
\alias{show,RangesList-method}

% split
\alias{split,Ranges-method}

\title{List of Ranges}
\description{An extension of \code{\linkS4class{AnnotatedList}} that holds
  only \code{\linkS4class{Ranges}} instances. Useful for storing ranges
  over a set of spaces (e.g. chromosomes), each of which requires a separate
  \code{Ranges} instance. As an \code{AnnotatedList}, \code{RangesList}
  may be assigned an annotation that indicates its universe (e.g. a
  genome) in which all of its spaces exist.
}

\section{Accessors}{
  In the code snippets below, \code{x} is a \code{RangesList}
  object. 

  All of these accessors collapse over the spaces:
  \describe{
    \item{}{\code{start(x)}: Get the starts of the ranges.}
    \item{}{\code{end(x)}: Get the ends of the ranges.}
    \item{}{\code{width(x)}: Get the widths of the ranges.}
    \item{}{\code{space(x)}: Gets the spaces of the ranges as a
      character vector. This is equivalent to \code{names(x)}, except each
      name is repeated according to the length of its element.
    }
  }

  These accessors are for the \code{universe} identifier:
  \describe{
    \item{}{\code{universe(x)}: gets the name of the universe as a
      single string, if one has been specified, \code{NULL} otherwise.
    }
    \item{}{\code{universe(x) <- value}: sets the name of the universe
      to \code{value}, a single string or \code{NULL}.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{\code{RangesList(..., universe = NULL)}:
      Each \code{Ranges} in \code{...}
      becomes an element in the new \code{RangesList}, in the same
      order. This is analogous to the \code{\link{list}} constructor,
      except every argument in \code{...} must be derived from
      \code{Ranges}. The universe is specified by the \code{universe}
      parameter, which should be a single string or NULL, to leave unspecified. 
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a \code{RangesList} object.

  \describe{
    \item{}{
      \code{x[i]}: Subset \code{x} by index \code{i}, with the
      same semantics as a basic \code{\linkS4class{TypedList}}, except
      \code{i} may itself be a \code{RangesList}, in which case only the
      ranges in \code{x} that overlap with those in \code{i} are kept. See
      the \code{\link[=overlap,RangesList,RangesList-method]{overlap}}
      method for more details.
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

\section{Arithmetic Operations}{
  Any arithmetic operation, such as \code{x + y}, \code{x * y}, etc,
  where \code{x} is a \code{RangesList}, is performed identically on each
  element. Currently, \code{Ranges} supports only the \code{*} operator,
  which zooms the ranges by a numeric factor.
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

  # subset by RangesList
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(1,15,20,45), end=c(5,15,100,80))
  collection <- RangesList(one = range1, range2)
  collection[RangesList()] # empty elements
  collection[RangesList(IRanges(4, 6), IRanges(50, 70))]
  collection[RangesList(IRanges(50, 70), one=IRanges(4, 6))]

  # same as list(range1, range2)
  as.list(RangesList(range1, range2))

  # coerce to data.frame
  as.data.frame(named)

  # set the universe
  universe(named) <- "hg18"
  universe(named)
  RangesList(range1, range2, universe = "hg18")

  ## zoom in 2X
  collection * 2
}
\keyword{methods}
\keyword{classes}
