\name{RangesList-class}
\docType{class}
\alias{class:RangesList-class}
\alias{class:SimpleRangesList-class}
\alias{RangesList-class}
\alias{SimpleRangesList-class}
\alias{RangesList}
\alias{SimpleRangesList}

% accessors
\alias{end,RangesList-method}
\alias{end<-,RangesList-method}
\alias{width,RangesList-method}
\alias{width<-,RangesList-method}
\alias{start,RangesList-method}
\alias{start<-,RangesList-method}
\alias{space}
\alias{space,RangesList-method}
\alias{universe,RangesList-method}
\alias{universe<-,RangesList-method}
\alias{universe}
\alias{universe<-}

\alias{isNormal,RangesList-method}
\alias{whichFirstNotNormal,RangesList-method}

\alias{coerce,list,RangesList-method}
\alias{coerce,Ranges,RangesList-method}

% show
\alias{show,RangesList-method}
\alias{showAsCell,RangesList-method}

% merge
\alias{merge,RangesList,missing-method}
\alias{merge,missing,RangesList-method}
\alias{merge,RangesList,RangesList-method}

\title{List of Ranges}
\description{An extension of \linkS4class{List} that holds only
  \linkS4class{Ranges} objects. Useful for storing ranges over a set
  of spaces (e.g. chromosomes), each of which requires a separate
  \code{Ranges} object. As a \code{Vector}, \code{RangesList} may be
  annotated with its universe identifier (e.g. a genome) in which all of its
  spaces exist.
}

\section{Accessors}{
  In the code snippets below, \code{x} is a \code{RangesList}
  object. 

  All of these accessors collapse over the spaces:
  \describe{
    \item{}{\code{start(x), start(x) <- value}: Get or set the starts of the
      ranges. When setting the starts, \code{value} can be an integer vector
      of length \code{sum(elementNROWS(x))} or an IntegerList object of
      length \code{length(x)} and names \code{names(x)}.}
    \item{}{\code{end(x), end(x) <- value}: Get or set the ends of the
      ranges. When setting the ends, \code{value} can be an integer vector
      of length \code{sum(elementNROWS(x))} or an IntegerList object of
      length \code{length(x)} and names \code{names(x)}.}
    \item{}{\code{width(x), width(x) <- value}: Get or set the widths of the
      ranges. When setting the widths, \code{value} can be an integer vector
      of length \code{sum(elementNROWS(x))} or an IntegerList object of
      length \code{length(x)} and names \code{names(x)}.}
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

\section{Coercion}{
  In the code snippet below, \code{x} is a \code{RangesList} object.
  \describe{
    \item{}{
      \code{as.data.frame(x, row.names = NULL, optional = FALSE,
         ..., value.name = "value", use.outer.mcols = FALSE, 
         group_name.as.factor = FALSE)}: 
      Coerces \code{x} to a \code{data.frame}. See as.data.frame on the 
      \code{List} man page for details (?\code{List}). 
    }
  }

  In the following code snippet, \code{from} is something other than a
  \code{RangesList}:
  \describe{
    \item{}{
      \code{as(from, "RangesList")}: When \code{from} is a
      \code{Ranges}, analogous to \code{as.list} on a vector.
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
\seealso{
  \code{\linkS4class{List}}, the parent of this class, for more
  functionality.
}
\examples{
## ---------------------------------------------------------------------
## Basic manipulation
## ---------------------------------------------------------------------

range1 <- IRanges(start=c(1, 2, 3), end=c(5, 2, 8))
range2 <- IRanges(start=c(15, 45, 20, 1), end=c(15, 100, 80, 5))
named <- RangesList(one = range1, two = range2)
length(named) # 2
start(named) # same as start(c(range1, range2))
names(named) # "one" and "two"
named[[1]] # range1
unnamed <- RangesList(range1, range2)
names(unnamed) # NULL

# edit the width of the ranges in the list
edited <- named
width(edited) <- rep(c(3,2), elementNROWS(named))
edited

# same as list(range1, range2)
as.list(RangesList(range1, range2))

# coerce to data.frame
as.data.frame(named)

# set the universe
universe(named) <- "hg18"
universe(named)
RangesList(range1, range2, universe = "hg18")

## zoom in 2X
collection <- RangesList(one = range1, range2)
collection * 2
}
\keyword{methods}
\keyword{classes}
