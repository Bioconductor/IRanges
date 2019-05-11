\name{IntegerRangesList-class}
\docType{class}

% Classes

\alias{class:IntegerRangesList}
\alias{IntegerRangesList-class}
\alias{IntegerRangesList}

\alias{class:SimpleIntegerRangesList}
\alias{SimpleIntegerRangesList-class}
\alias{SimpleIntegerRangesList}

% Accessors
\alias{start,RangesList-method}
\alias{start<-,IntegerRangesList-method}
\alias{end,RangesList-method}
\alias{end<-,IntegerRangesList-method}
\alias{width,RangesList-method}
\alias{width<-,IntegerRangesList-method}
\alias{pos,PosList-method}
\alias{space}
\alias{space,IntegerRangesList-method}

\alias{isNormal,IntegerRangesList-method}
\alias{whichFirstNotNormal,IntegerRangesList-method}

% Show
\alias{show,IntegerRangesList-method}

% Merge
\alias{merge,IntegerRangesList,missing-method}
\alias{merge,missing,IntegerRangesList-method}
\alias{merge,IntegerRangesList,IntegerRangesList-method}

\title{IntegerRangesList objects}

\description{
  The IntegerRangesList \emph{virtual} class is a general container for
  storing a list of \link{IntegerRanges} objects.

  Most users are probably more interested in the \link{IRangesList}
  container, an IntegerRangesList derivative for storing a list of
  \link{IRanges} objects.
}

\details{
  The place of IntegerRangesList in the \emph{Vector class hierarchy}:
  \preformatted{
                               Vector
                                 ^
                                 |
                                List
                                 ^
                                 |
                             RangesList
                              ^     ^
                             /       \
                            /         \
                           /           \
                          /             \
                         /               \
                        /                 \
         IntegerRangesList               GenomicRangesList
                ^                                 ^
                |                                 |
           IRangesList                       GRangesList
             ^     ^                           ^     ^
            /       \                         /       \
           /         \                       /         \
          /           \                     /           \
   SimpleIRangesList   \             SimpleGRangesList   \
            CompressedIRangesList             CompressedGRangesList
  }
  Note that the \emph{Vector class hierarchy} has many more classes.
  In particular \link[S4Vectors]{Vector}, \link[S4Vectors]{List},
  \link[IRanges]{RangesList}, and \link[IRanges]{IntegerRangesList}
  have other subclasses not shown here.
}

\section{Accessors}{
  In the code snippets below, \code{x} is a \code{IntegerRangesList}
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
}

\section{Coercion}{
  In the code snippet below, \code{x} is an \code{IntegerRangesList} object.
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
  \code{IntegerRangesList}:
  \describe{
    \item{}{
      \code{as(from, "IntegerRangesList")}: When \code{from} is a
      \code{IntegerRanges}, analogous to \code{as.list} on a vector.
    }
  }
}

\section{Arithmetic Operations}{
  Any arithmetic operation, such as \code{x + y}, \code{x * y}, etc,
  where \code{x} is a \code{IntegerRangesList}, is performed identically on
  each element. Currently, \code{IntegerRanges} supports only the \code{*}
  operator, which zooms the ranges by a numeric factor.
}

\author{M. Lawrence & H. Pagès}

\seealso{
  \itemize{
    \item \link{IRangesList} objects.

    \item \link{IntegerRanges} and \link{IRanges} objects.
  }
}

\examples{
## ---------------------------------------------------------------------
## Basic manipulation
## ---------------------------------------------------------------------

range1 <- IRanges(start=c(1, 2, 3), end=c(5, 2, 8))
range2 <- IRanges(start=c(15, 45, 20, 1), end=c(15, 100, 80, 5))
named <- IRangesList(one = range1, two = range2)
length(named) # 2
start(named) # same as start(c(range1, range2))
names(named) # "one" and "two"
named[[1]] # range1
unnamed <- IRangesList(range1, range2)
names(unnamed) # NULL

# edit the width of the ranges in the list
edited <- named
width(edited) <- rep(c(3,2), elementNROWS(named))
edited

# same as list(range1, range2)
as.list(IRangesList(range1, range2))

# coerce to data.frame
as.data.frame(named)

IRangesList(range1, range2)

## zoom in 2X
collection <- IRangesList(one = range1, range2)
collection * 2
}
\keyword{methods}
\keyword{classes}
