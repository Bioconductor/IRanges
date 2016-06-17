\name{CompressedList-class}
\docType{class}

\alias{class:CompressedList}
\alias{CompressedList}
\alias{CompressedList-class}

% accessors
\alias{length,CompressedList-method}
\alias{names,CompressedList-method}
\alias{names<-,CompressedList-method}
\alias{elementNROWS,CompressedList-method}
\alias{$<-,CompressedList-method}
\alias{[[<-,CompressedList-method}
\alias{is.na,CompressedList-method}

% coercion
\alias{unlist,CompressedList-method}
\alias{coerce,ANY,CompressedList-method}

% combining
\alias{c,CompressedList-method}

% looping
\alias{lapply,CompressedList-method}
\alias{endoapply,CompressedList-method}
\alias{mendoapply,CompressedList-method}
\alias{revElements,CompressedList-method}

% displaying
\alias{classNameForDisplay,CompressedList-method}

% ops
\alias{!,CompressedList-method}

% setops
\alias{match,CompressedList,vector-method}

\title{CompressedList objects}

\description{
  Like the \link[S4Vectors]{SimpleList} class defined in the \pkg{S4Vectors}
  package, the CompressedList class extends the \link[S4Vectors]{List}
  virtual class.
}

\details{
  Unlike the \link[S4Vectors]{SimpleList} class, CompressedList is virtual,
  that is, it cannot be instantiated. Many concrete (i.e. non-virtual)
  CompressedList subclasses are defined and documented in this package
  (e.g. \link{CompressedIntegerList}, \link{CompressedCharacterList},
  \link{CompressedRleList}, etc...), as well as in other packages (e.g.
  \link[GenomicRanges]{GRangesList} in the \pkg{GenomicRanges} package,
  \link[GenomicAlignments]{GAlignmentsList} in the \pkg{GenomicAlignments}
  package, etc...). It's easy for developers to extend CompressedList
  to create a new CompressedList subclass and there is generally very little
  work involved to make this new subclass fully operational.

  In a CompressedList object the list elements are concatenated together
  in a single vector-like object. The \emph{partitioning} of this single
  vector-like object (i.e. the information about where each original
  list element starts and ends) is also kept in the CompressedList object.
  This internal representation is generally more memory efficient than
  \link[S4Vectors]{SimpleList}, especially if the object has many list
  elements (e.g. thousands or millions). Also it makes it possible to
  implement many basic list operations very efficiently.

  Many objects like \link{LogicalList}, \link{IntegerList},
  \link{CharacterList}, \link{RleList}, etc... exist in 2 flavors:
  CompressedList and \link[S4Vectors]{SimpleList}. Each flavor is
  incarnated by a concrete subclass: \link{CompressedLogicalList} and
  \link{SimpleLogicalList} for virtual class \link{LogicalList},
  \link{CompressedIntegerList} and \link{SimpleIntegerList} for
  virtual class \link{IntegerList}, etc...
  It's easy to switch from one representation to the other with
  \code{as(x, "CompressedList")} and \code{as(x, "SimpleList")}.
  Also the constructor function for those virtual classes have a
  switch that lets the user choose the representation at construction
  time e.g. \code{CharacterList(..., compress=TRUE)} or
  \code{CharacterList(..., compress=FALSE)}. See below for more
  information.
}

\section{Constructor}{
  See the \link[S4Vectors]{List} man page in the \pkg{S4Vectors} package
  for a quick overview of how to construct \link{List} objects in general.

  Unlike for \link[S4Vectors]{SimpleList} objects, there is no
  \code{CompressedList} constructor function.

  However, many constructor functions for \link[S4Vectors]{List} objects
  have a switch that lets the user choose between the CompressedList and
  \link[S4Vectors]{SimpleList} representation at construction time.
  For example, a \link{CompressedCharacterList} object can be constructed
  with \code{CharacterList(..., compress=TRUE)}.
}

\section{Accessors}{
  Same as for \link[S4Vectors]{List} objects. See the \link[S4Vectors]{List}
  man page in the \pkg{S4Vectors} package for more information.
}

\section{Coercion}{
  All the coercions documented in the \link[S4Vectors]{List} man page
  apply to CompressedList objects.
}

\section{Subsetting}{
  Same as for \link[S4Vectors]{List} objects. See the \link[S4Vectors]{List}
  man page for more information.
}

\section{Looping and functional programming}{
  Same as for \link[S4Vectors]{List} objects. See
  \code{?`\link[S4Vectors]{List-utils}`} in the \pkg{S4Vectors} package
  for more information.
}

\section{Displaying}{
  When a CompressedList object is displayed, the "Compressed" prefix is
  removed from the real class name of the object.
  See \code{\link[S4Vectors]{classNameForDisplay}} in the \pkg{S4Vectors}
  package for more information about this.
}

\seealso{
  \itemize{
    \item The \link[S4Vectors]{List} class defined and documented in the
          \pkg{S4Vectors} package for the parent class.

    \item The \link[S4Vectors]{SimpleList} class defined and documented in the
          \pkg{S4Vectors} package for an alternative to CompressedList.

    \item The \link{CompressedIntegerList} class for a CompressedList
          subclass example.
  }
}

\examples{
## Displaying a CompressedList object:
x <- IntegerList(11:12, integer(0), 3:-2, compress=TRUE)
class(x)

## The "Simple" prefix is removed from the real class name of the
## object:
x

## This is controlled by internal helper classNameForDisplay():
classNameForDisplay(x)
}
\keyword{methods}
\keyword{classes}
