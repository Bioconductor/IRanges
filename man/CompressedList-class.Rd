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
\alias{is.na,CompressedList-method}

% coercion
\alias{unlist,CompressedList-method}
\alias{coerce,ANY,CompressedList-method}

% concatenation
\alias{bindROWS,CompressedList-method}

% looping
\alias{lapply,CompressedList-method}
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

  However, many constructor functions for \link[S4Vectors]{List} derivatives
  provide the \code{compress} argument that lets the user choose between the
  CompressedList and \link[S4Vectors]{SimpleList} representations at
  construction time.
  For example, depending on whether the \code{compress} argument of the
  \code{\link{CharacterList}()} constructor is set to \code{TRUE} or
  \code{FALSE}, a \link{CompressedCharacterList} or \link{SimpleCharacterList}
  instance will be returned.

  Finally let's mention that the most efficient way to construct a CompressedList
  derivative is with
\preformatted{    relist(unlisted, partitioning)
}
  where \code{unlisted} is a vector-like object and \code{partitioning} a
  \link{PartitioningByEnd} object describing a partitioning of \code{unlisted}.
  The cost of this relist operation is virtually zero because \code{unlisted}
  and \code{partitioning} get stored \emph{as-is} in the returned object.
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
    \item \link[S4Vectors]{List} in the \pkg{S4Vectors} package for an
          introduction to List objects and their derivatives (CompressedList
          is a direct subclass of List which makes CompressedList objects
          List derivatives).

    \item The \link[S4Vectors]{SimpleList} class defined and documented in
          the \pkg{S4Vectors} package for an alternative to CompressedList.

    \item \link[IRanges]{relist} and \link[IRanges]{extractList} for
          efficiently constructing a \link{List} derivative from a
          vector-like object.

    \item The \link{CompressedNumericList} class for an example of a
          concrete CompressedList subclass.

    \item \link{PartitioningByEnd} objects. These objects are used inside
          CompressedList derivatives to keep track of the \emph{partitioning}
          of the single vector-like object made of all the list elements
          concatenated together.
  }
}

\examples{
## Fastest way to construct a CompressedList object:
unlisted <- runif(12)
partitioning <- PartitioningByEnd(c(5, 5, 10, 12), names=LETTERS[1:4])
partitioning

x1 <- relist(unlisted, partitioning)
x1

stopifnot(identical(lengths(partitioning), lengths(x1)))

## Note that the class of the CompressedList derivative returned by
## relist() is determined by relistToClass():
relistToClass(unlisted)
stopifnot(relistToClass(unlisted) == class(x1))

## Displaying a CompressedList object:
x2 <- IntegerList(11:12, integer(0), 3:-2, compress=TRUE)
class(x2)

## The "Simple" prefix is removed from the real class name of the
## object:
x2

## This is controlled by internal helper classNameForDisplay():
classNameForDisplay(x2)
classNameForDisplay(x1)
}
\keyword{methods}
\keyword{classes}
