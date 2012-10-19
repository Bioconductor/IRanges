\name{Vector-class}
\docType{class}

% Vector class, functions and methods:
\alias{class:Vector}
\alias{Vector-class}
\alias{Vector}

\alias{NROW,Vector-method}
\alias{nlevels,Vector-method}
\alias{elementMetadata}
\alias{elementMetadata,Vector-method}
\alias{mcols}
\alias{mcols,Vector-method}
\alias{values}
\alias{values,Vector-method}
\alias{elementMetadata<-}
\alias{elementMetadata<-,Vector-method}
\alias{mcols<-}
\alias{mcols<-,Vector-method}
\alias{values<-}
\alias{values<-,Vector-method}

\alias{[,Vector-method}
\alias{[<-,Vector-method}
\alias{window,Vector-method}
\alias{window,NULL-method}
\alias{window,vector-method}
\alias{window,factor-method}
\alias{window<-,Vector-method}
\alias{window<-,vector-method}
\alias{window<-,factor-method}
\alias{seqselect}
\alias{seqselect,Vector-method}
\alias{seqselect,NULL-method}
\alias{seqselect,ANY-method}
\alias{seqselect,vector-method}
\alias{seqselect,factor-method}
\alias{seqselect<-}
\alias{seqselect<-,Vector-method}
\alias{seqselect<-,vector-method}
\alias{seqselect<-,factor-method}

\alias{head,Vector-method}
\alias{tail,Vector-method}
\alias{rev,Vector-method}
\alias{rep,Vector-method}
\alias{rep.int,Vector-method}
\alias{subset,Vector-method}

\alias{!=,Vector,Vector-method}
\alias{!=,Vector,ANY-method}
\alias{!=,ANY,Vector-method}
\alias{>=,Vector,Vector-method}
\alias{>=,Vector,ANY-method}
\alias{>=,ANY,Vector-method}
\alias{<,Vector,Vector-method}
\alias{<,Vector,ANY-method}
\alias{<,ANY,Vector-method}
\alias{>,Vector,Vector-method}
\alias{>,Vector,ANY-method}
\alias{>,ANY,Vector-method}

\alias{c,Vector-method}
\alias{append,Vector,Vector-method}

\alias{split,Vector-method}
\alias{mstack}
\alias{mstack,Vector-method}
\alias{mstack,vector-method}
\alias{split<-,Vector-method}

\alias{tapply,Vector-method}
\alias{shiftApply}
\alias{shiftApply,Vector,Vector-method}
\alias{shiftApply,vector,vector-method}
\alias{aggregate,Vector-method}
\alias{aggregate,vector-method}
\alias{aggregate,matrix-method}
\alias{aggregate,data.frame-method}
\alias{aggregate,ts-method}

\alias{rename}
\alias{rename,Vector-method}
\alias{rename,vector-method}

\alias{unique,Vector-method}
\alias{sort,Vector-method}
\alias{\%in\%,ANY,Vector-method}
\alias{\%in\%,Vector,ANY-method}
\alias{\%in\%,Vector,Vector-method}

\alias{as.logical,Vector-method}
\alias{as.integer,Vector-method}
\alias{as.numeric,Vector-method}
\alias{as.double,Vector-method}
\alias{as.complex,Vector-method}
\alias{as.character,Vector-method}
\alias{as.raw,Vector-method}
\alias{coerce,Vector,vector-method}
\alias{coerce,Vector,logical-method}
\alias{coerce,Vector,integer-method}
\alias{coerce,Vector,numeric-method}
\alias{coerce,Vector,double-method}
\alias{coerce,Vector,complex-method}
\alias{coerce,Vector,character-method}
\alias{coerce,Vector,raw-method}

\alias{coerce,Vector,data.frame-method}
\alias{as.data.frame,Vector-method}

\title{Vector objects}

\description{
  The Vector virtual class serves as the heart of the IRanges package
  and has over 90 subclasses. It serves a similar role as \link[base]{vector}
  in base R.

  The Vector class supports the storage of \emph{global} and
  \emph{element-wise} metadata:
  \enumerate{
  \item The \emph{global} metadata annotates the object as a whole:
        this metadata is accessed via the \code{metadata} accessor and
        is represented as an ordinary list;
  \item The \emph{element-wise} metadata annotates individual elements
        of the object: this metadata is accessed via the \code{mcols}
        accessor (\code{mcols} stands for \emph{metadata columns}) and
        is represented as a \link{DataTable} object (i.e. as an instance
        of a concrete subclass of \link{DataTable}, e.g. a \link{DataFrame}
        object), with a row for each element and a column for each
        metadata variable. Note that the element-wise metadata can also
        be \code{NULL}.
  }

  To be functional, a class that inherits from Vector must define at
  least a \code{length}, \code{names} and \code{"["} method.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a Vector object.
  
  \describe{
    \item{}{
      \code{length(x)}:
      Get the number of elements in \code{x}.
    }
    \item{}{
      \code{NROW(x)}:
      Defined as \code{length(x)} for any Vector object that is
      \emph{not} a \linkS4class{DataTable} object.
      If \code{x} is a \linkS4class{DataTable} object, then it's
      defined as \code{nrow(x)}.
    }
    \item{}{
      \code{names(x)}, \code{names(x) <- value}:
      Get or set the names of the elements in the Vector.
    }
    \item{}{
      \code{rename(x, value, ...)}:
      Replace the names of \code{x} according to a mapping defined by a named
      character vector, formed by concatenating \code{value} with any
      arguments in \code{...}. The names of the character vector
      indicate the source names, and the corresponding values the
      destination names. This also works on a plain old \code{vector}.
    }
    \item{}{
      \code{nlevels(x)}:
      Returns the number of factor levels.
    }
    \item{}{
      \code{mcols(x, use.names=FALSE)}, \code{mcols(x) <- value}:
      Get or set the metadata columns.
      If \code{use.names=TRUE} and the metadata columns are not \code{NULL},
      then the names of \code{x} are propagated as the row names of the
      returned \link{DataTable} object.
      When setting the metadata columns, the supplied value must be \code{NULL}
      or a \link{DataTable} object holding element-wise metadata.
    }
    \item{}{
       \code{elementMetadata(x, use.names=FALSE)},
       \code{elementMetadata(x) <- value},
       \code{values(x, use.names=FALSE)},
       \code{values(x) <- value}:
       Alternatives to \code{mcols} functions. Their use is discouraged.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a Vector object or regular R vector
  object. The R vector object methods for \code{window} and \code{seqselect}
  are defined in this package and the remaining methods are defined in base R.

  \describe{
    \item{}{
      \code{x[i, drop=TRUE]}:
      If defined, returns a new Vector object made of selected elements
      \code{i}, which can be missing; an NA-free logical, numeric, or
      character vector; or a logical Rle object. The \code{drop} argument
      specifies whether or not to coerce the returned sequence to a standard
      vector.
    }
    \item{}{
      \code{x[i] <- value}:
      Equivalent to \code{seqselect(x, i) <- value}.
    }
    \item{}{
      \code{window(x, start = NA, end = NA, width = NA, frequency = NULL, delta = NULL, ...)}:
      Extract the subsequence window from the Vector object using:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
      }
      In general, this is more efficient than using \code{"["} operator.
    }
    \item{}{
      \code{window(x, start = NA, end = NA, width = NA, keepLength = TRUE) <- value}:
      Replace the subsequence window specified on the left (i.e. the subsequence
      in \code{x} specified by \code{start}, \code{end} and \code{width})
      by \code{value}.
      \code{value} must either be of class \code{class(x)}, belong to a subclass
      of \code{class(x)}, be coercible to \code{class(x)}, or be \code{NULL}.
      If \code{keepLength} is \code{TRUE}, the elements of \code{value} are
      repeated to create a Vector with the same number of elements as the
      width of the subsequence window it is replacing.
      If \code{keepLength} is \code{FALSE}, this replacement method can modify
      the length of \code{x}, depending on how the length of the left
      subsequence window compares to the length of \code{value}.
    }
    \item{}{
      \code{seqselect(x, start=NULL, end=NULL, width=NULL)}:
      Similar to \code{window}, except that multiple consecutive subsequences
      can be requested for concatenation. As such two of the three \code{start},
      \code{end}, and \code{width} arguments can be used to specify the
      consecutive subsequences. Alternatively, \code{start} can take a Ranges
      object or something that can be converted to a Ranges object like an
      integer vector, logical vector or logical Rle. If the concatenation of
      the consecutive subsequences is undesirable, consider using
      \code{\link{Views}}.
    }
    \item{}{
      \code{seqselect(x, start=NULL, end=NULL, width=NULL) <- value}:
      Similar to \code{window<-}, except that multiple consecutive subsequences
      can be replaced by a \code{value} whose length is a divisor of the number
      of elements it is replacing. As such two of the three \code{start},
      \code{end}, and \code{width} arguments can be used to specify the
      consecutive subsequences. Alternatively, \code{start} can take a Ranges
      object or something that can be converted to a Ranges object like an
      integer vector, logical vector or logical Rle.
    }
    \item{}{
      \code{split(x, f, drop = FALSE) <- value}: Virtually
      splits \code{x} by the factor \code{f}, replaces the elements of
      the resulting list with the elements from the list \code{value},
      and restores \code{x} to its original form. Note that this works
      for any \code{Vector}, even though \code{split} itself is
      not universally supported.
    }
    \item{}{
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n elements of the Vector
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} elements
      of the Vector object.
    }
    \item{}{
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n elements of the Vector
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} elements
      of the Vector object.
    }
    \item{}{
      \code{rev(x)}:
      Return a new Vector object made of the original elements in the reverse
      order.
    }
    \item{}{
      \code{rep(x, times, length.out, each)}, \code{rep.int(x, times)}:
      Repeats the values in \code{x} through one of the following conventions:
      \describe{
        \item{\code{times}}{Vector giving the number of times to repeat each
          element if of length \code{length(x)}, or to repeat the whole vector
          if of length 1.}
        \item{\code{length.out}}{Non-negative integer. The desired length of
          the output vector.}
        \item{\code{each}}{Non-negative integer.  Each element of \code{x} is
          repeated \code{each} times.}
      }
    }
    \item{}{
      \code{subset(x, subset)}:
      Return a new Vector object made of the subset using logical vector
      \code{subset}, where missing values are taken as FALSE.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a Vector object.

  \describe{
    \item{}{
      \code{c(x, ...)}:
      Combine \code{x} and the Vector objects in \code{...} together.
      Any object in \code{...} must belong to the same class as \code{x},
      or to one of its subclasses, or must be \code{NULL}.
      The result is an object of the same class as \code{x}.
    }
    \item{}{\code{append(x, values, after = length(x))}: Insert the
      \code{Vector} \code{values} onto \code{x} at the position given by
      \code{after}. \code{values} must have an \code{elementType} that extends
      that of \code{x}.
    }
    \item{}{\code{mstack(..., .index.var = "name")}: A variant of
      \code{\link{stack}}, where the list is taken as the list of
      arguments in \code{...}, each of which should be a \code{Vector}
      or \code{vector} (mixing the two will not work).
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a Vector object.

  \describe{
    \item{}{
      \code{tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)}:
      Like the standard \code{\link[base]{tapply}} function defined in the
      base package, the \code{tapply} method for Vector objects applies a
      function to each cell of a ragged array, that is to each (non-empty)
      group of values given by a unique combination of the levels of certain
      factors.
    }
    \item{}{
      \code{shiftApply(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE, verbose = FALSE)}:
      Let \code{i} be the indices in \code{SHIFT},
      \code{X_i = window(X, 1 + OFFSET, length(X) - SHIFT[i])}, and
      \code{Y_i = window(Y, 1 + SHIFT[i], length(Y) - OFFSET)}. Calculates
      the set of \code{FUN(X_i, Y_i, ...)} values and return the results in a
      convenient form:
      \describe{
        \item{\code{SHIFT}}{A non-negative integer vector of shift values.}
        \item{\code{X}, \code{Y}}{The Vector or R vector objects to shift.}
        \item{\code{FUN}}{The function, found via \code{match.fun}, to be
          applied to each set of shifted vectors.}
        \item{\dots}{Further arguments for \code{FUN}.}
        \item{OFFSET}{A non-negative integer offset to maintain throughout
          the shift operations.}
        \item{\code{simplify}}{A logical value specifying whether or not the
          result should be simplified to a vector or matrix if possible.}
        \item{\code{verbose}}{A logical value specifying whether or not to
          print the \code{i} indices to track the iterations.}
      }
    }
    \item{}{
      \code{aggregate(x, by, FUN, start = NULL, end = NULL, width = NULL,
                      frequency = NULL, delta = NULL, ..., simplify = TRUE))}:
      Generates summaries on the specified windows and returns the result in a
      convenient form:
      \describe{
        \item{\code{by}}{An object with \code{start}, \code{end}, and
          \code{width} methods.}
        \item{\code{FUN}}{The function, found via \code{match.fun}, to be
          applied to each window of \code{x}.}
        \item{\code{start}, \code{end}, \code{width}}{the start, end, or width
          of the window. If \code{by} is missing, then must supply two of the
          three.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
        \item{\dots}{Further arguments for \code{FUN}.}
        \item{\code{simplify}}{A logical value specifying whether or not the
          result should be simplified to a vector or matrix if possible.}
      }
    }
  }
}

\section{Utilities}{
  \describe{
    \item{}{\code{unique(x)}: Returns the unique elements in
      \code{x}. Requires that the \code{Vector} derivative support
      \code{duplicated}.
    }
  }
}

\section{Coercion}{
  \describe{
    \item{}{\code{as(from, "data.frame")}, \code{as.data.frame(from)}:
      Coerces \code{from}, a \code{Vector}, to a \code{data.frame} by
      first coercing the \code{Vector} to a \code{vector} via
      \code{as.vector}. Note that many \code{Vector} derivatives do not
      support \code{as.vector}, so this coercion is possible only for
      certain types.
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \linkS4class{Rle} and \linkS4class{XRaw} for example implementations.

  \linkS4class{List} for a direct extension that serves a similar role
  as \link[base]{list} in base R.

  \linkS4class{DataTable} which is the type of objects returned by the
  \code{mcols} accessor.

  \linkS4class{Annotated} which Vector extends.
}

\examples{
  showClass("Vector")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
