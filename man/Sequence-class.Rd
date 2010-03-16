\name{Sequence-class}
\docType{class}

% Sequence class, functions and methods:
\alias{class:Sequence}
\alias{Sequence-class}
\alias{Sequence}

\alias{elementType}
\alias{elementType,Sequence-method}
\alias{elementType,vector-method}
\alias{elementLengths}
\alias{elementLengths,list-method}
\alias{elementLengths,Sequence-method}
\alias{elementLengths,CompressedList-method}
\alias{isEmpty}
\alias{isEmpty,ANY-method}
\alias{nlevels,Sequence-method}
\alias{elementMetadata}
\alias{elementMetadata,Sequence-method}
\alias{elementMetadata<-}
\alias{elementMetadata<-,Sequence,DataTableORNULL-method}

\alias{[,Sequence-method}
\alias{[<-,Sequence-method}
\alias{window,Sequence-method}
\alias{window,vector-method}
\alias{window,factor-method}
\alias{window<-,Sequence-method}
\alias{window<-,vector-method}
\alias{window<-,factor-method}
\alias{seqselect}
\alias{seqselect,Sequence-method}
\alias{seqselect,vector-method}
\alias{seqselect,factor-method}
\alias{seqselect<-}
\alias{seqselect<-,Sequence-method}
\alias{seqselect<-,vector-method}
\alias{seqselect<-,factor-method}

\alias{head,Sequence-method}
\alias{tail,Sequence-method}
\alias{rev,Sequence-method}
\alias{rep,Sequence-method}
\alias{subset,Sequence-method}

\alias{[[,Sequence-method}
\alias{$,Sequence-method}

\alias{!=,Sequence,Sequence-method}

\alias{c,Sequence-method}
\alias{append,Sequence,Sequence-method}

\alias{lapply,Sequence-method}
\alias{sapply,Sequence-method}
\alias{mapply}
\alias{mapply,Sequence-method}
\alias{tapply,Sequence-method}
\alias{endoapply,Sequence-method}
\alias{mendoapply,Sequence-method}
\alias{shiftApply}
\alias{shiftApply,Sequence,Sequence-method}
\alias{shiftApply,vector,vector-method}
\alias{aggregate,Sequence-method}
\alias{aggregate,vector-method}
\alias{aggregate,matrix-method}
\alias{aggregate,data.frame-method}
\alias{aggregate,ts-method}

\alias{coerce,Sequence,list-method}
\alias{as.list,Sequence-method}
\alias{as.env}
\alias{as.env,Sequence-method}

\alias{Reduce,ANY,Sequence-method}
\alias{Filter,ANY,Sequence-method}
\alias{Find,ANY,Sequence-method}
\alias{Map}
\alias{Map,Sequence-method}
\alias{Position,ANY,Sequence-method}

\alias{eval,expressionORlanguage,Sequence-method}
\alias{with,Sequence-method}

\title{Sequence objects}

\description{
  The Sequence virtual class serves as the heart of the IRanges package
  and has over 90 subclasses. It serves a similar role as \link[base]{vector}
  in base R. The Sequence class includes three slots: \code{elementType},
  \code{metadata} (via extension of the \linkS4class{Annotated} class), and
  \code{elementMetadata}. Their purpose is defined below.

  The \code{elementType} slot is the preferred location for Sequence
  subclasses to store the type of data represented in the sequence. It is
  designed to take a character of length 1 representing the class of the
  sequence elements. While the Sequence class performs no validity checking
  based on \code{elementType}, if a subclass expects elements to be of a
  given type, that subclass is expected to perform the necessary validity
  checking. For example, the subclass \link{IntegerList} has
  \code{elementType = "integer"} and its validity method checks if this
  condition is TRUE.

  The Sequence class supports the storage of global and element-wise metadata
  with its \code{metadata} and \code{elementMetadata} slots. The
  \code{metadata} slot can store a list of metadata pertaining to the whole
  object and the \code{elementMetadata} slot can store a
  \linkS4class{DataTable} (or \code{NULL}) for element-wise metadata with a
  row for each element and a column for each metadata variable.

  To be functional, a class that inherits from Sequence must define a
  \code{length} and \code{names} methods as well as one or both of the
  subscript methods \code{"[["} and \code{"["}.
}

\section{Accessors}{
  In the following code snippets, \code{x} is a Sequence object.
  
  \describe{
    \item{}{\code{length(x)}: Get the number of elements in \code{x}}
    \item{}{\code{names(x)}, \code{names(x) <- value}: Get or set the names of
      the elements in the Sequence.
    }
    \item{}{\code{elementType(x)}: Get the scalar string naming the class from
      which all elements must derive.
    }
    \item{}{\code{elementLengths(x)}: Get the 'length' of each of the elements.
    }
    \item{}{\code{isEmpty(x)}: Returns a logical indicating either if the
      sequence has no elements or if all its elements are empty.
    }
    \item{}{
      \code{nlevels(x)}:
      Returns the number of factor levels.
    }
    \item{}{\code{metadata(x)}, \code{metadata(x) <- value}:
      Get or set the list holding arbitrary R objects as annotations. May be,
      and often is, empty.
    }
    \item{}{\code{elementMetadata(x), elementMetadata(x) <- value}:
      Get or set the \linkS4class{DataTable} holding local metadata on each
      element. The rows are named according to the names of the elements.
      Optional, may be \code{NULL}.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is a Sequence object or regular R vector
  object. The R vector object methods for \code{window} and \code{seqselect}
  are defined in this package and the remaining methods are defined in base R.

  \describe{
    \item{}{
      \code{x[i, drop=TRUE]}:
      If defined, returns a new Sequence object made of selected elements
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
      Extract the subsequence window from the Sequence object using:
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
      repeated to create a Sequence with the same number of elements as the
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
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n elements of the Sequence
      object.
      If \code{n} is negative, returns all but the last \code{abs(n)} elements
      of the Sequence object.
    }
    \item{}{
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n elements of the Sequence
      object.
      If \code{n} is negative, returns all but the first \code{abs(n)} elements
      of the Sequence object.
    }
    \item{}{
      \code{rev(x)}:
      Return a new Sequence object made of the original elements in the reverse
      order.
    }
    \item{}{
      \code{rep(x, times, length.out, each)}:
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
      Return a new Sequence object made of the subset using logical vector
      \code{subset}, where missing values are taken as FALSE.
    }
  }
}

\section{Element extraction (list style)}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{
      \code{x[[i]]}:
      If defined, return the selected element \code{i}, where \code{i} is an
      numeric or character vector of length 1.
    }
    \item{}{
      \code{x$name}:
      Similar to \code{x[[name]]}, but \code{name} is taken literally as an
      element name.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{
      \code{c(x, ...)}:
      Combine \code{x} and the Sequence objects in \code{...} together.
      Any object in \code{...} must belong to the same class as \code{x},
      or to one of its subclasses, or must be \code{NULL}.
      The result is an object of the same class as \code{x}.
    }
    \item{}{\code{append(x, values, after = length(x))}: Insert the
      \code{Sequence} \code{values} onto \code{x} at the position given by
      \code{after}. \code{values} must have an \code{elementType} that extends
      that of \code{x}.
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{
      \code{lapply(X, FUN, ...)}:
      Like the standard \code{\link[base]{lapply}} function defined in the
      base package, the \code{lapply} method for Sequence objects returns
      a list of the same length as \code{X}, with each element being the
      result of applying \code{FUN} to the corresponding element of \code{X}.
    }
    \item{}{
      \code{sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)}:
      Like the standard \code{\link[base:lapply]{sapply}} function defined in the
      base package, the \code{sapply} method for Sequence objects is a
      user-friendly version of \code{lapply} by default returning a vector
      or matrix if appropriate.
    }
    \item{}{
      \code{mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)}:
      Like the standard \code{\link[base]{mapply}} function defined in the
      base package, the \code{mapply} method for Sequence objects is a
      multivariate version of \code{sapply}.
    }
    \item{}{
      \code{tapply(X, INDEX, FUN = NULL, ..., simplify = TRUE)}:
      Like the standard \code{\link[base]{tapply}} function defined in the
      base package, the \code{tapply} method for Sequence objects applies a
      function to each cell of a ragged array, that is to each (non-empty)
      group of values given by a unique combination of the levels of certain
      factors.
    }
    \item{}{
      \code{endoapply(X, FUN, ...)}:
      Similar to \code{\link[base]{lapply}}, but performs an endomorphism,
      i.e. returns an object of \code{class(X)}.
    }
    \item{}{
      \code{mendoapply(FUN, ..., MoreArgs = NULL)}:
      Similar to \code{\link[base]{mapply}}, but performs an endomorphism
      across multiple objects, i.e. returns an object of
      \code{class(list(...)[[1]])}.
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
        \item{\code{X}, \code{Y}}{The Sequence or R vector objects to shift.}
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

\section{Coercion}{
  In the code snippets below, \code{x} is a Sequence object.

  \describe{
    \item{}{\code{as.env(x, enclos = parent.frame())}:
      Creates an environment from \code{x} with a symbol for each
      \code{names(x)}. The values are not actually copied into the
      environment. Rather, they are dynamically bound using
      \code{\link{makeActiveBinding}}. This prevents unnecessary copying
      of the data from the external vectors into R vectors. The values
      are cached, so that the data is not copied every time the symbol
      is accessed.
    }
    \item{}{
      \code{as.list(x, ...)}, \code{as(from, "list")}:
      Turns \code{x} into a standard list.
    }
  }
}

\section{Functional Programming}{
  The R base package defines some Higher-Order functions that are commonly
  found in Functional Programming Languages. See \code{?\link[base]{Reduce}}
  for the details, and, in particular, for a description of their arguments.
  The IRanges package provides methods for Sequence objects, so, in addition
  to be a vector, the \code{x} argument can also be a Sequence object.

  \describe{
    \item{}{
      \code{Reduce(f, x, init, right = FALSE, accumulate = FALSE)}:
      Uses a binary function to successively combine the elements of \code{x}
      and a possibly given initial value.
      See \code{?\link[base]{Reduce}} (in the base package) for the details.
    }
    \item{}{
      \code{Filter(f, x)}:
      Extracts the elements of \code{x} for which function \code{f} is TRUE.
      See \code{?\link[base]{Filter}} (in the base package) for the details.
    }
    \item{}{
      \code{Find(f, x, right = FALSE, nomatch = NULL)}:
      Extracts the first or last such element in \code{x}.
      See \code{?\link[base]{Find}} (in the base package) for the details.
    }
    \item{}{
      \code{Map(f, ...)}:
      Applies a function to the corresponding elements of given Sequence
      objects.
      See \code{?\link[base]{Map}} (in the base package) for the details.
    }
    \item{}{
      \code{Position(f, x, right = FALSE, nomatch = NA_integer_)}:
      Extracts the first or last such position in \code{x}.
      See \code{?\link[base]{Position}} (in the base package) for the details.
    }
  }
}

\section{Evaluating}{
  In the code snippets below, \code{envir} and \code{data} are Sequence
  objects.

  \describe{
    \item{}{\code{eval(expr, envir, enclos = parent.frame())}:
      Converts the Sequence object specified in \code{envir} to an
      environment using \code{as.env}, with \code{enclos} as its parent,
      and then evaluates \code{expr} within that environment.
    }
    \item{}{\code{with(data, expr, \dots)}:
      Equivalent to \code{eval(quote(expr), data, ...)}.
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \linkS4class{Annotated},
  \linkS4class{DataTable},
  \linkS4class{SimpleList},
  \linkS4class{Ranges},
  \linkS4class{Rle},
  \linkS4class{XVector} for example implementations
}

\examples{
  showClass("Sequence")  # shows (some of) the known subclasses
}

\keyword{methods}
\keyword{classes}
