\name{XInteger-class}
\docType{class}

% Classes
\alias{class:XInteger}
\alias{XInteger-class}

% Constructor-like functions and generics:
\alias{XInteger}
\alias{initialize,XInteger-method}

% Accessor methods:
\alias{length,XInteger-method}

% Coercion:
\alias{as.integer,XInteger-method}

% "show" method:
\alias{show,XInteger-method}

% subsetting methods:
\alias{[,XInteger-method}
\alias{[<-,XInteger-method}

% Equality:
\alias{==,XInteger,XInteger-method}
\alias{!=,XInteger,XInteger-method}


\title{XInteger objects}

\description{
  The XInteger class is a general container for storing a big integer vector
  and for making its manipulation easy and efficient.
}

\details{
  The main difference between an XInteger object and a standard integer
  vector is that the data stored in an XInteger object are not copied on
  object duplication.
}

\section{Constructor-like functions and generics}{
  In the code snippet below,
  \code{\dots} can be an integer vector or an XInteger object.

  \describe{
    \item{}{
      \code{XInteger(\dots)}:
      Tries to convert \code{\dots} into an XInteger object.
    }
  }
}

\section{Accessor methods}{
  In the code snippets below, \code{x} is an XInteger object.

  \describe{
    \item{}{
      \code{length(x)}:
      Get the length of an XInteger object, i.e., its number of letters.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} is an XInteger object.

  \describe{
    \item{}{
      \code{as.integer(x)}:
      Converts \code{x} to an integer vector.
    }
  }
}

\section{Subsetting}{
  In the code snippets below, \code{x} is an XInteger object.

  \describe{
    \item{}{
      \code{x[i, drop = TRUE]}:
      Return a new XInteger object made of the selected elements (subscript
      \code{i} must be an NA-free numeric vector specifying the positions of
      the elements to select). The \code{drop} argument specifies whether or
      not to coerce the subset to an integer vector.
    }
  }
}

\section{Equality}{
  In the code snippets below, \code{e1} and \code{e2} are XInteger objects.

  \describe{
    \item{}{
      \code{e1 == e2}:
      \code{TRUE} if \code{e1} points to the same memory address as \code{e2}.
      \code{FALSE} otherwise.
    }
    \item{}{
      \code{e1 != e2}:
      Equivalent to \code{!(e1 == e2)}.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{XIntegerViews-class}
}

\examples{
  x1 <- XInteger(c(-1:10))
  x1
  length(x1)

  ## Subsetting
  x1[length(x1):1]
  x1[length(x1):1, drop = FALSE]
  x2 <- x1[length(x1):1, drop = FALSE]

  x2 == x1

  ## x1[1:length(x1)] is not equal x1!
  x1 == x1[1:length(x1), drop = FALSE]
}

\keyword{methods}
\keyword{classes}
