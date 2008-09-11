\name{XSeq-class}
\docType{class}

% XSeq class, functions and methods:
\alias{class:XSeq}
\alias{XSeq-class}
\alias{XSeq}

\alias{length,XSeq-method}

% XRaw class, functions and methods:
\alias{class:XRaw}
\alias{XRaw-class}
\alias{XRaw}

% XInteger class, functions and methods:
\alias{class:XInteger}
\alias{XInteger-class}
\alias{XInteger}

\alias{as.integer,XInteger-method}
\alias{[,XInteger-method}
\alias{[<-,XInteger-method}
\alias{show,XInteger-method}
\alias{==,XInteger,XInteger-method}
\alias{!=,XInteger,XInteger-method}

% XNumeric class, functions and methods:
\alias{class:XNumeric}
\alias{XNumeric-class}
\alias{XNumeric}


\title{XSeq objects}

\description{
  The XSeq virtual class is a general container for storing
  an "external sequence".
  The following classes derive directly from the XSeq class.

  The XRaw class is a container for storing an external sequence
  of bytes (stored as char values at the C level).

  The XInteger class is a container for storing an external sequence
  of integer values (stored as int values at the C level).

  The XNumeric class is a container for storing an external sequence
  of numeric values (stored as double values at the C level).

  The purpose of these containers is to provide a "pass by address"
  semantic and also to avoid the overhead of copying the sequence
  data when a linear subsequence needs to be extracted.
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
