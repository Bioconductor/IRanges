\name{Annotated-class}
\docType{class}
\alias{Annotated}
\alias{Annotated-class}

% accessors
\alias{metadata}
\alias{metadata,Annotated-method}
\alias{metadata<-}
\alias{metadata<-,Annotated-method}

\title{Annotated class}
\description{The virtual class \code{Annotated} is used to standardize the
  storage of metadata with a subclass.}

\details{
  The \code{Annotated} class supports the storage of global metadata in a
  subclass. This is done through the \code{metadata} slot that stores a list
  object.
}

\section{Accessors}{
  In the following code snippets, \code{x} is an \code{Annotated} object.

  \describe{
    \item{}{\code{metadata(x)}, \code{metadata(x) <- value}: Get or set the list
      holding arbitrary R objects as annotations. May be, and often is, empty.
    }
  }
}

\author{P. Aboyoun}
\seealso{\code{\linkS4class{Vector}} for example implementations}
\keyword{methods}
\keyword{classes}
