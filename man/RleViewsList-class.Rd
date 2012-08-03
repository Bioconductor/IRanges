\name{RleViewsList-class}
\docType{class}
\alias{RleViewsList-class}
\alias{SimpleRleViewsList-class}

% accessor
\alias{subject,SimpleRleViewsList-method}

% constructor
\alias{Views,RleList-method}
\alias{RleViewsList}

% coercion
\alias{coerce,RleViewsList,IRangesList-method}
\alias{coerce,RleViewsList,CompressedIRangesList-method}
\alias{coerce,RleViewsList,SimpleIRangesList-method}

\title{List of RleViews}
\description{An extension of \linkS4class{ViewsList} that holds only
  \linkS4class{RleViews} objects. Useful for storing coverage vectors
  over a set of spaces (e.g. chromosomes), each of which requires a separate
  \linkS4class{RleViews} object.
}

\details{
  For more information on methods available for RleViewsList objects consult
  the man pages for \link{ViewsList-class} and \link{Views-utils}.
}

\section{Constructor}{
  \describe{
    \item{}{\code{RleViewsList(..., rleList, rangesList, universe = NULL)}:
      Either \code{...} or the \code{rleList}/\code{rangesList} couplet
      provide the RleViews for the list. If \code{...} is provided, each
      of these arguments must be RleViews objects. Alternatively,
      \code{rleList} and \code{rangesList} accept Rle and Ranges objects
      respectively that are meshed together for form the RleViewsList.
      The universe is specified by the \code{universe} parameter, which
      should be a single string or NULL, to leave unspecified. 
    }
    \item{}{\code{Views(subject, start=NULL, end=NULL, width=NULL, names=NULL)}:
      Same as \code{RleViewsList(rleList = subject, rangesList = start)}.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{from} is an RleViewsList object:

  \describe{
    \item{}{
      \code{as(from, "IRangesList")}: Creates a \code{CompressedIRangesList}
      object containing the view locations in \code{from}.
    }
    \item{}{
      \code{as(from, "CompressedIRangesList")}: Creates a
      \code{CompressedIRangesList} object containing the view locations in
      \code{from}.
    }
    \item{}{
      \code{as(from, "SimpleIRangesList")}: Creates a \code{SimpleIRangesList}
      object containing the view locations in \code{from}.
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \link{ViewsList-class},
  \link{Views-utils}
}

\examples{
  ## Rle objects
  subject1 <- Rle(c(3L,2L,18L,0L), c(3,2,1,5))
  set.seed(0)
  subject2 <- Rle(c(0L,5L,2L,0L,3L), c(8,5,2,7,4))

  ## Views
  rleViews1 <- Views(subject1, 3:0, 5:8)
  rleViews2 <- Views(subject2, subject2 > 0) 

  ## RleList and RangesList objects
  rleList <- RleList(subject1, subject2)
  rangesList <- IRangesList(IRanges(3:0, 5:8), IRanges(subject2 > 0))

  ## methods for construction
  method1 <- RleViewsList(rleViews1, rleViews2)
  method2 <- RleViewsList(rleList = rleList, rangesList = rangesList)
  identical(method1, method2)

  ## calculation over the views
  viewSums(method1)
}

\keyword{methods}
\keyword{classes}
