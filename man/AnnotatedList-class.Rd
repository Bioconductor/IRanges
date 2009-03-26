\name{AnnotatedList-class}
\docType{class}
\alias{AnnotatedList-class}

% accessors
\alias{metadata}
\alias{metadata,AnnotatedList-method}
\alias{elementMetadata}
\alias{elementMetadata,AnnotatedList-method}
\alias{metadata<-}
\alias{metadata<-,AnnotatedList,list-method}
\alias{elementMetadata<-}
\alias{elementMetadata<-,AnnotatedList,XDataFrameORNULL-method}

\alias{AnnotatedList}

\title{Annotated Lists}
\description{The class \code{AnnotatedList} extends
  \code{\linkS4class{TypedList}} to contain two optional metadata
  elements: a list with arbitrary annotations, and a
  data frame with local metadata on each element.}

\details{
  Data analysis should not occur in a vacuum: every dataset floats in a
  sea of metadata, describing the observed features, as well as the
  experimental design and methodology. Some metadata is local to an
  experiment, such as its design, while other information, such as the
  layout of a microarray chip, transcends individual experiments.

  The list structure is fundamental to computationally representing
  datasets. For example, the \code{data.frame} is a list of column
  vectors. The \code{AnnotatedList} is a list that additionally aims to
  store local metadata and reference global annotation resources.

  This is implemented by adding two optional components on top of the
  underlying \code{TypedList}:
  \describe{
    \item{metadata}{
      A list, possibly empty, containing arbitrary annotations.
      For example, the name of the genome might be stored in a character
      vector as an element named "genome".
    }
    \item{elementMetadata}{
      A data frame of class \code{\linkS4class{XDataFrame}} (or
      \code{NULL}) with a row for each element and a column for each
      metadata variable. This is for storing metadata local to the
      experiment, such as experimental design, or chromosome-wide
      statistics for datasets of genomic features.
    }
  }
}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{TypedList} object.
  
  \describe{
    \item{}{\code{metadata(x)}, \code{metadata(x) <- value}: Get or set the
      list holding arbitary R objects as annotations. May be, and often
      is, empty.
    }
    \item{}{\code{elementMetadata(x), elementMetadata(x) <- value}:
      Get or set the
      \code{\linkS4class{XDataFrame}} holding local metadata on each
      element. The rows are named according to the names of the
      elements. Optional, may be \code{NULL}.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{\code{AnnotatedList(elements = list(), metadata = NULL,
        elementMetadata = NULL)}:
      Constructs an instance of an \code{AnnotatedList} containing the
      elements in the ordinary list \code{elements}. The parameters
      \code{metadata} and \code{elementMetadata} correspond to the
      components described above.
    }
  }
}

\author{Michael Lawrence}
\seealso{\code{\linkS4class{RangesList}} and
  \code{\linkS4class{XDataFrame}} both extend this class.}

\examples{
  ## demonstrated on RangesList objects, a subtype of AnnotatedList

  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))
  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  collection <- RangesList(range1, range2)

  ## access 'metadata' component
  metadata(collection)$genome <- "hg18"
  metadata(collection)

  ## set some element metadata
  elementMetadata(collection) <- XDataFrame(chrom = c("chr1", "chr2"))
  elementMetadata(collection)
}
\keyword{methods}
\keyword{classes}
