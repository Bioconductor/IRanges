\name{AnnotatedList-class}
\docType{class}
\alias{AnnotatedList-class}

% accessors
\alias{annotation}
\alias{annotation,AnnotatedList-method}
\alias{elementMetadata}
\alias{elementMetadata,AnnotatedList-method}
\alias{annotation<-}
\alias{annotation<-,AnnotatedList,characterORNULL-method}
\alias{elementMetadata<-}
\alias{elementMetadata<-,AnnotatedList,XDataFrameORNULL-method}

\alias{AnnotatedList}

\title{Annotated Lists}
\description{The class \code{AnnotatedList} extends
  \code{\linkS4class{TypedList}} to contain two optional metadata elements: an
  annotation string, referring to a separate metadata resource, and a
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
    \item{annotation}{
      A single string (or \code{NULL}) referring to
      external annotation resources. For example, the name of a genome
      or a microarray chip.
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
    \item{}{\code{annotation(x)}, \code{annotation(x) <- value}: Get or set the
      the single string referring to a separate metadata
      resource. Optional, may be \code{NULL}. 
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
    \item{}{\code{AnnotatedList(elements = list(), annotation = NULL,
        elementMetadata = NULL)}:
      Constructs an instance of an \code{AnnotatedList} containing the
      elements in the ordinary list \code{elements}. The parameters
      \code{annotation} and \code{elementMetadata} correspond to the
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

  ## access 'annotation' slot
  annotation(collection) <- "hg18"
  annotation(collection)

  ## set some element metadata
  elementMetadata(collection) <- XDataFrame(chrom = c("chr1", "chr2"))
  elementMetadata(collection)
}
\keyword{methods}
\keyword{classes}
