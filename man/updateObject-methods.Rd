\name{updateObject-methods}
\alias{updateObject-methods}

\alias{updateObject,AnnotatedList-method}
\alias{updateObject,CharacterList-method}
\alias{updateObject,ComplexList-method}
\alias{updateObject,FilterRules-method}
\alias{updateObject,IntegerList-method}
\alias{updateObject,IntervalTree-method}
\alias{updateObject,IRanges-method}
\alias{updateObject,IRangesList-method}
\alias{updateObject,LogicalList-method}
\alias{updateObject,MaskCollection-method}
\alias{updateObject,NormalIRanges-method}
\alias{updateObject,NumericList-method}
\alias{updateObject,RangedData-method}
\alias{updateObject,RangedDataList-method}
\alias{updateObject,RangesList-method}
\alias{updateObject,RawList-method}
\alias{updateObject,RDApplyParams-method}
\alias{updateObject,Rle-method}
\alias{updateObject,RleList-method}
\alias{updateObject,RleViews-method}
\alias{updateObject,SplitXDataFrameList-method}
\alias{updateObject,XDataFrame-method}
\alias{updateObject,XDataFrameList-method}
\alias{updateObject,XVector-method}
\alias{updateObject,XIntegerViews-method}

\title{Update an object of a class defined in IRanges to its current
       class definition}

\description{
  The IRanges package provides an extensive collection of
  \code{\link[BiocGenerics]{updateObject}} methods for updating
  almost any instance of a class defined in this package.
}
\usage{
## Showing usage of method defined for IntegerList objects only (usage
## is the same for all methods).

\S4method{updateObject}{IntegerList}(object, ..., verbose=FALSE)
}

\arguments{
  \item{object}{
    Object to be updated. Many (but not all) IRanges classes are supported.
    If no specific method is available for the object, then the default
    method (defined in the BiocGenerics package) is used.
    See \code{?\link[BiocGenerics]{updateObject}} for a description of
    the default method.
  }
  \item{..., verbose}{
    See \code{?\link[BiocGenerics]{updateObject}}.
  }
}

\value{
  Returns a valid instance of \code{object}.
}

\author{The Bioconductor Dev Team}

\seealso{
  \code{\link[BiocGenerics]{updateObject}}
}

\keyword{manip}
