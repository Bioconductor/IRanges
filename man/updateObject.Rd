\name{updateObject}
\alias{updateObject}

\alias{updateObject,AnnotatedList-method}
\alias{updateObject,ANY-method}
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
\alias{updateObject,RangesMatchingList-method}
\alias{updateObject,RawList-method}
\alias{updateObject,RDApplyParams-method}
\alias{updateObject,Rle-method}
\alias{updateObject,RleList-method}
\alias{updateObject,RleViews-method}
\alias{updateObject,SplitXDataFrameList-method}
\alias{updateObject,XDataFrame-method}
\alias{updateObject,XDataFrameList-method}
\alias{updateObject,XInteger-method}
\alias{updateObject,XIntegerViews-method}
\alias{updateObject,XNumeric-method}
\alias{updateObject,XRaw-method}

\title{Update an IRanges object from BioC 2.4 to current version}
\description{
  Updates an old IRanges object to the current version. This updating
  process is useful when an object has been serialized (e.g., stored to
  disk) for some time (e.g., months), and the class definition has changed.
  This change in class definition makes the serialized instance is no longer
  valid.
}
\usage{
updateObject(object, \dots, verbose=FALSE)
}
\arguments{
  \item{object}{Object to be updated, or for slot information to be
    extracted from.}
  \item{verbose}{A logical, indicating whether information about the
    update should be reported. Use \code{message} to report this.}
  \item{\dots}{Additional arguments, for use in specific update methods.}
}
\value{
  \code{updateObject} returns a valid instance of \code{object}.
}

\author{Biocore team}

\keyword{manip}
