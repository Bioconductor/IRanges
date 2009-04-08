\name{IRanges internals}
\docType{class}

\alias{setValidity2}
\alias{new2}
\alias{sapplyLength}
\alias{safeExplode}

% Some low-level helper functions and classes (from R/utils.R):
\alias{isTRUEorFALSE}
\alias{isSingleInteger}
\alias{isSingleNumber}
\alias{isSingleString}
\alias{isSingleIntegerOrNA}
\alias{isSingleNumberOrNA}
\alias{isSingleStringOrNA}
\alias{normargShift}

\alias{class:characterORNULL}
\alias{characterORNULL-class}
\alias{characterORNULL}

% ListLike VIRTUAL class, functions and methods:
\alias{class:ListLike}
\alias{ListLike-class}
\alias{ListLike}

\alias{[[,ListLike-method}
\alias{$,ListLike-method}
\alias{lapply,ListLike,ANY-method}
\alias{sapply,ListLike-method}
\alias{as.list,ListLike-method}

% SequencePtr class, functions and methods:
\alias{class:SequencePtr}
\alias{SequencePtr-class}
\alias{SequencePtr}

\alias{as.numeric,SequencePtr-method}
\alias{show,externalptr-method}
\alias{length,SequencePtr-method}
\alias{==,SequencePtr,SequencePtr-method}
\alias{!=,SequencePtr,SequencePtr-method}

% RawPtr class, functions and methods:
\alias{class:RawPtr}
\alias{RawPtr-class}
\alias{RawPtr}

\alias{show,RawPtr-method}
\alias{RawPtr.readInts}
\alias{RawPtr.writeInts}
\alias{RawPtr.read}
\alias{RawPtr.write}
\alias{RawPtr.copy}
\alias{RawPtr.reverseCopy}
\alias{RawPtr.readComplexes}
\alias{RawPtr.append}
\alias{RawPtr.compare}
\alias{as.integer,RawPtr-method}
\alias{toString,RawPtr-method}

% IntegerPtr class, functions and methods:
\alias{class:IntegerPtr}
\alias{IntegerPtr-class}
\alias{IntegerPtr}

\alias{show,IntegerPtr-method}
\alias{as.integer,IntegerPtr-method}

% NumericPtr class, functions and methods:
\alias{class:NumericPtr}
\alias{NumericPtr-class}
\alias{NumericPtr}

\alias{show,NumericPtr-method}
\alias{as.numeric,NumericPtr-method}


\title{IRanges internals}

\description{
  IRanges objects, classes and methods that are not intended to
  be used directly.
}

\keyword{internal}
\keyword{methods}
\keyword{classes}
