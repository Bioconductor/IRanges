\name{IRanges internals}
\docType{class}

\alias{setValidity2}
\alias{new2}
\alias{safeExplode}

% Some low-level helper functions and classes (from R/utils.R):
\alias{isTRUEorFALSE}
\alias{isSingleInteger}
\alias{isSingleNumber}
\alias{isSingleString}
\alias{isSingleIntegerOrNA}
\alias{isSingleNumberOrNA}
\alias{isSingleStringOrNA}
\alias{isConstant}
\alias{normargShift}
\alias{normargWeight}

\alias{class:characterORNULL}
\alias{characterORNULL-class}
\alias{characterORNULL}

\alias{coerce,ANY,vector-method}

% SharedVector and SharedVector_Pool classes, functions and methods:
\alias{class:SharedVector}
\alias{SharedVector-class}
\alias{SharedVector}
\alias{class:SharedVector_Pool}
\alias{SharedVector_Pool-class}

\alias{show,externalptr-method}
\alias{length,SharedVector-method}
\alias{show,SharedVector-method}
\alias{length,SharedVector_Pool-method}
\alias{width,SharedVector_Pool-method}
\alias{show,SharedVector_Pool-method}
\alias{coerce,SharedVector,SharedVector_Pool-method}
\alias{c,SharedVector_Pool-method}
\alias{SharedVector.compare}
\alias{SharedVector.copy}
\alias{SharedVector.reverseCopy}
\alias{as.numeric,SharedVector-method}
\alias{==,SharedVector,SharedVector-method}
\alias{!=,SharedVector,SharedVector-method}

% SharedRaw and SharedRaw_Pool classes, functions and methods:
\alias{class:SharedRaw}
\alias{SharedRaw-class}
\alias{SharedRaw}
\alias{class:SharedRaw_Pool}
\alias{SharedRaw_Pool-class}

\alias{[[,SharedRaw_Pool-method}
\alias{SharedRaw.readInts}
\alias{SharedRaw.writeInts}
\alias{SharedRaw.read}
\alias{SharedRaw.write}
\alias{SharedRaw.readComplexes}
\alias{SharedRaw.append}
\alias{as.integer,SharedRaw-method}
\alias{toString,SharedRaw-method}

% SharedInteger class, functions and methods:
\alias{class:SharedInteger}
\alias{SharedInteger-class}
\alias{SharedInteger}

\alias{show,SharedInteger-method}
\alias{as.integer,SharedInteger-method}

% SharedDouble class, functions and methods:
\alias{class:SharedDouble}
\alias{SharedDouble-class}
\alias{SharedDouble}

\alias{show,SharedDouble-method}
\alias{as.numeric,SharedDouble-method}

% copy():
\alias{copy}
\alias{copy,SharedRaw-method}
\alias{copy,SharedVector_Pool-method}


\title{IRanges internals}

\description{
  IRanges objects, classes and methods that are not intended to
  be used directly.
}

\keyword{internal}
\keyword{methods}
\keyword{classes}
