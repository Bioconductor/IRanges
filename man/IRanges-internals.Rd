\name{IRanges internals}
\docType{class}

\alias{setValidity2}
\alias{new2}

% Some low-level classes and helper functions (from R/utils.R):
\alias{AsIs-class}
\alias{xtabs-class}
\alias{file-class}
\alias{connection-class}

\alias{.Call2}
\alias{class:characterORNULL}
\alias{characterORNULL-class}
\alias{characterORNULL}
\alias{class:functionORNULL}
\alias{functionORNULL-class}
\alias{functionORNULL}
\alias{coerce,ANY,vector-method}

% More low-level helper functions (from R/normarg-utils.R):
\alias{isTRUEorFALSE}
\alias{isSingleInteger}
\alias{isSingleNumber}
\alias{isSingleString}
\alias{isSingleNumberOrNA}
\alias{isSingleStringOrNA}
\alias{recycleIntegerArg}
\alias{recycleNumericArg}
\alias{fold}

% SharedVector and SharedVector_Pool classes, functions and methods:
\alias{class:SharedVector}
\alias{SharedVector-class}
\alias{SharedVector}
\alias{class:SharedVector_Pool}
\alias{SharedVector_Pool-class}
\alias{SharedVector_Pool}

\alias{show,externalptr-method}
\alias{length,SharedVector-method}
\alias{show,SharedVector-method}
\alias{length,SharedVector_Pool-method}
\alias{width,SharedVector_Pool-method}
\alias{show,SharedVector_Pool-method}
\alias{coerce,SharedVector,SharedVector_Pool-method}
\alias{c,SharedVector_Pool-method}
\alias{[,SharedVector_Pool-method}
\alias{SharedVector.compare}
\alias{SharedVector.copy}
\alias{as.numeric,SharedVector-method}
\alias{==,SharedVector,SharedVector-method}
\alias{!=,SharedVector,SharedVector-method}

% SharedRaw and SharedRaw_Pool classes, functions and methods:
\alias{class:SharedRaw}
\alias{SharedRaw-class}
\alias{SharedRaw}
\alias{class:SharedRaw_Pool}
\alias{SharedRaw_Pool-class}
\alias{SharedRaw_Pool}

\alias{[[,SharedRaw_Pool-method}
\alias{SharedRaw.readInts}
\alias{SharedRaw.writeInts}
\alias{SharedRaw.read}
\alias{SharedRaw.write}
\alias{SharedRaw.readComplexes}
\alias{as.integer,SharedRaw-method}
\alias{toString,SharedRaw-method}

% SharedInteger class, functions and methods:
\alias{class:SharedInteger}
\alias{SharedInteger-class}
\alias{SharedInteger}

\alias{as.integer,SharedInteger-method}

% SharedDouble class, functions and methods:
\alias{class:SharedDouble}
\alias{SharedDouble-class}
\alias{SharedDouble}

\alias{as.numeric,SharedDouble-method}


\title{IRanges internals}

\description{
  IRanges objects, classes and methods that are not intended to
  be used directly.
}

\keyword{internal}
\keyword{methods}
\keyword{classes}
