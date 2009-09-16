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

% SharedVector class, functions and methods:
\alias{class:SharedVector}
\alias{SharedVector-class}
\alias{SharedVector}

\alias{as.numeric,SharedVector-method}
\alias{show,externalptr-method}
\alias{length,SharedVector-method}
\alias{==,SharedVector,SharedVector-method}
\alias{!=,SharedVector,SharedVector-method}

% SharedRaw class, functions and methods:
\alias{class:SharedRaw}
\alias{SharedRaw-class}
\alias{SharedRaw}

\alias{show,SharedRaw-method}
\alias{SharedRaw.readInts}
\alias{SharedRaw.writeInts}
\alias{SharedRaw.read}
\alias{SharedRaw.write}
\alias{SharedRaw.copy}
\alias{SharedRaw.reverseCopy}
\alias{SharedRaw.readComplexes}
\alias{SharedRaw.append}
\alias{SharedRaw.compare}
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


\title{IRanges internals}

\description{
  IRanges objects, classes and methods that are not intended to
  be used directly.
}

\keyword{internal}
\keyword{methods}
\keyword{classes}
