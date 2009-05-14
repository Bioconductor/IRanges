\name{Rle-class}
\docType{class}

\alias{class:Rle}
\alias{Rle-class}
\alias{Rle}
\alias{Rle,missing,missing-method}
\alias{Rle,vectorORfactor,missing-method}
\alias{Rle,vectorORfactor,integer-method}
\alias{Rle,vectorORfactor,numeric-method}

\alias{runLength}
\alias{runLength,Rle-method}
\alias{runValue}
\alias{runValue,Rle-method}
\alias{nrun}
\alias{nrun,Rle-method}
\alias{start,Rle-method}
\alias{end,Rle-method}
\alias{width,Rle-method}

\alias{runLength<-}
\alias{runLength<-,Rle-method}
\alias{runValue<-}
\alias{runValue<-,Rle-method}

\alias{as.vector,Rle,missing-method}
\alias{as.logical,Rle-method}
\alias{as.integer,Rle-method}
\alias{as.numeric,Rle-method}
\alias{as.complex,Rle-method}
\alias{as.character,Rle-method}
\alias{as.raw,Rle-method}
\alias{as.factor,Rle-method}
\alias{coerce,vector,Rle-method}
\alias{coerce,factor,Rle-method}
\alias{coerce,Rle,vector-method}
\alias{coerce,Rle,logical-method}
\alias{coerce,Rle,integer-method}
\alias{coerce,Rle,numeric-method}
\alias{coerce,Rle,complex-method}
\alias{coerce,Rle,character-method}
\alias{coerce,Rle,raw-method}
\alias{coerce,Rle,factor-method}
\alias{coerce,Rle,IRanges-method}
\alias{coerce,Rle,NormalIRanges-method}

\alias{Ops,Rle,Rle-method}
\alias{Ops,Rle,vector-method}
\alias{Ops,vector,Rle-method}
\alias{Math,Rle-method}
\alias{Math2,Rle-method}
\alias{Summary,Rle-method}
\alias{Complex,Rle-method}

\alias{[,Rle-method}
\alias{\%in\%,Rle,ANY-method}
\alias{aggregate,Rle-method}
\alias{c,Rle-method}
\alias{findRange}
\alias{findRange,Rle-method}
\alias{findRun}
\alias{findRun,Rle-method}
\alias{is.na,Rle-method}
\alias{length,Rle-method}
\alias{rep,Rle-method}
\alias{rep.int}
\alias{rep.int,Rle-method}
\alias{rev,Rle-method}
\alias{shiftApply,Rle,Rle-method}
\alias{show,Rle-method}
\alias{sort,Rle-method}
\alias{seqextract,Rle-method}
\alias{summary,Rle-method}
\alias{table}
\alias{table,Rle-method}
\alias{unique,Rle-method}
\alias{window,Rle-method}

\alias{!,Rle-method}
\alias{which,Rle-method}

\alias{pmax}
\alias{pmax,Rle-method}
\alias{pmin}
\alias{pmin,Rle-method}
\alias{pmax.int}
\alias{pmax.int,Rle-method}
\alias{pmin.int}
\alias{pmin.int,Rle-method}
\alias{diff,Rle-method}
\alias{mean,Rle-method}
\alias{var,Rle,missing-method}
\alias{var,Rle,Rle-method}
\alias{cov,Rle,Rle-method}
\alias{cor,Rle,Rle-method}
\alias{sd,Rle-method}
\alias{median,Rle-method}
\alias{quantile,Rle-method}
\alias{mad,Rle-method}

\alias{nchar,Rle-method}
\alias{substr,Rle-method}
\alias{substring,Rle-method}
\alias{chartr,ANY,ANY,Rle-method}
\alias{tolower,Rle-method}
\alias{toupper,Rle-method}
\alias{sub,ANY,ANY,Rle-method}
\alias{gsub,ANY,ANY,Rle-method}


\title{Rle objects}

\description{
  The Rle class is a general container for storing an atomic vector
  that is stored in a run-length encoding format. It is based on the
  \code{\link[base]{rle}} function from the base package.
}

\section{Constructors}{
  \describe{
    \item{}{
      \code{Rle(values)}:
      This constructor creates an Rle instances out of an atomic
      vector \code{values}.
    }
    \item{}{
      \code{Rle(values, lengths)}:
      This constructor creates an Rle instances out of an atomic
      vector or factor object \code{values} and an integer or numeric vector
      \code{lengths} with all positive elements that represent how many times
      each value is repeated. The length of these two vectors must be the same.
    }
    \item{}{
      \code{as(from, "Rle")}:
      This constructor creates an Rle instances out of an atomic
      vector \code{from}.
    }
  }
}

\section{Accessors}{
  In the code snippets below, \code{x} is an Rle object:

  \describe{
    \item{}{
      \code{runLength(x)}:
      Returns the run lengths for \code{x}.
    }
    \item{}{
      \code{runValue(x)}:
      Returns the run values for \code{x}.
    }
    \item{}{
      \code{nrun(x)}:
      Returns the number of runs in \code{x}.
    }
    \item{}{
      \code{start(x)}:
      Returns the starts of the runs for \code{x}.
    }
    \item{}{
      \code{end(x)}:
      Returns the ends of the runs for \code{x}.
    }
    \item{}{
      \code{width(x)}:
      Same as \code{runLength(x)}.
    }
  }
}

\section{Replacers}{
  In the code snippets below, \code{x} is an Rle object:

  \describe{
    \item{}{
      \code{runLength(x) <- value}:
      Replaces \code{x} with a new Rle object using run values
      \code{runValue(x)} and run lengths \code{value}.
    }
    \item{}{
      \code{runValue(x) <- value}:
      Replaces \code{x} with a new Rle object using run values
      \code{value} and run lengths \code{runLength(x)}.
    }
  }
}

\section{Coercion}{
  In the code snippets below, \code{x} and \code{from} are Rle objects:

  \describe{
    \item{}{
      \code{as.vector(x)}, \code{as(from, "vector")}: Creates an atomic vector
      of the most appropriate type based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.logical(x)}, \code{as(from, "logical")}: Creates a logical
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.integer(x)}, \code{as(from, "integer")}: Creates an integer
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.numeric(x)}, \code{as(from, "numeric")}: Creates a numeric
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.complex(x)}, \code{as(from, "complex")}: Creates a complex
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.character(x)}, \code{as(from, "character")}: Creates a character
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.raw(x)}, \code{as(from, "raw")}: Creates a raw
      vector based on the values contained in \code{x}.
    }
    \item{}{
      \code{as.factor(x)}, \code{as(from, "factor")}: Creates a factor object
      based on the values contained in \code{x}.
    }
    \item{}{
      \code{as(from, "IRanges")}: Creates an \link{IRanges} instance
      from a logical Rle. Note that this instance is guaranteed to be normal.
    }
    \item{}{
      \code{as(from, "NormalIRanges")}: Creates a \link{NormalIRanges} instance
      from a logical Rle.
    }
  }
}

\section{Group Generics}{
  Rle objects have support for S4 group generic functionality:
  \describe{
    \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
      \code{"\%\%"}, \code{"\%/\%"}, \code{"/"}}
    \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"}, \code{"!="}, 
      \code{"<="}, \code{">="}}
    \item{\code{Logic}}{\code{"&"}, \code{"|"}}
    \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}}
    \item{\code{Math}}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
      \code{"ceiling"}, \code{"floor"}, \code{"trunc"}, \code{"cummax"},
      \code{"cummin"}, \code{"cumprod"}, \code{"cumsum"}, \code{"log"},
      \code{"log10"}, \code{"log2"}, \code{"log1p"}, \code{"acos"},
      \code{"acosh"}, \code{"asin"}, \code{"asinh"}, \code{"atan"},
      \code{"atanh"}, \code{"exp"}, \code{"expm1"}, \code{"cos"},
      \code{"cosh"}, \code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"},
      \code{"gamma"}, \code{"lgamma"}, \code{"digamma"}, \code{"trigamma"}}
    \item{\code{Math2}}{\code{"round"}, \code{"signif"}}
    \item{\code{Summary}}{\code{"max"}, \code{"min"}, \code{"range"},
      \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}}
    \item{\code{Complex}}{\code{"Arg"}, \code{"Conj"}, \code{"Im"},
      \code{"Mod"}, \code{"Re"}}
  }
  See \link[methods]{S4groupGeneric} for more details.
}

\section{General Methods}{
  In the code snippets below, \code{x} is an Rle object:

  \describe{
    \item{}{
      \code{x[i, drop = !is.null(getOption("dropRle")) && getOption("dropRle")]}:
      Subsets \code{x} by index \code{i}, where \code{i} can be positive
      integers, negative integers, a logical vector of the same length as
      \code{x}, an Rle object of the same length as \code{x}
      containing logical values, or an \link{IRanges} object.
      When \code{drop = FALSE} returns an Rle object. When \code{drop = TRUE},
      returns an atomic vector.
    }
    \item{}{
      \code{x \%in\% table}:
      Returns a logical Rle representing set membership in
      \code{table}.
    }
    \item{}{
      \code{aggregate(x, by, FUN, start = NULL, end = NULL, width = NULL,
                      frequency = NULL, delta = NULL, ..., simplify = TRUE))}:
      Generates summaries on the specified windows and returns the result in a
      convenient form:
      \describe{
        \item{\code{by}}{An object with \code{start}, \code{end}, and
          \code{width} methods.}
        \item{\code{FUN}}{The function, found via \code{match.fun}, to be
          applied to each window of \code{x}.}
        \item{\code{start}, \code{end}, \code{width}}{the start, end, or width
          of the window. If \code{by} is missing, then must supply two of the
          three.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
        \item{\dots}{Further arguments for \code{FUN}.}
        \item{\code{simplify}}{A logical value specifying whether or not the
          result should be simplified to a vector or matrix if possible.}
      }
    }
    \item{}{
      \code{c(x, ...)}:
      Combines a set of Rle objects.
    }
    \item{}{
      \code{findRange(x, vec)}:
      Returns an \link{IRanges} object representing the ranges in Rle \code{vec}
      that are referenced by the indices in the integer vector \code{x}.
    }
    \item{}{
      \code{findRun(x, vec)}:
      Returns an integer vector indicating the run indices in Rle \code{vec}
      that are referenced by the indices in the integer vector \code{x}.
    }
    \item{}{
      \code{head(x, n = 6L)}:
      If \code{n} is non-negative, returns the first n elements of \code{x}.
      If \code{n} is negative, returns all but the last \code{abs(n)} elements
      of \code{x}.
    }
    \item{}{
      \code{is.na(x)}:
      Returns a logical Rle indicating with values are \code{NA}.
    }
    \item{}{
      \code{length(x)}:
      Returns the underlying vector length of \code{x}.
    }
    \item{}{
      \code{rep(x, times, length.out, each)}, \code{rep.int(x, times)}:
      Repeats the values in \code{x} through one of the following conventions:
      \describe{
        \item{\code{times}}{Vector giving the number of times to repeat each
          element if of length \code{length(x)}, or to repeat the whole vector
          if of length 1.}
        \item{\code{length.out}}{Non-negative integer. The desired length of
          the output vector.}
        \item{\code{each}}{Non-negative integer.  Each element of \code{x} is
          repeated \code{each} times.}
      }
    }
    \item{}{
      \code{rev(x)}:
      Reverses the order of the values in \code{x}.
    }
    \item{}{
      \code{shiftApply(SHIFT, X, Y, FUN, ..., OFFSET = 0L, simplify = TRUE, verbose = FALSE)}:
      Let \code{i} be the indices in \code{SHIFT},
      \code{X_i = window(X, 1 + OFFSET, length(X) - SHIFT[i])}, and
      \code{Y_i = window(Y, 1 + SHIFT[i], length(Y) - OFFSET)}. Calculates
      the set of \code{FUN(X_i, Y_i, ...)} values and return the results in a
      convenient form:
      \describe{
        \item{\code{SHIFT}}{A non-negative integer vector of shift values.}
        \item{\code{X}, \code{Y}}{The Rle objects to shift.}
        \item{\code{FUN}}{The function, found via \code{match.fun}, to be
          applied to each set of shifted vectors.}
        \item{\dots}{Further arguments for \code{FUN}.}
        \item{OFFSET}{A non-negative integer offset to maintain throughout
          the shift operations.}
        \item{\code{simplify}}{A logical value specifying whether or not the
          result should be simplified to a vector or matrix if possible.}
        \item{\code{verbose}}{A logical value specifying whether or not to
          print the \code{i} indices to track the iterations.}
      }
    }
    \item{}{
      \code{show(object)}:
      Prints out the Rle object in a user-friendly way.
    }
    \item{}{
      \code{sort(x, decreasing = FALSE, na.last = NA)}:
      Sorts the values in \code{x}.
      \describe{
        \item{\code{decreasing}}{If \code{TRUE}, sort values in decreasing
          order. If \code{FALSE}, sort values in increasing order.}
        \item{\code{na.last}}{If \code{TRUE}, missing values are placed last.
          If \code{FALSE}, they are placed first. If \code{NA}, they are
          removed.}
      }
    }
    \item{}{
      \code{seqextract(x, start = NULL, end = NULL, width = NULL)}:
      Creates a new sequence using consecutive subsequences from \code{x}
      specified by two of the three following values:  \code{start}, \code{end},
      and \code{width}. See \link{seqextract} for more details.
    }
    \item{}{
      \code{summary(object, ..., digits = max(3, getOption("digits") - 3))}:
      Summarizes the Rle object using an atomic vector convention. The
      \code{digits} argument is used for number formatting with
      \code{signif()}.
    }
    \item{}{
      \code{table(...)}:
      Returns a table containing the counts of the unique values.
    }
    \item{}{
      \code{tail(x, n = 6L)}:
      If \code{n} is non-negative, returns the last n elements of \code{x}.
      If \code{n} is negative, returns all but the first \code{abs(n)} elements
      of \code{x}.
    }
    \item{}{
      \code{unique(x, incomparables = FALSE, ...)}:
      Returns the unique run values. The \code{incomparables} argument takes a
      vector of values that cannot be compared with \code{FALSE} being a special
      value that means that all values can be compared. 
    }
    \item{}{
      \code{window(x, start = NULL, end = NULL, width = NULL, frequency = NULL, delta = NULL, ...)}:
      Extract the subsequence window from \code{x} specified by:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
      }
    }
  }
}

\section{Logical Data Methods}{
  In the code snippets below, \code{x} is an Rle object:

  \describe{
    \item{}{
      \code{!x}:
       Returns logical negation (NOT) of \code{x}.
    }
    \item{}{
      \code{which(x)}:
       Returns an integer vector representing the \code{TRUE} indices of
       \code{x}.
    }
  }
}

\section{Numerical Data Methods}{
  In the code snippets below, \code{x} is an Rle object:

  \describe{
    \item{}{
       \code{pmax(..., na.rm = FALSE)}, \code{pmax.int(..., na.rm = FALSE)}:
       Parallel maxima of the Rle input values. Removes \code{NA}s when
       \code{na.rm = TRUE}.
    }
    \item{}{
       \code{pmin(..., na.rm = FALSE)}, \code{pmin.int(..., na.rm = FALSE)}:
       Parallel minima of the Rle input values. Removes \code{NA}s when
       \code{na.rm = TRUE}.
    }
    \item{}{
      \code{diff(x, lag = 1, differences = 1}:
      Returns suitably lagged and iterated differences of \code{x}.
      \describe{
         \item{\code{lag}}{An integer indicating which lag to use.}
         \item{\code{differences}}{An integer indicating the order of the
           difference.}
      }
    }
    \item{}{
      \code{mean(x, na.rm = FALSE)}:
      Calculates the mean of \code{x}. Removes \code{NA}s when
      \code{na.rm = TRUE}.
    }
    \item{}{
      \code{var(x, y = NULL, na.rm = FALSE)}:
      Calculates the variance of \code{x} or covariance of \code{x} and \code{y}
      if both are supplied. Removes \code{NA}s when \code{na.rm = TRUE}.
    }
    \item{}{
      \code{cov(x, y, use = "everything")}, \code{cor(x, y, use = "everything")}:
      Calculates the covariance and correlation respectively of Rle objects
      \code{x} and \code{y}.
      The \code{use} argument is an optional character string giving a method for
      computing covariances in the presence of missing values. This must be
      (an abbreviation of) one of the strings \code{"everything"},
      \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or
      \code{"pairwise.complete.obs"}.
    }
    \item{}{
      \code{sd(x, na.rm = FALSE)}:
      Calculates the standard deviation of \code{x}. Removes \code{NA}s
      when \code{na.rm = TRUE}.
    }
    \item{}{
      \code{median(x, na.rm = FALSE)}:
      Calculates the median of \code{x}. Removes \code{NA}s when
      \code{na.rm = TRUE}.
    }
    \item{}{
      \code{quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7, ...)}:
      Calculates the specified quantiles of \code{x}.
      \describe{
        \item{\code{probs}}{A numeric vector of probabilities with values in
          [0,1].}
        \item{\code{na.rm}}{If \code{TRUE}, removes \code{NA}s from \code{x}
          before the quantiles are computed.}
        \item{\code{names}}{If \code{TRUE}, the result has names describing the
          quantiles.}
        \item{\code{type}}{An integer between 1 and 9 selecting one of the nine
          quantile algorithms detailed in \code{\link[stats]{quantile}}.}
        \item{\dots}{Further arguments passed to or from other methods.}
      }
    }
    \item{}{
      \code{mad(x, center = median(x), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE)}:
      Calculates the median absolute deviation of \code{x}.
      \describe{
        \item{\code{center}}{The center to calculate the deviation from.}
        \item{\code{constant}}{The scale factor.}
        \item{\code{na.rm}}{If \code{TRUE}, removes \code{NA}s from \code{x}
          before the mad is computed.}
        \item{\code{low}}{If \code{TRUE}, compute the 'lo-median'.}
        \item{\code{high}}{If \code{TRUE}, compute the 'hi-median'.}
      }
    }
  }
}

\section{Character Data Methods}{
  In the code snippets below, \code{x} is an Rle object:

  \describe{
    \item{}{
      \code{nchar(x, type = "chars", allowNA = FALSE)}:
      Returns an integer Rle representing the number of characters in the
      corresponding values of \code{x}.
      \describe{
         \item{\code{type}}{One of \code{c("bytes", "chars", "width")}.}
         \item{\code{allowNA}}{Should \code{NA} be returned for invalid multibyte
           strings rather than throwing an error?}
      }
    }
    \item{}{
      \code{substr(x, start, stop)}, \code{substring(text, first, last = 1000000L)}:
      Returns a character Rle containing the specified substrings beginning at
      \code{start}/\code{first} and ending at \code{stop}/\code{last}.
    }
    \item{}{
      \code{chartr(old, new, x)}:
      Returns a character translated version of \code{x}.
      \describe{
         \item{\code{old}}{A character string specifying the characters to be
           translated.}
         \item{\code{new}}{A character string specifying the translations.}
      }
    }
    \item{}{
      \code{tolower(x)}:
      Returns a lower case version of \code{x}.
    }
    \item{}{
      \code{toupper(x)}:
      Returns an upper case version of \code{x}.
    }
    \item{}{
      \code{sub(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
                perl = FALSE, fixed = FALSE, useBytes = FALSE)}:
      Returns a character Rle with replacements based on matches determined by
      regular expression matching. See \code{\link{sub}} for a description of the
      arguments.
    }
    \item{}{
      \code{gsub(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
                 perl = FALSE, fixed = FALSE, useBytes = FALSE)}:
      Returns a character Rle with replacements based on matches determined by
      regular expression matching. See \code{\link{gsub}} for a description of the
      arguments.
    }
  }
}

\author{P. Aboyoun}

\seealso{
  \code{\link[base]{rle}},
  \link{Sequence-class},
  \link[methods]{S4groupGeneric},
  \link{IRanges-class}
}

\examples{
  x <- Rle(10:1, 1:10)
  x

  runLength(x)
  runValue(x)
  nrun(x)

  diff(x)
  unique(x)
  sort(x)
  sqrt(x)
  x^2 + 2 * x + 1
  x[c(1,3,5,7,9)]
  window(x, 4, 14)
  range(x)
  table(x)
  sum(x)
  mean(x)
  x > 4
  aggregate(x, x > 4, mean)
  aggregate(x, FUN = mean, start = 1:(length(x) - 50), end = 51:length(x))

  y <- Rle(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE))
  y
  as.vector(y)
  rep(y, 10)
  c(y, x > 5)

  z <- c("the", "quick", "red", "fox", "jumps", "over", "the", "lazy", "brown", "dog")
  z <- Rle(z, seq_len(length(z)))
  chartr("a", "@", z)
  toupper(z)
}

\keyword{methods}
\keyword{classes}
