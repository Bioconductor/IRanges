\name{MaskCollection-class}
\docType{class}

% Classes:
\alias{class:MaskCollection}
\alias{MaskCollection-class}
\alias{MaskCollection}

% Basic accessor methods:
\alias{nir_list}
\alias{nir_list,MaskCollection-method}
\alias{length,MaskCollection-method}
\alias{width,MaskCollection-method}
\alias{active}
\alias{active,MaskCollection-method}
\alias{active<-}
\alias{active<-,MaskCollection-method}
\alias{names,MaskCollection-method}
\alias{names<-,MaskCollection-method}
\alias{desc}
\alias{desc,MaskCollection-method}
\alias{desc<-}
\alias{desc<-,MaskCollection-method}

% Constructor:
\alias{Mask}

% Other methods:
\alias{max,MaskCollection-method}
\alias{min,MaskCollection-method}
\alias{maskedwidth}
\alias{maskedwidth,MaskCollection-method}
\alias{maskedratio}
\alias{maskedratio,MaskCollection-method}

% Subsetting and appending:
\alias{[[,MaskCollection-method}
\alias{[,MaskCollection-method}
\alias{append,MaskCollection,MaskCollection-method}

% Endomorphisms:
\alias{collapse}
\alias{collapse,MaskCollection-method}

% Coercion:
\alias{coerce,MaskCollection,NormalIRanges-method}

% "show" method:
\alias{MaskCollection.show_frame}
\alias{show,MaskCollection-method}


\title{MaskCollection objects}

\description{
  The MaskCollection class is a container for storing a collection of masks
  that can be used to mask regions in a sequence.
}

\details{
  In the context of the Biostrings package, a mask is a set of regions
  in a sequence that need to be excluded from some computation.
  For example, when calling \code{\link[Biostrings:letterFrequency]{alphabetFrequency}}
  or \code{\link[Biostrings]{matchPattern}} on a chromosome sequence,
  you might want to exclude some regions like the centromere or the repeat
  regions. This can be achieved by putting one or several masks on the sequence
  before calling \code{\link[Biostrings:letterFrequency]{alphabetFrequency}} on it.

  A MaskCollection object is a vector-like object that represents
  such set of masks.
  Like standard R vectors, it has a "length" which is the number of
  masks contained in it. But unlike standard R vectors, it also has
  a "width" which determines the length of the sequences it can be "put on".
  For example, a MaskCollection object of width 20000 can only be put on
  an \link[Biostrings:XString-class]{XString} object of 20000 letters.

  Each mask in a MaskCollection object \code{x} is just a finite set of
  integers that are >= 1 and <= \code{width(x)}.
  When "put on" a sequence, these integers indicate the positions of the
  letters to mask.
  Internally, each mask is represented by a \link{NormalIRanges}
  object.
}

\section{Basic accessor methods}{
  In the code snippets below, \code{x} is a MaskCollection object.

  \describe{
    \item{}{
      \code{length(x)}:
      The number of masks in \code{x}.
    }
    \item{}{
      \code{width(x)}:
      The common with of all the masks in \code{x}.
      This determines the length of the sequences that \code{x} can be
      "put on".
    }
    \item{}{
      \code{active(x)}:
      A logical vector of the same length as \code{x} where each
      element indicates whether the corresponding mask is active or not.
    }
    \item{}{
      \code{names(x)}:
      \code{NULL} or a character vector of the same length as \code{x}.
    }
    \item{}{
      \code{desc(x)}:
      \code{NULL} or a character vector of the same length as \code{x}.
    }
    \item{}{
      \code{nir_list(x)}:
      A list of the same length as \code{x}, where each element is
      a \link{NormalIRanges} object representing a mask in \code{x}.
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{Mask(mask.width, start=NULL, end=NULL, width=NULL)}:
      Return a single mask (i.e. a MaskCollection object of length 1)
      of width \code{mask.width} (a single integer >= 1)
      and masking the ranges of positions specified by \code{start},
      \code{end} and \code{width}.
      See the \code{\link{IRanges}} constructor (\code{?\link{IRanges}})
      for how \code{start}, \code{end} and \code{width} can be specified.
      Note that the returned mask is active and unnamed.
    }
  }
}

\section{Other methods}{
  In the code snippets below, \code{x} is a MaskCollection object.

  \describe{
    \item{}{
      \code{isEmpty(x)}:
      Return a logical vector of the same length as \code{x}, indicating,
      for each mask in \code{x}, whether it's empty or not.
    }
    \item{}{
      \code{max(x)}:
      The greatest (or last, or rightmost) masked position for each mask.
      This is a numeric vector of the same length as \code{x}.
    }
    \item{}{
      \code{min(x)}:
      The smallest (or first, or leftmost) masked position for each mask.
      This is a numeric vector of the same length as \code{x}.
    }
    \item{}{
      \code{maskedwidth(x)}:
      The number of masked position for each mask.
      This is an integer vector of the same length as \code{x} where
      all values are >= 0 and <= \code{width(x)}.
    }
    \item{}{
      \code{maskedratio(x)}:
      \code{maskedwidth(x) / width(x)}
    }
  }
}

\section{Subsetting and appending}{
  In the code snippets below,
  \code{x} and \code{values} are MaskCollection objects.

  \describe{
    \item{}{
      \code{x[i]}:
      Return a new MaskCollection object made of the selected masks.
      Subscript \code{i} can be a numeric, logical or character vector.
    }
    \item{}{
      \code{x[[i, exact=TRUE]]}:
      Extract the mask selected by \code{i} as a \link{NormalIRanges} object.
      Subscript \code{i} can be a single integer or a character string.
    }
    \item{}{
      \code{append(x, values, after=length(x))}:
      Add masks in \code{values} to \code{x}.
    }
  }
}

\section{Other methods}{
  In the code snippets below, \code{x} is a MaskCollection object.

  \describe{
    \item{}{
      \code{collapse(x)}:
      Return a MaskCollection object of length 1 obtained by collapsing
      all the active masks in \code{x}.
    }
  }
}

\author{H. Pages}

\seealso{
  \link{NormalIRanges-class},
  \link{read.Mask},
  \link[Biostrings]{MaskedXString-class},
  \code{\link{reverse}},
  \code{\link[Biostrings]{alphabetFrequency}},
  \code{\link[Biostrings]{matchPattern}}
}

\examples{
  ## Making a MaskCollection object:
  mask1 <- Mask(mask.width=29, start=c(11, 25, 28), width=c(5, 2, 2))
  mask2 <- Mask(mask.width=29, start=c(3, 10, 27), width=c(5, 8, 1))
  mask3 <- Mask(mask.width=29, start=c(7, 12), width=c(2, 4))
  mymasks <- append(append(mask1, mask2), mask3)
  mymasks
  length(mymasks)
  width(mymasks)
  collapse(mymasks)

  ## Names and descriptions:
  names(mymasks) <- c("A", "B", "C")  # names should be short and unique...
  mymasks
  mymasks[c("C", "A")]  # ...to make subsetting by names easier
  desc(mymasks) <- c("you can be", "more verbose", "here")
  mymasks[-2]

  ## Activate/deactivate masks:
  active(mymasks)["B"] <- FALSE
  mymasks
  collapse(mymasks)
  active(mymasks) <- FALSE  # deactivate all masks
  mymasks
  active(mymasks)[-1] <- TRUE  # reactivate all masks except mask 1
  active(mymasks) <- !active(mymasks)  # toggle all masks

  ## Other advanced operations:
  mymasks[[2]]
  length(mymasks[[2]])
  mymasks[[2]][-3]
  append(mymasks[-2], gaps(mymasks[2]))
}

\keyword{methods}
\keyword{classes}
