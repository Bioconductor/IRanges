\name{GenomicData-class}
\docType{class}
\alias{GenomicData-class}

% constructor
\alias{GenomicData}

% accessors
\alias{chrom}
\alias{chrom,GenomicData-method}
\alias{genome}
\alias{genome,GenomicData-method}
\alias{strand}
\alias{strand,GenomicData-method}
\alias{strand,missing-method}

\title{Data on a Genome}
\description{\code{GenomicData} extends \code{RangedData} to more
  conveniently manipulate data
  on genomic ranges. The spaces are now called chromosomes (but could
  still refer to some other type of sequence). The annotation refers to
  the genome and there is formal treatment of an optional \code{strand}
  variable.}

\section{Accessors}{
  The \code{GenomicData} class essentially adds a set of convenience
  accessors on top of \code{RangedData}.   In the code snippets below,
  \code{x} is a \code{RangedData} object.

  \describe{
    \item{}{
      \code{chrom(x)}: Gets the chromosome names (a factor) over the
      ranges in \code{x}.
    }
    \item{}{
      \code{genome(x)}: Gets the genome for the ranges in \code{x}; a simple
      wrapper around \code{annotation(x)}.
    }
    \item{}{
      \code{strand(x)}: Gets the strand factor in \code{x}, or,
      if \code{x} is missing, return an empty factor with the standard
      levels: \code{-}, \code{+} and \code{*}, referring to the negative,
      positive and both strands, respectively. Any strand factor stored in
      \code{GenomicData} should have those levels.
      \code{NA}'s are allowed; the value is all \code{NA} if no strand
      has been specified. 
    }
  }
}

\section{Constructor}{
  \describe{
    \item{}{
      \code{GenomicData(ranges, ..., strand = NULL, chrom = NULL,
        genome = NULL)}: Constructs a \code{GenomicData} instance with
      the given \code{ranges} and variables in \code{...} (see the
      \code{\link{RangedData}} constructor). If
      non-\code{NULL}, \code{strand} specifies the strand of each
      range. It should be a character vector or factor of length equal to
      that of \code{ranges}. All values should be either \code{-}, \code{+},
      \code{*} or \code{NA}. To get these levels, call
      \code{levels(strand())}. \code{chrom} is analogous to
      \code{splitter} in \code{\link{RangedData}}; if non-\code{NULL} it
      should be coercible to a factor indicating how the ranges,
      variables and strand should be split up 
      across the chromosomes. The \code{genome} argument should be a
      scalar string and is treated as the \code{RangedData}
      annotation. See the examples.
    }
  }
}

\author{ Michael Lawrence }
\note{This may move to another package in the future, as it is not
  quite general enough for IRanges.}

\seealso{
  \code{\linkS4class{RangedData}}, on which this class is based.
}
\examples{
  range1 <- IRanges(start=c(1,2,3), end=c(5,2,8))

  ## just ranges
  gr <- GenomicData(range1) 

  ## with a genome (annotation)
  gr <- GenomicData(range1, genome = "hg18")
  genome(gr) ## "hg18"

  ## with some data
  filter <- c(1L, 0L, 1L)
  score <- c(10L, 2L, NA)
  strand <- factor(c("+", NA, "-"), levels = levels(strand()))
  gr <- GenomicData(range1, score, genome = "hg18")
  gr[["score"]]
  strand(gr) ## all NA
  gr <- GenomicData(range1, score, filt = filter, strand = strand)
  gr[["filt"]]
  strand(gr) ## equal to 'strand'

  range2 <- IRanges(start=c(15,45,20,1), end=c(15,100,80,5))
  ranges <- c(range1, range2)
  score <- c(score, c(0L, 3L, NA, 22L)) 
  chrom <- paste("chr", rep(c(1,2), c(length(range1), length(range2))), sep="")
  
  gr <- GenomicData(ranges, score, chrom = chrom, genome = "hg18")
  chrom(gr) # equal to 'chrom'
  gr[["score"]] # unlists over the chromosomes
  gr[1][["score"]] # equal to score[1:3]
}
\keyword{classes}
\keyword{methods}