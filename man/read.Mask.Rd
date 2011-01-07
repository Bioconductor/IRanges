\name{read.Mask}

\alias{read.Mask}
\alias{read.agpMask}
\alias{read.gapMask}
\alias{read.liftMask}
\alias{read.rmMask}
\alias{read.trfMask}


\title{Read a mask from a file}

\description{
  \code{read.agpMask} and \code{read.gapMask} extract the AGAPS mask from an
  NCBI "agp" file or a UCSC "gap" file, respectively.

  \code{read.liftMask} extracts the AGAPS mask from a UCSC "lift" file
  (i.e. a file containing offsets of contigs within sequences).

  \code{read.rmMask} extracts the RM mask from a RepeatMasker .out file.

  \code{read.trfMask} extracts the TRF mask from a Tandem Repeats Finder .bed
  file.
}

\usage{
read.agpMask(file, seqname="?", mask.width=NA, gap.types=NULL, use.gap.types=FALSE)
read.gapMask(file, seqname="?", mask.width=NA, gap.types=NULL, use.gap.types=FALSE)
read.liftMask(file, seqname="?", mask.width=NA)
read.rmMask(file, seqname="?", mask.width=NA, use.IDs=FALSE)
read.trfMask(file, seqname="?", mask.width=NA)
}

\arguments{
  \item{file}{
    Either a character string naming a file or a connection open
    for reading.
  }
  \item{seqname}{
    The name of the sequence for which the mask must be extracted.
    If no sequence is specified (i.e. \code{seqname="?"}) then an error is
    raised and the sequence names found in the file are displayed.
    If the file doesn't contain any information for the specified sequence,
    then a warning is issued and an empty mask of width \code{mask.width}
    is returned.
  }
  \item{mask.width}{
    The width of the mask to return i.e. the length of the sequence this
    mask will be put on.
    See \code{?`\link{MaskCollection-class}`} for more information about
    the width of a \link{MaskCollection} object.
  }
  \item{gap.types}{
    \code{NULL} or a character vector containing gap types.
    Use this argument to filter the assembly gaps that are to be extracted
    from the "agp" or "gap" file based on their type. Most common gap types
    are \code{"contig"}, \code{"clone"}, \code{"centromere"}, \code{"telomere"},
    \code{"heterochromatin"}, \code{"short_arm"} and \code{"fragment"}.
    With \code{gap.types=NULL}, all the assembly gaps described in the file
    are extracted.
    With \code{gap.types="?"}, an error is raised and the gap types found
    in the file for the specified sequence are displayed.
  }
  \item{use.gap.types}{
    Whether or not the gap types provided in the "agp" or "gap" file should
    be used to name the ranges constituing the returned mask.
    See \code{?`\link{IRanges-class}`} for more information about the
    names of an \link{IRanges} object.
  }
  \item{use.IDs}{
    Whether or not the repeat IDs provided in the RepeatMasker .out file
    should be used to name the ranges constituing the returned mask.
    See \code{?`\link{IRanges-class}`} for more information about the
    names of an \link{IRanges} object.
  }
}

\seealso{
  \link{MaskCollection-class},
  \link{IRanges-class}
}

\examples{
  ## ---------------------------------------------------------------------
  ## A. Extract a mask of assembly gaps ("AGAPS" mask) with read.agpMask()
  ## ---------------------------------------------------------------------
  ## Note: The hs_b36v3_chrY.agp file was obtained by downloading,
  ## extracting and renaming the hs_ref_chrY.agp.gz file from
  ##
  ##   ftp://ftp.ncbi.nih.gov/genomes/H_sapiens/Assembled_chromosomes/
  ##     hs_ref_chrY.agp.gz      5 KB  24/03/08  04:33:00 PM
  ##
  ## on May 9, 2008.

  chrY_length <- 57772954
  file1 <- system.file("extdata", "hs_b36v3_chrY.agp", package="IRanges")
  mask1 <- read.agpMask(file1, seqname="chrY", mask.width=chrY_length,
                        use.gap.types=TRUE)
  mask1
  mask1[[1]]

  mask11 <- read.agpMask(file1, seqname="chrY", mask.width=chrY_length,
                         gap.types=c("centromere", "heterochromatin"))
  mask11[[1]]

  ## ---------------------------------------------------------------------
  ## B. Extract a mask of assembly gaps ("AGAPS" mask) with read.liftMask()
  ## ---------------------------------------------------------------------
  ## Note: The hg18liftAll.lft file was obtained by downloading,
  ## extracting and renaming the liftAll.zip file from
  ##
  ##   http://hgdownload.cse.ucsc.edu/goldenPath/hg18/bigZips/
  ##     liftAll.zip             03-Feb-2006 11:35  5.5K
  ##
  ## on May 8, 2008.

  file2 <- system.file("extdata", "hg18liftAll.lft", package="IRanges")
  mask2 <- read.liftMask(file2, seqname="chr1")
  mask2
  if (interactive()) {
      ## contigs 7 and 8 for chrY are adjacent
      read.liftMask(file2, seqname="chrY")

      ## displays the sequence names found in the file
      read.liftMask(file2)

      ## specify an unknown sequence name
      read.liftMask(file2, seqname="chrZ", mask.width=300)
  }

  ## ---------------------------------------------------------------------
  ## C. Extract a RepeatMasker ("RM") or Tandem Repeats Finder ("TRF")
  ##    mask with read.rmMask() or read.trfMask()
  ## ---------------------------------------------------------------------
  ## Note: The ce2chrM.fa.out and ce2chrM.bed files were obtained by
  ## downloading, extracting and renaming the chromOut.zip and
  ## chromTrf.zip files from
  ##
  ##   http://hgdownload.cse.ucsc.edu/goldenPath/ce2/bigZips/
  ##     chromOut.zip            21-Apr-2004 09:05  2.6M
  ##     chromTrf.zip            21-Apr-2004 09:07  182K
  ##
  ## on May 7, 2008.

  ## Before you can extract a mask with read.rmMask() or read.trfMask(), you
  ## need to know the length of the sequence that you're going to put the
  ## mask on:
  if (interactive()) {
      library(BSgenome.Celegans.UCSC.ce2)
      chrM_length <- seqlengths(Celegans)[["chrM"]]

      ## Read the RepeatMasker .out file for chrM in ce2:
      file3 <- system.file("extdata", "ce2chrM.fa.out", package="IRanges")
      RMmask <- read.rmMask(file3, seqname="chrM", mask.width=chrM_length)
      RMmask

      ## Read the Tandem Repeats Finder .bed file for chrM in ce2:
      file4 <- system.file("extdata", "ce2chrM.bed", package="IRanges")
      TRFmask <- read.trfMask(file4, seqname="chrM", mask.width=chrM_length)
      TRFmask
      desc(TRFmask) <- paste(desc(TRFmask), "[period<=12]")
      TRFmask

      ## Put the 2 masks on chrM:
      chrM <- Celegans$chrM
      masks(chrM) <- RMmask  # this would drop all current masks, if any
      masks(chrM) <- append(masks(chrM), TRFmask)
      chrM
  }
}

\keyword{manip}
