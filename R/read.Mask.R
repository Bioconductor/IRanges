### =========================================================================
### Read a mask from a file
### -----------------------
###
### From an NCBI "agp" file (for chrY in hs b36v3):
###   library(BSgenome.Hsapiens.NCBI.b36v3)
###   file1 <- system.file("extdata", "hs_b36v3_chrY.agp", package="IRanges")
###   mask1 <- read.agpMask(file1, seqname="chrY", mask.width=length(Hsapiens$chrY))
###
### From an UCSC "gap" file (for chrY in hg18):
###   library(BSgenome.Hsapiens.UCSC.hg18)
###   file2 <- system.file("extdata", "chrY_gap.txt", package="IRanges")
###   mask2 <- read.gapMask(file2, seqname="chrY", mask.width=length(Hsapiens$chrY))
###
### From an UCSC "lift" file (for hg18):
###   file3 <- system.file("extdata", "hg18liftAll.lft", package="IRanges")
###   mask3 <- read.liftMask(file3, seqname="chr1")
###
### From a RepeatMasker .out file (for chrM in ce2):
###   library(BSgenome.Celegans.UCSC.ce2)
###   file4 <- system.file("extdata", "ce2chrM.fa.out", package="IRanges")
###   mask4 <- read.rmMask(file4, seqname="chrM", mask.width=length(Celegans$chrM)) 
###
### From a Tandem Repeats Finder .bed file (for chrM in ce2):
###   file5 <- system.file("extdata", "ce2chrM.bed", package="IRanges")
###   mask5 <- read.trfMask(file5, seqname="chrM", mask.width=length(Celegans$chrM)) 
###
### -------------------------------------------------------------------------


.showDistinctSeqnamesAndStop <- function(seqnames)
{
    distinct_seqnames <- paste("\"", unique(seqnames), "\"", sep="")
    distinct_seqnames <- paste(distinct_seqnames, collapse=", ")
    stop(length(distinct_seqnames), " distinct seqnames found in this file: ", distinct_seqnames)
}

.newEmptyMask <- function(seqname, mask.width, mask.name, mask.desc, nofound_what="information")
{
    msg <- paste("No ", nofound_what, " found for sequence \"",
                 seqname, "\" in this file. ", sep="")
    if (is.na(mask.width))
        stop(msg, "Please use the\n",
             "  'mask.width' argument to specify the width of the empty mask to\n",
             "  return (i.e. the length of the sequence this mask will be put on).")
    warning(msg, "returning empty mask")
    ans <- Mask(mask.width)  # empty mask
    names(ans) <- mask.name
    desc(ans) <- paste(mask.desc, "(empty)")
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### read.agpMask() and read.gapMask() extract the AGAPS mask from an NCBI
### "agp" file or a UCSC "gap" file, respectively.
###

.guessGapFileCOL2CLASS <- function(file)
{
    ## UCSC "gap" files generally have the 9 columns below except for some
    ## organisms like Rhesus that have only 8 columns (no 'bin' column).
    COL2CLASS <- c(
        `bin`="integer",
        `chrom`="character",
        `chr_start`="integer",
        `chr_stop`="integer",
        `part_no`="integer",
        `part_type`="character",
        `gap_len`="integer",
        `gap_type`="character",
        `bridge`="character"
    )
    line1 <- try(read.table(file, sep="\t",
                            col.names=names(COL2CLASS),
                            colClasses=COL2CLASS,
                            nrows=1L,
                            check.names=FALSE),
                 silent=TRUE)
    if (!inherits(line1, "try-error"))
        return(COL2CLASS)
    COL2CLASS <- COL2CLASS[-1L]
    line1 <- try(read.table(file, sep="\t",
                            col.names=names(COL2CLASS),
                            colClasses=COL2CLASS,
                            nrows=1L,
                            check.names=FALSE),
                 silent=TRUE)
    if (!inherits(line1, "try-error"))
        return(COL2CLASS)
    stop("unable to guess the column names in \"gap\" file '", file, "', sorry")
}

.read.agpORgapFile <- function(agp_or_gap, file)
{
    if (agp_or_gap == "agp") {
        COL2CLASS <- c(
            `chrom`="character",
            `chr_start`="integer",
            `chr_stop`="integer",
            `part_no`="integer",
            `part_type`="character",
            `gap_len`="character",
            `gap_type`="character",
            `linkage`="character",
            `empty`="character"
        )
    } else if (agp_or_gap == "gap") {
        COL2CLASS <- .guessGapFileCOL2CLASS(file)
    } else {
        stop("read.Mask internal error: please report")
    }
    COLS <- c(
        "chrom",
        "chr_start",
        "chr_stop",
        "part_type",
        "gap_len",
        "gap_type"
    )
    COL2CLASS[!(names(COL2CLASS) %in% COLS)] <- "NULL"
    data <- read.table(file,
                       sep="\t",
                       col.names=names(COL2CLASS),
                       colClasses=COL2CLASS,
                       check.names=FALSE,
                       fill=TRUE)
}

.read.agpORgapMask <- function(agp_or_gap, file, seqname, mask.width,
                               gap.types, use.gap.types)
{
    if (!isSingleString(seqname))
        stop("'seqname' must be a single string")
    if (!isSingleNumberOrNA(mask.width))
        stop("'mask.width' must be a single integer or 'NA'")
    if (!is.integer(mask.width))
        mask.width <- as.integer(mask.width)
    if (!is.null(gap.types) && (!is.character(gap.types)
                                || anyMissing(gap.types)
                                || anyDuplicated(gap.types)))
        stop("'gap.types' must be 'NULL' or a character vector ",
             "with no NAs and no duplicated")
    if (!isTRUEorFALSE(use.gap.types))
        stop("'use.gap.types' must be TRUE or FALSE")
    data <- .read.agpORgapFile(agp_or_gap, file)
    if (seqname == "?")
        .showDistinctSeqnamesAndStop(data$chrom)
    data <- data[data$chrom == seqname, ]
    ii <- data$part_type == "N"
    if (agp_or_gap == "agp") {
        data <- data[ii, ]
    } else if (!all(ii)) {
        warning("gap file contains gaps with a part_type that is not N")
    }
    if (length(gap.types) == 1 && gap.types == "?") {
        found_types <- paste("\"", unique(data$gap_type), "\"", sep="")
        found_types <- paste(found_types, collapse=", ")
        stop("gap types found in this file for sequence \"", seqname, "\": ", found_types)
    }
    mask.name <- "AGAPS"
    mask.desc <- "assembly gaps"
    if (!is.null(gap.types)) {
        data <- data[data$gap_type %in% gap.types, ]
        mask.desc <- paste(mask.desc, " [type=", paste(gap.types, collapse="|"), "]", sep="")
    }
    if (nrow(data) == 0)
        return(.newEmptyMask(seqname, mask.width, mask.name, mask.desc, mask.desc))
    if (agp_or_gap == "agp")
        ranges_start <- data$chr_start
    else
        ranges_start <- data$chr_start + 1L
    ranges <- IRanges(start=ranges_start, width=as.integer(data$gap_len))
    ## Sanity check
    if (!identical(end(ranges), data$chr_stop))
        stop("broken \"", agp_or_gap, "\" file: contains inconsistent ",
             "chr_start/chr_stop/gap_len values ",
             "for assembly gaps in sequence \"", seqname, "\"")
    if (use.gap.types) {
        names(ranges) <- data$gap_type
        if (isNotStrictlySorted(start(ranges)))
            ranges <- ranges[orderInteger(start(ranges))]
        if (!isNormal(ranges))
            stop("cannot use the gap types when some gaps are adjacent or overlap")
        nir1 <- asNormalIRanges(ranges, force=FALSE)
    } else {
        nir1 <- asNormalIRanges(ranges, force=TRUE)
    }
    ## Don't use new2(): the validity of the new mask needs to be checked!
    new2("MaskCollection", nir_list=list(nir1), width=mask.width, active=TRUE,
                           NAMES=mask.name, desc=mask.desc, check=FALSE)
}

read.agpMask <- function(file, seqname="?", mask.width=NA, gap.types=NULL, use.gap.types=FALSE)
    .read.agpORgapMask("agp", file, seqname, mask.width, gap.types, use.gap.types)

read.gapMask <- function(file, seqname="?", mask.width=NA, gap.types=NULL, use.gap.types=FALSE)
    .read.agpORgapMask("gap", file, seqname, mask.width, gap.types, use.gap.types)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### read.liftMask() extracts the AGAPS mask from a UCSC "lift" file.
###

.read.liftFile <- function(file)
{
    COL2CLASS <- c(
        `offset`="integer",
        `xxxx`="NULL",  # not sure how to call this
        `width`="integer",
        `seqname`="character",
        `seqlen`="integer"
    )
    read.table(file,
               col.names=names(COL2CLASS),
               colClasses=COL2CLASS,
               check.names=FALSE)
}

read.liftMask <- function(file, seqname="?", mask.width=NA)
{
    if (!isSingleString(seqname))
        stop("'seqname' must be a single string")
    if (!isSingleNumberOrNA(mask.width))
        stop("'mask.width' must be a single integer or 'NA'")
    if (!is.integer(mask.width))
        mask.width <- as.integer(mask.width)
    data <- .read.liftFile(file)
    if (seqname == "?")
        .showDistinctSeqnamesAndStop(data$seqname)
    data <- data[data$seqname == seqname, ]
    if (nrow(data) == 0)
        return(.newEmptyMask(seqname, mask.width, "AGAPS", "assembly gaps"))
    ## Sanity checks
    seqlen0 <- unique(data$seqlen)
    if (length(seqlen0) != 1)
        stop("broken \"lift\" file: contains different lengths ",
             "for sequence \"", seqname, "\"")
    if (!is.na(mask.width) && mask.width != seqlen0)
        stop("when supplied, 'mask.width' must match the length found ",
             "in the file for sequence \"", seqname, "\"")
    contigs0 <- IRanges(start=data$offset+1, width=data$width)
    contigs1 <- asNormalIRanges(contigs0, force=TRUE)
    if (length(contigs1) != length(contigs0))
        warning("some contigs are adjacent or overlapping")
    contigs <- Mask(seqlen0, start=start(contigs1), width=width(contigs1))
    ans <- gaps(contigs)
    names(ans) <- "AGAPS"
    desc(ans) <- "assembly gaps"
    ans
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### read.rmMask() extracts the RM mask from a RepeatMasker .out file.
###
### See http://www.repeatmasker.org/webrepeatmaskerhelp.html for a
### description of the RepeatMasker output format.
###

.read.rmFile <- function(file)
{
    COL2CLASS <- c(
        `SW_score`="integer",
        `perc_div`="numeric",
        `perc_del`="numeric",
        `perc_ins`="numeric",
        `query_sequence`="character",
        `begin_in_query`="integer",
        `end_in_query`="integer",
        `left_in_query`="character",
        `C`="character", 
        `matching_repeat`="character",
        `repeat_class_or_family`="character",
        `begin_in_repeat`="integer",
        `end_in_repeat`="integer",
        `left_in_repeat`="character",
        `ID`="character"
    )
    COLS <- c("query_sequence", "begin_in_query", "end_in_query", "ID")
    COL2CLASS[!(names(COL2CLASS) %in% COLS)] <- "NULL"
    read.table(file,
               col.names=names(COL2CLASS),
               colClasses=COL2CLASS,
               skip=3,
               check.names=FALSE)
}

read.rmMask <- function(file, seqname="?", mask.width=NA, use.IDs=FALSE)
{
    if (!isSingleString(seqname))
        stop("'seqname' must be a single string")
    if (!isSingleNumberOrNA(mask.width))
        stop("'mask.width' must be a single integer or 'NA'")
    if (!is.integer(mask.width))
        mask.width <- as.integer(mask.width)
    if (!isTRUEorFALSE(use.IDs))
        stop("'use.IDs' must be TRUE or FALSE")
    data <- .read.rmFile(file)
    if (seqname == "?")
        .showDistinctSeqnamesAndStop(data$query_sequence)
    data <- data[data$query_sequence == seqname, ]
    if (nrow(data) == 0)
        return(.newEmptyMask(seqname, mask.width, "RM", "RepeatMasker"))
    ranges <- IRanges(start=data$begin_in_query, end=data$end_in_query)
    if (use.IDs) {
        names(ranges) <- data$ID
        if (isNotStrictlySorted(start(ranges)))
            ranges <- ranges[orderInteger(start(ranges))]
        if (!isNormal(ranges))
            stop("cannot use the repeat IDs when some repeats are adjacent or overlap")
        nir1 <- asNormalIRanges(ranges, force=FALSE)
    } else {
        nir1 <- asNormalIRanges(ranges, force=TRUE)
    }
    ## Don't use new2(): the validity of the new mask needs to be checked!
    new2("MaskCollection", nir_list=list(nir1), width=mask.width, active=TRUE,
                           NAMES="RM", desc="RepeatMasker", check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### read.trfMask() extracts the TRF mask from a Tandem Repeats Finder .bed
### file.
###

.read.trfFile <- function(file)
{
    COL2CLASS <- c(
        `chrom`="character",
        `chromStart`="integer",
        `chromEnd`="integer",
        `name`="character",
        `period`="integer",
        `copyNum`="numeric",
        `consensusSize`="integer",
        `perMatch`="integer",
        `perIndel`="integer",
        `score`="integer",
        `A`="integer",
        `C`="integer",
        `G`="integer",
        `T`="integer",
        `entropy`="numeric",
        `sequence`="character"
    )
    COLS <- c("chrom", "chromStart", "chromEnd")
    COL2CLASS[!(names(COL2CLASS) %in% COLS)] <- "NULL"
    read.table(file,
               col.names=names(COL2CLASS),
               colClasses=COL2CLASS,
               check.names=FALSE)
}

read.trfMask <- function(file, seqname="?", mask.width=NA)
{
    if (!isSingleString(seqname))
        stop("'seqname' must be a single string")
    if (!isSingleNumberOrNA(mask.width))
        stop("'mask.width' must be a single integer or 'NA'")
    if (!is.integer(mask.width))
        mask.width <- as.integer(mask.width)
    data <- .read.trfFile(file)
    if (seqname == "?")
        .showDistinctSeqnamesAndStop(data$chrom)
    data <- data[data$chrom == seqname, ]
    if (nrow(data) == 0)
        return(.newEmptyMask(seqname, mask.width, "TRF", "Tandem Repeats Finder"))
    ranges <- IRanges(start=data$chromStart+1, end=data$chromEnd)
    nir1 <- asNormalIRanges(ranges, force=TRUE)
    ## Don't use new2(): the validity of the new mask needs to be checked!
    new2("MaskCollection", nir_list=list(nir1), width=mask.width, active=TRUE,
                           NAMES="TRF", desc="Tandem Repeats Finder", check=FALSE)
}

