### =========================================================================
### OverlapEncodings objects
### -------------------------------------------------------------------------
###


setClass("OverlapEncodings",
    contains="Vector",
    representation(
        Loffset="integer",      # no NAs, >= 0
        Roffset="integer",      # no NAs, >= 0
        encoding="factor",      # no NAs
        flippedQuery="logical"  # no NAs
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Slot getters.
###

setGeneric("Loffset", function(x) standardGeneric("Loffset"))
setMethod("Loffset", "OverlapEncodings", function(x) x@Loffset)

setGeneric("Roffset", function(x) standardGeneric("Roffset"))
setMethod("Roffset", "OverlapEncodings", function(x) x@Roffset)

setGeneric("encoding", function(x) standardGeneric("encoding"))
setMethod("encoding", "OverlapEncodings", function(x) x@encoding)

setMethod("levels", "OverlapEncodings", function(x) levels(encoding(x)))

setGeneric("flippedQuery", function(x) standardGeneric("flippedQuery"))
setMethod("flippedQuery", "OverlapEncodings", function(x) x@flippedQuery)

setMethod("length", "OverlapEncodings", function(x) length(encoding(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The Lencoding() and Rencoding() getters.
###

.extract_LRencoding_from_encoding_levels <- function(x, L.or.R)
{
    if (!is.character(x))
        stop("'x' must be a character vector")
    if (length(x) == 0L)
        return(character(0))
    encoding_blocks <- strsplit(x, ":", fixed=TRUE)
    nblock <- elementLengths(encoding_blocks)
    tmp <- strsplit(unlist(encoding_blocks, use.names=FALSE), "--", fixed=TRUE)
    tmp_elt_lens <- elementLengths(tmp)
    tmp_is_single_end <- tmp_elt_lens == 1L
    tmp_is_paired_end <- tmp_elt_lens == 2L
    nblock1 <- sum(LogicalList(relist(tmp_is_single_end, encoding_blocks)))
    nblock2 <- sum(LogicalList(relist(tmp_is_paired_end, encoding_blocks)))
    is_single_end_encoding <- nblock1 == nblock
    is_paired_end_encoding <- nblock2 == nblock
    if (!all(is_single_end_encoding | nblock1 == 0L) ||
        !all(is_paired_end_encoding | nblock2 == 0L) ||
        !all(is_single_end_encoding | is_paired_end_encoding))
        stop("'x' contains ill-formed encodings")
    any_single_end <- any(is_single_end_encoding)
    any_paired_end <- any(is_paired_end_encoding)
    if (any_single_end && any_paired_end)
        warning("'x' contains a mix of single-end and paired-end encodings")
    ans <- character(length(x))
    ans[] <- NA_character_
    if (any_paired_end) {
        tmp2 <- unlist(tmp[tmp_is_paired_end], use.names=FALSE)
        encodings_blocks2 <- encoding_blocks[is_paired_end_encoding]
        if (identical(L.or.R, "L")) {
            tmp2 <- tmp2[c(TRUE, FALSE)]
        } else if (identical(L.or.R, "R")) {
            tmp2 <- tmp2[c(FALSE, TRUE)]
        } else {
            stop("invalid supplied 'L.or.R' argument")
        }
        ans2 <- sapply(relist(tmp2, encodings_blocks2),
                       function(blocks) paste(blocks, collapse=":"))
        ans[is_paired_end_encoding] <- paste(ans2, ":", sep="")
    }
    ans
}

setGeneric("Lencoding", function(x) standardGeneric("Lencoding"))
setGeneric("Rencoding", function(x) standardGeneric("Rencoding"))

setMethod("Lencoding", "character",
    function(x) .extract_LRencoding_from_encoding_levels(x, L.or.R="L")
)
setMethod("Rencoding", "character",
    function(x) .extract_LRencoding_from_encoding_levels(x, L.or.R="R")
)

setMethod("Lencoding", "factor",
    function(x)
    {
        levels_Lencoding <- Lencoding(levels(x))
        factor(levels_Lencoding)[as.integer(x)]
    }
)
setMethod("Rencoding", "factor",
    function(x)
    {
        levels_Rencoding <- Rencoding(levels(x))
        factor(levels_Rencoding)[as.integer(x)]
    }
)

setMethod("Lencoding", "OverlapEncodings", function(x) Lencoding(encoding(x)))
setMethod("Rencoding", "OverlapEncodings", function(x) Rencoding(encoding(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The ngap(), Lngap(), and Rngap() getters.
###

.extract_ngap_from_encoding_levels <- function(x, L.or.R=NA)
{
    if (!is.character(x))
        stop("'x' must be a character vector")
    if (length(x) == 0L)
        return(integer(0))
    tmp <- strsplit(sub(":.*", "", x), "--", fixed=TRUE)
    elt_lens <- elementLengths(tmp)
    is_single_end_encoding <- elt_lens == 1L
    is_paired_end_encoding <- elt_lens == 2L
    if (!all(is_single_end_encoding | is_paired_end_encoding))
        stop("'x' contains ill-formed encodings")
    any_single_end <- any(is_single_end_encoding)
    any_paired_end <- any(is_paired_end_encoding)
    if (any_single_end && any_paired_end)
        warning("'x' contains a mix of single-end and paired-end encodings")
    ans <- integer(length(x))
    if (any_single_end) {
        if (identical(L.or.R, NA)) {
            tmp1 <- tmp[is_single_end_encoding]
            ngap1 <- suppressWarnings(as.integer(unlist(tmp1, use.names=FALSE)))
            if (any(is.na(ngap1)))
                stop("'x' contains ill-formed encodings")
            ngap1 <- ngap1 - 1L
            if (min(ngap1) < 0L)
                warning("some encodings in 'x' have a negative number of gaps")
        } else {
            ngap1 <- NA_integer_
        }
        ans[is_single_end_encoding] <- ngap1
    }
    if (any_paired_end) {
        tmp2 <- tmp[is_paired_end_encoding]
        ngap2 <- suppressWarnings(as.integer(unlist(tmp2, use.names=FALSE)))
        if (any(is.na(ngap2)))
            stop("'x' contains ill-formed encodings")
        ngap2 <- ngap2 - 1L
        if (min(ngap2) < 0L)
            warning("some encodings in 'x' have a negative number of gaps")
        Lngap2 <- ngap2[c(TRUE, FALSE)]
        Rngap2 <- ngap2[c(FALSE, TRUE)]
        if (identical(L.or.R, NA)) {
            ngap2 <- Lngap2 + Rngap2
        } else if (identical(L.or.R, "L")) {
            ngap2 <- Lngap2
        } else if (identical(L.or.R, "R")) {
            ngap2 <- Rngap2
        } else {
            stop("invalid supplied 'L.or.R' argument")
        }
        ans[is_paired_end_encoding] <- ngap2
    }
    ans
}

setGeneric("Lngap", function(x) standardGeneric("Lngap"))
setGeneric("Rngap", function(x) standardGeneric("Rngap"))

setMethod("ngap", "character",
    function(x) .extract_ngap_from_encoding_levels(x)
)
setMethod("Lngap", "character",
    function(x) .extract_ngap_from_encoding_levels(x, L.or.R="L")
)
setMethod("Rngap", "character",
    function(x) .extract_ngap_from_encoding_levels(x, L.or.R="R")
)

setMethod("ngap", "factor",
    function(x)
    {
        levels_ngap <- ngap(levels(x))
        levels_ngap[as.integer(x)]
    }
)
setMethod("Lngap", "factor",
    function(x)
    {
        levels_Lngap <- Lngap(levels(x))
        levels_Lngap[as.integer(x)]
    }
)
setMethod("Rngap", "factor",
    function(x)
    {
        levels_Rngap <- Rngap(levels(x))
        levels_Rngap[as.integer(x)]
    }
)

setMethod("ngap", "OverlapEncodings", function(x) ngap(encoding(x)))
setMethod("Lngap", "OverlapEncodings", function(x) Lngap(encoding(x)))
setMethod("Rngap", "OverlapEncodings", function(x) Rngap(encoding(x)))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setMethod("as.data.frame", "OverlapEncodings",
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        if (!(is.null(row.names) || is.character(row.names)))
            stop("'row.names' must be NULL or a character vector")
        data.frame(Loffset=Loffset(x),
                   Roffset=Roffset(x),
                   encoding=encoding(x),
                   flippedQuery=flippedQuery(x),
                   row.names=row.names,
                   check.rows=TRUE,
                   check.names=FALSE,
                   stringsAsFactors=FALSE)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### "show" method.
###

setMethod("show", "OverlapEncodings",
    function(object)
    {
        lo <- length(object)
        cat(class(object), " of length ", lo, "\n", sep="")
        if (lo == 0L)
            return(NULL)
        if (lo < 20L) {
            showme <-
              as.data.frame(object,
                            row.names=paste("[", seq_len(lo), "]", sep=""))
        } else {
            sketch <- function(x)
              c(as.character(window(x, 1L, 9L)),
                "...",
                as.character(window(x, length(x)-8L, length(x))))
            showme <-
              data.frame(Loffset=sketch(Loffset(object)),
                         Roffset=sketch(Roffset(object)),
                         encoding=sketch(encoding(object)),
                         flippedQuery=sketch(flippedQuery(object)),
                         row.names=c(paste("[", 1:9, "]", sep=""), "...",
                                     paste("[", (lo-8L):lo, "]", sep="")),
                         check.rows=TRUE,
                         check.names=FALSE,
                         stringsAsFactors=FALSE)
        }
        show(showme)
    }
)

