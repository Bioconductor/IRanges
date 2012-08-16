### =========================================================================
### MaskCollection objects
### -------------------------------------------------------------------------

setClass("MaskCollection",
    contains="RangesList",
    representation(
        nir_list="list",    # a list of NormalIRanges objects
        width="integer",
        active="logical",
        NAMES="character",  # R doesn't like @names !!
        desc="character"
    ),
    prototype(
        nir_list=list(),
        width=0L,
        active=logical(0),
        NAMES=as.character(NA),
        desc=as.character(NA)
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "length" and accessor methods.
###

setGeneric("nir_list", function(x) standardGeneric("nir_list"))

setMethod("nir_list", "MaskCollection", function(x) x@nir_list)

setMethod("length", "MaskCollection", function(x) length(nir_list(x)))

setMethod("width", "MaskCollection", function(x) x@width)

setGeneric("active", function(x) standardGeneric("active"))

setMethod("active", "MaskCollection",
    function(x)
    {
        ans <- x@active
        names(ans) <- names(x)
        ans
    }
)

setGeneric("active<-", signature="x",
    function(x, value) standardGeneric("active<-")
)

setReplaceMethod("active", "MaskCollection",
    function(x, value)
    {
        if (!is.logical(value) || anyMissing(value))
            stop("'value' must be a logical vector with no NAs")
        x@active[] <- value
        x
    }
)

setMethod("names", "MaskCollection",
    function(x) if (length(x@NAMES) == 1 && is.na(x@NAMES)) NULL else x@NAMES
)

setReplaceMethod("names", "MaskCollection",
    function(x, value)
    {
        if (is.null(value)) {
            x@NAMES <- as.character(NA)
            return(x)
        } else {
            value <- as.character(value)
        }
        ii <- is.na(value)
        if (any(ii))
            value[ii] <- ""
        if (length(value) > length(x))
            stop("too many names")
        if (length(value) < length(x))
            value <- c(value, character(length(x) - length(value)))
        x@NAMES <- value
        x
    }
)

setGeneric("desc", function(x) standardGeneric("desc"))

setMethod("desc", "MaskCollection",
    function(x) if (length(x@desc) == 1 && is.na(x@desc)) NULL else x@desc
)

setGeneric("desc<-", signature="x",
    function(x, value) standardGeneric("desc<-")
)

setReplaceMethod("desc", "MaskCollection",
    function(x, value)
    {
        if (is.null(value)) {
            x@desc <- as.character(NA)
            return(x)
        }
        if (!is.character(value))
            stop("'value' must be NULL or a character vector")
        ii <- is.na(value)
        if (any(ii))
            value[ii] <- ""
        if (length(value) > length(x))
            stop("too many names")
        if (length(value) < length(x))
            value <- c(value, character(length(x) - length(value)))
        x@desc <- value
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

.valid.MaskCollection.width <- function(x)
{
    if (!isSingleInteger(width(x)) || width(x) < 0)
        return("the width of the collection must be a single non-negative integer")
    NULL
}

.valid.MaskCollection.nir_list <- function(x)
{
    if (!is.list(nir_list(x))
     || !all(sapply(nir_list(x), function(nir) is(nir, "NormalIRanges"))))
        return("the 'nir_list' slot must contain a list of NormalIRanges objects")
    if (!all(1 <= min(x)) || !all(max(x) <= width(x)))
        return("the min and max of the masks must be >= 1 and <= width of the collection")
    NULL
}

.valid.MaskCollection.active <- function(x)
{
    if (!is.logical(active(x)) || anyMissing(active(x)))
        return("the 'active' slot must be a logical vector with no NAs")
    if (length(active(x)) != length(x))
        return("the length of the 'active' slot differs from the length of the object")
    NULL
}

.valid.MaskCollection.names <- function(x)
{
    if (!is.character(x@NAMES))
        return("the 'NAMES' slot must contain a character vector")
    if (is.null(names(x)))
        return(NULL)
    if (anyMissing(names(x)))
        return("the names must be non-NA strings")
    if (length(names(x)) != length(x))
        return("number of names and number of elements differ")
    NULL
}

.valid.MaskCollection.desc <- function(x)
{
    if (!is.character(x@desc))
        return("the 'desc' slot must contain a character vector")
    if (is.null(desc(x)))
        return(NULL)
    if (anyMissing(desc(x)))
        return("the descriptions must be non-NA strings")
    if (length(desc(x)) != length(x))
        return("number of descriptions and number of elements differ")
    NULL
}

.valid.MaskCollection <- function(x)
{
    ## The 'width' slot needs to be checked separately and we must return
    ## if it's invalid. This is because .valid.MaskCollection.nir_list()
    ## won't work properly if 'x@width' is NA.
    problems <- .valid.MaskCollection.width(x)
    if (!is.null(problems))
        return(problems)
    c(.valid.MaskCollection.nir_list(x),
      .valid.MaskCollection.active(x),
      .valid.MaskCollection.names(x),
      .valid.MaskCollection.desc(x))
}

setValidity2("MaskCollection", .valid.MaskCollection)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The safe and user-friendly "Mask" constructor.
###

Mask <- function(mask.width, start=NULL, end=NULL, width=NULL)
{
    nir <- asNormalIRanges(IRanges(start=start, end=end, width=width), force=FALSE)
    new2("MaskCollection", nir_list=list(nir),
                           width=numeric2integer(mask.width),
                           active=TRUE, check=FALSE)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "max" and "min" methods.
###

setMethod("max", "MaskCollection",
    function(x, ..., na.rm)
    {
        if (length(x) == 0)
            return(integer(0))
        sapply(nir_list(x), max)
    }
)

setMethod("min", "MaskCollection",
    function(x, ..., na.rm)
    {
        if (length(x) == 0)
            return(integer(0))
        sapply(nir_list(x), min)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "maskedwidth" and "maskedratio" generics and methods.
###

setGeneric("maskedwidth", function(x) standardGeneric("maskedwidth"))
setMethod("maskedwidth", "MaskCollection",
    function(x)
    {
        nir_list <- nir_list(x)
        if (length(nir_list) == 0)
            integer(0)
        else
            sapply(nir_list, function(nir) sum(width(nir)))
    }
)

setGeneric("maskedratio", function(x) standardGeneric("maskedratio"))
setMethod("maskedratio", "MaskCollection", function(x) maskedwidth(x) / width(x))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

setMethod("[[", "MaskCollection",
    function(x, i, j, ...)
    {
        i <- checkAndTranslateDbleBracketSubscript(x, i)
        nir_list(x)[[i]]
    }
)

### Always behaves like an endomorphism (i.e. ignores the 'drop' argument and
### behaves like if it was actually set to FALSE).
setMethod("[", "MaskCollection",
    function(x, i, j, ..., drop=TRUE)
    {
        if (!missing(j) || length(list(...)) > 0)
            stop("invalid subsetting")
        if (missing(i))
            i <- seq_len(length(x))
        else
            i <- normalizeSingleBracketSubscript(i, x)
        if (any(i > 0L)) {  # then 'all(i >= 0)' must be TRUE
            i <- i[i > 0L]
            if (anyDuplicated(i))
                stop("subscript would generate duplicated elements")
        }
        slot(x, "nir_list", check=FALSE) <- nir_list(x)[i]
        slot(x, "active", check=FALSE) <- active(x)[i]
        if (!is.null(names(x)))
            slot(x, "NAMES", check=FALSE) <- names(x)[i]
        if (!is.null(desc(x)))
            slot(x, "desc", check=FALSE) <- desc(x)[i]
        mcols(x) <- mcols(x)[i, , drop=FALSE]
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "append" method.
###
### TODO: Be more consistent with "[" which doesn't allow subscripts with
### duplicated positive values in order to make it harder for the user to
### produce a MaskCollection object with duplicated names.
### The "append" method below makes this too easy (with append(x, x)).
###

.append.names.or.desc <- function(nm1, l1, nm2, l2, after)
{
    if (is.null(nm1) && is.null(nm2))
        return(as.character(NA))
    if (is.null(nm1))
        nm1 <- rep.int("", l1)
    if (is.null(nm2))
        nm2 <- rep.int("", l2)
    append(nm1, nm2, after=after)
}

setMethod("append", c("MaskCollection", "MaskCollection"),
    function(x, values, after=length(x))
    {
        if (width(values) != width(x))
            stop("'x' and 'values' must have the same width")
        if (!isSingleNumber(after))
            stop("'after' must be a single number")
        if (length(values) == 0)
            return(x)
        ans_nir_list <- append(nir_list(x), nir_list(values), after=after)
        ans_active <- append(active(x), active(values), after=after)
        l1 <- length(x)
        l2 <- length(values)
        ans_NAMES <- .append.names.or.desc(names(x), l1, names(values), l2, after)
        ans_desc <- .append.names.or.desc(desc(x), l1, desc(values), l2, after)
        ## This transformation must be atomic.
        x@nir_list <- ans_nir_list
        x@active <- ans_active
        x@NAMES <- ans_NAMES
        x@desc <- ans_desc
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### collapse()
###

setGeneric("collapse", function(x) standardGeneric("collapse"))

### Always return a MaskCollection object of length 1 where the mask is active.
setMethod("collapse", "MaskCollection",
    function(x)
    {
        keep_it <- active(x)
        if (!all(keep_it))
            x <- x[keep_it]
        if (length(x) == 1)
            return(x)
        nir_list <- nir_list(x)
        if (length(nir_list) == 0) {
            nir1 <- new("NormalIRanges")
        } else {
            start1 <- unlist(lapply(nir_list, start))
            width1 <- unlist(lapply(nir_list, width))
            ranges <- new2("IRanges", start=start1, width=width1, check=FALSE)
            nir1 <- asNormalIRanges(ranges, force=TRUE)
        }
        ## This transformation must be atomic.
        x@nir_list <- list(nir1)
        x@active <- TRUE
        x@NAMES <- as.character(NA)
        x@desc <- as.character(NA)
        x
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

### From a MaskCollection object to a NormalIRanges object.
setAs("MaskCollection", "NormalIRanges",
    function(from) collapse(from)[[1L]]
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

MaskCollection.show_frame <- function(x)
{
    lx <- length(x)
    cat("masks:")
    if (lx == 0) {
        cat(" NONE\n")
    } else {
        cat("\n")
        ## Explictely specify 'row.names=NULL' otherwise data.frame() will
        ## try to use the names of the first component that has suitable
        ## names, which could be 'active(x)' (3rd component) if 'x' has names.
        frame <- data.frame(maskedwidth=maskedwidth(x),
                            maskedratio=maskedratio(x),
                            active=active(x),
                            row.names=NULL,
                            check.names=FALSE)
        frame$names <- names(x)
        frame$desc <- desc(x)
        show(frame)
        if (lx >= 2) {
            margin <- format("", width=nchar(as.character(lx)))
            cat("all masks together:\n")
            mask0 <- collapse(`active<-`(x, TRUE))
            frame <- data.frame(maskedwidth=maskedwidth(mask0),
                                maskedratio=maskedratio(mask0),
                                check.names=FALSE)
            row.names(frame) <- margin
            show(frame)
            if (sum(active(x)) < lx) {
                cat("all active masks together:\n")
                mask1 <- collapse(x)
                frame <- data.frame(maskedwidth=maskedwidth(mask1),
                                    maskedratio=maskedratio(mask1),
                                    check.names=FALSE)
                row.names(frame) <- margin
                show(frame)
            }
        }
    }
}

setMethod("show", "MaskCollection",
    function(object)
    {
        lo <- length(object)
        cat(class(object), " of length ", lo,
            " and width ", width(object), "\n", sep="")
        MaskCollection.show_frame(object)
    }
)

