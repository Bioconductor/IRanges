### =========================================================================
### OverlapEncodings objects
### -------------------------------------------------------------------------
###


setClass("OverlapEncodings",
    contains="Vector",
    representation(
        Loffset="integer",    # no NAs, >= 0
        Roffset="integer",    # no NAs, >= 0
        encoding="factor"     # no NAs
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters.
###

setGeneric("Loffset", function(x) standardGeneric("Loffset"))
setMethod("Loffset", "OverlapEncodings", function(x) x@Loffset)

setGeneric("Roffset", function(x) standardGeneric("Roffset"))
setMethod("Roffset", "OverlapEncodings", function(x) x@Roffset)

setGeneric("encoding", function(x) standardGeneric("encoding"))
setMethod("encoding", "OverlapEncodings", function(x) x@encoding)

setMethod("length", "OverlapEncodings", function(x) length(encoding(x)))


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
                         row.names=c(paste("[", 1:9, "]", sep=""), "...",
                                     paste("[", (lo-8L):lo, "]", sep="")),
                         check.rows=TRUE,
                         check.names=FALSE,
                         stringsAsFactors=FALSE)
        }
        show(showme)
    }
)

