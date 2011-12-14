###

setGeneric("encodePOverlaps",
    function(query, subject) standardGeneric("encodePOverlaps")
)

### > rawToChar(encodePOverlaps(IRanges(1:11, width=4), IRanges(6, 9)))
### [1] "abcccgkkklm"
### > rawToChar(encodePOverlaps(IRanges(4:6, width=6), IRanges(6, 9)))
### [1] "deh"
### > rawToChar(encodePOverlaps(IRanges(6:8, width=2), IRanges(6, 9)))
### [1] "fij"
setMethod("encodePOverlaps", c("Ranges", "Ranges"),
    function(query, subject)
        .Call2("encode_poverlaps",
               start(query), width(query), start(subject), width(subject),
               PACKAGE="IRanges")
)

setGeneric("encodeOverlaps", signature=c("query", "subject"),
    function(query, subject, sparse.output=TRUE, ...)
        standardGeneric("encodeOverlaps")
)

### > query <- IRanges(c(7, 15, 22), c(9, 19, 23))
### > subject <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
### > encodeOverlaps(query, subject)
### [1] "2:j<g<f"
setMethod("encodeOverlaps", c("Ranges", "Ranges"),
    function(query, subject, sparse.output=TRUE, as.raw=FALSE)
    {
        if (!isTRUEorFALSE(sparse.output))
            stop("'sparse.output' must be TRUE or FALSE")
        if (!isTRUEorFALSE(as.raw))
            stop("'as.raw' must be TRUE or FALSE")
        .Call2("encode_overlaps",
               start(query), width(query), start(subject), width(subject),
               sparse.output, as.raw,
               PACKAGE="IRanges")
    }
)

### TODO: Put this in the (upcoming) man page for encodeOverlaps().
### A simple but inefficient implementation of findOverlaps(). Complexity and
### memory usage is M x N where M and N are the lengths of 'query' and
### 'subject', respectively.
simpleFindOverlaps <- function(query, subject)
{
    ## WARNING: When using sparse.output=FALSE and as.raw=TRUE, the returned
    ## raw matrix is transposed!
    codes <- encodeOverlaps(query, subject, sparse.output=FALSE, as.raw=TRUE)
    offsets <- which(charToRaw("c") <= codes & codes <= charToRaw("k")) - 1L
    q_hits <- offsets %/% nrow(codes) + 1L
    s_hits <- offsets %% nrow(codes) + 1L
    cbind(query=q_hits, subject=s_hits)
}

