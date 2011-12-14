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

setGeneric("encodeOverlaps",
    function(query, subject) standardGeneric("encodeOverlaps")
)

### > query <- IRanges(c(7, 15, 22), c(9, 19, 23))
### > subject <- IRanges(c(1, 4, 15, 22), c(2, 9, 19, 25))
### > rawToChar(encodeOverlaps(query, subject))
### [1] "2:j<g<f"
setMethod("encodeOverlaps", c("Ranges", "Ranges"),
    function(query, subject)
        .Call2("overlaps_to_OGOCS",
               start(query), width(query), start(subject), width(subject),
               PACKAGE="IRanges")
)

