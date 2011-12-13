###

setGeneric("encodeOverlaps",
    function(query, subject) standardGeneric("encodeOverlaps")
)

### > rawToChar(encodeOverlaps(IRanges(1:11, width=4), IRanges(6, 9)))
### [1] "abcccgkkklm"
### > rawToChar(encodeOverlaps(IRanges(4:6, width=6), IRanges(6, 9)))
### [1] "deh"
### > rawToChar(encodeOverlaps(IRanges(6:8, width=2), IRanges(6, 9)))
### [1] "fij"
setMethod("encodeOverlaps", c("Ranges", "Ranges"),
    function(query, subject)
        .Call2("encode_overlaps",
               start(query), width(query), start(subject), width(subject),
               PACKAGE="IRanges")
)

