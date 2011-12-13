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

