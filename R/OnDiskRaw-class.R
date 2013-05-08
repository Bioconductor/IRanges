### =========================================================================
### OnDiskRaw objects
### -------------------------------------------------------------------------


setClass("OnDiskRaw",
    representation("VIRTUAL",
        filepath="character",
        length="integer",
        .cache="environment"
    )
)

setMethod("length", "OnDiskRaw", function(x) x@length)

setGeneric("loadSequence", signature="x",
    function(x, offset=0, length=NA) standardGeneric("loadSequence")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Load a sequence of values from a file containing a linear array of
### fixed-size atomic values (logical, integer, double, complex, or raw).
###

### Support the same modes as readBin(), except "character" which doesn't
### have fixed-size values.
.sizeOnDisk <- function(what)
{
    switch(what,
           logical=, integer=, int=4L,
           numeric=, double=8L,
           complex=16L,
           raw=1L,
           stop(what, ": unsupported mode"))
}

### An alternative to direct use of seek() for skipping bytes in connection,
### even when the connection doesn't support seek().
.skipBytes <- function(con, n)
{
    if (!isSingleNumber(n) || n < 0)
        stop("'n' must be a single non-negative number")
    ## Using seek() should be straightforward and fast. Nothing can beat
    ## that. Unfortunately, base::seek() is notoriously unreliable on
    ## Windows (see '?seek') but also appears to be broken in R-3.0.0 on
    ## my 64-bit Ubuntu laptop when used on a gzfile connection.
    ## TODO: Remove the '!is(con, "gzfile")' condition when seek() is fixed.
    use_seek <- isSeekable(con) &&
                .Platform$OS.type != "windows" &&
                !is(con, "gzfile")
    if (use_seek)
        return(seek(con, n, origin="current"))
    ### Values to skip are read by small chunks of 8M values that we
    ### don't keep in memory. This requires less memory than reading them
    ### all at once, and thus is slightly faster.
    chunk_size <- 8000000L
    nloop <- n %/% chunk_size
    for (i in seq_len(nloop))
        readBin(con, "raw", n=chunk_size)
    readBin(con, "raw", n = n %% chunk_size)
}

### 'obj_offset' and 'obj_length' are the offset (in bytes) and length (in
### nb of array values) of a linear array of fixed-size atomic values called
### the "object". 'offset' and 'length' are relative to the object and both
### must be expressed in nb of array values.
.loadSequence <- function(filepath, file_type, what,
                          obj_offset, obj_length, offset=0, length=NA)
{
    ## Check 'obj_offset'. Should typically be a double.
    if (!isSingleNumber(obj_offset) || obj_offset < 0)
        stop("'obj_offset' must be a single non-negative number")
    ## Check 'obj_length'.
    if (!isSingleNumber(obj_length))
        stop("'obj_length' must be a single integer")
    if (!is.integer(obj_length))
        obj_length <- as.integer(obj_length)
    if (obj_length < 0L)
        stop("'obj_length' cannot be negative")
    ## Check 'offset'.
    if (!isSingleNumber(offset))
        stop("'offset' must be a single integer")
    if (!is.integer(offset))
        offset <- as.integer(offset)
    if (offset < 0L)
        stop("'offset' cannot be negative")
    if (offset > obj_length)
        stop("'offset' cannot be greater than object length")
    ## Check 'length'.
    if (!isSingleNumberOrNA(length))
        stop("'length' must be a single integer or NA")
    if (!is.integer(length))
        length <- as.integer(length)
    if (is.na(length)) {
        length <- obj_length - offset
    } else {
        if (length < 0L)
            stop("'length' cannot be negative")
        if (offset + length > obj_length)
            stop("invalid 'offset' / 'length' combination (would result in ",
                 "reading data beyond the object)")
    }
    con <- get(file_type)(filepath, open="rb")
    on.exit(close(con))
    n <- obj_offset + as.double(offset) * .sizeOnDisk(what)
    .skipBytes(con, n)
    readBin(con, what, n=length, endian="big")
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### DirectRaw -- A concrete OnDiskRaw subclass that treats the bytes stored
###              in a file as the values of a raw vector.
###

.getFileSize <- function(filepath)
{
    if (!isSingleString(filepath))
        stop("'filepath' must be a single string")
    ans <- file.info(filepath)$size
    if (is.na(ans) || ans > .Machine$integer.max)
        stop("file size not available or file too big ",
             "(size > '.Machine$integer.max')")
    as.integer(ans)
}

setClass("DirectRaw", contains="OnDiskRaw")

DirectRaw <- function(filepath)
{
    ans_length <- .getFileSize(filepath)
    ans_cache <- new.env(parent=emptyenv())
    new("DirectRaw", filepath=filepath,
                     length=ans_length,
                     .cache=ans_cache)
}

setMethod("loadSequence", "DirectRaw",
    function(x, offset=0, length=NA)
    {
        .loadSequence(x@filepath, "file", "raw",
                      0, x@length, offset=offset, length=length)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### SerializedRaw -- A concrete OnDiskRaw subclass for fast access to the
###                  values stored in a serialized raw vector.
###

### Supports RDX2 and RDA2 formats only. These are the formats produced
### when calling save() with 'ascii=FALSE' (the default) and 'ascii=TRUE',
### respectively.
.extractRdaTypeAndFormat <- function(filepath)
{
    if (!isSingleString(filepath))
        stop("'filepath' must be a single string")
    RDX2 <- charToRaw("RDX2\nX\n")
    RDA2 <- charToRaw("RDA2\nA\n")
    ## file(), gzfile(), bzfile(), and xzfile() don't necessarily return a
    ## "connection" object with the type (i.e. first more specific class)
    ## that corresponds to the function that was called. For example, when
    ## called on a Xz file, gzfile() returns a "connection" object of type
    ## "xzfile".
    for (con_type in c("file", "gzfile", "bzfile", "xzfile")) {
        con <- get(con_type)(filepath, open="rb")
        buf <- readBin(con, "raw", n=7L)
        if (identical(buf, RDX2)) {
            on.exit(close(con))
            file_format <- "RDX2"
            break
        }
        if (identical(buf, RDA2)) {
            on.exit(close(con))
            file_format <- "RDA2"
            break
        }
        close(con)
    }
    if (!identical(try(isOpen(con), silent=TRUE), TRUE))
        stop("unknown file type/format for serialized R objects")
    list(file_type=class(con)[1L], file_format=file_format)
}

.filepos_envir <- new.env(hash=TRUE, parent=emptyenv())

.get_filepos <- function()
    get("filepos", envir=.filepos_envir, inherits=FALSE)

.set_filepos <- function(filepos)
    assign("filepos", filepos, envir=.filepos_envir)

### An enhanced readBin() that keeps track of the current position in the
### file. Needed because seek() is not supported or reliable on all
### connections.
.readBin2 <- function(con, what, n=1L)
{
    filepos <- .get_filepos()
    filepos <- filepos + as.double(n) * .sizeOnDisk(what)
    .set_filepos(filepos)
    readBin(con, what, n=n, endian="big")
}

### The length of a serialized object seems to be encoded only for a CHARSXP,
### an atomic vector (LGLSXP, INTSXP, REALSXP, CPLXSXP, STRSXP, RAWSXP),
### a list (VECSXP), and an expressions vector (EXPRSXP).
### See src/main/serialize.c in R source tree.
.TYPES_WITH_LENGTH <- c(CHARSXP=9L,
                        LGLSXP=10L, INTSXP=13L, REALSXP=14L,
                        CPLXSXP=15L, STRSXP=16L, RAWSXP=24L,
                        VECSXP=19L, EXPRSXP=20L)

### Supports RDX2 and RDA2 formats only.
.extractObjectInfoFromRda <- function(filepath)
{
    type_and_format <- .extractRdaTypeAndFormat(filepath)
    file_type <- type_and_format$file_type
    file_format <- type_and_format$file_format
    con <- get(file_type)(filepath, open="rb")
    on.exit(close(con))
    .set_filepos(0)
    obj_length <- NA_integer_
    obj_offset <- NA_real_
    if (file_format == "RDX2") {
        .readBin2(con, "raw", n=31L)
        obj_name_len <- .readBin2(con, "integer", n=1L)
        obj_name <- rawToChar(.readBin2(con, "raw", n=obj_name_len))
        flags <- .readBin2(con, "raw", n=4L)
        obj_type <- as.integer(flags[4L])
        other_flags <- as.integer(flags[3L])
        is_obj <- as.logical(other_flags %% 2L)
        has_attr <- as.logical((other_flags %/% 2L) %% 2L)
        has_tag <- as.logical((other_flags %/% 4L) %% 2L)
        if (obj_type %in% .TYPES_WITH_LENGTH) {
            obj_length <- .readBin2(con, "integer", n=1L)
            if (obj_length < -1L)
                stop("negative serialized length for vector")
            if (obj_length == -1L) {
                ## Length of a long vector is encoded with 2 integers.
                len1len2 <- .readBin2(con, "integer", n=2L)
                ## Compute length as a double.
                obj_length <- len1len2[1L] * (2L^32L) + len1len2[2L]
            }
            obj_offset <- .get_filepos()
        }
    } else if (file_format == "RDA2") {
        buf <- readLines(con, n=11L)
        obj_name <- buf[10L]
        flags <- as.numeric(buf[11L])
        obj_type <- as.integer(flags %% 256L)
        other_flags <- as.integer(flags %/% 256L)
        is_obj <- as.logical(other_flags %% 2L)
        has_attr <- as.logical((other_flags %/% 2L) %% 2L)
        has_tag <- as.logical((other_flags %/% 4L) %% 2L)
        if (obj_type %in% .TYPES_WITH_LENGTH) {
            obj_length <- as.integer(readLines(con, n=1L))
            if (obj_length < -1L)
                stop("negative serialized length for vector")
            if (obj_length == -1L) {
                ## Length of a long vector is encoded with 2 integers.
                len1len2 <- as.integer(readLines(con, n=2L))
                ## Compute length as a double.
                obj_length <- len1len2[1L] * (2L^32L) + len1len2[2L]
            }
        }
    }
    ans2 <- list(obj_name=obj_name,
                 obj_type=obj_type,
                 is_obj=is_obj,
                 has_attr=has_attr,
                 has_tag=has_tag,
                 obj_length=obj_length,
                 obj_offset=obj_offset)
    c(type_and_format, ans2)
}

### Supports logical, integer, double, complex, and raw vectors only.
### Extracts only the vector values. All attributes are ignored.
.loadSequenceFromRda <- function(filepath, offset=0, length=NA)
{
    info <- .extractObjectInfoFromRda(filepath)
    file_type <- info$file_type
    file_format <- info$file_format
    obj_type <- info$obj_type
    obj_length <- info$obj_length
    obj_offset <- info$obj_offset

    if (is.na(obj_length))
        stop("serialized object not a vector (has no length)")
    if (!is.integer(obj_length))
        stop("serialized long vectors are not supported")
    type <- names(.TYPES_WITH_LENGTH)[match(obj_type, .TYPES_WITH_LENGTH)]
    what <- switch(type,
                   LGLSXP="logical",
                   INTSXP="integer",
                   REALSXP="double",
                   CPLXSXP="complex",
                   RAWSXP="raw",
                   stop(type, ": unsupported type of serialized vector"))
    if (file_format == "RDX2") {
        ans <- .loadSequence(filepath, file_type, what,
                             obj_offset, obj_length,
                             offset=offset, length=length)
    } else {
        stop(file_format, " format not supported yet")
    }
    ans
}

setClass("SerializedRaw",
    contains="OnDiskRaw",
    representation(
        file_type="character",
        file_format="character",
        obj_offset="numeric"  # not integer, so works on a file > 2 Gb
    )
)

SerializedRaw <- function(filepath)
{
    info <- .extractObjectInfoFromRda(filepath)
    file_type <- info$file_type
    file_format <- info$file_format
    obj_type <- info$obj_type
    obj_length <- info$obj_length
    obj_offset <- info$obj_offset

    if (file_format != "RDX2")
        stop("Object in file was serialized in the ", file_format, " format, ",
             "which is not a binary\n  format. save() should be called with ",
             "'ascii=FALSE' (the default) in order to\n  produce a binary ",
             "file that can be used to construct a SerializedRaw object.")
    ## TODO: Remove this when seek() is fixed.
    if (file_type != "file")
        warning("The '", filepath, "' file is compressed (type: \"",
                file_type, "\").\n",
                "  At this moment (i.e. in R <= 3.0.0), seek() is reliable ",
                "only on an\n  uncompressed file (type: \"file\"), so it ",
                "cannot be used for fast access\n  to the data in your file. ",
                "This means that loadSequence() will be slow on\n  your ",
                "SerializedRaw object. For much faster access to the data, ",
                "consider\n  using an uncompressed '.rda' file instead. ",
                "This is obtained by using\n  save() with 'compress=FALSE'.")
    if (obj_type != .TYPES_WITH_LENGTH[["RAWSXP"]])
        stop("serialized object not a raw vector")
    if (!is.integer(obj_length))
        stop("serialized object is a long raw vector -> not supported")
    ans_cache <- new.env(parent=emptyenv())
    new("SerializedRaw", filepath=filepath,
                         length=obj_length,
                         .cache=ans_cache,
                         file_type=file_type,
                         file_format=file_format,
                         obj_offset=obj_offset)
}

setMethod("loadSequence", "SerializedRaw",
    function(x, offset=0, length=NA)
    {
        .loadSequence(x@filepath, x@file_type, "raw",
                      x@obj_offset, x@length,
                      offset=offset, length=length)
    }
)


