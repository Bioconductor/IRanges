### =========================================================================
### XRleIntegerViews objects
### -------------------------------------------------------------------------
###


### The XRleIntegerViews class is the basic container for storing a set of views
### (start/end locations) on the same XRleInteger object, called the "subject"
### vector.
setClass("XRleIntegerViews",
    contains="Views",
    representation(
        subject="XRleInteger"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### User-friendly constructor.
###

setMethod("Views", "XRleInteger",
    function(subject, start=NA, end=NA, names=NULL)
        newViews(subject, start=start, end=end, names=names,
                 Class="XRleIntegerViews")
)

setMethod("Views", "integer",
    function(subject, start=NA, end=NA, names=NULL)
    {
        xsubject <- XRleInteger(subject)
        Views(xsubject, start=start, end=end, names=names)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

### The 2 helper functions below convert a given view on an XRleInteger object
### into a character-string.
### Both assume that 'start' <= 'end' (so they don't check it) and
### padd the result with spaces to produce the "margin effect"
### if 'start' or 'end' are out of limits.

XRleIntegerViews.show_vframe_header <- function(iW, startW, endW, widthW)
{
    cat(format("", width=iW+1),
        format("start", width=startW, justify="right"), " ",
        format("end", width=endW, justify="right"), " ",
        format("width", width=widthW, justify="right"), "\n",
        sep="")
}

XRleIntegerViews.show_vframe_line <- function(x, i, iW, startW, endW, widthW)
{
    lsx <- length(subject(x))
    start <- start(x)[i]
    end <- end(x)[i]
    width <- end - start + 1
    snippetWidth <- getOption("width") - 6 - iW - startW - endW - widthW
    if (width > 0 && lsx > 0 && start <= lsx && end >= 1) {
        snippet <- toNumSnippet(subject(x)[max(min(start,lsx),1):max(min(end,lsx),1), drop = FALSE], snippetWidth)
    } else {
       snippet <- " "
    }
    cat(format(paste("[", i,"]", sep=""), width=iW, justify="right"), " ",
        format(start, width=startW, justify="right"), " ",
        format(end, width=endW, justify="right"), " ",
        format(width, width=widthW, justify="right"), " ",
        "[", snippet, "]\n",
        sep="")
}

### 'half_nrow' must be >= 1
XRleIntegerViews.show_vframe <- function(x, half_nrow=9L)
{
    cat("\nviews:")
    lx <- length(x)
    if (lx == 0)
        cat(" NONE\n")
    else {
        cat("\n")
        iW <- nchar(as.character(lx)) + 2 # 2 for the brackets
        startMax <- max(start(x))
        startW <- max(nchar(startMax), nchar("start"))
        endMax <- max(end(x))
        endW <- max(nchar(endMax), nchar("end"))
        widthMax <- max(width(x))
        widthW <- max(nchar(widthMax), nchar("width"))
        XRleIntegerViews.show_vframe_header(iW, startW, endW, widthW)
        if (lx <= 2*half_nrow+1) {
            for (i in seq_len(lx))
                XRleIntegerViews.show_vframe_line(x, i, iW, startW, endW, widthW)
        } else {
            for (i in 1:half_nrow)
                XRleIntegerViews.show_vframe_line(x, i, iW, startW, endW, widthW)
            cat(format("...", width=iW, justify="right"),
                " ",
                format("...", width=startW, justify="right"),
                " ",
                format("...", width=endW, justify="right"),
                " ",
                format("...", width=widthW, justify="right"),
                " ...\n", sep="")
            for (i in (lx-half_nrow+1L):lx)
                XRleIntegerViews.show_vframe_line(x, i, iW, startW, endW, widthW)
        }
    }
}

setMethod("show", "XRleIntegerViews",
    function(object)
    {
        subject <- subject(object)
        lsub <- length(subject)
        cat("  Views on a ", lsub, "-integer ", class(subject), " subject", sep="")
        cat("\nsubject: ", toNumSnippet(subject, getOption("width")-9), sep="")
        XRleIntegerViews.show_vframe(object)
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Equality.
###

### Assume that 'start1', 'end1', 'start2', 'end2' are single integers
### and that start1 <= end1 and start2 <= end2.
XRleIntegerViews.view1_equal_view2 <- function(x1, start1, end1, x2, start2, end2)
{
    one <- as.integer(1)
    w1 <- end1 - start1 + one
    w2 <- end2 - start2 + one
    if (w1 != w2)
        return(FALSE)

    lx1 <- length(x1)
    isBlank1 <- end1 < one || start1 > lx1
    lx2 <- length(x2)
    isBlank2 <- end2 < one || start2 > lx2
    if (isBlank1 && isBlank2)
        return(TRUE)
    if (isBlank1 || isBlank2)
        return(FALSE)

    # Left margin
    LmarginSize1 <- start1 < one
    LmarginSize2 <- start2 < one
    if (LmarginSize1 != LmarginSize2)
        return(FALSE)
    if (LmarginSize1) {
        # Both views have a left margin
        if (start1 != start2)
            return(FALSE)
        start1 <- one
        start2 <- one
    }

    # Right margin
    RmarginSize1 <- end1 > lx1
    RmarginSize2 <- end2 > lx2
    if (RmarginSize1 != RmarginSize2)
        return(FALSE)
    if (RmarginSize1) {
        # Both views have a right margin
        if (end1 - lx1 != end2 - lx2)
            return(FALSE)
        end1 <- lx1
        end2 <- lx2
    }

    # At this point, we can trust that 1 <= start1 <= end1 <= lx1
    # and that 1 <= start2 <= end2 <= lx2 so we can call unsafe
    # function IntegerPtr.read() with no fear...
    x1[start1:end1, drop = FALSE] == x2[start2:end2, drop = FALSE]
}

### 'x' and 'y' must be XRleIntegerViews objects.
### Returns a logical vector of length max(length(x), length(y)).
### Recycle its arguments.
XRleIntegerViews.equal <- function(x, y)
{
    lx <- length(x)
    ly <- length(y)
    if (lx < ly) {
        tmp <- x
        x <- y
        y <- tmp
        tmp <- lx
        lx <- ly
        ly <- tmp
    }
    if (ly == 0)
        return(logical(0))
    # Now we are sure that lx >= ly >= 1
    ans <- logical(lx)
    j <- 1
    for (i in seq_len(lx)) {
        ans[i] <- XRleIntegerViews.view1_equal_view2(
                      subject(x), start(x)[i], end(x)[i],
                      subject(y), start(y)[j], end(y)[j])
        # Recycle
        if (j < ly) j <- j + 1 else j <- 1
    }
    if (j != 1)
        warning(paste("longer object length",
                      "is not a multiple of shorter object length"))
    ans
}

### These methods are called if at least one side of the "==" (or "!=")
### operator is an XRleIntegerViews object. They have precedence over the
### corresponding methods defined for XRleInteger objects, i.e. they will
### be called if one side is an XRleIntegerViews object and the other side
### is an XRleInteger object.

setMethod("==", signature(e1="XRleIntegerViews", e2="XRleIntegerViews"),
    function(e1, e2)
    {
        XRleIntegerViews.equal(e1, e2)
    }
)
setMethod("==", signature(e1="XRleIntegerViews", e2="XRleInteger"),
    function(e1, e2)
    {
        XRleIntegerViews.equal(e1, Views(e2))
    }
)
setMethod("==", signature(e1="XRleIntegerViews", e2="integer"),
    function(e1, e2)
    {
        if (length(e2) == 0 || any(is.na(e2)))
            stop("comparison between an XRleIntegerViews object and an integer ",
                 "vector of length 0 or with NAs is not supported")
        XRleIntegerViews.equal(e1, Views(e2))
    }
)
setMethod("==", signature(e1="XRleInteger", e2="XRleIntegerViews"),
    function(e1, e2) e2 == e1
)
setMethod("==", signature(e1="integer", e2="XRleIntegerViews"),
    function(e1, e2) e2 == e1
)

setMethod("!=", signature(e1="XRleIntegerViews", e2="XRleIntegerViews"),
    function(e1, e2) !(e1 == e2)
)
setMethod("!=", signature(e1="XRleIntegerViews", e2="XRleInteger"),
    function(e1, e2) !(e1 == e2)
)
setMethod("!=", signature(e1="XRleIntegerViews", e2="integer"),
    function(e1, e2) !(e1 == e2)
)
setMethod("!=", signature(e1="XRleInteger", e2="XRleIntegerViews"),
    function(e1, e2) !(e1 == e2)
)
setMethod("!=", signature(e1="integer", e2="XRleIntegerViews"),
    function(e1, e2) !(e1 == e2)
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Other standard generic methods.
###

setMethod("as.integer", "XRleIntegerViews",
    function(x)
    {
        lapply(as.list(x), as.integer)
    }
)

