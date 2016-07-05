### =========================================================================
### RleViews objects
### -------------------------------------------------------------------------
###


### The RleViews class is the basic container for storing a set of views
### (start/end locations) on the same Rle object, called the "subject"
### vector.
setClass("RleViews",
    contains=c("Views", "RleList"),
    representation(
        subject="Rle"
    )
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### User-friendly constructor.
###

setMethod("Views", "Rle",
    function(subject, start=NULL, end=NULL, width=NULL, names=NULL)
        new_Views(subject,
                  start=start, end=end, width=width, names=names,
                  Class="RleViews")
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

setAs("AtomicList", "RleViews", function(from) {
  to <- Views(as(unlist(from, use.names = FALSE), "Rle"),
              PartitioningByEnd(from))
  names(to) <- names(from)
  to
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### The "show" method.
###

### The 2 helper functions below convert a given view on an Rle object
### into a character-string.
### Both assume that 'start' <= 'end' (so they don't check it) and
### padd the result with spaces to produce the "margin effect"
### if 'start' or 'end' are out of limits.

RleViews.show_vframe_header <- function(iW, startW, endW, widthW)
{
    cat(format("", width=iW+1),
        format("start", width=startW, justify="right"), " ",
        format("end", width=endW, justify="right"), " ",
        format("width", width=widthW, justify="right"), "\n",
        sep="")
}

RleViews.show_vframe_line <- function(x, i, iW, startW, endW, widthW)
{
    lsx <- length(subject(x))
    start <- start(x)[i]
    end <- end(x)[i]
    width <- end - start + 1
    snippetWidth <- getOption("width") - 10 - iW - startW - endW - widthW
    if (width > 0 && lsx > 0 && start <= lsx && end >= 1) {
        snippetStart <- max(min(start,lsx),1)
        snippetEnd <- max(min(end,lsx,start + snippetWidth),1)
        snippet <-
          format(as.vector(extractROWS(subject(x),
                                       IRanges(snippetStart, snippetEnd))))
        snippet <- snippet[cumsum(nchar(snippet) + 1L) < snippetWidth]
        if (length(snippet) < width) {
            snippet <- c(snippet, "...")
        }
        snippet <- paste(snippet, collapse = " ")
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
RleViews.show_vframe <- function(x, half_nrow=9L)
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
        RleViews.show_vframe_header(iW, startW, endW, widthW)
        if (lx <= 2*half_nrow+1) {
            for (i in seq_len(lx))
                RleViews.show_vframe_line(x, i, iW, startW, endW, widthW)
        } else {
            for (i in 1:half_nrow)
                RleViews.show_vframe_line(x, i, iW, startW, endW, widthW)
            cat(format("...", width=iW, justify="right"),
                " ",
                format("...", width=startW, justify="right"),
                " ",
                format("...", width=endW, justify="right"),
                " ",
                format("...", width=widthW, justify="right"),
                " ...\n", sep="")
            for (i in (lx-half_nrow+1L):lx)
                RleViews.show_vframe_line(x, i, iW, startW, endW, widthW)
        }
    }
}

setMethod("show", "RleViews",
    function(object)
    {
        cat("Views on a ", length(subject(object)), "-length Rle subject\n", sep="")
        RleViews.show_vframe(object)
    }
)
