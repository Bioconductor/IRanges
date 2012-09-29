### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Register S3 classes as S4 classes.
###
### We register the old-style (a.k.a. S3) classes below as formally defined
### classes (a.k.a. S4) because we are using them in some method signatures.
### Note that dispatch still works without this registration but causes
### 'R CMD INSTALL' to (gently) complain.
###

setOldClass("AsIs")
setOldClass("xtabs", "table")


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Some low-level helper functions and classes.
###
### Unless stated otherwise, nothing in this section is exported.
###

errorIfWarning <- function(expr)
{
    old_options <- options(warn=2)        
    on.exit(options(old_options))
    eval(expr)
}

AEbufs.use.malloc <- function(x)
    .Call("AEbufs_use_malloc", x, PACKAGE="IRanges")

AEbufs.free <- function()
    .Call("AEbufs_free", PACKAGE="IRanges")

### Exported!
.Call2 <- function(.NAME, ..., PACKAGE)
{
    #Turning off malloc-based Auto-Extending buffers again until I find the
    #time to troubleshoot 'R CMD check' segfault on moscato1 and pitt. 
    #AEbufs.use.malloc(TRUE)
    #on.exit({AEbufs.free(); AEbufs.use.malloc(FALSE)})    
    .Call(.NAME, ..., PACKAGE=PACKAGE)
}

### Exported!
setClassUnion("characterORNULL", c("character", "NULL"))

### Exported!
### We define the coercion method below as a workaround to the following
### bug in R:
###
###   setClass("A", representation(stuff="numeric"))
###   setMethod("as.vector", c("A", "missing"), function(x, mode) x@stuff)
###
###   a <- new("A", stuff=3:-5)
###   > as.vector(a)
###   [1]  3  2  1  0 -1 -2 -3 -4 -5
###   > as(a, "vector")
###   Error in as.vector(from) : 
###     no method for coercing this S4 class to a vector
###   > selectMethod("coerce", c("A", "vector"))
###   Method Definition:
###
###   function (from, to, strict = TRUE) 
###   {
###       value <- as.vector(from)
###       if (strict) 
###           attributes(value) <- NULL
###       value
###   }
###   <environment: namespace:methods>
###
###   Signatures:
###           from  to      
###   target  "A"   "vector"
###   defined "ANY" "vector"
###   > setAs("ANY", "vector", function(from) as.vector(from))
###   > as(a, "vector")
###   [1]  3  2  1  0 -1 -2 -3 -4 -5
setAs("ANY", "vector", function(from) as.vector(from))

coercerToClass <- function(class) {
  if (extends(class, "vector"))
    .as <- get(paste0("as.", class))
  else .as <- function(from) as(from, class)
  function(from) {
    setNames(.as(from), names(from))
  }
}

### Gets or sets the default value of the given slot of the given class by
### reading or altering the prototype of the class. setDefaultSlotValue() is
### typically used in the .onLoad() hook of a package when the DLL of the
### package needs to be loaded *before* the default value of a slot can be
### computed.
getDefaultSlotValue <- function(classname, slotname)
{
    classdef <- getClass(classname)
    if (!(slotname %in% names(attributes(classdef@prototype))))
        stop("prototype for class \"", classname, "\" ",
             "has no \"", slotname, "\" attribute")
    attr(classdef@prototype, slotname, exact=TRUE)
}

setDefaultSlotValue <- function(classname, slotname, value, where=.GlobalEnv)
{
    classdef <- getClass(classname)
    if (!(slotname %in% names(attributes(classdef@prototype))))
        stop("prototype for class \"", classname, "\" ",
             "has no \"", slotname, "\" attribute")
    attr(classdef@prototype, slotname) <- value
    assignClassDef(classname, classdef, where=where)
}


### Pretty printing

### Works as long as length(), "[" and as.numeric() work on 'x'.
### Not exported.
toNumSnippet <- function(x, max.width)
{
    if (length(x) <= 2L)
        return(paste(format(as.numeric(x)), collapse=" "))
    if (max.width < 0L)
        max.width <- 0L
    ## Elt width and nb of elt to display if they were all 0.
    elt_width0 <- 1L
    nelt_to_display0 <- min(length(x), (max.width+1L) %/% (elt_width0+1L))
    head_ii0 <- seq_len(nelt_to_display0 %/% 2L)
    tail_ii0 <- length(x) + head_ii0 - length(head_ii0)
    ii0 <- c(head_ii0, tail_ii0)
    ## Effective elt width and nb of elt to display
    elt_width <- format.info(as.numeric(x[ii0]))[1L]
    nelt_to_display <- min(length(x), (max.width+1L) %/% (elt_width+1L))
    if (nelt_to_display == length(x))
        return(paste(format(as.numeric(x), width=elt_width), collapse=" "))
    head_ii <- seq_len((nelt_to_display+1L) %/% 2L)
    tail_ii <- length(x) + seq_len(nelt_to_display %/% 2L) - nelt_to_display %/% 2L
    ans_head <- format(as.numeric(x[head_ii]), width=elt_width)
    ans_tail <- format(as.numeric(x[tail_ii]), width=elt_width)
    ans <- paste(paste(ans_head, collapse=" "), "...", paste(ans_tail, collapse=" "))
    if (nchar(ans) <= max.width || length(ans_head) == 0L)
        return(ans)
    ans_head <- ans_head[-length(ans_head)]
    ans <- paste(paste(ans_head, collapse=" "), "...", paste(ans_tail, collapse=" "))
    if (nchar(ans) <= max.width || length(ans_tail) == 0L)
        return(ans)
    ans_tail <- ans_tail[-length(ans_tail)]
    paste(paste(ans_head, collapse=" "), "...", paste(ans_tail, collapse=" "))
}

labeledLine <-
function(label, els, count = TRUE, labelSep = ":", sep = " ", ellipsis = "...")
{
    if (count)
        label <- paste(label, "(", length(els), ")", sep = "")
    label <- paste(label, labelSep, sep, sep = "")
    width <- getOption("width") - nchar(label)
    line <- ellipsize(els, width, sep, ellipsis)
    paste(label, line, "\n", sep = "")
}

ellipsize <-
function(obj, width = getOption("width"), sep = " ", ellipsis = "...")
{
    if (length(obj) > 2 * width)
        obj <- c(head(obj, width), tail(obj, width))
    str <- encodeString(obj)
    ## get order selectSome() would print
    half <- seq_len(ceiling(length(obj) / 2))
    ind <- as.vector(rbind(half, length(obj) - half + 1))
    nc <- cumsum(nchar(str[ind]) + nchar(sep)) - nchar(sep)
    last <- findInterval(width, nc)
    if (length(obj) > last) {
        ## make sure ellipsis fits
        while (last &&
               (nc[last] + nchar(sep)*2^(last>1) + nchar(ellipsis)) > width)
            last <- last - 1L
        if (last == 0) ## have to truncate the first element
            str <-
              paste(substring(str[1L], 1, width - nchar(ellipsis)), ellipsis,
                    sep = "")
        else if (last == 1) ## can only show the first
            str <- c(str[1L], "...")
        else
            str <- selectSome(str, last + 1L)
    }
    paste(str, collapse = sep)
}

## taken directly from Biobase
selectSome <- function (obj, maxToShow = 5) 
{
    len <- length(obj)
    if (maxToShow < 3) 
        maxToShow <- 3
    if (len > maxToShow) {
        maxToShow <- maxToShow - 1
        bot <- ceiling(maxToShow/2)
        top <- len - (maxToShow - bot - 1)
        nms <- obj[c(1:bot, top:len)]
        c(as.character(nms[1:bot]), "...", as.character(nms[-c(1:bot)]))
    } else {
        obj
    }
}

capitalize <- function(x) {
  substring(x, 1, 1) <- toupper(substring(x, 1, 1))
  x
}
