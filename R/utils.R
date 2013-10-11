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
###   setMethod("as.vector", "A", function(x, mode="any") x@stuff)
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Manipulating the prototype of an S4 class.
###

### Gets or sets the default value of the given slot of the given class by
### reading or altering the prototype of the class. setDefaultSlotValue() is
### typically used in the .onLoad() hook of a package when the DLL of the
### package needs to be loaded *before* the default value of a slot can be
### computed.
getDefaultSlotValue <- function(classname, slotname, where=.GlobalEnv)
{
    classdef <- getClass(classname, where=where)
    if (!(slotname %in% names(attributes(classdef@prototype))))
        stop("prototype for class \"", classname, "\" ",
             "has no \"", slotname, "\" attribute")
    attr(classdef@prototype, slotname, exact=TRUE)
}

setDefaultSlotValue <- function(classname, slotname, value, where=.GlobalEnv)
{
    classdef <- getClass(classname, where=where)
    if (!(slotname %in% names(attributes(classdef@prototype))))
        stop("prototype for class \"", classname, "\" ",
             "has no \"", slotname, "\" attribute")
    attr(classdef@prototype, slotname) <- value
    assignClassDef(classname, classdef, where=where)
    ## Re-compute the complete definition of the class. methods::setValidity()
    ## does that after calling assignClassDef() so we do it too.
    resetClass(classname, classdef, where=where)
}

setPrototypeFromObject <- function(classname, object, where=.GlobalEnv)
{
    classdef <- getClass(classname, where=where)
    if (class(object) != classname)
        stop("'object' must be a ", classname, " instance")
    object_attribs <- attributes(object)
    object_attribs$class <- NULL
    ## Sanity check.
    stopifnot(identical(names(object_attribs),
                        names(attributes(classdef@prototype))))
    attributes(classdef@prototype) <- object_attribs
    assignClassDef(classname, classdef, where=where)
    ## Re-compute the complete definition of the class. methods::setValidity()
    ## does that after calling assignClassDef() so we do it too.
    resetClass(classname, classdef, where=where)
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Pretty printing
###

### 'makeNakedMat.FUN' must be a function returning a character matrix.
makePrettyMatrixForCompactPrinting <- function(x, makeNakedMat.FUN)
{
  lx <- NROW(x)
  nhead <- get_showHeadLines()
  ntail <- get_showTailLines()

  if (lx < (nhead + ntail + 1L)) {
    ans <- makeNakedMat.FUN(x)
    ans_rownames <- .rownames2(names(x), lx)
  } else {
    top_idx <- 1:nhead
    if (nhead == 0)
      top_idx <- 0 
    bottom_idx=(lx-ntail+1L):lx
    if (ntail == 0)
      bottom_idx <- 0 
    ans_top <- makeNakedMat.FUN(x[top_idx,,drop=FALSE])
    ans_bottom <- makeNakedMat.FUN(x[bottom_idx,,drop=FALSE])
    ans <- rbind(ans_top,
                 matrix(rep.int("...", ncol(ans_top)), nrow=1L),
                 ans_bottom)
    ans_rownames <- .rownames2(names(x), lx, top_idx, bottom_idx)
  }
  rownames(ans) <- format(ans_rownames, justify="right")
  ans
}

.rownames2 <- function(names=NULL, len=NULL, tindex=NULL, bindex=NULL)
{
  if (is.null(tindex) && is.null(bindex)) {
    ## all lines
    if (len == 0L)
      character(0)
    else if (is.null(names))
      paste0("[", seq_len(len), "]")
    else
      names
  } else {
    ## head and tail 
    if (!is.null(names)) {
      c(names[tindex], "...", names[bindex])
    } else {
      s1 <- paste0("[", tindex, "]")
      s2 <- paste0("[", bindex, "]")
      if (all(tindex == 0)) 
        s1 <- character(0) 
      if (all(bindex == 0)) 
        s2 <- character(0) 
      c(s1, "...", s2)
    }
  }
}

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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### showHeadLines and showTailLines robust to NA, Inf and non-integer 
###

get_showHeadLines <- function()
{
    .get_showLines(5L, "showHeadLines") 
}

get_showTailLines <- function()
{
    .get_showLines(5L, "showTailLines") 
}

.get_showLines <- function(default, option)
{
    opt <- getOption(option, default=default)
    if (!is.infinite(opt))
        opt <- as.integer(opt)
    if (is.na(opt))
        opt <- default
    opt 
}

