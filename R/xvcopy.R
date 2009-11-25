### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Copying external vectors.
###
### The xvcopy() generic and its methods.
###

setGeneric("xvcopy", signature="x",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
        standardGeneric("xvcopy")
)

### Downgrades 'x' to a SharedRaw instance!
### TODO: Make this a method for SharedVector objects.
setMethod("xvcopy", "SharedRaw",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        solved_SEW <- solveUserSEW(length(x), start=start, end=end, width=width)
        if (!isTRUEorFALSE(reverse))
            stop("'reverse' must be TRUE or FALSE")
        ans_length <- width(solved_SEW)
        ans <- SharedRaw(ans_length)
        if (reverse)
            SharedVector.reverseCopy(ans, start(solved_SEW), end(solved_SEW),
                                     src=x, lkup=lkup)
        else
            SharedVector.copy(ans, start(solved_SEW), end(solved_SEW),
                              src=x, lkup=lkup)
        return(ans)
    }
)

### Like the "subseq" method for XVector objects, this is an endomorphism.
setMethod("xvcopy", "XVector",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        solved_SEW <- solveUserSEW(length(x), start=start, end=end, width=width)
        x@shared <- xvcopy(x@shared, start=start(solved_SEW)+x@offset,
                                     width=width(solved_SEW),
                                     lkup=lkup, reverse=reverse)
        x@offset <- 0L
        x@length <- width(solved_SEW)
        x
    }
)

setMethod("xvcopy", "SharedVector_Pool",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        solved_SEW <- solveUserSEW(width(x), start=start, end=end, width=width)
        shared_vector_list <- lapply(seq_len(length(x)),
                              function(i)
                                  xvcopy(x[[i]], start=start(solved_SEW)[i],
                                                 width=width(solved_SEW)[i],
                                                 lkup=lkup, reverse=reverse))
        new2(class(x),
             xp_list=lapply(shared_vector_list, function(xv) xv@xp),
             .link_to_cached_object_list=lapply(shared_vector_list,
                                         function(xv) xv@.link_to_cached_object),
             check=FALSE)
    }
)

### Like the "subseq" method for XVectorList objects, this is an endomorphism.
### TODO: Make this a method for XVectorList objects.
setMethod("xvcopy", "XRawList",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        if (!isTRUEorFALSE(reverse))
            stop("'reverse' must be TRUE or FALSE")
        x <- narrow(x, start=start, end=end, width=width)
        all_groups <- unique(x@ranges@group)
        for (group in all_groups) {
            ii <- which(x@ranges@group == group)
            ranges <- as(x@ranges[ii], "IRanges")
            frame <- reduce(ranges, with.inframe.attrib=TRUE)
            shared_length <- sum(width(frame))
            shared <- SharedRaw(shared_length)
            .Call("SharedVector_mcopy",
                  shared, 0L, x@pool[[group]], start(frame), width(frame),
                  lkup, reverse,
                  PACKAGE="IRanges")
            x@pool@xp_list[[group]] <- shared@xp
            inframe <- attr(frame, "inframe")
            if (reverse)
                ## We supply start=1 so reverse() doesn't have to determine
                ## it (by calling 'min(start(inframe))').
                inframe <- reverse(inframe, start=1L)
            x@ranges@start[ii] <- start(inframe)
            x@ranges@width[ii] <- width(inframe)
        }
        x
    }
)

