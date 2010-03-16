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
        ans_length <- width(solved_SEW)
        ans <- SharedRaw(ans_length)
        SharedVector.mcopy(ans, 0L, x, start(solved_SEW), ans_length,
                           lkup=lkup, reverse=reverse)
        return(ans)
    }
)

### Like the "subseq" method for XVector objects, this is an endomorphism.
setMethod("xvcopy", "XVector",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        y <- subseq(x, start=start, end=end, width=width)
        y@shared <- xvcopy(y@shared, start=y@offset+1L,
                                     width=y@length,
                                     lkup=lkup, reverse=reverse)
        y@offset <- 0L
        y
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
        y <- narrow(x, start=start, end=end, width=width)
        all_groups <- unique(y@ranges@group)
        for (group in all_groups) {
            ii <- which(y@ranges@group == group)
            ranges <- as(y@ranges[ii], "IRanges")
            frame <- reduce(ranges, with.inframe.attrib=TRUE)
            shared_length <- sum(width(frame))
            shared <- SharedRaw(shared_length)
            SharedVector.mcopy(shared, 0L,
                               y@pool[[group]], start(frame), width(frame),
                               lkup=lkup, reverse=reverse)
            y@pool@xp_list[[group]] <- shared@xp
            inframe <- attr(frame, "inframe")
            if (reverse)
                ## We supply start=1 so reverse() doesn't have to determine
                ## it (by calling 'min(start(inframe))').
                inframe <- reverse(inframe, start=1L)
            y@ranges@start[ii] <- start(inframe)
            y@ranges@width[ii] <- width(inframe)
        }
        y
    }
)

