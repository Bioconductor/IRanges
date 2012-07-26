### =========================================================================
### Object compaction
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### xvcopy()
###
### Internal compact() support function. Not intended to be called directly.
###

setGeneric("xvcopy", signature="x",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
        standardGeneric("xvcopy")
)

### Downgrades 'x' to one of the 3 concrete direct subclasses of SharedVector
### (SharedRaw, SharedInteger or SharedDouble). But those subclasses should
### not be extended anyway (like final classes in Java).
setMethod("xvcopy", "SharedVector",
    function(x, start=NA, end=NA, width=NA, lkup=NULL, reverse=FALSE)
    {
        solved_SEW <- solveUserSEW(length(x), start=start, end=end, width=width)
        ans_length <- width(solved_SEW)
        ans <- SharedVector(class(x), length=ans_length)
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
            y@pool[[group]] <- shared
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compact()
###

setGeneric("compact", signature="x",
    function(x, check=TRUE, ...) standardGeneric("compact")
)

setMethod("compact", "ANY",
    function(x, check=TRUE, ...)
    {
        if (is.list(x)) {
            ## By assigning to x[], we keep all the attributes (e.g. the
            ## row.names if 'x' is a data.frame).
            x[] <- lapply(x, compact)
            return(x)
        }
        if (isS4(x)) {
            for (name in slotNames(x))
                slot(x, name, check=check) <-
                    compact(slot(x, name), check=check, ...)
            return(x)
        }
        x
    }
)

### Both methods below first try to compact all the slots separately by
### calling the default "compact" method. In particular this could potentially
### achieve some real compaction of the "elementMetadata" and "metadata" slots.
setMethod("compact", "XVector",
    function(x, check=TRUE, ...)
    {
        x <- callNextMethod()
        xvcopy(x)
    }
)

setMethod("compact", "XVectorList",
    function(x, check=TRUE, ...)
    {
        x <- callNextMethod()
        xvcopy(x)
    }
)

