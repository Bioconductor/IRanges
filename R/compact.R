### =========================================================================
### Object compaction
### -------------------------------------------------------------------------
###


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### xvcopy() -- For internal use only!
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


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compact()
###
### Compacting an object 'x' is trying to change its internal representation
### in order to reduce its size in memory. This internal reorganization
### should be transparent to the user i.e. 'compact(x)' should look the same
### as 'x', or, more precisely, 'x' and 'compact(x)' should be interchangeable.
### However, because of the different internal representations, we generally
### don't expect 'identical(x, compact(x))' to be TRUE.
###

setGeneric("compact", signature="x",
    function(x, check=TRUE, ...) standardGeneric("compact")
)

### The user should be able to call compact() on anything.
### By default 'compact(x)' is obtained by compacting all the "components" in
### 'x'. Only 2 kinds of objects are considered to have "components": lists
### (the components are the list elements), and S4 objects (the components
### are the slots). The other objects are not considered to have components,
### so, by default, compact() does nothing on them. In particular, it does
### nothing on environments. Also the attributes of an object (other than the
### slots of an S4 object) are not considered to be "components" and therefore
### are not compacted.
### Note that in the absence of some specialized "compact" methods, this
### default behaviour would visit the tree of all the components,
### sub-components, sub-sub-components etc of object 'x' without actually
### modifying anything in 'x'! So of course, additional "compact" methods
### need to be defined for the objects that can *effectively* be compacted.
### Otherwise compact() would be equivalent to identity()!
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

setMethod("compact", "XVector", function(x, check=TRUE, ...) xvcopy(x))

setMethod("compact", "XVectorList", function(x, check=TRUE, ...) xvcopy(x))

