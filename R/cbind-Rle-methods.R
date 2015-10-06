### =========================================================================
### Binding Rle or RleList objects together
### -------------------------------------------------------------------------


### Return a DataFrame object with 1 row per run. Its first column is
### "runLength" and is followed by 1 column per supplied Rle object.
setMethod("cbind", "Rle",
    function(...)
    {
        args <- list(...)
        args_names <- names(args)
        if (is.null(args_names)) {
            noname_idx <- seq_along(args)
        } else {
            noname_idx <- which(args_names %in% c("", NA_character_))
        }
        if (length(noname_idx) != 0L)
            names(args)[noname_idx] <- paste0("V", noname_idx)
        ## TODO: Add 'with.revmap' arg to disjoin method for Ranges object.
        ## Then use that feature to avoid the call to findOverlaps() below.
        ans_runs <- disjoin(do.call(c, unname(lapply(args, ranges))))
        DataFrame(
            runLength=width(ans_runs),
            DataFrame(
                lapply(args, function(x) {
                    run_idx <- findOverlaps(ans_runs, ranges(x), type="within",
                                            select="arbitrary")
                    runValue(x)[run_idx]
                })
            )
        )
    }
)

### The supplied RleList objects are recycled the "mapply way" if necessary.
### Return a CompressedSplitDataFrameList object parallel to the longest
### supplied RleList object.
setMethod("cbind", "RleList",
    function(...)
    {
        args <- list(...)
        DF_list <- do.call(mapply, c(list(cbind), args, list(SIMPLIFY=FALSE)))
        as(DF_list, "CompressedSplitDataFrameList")
    }
)

