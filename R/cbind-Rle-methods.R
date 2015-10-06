### =========================================================================
### Binding Rle or RleList objects together
### -------------------------------------------------------------------------


### Return an ordinary data frame with 1 row per run. The first column is
### "runLength" and is followed by 1 column per argument.
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
        cbind(
            runLength=width(ans_runs),
            as.data.frame(
                lapply(args, function(x) {
                    run_idx <- findOverlaps(ans_runs, ranges(x), type="within",
                                            select="arbitrary")
                    runValue(x)[run_idx]
                }),
                stringsAsFactors=FALSE
            )
        )
    }
)

### Arguments are recycled the "mapply way" if necessary.
### Return an ordinary data frame with 1 row per run. The first 2 columns are
### "group" and "group_name", like in the data frame returned by the
### "as.data.frame" method for List objects. The remaining columns are the
### same as in the data frame returned by the above "cbind" method for Rle
### objects. The names on the first argument are used to populate the
### 'group_name' column.
setMethod("cbind", "RleList",
    function(...)
    {
        args <- list(...)
        df_list <- do.call(mapply, c(list(cbind), args, list(SIMPLIFY=FALSE)))
        df0 <- do.call(rbind, unname(df_list))
        elt_NROWS <- elementLengths(df_list)
        group <- rep.int(seq_along(df_list), elt_NROWS)
        df_list_names <- names(df_list)
        if (is.null(df_list_names)) {
            group_name <- rep.int(NA_character_, nrow(df0))
        } else {
            group_name <- rep.int(df_list_names, elt_NROWS)
        }
        cbind(group=group, group_name=group_name, df0, stringsAsFactors=FALSE)
    }
)

