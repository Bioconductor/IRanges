test_XDataFrame_construction <- function() {
  score <- c(1L, 3L, NA)
  counts <- c(10L, 2L, NA)

  checkException(XDataFrame(1, score)) # different lengths (no recycling)
  checkException(XDataFrame(new.env())) # cannot coerce environments to xdf
  checkException(XDataFrame(score, row.names = c("a", NA, "b"))) # na in rn
  checkException(XDataFrame(score, row.names = "a")) # invalid rn length
  checkException(XDataFrame(score, row.names = c("a", "b", "a"))) # dups in rn
  
  xdf <- XDataFrame() # no args
  checkTrue(validObject(xdf))
  row.names <- c("one", "two", "three")
  xdf <- XDataFrame(row.names = row.names) # no args, but row.names
  checkTrue(validObject(xdf))
  checkIdentical(rownames(xdf), row.names)
  
  xdf <- XDataFrame(score) # single, unnamed arg
  checkTrue(validObject(xdf))
  checkIdentical(xdf[["score"]], score)
  xdf <- XDataFrame(score, row.names = row.names) #with row names
  checkTrue(validObject(xdf))
  checkIdentical(rownames(xdf), row.names)
  
  xdf <- XDataFrame(vals = score) # named vector arg
  checkTrue(validObject(xdf))
  checkIdentical(xdf[["vals"]], score)
  xdf <- XDataFrame(counts, vals = score) # mixed named and unnamed
  checkTrue(validObject(xdf))
  checkIdentical(xdf[["vals"]], score)
  checkIdentical(xdf[["counts"]], counts)
  xdf <- XDataFrame(score + score) # unnamed arg with invalid name expression
  checkTrue(validObject(xdf))
  checkIdentical(xdf[["score...score"]], score + score)

  mat <- cbind(score)
  xdf <- XDataFrame(mat) # single column matrix with column name
  checkTrue(validObject(xdf))
  checkIdentical(xdf[["score"]], score)
  mat <- cbind(score, counts)
  xdf <- XDataFrame(mat) # two column matrix with col names
  checkTrue(validObject(xdf))
  checkIdentical(xdf[["score"]], score)
  checkIdentical(xdf[["counts"]], counts)
  colnames(mat) <- NULL
  xdf <- XDataFrame(mat) # two column matrix without col names
  checkTrue(validObject(xdf))
  checkIdentical(xdf[["X1"]], score)

  sw <- XDataFrame(swiss, row.names = rownames(swiss)) # a data.frame
  checkIdentical(as.data.frame(sw), swiss)
  rownames(swiss) <- NULL # strip row names to make them comparable
  sw <- XDataFrame(swiss) # a data.frame
  checkIdentical(as.data.frame(sw), swiss)
  sw <- XDataFrame(swiss[1:3,], score) # mixed data.frame and matrix args
  checkIdentical(as.data.frame(sw), data.frame(swiss[1:3,], score))
  sw <- XDataFrame(score = score, swiss = swiss[1:3,]) # named data.frame/matrix
  checkIdentical(as.data.frame(sw),
                 data.frame(score = score, swiss = swiss[1:3,]))
}

test_XDataFrame_subset <- function() {
  sw <- XDataFrame(swiss)

  checkException(sw[list()]) # non-atomic
  checkException(sw[NA]) # column indices cannot be NA
  checkException(sw[100]) # out of bounds col
  checkException(sw[,100])
  checkException(sw[1000,]) # out of bounds row
  options(warn=2)
  checkException(sw[1:3, drop=TRUE]) # drop ignored
  checkException(sw[drop=TRUE])
  checkException(sw[foo = "bar"]) # invalid argument
  options(warn=0)
  checkException(sw["Sion",]) # no row names
  checkException(sw[,"Fert"]) # bad column name
  colnames(sw) <- NULL
  checkException(sw[,"Fertility"]) # no column names

  ## FIXME: temporary hack until XNumeric supports subsetting
  rn <- rownames(swiss)
  swiss <- data.frame(lapply(swiss, as.integer))
  sw <- XDataFrame(swiss)
  
  checkIdentical(sw[], sw) # identity subset
  checkIdentical(sw[,], sw)

  checkIdentical(sw[NULL], XDataFrame(swiss[NULL])) # NULL subsetting
  checkIdentical(sw[,NULL], XDataFrame(swiss[,NULL]))
  checkIdentical(as.data.frame(sw[NULL,]), swiss[NULL,])
  
  checkIdentical(as.data.frame(sw[1:3]), swiss[1:3])      # select columns
  checkIdentical(as.data.frame(sw[, 1:3]), swiss[1:3])    # same
  ## select rows and columns
  checkIdentical(as.data.frame(sw[4:5, 1:3]), swiss[4:5,1:3])
  
  checkIdentical(as.data.frame(sw[1]), swiss[1])  # a one-column data frame
  checkIdentical(sw[,"Fertility"], swiss[,"Fertility"])
  ## the same
  checkIdentical(as.data.frame(sw[, 1, drop = FALSE]), swiss[, 1, drop = FALSE])
  checkIdentical(sw[, 1], swiss[,1])      # a (unnamed) vector
  checkIdentical(sw[[1]], swiss[[1]])      # the same
  checkIdentical(sw[["Fertility"]], swiss[["Fertility"]])
  checkIdentical(sw[["Fert"]], swiss[["Fert"]]) # should return 'NULL'
  checkIdentical(sw[,c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                 swiss[,c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)])
  
  checkIdentical(as.data.frame(sw[1,]), swiss[1,])       # a one-row data frame
  checkIdentical(sw[1,, drop=TRUE], swiss[1,, drop=TRUE]) # a list

  ## duplicate row, unique row names are created
  checkIdentical(as.data.frame(sw[c(1, 1:2),]), swiss[c(1,1:2),])
  ## need to copy subsetted XDataFrame when placed into another
  checkIdentical(as.data.frame(XDataFrame(sw[rep(1,nrow(swiss)),])),
                 swiss[rep(1,nrow(swiss)),])
  ## NOTE: NA subsetting not yet supported for XSequences
  ##checkIdentical(as.data.frame(sw[c(1, NA, 1:2, NA),]), # mixin some NAs
  ##               swiss[c(1, NA, 1:2, NA),])

  sw <- XDataFrame(swiss, row.names=rn)
  rownames(swiss) <- rn
  checkIdentical(as.data.frame(sw["Courtelary",]), swiss["Courtelary",])
  subswiss <- swiss[1:5,1:4]
  subsw <- sw[1:5,1:4]
  checkIdentical(as.data.frame(subsw["C",]), subswiss["C",]) # partially matches
  ## NOTE: NA subsetting not yet supported for XSequences
  ##checkIdentical(as.data.frame(subsw["foo",]), # bad row name
  ##               subswiss["foo",]) 
  ##checkIdentical(as.data.frame(sw[match("C", row.names(sw)), ]), 
  ##               swiss[match("C", row.names(sw)), ]) # no exact match
}

test_XDataFrame_dimnames_replace <- function() {
  cn <- paste("X", seq_len(ncol(swiss)))
  sw <- XDataFrame(swiss)
  colnames(sw) <- cn
  checkIdentical(colnames(sw), cn)
  cn <- seq_len(ncol(swiss))
  colnames(sw) <- cn
  checkIdentical(colnames(sw), make.names(cn, unique=TRUE))
  checkException(colnames(sw) <- cn[1])
  rn <- seq(nrow(sw))
  rownames(sw) <- rn
  checkIdentical(rownames(sw), as.character(rn))
  checkException(rownames(sw) <- rn[1])
  checkException(rownames(sw) <- rep(rn[1], nrow(sw)))
  rn[1] <- NA
  checkException(rownames(sw) <- rn)
}

test_XDataFrame_replacement <- function() {
  score <- c(1L, 3L, NA)
  counts <- c(10L, 2L, NA)

  xdf <- XDataFrame(score) # single, unnamed arg
  
  xdf[["counts"]] <- counts
  checkIdentical(xdf[["counts"]], counts)
  xdf[[3]] <- score
  checkIdentical(xdf[["X"]], score)
  xdf[[3]] <- NULL # deletion

  checkException(xdf[[13]] <- counts) # index must be < length+1
  checkException(xdf[["tooshort"]] <- counts[1:2])
}
