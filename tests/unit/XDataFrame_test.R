test_XDataFrame_construction <- function() {
  score <- c(1L, 3L, NA)
  counts <- c(10L, 2L, NA)

  ## na in rn
  checkException(XDataFrame(score, row.names = c("a", NA, "b")), silent = TRUE)
  ## invalid rn length
  checkException(XDataFrame(score, row.names = "a"), silent = TRUE)
  ## dups in rn
  checkException(XDataFrame(score, row.names = c("a", "b", "a")), silent = TRUE)
  
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
  checkIdentical(xdf[["V1"]], score)

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

  ## recycling
  xdf <- XDataFrame(1, score)
  checkIdentical(xdf[[1]], rep(1, 3)) 
}

test_XDataFrame_subset <- function() {
  data(swiss)
  sw <- XDataFrame(swiss)
  rn <- rownames(swiss)

  checkException(sw[list()], silent = TRUE) # non-atomic
  checkException(sw[NA], silent = TRUE) # column indices cannot be NA
  checkException(sw[100], silent = TRUE) # out of bounds col
  checkException(sw[,100], silent = TRUE)
  checkException(sw[1000,], silent = TRUE) # out of bounds row
  options(warn=2)
  checkException(sw[1:3, drop=TRUE], silent = TRUE) # drop ignored
  checkException(sw[drop=TRUE], silent = TRUE)
  checkException(sw[foo = "bar"], silent = TRUE) # invalid argument
  options(warn=0)
  checkException(sw["Sion",], silent = TRUE) # no row names
  checkException(sw[,"Fert"], silent = TRUE) # bad column name
  colnames(sw) <- NULL
  checkException(sw[,"Fertility"], silent = TRUE) # no column names

  sw <- XDataFrame(swiss)

  checkIdentical(sw[], sw) # identity subset
  checkIdentical(sw[,], sw)

  checkIdentical(sw[NULL], XDataFrame(swiss[NULL])) # NULL subsetting
  checkIdentical(sw[,NULL], XDataFrame(swiss[,NULL]))
  checkIdentical(as.data.frame(sw[NULL,]), data.frame(swiss[NULL,]))

  rownames(sw) <- rn

  ## select columns
  checkIdentical(as.data.frame(sw[1:3]), swiss[1:3])
  checkIdentical(as.data.frame(sw[, 1:3]), swiss[1:3])
  ## select rows
  checkIdentical(as.data.frame(sw[1:3,]), swiss[1:3,])
  checkIdentical(as.data.frame(sw[1:3,]), swiss[1:3,])
  checkIdentical(as.data.frame(sw[sw[["Education"]] == 7,]),
                 swiss[swiss[["Education"]] == 7,])
  checkIdentical(as.data.frame(sw[Rle(sw[["Education"]] == 7),]),
                 swiss[swiss[["Education"]] == 7,])
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

  ## NOTE: NA subsetting not yet supported for XSequences
  ##checkIdentical(as.data.frame(sw[c(1, NA, 1:2, NA),]), # mixin some NAs
  ##               swiss[c(1, NA, 1:2, NA),])

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
  data(swiss)
  cn <- paste("X", seq_len(ncol(swiss)), sep = ".")
  sw <- XDataFrame(swiss)
  colnames(sw) <- cn
  checkIdentical(colnames(sw), cn)
  cn <- seq_len(ncol(swiss))
  colnames(sw) <- cn
  checkIdentical(colnames(sw), make.names(cn, unique=TRUE))
  colnames(sw) <- cn[1]
  colnames(swiss) <- cn[1]
  checkIdentical(colnames(sw), c("X1", NA, NA, NA, NA, NA))
  rn <- seq(nrow(sw))
  rownames(sw) <- rn
  checkIdentical(rownames(sw), as.character(rn))
  checkException(rownames(sw) <- rn[1], silent = TRUE)
  checkException(rownames(sw) <- rep(rn[1], nrow(sw)), silent = TRUE)
  rn[1] <- NA
  checkException(rownames(sw) <- rn, silent = TRUE)
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
  xdf[["counts"]] <- NULL
  xdf$counts <- counts
  checkIdentical(xdf$counts, counts)
  
  
  checkException(xdf[[13]] <- counts, silent = TRUE) # index must be < length+1
  checkException(xdf[["tooshort"]] <- counts[1:2], silent = TRUE)
}

## splitting and combining
test_XDataFrame_combine <- function() {
  data(swiss)
  sw <- XDataFrame(swiss, row.names=rownames(swiss))
  rn <- rownames(swiss)
  
  ## split
  
  swsplit <- split(sw, sw[["Education"]])
  checkTrue(validObject(swsplit))
  swisssplit <- split(swiss, swiss$Education)
  checkIdentical(as.list(lapply(swsplit, as.data.frame)), swisssplit)
  checkTrue(validObject(split(XDataFrame(IRanges(1:26, 1:26), LETTERS),
                              letters)))
  
  ## rbind

  checkIdentical(rbind(XDataFrame(), XDataFrame()), XDataFrame())
  zr <- sw[FALSE,]
  checkIdentical(rbind(XDataFrame(), zr, zr[,1:2]), zr)
  checkIdentical(as.data.frame(rbind(XDataFrame(), zr, sw)), swiss)
  swissrbind <- do.call(rbind, swisssplit)
  rownames(swissrbind) <- NULL
  rownames(sw) <- NULL
  swsplit <- split(sw, sw[["Education"]])
  checkIdentical(as.data.frame(do.call(rbind, as.list(swsplit))), swissrbind)

  ## combining factors
  df1 <- data.frame(species = c("Mouse", "Chicken"), n = c(5, 6))
  xdf1 <- XDataFrame(df1)
  df2 <- data.frame(species = c("Human", "Chimp"), n = c(1, 2))
  xdf2 <- XDataFrame(df2)
  df12 <- rbind(df1, df2)
  rownames(df12) <- NULL
  checkIdentical(as.data.frame(rbind(xdf1, xdf2)), df12)
  
  rownames(sw) <- rn
  checkIdentical(rownames(rbind(sw, XDataFrame(swiss))), NULL)  
  swsplit <- split(sw, sw[["Education"]])
  rownames(swiss) <- rn
  swisssplit <- split(swiss, swiss$Education)
  checkIdentical(rownames(do.call(rbind, as.list(swsplit))),
                 unlist(lapply(swisssplit, rownames), use.names=FALSE))

  checkException(rbind(sw[,1:2], sw), silent = TRUE)
  other <- sw
  colnames(other)[1] <- "foo"
  checkException(rbind(other, sw), silent = TRUE)
}

test_XDataFrame_looping <- function() {
  data(iris)
  actual <- by(iris, iris$Species, nrow)
  ## a bit tricky because of the 'call' attribute
  attr(actual, "call")[[1]] <- as.name("by")
  iris <- XDataFrame(iris, row.names=rownames(iris))
  checkIdentical(actual, by(iris, iris$Species, nrow))
}
