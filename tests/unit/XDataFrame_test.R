test_XDataFrame_construction <- function() {
  score <- c(1.5, 3, NA)
  counts <- c(10L, 2L, NA)

  xdf <- XDataFrame()
  checkTRUE(validObject(xdf))
  xdf <- XDataFrame(score = score)
  checkTRUE(validObject(xdf))
  checkIdentical(xdf[["score"]], score)
  xdf <- XDataFrame(filter, score = score)
  checkTRUE(validObject(xdf))
  checkIdentical(xdf[["score"]], score)
  checkIdentical(xdf[["filter"]], filter)
  xdf <- XDataFrame(filter = filter, vals = score)
  checkTRUE(validObject(xdf))
  checkIdentical(xdf[["score"]], score)
  checkIdentical(xdf[["filter"]], vals)
  xdf <- XDataFrame(score + score)
  checkTRUE(validObject(xdf))
  checkIdentical(xdf[["score...score"]], score + score)

  sw <- XDataFrame(swiss)
  checkIdentical(as.data.frame(sw), swiss)

  sw <- XDataFrame(swiss[,1:3], swiss[,4:ncol(swiss)])
  checkIdentical(as.data.frame(sw),
                 data.frame(swiss[,1:3], swiss[,4:ncol(swiss)]))

  sw <- XDataFrame(swiss[,1:3], foo = swiss[,4:ncol(swiss)])
  checkIdentical(as.data.frame(sw),
                 data.frame(swiss[,1:3], foo = swiss[,4:ncol(swiss)]))
}

test_XDataFrame_subset <- function() {
  sw <- XDataFrame(swiss)

  checkIdentical(sw[], sw) # identity subset
  checkIdentical(sw[,], sw)
  
  checkIdentical(sw[1:3], XDataFrame(swiss[1:3]))      # select columns
  checkIdentical(sw[, 1:3], XDataFrame(swiss[1:3]))    # same
  ## select rows and columns
  checkIdentical(sw[4:5, 1:3], XDataFrame(swiss[4:5,1:3])) 
  
  checkIdentical(sw[1], XDataFrame(swiss[1]))        # a one-column data frame
  checkIdentical(sw[, "Fertility"], XDataFrame(swiss[, "Fertility"]))
  ## the same
  checkIdentical(sw[, 1, drop = FALSE], XDataFrame(swiss[, 1, drop = FALSE]))
  checkIdentical(sw[, 1], XDataFrame(swiss[,1]))      # a (unnamed) vector
  checkIdentical(sw[[1]], XDataFrame(swiss[[1]]))      # the same
  checkIdentical(sw[["Fertility"]], XDataFrame(swiss[["Fertility"]]))
  checkIdentical(sw[,c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)],
                 XDataFrame(swiss[,c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)]))
  
  checkIdentical(sw[1,], XDataFrame(swiss[1,]))       # a one-row data frame
  checkIdentical(sw[1,, drop=TRUE], XDataFrame(swiss[1,, drop=TRUE]))   # a list
  checkIdentical(sw["Courtelary",], XDataFrame(swiss["Courtelary",]))
  checkIdentical(sw[c(TRUE, rep(FALSE, nrow(sw)-1)),],
                 XDataFrame(swiss[c(TRUE, rep(FALSE, nrow(sw)-1)),]))
  
  checkIdentical(sw["C", ], XDataFrame(swiss["C",])) # partially matches
  checkIdentical(sw[match("C", row.names(sw)), ], # no exact match
                 XDataFrame(swiss[match("C", row.names(sw)), ])) 

  checkIdentical(swiss[c(1, 1:2),], #duplicate row, unique row names are created
                 XDataFrame(swiss[c(1,1:2),]))
  checkIdentical(swiss[c(1, NA, 1:2, NA),], # mixin some NAs
                 XDataFrame(swiss[c(1, NA, 1:2, NA),]))
}
