test_DataFrameList_construction <- function() {
    checkDFL2dfl <- function(DFL, dfl) {
        checkIdentical(lapply(as.list(DFL), as.data.frame), dfl)
    }
    data(swiss)
    checkDFL2dfl(DataFrameList(swiss, swiss), list(swiss, swiss))
}

test_SplitDataFrameList_construction <- function() {
    striprownames <- function(x) {
        lapply(x, function(y) {
                   rownames(y) <- NULL
                   y
               })
    }
    data(swiss)
    sw <- DataFrame(swiss, row.names=rownames(swiss))

    swsplit1 <- split(sw, sw[["Education"]])
    swsplit2 <-
      SplitDataFrameList(lapply(split(swiss, swiss[["Education"]]),
                                as, "DataFrame"))
    checkIdentical(swsplit1, swsplit2)

    sw <- DataFrame(swiss, row.names = rownames(swiss))
    swisssplit <- split(swiss, swiss[["Education"]])
    swsplit <- split(sw, sw[["Education"]])
    checkIdentical(lapply(as.list(swsplit), as.data.frame), swisssplit)

    swiss2 <- swiss
    rownames(swiss2) <- NULL
    sw2 <- DataFrame(swiss2)
    swiss2split <- striprownames(split(swiss2, swiss2[["Education"]]))
    sw2split <- split(sw2, sw2[["Education"]])
    checkIdentical(lapply(as.list(sw2split), as.data.frame), swiss2split)
}

test_SplitDataFrameList_replace <- function() {
    striprownames <- function(x) {
        lapply(x, function(y) {
                   rownames(y) <- NULL
                   y
               })
    }
    data(swiss)
    swiss2 <- swiss
    rownames(swiss2) <- NULL
    sw2 <- DataFrame(swiss2)

    swiss2split <- striprownames(split(swiss2, swiss2[["Education"]]))
    sw2split <- split(sw2, sw2[["Education"]])
    swiss2split[] <- swiss2split[1]
    sw2split[] <- sw2split[1]
    checkIdentical(lapply(as.list(sw2split), as.data.frame), swiss2split)

    swiss2split <- striprownames(split(swiss2, swiss2[["Education"]]))
    sw2split <- split(sw2, sw2[["Education"]])
    swiss2split[c(2, 4, 5)] <- swiss2split[1]
    sw2split[c(2, 4, 5)] <- sw2split[1]
    checkIdentical(lapply(as.list(sw2split), as.data.frame), swiss2split)
}
