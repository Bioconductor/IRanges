### =========================================================================
### nearest (and related) methods
### -------------------------------------------------------------------------
###


setClassUnion("RangesORmissing", c("Ranges", "missing"))


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### precede() and follow()
###

setGeneric("precede", function(x, subject = x, ...) standardGeneric("precede"))

setMethod("precede", c("Ranges", "RangesORmissing"),
    function(x, subject, select = c("first", "all"))
    {
      select <- match.arg(select)
      s <- start(subject)
      ord <- NULL
      if (isNotSorted(s)) {
        ord <- orderInteger(s)
        s <- s[ord]
      }
      if (select == "all") {
        srle <- Rle(s)
        s <- runValue(srle)
      }
      i <- findInterval(end(x), s) + 1L
      i[i > length(s)] <- NA
      if (select == "all") {
        .vectorToHits(i, srle, ord)
      } else {
        if (!is.null(ord))
          i <- ord[i]
        i
      }
    }
)

setGeneric("follow", function(x, subject = x, ...) standardGeneric("follow"))

setMethod("follow", c("Ranges", "RangesORmissing"),
    function(x, subject, select = c("last", "all"))
    {
      select <- match.arg(select)
      e <- end(subject)
      ord <- NULL
      if (isNotSorted(e)) {
        ord <- orderInteger(e)
        e <- e[ord]
      }
      if (select == "all") {
        srle <- Rle(e)
        e <- runValue(srle)
      }
      i <- findInterval(start(x) - 1L, e)
      i[i == 0] <- NA        
      if (select == "all") {
        .vectorToHits(i, srle, ord)
      } else {
        if (!is.null(ord))
          i <- ord[i]
        i
      }
    }
)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### nearest()
###

.hitsMatrixToVector <- function(hitsMatrix, queryLength) {
  hitsMatrix <-
    hitsMatrix[diffWithInitialZero(hitsMatrix[,1L,drop=TRUE]) != 0L,,
                drop=FALSE]
  ans <- rep.int(NA_integer_, queryLength)
  ans[hitsMatrix[,1L,drop=TRUE]] <- hitsMatrix[,2L,drop=TRUE]
  ans
}

.vectorToHits <- function(i, srle, ord) {
  lx <- length(i)
  v <- !is.na(i)
  i <- i[v]
  w <- width(srle)[i]
  subj <- as.integer(IRanges(start(srle)[i], width=w))
  m <- cbind(queryHits = rep(seq(lx)[v], w),
             subjectHits = if (!is.null(ord)) ord[subj] else subj)
  if (!is.null(ord))
    m <- m[orderIntegerPairs(m[,1L], m[,2L]),,drop=FALSE]
  ## unname() required because in case 'm' has only 1 row
  ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
  new("Hits", queryHits = unname(m[ , 1L]), subjectHits = unname(m[ , 2L]),
              queryLength = lx, subjectLength = length(srle))
}

setGeneric("nearest", function(x, subject, ...) standardGeneric("nearest"))

setMethod("nearest", c("Ranges", "RangesORmissing"),
          function(x, subject, select = c("arbitrary", "all"))
          {
            select <- match.arg(select)
            if (!missing(subject)) {
              ol <- findOverlaps(x, subject, select = select)
            } else {
              subject <- x
              ol <- findOverlaps(x, select = select, ignoreSelf = TRUE)
            }
            if (select == "all") {
              m <- as.matrix(ol)
              olv <- .hitsMatrixToVector(m, length(x))
            } else olv <- ol
            x <- x[is.na(olv)]
            before <- precede(x, subject,
                              if (select == "all") "all" else "first")
            after <- follow(x, subject,
                            if (select == "all") "all" else "last")
            if (select == "all") {
              before_m <- as.matrix(before)
              before <- .hitsMatrixToVector(before_m, length(x))
              after_m <- as.matrix(after)
              after <- .hitsMatrixToVector(after_m, length(x))
            }
            leftdist <- (start(subject)[before] - end(x))
            rightdist <- (start(x) - end(subject)[after])
            left <- leftdist < rightdist
            left[is.na(left)] <- is.na(after)[is.na(left)]
            if (select == "all") {
              filterMatchMatrix <- function(m, i) {
                qrle <- Rle(m[,1L])
                qstart <- qend <- integer(length(i))
                qstart[runValue(qrle)] <- start(qrle)
                qend[runValue(qrle)] <- end(qrle)
                rows <- as.integer(IRanges(qstart[i], qend[i]))
                m <- m[rows,,drop=FALSE]
                m[,1L] <- map[m[,1L]]
                m
              }
              map <- which(is.na(olv))
              right <- !left
              left[leftdist == rightdist] <- TRUE
              m <- rbind(m, filterMatchMatrix(before_m, left),
                         filterMatchMatrix(after_m, right))
              m <- m[orderIntegerPairs(m[,1L], m[,2L]),, drop=FALSE]
              ## unname() required because in case 'm' has only 1 row
              ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
              ol@queryHits <- unname(m[ , 1L])
              ol@subjectHits <- unname(m[ , 2L])
            } else {
              olv[is.na(olv)] <- ifelse(left, before, after)
              ol <- olv
            }
            ol
          })


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### distance()
###

setGeneric("distance",
           function(x, y, ...) standardGeneric("distance"))

setMethod("distance", c("Ranges", "Ranges"), function(x, y) {
  if (length(x) != length(y))
    stop("'x' and 'y' must have the same length")
  ans_end_plus1 <- pmax.int(start(x), start(y))
  ans_start <- pmin.int(end(x), end(y))
  ans <- ans_end_plus1 - ans_start
  pmax(ans, if (length(x)) 0L else integer())
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### distanceToNearest()
###

setGeneric("distanceToNearest",
           function(x, subject = x, ...) standardGeneric("distanceToNearest"))

setMethod("distanceToNearest", c("Ranges", "RangesORmissing"),
          function(x, subject, select = c("arbitrary", "all"))
          {
            select <- match.arg(select)
            if (missing(subject)) {
              subject <- x
              x_nearest <- nearest(x, select = select)
            } else x_nearest <- nearest(x, subject, select = select)
            if (select == "arbitrary")
              x_nearest <- cbind(queryHits = seq(length(x)),
                                 subjectHits = x_nearest)
            else x_nearest <- as.matrix(x_nearest)
            x <- x[x_nearest[,1]]
            subject <- subject[x_nearest[,2]]
            DataFrame(x_nearest, distance = distance(x, subject))
          })

