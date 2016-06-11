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
      if (S4Vectors:::isNotSorted(s)) {
        ord <- base::order(s)
        s <- s[ord]
      }
      if (select == "all") {
        srle <- Rle(s)
        s <- runValue(srle)
      }
      i <- findInterval(end(x), s) + 1L
      i[i > length(s)] <- NA
      if (select == "all") {
        vectorToHits(i, srle, ord)
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
      if (S4Vectors:::isNotSorted(e)) {
        ord <- base::order(e)
        e <- e[ord]
      }
      if (select == "all") {
        srle <- Rle(e)
        e <- runValue(srle)
      }
      i <- findInterval(start(x) - 1L, e)
      i[i == 0] <- NA        
      if (select == "all") {
        vectorToHits(i, srle, ord)
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

### Used in GenomicRanges.
### TODO: Move to Hits-class.R
vectorToHits <- function(i, srle, ord) {
  lx <- length(i)
  v <- !is.na(i)
  i <- i[v]
  w <- width(srle)[i]
  subj <- as.integer(IRanges(start(srle)[i], width=w))
  m <- cbind(queryHits = rep(seq(lx)[v], w),
             subjectHits = if (!is.null(ord)) ord[subj] else subj)
  if (!is.null(ord))
    m <- m[orderIntegerPairs(m[,1L], m[,2L]),,drop=FALSE]
  Hits(m[ , 1L], m[ , 2L], lx, length(srle), sort.by.query=TRUE)
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
              ol <- findOverlaps(x, select = select, drop.self = TRUE)
            }
            if (select == "all") {
              olv <- selectHits(ol, select="first")
            } else olv <- ol
            x <- x[is.na(olv)]
            before <- precede(x, subject,
                              if (select == "all") "all" else "first")
            after <- follow(x, subject,
                            if (select == "all") "all" else "last")
            if (select == "all") {
              before0 <- before
              before <- selectHits(before, select="first")
              after0 <- after
              after <- selectHits(after, select="first")
            }
            leftdist <- (start(subject)[before] - end(x))
            rightdist <- (start(x) - end(subject)[after])
            left <- leftdist < rightdist
            left[is.na(left)] <- is.na(after)[is.na(left)]
            if (select == "all") {
              filterHits <- function(hits, i) {
                m <- as.matrix(hits[as(hits, "IRanges")[i]])
                m[,1L] <- map[m[,1L]]
                m
              }
              map <- which(is.na(olv))
              right <- !left
              left[leftdist == rightdist] <- TRUE
              m <- rbind(as.matrix(ol), filterHits(before0, left),
                                        filterHits(after0, right))
              m <- m[orderIntegerPairs(m[,1L], m[,2L]),, drop=FALSE]
              ## unname() required because in case 'm' has only 1 row
              ## 'm[ , 1L]' and 'm[ , 2L]' will return a named atomic vector
              ol@from <- unname(m[ , 1L])
              ol@to <- unname(m[ , 2L])
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

setMethod("distance", c("Ranges", "Ranges"), 
    function(x, y) 
    {
        max_start <- pmax.int(start(x), start(y))
        min_end <- pmin.int(end(x), end(y))
        pmax.int(max_start - min_end - 1L, 0L) 
    }
)

setMethod("distance", c("Pairs", "missing"),
          function(x, y) {
              distance(first(x), second(x))
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
        } else {
            x_nearest <- nearest(x, subject, select = select)
        }
        if (select == "arbitrary") {
            queryHits <- seq_along(x)[!is.na(x_nearest)]
            subjectHits <- x_nearest[!is.na(x_nearest)]
        } else {
            queryHits <- queryHits(x_nearest)
            subjectHits <- subjectHits(x_nearest)
        }

        if (!length(subjectHits) || all(is.na(subjectHits))) {
            Hits(nLnode=length(x), 
                 nRnode=length(subject),
                 distance=integer(0),
                 sort.by.query=TRUE)
        } else {
            distance = distance(x[queryHits], subject[subjectHits])
            Hits(queryHits, subjectHits, length(x), length(subject), distance,
                 sort.by.query=TRUE)
        }
    }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selectNearest()
###

selectNearest <- function(hits, x, subject) {
    hits <- as(hits, "SortedByQueryHits")
    hitsByQuery <- relist(hits, as(hits, "Partitioning"))
    dist <- distance(x[queryHits(hits)], subject[subjectHits(hits)])
    distByQuery <- relist(dist, hitsByQuery)
    unlist(hitsByQuery[distByQuery == min(distByQuery)])
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Find 'k' nearest neighbors
###
### FIXME: Largely untested code; unexported for now
###

findKNN <- function(query, subject, k=5L, ignore.overlaps = FALSE,
                    ignore.strand = FALSE)
{
    seqlevels(subject) <- seqlevels(query)
    
    starts <- with(subject, GRanges(seqnames, IRanges(start, width=1L), strand))
    ends <- with(subject, GRanges(seqnames, IRanges(end, width=1L), strand))

    if (ignore.strand) {
        starts <- unstrand(starts)
        ends <- unstrand(ends)
    }
    
    start_ord <- order(starts)
    end_ord <- order(ends)

    starts <- starts[start_ord]
    ends <- ends[end_ord]

    phits <- precede(query, starts, ignore.strand=ignore.strand)
    fhits <- follow(query, ends, ignore.strand=ignore.strand)

    if (!ignore.strand) {
        exchange <- decode(strand(query) == "-")
        tmp <- phits[exchange]
        phits[exchange] <- fhits[exchange]
        fhits[exchange] <- tmp
    }

    findPart <- function(x, w) {
        S4Vectors:::findIntervalAndStartFromWidth(x, w)[["interval"]]
    }
    
    if (!ignore.strand) {
        b <- width(disjoin(c(ranges(seqnames(starts)), ranges(strand(starts)))))
    } else {
        b <- runLength(seqnames(starts))
    }
    
    seqends <- end(seqnames(starts))[findPart(phits, b)]
    phits[is.na(phits)] <- 1L
    seqends[is.na(seqends)] <- 0L
    pwindows <- restrict(IRanges(phits, width = k), end=seqends)

    seqstarts <- start(seqnames(ends))[findPart(fhits, b)]
    seqstarts[is.na(seqstarts)] <- 1L
    fhits[is.na(fhits)] <- 0L
    fwindows <- restrict(IRanges(end=fhits, width = k), seqstarts)
    
    dist <- pc(extractList(start(starts), pwindows) - end(query),
               end(query) - extractList(end(ends), fwindows))

    ans <- pc(extractList(start_ord, pwindows), extractList(end_ord, fwindows))

    if (!ignore.overlaps) {
        hits <- findOverlaps(query, subject, ignore.strand=ignore.strand)
        hitsList <- as(hits, "List")
        dist <- pc(dist, relist(rep(0L, length(hits)), hitsList))
        ans <- pc(ans, hitsList)
    }

    ans[phead(order(dist), k)]
}
