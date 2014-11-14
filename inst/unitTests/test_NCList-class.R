###

.compareHits <- function(target, current) identical(t(target), t(current))

findOverlaps_NCList <- IRanges:::findOverlaps_NCList

.overlap_score <- function(query, subject)
{
    pmin(end(query), end(subject)) - pmax(start(query), start(subject)) + 1L
}

.findOverlaps_naive <- function(query, subject,
                           maxgap=0L, minoverlap=1L,
                           type=c("any", "start", "end", "within", "equal"),
                           select=c("all", "first", "last"))
{
    min_score <- IRanges:::.min_overlap_score(maxgap, minoverlap)
    type <- match.arg(type)
    select <- match.arg(select)
    type_codes <- switch(type,
        "start"  = c("f", "g", "h"),
        "end"    = c("d", "g", "j"),
        "within" = c("f", "g", "i", "j"),
        "equal"  = "g"
    )
    hits_per_query <- lapply(seq_along(query),
        function(i) {
            query_i <- query[i]
            score_is_ok <- .overlap_score(query_i, subject) >= min_score
            if (type == "any")
                return(which(score_is_ok))
            codes <- rangeComparisonCodeToLetter(compare(query_i, subject))
            which(score_is_ok & codes %in% type_codes)
        })
    if (select == "all") {
        q_hits <- rep.int(seq_along(query), elementLengths(hits_per_query))
        s_hits <- unlist(hits_per_query, use.names=FALSE)
        ans <- new("Hits", queryHits=q_hits, subjectHits=s_hits,
                           queryLength=length(query),
                           subjectLength=length(subject))
        return(ans)
    }
    ans <- integer(length(query))
    ans[] <- NA_integer_
    idx1 <- which(elementLengths(hits_per_query) != 0L)
    if (length(idx1) != 0L) {
        select_FUN <- switch(select, "first"=min, "last"=max)
        ans[idx1] <- sapply(hits_per_query[idx1], select_FUN)
    }
    ans
}

test_findOverlaps_NCList <- function()
{
    query <- IRanges(-3:7, width=3)
    subject <- IRanges(rep.int(1:5, 5:1), c(1:5, 2:5, 3:5, 4:5, 5))

    current0 <- findOverlaps_NCList(query, NCList(subject))
    target0 <- .findOverlaps_naive(query, subject)
    checkTrue(.compareHits(target0, current0))

    permute_subject <- function(perm) {
        current <- findOverlaps_NCList(query, NCList(subject[perm]))
        revperm <- integer(length(perm))
        revperm[perm] <- seq_along(perm)
        target <- target0
        target@subjectHits <- revperm[target@subjectHits]
        checkTrue(.compareHits(target, current))
    }

    ## Reverse subject.
    permute_subject(rev(seq_along(subject)))

    ## Random permutations of subject.
    set.seed(333)
    for (i in 1:99)
        permute_subject(sample(length(subject)))
}

test_findOverlaps_NCList_with_filtering <- function()
{
    query <- IRanges(-3:7, width=3)
    subject <- IRanges(rep.int(1:5, 5:1), c(1:5, 2:5, 3:5, 4:5, 5))

    pp_subject <- NCList(subject)
    maxgap_minoverlap <- list(c(0,1), c(0,2), c(0,3), c(0,4),
                              c(1,1), c(2,1), c(3,1), c(4,1))
    for (m in maxgap_minoverlap) {
        maxgap <- m[[1L]]
        minoverlap <- m[[2L]]
        for (type in c("any", "start", "end", "within", "equal")) {
            for (select in c("all", "first", "last")) {
                #cat("maxgap=", maxgap, " minoverlap=", minoverlap,
                #    " type=", type, " select=", select, "\n", sep="")
                current <- findOverlaps_NCList(query, pp_subject,
                                               maxgap=maxgap,
                                               minoverlap=minoverlap,
                                               type=type, select=select)
                target <- .findOverlaps_naive(query, subject,
                                              maxgap=maxgap,
                                              minoverlap=minoverlap,
                                              type=type, select=select)
                checkTrue(.compareHits(target, current))
            }
        }
    }
}

