###

findOverlaps_NCList <- IRanges:::findOverlaps_NCList

.compareHits <- function(target, current) identical(t(target), t(current))

.overlap_score <- function(query, subject)
{
    pmin(end(query), end(subject)) - pmax(start(query), start(subject)) + 1L
}

.get_query_overlaps <- function(query, subject, min.score, type_codes)
{
    is_hit <- .overlap_score(query, subject) >= min.score
    if (!is.null(type_codes)) {
        codes <- rangeComparisonCodeToLetter(compare(query, subject))
        is_hit <- is_hit & codes %in% type_codes
    }
    which(is_hit)
}

.findOverlaps_naive <- function(query, subject, min.score=1L,
                                type=c("any", "start", "end",
                                       "within", "extend", "equal"),
                                select=c("all", "first", "last"))
{
    type <- match.arg(type)
    select <- match.arg(select)
    type_codes <- switch(type,
        "any"    = NULL,
        "start"  = c("f", "g", "h"),
        "end"    = c("d", "g", "j"),
        "within" = c("f", "g", "i", "j"),
        "extend" = c("d", "e", "g", "h"),
        "equal"  = "g"
    )
    hits_per_query <- lapply(seq_along(query),
        function(i) .get_query_overlaps(query[i], subject,
                                        min.score, type_codes))
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

    target0 <- .findOverlaps_naive(query, subject)
    current <- findOverlaps_NCList(query, NCList(subject))
    checkTrue(.compareHits(target0, current))
    current <- findOverlaps_NCList(NCList(query), subject)
    checkTrue(.compareHits(target0, current))

    ## Shuffle query and/or subject elements.
    permute_input <- function(q_perm, s_perm) {
        q_revperm <- integer(length(q_perm))
        q_revperm[q_perm] <- seq_along(q_perm)
        s_revperm <- integer(length(s_perm))
        s_revperm[s_perm] <- seq_along(s_perm)
        target <- remapHits(target0, query.map=q_revperm,
                                     new.queryLength=length(q_perm),
                                     subject.map=s_revperm,
                                     new.subjectLength=length(s_perm))
        current <- findOverlaps_NCList(query[q_perm], NCList(subject[s_perm]))
        checkTrue(.compareHits(target, current))
        current <- findOverlaps_NCList(NCList(query[q_perm]), subject[s_perm])
        checkTrue(.compareHits(target, current))
    }

    q_perm <- rev(seq_along(query))
    s_perm <- rev(seq_along(subject))
    permute_input(q_perm, seq_along(subject))  # reverse query
    permute_input(seq_along(query), s_perm)    # reverse subject
    permute_input(q_perm, s_perm)              # reverse both

    set.seed(97)
    for (i in 1:33) {
        ## random permutations
        q_perm <- sample(length(query))
        s_perm <- sample(length(subject))
        permute_input(q_perm, seq_along(subject))
        permute_input(seq_along(query), s_perm)
        permute_input(q_perm, s_perm)
    }
}

test_findOverlaps_NCList_with_filtering <- function()
{
    query <- IRanges(-3:7, width=3)
    subject <- IRanges(rep.int(1:5, 5:1), c(1:5, 2:5, 3:5, 4:5, 5))

    pp_query <- NCList(query)
    pp_subject <- NCList(subject)
    for (min.score in -3:4) {
        for (type in c("any", "start", "end", "within", "extend", "equal")) {
            for (select in c("all", "first", "last")) {
                #cat("min.score=", min.score,
                #    " type=", type, " select=", select, "\n", sep="")
                target <- .findOverlaps_naive(query, subject,
                                              min.score=min.score,
                                              type=type, select=select)
                current <- findOverlaps_NCList(query, pp_subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compareHits(target, current))
                current <- findOverlaps_NCList(pp_query, subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compareHits(target, current))
            }
        }
    }
}

test_findOverlaps_NCList_with_circular_space <- function()
{
    query0 <- IRanges(-2:17, width=3)
    subject0 <- IRanges(c(4, -1, 599), c(7, 0, 999))
    circle_length <- 10L
    x1 <- x2 <- x3 <- integer(length(query0))
    x1[c(5:10, 15:20L)] <- 1L
    x2[c(1:3, 10:13, 20L)] <- 1L
    x3[] <- 1L
    target1 <- findMatches(x1, 1L)
    target2 <- findMatches(x2, 1L)
    target3 <- findMatches(x3, 1L)
    target1 <- remapHits(target1, subject.map=1L, new.subjectLength=3L)
    target2 <- remapHits(target2, subject.map=2L, new.subjectLength=3L)
    target3 <- remapHits(target3, subject.map=3L, new.subjectLength=3L)
    target0 <- union(union(target1, target2), target3)

    for (i in -2:2) {
        query <- shift(query0, shift=i*circle_length)
        pp_query <- NCList(query, circle.length=circle_length)
        for (j in -2:2) {
            subject <- shift(subject0, shift=j*circle_length)
            pp_subject <- NCList(subject, circle.length=circle_length)
            for (select in c("all", "first", "last")) {
                target <- selectHits(target0, select=select)
                current <- findOverlaps_NCList(query, pp_subject,
                                               select=select,
                                               circle.length=circle_length)
                checkTrue(.compareHits(target, current))
                current <- findOverlaps_NCList(pp_query, subject,
                                               select=select,
                                               circle.length=circle_length)
                checkTrue(.compareHits(target, current))

                target <- selectHits(t(target0), select=select)
                current <- findOverlaps_NCList(pp_subject, query,
                                               select=select,
                                               circle.length=circle_length)
                checkTrue(.compareHits(target, current))
                current <- findOverlaps_NCList(subject, pp_query,
                                               select=select,
                                               circle.length=circle_length)
                checkTrue(.compareHits(target, current))
            }
        }
    }
}

