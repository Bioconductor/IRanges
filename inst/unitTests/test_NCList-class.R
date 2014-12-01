###

findOverlaps_NCList <- IRanges:::findOverlaps_NCList
findOverlaps_NCLists <- IRanges:::findOverlaps_NCLists

.transpose_hits <- function(hits)
{
    if (is.list(hits))
        return(lapply(hits, .transpose_hits))
    t(hits)
}

.compare_hits <- function(target, current)
{
    if (is.list(target) || is(target, "List")
     && is.list(current) || is(current, "List"))
        return(all(mapply(.compare_hits, target, current)))
    identical(.transpose_hits(target), .transpose_hits(current))
}

.make_Hits_from_q2s <- function(q2s, s_len)
{
    q_hits <- rep.int(seq_along(q2s), elementLengths(q2s))
    s_hits <- as.integer(unlist(q2s, use.names=FALSE))
    new("Hits", queryHits=q_hits, subjectHits=s_hits,
                queryLength=length(q2s),
                subjectLength=as.integer(s_len))
}

.make_Hits_from_s2q <- function(s2q, q_len)
    .transpose_hits(.make_Hits_from_q2s(s2q, q_len))

.select_hits <- function(x, select)
{
    if (is.list(x))
        return(lapply(x, .select_hits, select))
    selectHits(x, select)
}

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
                                select=c("all", "first", "last", "count"))
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
    if (select == "all")
        return(.make_Hits_from_q2s(hits_per_query, length(subject)))
    if (select == "count")
        return(elementLengths(hits_per_query))
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
    checkTrue(.compare_hits(target0, current))
    current <- findOverlaps_NCList(NCList(query), subject)
    checkTrue(.compare_hits(target0, current))
    current <- findOverlaps_NCList(query, subject)
    checkTrue(.compare_hits(target0, current))

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
        checkTrue(.compare_hits(target, current))
        current <- findOverlaps_NCList(NCList(query[q_perm]), subject[s_perm])
        checkTrue(.compare_hits(target, current))
        current <- findOverlaps_NCList(query[q_perm], subject[s_perm])
        checkTrue(.compare_hits(target, current))
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
            for (select in c("all", "first", "last", "count")) {
                #cat("min.score=", min.score,
                #    " type=", type, " select=", select, "\n", sep="")
                target <- .findOverlaps_naive(query, subject,
                                              min.score=min.score,
                                              type=type, select=select)
                current <- findOverlaps_NCList(query, pp_subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_NCList(pp_query, subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_NCList(query, subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
            }
        }
    }
}

.test_circularity <- function(query0, subject0, circle_length, target0,
                              pp, findOverlaps_pp, type)
{
    for (i in -2:2) {
        query <- shift(query0, shift=i*circle_length)
        pp_query <- pp(query, circle.length=circle_length)
        for (j in -2:2) {
            subject <- shift(subject0, shift=j*circle_length)
            pp_subject <- pp(subject, circle.length=circle_length)
            for (select in c("all", "first", "last", "count")) {
                target <- .select_hits(target0, select=select)
                current <- findOverlaps_pp(query, pp_subject,
                                           type=type, select=select,
                                           circle.length=circle_length)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_pp(pp_query, subject,
                                           type=type, select=select,
                                           circle.length=circle_length)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_pp(query, subject,
                                           type=type, select=select,
                                           circle.length=circle_length)
                checkTrue(.compare_hits(target, current))

                target <- .select_hits(.transpose_hits(target0), select=select)
                current <- findOverlaps_pp(pp_subject, query,
                                           type=type, select=select,
                                           circle.length=circle_length)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_pp(subject, pp_query,
                                           type=type, select=select,
                                           circle.length=circle_length)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_pp(subject, query,
                                           type=type, select=select,
                                           circle.length=circle_length)
                checkTrue(.compare_hits(target, current))
            }
        }
    }
}

test_findOverlaps_NCList_with_circular_space <- function()
{
    query <- IRanges(-2:17, width=3)
    subject <- IRanges(c(4, -1, 599), c(7, 0, 999))
    circle_length <- 10L

    ## type "any"
    s2q <- list(c(5:10, 15:20L), c(1:3, 10:13, 20L), 1:20)
    target <- .make_Hits_from_s2q(s2q, length(query))
    .test_circularity(query, subject, circle_length, target,
                      NCList, findOverlaps_NCList, "any")

    ## type "start"
    s2q <- lapply(start(subject),
                  function(s)
                      which((start(query) - s) %% circle_length == 0L))
    target <- .make_Hits_from_s2q(s2q, length(query))
    .test_circularity(query, subject, circle_length, target,
                      NCList, findOverlaps_NCList, "start")

    ## type "end"
    s2q <- lapply(end(subject),
                  function(e)
                      which((end(query) - e) %% circle_length == 0L))
    target <- .make_Hits_from_s2q(s2q, length(query))
    .test_circularity(query, subject, circle_length, target,
                      NCList, findOverlaps_NCList, "end")
}

test_findOverlaps_NCLists <- function()
{
    ir1 <- IRanges(-3:7, width=3)
    ir2 <- IRanges(rep.int(1:5, 5:1), c(1:5, 2:5, 3:5, 4:5, 5))
    target0 <- mapply(findOverlaps_NCList, list(ir1, ir2), list(ir2, ir1))
    for (compress in c(TRUE, FALSE)) {
        query <- IRangesList(ir1, ir2, IRanges(2, 7), compress=compress)
        pp_query <- NCLists(query)
        subject <- IRangesList(ir2, ir1, compress=compress)
        pp_subject <- NCLists(subject)
        for (select in c("all", "first", "last", "count")) {
            target <- .select_hits(target0, select=select)
            current <- findOverlaps_NCLists(query, pp_subject, select=select)
            checkTrue(.compare_hits(target, current))
            current <- findOverlaps_NCLists(pp_query, subject, select=select)
            checkTrue(.compare_hits(target, current))
            current <- findOverlaps_NCLists(query, subject, select=select)
            checkTrue(.compare_hits(target, current))
        }
    }
}

test_findOverlaps_NCLists_with_circular_space <- function()
{
    query1 <- IRanges(-2:17, width=3)
    subject1 <- IRanges(c(4, -1, 599), c(7, 0, 999))
    query <- IRangesList(query1, IRanges(), subject1)
    subject <- IRangesList(subject1, IRanges(), query1)
    circle_length <- c(10L, NA_integer_, 10L)

    s2q <- list(c(5:10, 15:20L), c(1:3, 10:13, 20L), 1:20)
    target1 <- .make_Hits_from_s2q(s2q, length(query1))
    target2 <- .make_Hits_from_s2q(list(), 0)
    target3 <- .transpose_hits(target1)
    target <- list(target1, target2, target3)
    .test_circularity(query, subject, circle_length, target,
                      NCLists, findOverlaps_NCLists, "any")
}

