###

findOverlaps_NCList <- IRanges:::findOverlaps_NCList
findOverlaps_NCLists <- IRanges:::findOverlaps_NCLists

.transpose_hits <- function(hits)
{
    if (is.list(hits))
        return(lapply(hits, .transpose_hits))
    t(hits)
}

### Used in the unit tests for GNCList located in GenomicRanges.
.compare_hits <- function(target, current)
{
    if (is.list(target) || is(target, "List")
     && is.list(current) || is(current, "List"))
        return(all(mapply(.compare_hits, target, current)))
    identical(.transpose_hits(target), .transpose_hits(current))
}

### Used in the unit tests for GNCList located in GenomicRanges.
.make_Hits_from_q2s <- function(q2s, s_len)
{
    q_hits <- rep.int(seq_along(q2s), elementLengths(q2s))
    s_hits <- as.integer(unlist(q2s, use.names=FALSE))
    Hits(q_hits, s_hits, length(q2s), s_len)
}

.make_Hits_from_s2q <- function(s2q, q_len)
    .transpose_hits(.make_Hits_from_q2s(s2q, q_len))

.select_hits <- function(x, select)
{
    if (is.list(x))
        return(lapply(x, .select_hits, select))
    selectHits(x, select)
}

### Used in the unit tests for GNCList located in GenomicRanges.
.overlap_score <- function(query, subject)
{
    pmin(end(query), end(subject)) - pmax(start(query), start(subject)) + 1L
}

.get_query_overlaps <- function(query, subject, min.score, type)
{
    ok <- .overlap_score(query, subject) >= min.score
    if (type %in% c("start", "end")) {
        if (type == "start")
            d <- abs(start(subject) - start(query))
        if (type == "end") 
            d <- abs(end(subject) - end(query))
        if (min.score >= 1L) {
            dmax <- 0L
        } else {
            dmax <- 1L - min.score
        }
        ok <- ok & (d <= dmax)
    } else if (type != "any") {
        codes <- rangeComparisonCodeToLetter(compare(query, subject))
        type_codes <- switch(type,
            #"start"  = c("f", "g", "h"),
            #"end"    = c("d", "g", "j"),
            "within" = c("f", "g", "i", "j"),
            "extend" = c("d", "e", "g", "h"),
            "equal"  = "g"
        )
        ok <- ok & (codes %in% type_codes)
    }
    which(ok)
}

.findOverlaps_naive <- function(query, subject, min.score=1L,
                                type=c("any", "start", "end",
                                       "within", "extend", "equal"),
                                select=c("all", "first", "last", "arbitrary",
                                         "count"))
{
    type <- match.arg(type)
    select <- match.arg(select)
    hits_per_query <- lapply(seq_along(query),
        function(i) .get_query_overlaps(query[i], subject, min.score, type))
    hits <- .make_Hits_from_q2s(hits_per_query, length(subject))
    selectHits(hits, select=select)
}

test_NCList <- function()
{
    x <- IRanges(rep.int(1:5, 5:1), c(1:5, 2:5, 3:5, 4:5, 5),
                 names=LETTERS[1:15])
    mcols(x) <- DataFrame(score=seq(0.7, by=0.045, length.out=15))
    nclist <- NCList(x)

    checkTrue(is(nclist, "NCList"))
    checkTrue(validObject(nclist, complete=TRUE))
    checkIdentical(x, ranges(nclist, use.mcols=TRUE))
    checkIdentical(length(x), length(nclist))
    checkIdentical(names(x), names(nclist))
    checkIdentical(start(x), start(nclist))
    checkIdentical(end(x), end(nclist))
    checkIdentical(width(x), width(nclist))
    checkIdentical(x, as(nclist, "IRanges"))
    checkIdentical(x[-6], as(nclist[-6], "IRanges"))
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
                ## query - subject
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
                ## subject - query
                target <- .findOverlaps_naive(subject, query,
                                              min.score=min.score,
                                              type=type, select=select)
                current <- findOverlaps_NCList(pp_subject, query,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_NCList(subject, pp_query,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_NCList(subject, query,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
                ## subject - subject
                target <- .findOverlaps_naive(subject, subject,
                                              min.score=min.score,
                                              type=type, select=select)
                current <- findOverlaps_NCList(pp_subject, subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_NCList(subject, pp_subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
                current <- findOverlaps_NCList(subject, subject,
                                               min.score=min.score,
                                               type=type, select=select)
                checkTrue(.compare_hits(target, current))
            }
        }
    }
}

.test_arbitrary_selection <- function(query, subject)
{
    pp_query <- NCList(query)
    pp_subject <- NCList(subject)
    for (min.score in -3:4) {
        for (type in c("any", "start", "end", "within", "extend", "equal")) {
            target <- as(.findOverlaps_naive(query, subject,
                                             min.score=min.score,
                                             type=type, select="all"),
                         "CompressedIntegerList")
            target_idx0 <- elementLengths(target) == 0L
            check_arbitrary_hits <- function(current) {
                current_idx0 <- is.na(current)
                checkIdentical(target_idx0, current_idx0)
                current <- as(current, "CompressedIntegerList")
                checkTrue(all(current_idx0 | as.logical(current %in% target)))
            }
            current <- findOverlaps_NCList(query, pp_subject,
                                           min.score=min.score,
                                           type=type, select="arbitrary")
            check_arbitrary_hits(current)
            current <- findOverlaps_NCList(pp_query, subject,
                                           min.score=min.score,
                                           type=type, select="arbitrary")
            check_arbitrary_hits(current)
            current <- findOverlaps_NCList(query, subject,
                                           min.score=min.score,
                                           type=type, select="arbitrary")
            check_arbitrary_hits(current)
        }
    }
}

test_findOverlaps_NCList_arbitrary <- function()
{
    query <- IRanges(4:3, 6)
    subject <- IRanges(2:4, 10)
    .test_arbitrary_selection(query, subject)
    query <- IRanges(-3:7, width=3)
    subject <- IRanges(rep.int(1:5, 5:1), c(1:5, 2:5, 3:5, 4:5, 5))
    .test_arbitrary_selection(query, subject)
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

test_NCLists <- function()
{
    x1 <- IRanges(-3:7, width=3)
    x2 <- IRanges()
    x3 <- IRanges(rep.int(1:5, 5:1), c(1:5, 2:5, 3:5, 4:5, 5))
    x <- IRangesList(x1=x1, x2=x2, x3=x3)
    mcols(x) <- DataFrame(label=c("first", "second", "third"))
    nclists <- NCLists(x)

    checkTrue(is(nclists, "NCLists"))
    checkTrue(validObject(nclists, complete=TRUE))
    checkIdentical(x, ranges(nclists, use.mcols=TRUE))
    checkIdentical(length(x), length(nclists))
    checkIdentical(names(x), names(nclists))
    checkIdentical(start(x), start(nclists))
    checkIdentical(end(x), end(nclists))
    checkIdentical(width(x), width(nclists))
    checkIdentical(x, as(nclists, "IRangesList"))
    checkIdentical(x[-1], as(nclists[-1], "IRangesList"))
    checkIdentical(elementLengths(x), elementLengths(nclists))

    nclist <- nclists[[3]]
    checkTrue(is(nclist, "NCList"))
    checkTrue(validObject(nclist, complete=TRUE))
    checkIdentical(x3, as(nclist, "IRanges"))
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

