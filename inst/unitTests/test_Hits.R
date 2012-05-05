test_Hits_as_matrix <- function() {
  ## sparse
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.matrix(result),
                 cbind(queryHits = c(1L, 1L, 3L),
                       subjectHits = 1:3))

  ## dense
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.matrix(result),
                 cbind(queryHits = rep(1:2, each=2),
                       subjectHits = rep(1:2, 2)))
}

test_Hits_matched <- function() {
  ## sparse
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2, 10), c(2, 3, 12))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.vector(as.table(result)), c(2L, 0L, 1L))
  checkIdentical(as.vector(as.table(t(result))), c(1L, 1L, 1L))

  ## dense
  query <- IRanges(c(1, 4, 9), c(5, 7, 10))
  subject <- IRanges(c(2, 2), c(5, 4))
  tree <- IntervalTree(subject)
  result <- findOverlaps(query, tree)

  checkIdentical(as.vector(as.table(result)), c(2L, 2L, 0L))
}

test_remapHits <- function()
{
    query_hits0 <- c(1L, 1L, 2L, 3L, 3L)
    subject_hits0 <- c(1L, 2L, 5L, 2L, 4L)
    hits0 <- new("Hits", queryHits=query_hits0,
                         subjectHits=subject_hits0,
                         queryLength=3L, subjectLength=6L)

    ## No remapping (i.e. map is missing or is the identity function).
    checkIdentical(remapHits(hits0), hits0)

    query.map1 <- seq_len(queryLength(hits0))
    new.queryLength1 <- queryLength(hits0)
    subject.map1 <- seq_len(subjectLength(hits0))
    new.subjectLength1 <- subjectLength(hits0)

    hits10 <- remapHits(hits0, query.map=query.map1,
                               new.queryLength=new.queryLength1)
    checkIdentical(hits10, hits0)

    hits01 <- remapHits(hits0, subject.map=subject.map1,
                               new.subjectLength=new.subjectLength1)
    checkIdentical(hits01, hits0)

    hits11 <- remapHits(hits0, query.map=query.map1,
                               new.queryLength=new.queryLength1,
                               subject.map=subject.map1,
                               new.subjectLength=new.subjectLength1)
    checkIdentical(hits11, hits0)

    ## With maps that add a fixed offset to the query hits, and a fixed offset
    ## to the subject hits.
    query.map2 <- query.map1 + 20L
    new.queryLength2 <- new.queryLength1 + 20L
    subject.map2 <- subject.map1 + 30L
    new.subjectLength2 <- new.subjectLength1 + 30L

    hits20 <- remapHits(hits0, query.map=query.map2,
                               new.queryLength=new.queryLength2)
    expected_hits20 <- new("Hits", queryHits=query_hits0 + 20L,
                                   subjectHits=subject_hits0,
                                   queryLength=23L, subjectLength=6L)
    checkIdentical(hits20, expected_hits20)

    hits02 <- remapHits(hits0, subject.map=subject.map2,
                               new.subjectLength=new.subjectLength2)
    expected_hits02 <- new("Hits", queryHits=query_hits0,
                                   subjectHits=subject_hits0 + 30L,
                                   queryLength=3L, subjectLength=36L)
    checkIdentical(hits02, expected_hits02)

    hits22 <- remapHits(hits0, query.map=query.map2,
                               new.queryLength=new.queryLength2,
                               subject.map=subject.map2,
                               new.subjectLength=new.subjectLength2)
    expected_hits22 <- new("Hits", queryHits=query_hits0 + 20L,
                                   subjectHits=subject_hits0 + 30L,
                                   queryLength=23L, subjectLength=36L)
    checkIdentical(hits22, expected_hits22)

    ## With injective and non-ascending maps.
    query.map3 <- 100L * rev(query.map1) + query.map1
    new.queryLength3 <- 400L
    subject.map3 <- 100L * rev(subject.map1) + subject.map1
    new.subjectLength3 <- 700L
    
    hits30 <- remapHits(hits0, query.map=query.map3,
                               new.queryLength=new.queryLength3)
    expected_hits30 <- new("Hits", queryHits=c(103L, 103L, 202L, 301L, 301L),
                                   subjectHits=c(2L, 4L, 5L, 1L, 2L),
                                   queryLength=400L, subjectLength=6L)
    checkIdentical(hits30, expected_hits30)

    hits03 <- remapHits(hits0, subject.map=subject.map3,
                               new.subjectLength=new.subjectLength3)
    expected_hits03 <- new("Hits", queryHits=query_hits0,
                                   subjectHits=c(502L, 601L, 205L, 304L, 502L),
                                   queryLength=3L, subjectLength=700L)
    checkIdentical(hits03, expected_hits03)

    hits33 <- remapHits(hits0, query.map=query.map3,
                               new.queryLength=new.queryLength3,
                               subject.map=subject.map3,
                               new.subjectLength=new.subjectLength3)
    expected_hits33 <- new("Hits", queryHits=c(103L, 103L, 202L, 301L, 301L),
                                   subjectHits=c(304L, 502L, 205L, 502L, 601L),
                                   queryLength=400L, subjectLength=700L)
    checkIdentical(hits33, expected_hits33)

    ## With non-injective maps (as factors).
    query.map4 <- factor(c("B", "A", "B"), levels=c("A", "B"))
    subject.map4 <- factor(c("a", "b", "a", "b", "a", "b"), levels=c("a", "b"))

    hits40 <- remapHits(hits0, query.map=query.map4)
    expected_hits40 <- new("Hits", queryHits=c(1L, 2L, 2L, 2L),
                                   subjectHits=c(5L, 1L, 2L, 4L),
                                   queryLength=2L, subjectLength=6L)
    checkIdentical(hits40, expected_hits40)

    hits04 <- remapHits(hits0, subject.map=subject.map4)
    expected_hits04 <- new("Hits", queryHits=c(1L, 1L, 2L, 3L),
                                   subjectHits=c(1L, 2L, 1L, 2L),
                                   queryLength=3L, subjectLength=2L)
    checkIdentical(hits04, expected_hits04)

    hits44 <- remapHits(hits0, query.map=query.map4, subject.map=subject.map4)
    expected_hits44 <- new("Hits", queryHits=c(1L, 2L, 2L),
                                   subjectHits=c(1L, 1L, 2L),
                                   queryLength=2L, subjectLength=2L)
    checkIdentical(hits44, expected_hits44)
}

