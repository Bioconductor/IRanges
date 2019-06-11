test_IPos_constructor_and_getters <- function()
{
    ## Empty object

    checkException(new("IPos"))

    ipos0a <- new("UnstitchedIPos")
    checkTrue(validObject(ipos0a))
    checkIdentical(0L, length(ipos0a))
    checkIdentical(integer(0), pos(ipos0a))
    checkIdentical(integer(0), start(ipos0a))
    checkIdentical(integer(0), end(ipos0a))
    checkIdentical(integer(0), width(ipos0a))
    checkTrue(is.null(names(ipos0a)))

    ipos0b <- new("StitchedIPos")
    checkTrue(validObject(ipos0b))
    checkIdentical(0L, length(ipos0b))
    checkIdentical(integer(0), pos(ipos0b))
    checkIdentical(integer(0), start(ipos0b))
    checkIdentical(integer(0), end(ipos0b))
    checkIdentical(integer(0), width(ipos0b))
    checkTrue(is.null(names(ipos0b)))

    checkIdentical(ipos0a, IPos())
    checkIdentical(ipos0b, IPos(stitch=TRUE))
    checkIdentical(ipos0a, IPos(stitch=FALSE))

    ## Positions supplied in an unnamed integer vector

    pos <- c(44:53, 10:5, -3:6)  # unnamed
    score <- runif(26)

    ipos1a <- IPos(pos, names=LETTERS, score=score)
    checkTrue(is(ipos1a, "UnstitchedIPos"))
    checkTrue(validObject(ipos1a))
    checkIdentical(length(pos), length(ipos1a))
    checkIdentical(pos, pos(ipos1a))
    checkIdentical(pos, start(ipos1a))
    checkIdentical(pos, end(ipos1a))
    checkIdentical(rep.int(1L, length(pos)), width(ipos1a))
    checkIdentical(LETTERS, names(ipos1a))
    checkIdentical(DataFrame(score=score), mcols(ipos1a, use.names=FALSE))
    checkIdentical(LETTERS, rownames(mcols(ipos1a)))

    ipos1b <- IPos(pos, names=LETTERS, score=score, stitch=TRUE)
    checkTrue(is(ipos1b, "StitchedIPos"))
    checkTrue(validObject(ipos1b))
    checkIdentical(length(pos), length(ipos1b))
    checkIdentical(pos, pos(ipos1b))
    checkIdentical(pos, start(ipos1b))
    checkIdentical(pos, end(ipos1b))
    checkIdentical(rep.int(1L, length(pos)), width(ipos1b))
    checkIdentical(LETTERS, names(ipos1b))
    checkIdentical(DataFrame(score=score), mcols(ipos1b, use.names=FALSE))
    checkIdentical(LETTERS, rownames(mcols(ipos1b)))

    ## Positions supplied in a named integer vector

    ipos2a <- IPos(setNames(pos, LETTERS), score=score)
    checkIdentical(ipos1a, ipos2a)

    ipos2b <- IPos(setNames(pos, LETTERS), score=score, stitch=TRUE)
    checkIdentical(ipos1b, ipos2b)

    ## Invalid positions

    checkException(IPos(c(35, NA, 5)))

    ## Positions specified as integer ranges

    ipos3 <- IPos(IRanges(c(25, 2), c(100, 50)))
    checkTrue(is(ipos3, "StitchedIPos"))
    checkTrue(validObject(ipos3))
    checkIdentical(125L, length(ipos3))
    checkIdentical(c(25:100, 2:50), pos(ipos3))

    checkIdentical(ipos3, IPos(c("25-100", "2-50")))
}

test_IPos_names_setter <- function()
{
    ipos0a <- IPos(stitch=FALSE)
    ipos0 <- `names<-`(ipos0a, names(ipos0a))  # no-op
    checkIdentical(ipos0a, ipos0)
    names(ipos0) <- character(0)
    checkTrue(validObject(ipos0))
    checkIdentical(character(0), names(ipos0))
    checkIdentical(ipos0a, unname(ipos0))

    ipos0b <- IPos(stitch=TRUE)
    ipos0 <- `names<-`(ipos0b, names(ipos0b))  # no-op
    checkIdentical(ipos0b, ipos0)
    names(ipos0) <- character(0)
    checkTrue(validObject(ipos0))
    checkIdentical(character(0), names(ipos0))
    checkIdentical(ipos0b, unname(ipos0))

    pos <- c(44:53, 10:5, -3:6)  # unnamed

    ipos1a <- IPos(pos)
    checkTrue(is.null(names(ipos1a)))
    checkIdentical(ipos1a, `names<-`(ipos1a, names(ipos1a)))  # no-op

    checkException(names(ipos1a) <- c(letters, LETTERS))
    names(ipos1a) <- LETTERS[26:22]
    checkIdentical(LETTERS[26:22], head(names(ipos1a), n=5))
    checkIdentical(rep.int(NA_character_, 21), tail(names(ipos1a), n=21))
    checkIdentical(ipos1a, `names<-`(ipos1a, names(ipos1a)))  # no-op
    checkIdentical(IPos(pos), unname(ipos1a))

    ipos1b <- IPos(pos, stitch=TRUE)
    checkTrue(is.null(names(ipos1b)))
    checkIdentical(ipos1b, `names<-`(ipos1b, names(ipos1b)))  # no-op

    checkException(names(ipos1b) <- c(letters, LETTERS))
    names(ipos1b) <- LETTERS[26:22]
    checkIdentical(LETTERS[26:22], head(names(ipos1b), n=5))
    checkIdentical(rep.int(NA_character_, 21), tail(names(ipos1b), n=21))
    checkIdentical(ipos1b, `names<-`(ipos1b, names(ipos1b)))  # no-op
    checkIdentical(IPos(pos, stitch=TRUE), unname(ipos1b))
}

test_IPos_mcols_setter <- function()
{
    ipos0a <- IPos(names=character(0), stitch=FALSE)
    ipos0 <- `mcols<-`(ipos0a, value=mcols(ipos0a))  # no-op
    checkIdentical(ipos0a, ipos0)
    mcols(ipos0)$score <- numeric(0)
    checkTrue(validObject(ipos0))
    checkTrue(is(mcols(ipos0), "DataFrame"))
    checkIdentical(c(0L, 1L), dim(mcols(ipos0)))
    checkIdentical(list(character(0), "score"), dimnames(mcols(ipos0)))
    checkIdentical(list(NULL, "score"), dimnames(mcols(ipos0, use.names=FALSE)))
    checkIdentical(ipos0a, `mcols<-`(ipos0, value=NULL))

    ipos0b <- IPos(names=character(0), stitch=TRUE)
    ipos0 <- `mcols<-`(ipos0b, value=mcols(ipos0b))  # no-op
    checkIdentical(ipos0b, ipos0)
    mcols(ipos0)$score <- numeric(0)
    checkTrue(validObject(ipos0))
    checkTrue(is(mcols(ipos0), "DataFrame"))
    checkIdentical(c(0L, 1L), dim(mcols(ipos0)))
    checkIdentical(list(character(0), "score"), dimnames(mcols(ipos0)))
    checkIdentical(list(NULL, "score"), dimnames(mcols(ipos0, use.names=FALSE)))
    checkIdentical(ipos0b, `mcols<-`(ipos0, value=NULL))

    pos <- c(44:53, 10:5, -3:6)  # unnamed

    ipos1a <- IPos(pos, names=LETTERS, stitch=FALSE)
    checkIdentical(ipos1a, `mcols<-`(ipos1a, value=mcols(ipos1a)))  # no-op
    mcols(ipos1a)$stuff <- 1:2
    mcols(ipos1a)$gene_id <- sprintf("ID%02d", 1:26)
    checkTrue(validObject(ipos1a))
    checkTrue(is(mcols(ipos1a), "DataFrame"))
    checkIdentical(c(26L, 2L), dim(mcols(ipos1a)))
    checkIdentical(c("stuff", "gene_id"), colnames(mcols(ipos1a)))
    checkIdentical(LETTERS, rownames(mcols(ipos1a)))
    checkIdentical(NULL, rownames(mcols(ipos1a, use.names=FALSE)))
    checkIdentical(rep.int(1:2, 13), mcols(ipos1a)$stuff)

    ipos1b <- IPos(pos, names=LETTERS, stitch=TRUE)
    checkIdentical(ipos1b, `mcols<-`(ipos1b, value=mcols(ipos1b)))  # no-op
    mcols(ipos1b)$stuff <- 1:2
    mcols(ipos1b)$gene_id <- sprintf("ID%02d", 1:26)
    checkTrue(validObject(ipos1b))
    checkTrue(is(mcols(ipos1b), "DataFrame"))
    checkIdentical(c(26L, 2L), dim(mcols(ipos1b)))
    checkIdentical(c("stuff", "gene_id"), colnames(mcols(ipos1b)))
    checkIdentical(LETTERS, rownames(mcols(ipos1b)))
    checkIdentical(NULL, rownames(mcols(ipos1b, use.names=FALSE)))
    checkIdentical(rep.int(1:2, 13), mcols(ipos1b)$stuff)
}

test_IPos_coercion <- function()
{
    pos <- c(44:53, 10:5, -3:6)
    ipos1a <- IPos(pos, LETTERS, stuff=1:2, stitch=FALSE)
    ipos1b <- IPos(pos, LETTERS, stuff=1:2, stitch=TRUE)

    ## Back and forth between UnstitchedIPos and StitchedIPos

    checkIdentical(ipos1b, as(ipos1a, "StitchedIPos"))
    checkIdentical(ipos1a, as(ipos1b, "UnstitchedIPos"))

    ## From IPos to IRanges

    ir1a <- as(ipos1a, "IRanges")
    ir1b <- as(ipos1b, "IRanges")
    checkIdentical(ir1a, ir1b)

    checkIdentical(pos, start(ir1a))
    checkIdentical(pos, end(ir1a))
    checkIdentical(names(ipos1a), names(ir1a))
    checkIdentical(mcols(ipos1a), mcols(ir1a))

    ## From IRanges to IPos

    checkIdentical(ipos1a, as(ir1a, "UnstitchedIPos"))
    checkIdentical(ipos1b, as(ir1a, "StitchedIPos"))
    checkIdentical(ipos1a, as(ir1a, "IPos"))

    checkException(as(IRanges(1:5, 5), "UnstitchedIPos"))
    checkException(as(IRanges(1:5, 5), "StitchedIPos"))
    checkException(as(IRanges(1:5, 5), "IPos"))
}

test_IPos_subsetting <- function()
{
    pos <- c(44:53, 10:5, -3:6)

    for (stitch in c(FALSE, TRUE)) {

        ## unnamed object
        ipos1 <- IPos(pos, stitch=stitch)

        ipos <- ipos1[12:5]
        checkIdentical(class(ipos1), class(ipos))
        checkTrue(validObject(ipos))
        checkIdentical(8L, length(ipos))
        checkIdentical(pos[12:5], pos(ipos))

        ipos <- ipos1[c(FALSE, TRUE)]
        checkIdentical(class(ipos1), class(ipos))
        checkTrue(validObject(ipos))
        checkIdentical(13L, length(ipos))
        checkIdentical(pos[c(FALSE, TRUE)], pos(ipos))

        ipos <- ipos1[-5]
        checkIdentical(class(ipos1), class(ipos))
        checkTrue(validObject(ipos))
        checkIdentical(25L, length(ipos))
        checkIdentical(pos[-5], pos(ipos))

        ipos <- tail(ipos1)
        checkIdentical(class(ipos1), class(ipos))
        checkTrue(validObject(ipos))
        checkIdentical(6L, length(ipos))
        checkIdentical(tail(pos), pos(ipos))

        ## named object
        names(ipos1) <- LETTERS

        ipos <- ipos1[12:5]
        checkIdentical(class(ipos1), class(ipos))
        checkTrue(validObject(ipos))
        checkIdentical(LETTERS[12:5], names(ipos))

        ## with metadata columns
        mcols(ipos1)$stuff <- 1:2
        mcols(ipos1)$ok <- c(TRUE, FALSE)

        ipos <- ipos1[12:5]
        checkIdentical(class(ipos1), class(ipos))
        checkTrue(validObject(ipos))
        checkIdentical(mcols(ipos1)[12:5, ], mcols(ipos))
    }
}

test_IPos_concatenation <- function()
{
    pos <- c(44:53, 10:5, -3:6)

    ## No medata columns

    ipos1 <- IPos(pos, names=LETTERS, stitch=FALSE)  # unstitched, named
    ipos2 <- IPos(c("-9-5", "41-55"))  # stitched, unnamed

    ipos12 <- c(ipos1, ipos2)
    checkTrue(is(ipos12, "UnstitchedIPos"))
    checkTrue(validObject(ipos12))
    checkIdentical(length(ipos1) + length(ipos2), length(ipos12))
    checkIdentical(c(pos(ipos1), pos(ipos2)), pos(ipos12))
    checkIdentical(c(names(ipos1), character(length(ipos2))), names(ipos12))

    ipos21 <- c(ipos2, ipos1)
    checkTrue(is(ipos21, "StitchedIPos"))
    checkTrue(validObject(ipos21))
    checkIdentical(length(ipos2) + length(ipos1), length(ipos21))
    checkIdentical(c(pos(ipos2), pos(ipos1)), pos(ipos21))
    checkIdentical(c(character(length(ipos2)), names(ipos1)), names(ipos21))

    ## With medata columns on one object

    mcols(ipos1)$stuff <- 1:2
    mcols(ipos1)$ok <- c(TRUE, FALSE)

    checkIdentical(ipos12, c(ipos1, ipos2, ignore.mcols=TRUE))
    ipos12 <- c(ipos1, ipos2)
    mcols12 <- mcols(ipos12)
    checkTrue(is(mcols12, "DataFrame"))
    checkIdentical(c(length(ipos12), 2L), dim(mcols12))
    checkIdentical(c("stuff", "ok"), colnames(mcols12))
    checkTrue(is.integer(mcols12$stuff))
    checkIdentical(mcols(ipos1)$stuff, head(mcols12$stuff, n=length(ipos1)))
    checkIdentical(rep.int(NA_integer_, length(ipos2)),
                   tail(mcols12$stuff, n=length(ipos2)))
    checkTrue(is.logical(mcols12$ok))
    checkIdentical(mcols(ipos1)$ok, head(mcols12$ok, n=length(ipos1)))
    checkIdentical(rep.int(NA, length(ipos2)),
                   tail(mcols12$ok, n=length(ipos2)))

    checkIdentical(ipos21, c(ipos2, ipos1, ignore.mcols=TRUE))
    ipos21 <- c(ipos2, ipos1)
    mcols21 <- mcols(ipos21)
    checkTrue(is(mcols21, "DataFrame"))
    checkIdentical(c(length(ipos21), 2L), dim(mcols21))
    checkIdentical(c("stuff", "ok"), colnames(mcols21))
    checkTrue(is.integer(mcols21$stuff))
    checkIdentical(rep.int(NA_integer_, length(ipos2)),
                   head(mcols21$stuff, n=length(ipos2)))
    checkIdentical(mcols(ipos1)$stuff, tail(mcols21$stuff, n=length(ipos1)))
    checkTrue(is.logical(mcols21$ok))
    checkIdentical(rep.int(NA, length(ipos2)),
                   head(mcols21$ok, n=length(ipos2)))
    checkIdentical(mcols(ipos1)$ok, tail(mcols21$ok, n=length(ipos1)))

    ## With medata columns on the two objects

    mcols(ipos2)$ok <- "yes"
    mcols(ipos2)$more_stuff <- Rle(1:5, 6)

    ipos12 <- c(ipos1, ipos2)
    mcols12 <- mcols(ipos12)
    checkTrue(is(mcols12, "DataFrame"))
    checkIdentical(c(length(ipos12), 3L), dim(mcols12))
    checkIdentical(c("stuff", "ok", "more_stuff"), colnames(mcols12))
    checkTrue(is.integer(mcols12$stuff))
    checkTrue(is.character(mcols12$ok))

    ipos21 <- c(ipos2, ipos1)
    mcols21 <- mcols(ipos21)
    checkTrue(is(mcols21, "DataFrame"))
    checkIdentical(c(length(ipos21), 3L), dim(mcols21))
    checkIdentical(c("ok", "more_stuff", "stuff"), colnames(mcols21))
    checkTrue(is.character(mcols21$ok))
    checkTrue(is.integer(mcols21$stuff))
}

