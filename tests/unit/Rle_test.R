test_Rle_construction <- function() {
    empty <- Rle(integer(0))
    checkTrue(validObject(empty))
    checkIdentical(length(empty), 0L)
    x <- Rle(rep(6:10, 1:5))
    checkTrue(validObject(x))
    checkIdentical(x, Rle(6:10, 1:5))
}

test_Rle_replace <- function() {
    x <- Rle(1:26, 1:26)
    runValue(x) <- letters
    checkTrue(validObject(x))
    checkIdentical(x, Rle(letters, 1:26))
    runLength(x) <- 26:1
    checkTrue(validObject(x))
    checkIdentical(x, Rle(letters, 26:1))
}

test_Rle_coersion <- function() {
    x <- rep(6:10, 1:5)
    xRle <- Rle(x)
    y <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
    yRle <- Rle(y)
    checkIdentical(x, as.vector(xRle))
    checkIdentical(as.integer(x), as.integer(xRle))
    checkIdentical(as.numeric(x), as.numeric(xRle))
    checkIdentical(as.complex(x), as.complex(xRle))
    checkIdentical(y, as.vector(yRle))
    checkIdentical(as.logical(y), as.logical(yRle))
    checkIdentical(as.character(y), as.character(yRle))
    checkIdentical(as.raw(y), as.raw(yRle))
}

test_Rle_general <- function() {
    x <- rep(6:10, 1:5)
    xRle <- Rle(x)
    checkIdentical(length(x), length(xRle))
    checkIdentical(c(x, x), as.vector(c(xRle, xRle)))
    checkIdentical(x[c(3,2,4,6)], as.vector(xRle[c(3,2,4,6)]))
    checkIdentical(x[9:15], as.vector(subseq(xRle,9,15)))
    checkIdentical(x[14:15], as.vector(subseq(xRle,14,15)))
    checkIdentical(rev(x), as.vector(rev(xRle)))
    checkIdentical(sort(c(x,x)), as.vector(sort(c(xRle,xRle))))
    checkIdentical(rep(x, times = 2), as.vector(rep(xRle, times = 2)))
    checkIdentical(rep(x, each = 2), as.vector(rep(xRle, each = 2)))
    checkIdentical(rep(x, length.out = 20), as.vector(rep(xRle, length.out = 20)))
    checkIdentical(table(as.vector(x)), table(xRle))
    checkIdentical(c(x,x) %in% c(7:9), as.vector(c(xRle,xRle)) %in% c(7:9))
}

test_Rle_groupGeneric <- function() {
    set.seed(0)
    x <- sample(0:3, 50, replace = TRUE)
    xRle <- Rle(x)
    checkIdentical(x + 1, as.vector(xRle + 1))
    checkIdentical(2 * x + 3, as.vector(2 * xRle + 3))    
    checkIdentical(x[(x > 0) & (x < 3)], as.vector(xRle[(xRle > 0) & (xRle < 3)]))
    checkIdentical(log(x), as.vector(log(xRle)))
    checkIdentical(range(x), range(xRle))
    checkIdentical(sum(x), sum(xRle))
    checkIdentical(prod(x), prod(xRle))
}

test_Rle_logical <- function() {
    x <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
    xRle <- Rle(x)
    checkIdentical(!x, as.vector(!x))
}

test_Rle_numerical <- function() {
    x <- cumsum(cumsum(1:10))
    xRle <- Rle(x)
    checkIdentical(pmax(x, rev(x)), as.vector(pmax(xRle, rev(xRle))))
    checkIdentical(pmin(x, rev(x)), as.vector(pmin(xRle, rev(xRle))))
    checkIdentical(pmax.int(x, rev(x)), as.vector(pmax.int(xRle, rev(xRle))))
    checkIdentical(pmin.int(x, rev(x)), as.vector(pmin.int(xRle, rev(xRle))))
    checkIdentical(diff(x), as.vector(diff(xRle)))
    checkIdentical(diff(x, lag = 2), as.vector(diff(xRle, lag = 2)))
    checkIdentical(diff(x, differences = 2), as.vector(diff(xRle, differences = 2)))
    checkIdentical(diff(x, lag = 2, differences = 2), 
                   as.vector(diff(xRle, lag = 2, differences = 2)))

    x <- rep(c(1, 3, NA, 7, 9), 1:5)
    xRle <- Rle(x)
    checkIdentical(mean(x), mean(xRle))
    checkIdentical(mean(x, na.rm = TRUE), mean(xRle, na.rm = TRUE))
    checkIdentical(median(x), median(xRle))
    checkIdentical(median(x, na.rm = TRUE), median(xRle, na.rm = TRUE))
    checkIdentical(var(x), median(xRle))
    checkIdentical(var(x, na.rm = TRUE), var(xRle, na.rm = TRUE))
    checkIdentical(sd(x), sd(xRle))
    checkIdentical(sd(x, na.rm = TRUE), sd(xRle, na.rm = TRUE))
}

test_Rle_character <- function() {
    txt <- c("The", "licenses", "for", "most", "software", "are",
             "designed", "to", "take", "away", "your", "freedom",
             "to", "share", "and", "change", "it.",
             "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
             "is", "intended", "to", "guarantee", "your", "freedom", "to",
             "share", "and", "change", "free", "software", "--",
             "to", "make", "sure", "the", "software", "is",
             "free", "for", "all", "its", "users")
     txt <- rep(txt, seq_len(length(txt)))
     txtRle <- Rle(txt)
     checkIdentical(nchar(txt), as.vector(nchar(txt)))
     checkIdentical(substr(txt, 3, 7), as.vector(substr(txt, 3, 7)))
     checkIdentical(substring(txt, 4, 9), as.vector(substring(txt, 4, 9)))
     checkIdentical(chartr("@!*", "alo", txt), as.vector(chartr("@!*", "alo", txt)))
     checkIdentical(tolower(txt), as.vector(tolower(txt)))
     checkIdentical(toupper(txt), as.vector(toupper(txt)))
     checkIdentical(gsub("[b-e]",".", txt), as.vector(gsub("[b-e]",".", txtRle)))
}
