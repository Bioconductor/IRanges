test_Rle_construction <- function() {
    empty <- Rle()
    checkTrue(validObject(empty))
    checkIdentical(Rle(), new("Rle"))
    checkIdentical(length(empty), 0L)
    x <- Rle(rep(6:10, 1:5))
    checkTrue(validObject(x))
    checkIdentical(x, Rle(6:10, 1:5))
    y <- Rle(factor(rep(letters, 1:26)))
    checkTrue(validObject(y))
    checkIdentical(y, Rle(factor(letters), 1:26))

    checkIdentical(Rle(c(TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)),
                   Rle(c(TRUE, FALSE, NA), c(2, 3, 3)))
    checkIdentical(Rle(c(1L, 1L, 1L, 2L, 2L, NA, NA, NA)),
                   Rle(c(1L, 2L, NA), c(3, 2, 3)))
    checkIdentical(Rle(c(1, 1, 1, 2, 2, NA, NA, NA)),
                   Rle(c(1, 2, NA), c(3, 2, 3)))
    checkIdentical(Rle(c("a", "a", "b", "b", "b", NA, NA, NA)),
                   Rle(c("a", "b", NA), c(2, 3, 3)))
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
    checkIdentical(as.factor(x), as.factor(xRle))
    checkIdentical(y, as.vector(yRle))
    checkIdentical(as.logical(y), as.logical(yRle))
    checkIdentical(as.character(y), as.character(yRle))
    checkIdentical(as.raw(y), as.raw(yRle))
    checkIdentical(as.factor(y), as.factor(yRle))
}

test_Rle_groupGeneric <- function() {
    set.seed(0)
    x <- sample(0:3, 50, replace = TRUE)
    xRle <- Rle(x)
    checkIdentical(numeric(0) + 1, as.vector(Rle(numeric(0)) + 1))
    checkIdentical(x + 1, as.vector(xRle + 1))
    checkIdentical(2 * x + 3, as.vector(2 * xRle + 3))    
    checkIdentical(x[(x > 0) & (x < 3)], as.vector(xRle[(xRle > 0) & (xRle < 3)]))
    checkIdentical(log(x), as.vector(log(xRle)))
    checkIdentical(range(x), range(xRle))
    checkIdentical(sum(x), sum(xRle))
    checkIdentical(prod(x), prod(xRle))
    checkIdentical(cumsum(x), as.vector(cumsum(xRle)))
    checkIdentical(cumprod(x), as.vector(cumprod(xRle)))
    checkIdentical(round(x + .25), as.vector(round(xRle + .25)))
    checkIdentical(signif(x + .25), as.vector(signif(xRle + .25)))
    checkIdentical(Im(x + 5i), as.vector(Im(xRle + 5i)))
}

test_Rle_general <- function() {
    x <- rep(6:10, 1:5)
    xRle <- Rle(x)
    checkIdentical(unique(x), unique(xRle))
    checkIdentical(x[c(3,2,4,6)], as.vector(xRle[c(3,2,4,6)]))
    checkIdentical(aggregate(xRle, IRanges(start = 3:6, end = 13:10), FUN = mean),
                   aggregate(xRle, FUN = mean, start = 3:6, width = seq(11, 5, by = -2)))
    exp <- c(mean(x[3:13]), mean(x[4:12]), mean(x[5:11]), mean(x[6:10]))
    agg <- aggregate(xRle, FUN = function(x) x, start = 3:6, end = 13:10)
    checkEquals(exp, sapply(agg, mean))
    checkEquals(exp, aggregate(xRle, FUN = mean, start = 3:6, end = 13:10))
    checkEquals(as.vector(aggregate.ts(ts(x, frequency = 5), FUN = mean)),
                aggregate(xRle, FUN = mean, start = c(1, 6, 11), end = c(5, 10, 15)))
    checkIdentical(append(x,x), as.vector(append(xRle,xRle)))
    checkIdentical(append(x,x,3), as.vector(append(xRle,xRle,3)))
    checkIdentical(c(x,x) %in% c(7:9), as.vector(c(xRle,xRle)) %in% c(7:9))
    checkIdentical(c(x, x), as.vector(c(xRle, xRle)))
    checkIdentical(findRange(c(1, 3, 5), xRle), IRanges(start = c(1,2,4), width = 1:3))
    checkIdentical(head(x, 8), as.vector(head(xRle, 8)))
    checkIdentical(head(x, -3), as.vector(head(xRle, -3)))
    checkIdentical(is.na(c(NA, x, NA, NA, NA, x, NA)),
                   as.vector(is.na(c(Rle(NA), xRle, Rle(NA, 3), xRle, Rle(NA)))))
    checkIdentical(is.unsorted(c(1,2,2,3)), is.unsorted(Rle(c(1,2,2,3))))
    checkIdentical(is.unsorted(c(1,2,2,3), strictly = TRUE),
                   is.unsorted(Rle(c(1,2,2,3)), strictly = TRUE))
    checkIdentical(length(x), length(xRle))
    checkIdentical(match(c(x,x), c(7:9)), as.vector(match(c(xRle,xRle), c(7:9))))
    checkIdentical(rep(x, times = 2), as.vector(rep(xRle, times = 2)))
    checkIdentical(rep(x, times = x), as.vector(rep(xRle, times = x)))
    checkIdentical(rep(x, length.out = 20), as.vector(rep(xRle, length.out = 20)))
    checkIdentical(rep(x, each = 2), as.vector(rep(xRle, each = 2)))
    checkIdentical(rep(x, x, 20), as.vector(rep(xRle, x, 20)))
    checkException(rep(xRle, x, each = 2), silent = TRUE)
    checkIdentical(rep(x, 2, each = 2), as.vector(rep(xRle, 2, each = 2)))
    checkIdentical(rep(x, length.out = 20, each = 2),
                   as.vector(rep(xRle, length.out = 20, each = 2)))
    checkIdentical(rep(x, x, 20, 2), as.vector(rep(xRle, x, 20, 2)))
    checkIdentical(rep.int(x, times = 2), as.vector(rep.int(xRle, times = 2)))
    checkIdentical(rev(x), as.vector(rev(xRle)))
    checkIdentical(as.vector(seqselect(xRle, start = 1:3, width = 1:3)),
                   x[c(1,2,3,3,4,5)])
    checkIdentical(as.vector(seqselect(xRle, IRanges(start = 1:3, width = 1:3))),
                   x[c(1,2,3,3,4,5)])
    z <- x
    z[] <- rev(z)
    zRle <- xRle
    zRle[] <- rev(zRle)
    checkIdentical(z, as.vector(zRle))
    z <- x
    z[c(1,5,3)] <- 3:1
    zRle <- xRle
    zRle[c(1,5,3)] <- 3:1
    checkIdentical(z, as.vector(zRle))
    z <- x
    z[1:5] <- 0L
    zRle <- xRle
    seqselect(zRle, IRanges(start = 1:3, width = 1:3)) <- 0L
    checkIdentical(z, as.vector(zRle))
    checkIdentical(sort(c(x,x)), as.vector(sort(c(xRle,xRle))))

    checkException(split(Rle(1:26), integer()), silent = TRUE)
    checkException(split(Rle(1:26), Rle()), silent = TRUE)
    checkIdentical(lapply(as.list(split(Rle(1:26), letters)), as.vector),
                   split(1:26, letters))
    checkIdentical(lapply(as.list(split(Rle(1:26), Rle(letters))), as.vector),
                   split(1:26, letters))
    checkIdentical(lapply(as.list(split(Rle(1:26), letters[1:2])), as.vector),
                   split(1:26, letters[1:2]))
    checkIdentical(lapply(as.list(split(Rle(1:26), Rle(letters[1:2]))), as.vector),
                   split(1:26, letters[1:2]))
    checkIdentical(lapply(as.list(split(Rle(integer()), letters)), as.vector),
                   split(integer(), letters))
    checkIdentical(lapply(as.list(split(Rle(integer()), Rle(letters))), as.vector),
                   split(integer(), letters))

    checkIdentical(splitRanges(Rle(letters, 1:26)),
                   split(IRanges(end = cumsum(1:26), width = 1:26), letters))
    checkIdentical(as.vector(subset(xRle, rep(c(TRUE, FALSE), length.out = length(x)))),
                   subset(x, rep(c(TRUE, FALSE), length.out = length(x))))
    checkIdentical(summary(x), summary(xRle))
    checkIdentical(table(as.vector(x)), table(xRle))
    checkIdentical(tail(x, 8), as.vector(tail(xRle, 8)))
    checkIdentical(tail(x, -3), as.vector(tail(xRle, -3)))
    checkException(tapply(xRle), silent = TRUE)
    checkIdentical(tapply(x, x), tapply(xRle, xRle))
    checkIdentical(tapply(x, x, mean), tapply(xRle, xRle, mean))
    checkIdentical(tapply(xRle, x, mean), tapply(xRle, xRle, mean))
    checkIdentical(tapply(x, x, mean, simplify = FALSE),
                   tapply(xRle, xRle, mean, simplify = FALSE))
    checkIdentical(tapply(xRle, x, mean, simplify = FALSE),
                   tapply(xRle, xRle, mean, simplify = FALSE))
    checkIdentical(as.vector(window(x, start = 3, end = 13)),
                   as.vector(window(xRle, start = 3, end = 13)))
    checkIdentical(as.vector(window(x, start = 3, end = 13, frequency = 1/2)),
                   as.vector(window(xRle, start = 3, end = 13, frequency = 1/2)))
    checkIdentical(as.vector(window(x, start = 3, end = 13, delta = 3)),
                   as.vector(window(xRle, start = 3, end = 13, delta = 3)))
    z <- x
    z[3:13] <- 0L
    zRle <- xRle
    window(zRle, start = 3, end = 13) <- 0L
    checkIdentical(z, as.vector(zRle))
}

test_Rle_logical <- function() {
    checkIdentical(logical(), as.vector(Rle(logical())))

    x <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
    xRle <- Rle(x)
    checkIdentical(!x, as.vector(!x))
    checkIdentical(which(x), as.vector(which(x)))
    checkIdentical(as(xRle, "IRanges"),
                   IRanges(start = c(1,5,7), width = c(2, 1, 3)))
}

test_Rle_numerical <- function() {
    checkIdentical(numeric(), as.vector(Rle(numeric())))

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

    x <- rep(c(1.2, 3.4, NA, 7.8, 9.0), 1:5)
    y <- x - rev(x)
    xRle <- Rle(x)
    yRle <- Rle(y)
    checkIdentical(mean(x), mean(xRle))
    checkIdentical(mean(x, na.rm = TRUE), mean(xRle, na.rm = TRUE))
    checkIdentical(var(x), var(xRle))
    checkEqualsNumeric(var(x, na.rm = TRUE), var(xRle, na.rm = TRUE))
    checkIdentical(var(x, y), var(xRle, yRle))
    checkEqualsNumeric(var(x, y, na.rm = TRUE), var(xRle, yRle, na.rm = TRUE))
    checkIdentical(cov(x, y), cov(xRle, yRle))
    checkEqualsNumeric(cov(x, y, use = "complete"), cov(xRle, yRle, use = "complete"))
    checkIdentical(cor(x, y), cor(xRle, yRle))
    checkEqualsNumeric(cor(x, y, use = "complete"), cor(xRle, yRle, use = "complete"))
    checkIdentical(sd(x), sd(xRle))
    checkEqualsNumeric(sd(x, na.rm = TRUE), sd(xRle, na.rm = TRUE))
    checkIdentical(median(x), median(xRle))
    checkIdentical(median(x, na.rm = TRUE), median(xRle, na.rm = TRUE))
    checkIdentical(quantile(x, na.rm = TRUE), quantile(xRle, na.rm = TRUE))
    checkIdentical(mad(x), mad(xRle))
    checkIdentical(mad(x, na.rm = TRUE), mad(xRle, na.rm = TRUE))
    checkIdentical(IQR(x, na.rm = TRUE), IQR(xRle, na.rm = TRUE))

    y <- (-20:20)^2
    y[c(1,10,21,41)] <- c(100L, 30L, 400L, 470L)
    checkEqualsNumeric(smoothEnds(y), as.vector(smoothEnds(Rle(y))))
    checkEqualsNumeric(runmed(y, 7), as.vector(runmed(Rle(y), 7)))
    checkEqualsNumeric(runmed(y, 11), as.vector(runmed(Rle(y), 11)))
    checkEqualsNumeric(runmed(y, 7, "keep"),
                       as.vector(runmed(Rle(y), 7, "keep")))
    checkEqualsNumeric(runmed(y, 11, "keep"),
                       as.vector(runmed(Rle(y), 11, "keep")))
    checkEqualsNumeric(runmed(y, 7, "constant"),
                       as.vector(runmed(Rle(y), 7, "constant")))
    checkEqualsNumeric(runmed(y, 11, "constant"),
                       as.vector(runmed(Rle(y), 11, "constant")))

    x <- rep(c(1.2, 3.4, 5.6, 7.8, 9.0), 1:5)
    y <- rep(1:5, c(4, 2, 5, 1, 3))
    xRle <- Rle(x)
    yRle <- Rle(y)
    checkEqualsNumeric(sapply(1:13, function(i) sum(window(x, i, i + 2))),
                       as.numeric(runsum(xRle, k = 3)))
#    checkEqualsNumeric(sapply(1:13, function(i) sum(window(rev(x), i, i + 2))),
#                       as.numeric(runsum(rev(xRle), k = 3)))
    checkEqualsNumeric(sapply(1:13, function(i) sum(window(y, i, i + 2))),
                       as.integer(runsum(yRle, k = 3)))
    checkEqualsNumeric(sapply(1:13, function(i) sum(window(rev(y), i, i + 2))),
                       as.integer(runsum(rev(yRle), k = 3)))
    checkEqualsNumeric(sapply(1:13, function(i) mean(window(x, i, i + 2))),
                       as.numeric(runmean(xRle, k = 3)))
    checkEqualsNumeric(sapply(1:13, function(i) mean(window(rev(x), i, i + 2))),
                       as.numeric(runmean(rev(xRle), k = 3)))
    checkEqualsNumeric(sapply(1:13, function(i) mean(window(y, i, i + 2))),
                       as.numeric(runmean(yRle, k = 3)))
    checkEqualsNumeric(sapply(1:13, function(i) mean(window(rev(y), i, i + 2))),
                       as.numeric(runmean(rev(yRle), k = 3)))
    checkEqualsNumeric(sapply(1:13, function(i) sum(window(x, i, i + 2))),
                       as.numeric(runwtsum(xRle, k = 3, wt = rep(1,3))))
    checkEqualsNumeric(sapply(1:13, function(i) sum(window(x, i, i + 2)/3)),
                       as.numeric(runwtsum(xRle, k = 3, wt = rep(1/3,3))))
    checkEqualsNumeric(sapply(1:13, function(i) sum(window(y, i, i + 2))),
                       as.numeric(runwtsum(yRle, k = 3, wt = rep(1,3))))
    checkEqualsNumeric(sapply(1:13, function(i) sum(window(y, i, i + 2)/3)),
                       as.numeric(runwtsum(yRle, k = 3, wt = rep(1/3,3))))
    checkEqualsNumeric(sapply(1:13, function(i) min(window(x, i, i + 2))),
                       as.numeric(runq(xRle, k = 3, i = 1)))
    checkEqualsNumeric(sapply(1:13, function(i) median(window(x, i, i + 2))),
                       as.numeric(runq(xRle, k = 3, i = 2)))
    checkEqualsNumeric(sapply(1:13, function(i) max(window(x, i, i + 2))),
                       as.numeric(runq(xRle, k = 3, i = 3)))
    checkIdentical(runq(xRle, k = 3, i = 2),
                   rev(runq(rev(xRle), k = 3, i = 2)))
    checkEqualsNumeric(sapply(1:13, function(i) min(window(y, i, i + 2))),
                       as.numeric(runq(yRle, k = 3, i = 1)))
    checkEqualsNumeric(sapply(1:13, function(i) median(window(y, i, i + 2))),
                       as.numeric(runq(yRle, k = 3, i = 2)))
    checkEqualsNumeric(sapply(1:13, function(i) max(window(y, i, i + 2))),
                       as.numeric(runq(yRle, k = 3, i = 3)))
    checkIdentical(runq(yRle, k = 3, i = 2),
                   rev(runq(rev(yRle), k = 3, i = 2)))
}

test_Rle_character <- function() {
    checkIdentical(character(), as.vector(Rle(character())))

    txt <-
      c("The", "licenses", "for", "most", "software", "are", "designed",
        "to", "take", "away", "your", "freedom", "to", "share", "and",
        "change", "it.", "", "By", "contrast,", "the", "GNU", "General",
        "Public", "License", "is", "intended", "to", "guarantee", "your",
        "freedom", "to", "share", "and", "change", "free", "software",
        "--", "to", "make", "sure", "the", "software", "is", "free", "for",
        "all", "its", "users")
     txt <- rep(txt, seq_len(length(txt)))
     txtRle <- Rle(txt)
     checkIdentical(nchar(txt), as.vector(nchar(txtRle)))
     checkIdentical(substr(txt, 3, 7), as.vector(substr(txtRle, 3, 7)))
     checkIdentical(substring(txt, 4, 9), as.vector(substring(txtRle, 4, 9)))
     checkIdentical(chartr("@!*", "alo", txt),
                    as.vector(chartr("@!*", "alo", txtRle)))
     checkIdentical(tolower(txt), as.vector(tolower(txtRle)))
     checkIdentical(toupper(txt), as.vector(toupper(txtRle)))
     checkIdentical(sub("[b-e]",".", txt), as.vector(sub("[b-e]",".", txtRle)))
     checkIdentical(gsub("[b-e]",".", txt), as.vector(gsub("[b-e]",".", txtRle)))
     checkIdentical(paste(txt, rev(txt), sep = "|"),
                    as.vector(paste(txtRle, rev(txtRle), sep = "|")))

     modifyFactor <- function(x, FUN, ...) {
         levels(x) <- FUN(levels(x), ...)
         x
     }
     fac <- factor(txt)
     facRle <- Rle(fac)
     checkIdentical(nchar(fac), as.vector(nchar(facRle)))
     checkIdentical(modifyFactor(fac, substr, 3, 7),
                    as.factor(substr(facRle, 3, 7)))
     checkIdentical(modifyFactor(fac, substring, 4, 9),
                    as.factor(substring(facRle, 4, 9)))
     checkIdentical(modifyFactor(fac, chartr, old = "@!*", new = "alo"),
                    as.factor(chartr("@!*", "alo", facRle)))
     checkIdentical(modifyFactor(fac, tolower), as.factor(tolower(facRle)))
     checkIdentical(modifyFactor(fac, toupper), as.factor(toupper(facRle)))
     checkIdentical(modifyFactor(fac, sub, pattern = "[b-e]",
                                 replacement = "."),
                    as.factor(sub("[b-e]",".", facRle)))
     checkIdentical(modifyFactor(fac, gsub, pattern = "[b-e]",
                                 replacement = "."),
                    as.factor(gsub("[b-e]",".", facRle)))
     checkTrue(is.factor(runValue(paste(facRle, rev(facRle), sep = "|"))))
}

test_Rle_factor <- function() {
    checkIdentical(factor(character()),
                   as.factor(Rle(factor(character()))))

    x <- factor(rep(letters, 1:26))
    xRle <- Rle(x)
    checkIdentical(levels(x), levels(xRle))
    levels(x) <- LETTERS
    levels(xRle) <- LETTERS
    checkIdentical(levels(x), levels(xRle))
    checkIdentical(nlevels(x), 26L)
    xRle[] <- xRle
    checkIdentical(Rle(x), xRle)
    checkIdentical(x, xRle[TRUE,drop=TRUE])
}
