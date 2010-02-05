test_AtomicList_GroupGenerics <- function() {
    vec1 <- c(1L,2L,3L,5L,2L,8L)
    vec2 <- c(15L,45L,20L,1L,15L,100L,80L,5L)
    for (compress in c(TRUE, FALSE)) {
        for (type in c("IntegerList", "RleList")) {
            list1 <- do.call(type, list(one = vec1, vec2, compress = compress))
            checkIdentical(as.list(list1 + list1), Map("+", list1, list1))
            checkIdentical(as.list(log(list1)), lapply(list1, log))
            checkIdentical(as.list(round(sqrt(list1))),
                           lapply(list1, function(x) round(sqrt(x))))
            checkIdentical(sum(list1), sapply(list1, sum))
        }
    }
}

test_AtomicList_general <- function() {
    vec1 <- c(1L,2L,NA,3L,NA,5L,2L,8L)
    vec2 <- c(NA,15L,45L,20L,NA,1L,15L,100L,80L,5L,NA)
    for (compress in c(TRUE, FALSE)) {
        for (type in c("IntegerList", "RleList")) {
            list1 <- do.call(type, list(one = vec1, vec2, compress = compress))
            checkIdentical(as.list(list1 %in% c(1L, 5L)),
                           lapply(list1, "%in%", c(1L, 5L)))
            checkIdentical(lapply(list1 %in%
                                  IntegerList(one = vec1, vec2,
                                              compress = compress),
                                  as.vector),
                           mapply("%in%", lapply(list1, as.vector),
                                  list(one = vec1, vec2)))
            checkIdentical(as.list(is.na(list1)), lapply(list1, is.na))
            checkIdentical(as.list(match(list1, c(1L, 5L))),
                           lapply(list1, match, c(1L, 5L)))
            checkIdentical(lapply(match(list1,
                                        IntegerList(one = vec1, vec2,
                                                    compress = compress)),
                                  as.vector),
                           mapply(match, lapply(list1, as.vector),
                                  list(one = vec1, vec2)))
            checkIdentical(as.list(sort(list1)), lapply(list1, sort))
            checkIdentical(as.list(unique(list1)), lapply(list1, unique))
        }
    }
}

test_AtomicList_logical <- function() {
    vec1 <- c(TRUE,NA,FALSE, NA)
    vec2 <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE)
    for (compress in c(TRUE, FALSE)) {
        for (type in c("LogicalList", "RleList")) {
            list1 <- do.call(type, list(one = vec1, vec2, compress = compress))
            checkIdentical(as.list(!list1), lapply(list1, "!"))
            checkIdentical(as.list(which(list1)), lapply(list1, which))
        }
    }
}

test_AtomicList_numerical <- function() {
    vec1 <- c(1L,2L,NA,3L,NA,5L,2L,8L)
    vec2 <- c(NA,15L,45L,20L,NA,1L,15L,100L,80L,5L,NA)
    for (compress in c(TRUE, FALSE)) {
        for (type in c("IntegerList", "RleList")) {
            list1 <- do.call(type, list(one = vec1, vec2, compress = compress))
            list2 <- endoapply(list1, rev)
            checkIdentical(as.list(diff(list1)), lapply(list1, diff))
            checkIdentical(as.list(pmax(list1, list2)),
                           mapply(pmax, list1, list2))
            checkIdentical(as.list(pmin(list1, list2)),
                           mapply(pmin, list1, list2))
            checkIdentical(as.list(pmax.int(list1, list2)),
                           mapply(pmax.int, list1, list2))
            checkIdentical(as.list(pmin.int(list1, list2)),
                           mapply(pmin.int, list1, list2))
            checkIdentical(mean(list1, na.rm=TRUE),
                           sapply(list1, mean, na.rm=TRUE))
            checkIdentical(var(list1, na.rm=TRUE),
                           sapply(list1, var, na.rm=TRUE))
            checkIdentical(cov(list1, list2, use="complete.obs"),
                           mapply(cov, list1, list2,
                                  MoreArgs = list(use="complete.obs")))
            checkIdentical(cor(list1, list2, use="complete.obs"),
                           mapply(cor, list1, list2,
                                  MoreArgs = list(use="complete.obs")))
            checkIdentical(sd(list1, na.rm=TRUE),
                           sapply(list1, sd, na.rm=TRUE))
            checkIdentical(median(list1, na.rm=TRUE),
                           sapply(list1, median, na.rm=TRUE))
            checkIdentical(quantile(list1, na.rm=TRUE),
                           sapply(list1, quantile, na.rm=TRUE))
            checkIdentical(mad(list1, na.rm=TRUE),
                           sapply(list1, mad, na.rm=TRUE))
            checkIdentical(IQR(list1, na.rm=TRUE),
                           sapply(list1, IQR, na.rm=TRUE))

            vec3 <- (-20:20)^2
            vec3[c(1,10,21,41)] <- c(100L, 30L, 400L, 470L)
            list3 <- do.call(type, list(one = vec3, rev(vec3), compress = compress))
            checkIdentical(as.list(smoothEnds(list3)), lapply(list3, smoothEnds))
            checkIdentical(as.list(runmed(list3, 7)),
                           lapply(list3, function(x) {
                                      y <- runmed(x, 7)
                                      if (type != "RleList")
                                          y <- as.vector(y)
                                      y
                                  }))
        }
    }
}

test_AtomicList_character <- function() {
    txt <- c("The", "licenses", "for", "most", "software", "are",
             "designed", "to", "take", "away", "your", "freedom",
             "to", "share", "and", "change", "it.",
             "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
             "is", "intended", "to", "guarantee", "your", "freedom", "to",
             "share", "and", "change", "free", "software", "--",
             "to", "make", "sure", "the", "software", "is",
             "free", "for", "all", "its", "users")
     for (compress in c(TRUE, FALSE)) {
         for (type in c("CharacterList", "RleList")) {
             list1 <- do.call(type, list(one = txt, rev(txt), compress = compress))
             checkIdentical(as.list(nchar(list1)), lapply(list1, nchar))
             checkIdentical(as.list(chartr("@!*", "alo", list1)),
                            lapply(list1, chartr, old="@!*", new="alo"))
             checkIdentical(as.list(tolower(list1)), lapply(list1, tolower))
             checkIdentical(as.list(toupper(list1)), lapply(list1, toupper))
             checkIdentical(as.list(sub("[b-e]",".", list1)),
                            lapply(list1, sub, pattern="[b-e]", replacement="."))
             checkIdentical(as.list(gsub("[b-e]",".", list1)),
                            lapply(list1, gsub, pattern="[b-e]", replacement="."))
        }
    }
}
