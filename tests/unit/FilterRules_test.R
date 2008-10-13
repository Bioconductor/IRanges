test_FilterRules_construct <- function() {

  ## as a simple character vector
  filts <- c("peaks", "promoters")
  parsedFilts <- list(peaks = expression(peaks),
                      promoters = expression(promoters))

  filters <- FilterRules()
  checkTrue(validObject(filters))
  checkIdentical(as.list(filters), structure(list(), names = character()))
  
  filters <- FilterRules(filts)
  checkTrue(validObject(filters))
  checkIdentical(as.list(filters), parsedFilts)
  checkIdentical(active(filters), structure(rep(TRUE, 2), names=filts))

  ## with functions and expressions
  filts <- c(parsedFilts, list(find_eboxes = function(rd) rep(FALSE, nrow(rd))))
  filters <- FilterRules(filts, active = FALSE)
  checkTrue(validObject(filters))
  checkIdentical(as.list(filters), filts)
  checkIdentical(active(filters), structure(rep(FALSE, 3), names=names(filts)))

  ## direct, quoted args (character literal parsed)
  filters <- FilterRules(under_peaks = peaks, in_promoters = "promoters")
  filts <- list(under_peaks = expression(peaks),
                in_promoters = expression(promoters))
  checkTrue(validObject(filters))
  checkIdentical(as.list(filters), filts)
  ## mix them up
  filters <- FilterRules(filts, diffexp = de)
  checkTrue(validObject(filters))
  checkIdentical(as.list(filters), c(list(diffexp = expression(de)), filts))
  filts <- as.list(filters)
  
  checkException(FilterRules(c(filts, 1)))
  checkException(FilterRules(filts, active = filts))
  checkException(FilterRules(filts, active = rep(TRUE, 5)))
  checkException(FilterRules(filts, active = rep(TRUE, 2)))
  checkException(FilterRules(list(find_eboxes = function() NULL)))
}

test_FilterRules_append <- function() {
  filts <- c("peaks", "promoters")
  filts2 <- c("introns", "exons")

  filters <- FilterRules(filts)
  filters2 <- FilterRules(filts2, active=FALSE)

  both <- append(filters, filters2)
  checkTrue(validObject(both))
  bothFilts <- structure(list(quote(peaks), quote(promoters),
                              quote(introns), quote(exons)),
                         names = c(filts, filts2))
  checkIdentical(unlist(both), bothFilts)
  bothActive <- structure(c(TRUE, TRUE, FALSE, FALSE), names = names(bothFilts))
  checkIdentical(active(both), bothActive)
  both <- c(filters, filters2)
  checkTrue(validObject(both))
  checkIdentical(unlist(both), bothFilts)
  checkIdentical(active(both), bothActive)

  filters[["cons"]] <- "cons"
  filts <- list(peaks = quote(peaks), promoters = quote(promoters))
  filts <- c(filts, cons = quote(cons))
  checkIdentical(unlist(filters), filts)
  filters[["cons"]] <- quote(cons)
  checkIdentical(unlist(filters), filts)
  filters[["cons"]] <- expression(cons)
  checkIdentical(unlist(filters), filts)
  fun <- function(rd) rep(FALSE, nrow(rd))
  filters[[4]] <- fun
  filts <- c(filts, X = fun)
  checkIdentical(unlist(filters), filts)
  
  checkException(filters[[]] <- "threeprime")
  checkException(filters[[1]] <- 2)
  checkException(filters[[1]] <- list(quote(foo), quote(bar)))
}

test_FilterRules_subset <- function() {
  filts <- c("peaks", "promoters", "introns")
  filters <- FilterRules(filts)

  checkIdentical(sapply(unlist(filters[1:2]), deparse),
                 structure(filts[1:2], names = filts[1:2]))
  checkIdentical(sapply(unlist(filters[]),deparse),
                 structure(filts, names = filts))
  checkException(filters[1,2])
}

test_FilterRules_active <- function() {
  filts <- c("peaks", "promoters", "introns")
  filters <- FilterRules(filts)

  ## set the active state directly
  
  active(filters) <- FALSE
  checkIdentical(active(filters), structure(rep(FALSE, 3), names = filts))
  active(filters) <- TRUE
  checkIdentical(active(filters), structure(rep(TRUE, 3), names = filts))
  active(filters) <- c(FALSE, FALSE, TRUE)
  checkIdentical(active(filters),
                 structure(c(FALSE, FALSE, TRUE), names = filts))
  active(filters)["promoters"] <- TRUE
  checkIdentical(active(filters),
                 structure(c(FALSE, TRUE, TRUE), names = filts))
  checkException(active(filters) <- rep(FALSE, 2))
  checkException(active(filters) <- rep(FALSE, 5))
  checkException(active(filters)["introns"] <- NA)
  
  ## toggle the active state by name or index
  
  active(filters) <- c(NA, 2) # NA's are dropped
  checkIdentical(active(filters),
                 structure(c(FALSE, TRUE, FALSE), names = filts))
  active(filters) <- c("peaks", NA) 
  checkIdentical(active(filters),
                 structure(c(TRUE, FALSE, FALSE), names = filts))
  checkException(active(filters) <- "foo")
  checkException(active(filters) <- 15)  
}
