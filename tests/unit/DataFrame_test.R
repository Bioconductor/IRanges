test_DataFrame_basic <- function() {
  x <- XDataFrame(a = 1:10, b = 11:20)
  y <- as.data.frame(x)

  checkIdentical(x[,1], y[,1])
  checkIdentical(as.data.frame(x[,2:1]), y[,2:1])
#  checkIdentical(as.data.frame(cbind(x,x)), cbind(y,y))
  checkIdentical(dim(x), dim(y))
  checkIdentical(nrow(x), nrow(y))
  checkIdentical(ncol(x), ncol(y))
  checkIdentical(as.data.frame(head(x)), head(y))
  checkTrue(is.array(x))
  checkIdentical(as.data.frame(rbind(x,x)), rbind(y,y))
#  checkIdentical(as.data.frame(tail(x)), tail(y))
}

test_DataFrame_subset <- function() {
  y <- airquality
  rownames(y) <- as.character(seq_len(nrow(y)))
  x <- as(y, "XDataFrame")
  checkIdentical(as.data.frame(subset(x, Temp > 80, select = c(Ozone, Temp))),
                 subset(y, Temp > 80, select = c(Ozone, Temp)))
  checkIdentical(as.data.frame(subset(x, Day == 1, select = -Temp)),
                 subset(y, Day == 1, select = -Temp))
  checkIdentical(as.data.frame(subset(x, select = Ozone:Wind)),
                 subset(y, select = Ozone:Wind))
}