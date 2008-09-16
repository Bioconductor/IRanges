library("IRanges")
library("RUnit")

options(warn=1)

dirs <- 'unit'

testFilePat <- ".*_test\\.R$"

allSuite <- defineTestSuite(name="allSuite",
                            dirs=dirs,
                            testFileRegexp=testFilePat,
                            rngKind="default",
                            rngNormalKind="default")

testData <- runTestSuite(allSuite)


q(runLast=FALSE)
