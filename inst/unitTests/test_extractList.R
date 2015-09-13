###

test_mergeGroupsInSupergroups <- function()
{
    mergeGroupsInSupergroups <- IRanges:::mergeGroupsInSupergroups

    x <- CharacterList(
           NULL,
           LETTERS[1:3],
           LETTERS[4:5],
           letters[1:5],
           NULL,
           letters[6:7]
    )

    supergroups <- PartitioningByEnd(c(SG1=3, SG2=6))
    current <- mergeGroupsInSupergroups(x, supergroups)
    target <- CharacterList(SG1=LETTERS[1:5], SG2=letters[1:7])
    checkIdentical(target, current)

    supergroups <- PartitioningByEnd(c(SG1=2, SG2=5, SG3=6))
    current <- mergeGroupsInSupergroups(x, supergroups)
    target <- CharacterList(SG1=LETTERS[1:3],
                            SG2=c(LETTERS[4:5], letters[1:5]),
                            SG3=letters[6:7])
    checkIdentical(target, current)

    supergroups <- PartitioningByEnd(c(SG1=2, 2, SG2=5, SG3=6))
    current <- mergeGroupsInSupergroups(x, supergroups)
    target <- c(target[1], CharacterList(NULL), target[2:3])
    checkIdentical(target, current)

    supergroups <- PartitioningByEnd(6)
    current <- mergeGroupsInSupergroups(x, supergroups)
    target <- CharacterList(unlist(x, use.names=FALSE))
    checkIdentical(target, current)

    supergroups <- PartitioningByEnd(c(SG1=6, SG2=6, SG3=6))
    current <- mergeGroupsInSupergroups(x, supergroups)
    target <- CharacterList(SG1=unlist(x, use.names=FALSE), SG2=NULL, SG3=NULL)
    checkIdentical(target, current)

    supergroups <- PartitioningByEnd(c(0, 0, 0, 6, 6))
    current <- mergeGroupsInSupergroups(x, supergroups)
    target <- CharacterList(NULL, NULL, NULL,
                            unlist(x, use.names=FALSE),
                            NULL)
    checkIdentical(target, current)

    supergroups <- PartitioningByEnd(seq_along(x))
    checkIdentical(x, mergeGroupsInSupergroups(x, supergroups))  # no-op
}

