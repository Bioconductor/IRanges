## Sets up the generics etc. for expand() method.
## require(IRanges)

setGeneric("expand", signature="x",
           function(x, colnames, keepEmptyRows) standardGeneric("expand"))



## A helper function to do the work
.expandOneCol <- function(x, colname, keepEmptyRows)
{
  if(keepEmptyRows==TRUE){
    x[[colname]][elementLengths(x[[colname]])==0] <- NA
  }
    if (!is(x, "DataFrame"))
        stop("'x' must be a DataFrame object")
    if (!isSingleString(colname) && !isSingleNumber(colname))
        stop("'x' must be a single string or number")
    col <- x[[colname]]
    if (is.null(col))
        stop("'colname' must be a valid colname name or index")
    idx <- rep.int(seq_len(nrow(x)), elementLengths(col))
    ans <- x[idx, ]
    ans[[colname]] <- unlist(col, use.names=FALSE)
    ans
}


## A better helper
.expand <- function(x, colnames, keepEmptyRows){
  for(i in seq_len(length(colnames))){
    x <- .expandOneCol(x, colnames[i], keepEmptyRows)
  }
  x
}


## method for DataFrame
setMethod("expand", "DataFrame",
    function(x, colnames, keepEmptyRows){
      .expand(x, colnames, keepEmptyRows)
    }
)


