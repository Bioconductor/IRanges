### =========================================================================
### "tile" methods
### -------------------------------------------------------------------------
###

setGeneric("tile", function(x, n, width, ...) standardGeneric("tile"),
           signature="x")

setMethod("tile", "Ranges", function(x, n, width, ...) {
  if (!missing(n)) {
    if (!missing(width))
      stop("only one of 'n' and 'width' can be specified")
    if (any(IRanges::width(x) < n))
      stop("some width(x) are less than 'n'")
    if (any(n < 0L))
      stop("some 'n' are negative")
    n <- S4Vectors:::recycleVector(n, length(x))
  }
  if (!missing(width)) {
    if (!missing(n))
      stop("only one of 'n' and 'width' can be specified")
    if (any(width < 0L))
      stop("some 'width' are negative")
    n <- ceiling(width(x) / width)
  }
  width <- IRanges::width(x) / n
  ## The floor() is intentional for compatibility with Jim Kent's BigWig code
  ## tileGenome() uses ceiling() instead
  tile.end <- floor(as.integer(IRanges(rep(1L, length(n)), width=n)) *
                    rep(width, n))
  tile.end.abs <- tile.end + rep(start(x), n) - 1L
  tile.width <- diffWithInitialZero(as.integer(tile.end.abs))
  p <- PartitioningByWidth(n)
  tile.width[start(p)] <- tile.end[start(p)]
  relist(IRanges(width=tile.width, end=tile.end.abs), p)
})
