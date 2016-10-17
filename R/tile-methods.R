### =========================================================================
### "tile" methods
### -------------------------------------------------------------------------
###

### TODO: We have a profileration of tools for creating these "sliding
### windows" or "tiles": successiveIRanges(), tileGenome(), tile(), and now
### slidingWindows(). With no visible coherent naming scheme. Introducing
### a new verb each time we get frustrated because the existing tools don't
### let us create tiles or windows exactly the way we'd like for the use case
### of the day is not a sustainable strategy in the long run. This just adds
### more and more confusion for the end-user.
### So some effort will need to be done towards unification of all these
### tools. H.P. -- Oct 16, 2016.

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
  tile.width <- S4Vectors:::diffWithInitialZero(as.integer(tile.end.abs))
  p <- PartitioningByWidth(n)
  tile.width[start(p)] <- tile.end[start(p)]
  relist(IRanges(width=tile.width, end=tile.end.abs), p)
})

### =========================================================================
### "slidingWindows" methods
### -------------------------------------------------------------------------
###

setGeneric("slidingWindows",
           function(x, width, step = 1L, ...) standardGeneric("slidingWindows"),
           signature="x")

setMethod("slidingWindows", "Ranges", function(x, width, step = 1L) {
    if (!isSingleNumber(width))
        stop("'width' must be a single, non-NA number")
    if (!isSingleNumber(step))
        stop("'step' must be a single, non-NA number")
    if (any(width < 0L))
        stop("some 'width' are negative")
    if (any(step < 0L))
        stop("some 'step' are negative")
    n <- ceiling(pmax(width(x) - width, 0L) / step) + 1L
    window.starts <- as.integer(IRanges(rep(0L, length(n)), width=n)) *
        step + 1L
    windows <- restrict(IRanges(window.starts, width=width),
                        end=rep(width(x), n))
    windows.abs <- shift(windows, rep(start(x), n) - 1L)
    relist(windows.abs, PartitioningByWidth(n))
})
