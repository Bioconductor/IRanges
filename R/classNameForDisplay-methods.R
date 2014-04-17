### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### classNameForDisplay()
###

setGeneric("classNameForDisplay",
    function(x) standardGeneric("classNameForDisplay"))

setMethod("classNameForDisplay", "ANY",
   function(x)
   {
       ## Selecting the 1st element guarantees that we return a single string
       ## (e.g. on an ordered factor, class(x) returns a character vector of
       ## length 2).
       class(x)[1L]
   }
)

.classNameForDisplay_shorten <- function(x)
{
    sub("^(Compressed|Simple)", "", class(x))
}

setMethod("classNameForDisplay", "CompressedList",
    .classNameForDisplay_shorten)

setMethod("classNameForDisplay", "SimpleList",
    .classNameForDisplay_shorten)

setMethod("classNameForDisplay", "AsIs", function(x) {
  class(x) <- setdiff(class(x), "AsIs")
  classNameForDisplay(x)
})
