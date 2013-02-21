### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### classNameForDisplay()
###

setGeneric("classNameForDisplay",
    function(x) standardGeneric("classNameForDisplay"))

setMethod("classNameForDisplay", "ANY",
   function(x)
   {
       class(x)
   }
)

.classNameForDisplay_shorten <- function(x)
{
    sub("^(Compressed|Simple)", "", class(x))
}

setMethod("classNameForDisplay", "CompressedList",
    .classNameForDisplay_shorten)

setMethod("classNameForDisplay", "CompressedNormalIRangesList",
    .classNameForDisplay_shorten)

setMethod("classNameForDisplay", "SimpleList",
    .classNameForDisplay_shorten)

setMethod("classNameForDisplay", "SimpleNormalIRangesList",
    .classNameForDisplay_shorten)
