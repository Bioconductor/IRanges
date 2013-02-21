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

.classNameForDisplay_Compressed <- function(x)
{
    sub("^Compressed", "", class(x))
}

setMethod("classNameForDisplay", "CompressedList",
    .classNameForDisplay_Compressed)

setMethod("classNameForDisplay", "CompressedNormalIRangesList",
    .classNameForDisplay_Compressed)
