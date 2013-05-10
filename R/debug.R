###

debug_AEbufs <- function()
    invisible(.Call2("debug_AEbufs", PACKAGE="IRanges"))

debug_IRanges_class <- function()
    invisible(.Call2("debug_IRanges_class", PACKAGE="IRanges"))

debug_Grouping_class <- function()
    invisible(.Call2("debug_Grouping_class", PACKAGE="IRanges"))

debug_inter_range_methods <- function()
    invisible(.Call2("debug_inter_range_methods", PACKAGE="IRanges"))

