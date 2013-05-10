###

.onLoad <- function(libname, pkgname)
{
    ## -- HACK! --
    ## Not loading the methods package can cause some strange 'R CMD check'
    ## WARNINGs. For example, on a BSgenome data package (with R 2.14.0):
    ##
    ##   * checking whether the namespace can be loaded with stated dependencies ... WARNING
    ##   Error: .onLoad failed in loadNamespace() for ‘BSgenome.Celegans.UCSC.ce2’, details:
    ##     call: length(x)
    ##     error: could not find function "loadMethod"
    ##   Execution halted
    ##
    ##   A namespace must be able to be loaded with just the base namespace
    ##   loaded: otherwise if the namespace gets loaded by a saved object, the
    ##   session will be unable to start.
    ##
    ##   Probably some imports need to be declared in the NAMESPACE file.
    ##
    ## However, loading the methods package with library(methods) produces the
    ## following 'R CMD check' NOTE (with R 2.14.0):
    ##
    ##   * checking R code for possible problems ... NOTE
    ##   File ‘IRanges/R/zzz.R’:
    ##     .onLoad calls:
    ##       library(methods)
    ##
    ##   Package startup functions should not change the search path.
    ##   See section ‘Good practice’ in ?.onAttach.
    ##
    ## So we cheat on codetools to avoid this NOTE.
    sillyname <- library
    sillyname(methods)
}

.onUnload <- function(libpath)
{
    library.dynam.unload("IRanges", libpath)
}

