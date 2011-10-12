###

.onLoad <- function(libname, pkgname)
{
    ## -- FIRST HACK --
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

    ## -- SECOND HACK --
    ## The purpose of this 2nd hack below is to fix the prototypes of the
    ## following classes: SharedRaw, SharedInteger, SharedDouble, XRaw,
    ## XInteger and XDouble. Without this hack, calling new() on any of those
    ## classes (with e.g. 'new("SharedRaw")') returns an invalid object.
    ## In order to "fix" those prototypes, we cannot use the standard
    ## mechanism (which is to specify default slot values in the prototype
    ## part of the setClass() statements) because the DLL of the package needs
    ## to be loaded before those default values can be produced.

    ## Note that we must fix the prototypes of the 3 SharedVector concrete
    ## subclasses defined in this package *before* we fix the prototypes of
    ## the 3 XVector concrete subclasses defined in this package.

    ## 3 SharedVector concrete subclasses:
    setDefaultSlotValue("SharedRaw", "xp",
                        newExternalptrWithTag(raw(0L)),
                        where=asNamespace(pkgname))

    setDefaultSlotValue("SharedInteger", "xp",
                        newExternalptrWithTag(integer(0L)),
                        where=asNamespace(pkgname))

    setDefaultSlotValue("SharedDouble", "xp",
                        newExternalptrWithTag(double(0L)),
                        where=asNamespace(pkgname))

    ## 3 XVector concrete subclasses:
    setDefaultSlotValue("XRaw", "shared",
                        new("SharedRaw"),  # is fixed now!
                        where=asNamespace(pkgname))

    setDefaultSlotValue("XInteger", "shared",
                        new("SharedInteger"),  # is fixed now!
                        where=asNamespace(pkgname))

    setDefaultSlotValue("XDouble", "shared",
                        new("SharedDouble"),  # is fixed now!
                        where=asNamespace(pkgname))
}

.onUnload <- function(libpath)
{
    library.dynam.unload("IRanges", libpath)
}

