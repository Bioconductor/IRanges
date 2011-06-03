###

.onLoad <- function(libname, pkgname)
{
    ## The purpose of the hack below is to fix the prototypes of the following
    ## classes: SharedRaw, SharedInteger, SharedDouble, XRaw, XInteger and
    ## XDouble. Without this hack, calling new() on any of those classes
    ## (with e.g. 'new("SharedRaw")') returns an invalid object.
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
