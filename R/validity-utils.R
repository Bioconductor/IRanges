### 
### Utility functions for reducing redundant testing of object validity.
###

.validity_options <- new.env(hash=TRUE, parent=emptyenv())

assign("debug", FALSE, envir=.validity_options)
assign("disabled", FALSE, envir=.validity_options)

debugValidity <- function(debug)
{
    if (missing(debug))
        return(get("debug", envir=.validity_options))
    debug <- isTRUE(debug)
    assign("debug", debug, envir=.validity_options)
    debug
}

disableValidity <- function(disabled)
{
    if (missing(disabled))
        return(get("disabled", envir=.validity_options))
    disabled <- isTRUE(disabled)
    assign("disabled", disabled, envir=.validity_options)
    disabled
}

setValidity2 <- function(Class, valid.func, where=topenv(parent.frame()))
{
    setValidity(Class,
        function(object)
        {
            if (disableValidity())
                return(TRUE)
            if (debugValidity()) {
                whoami <- paste("validity method for", Class, "object")
                cat("[debugValidity] Entering ", whoami, "\n", sep="")
                on.exit(cat("[debugValidity] Leaving ", whoami, "\n", sep=""))
            }
            problems <- valid.func(object)
            if (isTRUE(problems) || length(problems) == 0L)
                return(TRUE)
            problems
        },
        where=where
    )
}

new2 <- function(..., check=TRUE)
{
    if (!isTRUEorFALSE(check))
        stop("'check' must be TRUE or FALSE")
    old_val <- disableValidity()
    on.exit(disableValidity(old_val))
    disableValidity(!check)
    new(...)
}

stopIfProblems <- function(problems)
    if (!is.null(problems)) stop(paste(problems, collapse="\n  "))

