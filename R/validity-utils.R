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

setValidity2 <- function(Class, valid.func)
{
    setValidity(Class,
        function(object)
        {
            if (debugValidity()) {
                whoami <- paste("validity method for", Class, "object")
                cat("[debugValidity] Entering ", whoami, "\n", sep="")
                on.exit(cat("[debugValidity] Leaving ", whoami, "\n", sep=""))
            }
            if (disableValidity())
                return(TRUE)
            problems <- valid.func(object)
            if (isTRUE(problems) || length(problems) == 0)
                return(TRUE)
            problems
        }
    )
}

new2 <- function(..., check=TRUE)
{
    old_val <- disableValidity()
    disableValidity(!check)
    on.exit(disableValidity(old_val))
    new(...)
}

stopIfProblems <- function(problems)
    if (!is.null(problems)) stop(paste(problems, collapse="\n  "))

