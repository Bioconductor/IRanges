### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Statistical routines
###

## TODO: lm, glm, loess, ...

setGeneric("xtabs", signature = c("formula", "data"),
           function(formula = ~., data = parent.frame(), subset, na.action,
                    exclude = c(NA, NaN), drop.unused.levels = FALSE)
           standardGeneric("xtabs"))

setMethod("xtabs", signature(formula = "ANY", data = "DataTable"),
          function(formula, data, subset, na.action, exclude,
                   drop.unused.levels)
          {
            data <- as.env(data)
            callGeneric()
          })

