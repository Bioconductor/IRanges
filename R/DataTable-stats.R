### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Statistical routines
###

## TODO: lm, glm, loess, ...

setGeneric("xtabs", 
           function(formula = ~., data = parent.frame(), subset, na.action,
                    exclude = c(NA, NaN), drop.unused.levels = FALSE)
           standardGeneric("xtabs"))

setMethod("xtabs", signature(data = "DataTable"),
          function(formula, data, subset, na.action, exclude,
                   drop.unused.levels)
          {
            data <- as.env(data)
            callGeneric()
          })

