### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Statistical routines
###

## TODO: lm, glm, loess, ...

setMethod("xtabs", signature(data = "DataTable"),
          function(formula = ~., data, subset, na.action, exclude = c(NA, NaN),
                   drop.unused.levels = FALSE)
          {
            data <- as.data.frame(data)
            callGeneric()
          })

