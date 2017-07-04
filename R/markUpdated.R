##' Mark Updated Cells
##' 
##' This function compares two data.tables for differences.  Note that it 
##' assumes a very specific structure, as it is intended to be used with the 
##' standardization code only.  An additional column is placed on the first 
##' data.table that is named updateFlag, and it is TRUE if there is a difference
##' and FALSE otherwise.
##' 
##' @param data The new data.table with the standardization data.
##' @param old The original data.table with the standardization data.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return The same data.table as new but with an additional column.
##'   

markUpdated = function(new, old, standParams){
    old = old[, c(standParams$mergeKey, standParams$elementVar, "Value"),
              with = FALSE]
    new = merge(new, old, by = c(standParams$mergeKey, standParams$elementVar),
                all.x = TRUE, suffixes = c("", ".old"))
    new[, updateFlag := !mapply(identical, x = Value, y = Value.old)]
    new[, Value.old := NULL]
    return(new)
}