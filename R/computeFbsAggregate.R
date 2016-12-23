##' Compute FBS Aggregate
##' 
##' This function takes the FBS aggregation tree and rolls up the SUA data to 
##' all FBS levels.
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param fbsTree This "tree" should just have three columns: 
##'   standParams$parentID, standParams$childID, and standParams$extractVar 
##'   (which if missing will just be assigned all values of 1).  This tree 
##'   specifies how SUA commodities get combined to form the FBS aggregates.  If
##'   NULL, the last step (aggregation to FBS codes) is skipped and data is 
##'   simply returned at SUA level.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##'   
##' @return A list of four data.tables, each containing the final FBS data at
##'   the four different FBS levels.
##' 

computeFbsAggregate = function(data, fbsTree, standParams){
    ## Data Quality Checks
    stopifnot(standParams$itemVar %in% colnames(data))
    stopifnot(standParams$itemVar %in% colnames(fbsTree))
    stopifnot(paste0("fbsID", 1:4) %in% colnames(fbsTree))
    
    data = merge(data, fbsTree, by = standParams$itemVar)
    out = list()
    out[[1]] = data[, list(Value = sum(Value, na.rm = TRUE)),
                    by = c(standParams$elementVar, standParams$yearVar,
                           standParams$geoVar, "fbsID4")]
    out[[2]] = data[, list(Value = sum(Value, na.rm = TRUE)),
                    by = c(standParams$elementVar, standParams$yearVar,
                           standParams$geoVar, "fbsID3")]
    out[[3]] = data[, list(Value = sum(Value, na.rm = TRUE)),
                    by = c(standParams$elementVar, standParams$yearVar,
                           standParams$geoVar, "fbsID2")]
    out[[4]] = data[, list(Value = sum(Value, na.rm = TRUE)),
                    by = c(standParams$elementVar, standParams$yearVar,
                           standParams$geoVar, "fbsID1")]
    return(out)
}