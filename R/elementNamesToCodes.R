##' Element Names to Codes
##' 
##' This function converts element names (such as production, imports, ...) into
##' codes (such as 5510, 5321, etc.).  It only works with the codes defined in 
##' this file on the share drive: 
##' paste0(R_SWS_SHARE_PATH,"/browningj/elementCodes.csv") (which will 
##' eventually become an adhoc table).  The tricky part of this function is that
##' the commodity type must be used to correctly choose the element code, as 
##' different commodities may use different codes for production, yield, ...
##' 
##' @param data The data.table containing the codes to be mapped.  It is assumed
##'   to have one column with element codes and one column with item codes.
##' @param elementCol The column name of the column with element codes.
##' @param itemCol The column name of the column with item codes.
##' @param standParams A list of parameters (see 
##'   faoswsStandardization::defaultStandardizationParameters).  If supplied, 
##'   elementCol and itemCol can be NULL.
##'   
##' @return The passed data.table "data" but with an updated element column 
##'   (with codes instead of names).
##'   
##' @export

elementNamesToCodes = function(data, elementCol = NULL, itemCol = NULL,
                               standParams = NULL){
    ## Copy data so as to not modify it in place.  Note: we can't have this
    ## function simply modify data in place because it has to merge data with
    ## other objects.
    out = copy(data)
    
    ## Data quality checks
    stopifnot(is(out, "data.table"))
    if((is.null(elementCol) | is.null(itemCol)) & is.null(standParams)){
        stop("Must supply either standParams or both itemCol and elementCol!")
    }
    if(!is.null(standParams)){
        elementCol = standParams$elementVar
        itemCol = standParams$itemVar
    }
    stopifnot(c(elementCol, itemCol) %in% colnames(out))
    if("type" %in% colnames(out)){
        stop("'type' is in colnames(data), and this function (elementCodesToNames)",
             " creates a new column with that name.  For safety, an error is thrown.")
    }
    
    ## Get the mapping from item code to item type.
    elementMap = GetCodeList("suafbs", "fbs", "measuredItemSuaFbs")
    setnames(elementMap, "code", itemCol)
    elementMap = elementMap[, c(itemCol, "type"), with = FALSE]
    out = merge(out, elementMap, by = itemCol, all.x = TRUE)
    if(any(is.na(out$type))){
        failures = out[is.na(type), unique(get(itemCol))]
        if(length(failures) > 20) failures = failures[1:20]
        warning(sum(is.na(out$type)), " total item types are missing (of ",
                nrow(out), " observations).", "  Here's some examples:\n",
                paste(failures, collapse = ", "))
    }
    
    ## Map element codes to names
    warning("This map should eventually be updated as an adhoc table!")
    itemCodeKey = fread(paste0(R_SWS_SHARE_PATH,"/browningj/elementCodes.csv"))
    itemCodeKey[, c("description", "factor") := NULL]
    ## Assign different names to repeated records (caused by things like
    ## biological meat).
    itemCodeKey = itemCodeKey[order(itemType, production), ]
    itemCodeKey[, suffix := paste0("_", 1:.N), by = "itemType"]
    itemCodeKey[suffix == "_1", suffix := ""]
    itemCodeKey = melt(data = itemCodeKey, id.vars = c("itemType", "suffix"))
    itemCodeKey = itemCodeKey[!is.na(value), ]
    ## Verify we don't have duplicated codes
    if(nrow(itemCodeKey[, !"suffix", with = FALSE]) >
       nrow(unique(itemCodeKey[, !"suffix", with = FALSE]))){
        stop("We have 2+ records with the same element code and item type yet",
             " representing different things (i.e. biological and standard ",
             "meat).  This will cause ambiguity in the data and must be ",
             "fixed.")
    }
    itemCodeKey[, variable := paste0(variable, suffix)]
    itemCodeKey[, suffix := NULL]
    setnames(itemCodeKey, c("itemType", "variable"), c("type", elementCol))
    out = merge(out, itemCodeKey, by = c("type", elementCol), all.x = TRUE)
    if(any(is.na(out$value))){
        failures = unique(out[is.na(value), list(get(itemCol), type)])
        if(length(failures) > 20) failures = failures[1:20]
        warning(sum(is.na(out$value)), " total item types are missing (of ",
                nrow(out), " observations) and there are ", nrow(failures),
                " unique missing combinations.")
    }
    
    ## Replace elementCol with the new names:
    out[, c(elementCol) := value]
    out[, c("value", "type") := NULL]
    return(out)
}