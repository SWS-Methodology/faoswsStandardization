##' Element Codes to Names
##' 
##' This function converts element codes (such as 5510, 5421, ...) into 
##' meaningful names (such as "production", "yield", etc.).  It only works with 
##' the codes defined in a file called elementCodes, which was om the Share drive
##' paste0(R_SWS_SHARE_PATH,"/browningj/") but now is an ad hoc dataTable in the SWS
##' in the AgricultureProduction domain.  The tricky part of this function is that
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
##' @export
##'   
##' @return The passed data.table "data" but with an updated element column
##'   (with names instead of codes).
##' 

elementCodesToNames = function(data, elementCol = NULL, itemCol = NULL,
                               standParams = NULL){
    ## Copy data so as to not modify it in place.  Note: we can't have this
    ## function simply modify data in place because it has to merge data with
    ## other objects.
    out = copy(data)
    
    ## Data quality checks
    stopifnot(is(out, "data.table"))
    if((is.null(elementCol) || is.null(itemCol)) && is.null(standParams)){
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
    elementMap = GetCodeList("agriculture", "aproduction", "measuredItemCPC")
    
    message("Get Code List ok")
    
    #### ADDED for solving the problem of cassava code: CRISTINA 
    elementMap[code=="01520.02",type:="CRPR"]
    elementMap[code=="39120.18",type:="CRNP"]
    #### 
    
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
    
    # Issue #13 - no data for these
    # These items are missing in the above:
    #        code                         description
    # 1: 01219.90 Other leafy or stem vegetables n.e.
    # 2: 23130.01   Groats, meal and pellets of Wheat
    # 3:     2351              Raw cane or beet sugar
    
    ## Map element codes to names

    
    itemCodeKey = ReadDatatable("element_codes")
    itemCodeKey[, c("description", "factor") := NULL]
    ## Cheap duct-taped on method for removing all the indigenous and biological
    ## cattle
    itemCodeKey = itemCodeKey[order(itemtype, production), ]
    itemCodeKey[, suffix := paste0("_", seq_len(.N)), by = "itemtype"]
    itemCodeKey[suffix == "_1", suffix := ""]
    # warning("All indigenous and biological cattle and poultry have been
    # removed. This is a bad thing and it would be best to find a way to replace
    # them. I recommend looking at issue #17")
    itemCodeKey = itemCodeKey[suffix == "", ]
    itemCodeKey = melt(data = itemCodeKey, id.vars = c("itemtype", "suffix"))
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
    setnames(itemCodeKey, c("itemtype", "value"), c("type", elementCol))
    itemCodeKey[, c(elementCol) := as.character(get(elementCol))]
    out = merge(out, itemCodeKey, by = c("type", elementCol), all.x = TRUE)
    if(any(is.na(out$variable))){
        failures = unique(out[is.na(variable), list(get(itemCol), type)])
        if(length(failures) > 20) failures = failures[1:20]
        warning(sum(is.na(out$variable)), " total item types are missing (of ",
                nrow(out), " observations) and there are ", nrow(failures),
                " unique missing combinations.")
    }
    
    ## Replace elementCol with the new names:
    out[, c(elementCol) := variable]
    out[, c("variable", "type") := NULL]
    return(out[])
}
