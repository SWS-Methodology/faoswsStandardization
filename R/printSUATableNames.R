##' Print SUA Table with name of commodities
##' 
##' @param data The data.table containing the full dataset for standardization.
##' @param standParams The parameters for standardization.  These parameters 
##'   provide information about the columns of data and tree, specifying (for 
##'   example) which columns should be standardized, which columns represent 
##'   parents/children, etc.
##' @param printCodes A character vector of the elements of interest.  This is
##'   required to keep the table from getting to big/unreadable.
##' @param printProcessing Logical.  Should food processing also be printed?
##' @param nutrientElements A list of the nutrient codes which should also be
##'   printed.
##'   
##' @return Nothing is returned, but a table is returned in a nice format to 
##'   show the current SUA data.
##'   

## Function for printing the main table
printSUATableNames = function(data, standParams, printCodes, printProcessing = TRUE,
                         nutrientElements = c()){
    printDT = copy(data)
    nameDT = data.table(data.frame(printDT[,c("measuredItemSuaFbs","measuredItemFbsSua_description"),with=FALSE]))
    setnames(nameDT,colnames(nameDT),c("Item","itemName"))
    nameDT=unique(nameDT[Item%in%printCodes])
    nameDT[,itemName:=strtrim(itemName,20)]
    
    if(!"updateFlag" %in% colnames(printDT)){
        printDT[, updateFlag := FALSE]
    }
    printDT = printDT[, c(standParams$mergeKey, standParams$elementVar,
                          "Value", "updateFlag"), with = FALSE]
    printDT[, c(standParams$elementVar) := paste0("Value_measuredElement_",
                                                  get(standParams$elementVar))]
    printDT = printDT[get(standParams$itemVar) %in% printCodes, ]
    if(nrow(printDT) == 0){
        cat("None of the printCode are in the data!  Not printing anything...")
        return(NULL)
    }
    
    fbsElements = c(standParams$productionCode, standParams$feedCode,
                    standParams$seedCode, standParams$wasteCode,
                    standParams$foodCode, standParams$stockCode,
                    standParams$importCode, standParams$exportCode,
                    standParams$foodProcCode, standParams$industrialCode,
                    standParams$touristCode,standParams$residualCode)

    printDT[, Value := ifelse(is.na(Value), "-", sapply(Value, roundNum))]
    printDT[(updateFlag), Value := paste0("**", Value, "**")]
    printDT[, updateFlag := NULL]
    if(max(printDT[, .N, by = c(standParams$itemVar,
                                standParams$elementVar)]$N) > 1){
        warning("More than one record for a unique combination of item/element.",
                "Collapsing the data by pasting together values!")
        printDT = dcast(data = printDT, as.formula(paste0(standParams$itemVar, "~",
                                                          standParams$elementVar)),
                                value.var = "Value", fill = NA, fun.aggregate = function(...){
                                    paste(..., collapse = "-")
                                    }
                        )
    } else {
        printDT = dcast(data = printDT, as.formula(paste0(standParams$itemVar, "~",
                                                          standParams$elementVar)),
                                value.var = "Value", fill = NA)
    }
    setnames(printDT, standParams$itemVar,"Item")

    oldNames = paste0("Value_measuredElement_", fbsElements)
    nameFilter = oldNames %in% colnames(printDT)
    setnames(printDT, oldNames[nameFilter],
             c("Production", "Feed", "Seed", "Loss",
               "Food", "StockChange", "Imports", "Exports",
               "Food Processing", "Industrial", "Tourist","Residuals")[nameFilter])
    if(printProcessing){
        items = c("Item", "Production", "Imports", "Exports", "StockChange",
                  "Food", "Food Processing", "Feed", "Seed", "Tourist",
                  "Industrial", "Loss","Residuals", nutrientElements)
    } else {
        fbsElements = fbsElements[fbsElements != standParams$foodProcCode]
        items = c("Item", "Production", "Imports", "Exports", "StockChange",
                  "Food", "Feed", "Seed", "Tourist", "Industrial", "Loss","Residuals",
                  nutrientElements)
    }
    if(length(nutrientElements) > 0){
        setnames(printDT, paste0("Value_measuredElement_", nutrientElements),
                 nutrientElements)
    }
    sapply(items, function(colName){
        if(!colName %in% colnames(printDT)){
            printDT[, (colName) := 0]
        } else {
            printDT[is.na(get(colName)), c(colName) := "-"]
        }
    })
    
    printDT=data.table(left_join(printDT,nameDT,by="Item"))
    
    if(length(nutrientElements) > 0){
      printDT=printDT[,c("itemName","Item","Production", "Imports","Exports","StockChange",
                         "Food","Food Processing","Feed","Seed","Tourist","Industrial","Loss","Residuals",nutrientElements),with=FALSE]
      printDT=data.table(printDT)
    }else{
      printDT=printDT[,.(itemName,Item,Production, Imports,Exports,StockChange,
                         Food,`Food Processing`,Feed,Seed,Tourist,Industrial,Loss,Residuals)]

    }
    
      items = c("itemName","Item", "Production", "Imports", "Exports", "StockChange",
                "Food","Food Processing", "Feed", "Seed", "Tourist", "Industrial", "Loss","Residuals",
                nutrientElements) 

    out = print(knitr::kable(printDT[, items, with = FALSE], align = 'r'))
    return(out)
}

##' Round numbers
##' 
##' Helper function for rounding numbers for display.
##' 
##' @param x A number to be nicely formatted.
##' 
##' @return The number as a character string, formatted "nicely".
##' 

roundNum = function(x){
    if(is.na(x)){
        return(x)
    }
    initialSign = sign(x)
    x = abs(x)
    # # 1 or 2 digits: multiple of 5.
    # # 3 digits: multiple of 10.
    # # 4 to 7 digits: multiple of 100
    # # 8+ digits: 4 significant digits.
    # if(x < 100){
    #     x = round(x/5, 0)*5
    # } else if(x < 1000){
    #     x = round(x/10)*10
    # } else if(x < 10000000){
    #     x = round(x/100)*100
    # } else {
    #     x = formatC(x, digits = 4)
    #     x = as.numeric(x)
    # }
    x=round(x,0)
    x = x * initialSign
    x = prettyNum(x, big.mark = ",", scientific = FALSE)
    return(x)
}