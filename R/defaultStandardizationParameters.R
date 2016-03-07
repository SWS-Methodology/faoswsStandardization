##' Default Standardization Parameters
##' 
##' Provides an object which contains the standardization parameters.  This
##' allows for easy passing into functions.
##' 
##' @return A list with the standardization parameters.
##' 
##' @export
##' 

defaultStandardizationParameters = function(){
    geoVar = "geographicAreaM49"
    yearVar = "timePointYears"
    itemVar = "measuredItemCPC"
    list(
        geoVar = geoVar,
        yearVar = yearVar,
        itemVar = itemVar,
        elementVar = "measuredElement",
        mergeKey = c(geoVar, yearVar, itemVar), # For merging with the main data
        groupID = "groupID",
        elementPrefix = "Value_measuredElement_",
        childVar = "childID",
        parentVar = "parentID",
        standParentVar = "standParentID",
        extractVar = "extractionRate",
        standExtractVar = "standExtractionRate",
        shareVar = "share",
        targetVar = "target",
        productionCode = "production",
        yieldCode = "yield",
        areaHarvCode = "areaHarvested",
        importCode = "imports",
        exportCode = "exports",
        stockCode = "stockChange",
        foodCode = "food",
        foodProcCode = "f???",
        feedCode = "feed",
        wasteCode = "loss",
        seedCode = "seed",
        industrialCode = "industrial???",
        touristCode = "ttourist",
        residualCode = "residual"
#        adjustVar = "adjustment"
    )
}