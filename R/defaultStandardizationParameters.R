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
        productionCode = "5510",
        yieldCode = "5421",
        areaHarvCode = "5312",
        importCode = "5600",
        exportCode = "5900",
        stockCode = "71",
        foodCode = "5141",
        foodProcCode = "f???",
        feedCode = "5520",
        wasteCode = "5120",
        seedCode = "5525",
        industrialCode = "i???",
        touristCode = "t???",
        residualCode = "r???"
#        adjustVar = "adjustment"
    )
}