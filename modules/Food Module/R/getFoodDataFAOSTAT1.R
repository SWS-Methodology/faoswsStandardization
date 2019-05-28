##' Get Total Trade Data From FAOSTAT1
##'
##' This function pulls the old food data from FAOSTAT1 and converts the codes 
##' for the countries, items and flags.
##'
##' @param geographicAreaM49 Character vector with the countries in m49 code.
##' @param measuredItemCPC Character vector with the cpc codes.
##' @param yearRange Character vector with the range of the years.
##' @param dataset String with the id name of the dataset in the domain FAOSTAT1. 
##'
##' @return The dataset with the food data.
##'
##' @export
##'

getFoodDataFAOSTAT1 <- function(geographicAreaM49, measuredItemCPC, yearRange, dataset) {
    foodCode <- c("141")
    fcl =  suppressWarnings(as.character(as.numeric(cpc2fcl(measuredItemCPC, returnFirst = T))))
    fcl = fcl[!is.na(fcl)]
    countryFS =  suppressWarnings(m492fs(geographicAreaM49))
    countryFS = countryFS[!is.na(countryFS)]
    foodKey = DatasetKey(
        domain = "faostat_one",
        dataset = dataset,
        dimensions = list(
            Dimension(name = "geographicAreaFS",
                      keys = countryFS),
            Dimension(name = "measuredElementFS", keys = foodCode),
            Dimension(name = "timePointYears", keys = yearRange),
            Dimension(name = "measuredItemFS",
                      keys = fcl)
        )
    )
    
    foodData = GetData(
        foodKey,
        flags = TRUE)
    
    foodData[, geographicAreaM49 := fs2m49(geographicAreaFS)]
    foodData[, measuredItemCPC := fcl2cpc(formatC(as.numeric(measuredItemFS), width = 4,
                                                        flag = "0"))]
    
    foodData[, flagObservationStatus := getFlagObservationStatus(flagFaostat)]
    foodData[, flagMethod := getFlagMethod(flagFaostat)]    
    
    foodData[, measuredElement := "5141"]
    foodData[, c("geographicAreaFS", "measuredItemFS", "measuredElementFS", "flagFaostat") := NULL]
    
    setcolorder(foodData, c("geographicAreaM49", "measuredElement",
                                  "measuredItemCPC", "Value", "timePointYears",
                                  "flagObservationStatus", "flagMethod"))

    foodData <- foodData[!is.na(geographicAreaM49)]
    
}
