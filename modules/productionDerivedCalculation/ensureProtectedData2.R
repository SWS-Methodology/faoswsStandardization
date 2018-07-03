ensureProtectedData2 = function (data, domain = "suafbs", dataset = "sua_unbalanced", 
          areaVar = "geographicAreaM49", itemVar = "measuredItemChildCPC", 
          elementVar = "measuredElement", yearVar = "timePointYears", 
          flagObservationVar = "flagObservationStatus", flagMethodVar = "flagMethod", 
          flagTable = flagValidTable, returnData = TRUE, normalised = TRUE, 
          denormalisedKey = "measuredElement", getInvalidData = FALSE) 
{
    dataCopy = copy(data)
    if (!normalised) {
        dataCopy = normalise(dataCopy)
    }
    setkeyv(dataCopy, col = c(areaVar, itemVar, elementVar, yearVar))
    ensureDataInput(data = dataCopy, requiredColumn = c(areaVar, 
                                                        itemVar, elementVar, yearVar), returnData = FALSE)
    if (NROW(dataCopy) > 0) {
        importCode = "5610"
        exportCode = "5910"
        productionCode="5510"
        seedCode="5525"
        stocksCode = "5071"
        
        message("Pulling data from SUA_unbalanced")
        
        geoDim = Dimension(name = "geographicAreaM49", keys = currentGeo)
        eleDim = Dimension(name = "measuredElementSuaFbs", keys = c(productionCode, seedCode, importCode, exportCode,stocksCode))
        
        itemDim = Dimension(name = "measuredItemFbsSua", keys = unique(dataCopy$measuredItemChildCPC))
        sua_un_key = DatasetKey(domain = "suafbs", dataset = "sua_unbalanced",
                                dimensions = list(
                                    geographicAreaM49 = geoDim,
                                    measuredElementSuaFbs = eleDim,
                                    measuredItemFbsSua = itemDim,
                                    timePointYears = timeDim)
        )
        dbData = GetData(sua_un_key)
        setkeyv(dbData, col = c(areaVar, "measuredItemFbsSua", "measuredElementSuaFbs", 
                                yearVar))
        matchSet = dbData[dataCopy, ]
        protectedFlagCombination = with(flagTable[flagTable$Protected, 
                                                  ], paste0("(", flagObservationStatus, ", ", flagMethod, 
                                                            ")"))
        matchSet[, `:=`(c("flagCombination"), paste0("(", flagObservationStatus, 
                                                     ", ", flagMethod, ")"))]
        invalidData = matchSet[matchSet$flagCombination %in% 
                                   protectedFlagCombination, ]
        if (getInvalidData) {
            if (!normalised) {
                invalidData = normalise(invalidData, denormalisedKey)
            }
            return(invalidData)
        }
        else {
            if (nrow(invalidData) > 0) 
                stop("Protected Data being over written!")
            if (!normalised) {
                dataCopy = denormalise(dataCopy, denormalisedKey)
            }
            message("Data can be safely saved back to database")
            if (returnData) 
                return(dataCopy)
        }
    }
    else {
        warning("Data to be saved contain no entry")
        return(dataCopy)
    }
}