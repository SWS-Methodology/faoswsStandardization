
# Retain only official loss (flagobservationStatus "" and "T") data for year 2014 in agriculture produciton domain. 


## load the libraries
library(faosws)
library(data.table)
library(faoswsUtil)
library(sendmailR)
library(dplyr)
library(faoswsFlag)


## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/lossData_deletion/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}








message("Pulling Loss Data from agriculture domain in QA") 



#Define element dimension. These elements are needed to calculate net supply (production + net trade)

eleDim <- Dimension(name = "measuredElement", keys = c("5016"))


#Define item dimension

itemKeys = GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemKeys = itemKeys[, code]


itemDim <- Dimension(name = "measuredItemCPC", keys = itemKeys)


geoKeys = GetCodeList(domain = "agriculture", dataset = "aproduction",
                      dimension = "geographicAreaM49")[type == "country", code]


#take geo keys
geoDim = Dimension(name = "geographicAreaM49", keys = geoKeys)


# Define time dimension

timeDim <- Dimension(name = "timePointYears", keys = as.character(2014:2016))



#Define the key to pull SUA data
key = DatasetKey(domain = "agriculture", dataset = "aproduction", dimensions = list(
  geographicAreaM49 = geoDim,
  measuredElement = eleDim,
  measuredItemCPC = itemDim,
  timePointYears = timeDim
))



loss_data <- GetData(key)
data_to_delete <- loss_data[!flagObservationStatus %in% c("","T")]


data_to_delete[, Value := NA_real_]
data_to_delete[, flagObservationStatus := NA_character_]
data_to_delete[, flagMethod := NA_character_]

# Save final data to SWS
cat("Save the final data...\n")

stats = SaveData(domain = "agriculture", dataset = "aproduction", data = setDT(data_to_delete), waitTimeout = 1800)

paste0("Only Loss Official Data have been retained for 2014")

