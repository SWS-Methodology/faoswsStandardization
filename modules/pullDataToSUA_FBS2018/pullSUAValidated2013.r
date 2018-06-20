# from Francesca Rosa
# pull SUA validated data 2013 



# load("C:/Users/Rosa/Desktop/test/SUA_sua updated 2013/oldSua12032018_2013_AllCountriesDES.RData")
##unique(oldSua13[,measuredElementSuaFbs])

########################################################################################################
## load the library
library(faosws)
suppressMessages({library(data.table)})
library(faoswsUtil)
library(sendmailR)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")
  
  library(faoswsModules)
  SETT <- ReadSettings("modules/pullDataToSUA_FBS2018/sws.yml")
  
  R_SWS_SHARE_PATH <- SETT[["share"]]  
  ## Get SWS Parameters
  SetClientFiles(dir = SETT[["certdir"]])
  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

yearkey = 2000:2013
geoKey = GetCodeList(domain = "suafbs", dataset = "sua_validated_2015", "geographicAreaM49")[,code]
itemKey = GetCodeList(domain = "suafbs", dataset = "sua_validated_2015", "measuredItemFbsSua")[,code]
elementKey = GetCodeList(domain = "suafbs", dataset = "sua_validated_2015", "measuredElementSuaFbs")[,code]


# sessionKey = swsContext.datasets[[1]]


################################################
##### Harvest from SUA Validated 2015      #####
################################################

message("Pulling data from SUA Validated 2015")

geoDim = Dimension(name = "geographicAreaM49", keys = geoKey)

eleDim = Dimension(name = "measuredElementSuaFbs", keys = elementKey)

itemDim = Dimension(name = "measuredItemFbsSua", keys = itemKey)

timeDim = Dimension(name = "timePointYears", keys = as.character(yearkey))


suaKey = DatasetKey(domain = "suafbs", dataset = "sua_validated_2015",
                   dimensions = list(
                     geographicAreaM49 = geoDim,
                     measuredElement = eleDim,
                     measuredItemCPC = itemDim,
                     timePointYears = timeDim)
)

suaData = GetData(suaKey)
setnames(suaData, c("measuredElement", "measuredItemCPC"),
         c("measuredElementSuaFbs", "measuredItemSuaFbs"))



##Flag
# convertFSMethodFlag <- function(flags){
#   
#   new_flags <- rep(NA_character_, length(flags))
#   
#   flags <- trimws(flags)
#   
#   new_flags[nchar(flags) > 1L] <- paste0("#", new_flags[nchar(flags) > 1L])
#   new_flags[nchar(flags) == 0L] <- "-"
#   
#   new_flags[flags == "*"] <- "-"
#   new_flags[flags %in% c("/", "X")] <- "c"
#   new_flags[flags == "E"] <- "e"
#   new_flags[flags == "F"] <- "f"
#   new_flags[flags == "P"] <- "p"
#   new_flags[flags == "M"] <- "u"
#   new_flags[flags == "B"] <- "b"
#   new_flags[flags == "C"] <- "i"
#   new_flags[flags == "T"] <- "t"
#   
#   new_flags[is.na(new_flags)] <- paste0("$", flags[is.na(new_flags)])
#   
#   return(new_flags)
#   
# }
# 
# convertFSFlag <- function(flags){
#   new_flags <- rep(NA_character_, length(flags))
#   
#   new_flags[nchar(flags) > 1L] <- "#"
#   new_flags[nchar(flags) == 0L] <- ""
#   
#   new_flags[flags %in% c("*", "P", "X")] <- "T"
#   new_flags[flags == "/"] <- ""
#   new_flags[flags %in% c("F", "T")] <- "E"
#   new_flags[flags == "M"] <- "M"
#   new_flags[flags %in% c("E", "B", "C")] <- "I"
#   
#   new_flags[is.na(new_flags)] <- paste0("$", flags[is.na(new_flags)])
#   
#   return(new_flags)
# }


# oldSua13[, flagObservationStatus:=convertFSFlag(flagFaostat)]
# oldSua13[, flagMethod:=convertFSMethodFlag (flagFaostat)]
# oldSua13[,flagFaostat:=NULL]



if(max(suaData[,unique(timePointYears)])>2013){
  stop("import ONLY up to 2013!!!!")
}else{
  
  timeWindow=unique(suaData[, timePointYears])
  
  for(i in seq_along(timeWindow)){
    
    currentYear=timeWindow[i]
    
    currentSuaData=suaData[timePointYears==currentYear]
    
    message(paste0("Number of rows:", dim(currentSuaData)[1]), ". Current year: ", currentYear)
    
    message(paste0("Save data for", currentYear))
    
    start.time <- Sys.time()
    
    SaveData(CONFIG$domain,CONFIG$dataset,currentSuaData,waitTimeout = Inf)
    
    end.time <- Sys.time()
    
    duration= end.time-start.time
    message(paste0("Time: ", duration))
  }  
}