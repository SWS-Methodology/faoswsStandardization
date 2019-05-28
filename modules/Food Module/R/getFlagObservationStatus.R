##' Get Flag Observation Status
##'
##' This function converts the Faostat flag to the flag osbervation status
##'
##' @param flags Character vector with the Faostat flag.
##' @return Vector with the method flag.
##'
##' @export
##'


getFlagObservationStatus <- function(flags){
    new_flags <- rep(NA_character_, length(flags))
    
    new_flags[nchar(flags) > 1L] <- "#"
    new_flags[nchar(flags) == 0L] <- ""
    
    new_flags[flags %in% c("*", "P", "X")] <- "T"
    new_flags[flags == "/"] <- ""
    new_flags[flags %in% c("F", "T")] <- "E"
    new_flags[flags == "M"] <- "M"
    new_flags[flags %in% c("E", "B", "C")] <- "I"
    
    new_flags[is.na(new_flags)] <- paste0("$", flags[is.na(new_flags)])
    
    return(new_flags)
}