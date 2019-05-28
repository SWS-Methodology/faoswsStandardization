##' Get Flag Method
##'
##' This function converts the Faostat flag to the method flag
##'
##' @param flags Character vector with the Faostat flag.
##' @return Vector with the method flag.
##'
##' @export
##'

getFlagMethod <- function(flags){
    
    new_flags <- rep(NA_character_, length(flags))
    
    # flags <- base::trimws(flags)
    flags <- sub(" +$", "", sub("^ +", "", flags))
    
    new_flags[nchar(flags) > 1L] <- paste0("#", new_flags[nchar(flags) > 1L])
    new_flags[nchar(flags) == 0L] <- "-"
    
    new_flags[flags == "*"] <- "-"
    new_flags[flags %in% c("/", "X")] <- "c"
    new_flags[flags == "E"] <- "e"
    new_flags[flags == "F"] <- "f"
    new_flags[flags == "P"] <- "p"
    new_flags[flags == "M"] <- "u"
    new_flags[flags == "B"] <- "b"
    new_flags[flags == "C"] <- "i"
    new_flags[flags == "T"] <- "t"
    
    new_flags[is.na(new_flags)] <- paste0("$", flags[is.na(new_flags)])
    
    return(new_flags)
    
}
