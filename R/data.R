
"utilizationTable"

#' Table of possible utilization by country/commodity combinations
#' 
#' THIS TABLE EXCLUDES LOSS FOR DERIVED COMMODITIES
#' 
#' Utilization have a rank representing the importance of that element, 
#' in terms of mean value over all the years for that combination 
#' 
#' @format a data.table with 155043 rows and 5 columns
#' \itemize{
#'  \item geographicAreaM49, M49 code of the country
#'  \item measuredElementSuaFbs, utilization Element
#'  \item measuredItemSuaFbs, CPC code
#'  \item rank, rank of that element
#'  \item rankInv, inverse rank of that element
#'  }
#'  

"zeroWeight"

#' co-products of the processing chain which do NOT have to be back Standardized for
#' not to create double counting
#' 
#' @format a vector of 90 cpc codes

"cutItems"

"fbsTree"

