#' USA wheat SUA from 2011
#' 
#' This dataset contains an example of the data that could occur in the SUA
#' file.  Note that, at the time of creation, this data represents all the data
#' that was currently available for USA wheat in the SUA table, but that more
#' data would become available as other modules are run.  Thus, this dataset is
#' intended to be used ONLY as an example of how the code works.
#' 
#' \itemize{
#'      \item geographicAreaM49 The geographic area code for USA.
#'      \item measuredElementSuaFbs The element code.  5510 represents
#'      production, for example.
#'      \item measuredItemSuaFbs The commodity code, in CPC format.  For
#'      example, 0111 corresponds to wheat.
#'      \item timePointYears The year.
#'      \item Value The value corresponding to the particular commodity/element
#'      for the USA in 2011.
#'      \item flagObservationStatus The first flag, indicating the status of the
#'      observation.
#'      \item flagMethod The second flag, providing more detail on the
#'      collection/imputation method for the observations.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name usaWheat
#' @usage data(usaWheat)
#' @format A data table with 22 rows and 7 variables
NULL