library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(dplyr)
library(data.table)
library(tidyr)

# The only parameter is the string to print
# COUNTRY is taken from environment (parameter)
dbg_print <- function(x) {
  print(paste0("NEWBAL (", COUNTRY, "): ", x))
}

start_time <- Sys.time()

if (CheckDebug()) {
  mydir <- "modules/newBalancing_test"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
  R_SWS_SHARE_PATH <- "//hqlprsws1.hq.un.fao.org/sws_r_share"
}


COUNTRY <- as.character(swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys)

COUNTRY_NAME <-
  nameData(
    "suafbs", "sua_unbalanced",
    data.table(geographicAreaM49 = COUNTRY))$geographicAreaM49_description

dbg_print("parameters")

USER <- regmatches(
  swsContext.username,
  regexpr("(?<=/).+$", swsContext.username, perl = TRUE)
)

STOP_AFTER_DERIVED <- as.logical(swsContext.computationParams$stop_after_derived)

#BALANCING_METHOD <- swsContext.computationParams$balancing_method
BALANCING_METHOD <- "proportional"

#THRESHOLD_METHOD <- swsContext.computationParams$threshold_method
THRESHOLD_METHOD <- 'share'

FIX_OUTLIERS <- TRUE

#FILL_EXTRACTION_RATES<-as.logical(swsContext.computationParams$fill_extraction_rates)
FILL_EXTRACTION_RATES <- TRUE

YEARS <- as.character(2000:2017)

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
if (CheckDebug()) {
  R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"
}

p <- defaultStandardizationParameters()

p$itemVar <- "measuredItemSuaFbs"
p$mergeKey[p$mergeKey == "measuredItemCPC"] <- "measuredItemSuaFbs"
p$elementVar <- "measuredElementSuaFbs"
p$childVar <- "measuredItemChildCPC"
p$parentVar <- "measuredItemParentCPC"
p$createIntermetiateFile <- "TRUE"
p$protected <- "Protected"
p$official <- "Official"

shareDownUp_file <-
  file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_", COUNTRY, ".csv"))



# Always source files in R/ (useful for local runs).
# Your WD should be in faoswsStandardization/
sapply(dir("R", full.names = TRUE), source)

# Flags set by this plugin. These are temporary: once the testing phase is over,
# most of these should be changed to I,e, E,e, I,i. See the email sent.

dbg_print("define functions")

######### FUNCTIONS: at some point, they will be moved out of this file. ####

coeffs_stocks_mod <- function(x) {
  tmp <- lm(data = x[timePointYears <= 2013], supply_inc ~ supply_exc + trend)

  as.list(tmp$coefficients)
}


# Fill NAs by LOCF/FOCB/interpolation if more than two
# non-missing observations are available, otherwhise just
# replicate the only non-missing observation
na.fill_ <- function(x) {
  if(sum(!is.na(x)) > 1) {
    zoo::na.fill(x, "extend")
  } else {
    rep(x[!is.na(x)], length(x))
  }
}



`%!in%` <- Negate(`%in%`)


# RemainingToProcessedParent() and RemainingProdChildToAssign() will
# be used in the derivation of shareDownUp

RemainingToProcessedParent<-function(data){
  data[, 
       parent_already_processed:=ifelse(is.na(parent_qty_processed),parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
       ]
  
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  data[,
       only_child_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                 is.na(processed_to_child) &
                                 !is.na(production_of_child) &
                                 !is.na(parent_qty_processed) & 
                                 production_of_child>0,TRUE,FALSE),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
       ]
  
  data[only_child_left==TRUE,processed_to_child:=remaining_processed_parent*extractionRate]
  
  data[,
       parent_already_processed:=ifelse(is.na(parent_qty_processed), parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
       ]
  
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  
  return(data)
}


RemainingProdChildToAssign<-function(data){
  
  data[,
       available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
       ]
  
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  
  data[,
       only_parent_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                  is.na(processed_to_child) &
                                  !is.na(parent_qty_processed) & 
                                  parent_qty_processed>=0,TRUE,FALSE)
       ]
  
  data[only_parent_left==TRUE,processed_to_child:=0]
  
  data[,available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
       ]
  
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  return(data)
  
}





# The fmax function is used when fixing the processingShare of coproducts.
# If TRUE it means that "+" or "or" cases are involved.
fmax <- function(child, main, share, plusor = FALSE) {
  main <- unique(main)

   if (plusor) {
     found <- sum(sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE))

     if (found == 0) {
       return(max(share, na.rm = TRUE))
     } else if (found == 1) {
       return(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]])
     } else { # should be 2
       return(max(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]], na.rm = TRUE))
     }
  } else {
    if (sum(grepl(main, child)) > 0) {
      share[child == main]
    } else {
      max(share, na.rm = TRUE)
    }
  }
}

# Function that calculates imbalance as supply - utilizations (both calculated
# inside the function, dropped if keep_(supply|utilizations) set to FALSE).
calculateImbalance <- function(data,
                               supply_add = c("production", "imports"),
                               supply_subtract = c("exports", "stockChange"),
                               supply_all = union(supply_add, supply_subtract),
                               item_name = "measuredItemSuaFbs",
                               bygroup = c("geographicAreaM49", "timePointYears", item_name),
                               keep_supply = TRUE,
                               keep_utilizations = TRUE) {
  
  stopifnot(is.data.table(data))
  
  data[,
    `:=`(
      supply =
        sum(Value[measuredElementSuaFbs %chin% supply_add],
            - Value[measuredElementSuaFbs %chin% supply_subtract],
            na.rm = TRUE),
      # All elements that are NOT supply elements
      utilizations =
        sum(Value[!(measuredElementSuaFbs %chin% supply_all)],
            na.rm = TRUE)
    ),
    by = bygroup
  ][,
    imbalance := supply - utilizations
  ]
  
  if (keep_supply == FALSE) {
    data[, supply := NULL]
  }
  
  if (keep_utilizations == FALSE) {
    data[, utilizations := NULL]
  }
  
}

outside <- function(x, lower = NA, upper = NA) {
  x < lower | x > upper
}


send_mail <- function(from = NA, to = NA, subject = NA,
                      body = NA, remove = FALSE) {
  
  if (missing(from)) from <- 'no-reply@fao.org'
  
  if (missing(to)) {
    if (exists('swsContext.userEmail')) {
      to <- swsContext.userEmail
    }
  }
  
  if (is.null(to)) {
    stop('No valid email in `to` parameter.')
  }
  
  if (missing(subject)) stop('Missing `subject`.')
  
  if (missing(body)) stop('Missing `body`.')
  
  if (length(body) > 1) {
    body <-
      sapply(
        body,
        function(x) {
          if (file.exists(x)) {
            # https://en.wikipedia.org/wiki/Media_type 
            file_type <-
              switch(
                tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                txt  = 'text/plain',
                csv  = 'text/csv',
                png  = 'image/png',
                jpeg = 'image/jpeg',
                jpg  = 'image/jpeg',
                gif  = 'image/gif',
                xls  = 'application/vnd.ms-excel',
                xlsx = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                doc  = 'application/msword',
                docx = 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
                pdf  = 'application/pdf',
                zip  = 'application/zip',
                # https://stackoverflow.com/questions/24725593/mime-type-for-serialized-r-objects
                rds  = 'application/octet-stream'
              )
            
            if (is.null(file_type)) {
              stop(paste(tolower(sub('.*\\.([^.]+)$', '\\1', basename(x))),
                         'is not a supported file type.'))
            } else {
              return(sendmailR:::.file_attachment(x, basename(x), type = file_type))
            }
            
            if (remove) {
              unlink(x)
            }
          } else {
            return(x)
          }
        }
      )
  } else if (!is.character(body)) {
    stop('`body` should be either a string or a list.')
  }
  
  sendmailR::sendmail(from, to, subject, as.list(body))
}


################### optim stuff ###########################

my_fun <- function(s) {
  
  zero_if_na <- function(x) {
    ifelse(length(x) == 0, 0, ifelse(is.na(x), 0, x))
  }
  
  prod_i <- x[, measuredElementSuaFbs == 'production']
  impo_i <- x[, measuredElementSuaFbs == 'imports']
  expo_i <- x[, measuredElementSuaFbs == 'exports']
  stoc_i <- x[, measuredElementSuaFbs == 'stockChange']
  food_i <- x[, measuredElementSuaFbs == 'food']
  feed_i <- x[, measuredElementSuaFbs == 'feed']
  seed_i <- x[, measuredElementSuaFbs == 'seed']
  proc_i <- x[, measuredElementSuaFbs == 'foodManufacturing']
  indu_i <- x[, measuredElementSuaFbs == 'industrial']
  loss_i <- x[, measuredElementSuaFbs == 'loss']
  tour_i <- x[, measuredElementSuaFbs == 'tourist']
  resi_i <- x[, measuredElementSuaFbs == 'residual']
  
  prod_v <- zero_if_na(x$Value[prod_i])
  impo_v <- zero_if_na(x$Value[impo_i])
  expo_v <- zero_if_na(x$Value[expo_i])
  stoc_v <- zero_if_na(x$Value[stoc_i])
  food_v <- zero_if_na(x$Value[food_i])
  feed_v <- zero_if_na(x$Value[feed_i])
  seed_v <- zero_if_na(x$Value[seed_i])
  proc_v <- zero_if_na(x$Value[proc_i])
  indu_v <- zero_if_na(x$Value[indu_i])
  loss_v <- zero_if_na(x$Value[loss_i])
  tour_v <- zero_if_na(x$Value[tour_i])
  resi_v <- zero_if_na(x$Value[resi_i])
  
  #  prod_p <- x$Protected[prod_i] ; prod_p <- ifelse(length(prod_p) == 0, 0, ifelse(is.na(prod_p), 0, prod_p))
  #  impo_p <- x$Protected[impo_i] ; impo_p <- ifelse(length(impo_p) == 0, 0, ifelse(is.na(impo_p), 0, impo_p))
  #  expo_p <- x$Protected[expo_i] ; expo_p <- ifelse(length(expo_p) == 0, 0, ifelse(is.na(expo_p), 0, expo_p))
  #  stoc_p <- x$Protected[stoc_i] ; stoc_p <- ifelse(length(stoc_p) == 0, 0, ifelse(is.na(stoc_p), 0, stoc_p))
  #  food_p <- x$Protected[food_i] ; food_p <- ifelse(length(food_p) == 0, 0, ifelse(is.na(food_p), 0, food_p))
  #  feed_p <- x$Protected[feed_i] ; feed_p <- ifelse(length(feed_p) == 0, 0, ifelse(is.na(feed_p), 0, feed_p))
  #  seed_p <- x$Protected[seed_i] ; seed_p <- ifelse(length(seed_p) == 0, 0, ifelse(is.na(seed_p), 0, seed_p))
  #  proc_p <- x$Protected[proc_i] ; proc_p <- ifelse(length(proc_p) == 0, 0, ifelse(is.na(proc_p), 0, proc_p))
  #  indu_p <- x$Protected[indu_i] ; indu_p <- ifelse(length(indu_p) == 0, 0, ifelse(is.na(indu_p), 0, indu_p))
  #  loss_p <- x$Protected[loss_i] ; loss_p <- ifelse(length(loss_p) == 0, 0, ifelse(is.na(loss_p), 0, loss_p))
  #  tour_p <- x$Protected[tour_i] ; tour_p <- ifelse(length(tour_p) == 0, 0, ifelse(is.na(tour_p), 0, tour_p))
  #  resi_p <- x$Protected[resi_i] ; resi_p <- ifelse(length(resi_p) == 0, 0, ifelse(is.na(resi_p), 0, resi_p))
  
  supply <- prod_v + impo_v - expo_v - stoc_v
  
  #  utilizations <-
  #    (food_v * !food_p) * s[1] +
  #    (feed_v * !feed_p) * s[2] +
  #    (seed_v * !seed_p) * s[3] +
  #    (proc_v * !proc_p) * s[4] +
  #    (indu_v * !indu_p) * s[5] +
  #    (loss_v * !loss_p) * s[6] +
  #    (tour_v * !tour_p) * s[7] +
  #    (resi_v * !resi_p) * s[8]
  
  utilizations <-
    food_v * s[1] +
    feed_v * s[2] +
    seed_v * s[3] +
    proc_v * s[4] +
    indu_v * s[5] +
    loss_v * s[6] +
    tour_v * s[7] +
    resi_v * s[8]
  
  abs(supply - utilizations)
}


do_optim <- function(d) {
  
  elem_names <- c('food', 'feed', 'seed', 'foodManufacturing',
                  'industrial', 'loss', 'tourist', 'residual')
  
  my_lower <- d$min_adj
  my_upper <- d$max_adj
  
  my_protected <- d$Protected
  
  names(my_lower) <- names(my_upper) <- names(my_protected) <- d$measuredElementSuaFbs
  
  my_lower <- my_lower[elem_names]
  my_upper <- my_upper[elem_names]
  my_protected <- my_protected[elem_names]
  
  names(my_lower) <- names(my_upper) <- names(my_protected) <- elem_names
  
  my_lower[is.na(my_lower) | my_protected == TRUE] <- 0.999999
  my_upper[is.na(my_upper) | my_protected == TRUE] <- 1.000001
  
  # XXX this used to be &, but we want now all 1s so it chan change
  initial <- (my_lower | my_upper ) * 1.0
  
  #initial[initial < 1] <- 0.00000001
  
  opt <- optim(initial, my_fun, method = "L-BFGS-B",
               lower = my_lower, upper = my_upper,
               control = list(pgtol = 0.0001))
  
  opt$par <- opt$par * !dplyr::near(opt$par, 0, 0.0000001)
  
  res <- reshape2::melt(as.list(opt$par))
  
  setDT(res)
  
  setnames(res, c("value", "L1"), c("adj", "measuredElementSuaFbs"))
  
  # Round to keep things actually different from 1.
  res[, adj := ifelse(dplyr::near(adj, 1, tol = 0.0001), 1, adj)]
  
  return(res)
}


balance_optimization <- function(d) {
  myres_optim <- do_optim(d)
  
  res <- myres_optim[d[, list(measuredElementSuaFbs, Value)],
                     on = c('measuredElementSuaFbs')]
  
  res[, adj := adj * Value]
  
  res[dplyr::near(adj, 0) | dplyr::near(adj, 1), adj := NA_real_]
  
  return(res$adj)
}

################### / optim stuff ###########################


balance_proportional <- function(data) {

  x <- copy(data)

  x <-
    x[
      Protected == FALSE &
        !measuredElementSuaFbs %in%
        c("production", "imports", "exports", "stockChange", "foodManufacturing"),
      list(measuredElementSuaFbs, measuredItemSuaFbs, Value,
           mov_share, imbalance, min_threshold, max_threshold)
    ]

  # Recalculate mov_share, as we are excluding protected values
  x[, mov_share := mov_share / sum(mov_share, na.rm = TRUE)]

  x[is.na(Value), Value := 0]

  x[, adjusted_value := ifelse(Value + mov_share * imbalance >= 0, Value + mov_share * imbalance, 0)]

  x[adjusted_value > Value & adjusted_value > max_threshold, adjusted_value := max_threshold]

  x[adjusted_value < Value & adjusted_value < min_threshold, adjusted_value := min_threshold]

  x[, Value := NULL]

  x <-
    merge(
      data,
      x[, list(measuredElementSuaFbs, measuredItemSuaFbs, adjusted_value)],
      by = c("measuredElementSuaFbs", "measuredItemSuaFbs"),
      all.x = TRUE
    )

  return(as.numeric(x$adjusted_value))
}


# rollavg() is a rolling average function that uses computed averages
# to generate new values if there are missing values (and FOCB/LOCF).
# I.e.:
# vec <- c(NA, 2, 3, 2.5, 4, 3, NA, NA, NA)
#
#> RcppRoll::roll_mean(myvec, 3, fill = 'extend', align = 'right')
#[1]       NA       NA       NA 2.500000 3.166667 3.166667       NA       NA       NA 
#
#> rollavg(myvec)
#[1] 2.000000 2.000000 3.000000 2.500000 4.000000 3.000000 3.166667 3.388889 3.185185

rollavg <- function(x, order = 3) {
  # order should be > 2
  stopifnot(order >= 3)
  
  non_missing <- sum(!is.na(x))
  
  # For cases that have just two non-missing observations
  order <- ifelse(order > 2 & non_missing == 2, 2, order)
  
  if (non_missing == 1) {
    x[is.na(x)] <- na.omit(x)[1]
  } else if (non_missing >= order) {
    n <- 1
    while(any(is.na(x)) & n <= 10) { # 10 is max tries
      movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
      movav <- data.table::shift(movav)
      x[is.na(x)] <- movav[is.na(x)]
      n <- n + 1
    }
    
    x <- zoo::na.fill(x, 'extend')
  }
  
  return(x)
}


newBalancing <- function(data, tree, utilizationTable, Utilization_Table, zeroWeight) {
  
  # Contains a variable that indicates whether stocks changed
  data[, change_stocks := NA_integer_]
  
  if (nrow(tree) > 0) {
    #level <- findProcessingLevel(tree, from = p$parentVar,
    #                             to = p$childVar, aupusParam = p)
    #
    ## TODO check if there are multiple levels and how to handle that
    #
    #data <- merge(data, level, by = 'measuredItemSuaFbs', all.x = TRUE)

    #lev_issues <- list()

    #for (ii in unique(tree$measuredItemParentCPC)) {
    #  lev_parent <- unique(data[measuredItemSuaFbs == ii]$processingLevel)
    #  if (length(lev_parent) > 0) {
    #    children <- unique(data[measuredItemSuaFbs %in% unique(tree[measuredItemParentCPC == ii]$measuredItemChildCPC), .(geographicAreaM49, measuredItemChildCPC = measuredItemSuaFbs, processingLevel)])
    #    children[, measuredItemParentCPC := ii]
    #    setcolorder(children, c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC", "processingLevel"))
    #    if (nrow(children[processingLevel == lev_parent]) > 0) {
    #      lev_issues[[length(lev_issues) + 1]] <- children[processingLevel == lev_parent]
    #    }
    #  }
    #}
    
    # TODO: balance also these: data[is.na(processingLevel)]
    # XXX: correct?
    #data[is.na(processingLevel), processingLevel := 0]
    
    # XXX: na.rm = TRUE?
    #data[,
    #  min_processingLevel := min(processingLevel),
    #  by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
    #]
    
    
    # XXX define "food residual" those items for which the only utilization
    # is food. Food processing can also be another possible utilization and
    # if there is that does not change its food-residualness, this is why
    # the check is done before assigning food processing.
    # NW: I have commented the conditions out
    # Now it only checks to make sure that food is the only utilization
    # We noticed that some of the food items were missing from the utilization table
    # This is still different from the previous approach of assigning all of the imbalance to 
    # food when "none of the other utilizations are activable"
    data[,
      food_resid :=
        # It's a food item & ...
        (measuredItemSuaFbs %in% Utilization_Table[food_item == 'X', cpc_code] |
        # food exists & ...
        # !is.na(Value[measuredElementSuaFbs == 'food']) &
        Food_Median > 0 & !is.na(Food_Median)) &
        # ... is the only utilization
        all(is.na(Value[!(measuredElementSuaFbs %in%
                            c('food', 'production', 'imports', 'exports', 'stockChange','foodManufacturing'))])),
      by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
    ]
    
    #Checking if the commodity has past value before assigning the residual imbalance at the end
    #of the balancing procees
    data[,
      `:=`(
        feed_resid =
          # It's a feed item or have past value & ...
          #(measuredItemSuaFbs %in% Utilization_Table[feed == 'X', cpc_code] |
          (Feed_Median > 0 & !is.na(Feed_Median)) &
          #feed is the only utilization....
          all(is.na(Value[!(measuredElementSuaFbs %chin%
                               c('feed', 'production', 'imports', 'exports', 'stockChange','foodManufacturing'))])),
        # It's a industrial item or have past value & ...
        industrial_resid = Industrial_Median > 0 & !is.na(Industrial_Median)),
      by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
    ] 
     
    # Commodities with missing processing level => they are not in the tree.
    # This should be investigated, they will be balanced separately.
    
    #for (lev in rev(unique(level$processingLevel))) {
      
      #data <- data[processingLevel == lev]

      #if (nrow(data[measuredItemSuaFbs == "22110.02"]) > 0) {
      #  browser()
      #}

      # This is used only to check whether production needs to be generated,
      # hence no stocks are considered.
      
      data[,
        supply :=
          sum(
            Value[measuredElementSuaFbs %chin% c('production', 'imports')],
            - Value[measuredElementSuaFbs %chin% c('exports')],
            na.rm = TRUE
          ),
        # year is quite unnecessary, but let's use it in any case
        by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
      ]
      
      
      # When production needs to be created
      
      data[
        Protected == FALSE &
          # Only primary
          measuredItemSuaFbs %chin% Utilization_Table[primary_item == "X"]$cpc_code &
          measuredElementSuaFbs == 'production' &
          supply < 0 &
          stockable == FALSE,
        `:=`(
          Value = ifelse(is.na(Value), 0, Value) - supply,
          flagObservationStatus = "E",
          flagMethod = "c"
        )
      ]
      
      calculateImbalance(data)

      # Try to assign the maximum of imbalance to stocks

      # NOTE: in the conditions below, 2 was 0.2, indicating that no more than
      # 20% should go to stocks. Now, the condition was relaxed a lot (200%)

      data <-
        merge(
          data,
          all_opening_stocks[,
            .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
              timePointYears, opening_stocks = Value)
          ],
          by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"),
          all.x = TRUE
        )
      
      data[,
        Value_0 := ifelse(is.na(Value), 0, Value)
      ][
        Protected == FALSE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "stockChange" & stockable == TRUE,
        change_stocks :=
          # The numbers indicate the case. Assignmnet (value and flags) will be done below
          case_when(
            # case 1: we don't want stocks to change sign.
            sign(Value_0) * sign(Value_0 + imbalance) == -1                                                   ~ 1L,
            # case 2: if value + imbalance takes LESS than opening stock, take all from stocks
            Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) <= opening_stocks            ~ 2L,
            # case 3: if value + imbalance takes MORE than opening stock, take max opening stocks
            Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) > opening_stocks             ~ 3L,
            # case 4: if value + imbalance send LESS than 200% of supply, send all
            Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks <= supply * 2)  ~ 4L,
            # case 5: if value + imbalance send MORE than 200% of supply, send 200% of supply
            Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks > supply * 2)   ~ 5L
          )
      ]

      data[change_stocks == 1L, Value := 0]
      data[change_stocks == 2L, Value := Value_0 + imbalance]
      data[change_stocks == 3L, Value := - opening_stocks]
      data[change_stocks == 4L, Value := Value_0 + imbalance]
      data[change_stocks == 5L, Value := ifelse(opening_stocks < supply * 2, supply * 2 - opening_stocks, 0)]

      data[change_stocks %in% 1L:5L, `:=`(flagObservationStatus = "E", flagMethod = "s")]
        
      data[, Value_0 := NULL]

      data[, opening_stocks := NULL]
      
      # Recalculate imbalance
      calculateImbalance(data)

      # Assign imbalance to food if food "only" (not "residual") item
      data[
        Protected == FALSE & food_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "food",
        `:=`(
          Value = ifelse(is.na(Value) & imbalance>0,imbalance,ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
          flagObservationStatus = "E",
          flagMethod = "h"
        )
      ]
          
      for (j in 1:10) {

        # Recalculate imbalance
        calculateImbalance(data)
        
        ###    data[,
        ###      mov_share_rebased := mov_share / sum(mov_share[Protected == FALSE], na.rm = TRUE),
        ###      by = list(geographicAreaM49, timePointYears, measuredItemSuaFbs)
        ###    ]
        
        ###    # Assign remaining imbalance proportionally, using rebased moving shares.
        ###    data[
        ###      Protected == FALSE & food_resid == FALSE & outside(imbalance, -100, 100) & !(measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stockChange')),
        ###      `:=`(
        ###        Value = Value + mov_share_rebased * imbalance,
        ###        flagObservationStatus = "E",
        ###        flagMethod = "u"
        ###      )
        ###    ]
        
        
        if (nrow(data[outside(imbalance, -1, 1)]) > 0) {
          
          data_level_no_imbalance <- data[data.table::between(imbalance, -1, 1)]
          data_level_with_imbalance <- data[outside(imbalance, -1, 1)]
          
          levels_to_optimize <- unique(data_level_with_imbalance[, .(geographicAreaM49, timePointYears, measuredItemSuaFbs)])
          
          D_adj <- list()
          
          for (i in 1:nrow(levels_to_optimize)) {
            #print(i) ; flush.console()
            # FIXME: remove this (ugly) global assignment
            x <<- data_level_with_imbalance[levels_to_optimize[i], on = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs')]
            
            if (BALANCING_METHOD == "proportional") {
              x[, adjusted_value := balance_proportional(x)]
            } else if (BALANCING_METHOD == "optimization") {
              x[, adjusted_value := balance_optimization(x)]
            } else {
              stop("Invalid balancing method.")
            }
            
            x[
              !is.na(adjusted_value) & adjusted_value != Value,
              `:=`(
                Value = adjusted_value,
                flagObservationStatus = "E",
                flagMethod = "-"
              )
            ]
            
            D_adj[[i]] <- x
            
            rm(x)
          }
          
          data_level_with_imbalance <- rbindlist(D_adj)
          
          data_level_with_imbalance[, adjusted_value := NULL]
          
          data <- rbind(data_level_with_imbalance, data_level_no_imbalance)
          
        }
      }
      
      # At this point the imbalance (in the best case scenario) should be zero,
      # the following re-calculation is useful only for debugging
      
      calculateImbalance(data)

      # Assign imbalance to food if food "only" (not "residual") item

      data[
        Protected == FALSE & food_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "food",
        `:=`(
          Value = ifelse(is.na(Value) & imbalance>0,imbalance,ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
          flagObservationStatus = "E",
          flagMethod = "h"
        )
      ]
      
      calculateImbalance(data)
      
      # Assign the residual imbalance to industrial if the conditions are met
      data[
        Protected == FALSE & industrial_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "industrial",
        `:=`(
          Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, Value)),
          flagObservationStatus = "E",
          flagMethod = "b"
        )
      ]

      calculateImbalance(data)
      
      # Assign the residual imbalance to feed if the conditions are met
      data[
        Protected == FALSE & feed_resid == TRUE & outside(imbalance, -1, 1) & measuredElementSuaFbs == "feed",
        `:=`(
          Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, Value)),
          flagObservationStatus = "E",
          flagMethod = "b"
        )
      ]
     
      calculateImbalance(data)
      
      
      data[, c("supply", "utilizations", "imbalance", "mov_share_rebased") := NULL]
      
      #data <- rbind(data[processingLevel != lev], data)
      
    #}
    
    #data[, c("processingLevel", "min_processingLevel") := NULL]
  } else {
    primaryEl <- c()
  }
  
  return(list(balanced = data))
  #return(list(balanced = data, issues = lev_issues))
}




############################## / FUNCTIONS ##################################


dbg_print("end functions")






#####################################  TREE #################################

dbg_print("download tree")

tree <- getCommodityTreeNewMethod(COUNTRY, YEARS)

stopifnot(nrow(tree) > 0)

tree <- tree[geographicAreaM49 %chin% COUNTRY]


# The `tree_exceptions` will npo be checked by validateTree()

# Exception: high share conmfirmed by official data
tree_exceptions <- tree[geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01"]

if (nrow(tree_exceptions) > 0) {
  tree <- tree[!(geographicAreaM49 == "392" & measuredItemParentCPC == "0141" & measuredItemChildCPC == "23995.01")]
}

validateTree(tree)

if (nrow(tree_exceptions) > 0) {
  tree <- rbind(tree, tree_exceptions)
  rm(tree_exceptions)
}

## NA ExtractionRates are recorded in the sws dataset as 0
## for the standardization, we nee them to be treated as NA
## therefore here we are re-changing it

tree[Value == 0, Value := NA]

#proc_level_exceptions <- ReadDatatable("processing_level_exceptions")
#
#if (nrow(proc_level_exceptions) > 0) {
#  setnames(proc_level_exceptions, c("m49_code", "parent", "child"),
#           c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC"))
#
#  tree <-
#    tree[!proc_level_exceptions[is.na(level)],
#         on = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")]
#
#  proc_level_exceptions <- proc_level_exceptions[!is.na(level)]
#}

tree_to_send <- tree[is.na(Value) & measuredElementSuaFbs=="extractionRate"]

if (FILL_EXTRACTION_RATES == TRUE) {

  expanded_tree <-
    merge(
      data.table(
        geographicAreaM49 = unique(tree$geographicAreaM49),
        timePointYears = sort(unique(tree$timePointYears))
      ),
      unique(tree[, .(geographicAreaM49, measuredElementSuaFbs,
                      measuredItemParentCPC, measuredItemChildCPC)]),
      by = "geographicAreaM49",
      all = TRUE,
      allow.cartesian = TRUE
    )

  tree <- tree[expanded_tree, on = colnames(expanded_tree)]

  # flags for carry forward/backward
  tree[is.na(Value), c("flagObservationStatus", "flagMethod") := list("E", "t")]

  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = -Inf
    ]

  tree <-
    tree[!is.na(Value)][
      tree,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemParentCPC", "measuredItemChildCPC",
             "timePointYears"),
      roll = Inf
  ]

  # keep orig flags
  tree[, flagObservationStatus := i.i.flagObservationStatus]
  tree[, flagMethod := i.i.flagMethod]

  tree[, names(tree)[grep("^i\\.", names(tree))] := NULL]
}

saveRDS(
  tree[
    !is.na(Value) & measuredElementSuaFbs == "extractionRate",
    -grepl("measuredElementSuaFbs", names(tree)),
    with = FALSE
  ],
  file.path(R_SWS_SHARE_PATH, "FBS_validation", COUNTRY, "tree.rds")
)

tree_to_send <-
  tree_to_send %>% 
  dplyr::anti_join(tree[is.na(Value) & measuredElementSuaFbs == "extractionRate"], by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears", "Value", "flagObservationStatus", "flagMethod")) %>%
  dplyr::select(-Value) %>%
  dplyr::left_join(tree, by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears", "flagObservationStatus", "flagMethod")) %>%
  setDT()

tree_to_send <-
  tree_to_send[,
    .(geographicAreaM49, measuredElementSuaFbs, measuredItemParentCPC,
      measuredItemChildCPC, timePointYears, Value, flagObservationStatus, flagMethod)]

setnames(
  tree_to_send,
  c("measuredItemParentCPC", "measuredItemChildCPC"),
  c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree")
)

tree_to_send <-
  nameData("suafbs", "ess_fbs_commodity_tree2", tree_to_send, except = c('measuredElementSuaFbs', 'timePointYears'))

tree_to_send[,
  `:=`(
    measuredItemParentCPC_tree = paste0("'", measuredItemParentCPC_tree),
    measuredItemChildCPC_tree = paste0("'", measuredItemChildCPC_tree))
]

tmp_file_name_extr <- tempfile(pattern = paste0("FILLED_ER_", COUNTRY, "_"), fileext = '.csv')

write.csv(tree_to_send, tmp_file_name_extr)

# XXX remove NAs
tree <- tree[!is.na(Value)]

### Update tree by setting some edges to "F"
#FPCommodities <- c("01499.06", "01921.01")
#
## These commodities are forwards processed instead of backwards processed:
##        code             description type
## 3: 01499.06      Kapokseed in shell CRNP
## 4: 01921.01   Seed cotton, unginned CRPR
#
#tree[, target := ifelse(measuredItemParentCPC %in% FPCommodities, "F", "B")]

uniqueLevels <- unique(tree[, list(geographicAreaM49, timePointYears)])

levels <- list()

treeLevels <- list()

for (i in seq_len(nrow(uniqueLevels))) {
  filter <- uniqueLevels[i, ]
  
  treeCurrent <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  
  levels <- findProcessingLevel(treeCurrent, "measuredItemParentCPC", "measuredItemChildCPC")
  setnames(levels, "temp", "measuredItemParentCPC")
  
  treeLevels[[i]] <- merge(treeCurrent, levels, by = c("measuredItemParentCPC"), all.x = TRUE)
}

tree <- rbindlist(treeLevels)

#if (nrow(proc_level_exceptions) > 0) {
#  tree <- merge(tree, proc_level_exceptions, by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC"), all.x = TRUE)
#  tree[!is.na(level), processingLevel := level]
#  tree[, level := NULL]
#}

# XXX there are no different process levels, but check it
tree[,
  processingLevel := max(processingLevel, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears",
         "measuredElementSuaFbs", "measuredItemChildCPC")
]



# XXX Check if this one is still good or it can be obtained within the dataset
processed_item_datatable <- ReadDatatable("processed_item")
processedCPC <- processed_item_datatable[, measured_item_cpc]


# XXX what is this for?
itemMap <- GetCodeList(domain = "agriculture", dataset = "aproduction", "measuredItemCPC")
itemMap <- itemMap[, list(measuredItemSuaFbs = code, type)]

##################################### / TREE ################################






coproduct_table <- ReadDatatable('zeroweight_coproducts')





############################ POPULATION #####################################

key <-
  DatasetKey(
    domain = "population",
    dataset = "population_unpd",
    dimensions =
      list(
        geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
        measuredElementSuaFbs = Dimension(name = "measuredElement", keys = "511"), # 511 = Total population
        timePointYears = Dimension(name = "timePointYears", keys = as.character(2000:2017))
      )
  )

dbg_print("download population")

popSWS <- GetData(key)

stopifnot(nrow(popSWS) > 0)

popSWS[geographicAreaM49 == "156", geographicAreaM49 := "1248"]


############################ / POPULATION ##################################




############################################################### DATA #######




# 5510 Production[t]
# 5610 Import Quantity [t]
# 5071 Stock Variation [t]
# 5023 Export Quantity [t]
# 5910 Loss [t]
# 5016 Industrial uses [t]
# 5165 Feed [t]
# 5520 Seed [t]
# 5525 Tourist Consumption [t]
# 5164 Residual other uses [t]
# 5141 Food [t]
# 664 Food Supply (/capita/day) [Kcal]

elemKeys <- c("5510", "5610", "5071", "5113", "5023", "5910", "5016",
              "5165", "5520", "5525", "5164", "5166", "5141")

itemKeys <- GetCodeList(domain = "suafbs", dataset = "sua_unbalanced", "measuredItemFbsSua")
itemKeys <- itemKeys$code


# NOTE: the definition of "food_resid" changed (see inside newBalancing)
# (so, the tables below are not used anymore)

#food_classification_country_specific <-
#  ReadDatatable("food_classification_country_specific",
#                where = paste0("geographic_area_m49 IN ('", COUNTRY, "')"))
#
#food_classification_country_specific <-
#  food_classification_country_specific[geographic_area_m49 == COUNTRY]
#
#food_only_items <- food_classification_country_specific[food_classification == 'Food Residual', measured_item_cpc]


Utilization_Table <- ReadDatatable("utilization_table_2018")

stockable_items <- Utilization_Table[stock == 'X', cpc_code]

zeroWeight <- ReadDatatable("zero_weight")[, item_code]

flagValidTable <- ReadDatatable("valid_flags")

# XXX: we decided to unprotect E,e (replaced outliers)
flagValidTable[flagObservationStatus == 'E' & flagMethod == 'e', Protected := FALSE]
# XXX: we need to unprotect I,c because it was being assigned
# to imputation of production of derived which is NOT protected.
# This will need to change in the next exercise.
flagValidTable[flagObservationStatus == 'I' & flagMethod == 'c', Protected := FALSE]

# utilizationTable=ReadDatatable("utilization_table")
utilizationTable <-
  ReadDatatable(
    "utilization_table_percent",
    where = paste0("area_code IN (", paste(shQuote(COUNTRY, type = "sh"), collapse = ", "), ")")
  )

setnames(utilizationTable, colnames(utilizationTable), c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs",
                                                         "percent", "rank", "rankInv"))
nutrientData <-
  getNutritiveFactors(
    measuredElement = "1001", # "1001" is code for calories per 100 grams
    timePointYears = as.character(2014:2017),
    geographicAreaM49 = COUNTRY
  )



if (CheckDebug()) {
  key <-
    DatasetKey(
      domain = "suafbs",
      dataset = "sua_unbalanced",
      dimensions =
        list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys),
          measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
          timePointYears = Dimension(name = "timePointYears", keys = YEARS)
        )
    )
} else {
  key <- swsContext.datasets[[2]]
  
  key@dimensions$timePointYears@keys <- YEARS
  key@dimensions$measuredItemFbsSua@keys <- itemKeys
  key@dimensions$measuredElementSuaFbs@keys <- elemKeys
  key@dimensions$geographicAreaM49@keys <- COUNTRY
}

#       # If need to read from session:
#       GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = 'edfe83e1-47d8-499d-88fa-35e6a9dca850')
#
#       key <- swsContext.datasets[[2]]
#
#       key@dimensions$timePointYears@keys <- YEARS
#       key@dimensions$measuredItemFbsSua@keys <- itemKeys
#       key@dimensions$measuredElementSuaFbs@keys <- elemKeys
#       key@dimensions$geographicAreaM49@keys <- COUNTRY


dbg_print("download data")


data <- GetData(key)
# LOAD
# data <- readRDS(paste0('c:/Users/mongeau.FAODOMAIN/tmp/new_balancing/data_', COUNTRY, '.rds'))



#################### FODDER CROPS ##########################################

# Some of these items may be missing in reference files,
# thus we carry forward the last observation.
fodder_crops_items <-
  tibble::tribble(
    ~description, ~code,
    "Maize for forage", "01911",
    "Alfalfa for forage", "01912",
    "Sorghum for forage", "01919.01",
    "Rye grass for forage", "01919.02",
    "Other grasses for forage", "01919.91",
    "Clover for forage", "01919.03",
    "Other oilseeds for forage", "01919.94",
    "Other legumes for  forage", "01919.92",
    "Cabbage for fodder", "01919.04",
    "Mixed grass and legumes for forage", "01919.93",
    "Turnips for fodder", "01919.05",
    "Beets for fodder", "01919.06",
    "Carrots for fodder", "01919.07",
    "Swedes for fodder", "01919.08",
    "Other forage products, nes", "01919.96",
    "Other forage crops, nes", "01919.95",
    "Hay for forage, from legumes", "01919.10",
    "Hay for forage, from grasses", "01919.09",
    "Hay for forage, from other crops nes", "01919.11"
  )


fodder_crops_availab <-
  data[
    measuredItemFbsSua %in% fodder_crops_items$code &
      measuredElementSuaFbs == "5510"
  ]

if (nrow(fodder_crops_availab) > 0) {

  fodder_crops_complete <-
    CJ(
      geographicAreaM49 = unique(fodder_crops_availab$geographicAreaM49),
      measuredElementSuaFbs = "5510",
      timePointYears = unique(data$timePointYears),
      measuredItemFbsSua = unique(fodder_crops_availab$measuredItemFbsSua)
    )

  fodder_crops_complete <-
    fodder_crops_complete[order(geographicAreaM49, measuredItemFbsSua, timePointYears)]

  fodder_crops <-
    merge(
      fodder_crops_complete,
      fodder_crops_availab[, .(geographicAreaM49, measuredElementSuaFbs,
                               measuredItemFbsSua, timePointYears, Value)],
      by = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears"),
      all.x = TRUE
  )

  fodder_crops[,
    Value := zoo::na.locf(Value),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ]

  fodder_crops_new <-
    fodder_crops[
      !fodder_crops_availab,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears")
    ]


  if (nrow(fodder_crops_new) > 0) {

    fodder_crops_new[, `:=`(flagObservationStatus = "E", flagMethod = "t")]

    data <- rbind(data, fodder_crops_new)

  }
}

#################### / FODDER CROPS ########################################




# TODO: add `where` clause to subset for countries
opening_stocks_2014 <-
  ReadDatatable(
    "opening_stocks_2014",
    where = paste0("m49_code IN (",
                   paste(shQuote(COUNTRY, type = "sh"), collapse = ", "), ")")
  )

stopifnot(nrow(opening_stocks_2014) > 0)

opening_stocks_2014[opening_stocks < 0, opening_stocks := 0]

opening_stocks_2014 <-
  opening_stocks_2014[
    m49_code %in% COUNTRY,
    .(
      geographicAreaM49 = m49_code,
      measuredItemFbsSua = cpc_code,
      timePointYears = "2014",
      Value_cumulated = opening_stocks
    )
  ]

original_opening_stocks <- data[measuredElementSuaFbs == 5113]

original_opening_stocks <-
  flagValidTable[
    original_opening_stocks,
    on = c("flagObservationStatus", "flagMethod")
  ][,
    Valid := NULL
  ]


# Remove protected in 2014 from cumulated
opening_stocks_2014 <-
  opening_stocks_2014[
    !original_opening_stocks[timePointYears == "2014" & Protected == TRUE, .(geographicAreaM49, measuredItemFbsSua)],
    on = c("geographicAreaM49", "measuredItemFbsSua")
  ]


all_opening_stocks <-
  merge(
    original_opening_stocks,
    opening_stocks_2014,
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    all = TRUE
  )


all_opening_stocks[
  !Protected %in% TRUE & is.na(Value) & !is.na(Value_cumulated),
  `:=`(
    Value = Value_cumulated,
    flagObservationStatus = "I",
    flagMethod = "i",
    # We protect these, in any case, because they should not
    # be overwritten, even if not (semi) official or expert
    Protected = TRUE,
    measuredElementSuaFbs = "5113",
    timePointYears = "2014"
  )
][,
  Value_cumulated := NULL
]


# Now, for all remaining stockable items, we create opening
# stocks in 2014 as 20% of supply in 2013

remaining_opening_stocks <-
  data[
    timePointYears == "2013" &
      measuredItemFbsSua %chin%
        setdiff(
          stockable_items,
          all_opening_stocks$measuredItemFbsSua
        )
  ][,
    .(
      opening_20 =
        sum(
          Value[measuredElementSuaFbs %chin% c("5510", "5610")],
          - Value[measuredElementSuaFbs == "5910"],
          na.rm = TRUE
        ) * 0.2,
      timePointYears = "2014"
    ),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ]

remaining_opening_stocks[opening_20 < 0, opening_20 := 0]

all_opening_stocks <-
  merge(
    all_opening_stocks,
    remaining_opening_stocks,
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    all = TRUE
  )

all_opening_stocks[
  !Protected %in% TRUE & is.na(Value) & !is.na(opening_20),
  `:=`(
    Value = opening_20,
    flagObservationStatus = "I",
    flagMethod = "i",
    # We protect these, in any case, because they should not
    # be overwritten, even if not (semi) official or expert
    Protected = TRUE
  )
][,
  opening_20 := NULL
]


data <- merge(data, flagValidTable, by = c("flagObservationStatus", "flagMethod"), all.x = TRUE)

data[flagObservationStatus %in% c("", "T"), `:=`(Official = TRUE, Protected = TRUE)]
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# We remove "5113" (opening stocks) as will be stored in separate table.
data <- data[measuredElementSuaFbs != "5113"]

dbg_print("elementToCodeNames")

data <-
  elementCodesToNames(
    data,
    itemCol = "measuredItemFbsSua",
    elementCol = "measuredElementSuaFbs"
  )


# XXX: there are some NAs here, but probably there shouldn't
data <- data[!is.na(measuredElementSuaFbs)]

setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")

dbg_print("convert sugar")

# XXX
##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data <- convertSugarCodes(data)

## XXX: check for max processing level
#level <- findProcessingLevel(tree, from = p$parentVar,
#                            to = p$childVar, aupusParam = p)


#primaryEl <- level[processingLevel == 0, get(p$itemVar)]

## XXX what is this?
#data[
#  !(get(p$protected) == TRUE | (flagObservationStatus == "I" & flagMethod %in% c("i", "e"))) &
#    get(p$elementVar) == p$productionCode &
#    !(get(p$itemVar) %chin% primaryEl),
#  Value_xxx := NA
#]


#data[,
#  availability :=
#    sum(
#      Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
#      - Value[get(p$elementVar) %in% p$exportCode],
#      na.rm = TRUE
#    ),
#  by = c(p$mergeKey)
#]




########## Remove feed if new element and negative imbalance is huge

# Feed requires country-specific reference files, which are being updated.
# Until these get reviewed, the feed module will generate feed items in
# some countries, where it is not required/needed/appropriate. Below, we
# remove the NEW feed item if the NEGATIVE imbalance obtained by including
# it is more than 50% of supply (e.g., -72%).

new_feed <-
  data[
    measuredElementSuaFbs == "feed",
    .(
      pre = sum(!is.na(Value[timePointYears <= 2013])),
      post = sum(!is.na(Value[timePointYears >= 2014]))
    ),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
  ][
    pre == 0 & post > 0
  ][,
    c("pre", "post") := NULL
  ]

if (nrow(new_feed) > 0) {

  prev_data_avg <-
    data[
      new_feed, on = c("geographicAreaM49", "measuredItemSuaFbs")
    ][
      timePointYears >= 2010 & timePointYears <= 2013
    ][
      order(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears)
    ][,
      Value := na.fill_(Value),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ][,
      .(Value = sum(Value) / sum(!is.na(Value)), timePointYears = 0),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ]

  calculateImbalance(prev_data_avg, supply_subtract = c("exports", "stock_change"))

  prev_processed_avg <-
    prev_data_avg[
      supply > 0 &
        utilizations > 0 &
        measuredElementSuaFbs == "foodmanufacturing" &
        !is.na(Value),
      .(geographicAreaM49, measuredItemSuaFbs, proc_ratio = Value / supply)
    ]


  new_data_avg <-
    data[
      new_feed, on = c("geographicAreaM49", "measuredItemSuaFbs")
    ][
      measuredElementSuaFbs != "foodmanufacturing" & timePointYears >= 2014
    ][
      order(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears)
    ][,
      Value := na.fill_(Value),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ][,
      .(Value = sum(Value) / sum(!is.na(Value)), timePointYears = 1),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ]

  calculateImbalance(
    new_data_avg,
    keep_utilizations = FALSE,
    supply_subtract = c("exports", "stock_change")
  )

  new_data_supply <-
    unique(new_data_avg[, .(geographicAreaM49, measuredItemSuaFbs, timePointYears, supply)])

  new_data_proc <-
    merge(
      new_data_supply,
      prev_processed_avg,
      by = c("geographicAreaM49", "measuredItemSuaFbs")
    )

  new_data_proc <-
    new_data_proc[
      supply > 0,
      .(geographicAreaM49, measuredItemSuaFbs,
        measuredElementSuaFbs = "foodmanufacturing",
        Value = supply * proc_ratio, timePointYears = 1)
    ]

  new_data_avg[, c("supply", "imbalance") := NULL]

  new_data_avg <-
    rbind(
      new_data_avg,
      new_data_proc
    )

  calculateImbalance(new_data_avg, supply_subtract = c("exports", "stock_change"))


  new_feed_to_remove <-
    unique(new_data_avg[imbalance / supply <= -0.3, .(geographicAreaM49, measuredItemSuaFbs)])

  new_feed_dubious <-
    unique(new_data_avg[imbalance / supply > -0.3 & imbalance / supply <= -0.05,
           .(geographicAreaM49, measuredItemSuaFbs, x = imbalance / supply)][order(x)][, x := NULL])

  new_feed_to_remove[, measuredElementSuaFbs := "feed"]

  data <-
    data[
      !new_feed_to_remove,
      on = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ]

  if (nrow(new_feed_to_remove) > 0) {
    msg_new_feed_remove <- paste(new_feed_to_remove$measuredItemSuaFbs, collapse = ", ")
  } else {
    msg_new_feed_remove <- "No NEW feed removed"
  }

  if (nrow(new_feed_dubious) > 0) {
    msg_new_feed_dubious <- paste(new_feed_dubious$measuredItemSuaFbs, collapse = ", ")
  } else {
    msg_new_feed_dubious <- "No other NEW items should be removed"
  }

}


########## / Remove feed if new element and negative imbalance is huge


treeRestricted <- tree[, .(measuredItemParentCPC, measuredItemChildCPC, processingLevel)]
treeRestricted <- unique(treeRestricted[order(measuredItemChildCPC)])

primaryInvolved <- faoswsProduction::getPrimary(processedCPC, treeRestricted, p)

dbg_print("primary involved descendents")

# XXX: check here, 0111 is in results
primaryInvolvedDescendents <-
  getChildren(
    commodityTree = treeRestricted,
    parentColname = "measuredItemParentCPC",
    childColname = "measuredItemChildCPC",
    topNodes = primaryInvolved
  )


# stocks need to be generated for those items for
# which "opening stocks" are available

items_to_generate_stocks <-
  unique(all_opening_stocks$measuredItemFbsSua)

stock <-
  CJ(
    measuredItemSuaFbs    = items_to_generate_stocks,
    measuredElementSuaFbs = 'stock_change',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )

# rbind with anti_join
data <-
  rbind(
    data,
    stock[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )

# XXX what is primaryInvolvedDescendents ?????????
#deriv <- CJ(measuredItemSuaFbs = primaryInvolvedDescendents, measuredElementSuaFbs = 'production', geographicAreaM49 = unique(data$geographicAreaM49), timePointYears = unique(data$timePointYears))
deriv <-
  CJ(
    measuredItemSuaFbs    = unique(tree$measuredItemChildCPC),
    measuredElementSuaFbs = 'production',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )

# rbind with anti_join
data <-
  rbind(
    data,
    deriv[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )



data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# This function will recalculate opening stocks from the first
# observation. Always.
update_opening_stocks <- function(x) {
  x <- x[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]

  groups <- unique(x[, .(geographicAreaM49, measuredItemSuaFbs)])

  res <- list()

  for (i in seq_len(nrow(groups))) {
    z <- x[groups[i], on = c("geographicAreaM49", "measuredItemSuaFbs")]
    if (nrow(z) > 1) {
      for (j in seq_len(nrow(z))[-1]) {
        # negative delta cannot be more than opening
        if (z$delta[j-1] < 0 & abs(z$delta[j-1]) > z$new_opening[j-1]) {
          z$delta[j-1] <- - z$new_opening[j-1]
        }
        z$new_opening[j] <- z$new_opening[j-1] + z$delta[j-1]
      }
      # negative delta cannot be more than opening
      if (z$delta[j] < 0 & abs(z$delta[j]) > z$new_opening[j]) {
        z$delta[j] <- - z$new_opening[j]
      }
    }
    res[[i]] <- z
  }
  res <- rbindlist(res)
}


data[, stockable := measuredItemSuaFbs %chin% stockable_items]


# XXX: Remove items for which no production, imports or exports exist after 2013.
non_existing_for_imputation <-
  setdiff(
    data[measuredElementSuaFbs %in% c('production', 'imports', 'exports') & timePointYears <= 2013, unique(measuredItemSuaFbs)],
    data[measuredElementSuaFbs %in% c('production', 'imports', 'exports') & timePointYears >= 2014, unique(measuredItemSuaFbs)]
  )

data <- data[!(measuredItemSuaFbs %in% non_existing_for_imputation)]


############################################### Create derived production ###################################

dbg_print("derivation of shareDownUp")

############# ShareDownUp ----------------------------------------

#this table will be used to assign to zeroweight comodities 
#the processed quantities of their coproduct
coproduct_for_sharedownup <- ReadDatatable('zeroweight_coproducts')

stopifnot(nrow(coproduct_for_sharedownup) > 0)

coproduct_for_sharedownup <- coproduct_for_sharedownup[, .(measured_item_child_cpc, branch)]

#setnames(coproduct_for_sharedownup, "measured_item_child_cpc", "measuredItemChildCPC")

# Can't do anything if this information if missing, so remove these cases
coproduct_for_sharedownup <- coproduct_for_sharedownup[!is.na(branch)]

coproduct_for_sharedownup_easy <- coproduct_for_sharedownup [branch != '22242.01 + 22110.04']

 coproduct_for_sharedownup_easy <- coproduct_for_sharedownup[!grepl('\\+|or', branch)]

coproduct_for_sharedownup_plus <- coproduct_for_sharedownup[grepl('\\+', branch)]

coproduct_for_sharedownup_plus <-
  rbind(
    tidyr::separate(coproduct_for_sharedownup_plus, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(coproduct_for_sharedownup_plus, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_table_plus <- unique(coproduct_table_plus)


coproduct_for_sharedownup_or <- coproduct_for_sharedownup[grepl('or', branch)]

coproduct_for_sharedownup_or <-
  rbind(
    #coproduct_table_or,
    tidyr::separate(coproduct_for_sharedownup_or, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *or *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(coproduct_for_sharedownup_or, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *or *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_for_sharedownup_or <- unique(coproduct_for_sharedownup_or)

coproduct_for_sharedownup<-
  rbind(
    coproduct_for_sharedownup_easy,
    coproduct_for_sharedownup_plus,
    coproduct_for_sharedownup_or
  )


#Using the whole tree not by level
ExtrRate <-
  tree[
    !is.na(Value) &
      measuredElementSuaFbs == 'extractionRate'
    ][,
      list(
        measuredItemParentCPC,
        geographicAreaM49,
        measuredItemChildCPC,
        timePointYears,
        extractionRate = Value,
        processingLevel
      )
      ]

# We include utilizations to identify if proceseed if the only utilization
data_tree <-
  data[
    measuredElementSuaFbs %chin%
      c('production', 'imports', 'exports',
        'stock_change', 'foodmanufacturing', 'loss',
        'food', 'industrial', 'feed', 'seed')
  ]


#subset the tree accordingly to parents and child present in the SUA data

ExtrRate <- ExtrRate[measuredItemChildCPC %in% data_tree[,get("measuredItemSuaFbs")]&
                       measuredItemParentCPC %in% data_tree[,get("measuredItemSuaFbs")],]


setnames(data_tree, "measuredItemSuaFbs", "measuredItemParentCPC")

data_tree <-
  merge(
    data_tree,
    ExtrRate,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE,
    all.y = TRUE
  )

data_tree <- as.data.table(data_tree)

# the availability for parent that have one child and only processed as utilization will 
# be entirely assigned to processed for that its unique child even for 2014 onwards
data_tree[,
  availability :=
    sum(
      Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
      - Value[get(p$elementVar) %in% c(p$exportCode, "stock_change")],
      na.rm = TRUE
    ),
  by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
]

# used to chack if a parent has processed as utilization
data_tree[,
  proc_Median :=
    median(
      Value[measuredElementSuaFbs == "foodmanufacturing" & timePointYears %in% 2000:2017],
      na.rm=TRUE
    ),
  by = c(p$parentVar, p$geoVar)
]

#boolean variable taking TRUE if the parent has only processed as utilization 
data_tree[,
  unique_proc :=
    proc_Median > 0 &
      !is.na(proc_Median) &
      # ... is the only utilization
      all(is.na(Value[!(measuredElementSuaFbs %in%
                        c('production', 'imports', 'exports', 'stock_change','foodmanufacturing'))])),
  by = c(p$parentVar, p$geoVar, p$yearVar)
]

data_tree<-
  unique(
    data_tree[,list(measuredItemParentCPC,geographicAreaM49,timePointYears,measuredElementSuaFbs,
                    flagObservationStatus,flagMethod,Value,Official,measuredItemChildCPC,extractionRate,
                    processingLevel,availability,unique_proc)],
    
    by=c("measuredItemParentCPC","geographicAreaM49","timePointYears","measuredElementSuaFbs",
         "flagObservationStatus","flagMethod","Value","Official","measuredItemChildCPC","extractionRate",
         "processingLevel")
  )


#dataset to calculate the number of parent of each child and the number of children of each parent
#including zeroweight commodities
data_count<-unique(
  data_tree[,
            .(geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,timePointYears)], 
  by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC","timePointYears")
)

#Caculate the number of parent of each child
data_count[,
  number_of_parent := .N,
  by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
]

# #calculate the number of children of each parent
#we exclude zeroweight to avoid doublecounting of children (processing)
data_count[measuredItemChildCPC %!in% zeroWeight,
  number_of_children := uniqueN(measuredItemChildCPC),
  by=c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
]

data_tree <- merge(
  data_tree,
  data_count,
  by = c(p$parentVar,p$childVar, p$geoVar, p$yearVar),
  allow.cartesian = TRUE,
  all.x = TRUE
)

#dataset containing the processed quantity of parents
food_proc <- unique(
  data_tree[measuredElementSuaFbs=="foodmanufacturing",
            list(geographicAreaM49,measuredItemParentCPC,timePointYears,Value)],
  by=c("geographicAreaM49","measuredItemParentCPC","timePointYears","Value")
)
setnames(food_proc,"Value","parent_qty_processed")

#avoid recaculation of shareDownUp from 2014 onwards
food_proc[timePointYears>2013,parent_qty_processed:=NA_real_]

data_tree <- merge(
  data_tree,
  food_proc,
  by = c(p$parentVar, p$geoVar, p$yearVar),
  allow.cartesian = TRUE,
  all.x = TRUE
)

data_tree<-
  unique(
    data_tree[,list(geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,
                    timePointYears,extractionRate,parent_qty_processed,processingLevel,
                    number_of_parent,number_of_children,availability,unique_proc)],
    by=c("geographicAreaM49","measuredItemParentCPC","measuredItemChildCPC",
         "timePointYears","extractionRate","parent_qty_processed","processingLevel",
         "number_of_parent","number_of_children")
  )


#dataset containing the production of child commodities
dataprodchild <- data[measuredElementSuaFbs %chin% c('production')]

setnames(dataprodchild, "measuredItemSuaFbs", "measuredItemChildCPC")

dataprodchild<-
  unique(
    dataprodchild[,list(geographicAreaM49,measuredItemChildCPC,
                        timePointYears,Value,flagObservationStatus, flagMethod)],
    by=c("geographicAreaM49","measuredItemChildCPC","timePointYears",
         "Value","flagObservationStatus","flagMethod")
  )
setnames(dataprodchild, "Value", "production_of_child")


#to avoid resestimation based on estimated data (processed and production of child) from 2014 onwards
dataprodchild[timePointYears>2013,production_of_child:=NA_real_]


data_tree<-merge(
  data_tree,
  dataprodchild,
  by=c(p$geoVar,p$childVar,p$yearVar)
)


# ShareDownups for zeroweights are calculated  separately
# to avoid double counting when agregating processed quantities of parent

# dataset containing informations of zeroweight commodities
data_zeroweight <- data_tree[measuredItemChildCPC %in% zeroWeight,]

# import data for coproduct relation
zw_coproduct <-
  coproduct_for_sharedownup[,
    .(zeroweight = measured_item_child_cpc, measuredItemChildCPC = branch)
  ]

#setnames(zw_coproduct,"measuredItemChildCPC","zeroweight")
#
#setnames(zw_coproduct,"branch","measuredItemChildCPC")

zw_coproduct <- unique(zw_coproduct, by=c("measuredItemChildCPC","zeroweight"))

# We subset the zeroweight coproduct reference table by taking only zeroweights and their coproduct
# that are childcommodities in the tree of the country
zw_coproduct <- zw_coproduct[measuredItemChildCPC %in% data_tree[,get("measuredItemChildCPC")]&
                           zeroweight %in% data_tree[,get("measuredItemChildCPC")],]


# Computing information for non zeroweight commodities
data_tree <- data_tree[measuredItemChildCPC %!in% zeroWeight,]

# Quantity of parent destined to the production of the given child (only for child with one parent for the moment)
data_tree[, processed_to_child := ifelse(number_of_parent == 1, production_of_child, NA_real_)]

# if a parent has one child, all the production of the child comes from that parent
data_tree[number_of_children == 1, processed_to_child := parent_qty_processed*extractionRate, processed_to_child]

data_tree[production_of_child == 0, processed_to_child := 0]

# assigning the entired availability to processed for parent having only processed as utilization
data_tree[number_of_children == 1 & unique_proc == TRUE, processed_to_child := availability * extractionRate]


# mirror assignment for imputing processed quantity for multple parent children
# 5 loop is sufficient to deal with all the cases

for(k in 1:5){
  data_tree<-RemainingToProcessedParent(data_tree)
  data_tree<-RemainingProdChildToAssign(data_tree)
}
data_tree<-RemainingToProcessedParent(data_tree)

# proportional allocation of the remaing production of multiple parent children
data_tree[,
  processed_to_child:=
    ifelse(
      number_of_parent > 1 & is.na(processed_to_child),
      (remaining_to_process_child * is.na(processed_to_child) * remaining_processed_parent) / sum((remaining_processed_parent * is.na(processed_to_child)), na.rm = TRUE),
      processed_to_child),
  by = c("geographicAreaM49","measuredItemChildCPC","timePointYears")
]

# Update of remaining production to assing ( should be zero for 2000:2013)
data_tree[,
  parent_already_processed :=
    ifelse(
      is.na(parent_qty_processed),
      parent_qty_processed,
      sum(processed_to_child/extractionRate,na.rm = TRUE)
    ),
  by = c("geographicAreaM49","measuredItemParentCPC","timePointYears")
]

data_tree[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]

data_tree[remaining_processed_parent < 0, remaining_processed_parent := 0]


#Impute processed quantity for 2014 onwards using 3 years average
#(this only to imput shareDownUp)
data_tree <-
  data_tree[
    order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears)
  ][,
    processed_to_child_avg := rollavg(processed_to_child, order = 3),
    by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
  ]

setkey(data_tree, NULL)

data_tree[timePointYears > 2013 & is.na(processed_to_child), processed_to_child := processed_to_child_avg]


#Back to zeroweight cases(we assign to zeroweights the processed quantity of their coproduct(already calculated))

zw_coproduct<-merge(
  data_tree,
  zw_coproduct,
  by=c("measuredItemChildCPC"),
  allow.cartesian = TRUE,
  all.y = TRUE
)

zw_coproduct[,`:=`(measuredItemChildCPC=zeroweight,
                  processed_to_child=processed_to_child/extractionRate)]

zw_coproduct<-zw_coproduct[,list(geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,
                                 timePointYears,processed_to_child)]


data_zeroweight<-merge(
  data_zeroweight,
  zw_coproduct,
  by=c("geographicAreaM49","measuredItemChildCPC","measuredItemParentCPC","timePointYears"),
  all.x = TRUE
)

data_zeroweight<-data_zeroweight[, processed_to_child := processed_to_child*extractionRate]

data_zeroweight<-data_zeroweight[,list(geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,
                                       timePointYears,number_of_parent,parent_qty_processed,production_of_child,
                                       processed_to_child)]

data_tree<-data_tree[,list(geographicAreaM49,measuredItemParentCPC,measuredItemChildCPC,timePointYears,
                           number_of_parent,parent_qty_processed,production_of_child,processed_to_child)]

#combining zeroweight and non zero weight commodities
data_tree <- rbind(data_tree, data_zeroweight)

#calculate ShareDownUp
data_tree[,
  shareDownUp := processed_to_child / sum(processed_to_child, na.rm = TRUE),
  by = c("geographicAreaM49","measuredItemChildCPC","timePointYears")
]

#some corrections...
data_tree[is.na(shareDownUp) & number_of_parent == 1, shareDownUp := 1]
data_tree[(production_of_child==0 | is.na(production_of_child)) & measuredItemChildCPC %!in% zeroWeight &
            timePointYears < 2014, shareDownUp:=0]
data_tree[(parent_qty_processed==0 | is.na(parent_qty_processed)) & timePointYears < 2014, shareDownUp:=0]
data_tree[is.na(shareDownUp), shareDownUp:=0]
data_tree <-
  unique(
    data_tree[,
      list(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears, shareDownUp)
    ],
    by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears", "shareDownUp")
  )

setDT(data_tree)

# / ShareDownUp -------------------------------------------------------



# Tables that will be required by the co-product issue (fix processingShare)


stopifnot(nrow(coproduct_table) > 0)

coproduct_table <- coproduct_table[, .(measured_item_child_cpc, branch)]

setnames(coproduct_table, "measured_item_child_cpc", "measuredItemChildCPC")

# Can't do anything if this information if missing, so remove these cases
coproduct_table <- coproduct_table[!is.na(branch)]

### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
### XXX removing these cases as '22242.01' appears as zero-weight XXXX
### XXX for main product = '22110.04'                             XXXX
### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
coproduct_table <- coproduct_table [branch != '22242.01 + 22110.04']


coproduct_table_easy <- coproduct_table[!grepl('\\+|or', branch)]

coproduct_table_easy <-
  rbind(
    coproduct_table_easy,
    coproduct_table_easy[, .(measuredItemChildCPC = branch, branch)]
  )

coproduct_table_easy <- unique(coproduct_table_easy)

coproduct_table_plus <- coproduct_table[grepl('\\+', branch)]

coproduct_table_plus <-
  rbind(
    coproduct_table_plus,
    tidyr::separate(coproduct_table_plus, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *')[, .(measuredItemChildCPC = main1, branch)],
    tidyr::separate(coproduct_table_plus, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *\\+ *')[, .(measuredItemChildCPC = main2, branch)]
  )

coproduct_table_plus <- unique(coproduct_table_plus)


coproduct_table_or <- coproduct_table[grepl('or', branch)]

coproduct_table_or <-
  rbind(
    coproduct_table_or,
    tidyr::separate(coproduct_table_or, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *or *')[, .(measuredItemChildCPC = main1, branch)],
    tidyr::separate(coproduct_table_or, branch, into = c('main1', 'main2'), remove = FALSE, sep = ' *or *')[, .(measuredItemChildCPC = main2, branch)]
  )

coproduct_table_or <- unique(coproduct_table_or)

# / Tables that will be required by the co-product issue (fix processingShare)


if (file.exists(shareDownUp_file)) {

  SHAREDOWNUP_LOADED <- TRUE

  shareDownUp_previous <- fread(shareDownUp_file, colClasses = c(rep("character", 4), "numeric", "logical"))

  # Check on consistency of shareDownUp
  shareDownUp_invalid <-
    shareDownUp_previous[,
      .(sum_shares = round(sum(shareDownUp)), 3),
      by = c("geographicAreaM49", "timePointYears", "measuredItemChildCPC")
    ][
      !dplyr::near(sum_shares, 1) & !dplyr::near(sum_shares, 0)
    ]

  if (nrow(shareDownUp_invalid) > 0) {

    shareDownUp_invalid <-
      shareDownUp_previous[
        shareDownUp_invalid,
        on = c("geographicAreaM49", "timePointYears", "measuredItemChildCPC")
      ]

    write.csv(
      shareDownUp_invalid,
      file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_INVALID_", COUNTRY, ".csv"))
    )

    if (!CheckDebug()) {
      send_mail(
        from = "do-not-reply@fao.org",
        to = swsContext.userEmail,
        subject = "Some shareDownUp are invalid",
        body = c(paste("There are some invalid shareDownUp (they do not sum to 1). See attachment and fix them in", sub("/work/SWS_R_Share/", "", shareDownUp_file)),
                file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_INVALID_", COUNTRY, ".csv")))
      )
    }

    unlink(file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_INVALID_", COUNTRY, ".csv")))

    stop("Some shares are invalid. Check your email.")
  }

  shareDownUp_previous[,
    `:=`(
      measuredItemParentCPC = sub("'", "", measuredItemParentCPC),
      measuredItemChildCPC = sub("'", "", measuredItemChildCPC)
    )
  ]

  setnames(shareDownUp_previous, "shareDownUp", "shareDownUp_prev")


} else {

  SHAREDOWNUP_LOADED <- FALSE

  shareDownUp_previous <-
    data.table(
      geographicAreaM49 = character(),
      timePointYears = character(),
      measuredItemParentCPC = character(),
      measuredItemChildCPC = character(),
      shareDownUp_prev = numeric(),
      protect_share = logical()
    )
}

# stockable items for which a historical series of at least
# 5 non-missing/non-null data points exist
historical_avail_stocks <-
  data[
    measuredElementSuaFbs == "stock_change" &
      timePointYears <= 2013 &
      !is.na(Value) &
      stockable == TRUE,
    .(n = sum(!dplyr::near(Value, 0))),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
  ][
    n >= 5
  ][,
    n := NULL
  ]


# Keep processingShare and shareDownUp
computed_shares <- list()
computed_shares_send <- list()
# Keep negative availability
negative_availability <- list()

fixed_proc_shares <- list()

original_stock_variation <-
  data[
    measuredElementSuaFbs == "stock_change" & timePointYears >= 2014 & !is.na(Value),
    .(
      geographicAreaM49, measuredItemSuaFbs, timePointYears, Value, flagObservationStatus, flagMethod
    )
  ]

dbg_print("starting derived production loop")

if (length(primaryInvolvedDescendents) == 0) {
  message("No primary commodity involved in this country")
} else {
  for (lev in sort(unique(tree$processingLevel))) {

    dbg_print(paste("derived production loop, level", lev))

    treeCurrentLevel <-
      tree[
        !is.na(Value) &
          processingLevel == lev &
          measuredElementSuaFbs == 'extractionRate'
        ][,
          list(
            measuredItemParentCPC,
            geographicAreaM49,
            measuredItemChildCPC,
            timePointYears,
            extractionRate = Value
          )
        ]

    # Stocks will be generated also for those cases for which we have data
    condition_for_stocks <-
      data$stockable == TRUE &
      data$measuredItemSuaFbs %chin% items_to_generate_stocks &
      #data$measuredItemSuaFbs %chin% treeCurrentLevel$measuredItemParentCPC &
      data$measuredElementSuaFbs %chin% c('production', 'imports', 'exports')

    if (any(condition_for_stocks)) {

      dbg_print("generating stocks")

      data_histmod_stocks <-
        data[
          measuredItemSuaFbs %chin% historical_avail_stocks$measuredItemSuaFbs
        ]

      if (nrow(data_histmod_stocks) > 0) {

        dbg_print("data for histmod available")

        data_histmod_stocks <-
          data_histmod_stocks[,
            .(
              supply_inc =
                sum(
                  Value[measuredElementSuaFbs %chin% c("production", "imports")],
                  - Value[measuredElementSuaFbs %chin% c("exports", "stock_change")],
                  na.rm = TRUE
                ),
              supply_exc =
                sum(
                  Value[measuredElementSuaFbs %chin% c("production", "imports")],
                  - Value[measuredElementSuaFbs == "exports"],
                  na.rm = TRUE
                )
            ),
            keyby = .(geographicAreaM49, measuredItemSuaFbs, timePointYears)
          ][,
            trend := seq_len(.N),
            .(geographicAreaM49, measuredItemSuaFbs)
          ]

        dbg_print("coeffs_stocks_mod")

        data_histmod_stocks[,
          c("c_int", "c_sup", "c_trend") := coeffs_stocks_mod(.SD),
          by = c("geographicAreaM49", "measuredItemSuaFbs")
        ][,
          supply_inc_pred := c_int + c_sup * supply_exc + c_trend * trend
        ][,
          delta_pred := supply_exc - supply_inc_pred
        ]

        data_for_stocks <-
          data_histmod_stocks[
            timePointYears >= 2013 ,
            .(geographicAreaM49, measuredItemSuaFbs, timePointYears, delta = delta_pred)
          ]

        if (nrow(data_for_stocks) > 0) {

          data_for_stocks <-
            merge(
              data_for_stocks,
              all_opening_stocks[, .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua, timePointYears, new_opening = Value)],
              by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"),
              all.x = TRUE
            )

          data_for_stocks <- data_for_stocks[timePointYears >= 2013]

          # If there are opening stocks for 2013, but not for 2014
          data_for_stocks[,
            replace_opening :=
              ifelse(
                timePointYears == "2014" &
                  !is.na(new_opening[timePointYears == "2013"]) &
                  is.na(new_opening[timePointYears == "2014"]),
                new_opening[timePointYears == "2013"] + delta[timePointYears == "2013"],
                NA_real_
              ),
            by = c("geographicAreaM49", "measuredItemSuaFbs")
          ]

          data_for_stocks[
            is.na(new_opening) & !is.na(replace_opening),
            new_opening := ifelse(replace_opening >= 0, replace_opening, 0)
          ]

          data_for_stocks[, replace_opening := NULL]

          data_for_stocks <- data_for_stocks[timePointYears >= 2014]

          stockdata <-
            data[
              Protected == TRUE &
                measuredElementSuaFbs == "stock_change" &
                !is.na(Value) &
                measuredItemSuaFbs %chin% data_for_stocks$measuredItemSuaFbs,
              .(
                geographicAreaM49,
                measuredItemSuaFbs,
                timePointYears,
                stockvar = Value
              )
            ]

          data_for_stocks <-
            merge(
              data_for_stocks,
              stockdata,
              by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"),
              all.x = TRUE
            )

          data_for_stocks[!is.na(stockvar), delta := stockvar][, stockvar := NULL]

          # Stock withdrawal cannot exceed opening stocks in the first year
          data_for_stocks[
            timePointYears == "2014" & delta < 0 & abs(delta) > new_opening,
            delta := - new_opening
          ]

          # NOTE: Data here should be ordered by country/item/year (CHECK)
          data_for_stocks <- update_opening_stocks(data_for_stocks)

          data <-
            merge(
              data,
              data_for_stocks,
              by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
              all.x = TRUE
            )

          #rm(data_for_stocks)

          # Overwrite even protected figures, because, if new delta generated,
          # it's not compatible with previous stock variations
          data[
            measuredElementSuaFbs == "stock_change" &
              !is.na(delta) &
              # allow for some small discrepancy
              (abs(delta) <= 0.999 * abs(Value) | abs(delta) >= 1.001 * abs(Value)),
            `:=`(
              Value = delta,
              flagObservationStatus = "E",
              flagMethod = "u",
              Protected = FALSE
            )
          ]

          data[, c("delta", "new_opening") := NULL]

          all_opening_stocks <-
            merge(
              all_opening_stocks,
              data_for_stocks[,
                .(
                  geographicAreaM49,
                  measuredItemFbsSua = measuredItemSuaFbs,
                  timePointYears,
                  new_opening
                )
              ],
              by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
              all = TRUE
            )

          all_opening_stocks[
            !is.na(new_opening) & (round(new_opening) != round(Value) | is.na(Value)),
            `:=`(
              Value = new_opening,
              flagObservationStatus = "E",
              flagMethod = "u",
              Protected = FALSE
            )
          ]

          all_opening_stocks[, new_opening := NULL]
        }
      }
    }
    
    dataMergeTree <- data[measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stock_change')]
    
    setnames(dataMergeTree, "measuredItemSuaFbs", "measuredItemParentCPC")
    
    dbg_print("dataMerge + treeCurrentLevel")

    dataMergeTree <-
      merge(
        dataMergeTree,
        treeCurrentLevel,
        by = c(p$parentVar, p$geoVar, p$yearVar),
        allow.cartesian = TRUE
      )

    dbg_print("dataMerge + data_tree")

    dataMergeTree <-
      merge(
        dataMergeTree,
        data_tree,
        by = c(p$parentVar, p$childVar, p$geoVar, p$yearVar),
        allow.cartesian = TRUE
      )
    
    dbg_print("dataMerge availability")

    dataMergeTree[,
      availability :=
        sum(
          Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
          # XXX p$stockCode is "stockChange", not "stock_change"
          - Value[get(p$elementVar) %in% c(p$exportCode, "stock_change")],
          na.rm = TRUE
        ),
      by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
    ]
    
    dbg_print("negative availability")

    negative_availability[[as.character(lev)]] <-
      unique(
        dataMergeTree[
          availability < -100,
          list(
            country            = geographicAreaM49,
            year               = timePointYears,
            measuredItemFbsSua = measuredItemParentCPC,
            element            = measuredElementSuaFbs,
            Value,
            flagObservationStatus,
            flagMethod,
            availability
          )
        ]
      )
    
    dbg_print("zero out negative availability")

    # XXX: Replace negative availability, so we get zero production, instead of negative. This, , however, should be fixed in advance, somehow.
    dataMergeTree[availability < 0, availability := 0]
    
    dbg_print("dataMergeTree + shareDownUp_previous")

    dataMergeTree <-
      merge(
        dataMergeTree,
        shareDownUp_previous,
        by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC", "measuredItemChildCPC"),
        all.x = TRUE
      )

    dataMergeTree[protect_share == TRUE, shareDownUp := shareDownUp_prev]

    dataMergeTree[, c("shareDownUp_prev", "protect_share") := NULL]

    # Key here was implicitly set by a previous order()
    setkey(dataMergeTree, NULL)
    
    dataMergeTree <-
      unique(
        dataMergeTree[,
          list(geographicAreaM49, timePointYears, measuredItemParentCPC,
               measuredItemChildCPC, extractionRate, availability, shareDownUp)
        ]
      )

    dbg_print("dataMergeTree + production")
    
    dataMergeTree <-
      merge(
        dataMergeTree,
        data[
          measuredElementSuaFbs == 'production',
          list(geographicAreaM49, timePointYears,
               measuredItemChildCPC = measuredItemSuaFbs, Value, Protected)
          ],
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemChildCPC')
      )
    
    dataMergeTree[timePointYears >= 2014 & Protected == FALSE, Value := NA][, Protected := NULL]
    
    dbg_print("processingShare")

    dataMergeTree[, processingShare := Value / extractionRate * shareDownUp / availability]
    
    # Fix weird processing shares. This should in theory never happen, but it does in some cases.
    dataMergeTree[processingShare < 0, processingShare := 0]
    dataMergeTree[processingShare > 1, processingShare := 1]
    
    dbg_print("processingShare_avg")

    dataMergeTree <-
      dataMergeTree[
        order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears)
      ][,
        processingShare_avg := rollavg(processingShare, order = 3),
        by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
      ]
    
    # Key here was implicitly set by a previous order()
    setkey(dataMergeTree, NULL)
    
    #dataMergeTree[, nonna := sum(!is.na(processingShare)), list(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC)]
    
    dbg_print("replace processingShare with processingShare_avg")

    dataMergeTree[is.na(processingShare), processingShare := processingShare_avg]
    
    dataMergeTree[, processingShare_avg := NULL]


    dbg_print("Fix processing shares for co-products")

    ############### Fix processing shares for co-products

    ################################# Easy cases


    if (nrow(coproduct_table_easy) > 0) {

      dbg_print("Fix processing shares, easy")

      output_easy <-
        dataMergeTree[
          measuredItemChildCPC %in% unique(coproduct_table_easy$measuredItemChildCPC),
          .(geographicAreaM49, timePointYears, measuredItemChildCPC, measuredItemParentCPC, processingShare)
        ]

      if (nrow(output_easy) > 0) {
        output_easy <-
          merge(output_easy, coproduct_table_easy, by = 'measuredItemChildCPC', all.x = TRUE)

        output_easy[,
          reference_share := fmax(measuredItemChildCPC, branch, processingShare, FALSE),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]

        output_easy[,
          # Ask whether rounding by 2 is fine
          reimpute := (round(processingShare, 2) != round(reference_share, 2)) | is.na(processingShare),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]
      }
    }

    ################################# / Easy cases


    ################################# Plus cases


    if (nrow(coproduct_table_plus) > 0) {

      dbg_print("Fix processing shares, plus")

      output_plus <-
        dataMergeTree[
          measuredItemChildCPC %in% unique(coproduct_table_plus$measuredItemChildCPC),
          .(geographicAreaM49, timePointYears, measuredItemChildCPC, measuredItemParentCPC, processingShare)
        ]


      if (nrow(output_plus) > 0) {
        output_plus <-
          merge(output_plus, coproduct_table_plus, by = 'measuredItemChildCPC', all.x = TRUE)

         output_plus[,
          reference_share := fmax(measuredItemChildCPC, branch, processingShare, TRUE),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]

         output_plus[,
          # Ask whether rounding by 2 is fine
          reimpute := (round(processingShare, 2) != round(reference_share, 2)) | is.na(processingShare),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]
      }
    }

    ################################# / Plus cases


    ################################# Or cases

    if (nrow(coproduct_table_or) > 0) {

      dbg_print("Fix processing shares, or")

      output_or <-
        dataMergeTree[
          measuredItemChildCPC %in% unique(coproduct_table_or$measuredItemChildCPC),
          .(geographicAreaM49, timePointYears, measuredItemChildCPC, measuredItemParentCPC, processingShare)
        ]


      if (nrow(output_or) > 0) {
        output_or <-
          merge(output_or, coproduct_table_or, by = 'measuredItemChildCPC', all.x = TRUE)

        output_or[,
          reference_share := fmax(measuredItemChildCPC, branch, processingShare, TRUE),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]

        output_or[,
          # Ask whether rounding by 2 is fine
          reimpute := (round(processingShare, 2) != round(reference_share, 2)) | is.na(processingShare),
          by = c("geographicAreaM49", "timePointYears", "branch", "measuredItemParentCPC")
        ]
      }
    }

    ################################# / Or cases


    output_to_check <-
      rbindlist(
        list(
          if (exists("output_easy")) output_easy else NULL,
          if (exists("output_plus")) output_plus else NULL,
          if (exists("output_or"))   output_or   else NULL
        ),
        fill = TRUE
      )

    if (nrow(output_to_check) > 0) {

      output_to_check <-
        output_to_check[
          reimpute == TRUE,
          .(geographicAreaM49, timePointYears, measuredItemParentCPC, measuredItemChildCPC, reference_share)
        ]

      dataMergeTree <-
        merge(
          dataMergeTree,
          output_to_check,
          by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC", "measuredItemChildCPC"),
          all.x = TRUE
        )

      fixed_proc_shares[[as.character(lev)]] <- dataMergeTree[!is.na(reference_share)]

      dbg_print("replace processing share with reference share")

      dataMergeTree[!is.na(reference_share), processingShare := reference_share]

      dataMergeTree[, reference_share := NULL]
    }


    ############### / Fix processing shares for co-products
    
    dataMergeTree <-
      unique(
        dataMergeTree[,
          list(geographicAreaM49, timePointYears, measuredItemParentCPC,
               measuredItemChildCPC, Value, extractionRate, shareDownUp,
               processingShare, availability)]
      )
    
    dbg_print("impute production of derived")

    dataMergeTree[, new_imputation := availability * processingShare * extractionRate]
    
    computed_shares[[as.character(lev)]] <-
      dataMergeTree[,
        list(geographicAreaM49, timePointYears, measuredItemParentCPC,
             measuredItemChildCPC, shareDownUp, processingShare)
      ]
    
    # XXX: change this name
    computed_shares_send[[as.character(lev)]] <-
      dataMergeTree[,
        list(geographicAreaM49, timePointYears, measuredItemParentCPC,
             measuredItemChildCPC, extractionRate, shareDownUp,
             processingShare, availability_parent = availability)
      ]
    
    dbg_print("sum unique of new_imputation")

    dataMergeTree <-
      dataMergeTree[,
        list(
          measuredElementSuaFbs = 'production',
          # NOTE: the sum(unique()) is just a temporary HACK: check how to do it properly
          imputed_deriv_value = sum(unique(round(new_imputation, 2)), na.rm = TRUE)
        ),
        list(geographicAreaM49, timePointYears, measuredItemSuaFbs = measuredItemChildCPC)
      ]
    
    data <-
      merge(
        data,
        dataMergeTree,
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs', 'measuredElementSuaFbs'),
        all.x = TRUE
      )

    dbg_print("z data for shares to send")

    z <- data[
        measuredElementSuaFbs %in% c("production", "imports", "exports", "stock_change"),
        list(geographicAreaM49, timePointYears, measuredItemSuaFbs, measuredElementSuaFbs,
             Value = ifelse(measuredElementSuaFbs == 'production' & Protected == FALSE, imputed_deriv_value, Value),
             Protected)
        ]

    dbg_print("dcast z data")

    z <- data.table::dcast(z, geographicAreaM49+timePointYears+measuredItemSuaFbs~measuredElementSuaFbs, value.var = c("Value", "Protected"))
    
    # XXX stock_change may change below (and also production, if required)

    dbg_print("z data + computed_shares_send")

    computed_shares_send[[as.character(lev)]] <-
      z[computed_shares_send[[as.character(lev)]],
        on = c('geographicAreaM49'  = 'geographicAreaM49',
               'timePointYears'     = 'timePointYears',
               'measuredItemSuaFbs' = 'measuredItemChildCPC')]
    
    dbg_print("assign production of derived to non protected/frozen data")

    # Assign if non-protected only non-fozen data
    # (XXX: here only measuredItemSuaFbs that are child should be assigned)
    data[
      timePointYears >= 2014 &
        Protected == FALSE &
        measuredElementSuaFbs == 'production' &
        !is.na(imputed_deriv_value),
      `:=`(
        Value = imputed_deriv_value,
        flagObservationStatus = "I", flagMethod = "c")]
    
    data[, imputed_deriv_value := NULL]
  }
}


computed_shares <- rbindlist(computed_shares)

# Check on consistency of shareDownUp
shareDownUp_invalid <-
  computed_shares[,
    .(sum_shares = sum(shareDownUp)),
    by = c("geographicAreaM49", "timePointYears", "measuredItemChildCPC")
  ][
    !dplyr::near(sum_shares, 1) & !dplyr::near(sum_shares, 0)
  ]

if (nrow(shareDownUp_invalid) > 0) {

  shareDownUp_invalid <-
    computed_shares[
      shareDownUp_invalid,
      on = c("geographicAreaM49", "timePointYears", "measuredItemChildCPC")
    ]

  write.csv(
    shareDownUp_invalid,
    file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_INVALID_", COUNTRY, ".csv"))
  )

  if (!CheckDebug()) {
    send_mail(
      from = "do-not-reply@fao.org",
      to = swsContext.userEmail,
      subject = "Some shareDownUp are invalid",
      body = c(paste("There are some invalid shareDownUp (they do not sum to 1). See attachment and fix them in", sub("/work/SWS_R_Share/", "", shareDownUp_file)),
              file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_INVALID_", COUNTRY, ".csv")))
    )
  }

  unlink(file.path(R_SWS_SHARE_PATH, USER, paste0("shareDownUp_INVALID_", COUNTRY, ".csv")))

  stop("Some shares are invalid. Check your email.")
}
# / Check on consistency of shareDownUp

shareDownUp_save <-
  computed_shares[,
    .(geographicAreaM49, timePointYears, measuredItemParentCPC,
      measuredItemChildCPC, shareDownUp)]

if (nrow(shareDownUp_previous) == 0) {
  shareDownUp_save[, protect_share := FALSE]
} else {
  shareDownUp_save <-
    merge(
      computed_shares[, -"processingShare", with = FALSE],
      shareDownUp_previous[
        protect_share == TRUE,
        .(geographicAreaM49, timePointYears, measuredItemParentCPC, measuredItemChildCPC, protect_share)
      ],
      by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC", "measuredItemChildCPC"),
      all = TRUE
    )

  shareDownUp_save[is.na(protect_share), protect_share := FALSE]
}

shareDownUp_save[,
  `:=`(
    measuredItemParentCPC = paste0("'", measuredItemParentCPC),
    measuredItemChildCPC = paste0("'", measuredItemChildCPC)
  )
]

if (!file.exists(dirname(shareDownUp_file))) {
  dir.create(dirname(tmp_file_outliers), recursive = TRUE)
}

write.csv(shareDownUp_save, shareDownUp_file, row.names = FALSE)

dbg_print("end of derived production loop")


if (STOP_AFTER_DERIVED == TRUE) {
  dbg_print("stop after production of derived")
  
  data_deriv <-
    data[
      measuredElementSuaFbs == 'production' &
        timePointYears %in% 2014:2017,
      list(
        geographicAreaM49,
        measuredElementSuaFbs = "5510",
        measuredItemFbsSua = measuredItemSuaFbs,
        timePointYears,
        Value,
        flagObservationStatus,
        flagMethod
      )
    ]

  # Save stock data in SUA Unbalanced

  data_stock_to_save <-
    data[
      measuredElementSuaFbs == 'stock_change' &
        timePointYears %in% 2014:2017 &
        !is.na(Value),
      list(
        geographicAreaM49,
        measuredElementSuaFbs = "5071",
        measuredItemFbsSua = measuredItemSuaFbs,
        timePointYears,
        Value,
        flagObservationStatus,
        flagMethod
      )
    ]

  opening_stocks_to_save <- copy(all_opening_stocks)
  opening_stocks_to_save[, Protected := NULL]

  data_to_save_unbalanced <- rbind(data_deriv, data_stock_to_save, opening_stocks_to_save)
  
  out <-
    SaveData(
      domain = "suafbs",
      dataset = "sua_unbalanced",
      data = data_to_save_unbalanced,
      waitTimeout = 20000
    )
  
  if (exists("out")) {
    
    print(paste(out$inserted + out$ignored, "derived products written"))

    if (!CheckDebug()) {
      send_mail(
        from = "do-not-reply@fao.org",
        to = swsContext.userEmail,
        subject = "Production of derived items created",
        body = paste("The plugin stopped after production of derived items. A file with shareDownUp is available in", sub("/work/SWS_R_Share/", "", shareDownUp_file))

      )
    }
    
  } else {
    print("The newBalancing plugin had a problem when saving derived data.")
  }
  
  stop("Plugin stopped after derived, as requested. This is fine.")
}

fixed_proc_shares <- rbindlist(fixed_proc_shares, idcol = "level")

if (nrow(fixed_proc_shares) > 0) {

  setnames(fixed_proc_shares, c("processingShare", "reference_share"), c("orig_proc_share", "new_proc_share"))

  fixed_proc_shares <-
    fixed_proc_shares[order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears)]

  fixed_proc_shares[,
    `:=`(
      measuredItemParentCPC = paste0("'", measuredItemParentCPC),
      measuredItemChildCPC = paste0("'", measuredItemChildCPC)
    )
  ]

  # XXX: removing value here as it is not the last computed value,
  # but we probably keep both (old and new)
  fixed_proc_shares[, Value := NULL]
} else {
  fixed_proc_shares <- data.table(info = "No processing shares need to be fixed.")
}

computed_shares_send <- rbindlist(computed_shares_send, fill = TRUE)

colnames(computed_shares_send) <- sub("Value_", "", colnames(computed_shares_send))

setnames(
  computed_shares_send,
  c("timePointYears", "measuredItemSuaFbs", "measuredItemParentCPC"),
  c("year", "measuredItemChildCPC_tree", "measuredItemParentCPC_tree")
)

computed_shares_send <- nameData("suafbs", "ess_fbs_commodity_tree2", computed_shares_send)

setnames(
  computed_shares_send,
  c("geographicAreaM49", "geographicAreaM49_description",
    "measuredItemChildCPC_tree", "measuredItemChildCPC_tree_description",
    "measuredItemParentCPC_tree", "measuredItemParentCPC_tree_description"),
  c("Country", "Country_name", "Child", "Child_name", "Parent", "Parent_name")
)

computed_shares_send[, zero_weigth := Child %in% zeroWeight]

computed_shares_send[, `:=`(Child = paste0("'", Child), Parent = paste0("'", Parent))]

negative_availability <- rbindlist(negative_availability)

if (nrow(negative_availability) > 0) {

  # FIXME: stocks may be generated twice for parents in multiple level,
  # (though, the resulting figures are the same). Fix in the prod deriv loop.
  negative_availability <- unique(negative_availability)

  negative_availability_var <-
    negative_availability[,
      .(
        country,
        year,
        measuredItemFbsSua,
        element = "availability",
        Value = availability,
        flagObservationStatus = "I",
        flagMethod = "i"
      )
    ]

  negative_availability[, availability := NULL]

  negative_availability <-
    rbind(
      negative_availability,
      unique(negative_availability_var) #FIXME
    )

  negative_availability <-
    data.table::dcast(
      negative_availability,
      country + measuredItemFbsSua + year ~ element,
      value.var = "Value"
    )

  negative_availability <- nameData("suafbs", "sua_unbalanced", negative_availability)

  negative_availability[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

}



dbg_print("start outliers")

######################## OUTLIERS #################################
# re-writing of Cristina's outliers plugin with data.table syntax #

if (FIX_OUTLIERS == TRUE) {

  commDef <- ReadDatatable("fbs_commodity_definitions") # XXX: updated?

  primaryProxyPrimary_items <- commDef[proxy_primary == "X" | primary_commodity == "X"]$cpc
  food_items <- commDef[food_item == "X"]$cpc

  dout <-
    CJ(
      measuredItemSuaFbs = unique(data$measuredItemSuaFbs),
      measuredElementSuaFbs = unique(data$measuredElementSuaFbs),
      geographicAreaM49 = unique(data$geographicAreaM49),
      timePointYears = unique(data$timePointYears)
    )

  dout <-
    merge(
      dout,
      data,
      by = c('geographicAreaM49', 'timePointYears',
             'measuredItemSuaFbs', 'measuredElementSuaFbs'),
      all.x = TRUE
    )

  dout[is.na(Protected), Protected := FALSE]

  dout[,
    `:=`(
      production = Value[measuredElementSuaFbs  == "production"],
      supply     = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                       - Value[measuredElementSuaFbs %in% c("exports", "stock_change")],
                       na.rm = TRUE),
      domsupply  = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                       na.rm = TRUE)
    ),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears')
  ]

  dout[measuredElementSuaFbs %in% c("feed", "industrial"), element_supply := supply]
  dout[measuredElementSuaFbs == "seed", element_supply := production]
  dout[measuredElementSuaFbs == "loss", element_supply := domsupply]

  dout[element_supply < 0, element_supply := NA_real_]

  dout[, ratio := Value / element_supply]

  dout[,
    `:=`(
      mean_ratio = mean(ratio[timePointYears %in% 2011:2013], na.rm = TRUE),
      Meanold    = mean(Value[timePointYears < 2014], na.rm = TRUE)
    ),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
  ]

  # If the element is new, there will be no `mean_ratio` and `Meanold`, thus we
  # use the new values to re-calculate them.

  dout[
    timePointYears >= 2014 & (is.na(mean_ratio) | is.nan(mean_ratio) | is.infinite(mean_ratio)),
    mean_ratio := mean(ratio[timePointYears >= 2014], na.rm = TRUE),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
  ]

  dout[
    timePointYears >= 2014 & (is.na(Meanold) | is.nan(Meanold) | is.infinite(Meanold)),
    Meanold := mean(Meanold[timePointYears >= 2014], na.rm = TRUE),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs')
  ]

  dout[mean_ratio > 1, mean_ratio := 1]

  dout[, abs_diff_threshold := ifelse(measuredElementSuaFbs == "feed", 0.1, 0.05)]

  dout[
    Protected == FALSE &
      timePointYears %in% 2014:2017 & # XXX: parameterise
      mean_ratio > 0 &
      abs(element_supply) > 0 &
      measuredElementSuaFbs %in% c("feed", "seed", "loss", "industrial") &
      # the conditions below define the outlier, or cases that were NA
      (is.na(Value) |
         abs(ratio - mean_ratio) > abs_diff_threshold |
         mean_ratio == 1 |
         (mean_ratio != 0 & dplyr::near(ratio, 0)) |
         (outside(Value / Meanold, 0.5, 2) & outside(Value - Meanold, -10000, 10000))),
    impute := element_supply * mean_ratio
  ]


  # Remove imputation for loss that is non-food and non-primaryProxyPrimary
  dout[
    measuredElementSuaFbs == "loss" &
      !(measuredItemSuaFbs %in% food_items &
          measuredItemSuaFbs %in% primaryProxyPrimary_items),
    impute := NA_real_
  ]

  dout <-
    dout[
      !is.na(impute) & (is.na(Value) | round(Value, 1) != round(impute, 1)),
      list(
        geographicAreaM49,
        measuredItemSuaFbs,
        measuredElementSuaFbs,
        timePointYears,
        Value_imputed = impute
      )
    ]

  if (nrow(dout) > 0) {
    
    data <-
      merge(
        data,
        dout,
        by = c('geographicAreaM49', 'measuredItemSuaFbs', 'measuredElementSuaFbs', 'timePointYears'),
        all = TRUE
      )
    
    data[
      !is.na(Value_imputed),
      `:=`(
        Value = Value_imputed,
        flagObservationStatus = "E",
        flagMethod = "e")
      ]
    
    data[, Value_imputed := NULL]
  }
}

dbg_print("end outliers")


####################### / OUTLIERS #################################




# Protect all loss data, to keep it consistent with SDG indicator,
# whatever the flag is.
data[measuredElementSuaFbs == "loss", Protected := TRUE]



data <- merge(data, itemMap, by = "measuredItemSuaFbs")

#setnames(itemMap, "measuredItemSuaFbs", "measuredItemParentCPC")

#data <- data[, list(measuredItemSuaFbs, measuredElementSuaFbs, geographicAreaM49,
#             timePointYears, Value, flagObservationStatus, flagMethod,
#             Valid, Protected, Official, type)]



# XXX ???
#cutItems <- ReadDatatable("cut_items2")[, cpc_code]

## XXX ???
#fbsTree <- ReadDatatable("fbs_tree")
## XXX: why this order?
#fbsTree <- fbsTree[, list(fbsID4 = id4, measuredItemSuaFbs = item_sua_fbs, fbsID1 = id1, fbsID2 = id1, fbsID3 = id3)]



#data <- data[, mget(c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49", "timePointYears", "Value", "Official", "Protected", "type", "flagObservationStatus", "flagMethod"))]


## Split data based on the two factors we need to loop over
uniqueLevels <- unique(data[, list(geographicAreaM49, timePointYears)])

uniqueLevels <- uniqueLevels[order(geographicAreaM49, timePointYears)]

#parentNodes <- getCommodityLevel(tree, parentColname = "measuredItemParentCPC", childColname = "measuredItemChildCPC")
#
#parentNodes <- parentNodes[level == 0, node]



# FIXME: some names are set lowercase, but should be now camelCase
data[measuredElementSuaFbs == 'foodmanufacturing', measuredElementSuaFbs := 'foodManufacturing']
data[measuredElementSuaFbs == 'stock_change', measuredElementSuaFbs := 'stockChange']


data <-
  plyr::ddply(
    data,
    .variables = c('geographicAreaM49', 'timePointYears'),
    .fun = function(x) addMissingElements(as.data.table(x), p)
  )

setDT(data)

data[, stockable := measuredItemSuaFbs %chin% stockable_items]

# NOTE: the definition of "food_resid" changed (see inside newBalancing)
#data[, food_resid := measuredItemSuaFbs %chin% food_only_items]



# Remove stocks for non stockable items
data <- data[!(measuredElementSuaFbs == 'stock_change' & stockable == 'FALSE')]

#setDT(data)



# Remove residual and tourist (for now) [with exceptions]
if (COUNTRY == "28") {
  data <- data[!(measuredElementSuaFbs %chin% c('residual'))]
} else {
  data <- data[!(measuredElementSuaFbs %chin% c('residual', 'tourist'))]
}



######################### Save UNB for validation #######################

sua_unbalanced <- data[, .(geographicAreaM49, timePointYears,
                           measuredItemSuaFbs, measuredElementSuaFbs,
                           Value, flagObservationStatus, flagMethod)]

sua_unbalanced_aux <-
  sua_unbalanced[,
    .(
      supply =
        sum(Value[measuredElementSuaFbs %chin% c("production", "imports")],
            - Value[measuredElementSuaFbs %chin% c("exports", "stockChange")],
            na.rm = TRUE),
      utilizations =
        sum(Value[!(measuredElementSuaFbs %chin%
                    c("production", "imports", "exports", "stockChange"))],
            na.rm = TRUE)
    ),
    by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
  ][,
    imbalance := supply - utilizations
  ][
    supply > 0,
    imbalance_pct := imbalance / supply * 100
  ]

sua_unbalanced_aux <-
  melt(
    sua_unbalanced_aux,
    c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"),
    variable.name = "measuredElementSuaFbs",
    value.name = "Value"
  )

sua_unbalanced_aux[, `:=`(flagObservationStatus = "I", flagMethod = "i")]

sua_unbalanced <- rbind(sua_unbalanced, sua_unbalanced_aux)


saveRDS(
  sua_unbalanced,
  file.path(R_SWS_SHARE_PATH, "FBS_validation", COUNTRY, "sua_unbalanced.rds")
)

######################### Save UNB for validation #######################




data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  movsum_value := RcppRoll::roll_sum(shift(Value), 3, fill = 'extend', align = 'right'),
  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
]

data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  mov_share := movsum_value / sum(movsum_value, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
]

# Impute share if missing
data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  mov_share := rollavg(mov_share),
  by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
]

# Set sum of shares = 1
data[
  measuredElementSuaFbs %chin% c("feed", "food", "industrial", "loss", "seed", "tourist"),
  mov_share := mov_share / sum(mov_share, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
]



data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# XXX Only from 2004 onwards
uniqueLevels <- uniqueLevels[timePointYears >= 2014][order(timePointYears)]


# Filter elements that appear for the first time

data_complete <-
  data.table(
    geographicAreaM49 = unique(data$geographicAreaM49),
    timePointYears = sort(unique(data$timePointYears)))[
      unique(data[, .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs)]),
      on = "geographicAreaM49",
      allow.cartesian = TRUE
    ]

data_complete <-
  merge(
    data_complete,
    data[Value > 0, .(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears, Value)],
    by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs", "timePointYears"),
    all.x = TRUE
  )

data_complete[, y := 1]

data_complete <-
  data_complete[,
    .(
      t_pre   = sum(y[timePointYears <= 2013]),
      t_post  = sum(y[timePointYears >= 2014]),
      na_pre  = sum(is.na(Value[timePointYears <= 2013])),
      na_post = sum(is.na(Value[timePointYears >= 2014]))
    ),
    by = c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemSuaFbs")
  ]

new_elements <-
  data_complete[
    na_pre == t_pre & na_post < t_post
  ][,
    .(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua = measuredItemSuaFbs)
  ][
    order(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua)
  ]

new_loss <-
  new_elements[
    measuredElementSuaFbs == "loss",
    .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua, new_loss = TRUE)
  ]

new_elements <- nameData("suafbs", "sua_unbalanced", new_elements, except = "measuredElementSuaFbs")

new_elements[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

tmp_file_name_new <- tempfile(pattern = paste0("NEW_ELEMENTS_", COUNTRY, "_"), fileext = '.csv')

write.csv(new_elements, tmp_file_name_new)

# / Filter elements that appear for the first time



dbg_print("set thresholds")


if (THRESHOLD_METHOD == 'nolimits') {
  
  dbg_print("thresholds, nolimits")
  
  data[, min_threshold := -Inf]
  data[, max_threshold := Inf]
  
} else if (THRESHOLD_METHOD == 'level') { ############ DON'T USE
  
  dbg_print("thresholds, level")
  
  data[,
    `:=`(
      min_threshold = min(Value[timePointYears %in% 2000:2013], na.rm = TRUE),
      max_threshold = max(Value[timePointYears %in% 2000:2013], na.rm = TRUE)
    ),
    by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
  ]
  
} else if (THRESHOLD_METHOD == 'levelquartile') {
  
  dbg_print("thresholds, share")
  
  data[,
    `:=`(
      min_threshold = quantile(Value[timePointYears %in% 2000:2013], 0.25, na.rm = TRUE),
      max_threshold = quantile(Value[timePointYears %in% 2000:2013], 0.75, na.rm = TRUE)
    ),
    by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
  ]
  
} else if (THRESHOLD_METHOD == 'share') {
  
  dbg_print("thresholds, share")
  
  data[,
    supply :=
      sum(
        Value[measuredElementSuaFbs %in% c('production', 'imports')],
        - Value[measuredElementSuaFbs %in% c('exports', 'stockChange')],
        na.rm = TRUE
      ),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]

  # NOTE: here we redefine what "supply" is just for seed.

  data[
    measuredElementSuaFbs == "seed",
    supply := Value[measuredElementSuaFbs == "production"],
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]

  data[supply < 0, supply := 0]

  data[
    !(measuredElementSuaFbs %chin% c("production", "imports", "exports", "stockChange")),
    util_share := Value / supply
  ]

  data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]

  # This really shouldn't happen
  data[util_share < 0 & measuredElementSuaFbs != "tourist", util_share := 0]

  data[util_share > 1, util_share := 1]
  
  data[,
    `:=`(
      min_util_share = min(util_share[timePointYears %in% 2000:2013], na.rm = TRUE),
      max_util_share = max(util_share[timePointYears %in% 2000:2013], na.rm = TRUE)
    ),
    by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
  ]
  
  data[is.infinite(min_util_share) | is.nan(min_util_share), min_util_share := NA_real_]
  
  data[is.infinite(max_util_share) | is.nan(max_util_share), max_util_share := NA_real_]
  
  data[max_util_share > 1, max_util_share := 1]
  
  data[,
    `:=`(
      min_threshold = supply * min_util_share,
      max_threshold = supply * max_util_share
    )
  ]
  
  data[, supply := NULL]
  
} else if (THRESHOLD_METHOD == "sharequartile") {
  
  dbg_print("thresholds, sharequartile")
  
  data[,
    supply :=
      sum(
        Value[measuredElementSuaFbs %in% c('production', 'imports')],
        - Value[measuredElementSuaFbs %in% c('exports', 'stockChange')],
        na.rm = TRUE
      ),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
  
  data[supply < 0, supply := 0]
  
  data[, util_share := Value / supply]
  
  data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]
  
  data[,
    `:=`(
      min_util_share = quantile(util_share[timePointYears %in% 2000:2013], 0.25, na.rm = TRUE),
      max_util_share = quantile(util_share[timePointYears %in% 2000:2013], 0.75, na.rm = TRUE)
    ),
    by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
  ]
  
  data[is.infinite(min_util_share) | is.nan(min_util_share), min_util_share := NA_real_]
  
  data[is.infinite(max_util_share) | is.nan(max_util_share), max_util_share := NA_real_]
  
  data[max_util_share > 1, max_util_share := 1]
  
  data[,
    `:=`(
      min_threshold = supply * min_util_share,
      max_threshold = supply * max_util_share
    )
  ]
  
  data[, supply := NULL]
  
} else if (THRESHOLD_METHOD == "sharedeviation1") { #  XXXXXXXXXXXXX don't use. DO NOT.
  
  dbg_print("thresholds, sharedeviation1")
  
  data[,
    supply :=
      sum(
        Value[measuredElementSuaFbs %in% c('production', 'imports')],
        - Value[measuredElementSuaFbs %in% c('exports', 'stockChange')],
        na.rm = TRUE
      ),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
  
  data[supply < 0, supply := 0]
  
  data[, util_share := Value / supply]
  
  data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]
  
  data[,
    util_share_sd := sd(util_share[timePointYears %in% 2000:2013], na.rm = TRUE),
    by = c("measuredItemSuaFbs", "measuredElementSuaFbs", "geographicAreaM49")
  ]
  
  data[,
    `:=`(
      min_util_share = util_share - ifelse(!is.na(util_share_sd), util_share_sd, 0),
      max_util_share = util_share + ifelse(!is.na(util_share_sd), util_share_sd, 0)
    )
  ]
  
  data[is.infinite(min_util_share) | is.nan(min_util_share), min_util_share := NA_real_]
  
  data[is.infinite(max_util_share) | is.nan(max_util_share), max_util_share := NA_real_]
  
  data[max_util_share > 1, max_util_share := 1]
  
  data[,
    `:=`(
      min_threshold = supply * min_util_share,
      max_threshold = supply * max_util_share
    )
  ]
  
  data[, supply := NULL]
  
} else {
  stop("Invalid method.")
}


data[,
  `:=`(
    min_adj = min_threshold / Value,
    max_adj = max_threshold / Value
  )
]


data[min_adj > 1, min_adj := 0.9]
data[min_adj < 0, min_adj := 0.01]

data[max_adj < 1, max_adj := 1.1]
data[max_adj > 10, max_adj := 10] # XXX Too much?

# Fix for new "loss" element. If loss gets in as a new utilization,
# remove the thresholds for food.

data <-
  merge(
    data,
    new_loss,
    by = c('geographicAreaM49', 'measuredItemSuaFbs'),
    all.x = TRUE
  )

data[
  new_loss == TRUE & measuredElementSuaFbs == "food",
  `:=`(min_adj = 0, max_adj = 10)
] # XXX is 10 enough?

data[, new_loss := NULL]

# / Fix for new "loss" element. If loss gets in as a new utilization,
# / remove the thresholds for food.



# Recalculate the levels, given the recalculation of adj factors
data[, min_threshold := Value * min_adj]
data[, max_threshold := Value * max_adj]

# We need the min and max to be "near" one (but not too near) in case of
# Protected figures, so that they are not changed

data[Protected == TRUE, min_adj := 0.999]
data[Protected == TRUE, max_adj := 1.001]

# This fix should also be done in optim function?
data[
  dplyr::near(min_adj, 1) & dplyr::near(max_adj, 1),
  `:=`(
    min_adj = 0.999,
    max_adj = 1.001
  )
]
 
data[,
  `:=`(
    Food_Median       = median(Value[measuredElementSuaFbs=="food" & timePointYears %in% 2000:2013], na.rm=TRUE),
    Feed_Median       = median(Value[measuredElementSuaFbs=="feed" & timePointYears %in% 2000:2013], na.rm=TRUE),
    Industrial_Median = median(Value[measuredElementSuaFbs=="industrial" & timePointYears %in% 2000:2013], na.rm=TRUE)
  ),
  by = c("geographicAreaM49", "measuredItemSuaFbs")
]



# Calculate processed

# Now, let's calculate food processing
data_for_proc <-
  data[
    measuredElementSuaFbs == 'production',
    list(geographicAreaM49, timePointYears, measuredItemSuaFbs, Value)
    ][
      computed_shares,
      on = c('geographicAreaM49' = 'geographicAreaM49',
             'timePointYears' = 'timePointYears',
             'measuredItemSuaFbs' = 'measuredItemChildCPC'),
      nomatch = 0
    ]

food_proc_table <-
  tree[
    measuredElementSuaFbs == 'extractionRate',
    list(measuredItemParentCPC, measuredItemSuaFbs = measuredItemChildCPC, extractionRate = Value, timePointYears)
  ][
    data_for_proc,
    on = c('measuredItemParentCPC', 'measuredItemSuaFbs', 'timePointYears')
  ][,
    zero_weight := measuredItemSuaFbs %in% zeroWeight
  ][,
    food_proc_i := Value / extractionRate * shareDownUp * !zero_weight
  ][,
    list(food_proc = sum(food_proc_i)),
    by = list(geographicAreaM49, timePointYears, measuredItemSuaFbs = measuredItemParentCPC)
  ]

food_proc_table <- food_proc_table[food_proc > 0 & timePointYears >= 2014]
      
data <-
  merge(
    data,
    food_proc_table,
    by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
    all.x = TRUE
  )

data[
  measuredElementSuaFbs == "foodManufacturing" &
    Protected == FALSE & !is.na(food_proc),
  `:=`(
    Value = food_proc,
    flagObservationStatus = "E",
    flagMethod = "i"
  )
][,
  food_proc := NULL
]


## 1 => year = 2014
i <- 1

dbg_print("starting balancing loop")

standData <- vector(mode = "list", length = nrow(uniqueLevels))

#issues <- list()

for (i in seq_len(nrow(uniqueLevels))) {

  # For stocks, the first year no need to see back in time. After the first year was done,
  # stocks may have changed, so opening need to be changed in "data".

  if (i > 1) {
    items_stocks_changed <-
      unique(standData[[i-1]][!is.na(change_stocks)]$measuredItemSuaFbs)

    if (length(items_stocks_changed) > 0) {
    
      stocks_modif <-
        rbind(
          # Previous data (balanced)
          standData[[i-1]][
            !is.na(Value) & measuredElementSuaFbs == 'stockChange',
            list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value)
          ],
          # New data (unbalanced)
          data[
            !is.na(Value) & timePointYears > unique(standData[[i-1]]$timePointYears) &
              measuredElementSuaFbs == 'stockChange',
            list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value)
          ]
        )

      data_for_opening <-
        merge(
          all_opening_stocks[
            timePointYears >= 2014,
            .(geographicAreaM49,  measuredItemSuaFbs =  measuredItemFbsSua,
              timePointYears, new_opening = Value)
          ],
          stocks_modif,
          by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears"),
          all.x = TRUE
        )

      data_for_opening[is.na(delta), delta := 0]

      data_for_opening <- data_for_opening[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]

      data_for_opening <- update_opening_stocks(data_for_opening)

      all_opening_stocks <-
        merge(
          all_opening_stocks,
          data_for_opening[,
            .(
              geographicAreaM49,
              measuredItemFbsSua = measuredItemSuaFbs,
              timePointYears,
              new_opening
            )
          ],
          by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
          all.x = TRUE
        )

      all_opening_stocks[
        !is.na(new_opening) & (round(new_opening) != round(Value) | is.na(Value)),
        `:=`(
          Value = new_opening,
          flagObservationStatus = "E",
          flagMethod = "u",
          Protected = FALSE
        )
      ]

      all_opening_stocks[, new_opening := NULL]
    }
  }
  
  filter <- uniqueLevels[i, ]

  utilizationTableSubset <-
    utilizationTable[uniqueLevels[i, list(geographicAreaM49)], on = 'geographicAreaM49']
  
  treeSubset <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  
  treeSubset[, c("geographicAreaM49", "timePointYears") := NULL]
  
  dataSubset <- data[filter, on = c("geographicAreaM49", "timePointYears")]

  newBal_result <- 
    newBalancing(
      data = dataSubset,
      tree = treeSubset,
      #nutrientData = subNutrientData,
      #batchnumber = batchnumber,
      utilizationTable = utilizationTableSubset, # XXX: this one needs to be removed
      Utilization_Table = Utilization_Table,
      zeroWeight = zeroWeight #,
      #fbsTree = fbsTree,
      #cutItems = cutItems
    )

  standData[[i]] <- newBal_result[["balanced"]]

  #issues[[length(issues) + 1]] <- newBal_result[["issues"]]

  # FIXME: we are now assigning the "Protected" flag to ALL processing as
  # after the first loop it should have been computed and that value SHOULD
  # never be touched again.
  standData[[i]][measuredElementSuaFbs == "foodManufacturing", Protected := TRUE]

}

## The list is repeated, so it's sufficient to take the first element
#issues <- rbindlist(issues[[1]])

#if (nrow(issues) > 0) {
#    write.csv(
#      issues,
#      file.path(R_SWS_SHARE_PATH, USER, paste0("processing_level_issues_", COUNTRY, ".csv"))
#    )
#
#    if (!CheckDebug()) {
#      send_mail(
#        from = "do-not-reply@fao.org",
#        to = swsContext.userEmail,
#        subject = "Issues with processing level",
#        body = c("Some parents and children are processed at the same level. See attached file. You may also want to see the Tree section in http://hqlprsws1.hq.un.fao.org:3838/FBSvalidation/",
#                file.path(R_SWS_SHARE_PATH, USER, paste0("processing_level_issues_", COUNTRY, ".csv")))
#      )
#    }
#
#    unlink(file.path(R_SWS_SHARE_PATH, USER, paste0("processing_level_issues_", COUNTRY, ".csv")))
#}

dbg_print("end of balancing loop")

standData <- rbindlist(standData)

calculateImbalance(standData)

standData[
  supply > 0,
  imbalance_percent := imbalance / supply * 100
]

# If the imbalance is relatively small (less than 5% in absoulte value)
# a new allocation is done, this time with no limits.

if (nrow(standData[data.table::between(imbalance, -5, 5)]) > 0) {

  standData_no_imbalance <- standData[data.table::between(imbalance, -1, 1)]
  standData_with_imbalance <- standData[outside(imbalance, -1, 1)]

  levels_to_optimize <- unique(standData_with_imbalance[, .(geographicAreaM49, timePointYears, measuredItemSuaFbs)])

  D_adj <- list()

  for (i in 1:nrow(levels_to_optimize)) {
    #print(i) ; flush.console()
    # FIXME: remove this (ugly) global assignment
    x <<- standData_with_imbalance[levels_to_optimize[i], on = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs')]

    # The following two instructions basically imply to assign the
    # (small) imbalance with no limits

    x[measuredElementSuaFbs %!in% c("production", "imports", "exports", "stockChange") &
      Protected == FALSE & !is.na(min_threshold), min_threshold := 0]

    x[measuredElementSuaFbs %!in% c("production", "imports", "exports", "stockChange") &
      Protected == FALSE & !is.na(max_threshold), max_threshold := Inf]

    if (BALANCING_METHOD == "proportional") {
      x[, adjusted_value := balance_proportional(x)]
    } else if (BALANCING_METHOD == "optimization") {
      x[, adjusted_value := balance_optimization(x)]
    } else {
      stop("Invalid balancing method.")
    }

    x[
      !is.na(adjusted_value) & adjusted_value != Value,
      `:=`(
        Value = adjusted_value,
        flagObservationStatus = "E",
        flagMethod = "n"
      )
    ]

    D_adj[[i]] <- x

    rm(x)
  }

  standData_with_imbalance <- rbindlist(D_adj)

  standData_with_imbalance[, adjusted_value := NULL]

  standData <- rbind(standData_with_imbalance, standData_no_imbalance)

}

calculateImbalance(standData)

standData[
  supply > 0,
  imbalance_percent := imbalance / supply * 100
]



######################### Save BAL for validation #######################

dbg_print("sua_balanced for validation")

sua_balanced <-
  rbind(
    data[
      timePointYears < 2014,
      .(geographicAreaM49, timePointYears, measuredItemSuaFbs,
        measuredElementSuaFbs, Value, flagObservationStatus, flagMethod)],
    standData[,
      .(geographicAreaM49, timePointYears, measuredItemSuaFbs,
        measuredElementSuaFbs, Value, flagObservationStatus, flagMethod)]
  )

sua_balanced_aux <-
  sua_balanced[,
    .(
      supply =
        sum(Value[measuredElementSuaFbs %chin% c("production", "imports")],
            - Value[measuredElementSuaFbs %chin% c("exports", "stockChange")],
            na.rm = TRUE),
      utilizations =
        sum(Value[!(measuredElementSuaFbs %chin%
                    c("production", "imports", "exports", "stockChange"))],
            na.rm = TRUE)
    ),
    by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
  ][,
    imbalance := supply - utilizations
  ][
    supply > 0,
    imbalance_pct := imbalance / supply * 100
  ]

sua_balanced_aux <-
  melt(
    sua_balanced_aux,
    c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"),
    variable.name = "measuredElementSuaFbs",
    value.name = "Value"
  )

sua_balanced_aux[, `:=`(flagObservationStatus = "I", flagMethod = "i")]

sua_balanced <- rbind(sua_balanced, sua_balanced_aux)

saveRDS(
  sua_balanced,
  file.path(R_SWS_SHARE_PATH, "FBS_validation", COUNTRY, "sua_balanced.rds")
)


######################### / Save BAL for validation #######################



imbalances <-
  unique(
    standData[,
      list(
        geographicAreaM49,
        measuredElementSuaFbs = "5166",
        measuredItemFbsSua = measuredItemSuaFbs,
        timePointYears,
        Value = imbalance,
        flagObservationStatus = "I",
        flagMethod = "i",
        Protected = FALSE
      )
    ]
  )

imbalances_to_send <-
  standData[
    !is.na(Value) & outside(imbalance, -100, 100) & timePointYears >= 2014,
    .(country = geographicAreaM49, year = timePointYears, measuredItemSuaFbs,
      element = measuredElementSuaFbs, value = Value,
      flag = paste(flagObservationStatus, flagMethod, sep = ","))
  ]

imbalances_to_send <-
  data.table::dcast(
    imbalances_to_send,
    country + measuredItemSuaFbs + year ~ element,
    value.var = c("value", "flag")
  )

names(imbalances_to_send) <- sub("value_", "", names(imbalances_to_send))

imbalances_to_send <-
  merge(
    imbalances_to_send,
    unique(standData[, .(country = geographicAreaM49, year = timePointYears,
                         measuredItemSuaFbs, supply, utilizations, imbalance,
                         imbalance_percent)]),
    by = c("country", "year", "measuredItemSuaFbs"),
    all.x = TRUE
  )

d_imbal_info <-
  imbalances_to_send[,
    .(country, year, measuredItemSuaFbs,
      supply = round(supply, 2),
      imbalance = round(imbalance, 2),
      perc_imb = round(abs(imbalance / supply) * 100, 2))
  ]

imbalances_info <-
  c(
    all_items = nrow(unique(standData[, .(geographicAreaM49, timePointYears, measuredItemSuaFbs)])),
    imb_tot = nrow(d_imbal_info),
    imb_pos_supply = nrow(d_imbal_info[supply > 0]),
    imb_gt_5percent = nrow(d_imbal_info[supply > 0][perc_imb > 5]),
    imb_avg_percent = d_imbal_info[supply > 0, mean(abs(perc_imb))]
  )

setnames(imbalances_to_send, "measuredItemSuaFbs", "measuredItemFbsSua")

imbalances_to_send <-
  nameData('suafbs', 'sua_unbalanced', imbalances_to_send, except = c('measuredElementSuaFbs'))

imbalances_to_send[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

data_negtrade <-
  imbalances_to_send[
    utilizations == 0 &
      imbalance < 0 &
      round(imbalance, 10) == round(supply, 10)
  ]

data_negtrade[, imbalance_percent := NULL]

non_existing_for_imputation <-
  data.table(measuredItemFbsSua = non_existing_for_imputation)

non_existing_for_imputation <-
  nameData('suafbs', 'sua_unbalanced', non_existing_for_imputation)

non_existing_for_imputation[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

tmp_file_name_imb       <- tempfile(pattern = paste0("IMBALANCE_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_shares    <- tempfile(pattern = paste0("SHARES_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_negative  <- tempfile(pattern = paste0("NEGATIVE_AVAILAB_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_non_exist <- tempfile(pattern = paste0("NONEXISTENT_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_fix_shares  <- tempfile(pattern = paste0("FIXED_PROC_SHARES_", COUNTRY, "_"), fileext = '.csv')
tmp_file_name_NegNetTrade <- tempfile(pattern = paste0("NEG_NET_TRADE_", COUNTRY, "_"), fileext = '.csv')

if (!file.exists(dirname(tmp_file_name_imb))) {
  dir.create(dirname(tmp_file_name_imb), recursive = TRUE)
}

write.csv(imbalances_to_send,          tmp_file_name_imb)
write.csv(computed_shares_send,        tmp_file_name_shares)
write.csv(negative_availability,       tmp_file_name_negative)
write.csv(non_existing_for_imputation, tmp_file_name_non_exist)
write.csv(fixed_proc_shares,           tmp_file_name_fix_shares)
write.csv(data_negtrade,               tmp_file_name_NegNetTrade)

saveRDS(
  computed_shares_send,
  file.path(R_SWS_SHARE_PATH, 'mongeau', paste0('computed_shares_send_', COUNTRY, '.rds'))
)




# XXX fix this
codes <- tibble::tribble(
  ~code,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodManufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5164", "tourist",
  "5071", "stockChange"
)

setDT(codes)

standData <- standData[codes, on = c('measuredElementSuaFbs' = 'name')]


standData <-
  standData[,
    list(
      geographicAreaM49,
      measuredElementSuaFbs = code,
      measuredItemFbsSua = measuredItemSuaFbs,
      timePointYears,
      Value,
      flagObservationStatus,
      flagMethod,
      Protected
    )
  ]

standData <- standData[!is.na(Value)]

# These cases should not happen (i.e., all flags should already be
# set), but in any case, add specific flags so to check.
standData[is.na(flagObservationStatus), flagObservationStatus := "M"]
standData[is.na(flagMethod), flagObservationStatus := "q"]


# Calculate calories

calories_per_capita <-
  merge(
    # Food
    standData[
      measuredElementSuaFbs == '5141',
      list(
        geographicAreaM49,
        measuredElementSuaFbs = "664",
        measuredItemFbsSua,
        timePointYears,
        food = Value,
        flagObservationStatus = "T",
        flagMethod = "i"
      )
    ],
    # Calories
    nutrientData[,
      list(
        geographicAreaM49,
        measuredItemFbsSua = measuredItemCPC,
        timePointYears = timePointYearsSP,
        calories = Value
      )
    ],
    by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua'),
    all.x = TRUE
  )


calories_per_capita <-
  merge(
    calories_per_capita,
    popSWS[, list(geographicAreaM49, timePointYears, population = Value)],
    by = c('geographicAreaM49', 'timePointYears'),
    all.x = TRUE
  )


calories_per_capita[, Value := food * calories / population / 365 * 10]

calories_per_capita[, Protected := FALSE]

calories_per_capita[, c("food", "calories", "population") := NULL]

standData <-
  rbind(
    standData,
    imbalances,
    all_opening_stocks,
    calories_per_capita
  )

#standData[dplyr::near(Value, 0) & Protected == FALSE, Value := NA_real_]

standData <- standData[timePointYears >= 2014 & !is.na(Value)]

standData[, Protected := NULL]


# Download also calories
elemKeys_all <- c(elemKeys, "664")


# Get ALL data from SUA BALANCED, do an anti-join, set to NA whatever was not
# generated here so to clean the session on unbalanced of non-existing cells

if (CheckDebug()) {
  key_all <-
    DatasetKey(
      domain = "suafbs",
      dataset = "sua_balanced",
      dimensions =
        list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", keys = elemKeys_all),
          measuredItemFbsSua = Dimension(name = "measuredItemFbsSua", keys = itemKeys),
          # Get from 2010, just for the SUA aggregation
          timePointYears = Dimension(name = "timePointYears", keys = as.character(2010:2017))
        )
    )
} else {
  key_all <- swsContext.datasets[[1]]
  
  key_all@dimensions$timePointYears@keys <- as.character(2010:2017)
  key_all@dimensions$measuredItemFbsSua@keys <- itemKeys
  key_all@dimensions$measuredElementSuaFbs@keys <- elemKeys_all
  key_all@dimensions$geographicAreaM49@keys <- COUNTRY
}


data_suabal <- GetData(key_all)

######### Combine old and new calories and data, and get outliers #############

combined_calories <-
  rbind(
    data_suabal[measuredElementSuaFbs == "664" & timePointYears <= 2013],
    calories_per_capita[, names(data_suabal), with = FALSE]
  )

combined_calories <-
  combined_calories[order(geographicAreaM49, measuredItemFbsSua, timePointYears)]

combined_calories <-
  combined_calories[,
    `:=`(
      perc.change = (Value / shift(Value) - 1) * 100,
      Historical_value_2010_2013 = mean(Value[timePointYears <= 2013], na.rm = TRUE)
    ),
    by = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")
  ]

combined_calories[, outlier := ""]

combined_calories[
  (shift(Value) > 5 | Value > 5) & abs(perc.change) > 10 & timePointYears >= 2014,
  outlier := "OUTLIER",
  by = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")
]

combined_calories <-
  combined_calories[
    unique(combined_calories[outlier == "OUTLIER", .(geographicAreaM49, measuredItemFbsSua)]),
    on = c("geographicAreaM49", "measuredItemFbsSua")
  ]

old_sua_bal <- data_suabal[timePointYears <= 2013]

old_sua_bal <-
  old_sua_bal[,
    supply :=
      sum(
        Value[measuredElementSuaFbs %chin% c("5510", "5610")],
        - Value[measuredElementSuaFbs %chin% c("5910", "5071")],
        na.rm = TRUE
      ),
  by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
]

old_sua_bal <-
  old_sua_bal[
    measuredElementSuaFbs %in% c("5510", "5023", "5016", "5165", "5520", "5525", "5164", "5141")
  ]

old_sua_bal <-
  old_sua_bal[,
    .(
      mean_ratio = mean(Value / supply, na.rm = TRUE),
      Meanold = mean(Value, na.rm = TRUE)
    ),
    by = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")
  ]

new_sua_bal <-
  merge(
    standData,
    old_sua_bal,
    by = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs"),
    all.x = TRUE
  )

new_sua_bal[
  measuredElementSuaFbs %in% c("5510", "5023", "5016", "5165", "5520", "5525", "5164", "5141"),
  outlier := outside(Value / Meanold, 0.5, 2)
]

new_sua_bal[,
  supply :=
    sum(
      Value[measuredElementSuaFbs %chin% c("5510", "5610")],
      - Value[measuredElementSuaFbs %chin% c("5910", "5071")],
      na.rm = TRUE
    ),
  by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
]

new_sua_bal[, outl_on_supp := abs(Value / supply - mean_ratio) >= 0.1]

new_sua_bal <-
  new_sua_bal[measuredElementSuaFbs %in% c("5510", "5023", "5016", "5165", "5520", "5525", "5164", "5141")]

new_sua_bal <-
  new_sua_bal[
    abs(Meanold - Value) > 10000 &
    ((measuredElementSuaFbs !=5510 & outlier == TRUE & outl_on_supp == TRUE &
      Value > 1000 & abs(Meanold - Value) > 10000) |
    (outlier == TRUE & measuredElementSuaFbs == 5510))
  ]

if (nrow(new_sua_bal) > 0) {

  new_sua_bal <-
    new_sua_bal[,
      .(geographicAreaM49, measuredItemFbsSua, measuredElementSuaFbs,
        timePointYears, Historical_value_2010_2013 = Meanold, outlier = "OUTLIER")
    ]

  out_elems_items <-
    rbind(
      data_suabal[timePointYears <= 2013][
        unique(new_sua_bal[, .(geographicAreaM49, measuredItemFbsSua, measuredElementSuaFbs)]),
        on = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")],
      standData[
        unique(new_sua_bal[, .(geographicAreaM49, measuredItemFbsSua, measuredElementSuaFbs)]),
        on = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")]
    )

  out_elems_items <-
    merge(
      out_elems_items,
      new_sua_bal,
      by = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs", "timePointYears"),
      all.x = TRUE
    )

  out_elems_items[is.na(outlier), outlier := ""]

  out_elems_items[,
    Historical_value_2010_2013 := unique(na.omit(Historical_value_2010_2013)),
    c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")
  ]

  out_elems_items <-
    out_elems_items[order(geographicAreaM49, measuredItemFbsSua, measuredElementSuaFbs, timePointYears)]

  out_elems_items[,
    perc.change := (Value / shift(Value) - 1) * 100,
    by = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")
  ]
} else {
  out_elems_items <-
    new_sua_bal[0, .(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua, timePointYears, Value, flagObservationStatus, flagMethod, perc.change = NA_real_, Historical_value_2010_2013 = NA_real_, outlier)]
}

out_elems_items <- rbind(combined_calories, out_elems_items, fill = TRUE)

out_elems_items[, Value := round(Value, 2)]
out_elems_items[, perc.change := round(perc.change, 2)]
out_elems_items[, Historical_value_2010_2013 := round(Historical_value_2010_2013, 2)]


tmp_file_outliers <-
  tempfile(pattern = paste0("OUTLIERS_", COUNTRY, "_"), fileext = '.csv')

if (!file.exists(dirname(tmp_file_outliers))) {
  dir.create(dirname(tmp_file_outliers), recursive = TRUE)
}

out_elems_items <-
  nameData("suafbs", "sua_unbalanced", out_elems_items, except = "timePointYears")

write.csv(out_elems_items, tmp_file_outliers)


######### / Combine old and new calories and data, and get outliers ###########


if (nrow(data_suabal[timePointYears >= 2014]) > 0) {
  
  data_suabal_missing <-
    data_suabal[
      timePointYears >= 2014
      ][
        !standData,
        on = c('geographicAreaM49', 'measuredElementSuaFbs',
               'measuredItemFbsSua', 'timePointYears')
      ]
  
  if (nrow(data_suabal_missing) > 0) {
    
    data_suabal_missing[,
      `:=`(
        Value = NA_real_,
        flagObservationStatus = NA_character_,
        flagMethod = NA_character_
      )
    ]
    
    standData <- rbind(standData, data_suabal_missing)
  }
}


########## DES calculation

des <- 
  rbind(
    data_suabal[
      measuredElementSuaFbs == '664' & timePointYears %in% 2010:2013
      ][,
        list(Value = sum(Value, na.rm = TRUE)),
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua')
        ],
    standData[
      measuredElementSuaFbs == '664' & timePointYears >= 2014
    ][,
      list(Value = sum(Value, na.rm = TRUE)),
      by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua')
    ]
  )

des <-
  rbind(
    des,
    des[,
      list(measuredItemFbsSua = 'S2901', Value = sum(Value)),
      by = c('geographicAreaM49', 'timePointYears')
    ]
  )

des[, Value := round(Value, 2)]

f_des <- file.path(R_SWS_SHARE_PATH, "FBS_validation", COUNTRY, "des.rds")

if (file.exists(f_des)) {
  file.copy(f_des, sub("des\\.rds", "des_prev.rds", f_des))
}

saveRDS(des, f_des)

##### Plot of main DES absolute variations

des_diff <-
  des[
    order(geographicAreaM49, measuredItemFbsSua, timePointYears)
  ][,
    .(year = timePointYears, diff = Value - shift(Value)),
    .(geographicAreaM49, item = measuredItemFbsSua)
  ][
    year != min(year)
  ]

des_diff <-
  des_diff[item %in% des_diff[abs(diff) > 20]$item]

des_diff[item == "S2901", item := "GRAND TOTAL"]

plot_main_des_diff <-
  ggplot(des_diff, aes(x = year, diff, group = item, color = item)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  ggtitle("Absolute variation of DES and main variations of items",
          subtitle = COUNTRY_NAME)

tmp_file_plot_main_des_diff <-
  tempfile(pattern = paste0("PLOT_MAIN_DES_DIFF_", COUNTRY, "_"), fileext = '.pdf')

ggsave(tmp_file_plot_main_des_diff, plot = plot_main_des_diff)

##### / Plot of main DES absolute variations

##### Plot of main DES

main_des_items <-
  des[,
    .(geographicAreaM49, year = timePointYears, item = measuredItemFbsSua, Value)
  ][
    item %in% des[Value > 100]$measuredItemFbsSua][order(geographicAreaM49, item, year)
  ]

main_des_items[item == "S2901", item := "GRAND TOTAL"]

plot_main_des_items <-
  ggplot(main_des_items, aes(x = year, Value, group = item, color = item)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  ggtitle("Main DES items (> 100 Calories)",
          subtitle = COUNTRY_NAME)

tmp_file_plot_main_des_items <-
  tempfile(pattern = paste0("PLOT_MAIN_DES_ITEMS_", COUNTRY, "_"), fileext = '.pdf')

ggsave(tmp_file_plot_main_des_items, plot = plot_main_des_items)

##### / Plot of main DES

##### Plot of main diff avg DES
des_main_diff_avg <-
  des[
    measuredItemFbsSua != "S2901",
    .(pre = mean(Value[timePointYears <= 2013]), post = mean(Value[timePointYears >= 2014])),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ]

des_main_diff_avg <- des_main_diff_avg[abs(post - pre) > 20]

des_main_diff_avg <- melt(des_main_diff_avg, c("geographicAreaM49", "measuredItemFbsSua"))

des_main_diff_avg <- nameData("suafbs", "sua_unbalanced", des_main_diff_avg, except = "geographicAreaM49")

plot_des_main_diff_avg <-
  ggplot(des_main_diff_avg,
         aes(x = measuredItemFbsSua_description, y = value,
             group = rev(variable), fill = variable)) +
  geom_col(position = "dodge") +
  coord_flip() +
  ggtitle("Main diff (> 20 Cal) in DES average pre (<= 2013) and post (>= 2014)",
          subtitle = COUNTRY_NAME)


tmp_file_plot_des_main_diff_avg <-
  tempfile(pattern = paste0("PLOT_DES_MAIN_DIFF_AVG_", COUNTRY, "_"), fileext = '.pdf')

ggsave(tmp_file_plot_des_main_diff_avg, plot = plot_des_main_diff_avg)


##### Plot of main diff avg DES


des_cast <-
  data.table::dcast(
    des,
    geographicAreaM49 + measuredItemFbsSua ~ timePointYears,
    fun.aggregate = sum,
    value.var = "Value"
  )

des_cast <- nameData("suafbs", "sua_balanced", des_cast)

# The "000" is a trick so that it appears in first place after ordering
des_cast[measuredItemFbsSua == "S2901", measuredItemFbsSua_description := paste0("000", measuredItemFbsSua_description)]

des_cast <- des_cast[order(measuredItemFbsSua_description)]

des_cast[, measuredItemFbsSua_description := sub("^000", "", measuredItemFbsSua_description)]


# Main items, more then 90%

des_main_90 <-
  des[
    measuredItemFbsSua != "S2901" & timePointYears >= 2014,
    .(tot = sum(Value)),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ][
    order(-tot)
  ][,
    cumsum := cumsum(tot)
  ]

des_main_90 <-
  des_main_90[
    des[
      measuredItemFbsSua != "S2901" & timePointYears >= 2014,
      .(maintot = sum(Value)),
      by = c("geographicAreaM49")
    ],
    on = "geographicAreaM49"
  ][
    cumsum < maintot * 0.9
  ][,
    c("tot", "cumsum", "maintot") := NULL
  ]

des_main <-
  rbind(
    des_cast[measuredItemFbsSua == "S2901"],
    des_cast[des_main_90, on = c("geographicAreaM49", "measuredItemFbsSua")]
  )

des_main <-
  rbind(
    des_main,
    des_main[,
      lapply(.SD, function(x) round(sum(x[-1]) / x[1] * 100, 2)),
      .SDcols = c(sort(unique(des$timePointYears)))
    ],
    fill = TRUE
  )

des_main[
  is.na(measuredItemFbsSua),
  measuredItemFbsSua_description := "PERCENTAGE OF MAIN OVER TOTAL"
]

des_cast[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]
des_main[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

tmp_file_des      <- tempfile(pattern = paste0("DES_", COUNTRY, "_"), fileext = '.csv')
tmp_file_des_main <- tempfile(pattern = paste0("DES_MAIN_ITEMS_", COUNTRY, "_"), fileext = '.csv')

write.csv(des_cast, tmp_file_des)
write.csv(des_main, tmp_file_des_main)

########## / DES calculation




out <- SaveData(domain = "suafbs", dataset = "sua_balanced", data = standData, waitTimeout = 20000)

if (exists("out")) {
  
  body_message <-
    sprintf(
      "Plugin completed in %1.2f minutes.

      ################################################
      # Imbalances (greater than 100 t in abs value) #
      ################################################

      unbalanced items: %s (out of %s)
      unbalanced items for items with positive supply: %s
      unbalanced items for items with imbalance > 5%%: %s
      average percent imbalance in absolute value: %1.2f

      ###############################################
      ############       Parameters       ###########
      ###############################################

      country = %s
      balancing method = %s
      thresold method = %s
      fill_extraction_rates= %s

      ###############################################
      ###########       ShareDownUp       ###########
      ###############################################

      shareDownUp can be modified here:

      %s

      ###############################################
      ##############   Removed feed   ###############
      ###############################################

      The following NEW feed items were removed as including them
      would have created a huge negative imbalance:

      %s

      The following NEW feed items are dubious, they might be
      removed, but you will need to do it manually:

      %s

      ###############################################
      ##############       Flags       ##############
      ###############################################

      E,-: Balancing: utilization modified
      E,b: Final balancing (industrial, Feed)
      E,c: Balancing: production modified to compensate net trade
      E,e: Outlier replaced
      E,h: Balancing: imbalance to food, given food is only utilization
      E,i: Food processing generated
      E,n: Balancing of small imbalances
      E,s: Balancing: stocks modified
      E,u: Stocks variation generated
      I,c: Derived production generated
      I,e: Module imputation
      I,i: Residual item (identity)
      M,q: Cases for which flags were not set (should never happen)
      T,c: Opening stocks updated
      T,i: Calories per capita created
      T,p: USDA stocks data

      ###############################################
      ##############       Files       ##############
      ###############################################

      The following files are attached:

      - PLOT_MAIN_DES_ITEMS_*.pdf = Plot of main DES items (> 100 Calories)

      - PLOT_MAIN_DES_DIFF_*.pdf = Plot of main DES Calories variations

      - PLOT_DES_MAIN_DIFF_AVG_*.pdf = Plot of main variations (> 20 Calories)
          in the AVERAGE DES pre (year <= 2013) and post (year >= 2014)

      - DES_*.csv = calculation of DES (total and by items)

      - DES_MAIN_ITEMS_*.csv = as DES_*.csv, but only with items that
          accounted for nearly 90%% on average over 2014-2017

      - OUTLIERS_*.csv = outliers in Calories (defined as those that account
          for more than 5 Calories with an increase of more than 15%%)

      - SHARES_*.csv = Parents/Children shares, availability, etc.

      - FILLED_ER_*.csv = filled extraction rates

      - IMBALANCE_*.csv = imbalances, supply and utilizations

      - NEGATIVE_AVAILAB_*.csv = items with negative availability

      - NONEXISTENT_*.csv = items for which no production, imports,
          or exports exist after 2013

      - NEW_ELEMENTS_*.csv = elements that appear for the first time

      - FIXED_PROC_SHARES_*.csv = items for which the processing share
          was fixed, to keep it consistent with its main co-product

      - NEG_NET_TRADE_*.csv = negative net trade

      ",
      difftime(Sys.time(), start_time, units = "min"),
      imbalances_info[['imb_tot']],
      prettyNum(imbalances_info[['all_items']], big.mark = ","),
      imbalances_info[['imb_pos_supply']],
      imbalances_info[['imb_gt_5percent']],
      imbalances_info[['imb_avg_percent']],
      COUNTRY,
      BALANCING_METHOD,
      THRESHOLD_METHOD,
      FILL_EXTRACTION_RATES,
      sub('/work/SWS_R_Share/', '', shareDownUp_file),
      msg_new_feed_remove,
      msg_new_feed_dubious
    )
  
  if (!CheckDebug()) {
    send_mail(
      from = "do-not-reply@fao.org",
      to = swsContext.userEmail,
      subject = "Results from newBalancing plugin",
      body = c(body_message,
               tmp_file_plot_main_des_items,
               tmp_file_plot_main_des_diff,
               tmp_file_plot_des_main_diff_avg,
               tmp_file_des,
               tmp_file_des_main,
               tmp_file_outliers,
               tmp_file_name_shares,
               tmp_file_name_imb,
               tmp_file_name_extr,
               tmp_file_name_negative,
               tmp_file_name_non_exist,
               tmp_file_name_new,
               tmp_file_name_fix_shares,
               tmp_file_name_NegNetTrade
              )
    )
  }
  
  print(paste(out$inserted + out$ignored, "observations written and problems with", out$discarded))
  
} else {
  
  print("The newBalancing plugin had a problem when saving data.")
  
}

