library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(dplyr)
library(data.table)
library(tidyr)
library(openxlsx)

# The only parameter is the string to print
# COUNTRY is taken from environment (parameter)
dbg_print <- function(x) {
  message(paste0("NEWBAL (", COUNTRY, "): ", x))
}

start_time <- Sys.time()

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")

if (CheckDebug()) {
  R_SWS_SHARE_PATH <- "//hqlprsws1.hq.un.fao.org/sws_r_share"

  mydir <- "modules/newBalancing_test"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
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

THRESHOLD_METHOD <- 'share'

FIX_OUTLIERS <- TRUE

FILL_EXTRACTION_RATES <- TRUE

YEARS <- as.character(2000:2017)

TMP_DIR <- file.path(tempdir(), USER)
if (!file.exists(TMP_DIR)) dir.create(TMP_DIR, recursive = TRUE)

tmp_file_imb         <- file.path(TMP_DIR, paste0("IMBALANCE_", COUNTRY, ".csv"))
tmp_file_shares      <- file.path(TMP_DIR, paste0("SHARES_", COUNTRY, ".xlsx"))
tmp_file_negative    <- file.path(TMP_DIR, paste0("NEGATIVE_AVAILAB_", COUNTRY, ".csv"))
tmp_file_non_exist   <- file.path(TMP_DIR, paste0("NONEXISTENT_", COUNTRY, ".csv"))
tmp_file_fix_shares  <- file.path(TMP_DIR, paste0("FIXED_PROC_SHARES_", COUNTRY, ".csv"))
tmp_file_NegNetTrade <- file.path(TMP_DIR, paste0("NEG_NET_TRADE_", COUNTRY, ".csv"))


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

tourist_cons_table <- ReadDatatable("keep_tourist_consumption")

stopifnot(nrow(tourist_cons_table) > 0)

TourismNoIndustrial <- tourist_cons_table[small == "X"]$tourist


# Always source files in R/ (useful for local runs).
# Your WD should be in faoswsStandardization/
sapply(dir("R", full.names = TRUE), source)


dbg_print("define functions")

######### FUNCTIONS: at some point, they will be moved out of this file. ####

# Replacement for merge(x, y, by = VARS, all.x = TRUE) that do not set keys
# By default it behaves as dplyr::left_join(). If nomatch = 0, non-matching
# rows will not be returned
dt_left_join <- function(x, y, by = NA, allow.cartesian = FALSE,
                         nomatch = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }

  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }

  res <- y[x, on = by, allow.cartesian = allow.cartesian, nomatch = nomatch]

  setcolorder(res, c(names(x), setdiff(names(y), names(x))))

  res
}

dt_full_join <- function(x, y, by = NA) {
  if (anyNA(by)) {
    stop("'by' is required")
  }

  if (any(!is.data.table(x), !is.data.table(y))) {
    stop("'x' and 'y' should be data.tables")
  }

  res <- merge(x, y, by = by, all = TRUE)

  # merge sets the key to `by`
  setkey(res, NULL)

  res
}


coeffs_stocks_mod <- function(x) {
  tmp <- lm(data = x[timePointYears <= 2013], supply_inc ~ supply_exc + trend)

  as.list(tmp$coefficients)
}

# This function will recalculate opening stocks from the first
# observation. Always.
update_opening_stocks <- function(x) {
  x <- x[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]

  groups <- unique(x[, c("geographicAreaM49", "measuredItemSuaFbs"), with = FALSE])

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
  rbindlist(res)
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

RemainingToProcessedParent <- function(data) {
  data[, 
    parent_already_processed :=
      ifelse(
        is.na(parent_qty_processed),
        parent_qty_processed,
        sum(processed_to_child / extractionRate, na.rm = TRUE)
      ),
    by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
  ]
  
  data[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]
  
  data[remaining_processed_parent < 0, remaining_processed_parent := 0]

  data[,
    only_child_left :=
      sum(is.na(processed_to_child)) == 1 &
        is.na(processed_to_child) &
        !is.na(production_of_child) &
        !is.na(parent_qty_processed) &
        production_of_child > 0,
    by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
  ]
  
  data[
    only_child_left == TRUE,
    processed_to_child := remaining_processed_parent * extractionRate
  ]
  
  data[,
    parent_already_processed :=
      ifelse(
        is.na(parent_qty_processed),
        parent_qty_processed,
        sum(processed_to_child / extractionRate, na.rm = TRUE)
      ),
    by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
  ]
  
  data[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]

  data[remaining_processed_parent < 0, remaining_processed_parent := 0]
  
  return(data)
}


RemainingProdChildToAssign <- function(data) {
  
  data[,
    available_processed_child := sum(processed_to_child, na.rm = TRUE),
    by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
  ]
  
  data[, remaining_to_process_child := round(production_of_child - available_processed_child)]

  data[remaining_to_process_child < 0, remaining_to_process_child := 0]
  
  data[,
    only_parent_left :=
      sum(is.na(processed_to_child)) == 1 &
        is.na(processed_to_child) &
        !is.na(parent_qty_processed) &
        parent_qty_processed >= 0
    ]
  
  data[only_parent_left==TRUE,processed_to_child:=ifelse(only_parent_left==TRUE,remaining_to_process_child,processed_to_child)]
  
  data[,
    available_processed_child := sum(processed_to_child, na.rm = TRUE),
    by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
  ]
  
  data[, remaining_to_process_child := round(production_of_child - available_processed_child)]

  data[remaining_to_process_child < 0, remaining_to_process_child := 0]

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
      return(
        share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]]
      )
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

balance_proportional <- function(data) {

  x <- copy(data)

  x <-
    x[
      can_balance == TRUE,
      c("measuredElementSuaFbs", "Value",
        "mov_share", "imbalance", "min_threshold",
        "max_threshold", "can_balance"),
      with = FALSE
    ]

  mov_share_sum <- sum(x$mov_share, na.rm = TRUE)

  if (mov_share_sum > 0) {
    # Recalculate mov_share, as we are excluding protected values
    x[, mov_share := mov_share / mov_share_sum]
  } else {
    # It means that there are no back shares, so use current values
    x[!is.na(Value) & can_balance == TRUE, mov_share := Value / sum(Value, na.rm = TRUE)]
  }

  x[is.na(Value), Value := 0]

  x[, adjusted_value := 0]

  x[Value + mov_share * imbalance >= 0, adjusted_value := Value + mov_share * imbalance]

  x[adjusted_value > Value & adjusted_value > max_threshold, adjusted_value := max_threshold]

  x[adjusted_value < Value & adjusted_value < min_threshold, adjusted_value := min_threshold]

  x <-  x[, c("measuredElementSuaFbs", "adjusted_value"), with = FALSE][data, on = "measuredElementSuaFbs"]

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


newBalancing <- function(data, Utilization_Table) {
  
  # Contains a variable that indicates whether stocks changed
  data[, change_stocks := NA_integer_]
  
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
      (measuredItemSuaFbs %chin% Utilization_Table[food_item == 'X', cpc_code] |
      # food exists & ...
      # !is.na(Value[measuredElementSuaFbs == 'food']) &
      Food_Median > 0 & !is.na(Food_Median)) &
      # ... is the only utilization
      all(is.na(Value[!(measuredElementSuaFbs %chin%
                          c('loss', 'food', 'production', 'imports',
                            'exports', 'stockChange','foodManufacturing', 'tourist'))])),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
  
  # Checking if the commodity has past value before assigning the residual
  # imbalance at the end of the balancing procees
  data[,
    `:=`(
      feed_resid =
        # It's a feed item or have past value & ...
        #(measuredItemSuaFbs %in% Utilization_Table[feed == 'X', cpc_code] |
        (Feed_Median > 0 & !is.na(Feed_Median)) &
        #feed is the only utilization....
        all(is.na(Value[!(measuredElementSuaFbs %chin%
                             c('feed', 'production', 'imports', 'exports',
                               'stockChange','foodManufacturing'))])),
      # It's a industrial item or have past value & ...
      industrial_resid = Industrial_Median > 0 & !is.na(Industrial_Median)),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ] 
   
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
  
  
  if (COUNTRY %in% TourismNoIndustrial) {
    data[,
      Value :=
        ifelse(
          measuredElementSuaFbs == "industrial" &
            !is.na(Value[measuredElementSuaFbs == "industrial"]) &
            !is.na(Value[measuredElementSuaFbs == "tourist"]),
          ifelse(
            Value[measuredElementSuaFbs == "industrial"] -
              Value[measuredElementSuaFbs == "tourist"] < 0,
            0,
            Value[measuredElementSuaFbs == "industrial"] -
              Value[measuredElementSuaFbs == "tourist"]
          ),
          Value
        ),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    ]
  }
  
  calculateImbalance(data)
  
  # Try to assign the maximum of imbalance to stocks
  # NOTE: in the conditions below, 2 was 0.2, indicating that no more than
  # 20% should go to stocks. Now, the condition was relaxed a lot (200%)
  data <-
    dt_left_join(
      data,
      all_opening_stocks[,
        .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
          timePointYears, opening_stocks = Value)
      ],
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    )
  
  data[,
    Value_0 := ifelse(is.na(Value), 0, Value)
  ][
    Protected == FALSE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "stockChange" &
      stockable == TRUE,
    change_stocks :=
      # The numbers indicate the case. Assignmnet (value and flags) will be done below
      case_when(
        # case 1: we don't want stocks to change sign.
        sign(Value_0) * sign(Value_0 + imbalance) == -1                                                  ~ 1L,
        # case 2: if value + imbalance takes LESS than opening stock, take all from stocks
        Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) <= opening_stocks           ~ 2L,
        # case 3: if value + imbalance takes MORE than opening stock, take max opening stocks
        Value_0 <= 0 & (Value_0 + imbalance <= 0) & abs(Value_0 + imbalance) > opening_stocks            ~ 3L,
        # case 4: if value + imbalance send LESS than 200% of supply, send all
        Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks <= supply * 2) ~ 4L,
        # case 5: if value + imbalance send MORE than 200% of supply, send 200% of supply
        Value_0 >= 0 & (Value_0 + imbalance >= 0) & (Value_0 + imbalance + opening_stocks > supply * 2)  ~ 5L
      )
  ]

  data[change_stocks == 1L, Value := 0]
  data[change_stocks == 2L, Value := Value_0 + imbalance]
  data[change_stocks == 3L, Value := - opening_stocks]
  data[change_stocks == 4L, Value := Value_0 + imbalance]
  # Only case for which grouping is required
  data[
    change_stocks == 5L,
    Value := max(supply * 2 - opening_stocks, 0),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
  ]

  data[
    change_stocks %in% 1L:5L,
    `:=`(flagObservationStatus = "E", flagMethod = "s")
  ]
    
  data[, Value_0 := NULL]

  data[, opening_stocks := NULL]
  
  # Recalculate imbalance
  calculateImbalance(data)

  # Assign imbalance to food if food "only" (not "residual") item
  data[
    Protected == FALSE &
      food_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "food",
    `:=`(
      Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
      flagObservationStatus = "E",
      flagMethod = "h"
    )
  ]

  for (j in 1:10) {

    # Recalculate imbalance
    calculateImbalance(data)

    data[, can_balance := FALSE]

    data[
      !is.na(Value) &
        Protected == FALSE &
        !(data.table::between(Value, min_threshold, max_threshold) %in% FALSE) &
        !(measuredElementSuaFbs %chin%
          c("production", "imports", "exports", "stockChange", "foodManufacturing")),
      can_balance := TRUE
    ]

    data[,
      elements_balance := any(can_balance),
      by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
    ]
    
    data[
      dplyr::near(imbalance, 0) == FALSE &
        elements_balance == TRUE,
      adjusted_value := balance_proportional(.SD),
      by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
    ]

    data[
      !is.na(adjusted_value) & adjusted_value != Value,
      `:=`(
        Value = adjusted_value,
        flagObservationStatus = "E",
        flagMethod = "-"
      )
    ]

    data[, adjusted_value := NULL]

  }
      
  # At this point the imbalance (in the best case scenario) should be zero,
  # the following re-calculation is useful only for debugging
  
  calculateImbalance(data)

  # Assign imbalance to food if food "only" (not "residual") item
  data[
    Protected == FALSE &
      food_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "food",
    `:=`(
      Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, 0)),
      flagObservationStatus = "E",
      flagMethod = "h"
    )
  ]
  
  calculateImbalance(data)
  
  # Assign the residual imbalance to industrial if the conditions are met
  data[
    Protected == FALSE &
      industrial_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "industrial",
    `:=`(
      Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, Value)),
      flagObservationStatus = "E",
      flagMethod = "b"
    )
  ]
  
  if (COUNTRY %in% TourismNoIndustrial) {

    data[,
      Value :=
        ifelse(
          measuredElementSuaFbs == "tourist" &
            !is.na(Value[measuredElementSuaFbs == "industrial"]),
          ifelse(
            is.na(Value[measuredElementSuaFbs == "tourist"]),
            Value[measuredElementSuaFbs == "industrial"],
            Value[measuredElementSuaFbs == "tourist"] +
              Value[measuredElementSuaFbs == "industrial"]
          ),
          Value
        ),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    ]

    data[,
      Value :=
        ifelse(
          measuredElementSuaFbs == "industrial" &
            !is.na(Value[measuredElementSuaFbs == "industrial"]) &
            !is.na(Value[measuredElementSuaFbs == "tourist"]),
          NA_real_,
          Value
        ),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
    ]
  }

  calculateImbalance(data)
  
  # Assign the residual imbalance to feed if the conditions are met
  data[
    Protected == FALSE &
      feed_resid == TRUE &
      dplyr::near(imbalance, 0) == FALSE &
      measuredElementSuaFbs == "feed",
    `:=`(
      # XXX: this creates a warning when no assignment is done:
      # Coerced 'logical' RHS to 'double'
      Value = ifelse(is.na(Value) & imbalance > 0, imbalance, ifelse(Value + imbalance >= 0, Value + imbalance, Value)),
      flagObservationStatus = "E",
      flagMethod = "b"
    )
  ]
 
  calculateImbalance(data)
  
  data[, c("supply", "utilizations", "imbalance", "mov_share_rebased") := NULL]
    
  return(data)
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

# TODO: Keep all protected 0 ERs
tree[Value == 0 & !(flagObservationStatus == "E" & flagMethod == "f"), Value := NA]

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
      unique(
        tree[,
          c("geographicAreaM49", "measuredElementSuaFbs",
            "measuredItemParentCPC", "measuredItemChildCPC"),
          with = FALSE
        ]
      ),
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


# XXX: connections removed here that should not exist in
# the commodity tree (so, they should be fixed there)
tree[
  timePointYears >= 2014 &
    ((measuredItemParentCPC == "02211" & measuredItemChildCPC == "22212") |
      #cheese from whole cow milk cannot come from skim mulk of cow
    (measuredItemParentCPC == "22110.02" & measuredItemChildCPC == "22251.01")),
  `:=`(
    Value = NA,
    flagObservationStatus = "M",
    flagMethod = "n"
  )
]


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
  dplyr::anti_join(
    tree[is.na(Value) & measuredElementSuaFbs == "extractionRate"],
    by = c("geographicAreaM49", "measuredElementSuaFbs",
           "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears",
           "Value", "flagObservationStatus", "flagMethod")
  ) %>%
  dplyr::select(-Value) %>%
  dplyr::left_join(
    tree,
    by = c("geographicAreaM49", "measuredElementSuaFbs",
           "measuredItemParentCPC", "measuredItemChildCPC",
           "timePointYears", "flagObservationStatus", "flagMethod")
  ) %>%
  setDT()

tree_to_send <-
  tree_to_send[,
    c("geographicAreaM49", "measuredElementSuaFbs", "measuredItemParentCPC",
      "measuredItemChildCPC", "timePointYears", "Value",
      "flagObservationStatus", "flagMethod"),
    with = FALSE
  ]

setnames(
  tree_to_send,
  c("measuredItemParentCPC", "measuredItemChildCPC"),
  c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree")
)

tree_to_send <-
  nameData("suafbs", "ess_fbs_commodity_tree2", tree_to_send,
           except = c('measuredElementSuaFbs', 'timePointYears'))

tree_to_send[,
  `:=`(
    measuredItemParentCPC_tree = paste0("'", measuredItemParentCPC_tree),
    measuredItemChildCPC_tree = paste0("'", measuredItemChildCPC_tree))
]

tmp_file_extr <- file.path(TMP_DIR, paste0("FILLED_ER_", COUNTRY, ".csv"))

write.csv(tree_to_send, tmp_file_extr)

# XXX remove NAs
tree <- tree[!is.na(Value)]

uniqueLevels <- unique(tree[, c("geographicAreaM49", "timePointYears"), with = FALSE])

levels <- list()

treeLevels <- list()

for (i in seq_len(nrow(uniqueLevels))) {
  filter <- uniqueLevels[i, ]
  
  treeCurrent <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  
  levels <- findProcessingLevel(treeCurrent, "measuredItemParentCPC", "measuredItemChildCPC")

  setnames(levels, "temp", "measuredItemParentCPC")
  
  treeLevels[[i]] <- dt_left_join(treeCurrent, levels, by = "measuredItemParentCPC")
}

tree <- rbindlist(treeLevels)

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
itemMap <- itemMap[, .(measuredItemSuaFbs = code, type)]

##################################### / TREE ################################


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

dbg_print("download data")


# LOAD
data <- GetData(key)


#################### FODDER CROPS ##########################################

# TODO: create SWS datatable for this.
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
    measuredItemFbsSua %chin% fodder_crops_items$code &
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
    dt_left_join(
      fodder_crops_complete,
      fodder_crops_availab[, .(geographicAreaM49, measuredElementSuaFbs,
                               measuredItemFbsSua, timePointYears, Value)],
      by = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears")
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


opening_stocks_2014 <-
  ReadDatatable(
    "opening_stocks_2014",
    where = paste0("m49_code IN (",
                   paste(shQuote(COUNTRY, type = "sh"), collapse = ", "), ")")
  )

stopifnot(nrow(opening_stocks_2014) > 0)

non_null_prev_deltas <-
  unique(
    data[
      measuredElementSuaFbs == "5071" & timePointYears %in% 2009:2013
    ][,
      .SD[sum(!dplyr::near(Value, 0)) > 0],
      by = c("geographicAreaM49", "measuredItemFbsSua")
    ][,
      .(geographicAreaM49, measuredItemFbsSua)
    ]
  )

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

# Keep only those for which recent variations are available
opening_stocks_2014 <-
  opening_stocks_2014[
    non_null_prev_deltas,
    on = c("geographicAreaM49", "measuredItemFbsSua")
  ]

original_opening_stocks <- data[measuredElementSuaFbs == "5113"]

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
    !original_opening_stocks[
      timePointYears == "2014" & Protected == TRUE,
      .(geographicAreaM49, measuredItemFbsSua)
    ],
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
    flagMethod = "-",
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

all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]

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


complete_all_opening <-
  CJ(
    geographicAreaM49 = unique(all_opening_stocks$geographicAreaM49),
    timePointYears = as.character(min(all_opening_stocks$timePointYears):2017),
    measuredItemFbsSua = unique(all_opening_stocks$measuredItemFbsSua)
  )


all_opening_stocks <-
  merge(
    complete_all_opening,
    all_opening_stocks,
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    all = TRUE
  )

all_opening_stocks[, orig_val := Value]

all_opening_stocks <-
  all_opening_stocks[!is.na(Value)][
    all_opening_stocks,
    on = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears"),
    roll = Inf]

all_opening_stocks[
  is.na(orig_val) & !is.na(Value),
  `:=`(
    Protected = FALSE,
    flagObservationStatus = "E",
    flagMethod = "t"
  )
]

all_opening_stocks[, measuredElementSuaFbs := "5113"]

all_opening_stocks[, orig_val := NULL]

all_opening_stocks[, names(all_opening_stocks)[grep("^i\\.", names(all_opening_stocks))] := NULL]

all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]



# Generate stocks variations for items for which opening
# exists for ALL years and variations don't exist

opening_avail_all_years <-
  all_opening_stocks[
    !is.na(Value) & timePointYears >= 2014,
    .SD[.N == (2017 - 2014 + 1)],
    measuredItemFbsSua
  ]

delta_avail_for_open_all_years <-
  data[
    measuredElementSuaFbs == "5071" &
      timePointYears >= 2014 &
      measuredItemFbsSua %chin% opening_avail_all_years$measuredItemFbsSua,
    .SD[.N == (2017 - 2014 + 1)],
    measuredItemFbsSua
  ]

to_generate_by_ident <-
  setdiff(
    opening_avail_all_years$measuredItemFbsSua,
    delta_avail_for_open_all_years$measuredItemFbsSua
  )

data_rm_ident <- data[measuredElementSuaFbs == "5071" & measuredItemFbsSua %chin% to_generate_by_ident]

if (length(to_generate_by_ident) > 0) {

  opening_avail_all_years <-
    opening_avail_all_years[measuredItemFbsSua %chin% to_generate_by_ident]

  opening_avail_all_years[, Protected := NULL]

  next_opening_key <-
    DatasetKey(
      domain = "Stock",
      dataset = "stocksdata",
      dimensions =
        list(
          geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = COUNTRY),
          measuredElementSuaFbs = Dimension(name = "measuredElement", keys = "5113"),
          measuredItemFbsSua = Dimension(name = "measuredItemCPC", keys = unique(opening_avail_all_years$measuredItemFbsSua)),
          timePointYears = Dimension(name = "timePointYears", keys = as.character(as.numeric(max(opening_avail_all_years$timePointYears)) + 1))
        )
    )

  next_opening <- GetData(next_opening_key)

  setnames(
    next_opening,
    c("measuredItemCPC", "measuredElement"),
    c("measuredItemFbsSua", "measuredElementSuaFbs")
  )

  opening_avail_all_years <-
    rbind(opening_avail_all_years, next_opening)

  opening_avail_all_years <-
    opening_avail_all_years[
      order(geographicAreaM49, measuredItemFbsSua, timePointYears)
    ]

  opening_avail_all_years[,
    delta := shift(Value, type = "lead") - Value,
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ]

  opening_avail_all_years[,
    delta :=
      ifelse(
        timePointYears == max(timePointYears) & is.na(delta),
        0,
        delta
      ),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ]

  opening_avail_all_years <-
    dt_left_join(
      opening_avail_all_years,
      flagValidTable,
      by = c("flagObservationStatus", "flagMethod")
    )

  opening_avail_all_years[shift(Protected, type = "lead") == TRUE & Protected == TRUE, `:=`(delta_flag_obs = "T", delta_flag_method = "p")]
  opening_avail_all_years[!(shift(Protected, type = "lead") == TRUE & Protected == TRUE), `:=`(delta_flag_obs = "I", delta_flag_method = "i")]

  delta_identity <-
    opening_avail_all_years[
      timePointYears %in% 2014:2017,
      .(
        geographicAreaM49,
        measuredItemFbsSua,
        timePointYears,
        measuredElementSuaFbs = "5071",
        Value = delta,
        flagObservationStatus = delta_flag_obs,
        flagMethod = delta_flag_method
        )
    ]

  delta_identity <-
    delta_identity[!data_rm_ident,
                   on = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")]

  data <- rbind(data, delta_identity)

  data <-
    data[order(geographicAreaM49, measuredItemFbsSua,
               timePointYears, measuredElementSuaFbs)]

}

# / Generate stocks variations for items for which opening
# / exists for ALL years and variations don't exist



# Recalculate opening stocks

data_for_opening <-
  dt_left_join(
    all_opening_stocks[,
      .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
        timePointYears, new_opening = Value)
    ],
    data[
      measuredElementSuaFbs == '5071' &
        timePointYears %in% 2014:2017 &
        !is.na(Value),
      .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
        timePointYears, delta = Value)
    ],
    by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
  )

data_for_opening[is.na(delta), delta := 0]

data_for_opening <- data_for_opening[timePointYears >= 2014]

data_for_opening <- update_opening_stocks(data_for_opening)

all_opening_stocks <-
  dt_left_join(
    all_opening_stocks,
    data_for_opening[,
      .(
        geographicAreaM49,
        measuredItemFbsSua = measuredItemSuaFbs,
        timePointYears,
        new_opening
      )
    ],
    by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
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

# / Recalculate opening stocks



data <-
  dt_left_join(data, flagValidTable,
               by = c("flagObservationStatus", "flagMethod"))

data[flagObservationStatus %chin% c("", "T"), `:=`(Official = TRUE, Protected = TRUE)]
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]


# We remove "5113" (opening stocks) as will be stored in separate table.
data <- data[measuredElementSuaFbs != "5113"]

dbg_print("elementToCodeNames")

# XXX FIXME: the elementCodesToNames below is
# not working proberply, see issue #38
codes <- as.data.table(tibble::tribble(
  ~measuredElementSuaFbs,  ~name,
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
))

data <- dt_left_join(data, codes, by = "measuredElementSuaFbs")

data[, measuredElementSuaFbs := name]

data[, name := NULL]

# NOTE: if this should be used again (#38), camelCase the element names (#45)
#data <-
#  elementCodesToNames(
#    data,
#    itemCol = "measuredItemFbsSua",
#    elementCol = "measuredElementSuaFbs"
#  )


# XXX: there are some NAs here, but probably there shouldn't
data <- data[!is.na(measuredElementSuaFbs)]

setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")

dbg_print("convert sugar")

# XXX
##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data <- convertSugarCodes(data)


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

msg_new_feed_remove <- "No NEW feed removed"
msg_new_feed_dubious <- "No other NEW items should be removed"

if (nrow(new_feed) > 0) {

  prev_data_avg <-
    data[
      new_feed, on = c("geographicAreaM49", "measuredItemSuaFbs")
    ][
      timePointYears >= 2010 & timePointYears <= 2013
    ][
      order(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears),
      Value := na.fill_(Value),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ][,
      .(Value = sum(Value) / sum(!is.na(Value)), timePointYears = 0),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ]

  calculateImbalance(prev_data_avg, supply_subtract = c("exports", "stockChange"))

  prev_processed_avg <-
    prev_data_avg[
      supply > 0 &
        utilizations > 0 &
        measuredElementSuaFbs == "foodManufacturing" &
        !is.na(Value),
      .(geographicAreaM49, measuredItemSuaFbs, proc_ratio = Value / supply)
    ]


  new_data_avg <-
    data[
      new_feed,
      on = c("geographicAreaM49", "measuredItemSuaFbs")
    ][
      measuredElementSuaFbs != "foodManufacturing" & timePointYears >= 2014
    ][
      order(geographicAreaM49, measuredItemSuaFbs, measuredElementSuaFbs, timePointYears),
      Value := na.fill_(Value),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ][,
      .(Value = sum(Value) / sum(!is.na(Value)), timePointYears = 1),
      by = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ]

  calculateImbalance(
    new_data_avg,
    keep_utilizations = FALSE,
    supply_subtract = c("exports", "stockChange")
  )

  new_data_supply <-
    unique(
      new_data_avg[,
        c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears", "supply"),
        with = FALSE
      ]
    )

  new_data_proc <-
    dt_left_join(
      new_data_supply,
      prev_processed_avg,
      by = c("geographicAreaM49", "measuredItemSuaFbs"),
      nomatch = 0
    )

  new_data_proc <-
    new_data_proc[
      supply > 0,
      .(geographicAreaM49, measuredItemSuaFbs,
        measuredElementSuaFbs = "foodManufacturing",
        Value = supply * proc_ratio, timePointYears = 1)
    ]

  new_data_avg[, c("supply", "imbalance") := NULL]

  new_data_avg <-
    rbind(
      new_data_avg,
      new_data_proc
    )

  calculateImbalance(new_data_avg, supply_subtract = c("exports", "stockChange"))


  new_feed_to_remove <-
    unique(
      new_data_avg[
        imbalance / supply <= -0.3,
        c("geographicAreaM49", "measuredItemSuaFbs"),
        with = FALSE
      ]
    )

  new_feed_dubious <-
    unique(
      new_data_avg[
        imbalance / supply > -0.3 & imbalance / supply <= -0.05,
        .(geographicAreaM49, measuredItemSuaFbs, x = imbalance / supply)
      ][order(x)][, x := NULL]
    )

  new_feed_to_remove[, measuredElementSuaFbs := "feed"]

  data <-
    data[
      !new_feed_to_remove,
      on = c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")
    ]

  if (nrow(new_feed_to_remove) > 0) {
    msg_new_feed_remove <- paste(new_feed_to_remove$measuredItemSuaFbs, collapse = ", ")
  }

  if (nrow(new_feed_dubious) > 0) {
    msg_new_feed_dubious <- paste(new_feed_dubious$measuredItemSuaFbs, collapse = ", ")
  }
}


########## / Remove feed if new element and negative imbalance is huge


treeRestricted <-
  tree[,
    c("measuredItemParentCPC", "measuredItemChildCPC", "processingLevel"),
    with = FALSE
  ]

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
    measuredElementSuaFbs = 'stockChange',
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



data[, stockable := measuredItemSuaFbs %chin% stockable_items]


# XXX: Remove items for which no production, imports or exports exist after 2013.
non_existing_for_imputation <-
  setdiff(
    data[
      measuredElementSuaFbs %chin% c('production', 'imports', 'exports') &
        timePointYears <= 2013,
      unique(measuredItemSuaFbs)
    ],
    data[
      measuredElementSuaFbs %chin% c('production', 'imports', 'exports') &
        timePointYears >= 2014,
      unique(measuredItemSuaFbs)
    ]
  )

data <- data[!(measuredItemSuaFbs %chin% non_existing_for_imputation)]


############################################### Create derived production ###################################

dbg_print("derivation of shareDownUp")

############# ShareDownUp ----------------------------------------

#this table will be used to assign to zeroweight comodities 
#the processed quantities of their coproduct
coproduct_table <- ReadDatatable('zeroweight_coproducts')

stopifnot(nrow(coproduct_table) > 0)

# Can't do anything if this information if missing, so remove these cases
coproduct_table <- coproduct_table[!is.na(branch)]

### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
### XXX removing these cases as '22242.01' appears as zero-weight XXXX
### XXX for main product = '22110.04'                             XXXX
### XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
coproduct_table <- coproduct_table[branch != '22242.01 + 22110.04']

coproduct_table <- coproduct_table[, .(measured_item_child_cpc, branch)]

coproduct_for_sharedownup <- copy(coproduct_table)

coproduct_for_sharedownup_easy <- coproduct_for_sharedownup[!grepl('\\+|or', branch)]

coproduct_for_sharedownup_plus <- coproduct_for_sharedownup[grepl('\\+', branch)]

coproduct_for_sharedownup_plus <-
  rbind(
    tidyr::separate(
      coproduct_for_sharedownup_plus,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *\\+ *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(
      coproduct_for_sharedownup_plus,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *\\+ *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_for_sharedownup_plus <- unique(coproduct_for_sharedownup_plus)


coproduct_for_sharedownup_or <- coproduct_for_sharedownup[grepl('or', branch)]

coproduct_for_sharedownup_or <-
  rbind(
    #coproduct_table_or,
    tidyr::separate(
      coproduct_for_sharedownup_or,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *or *')[, .(measured_item_child_cpc, branch= main1)],
    tidyr::separate(
      coproduct_for_sharedownup_or,
      branch,
      into = c('main1', 'main2'),
      remove = FALSE,
      sep = ' *or *')[, .(measured_item_child_cpc,branch = main2)]
  )

coproduct_for_sharedownup_or <- unique(coproduct_for_sharedownup_or)

coproduct_for_sharedownup <-
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
      .(
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
        'stockChange', 'foodManufacturing', 'loss',
        'food', 'industrial', 'feed', 'seed')
  ]


#subset the tree accordingly to parents and child present in the SUA data

ExtrRate <-
  ExtrRate[
    measuredItemChildCPC %chin% data_tree$measuredItemSuaFbs &
      measuredItemParentCPC %chin% data_tree$measuredItemSuaFbs
  ]


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
      - Value[get(p$elementVar) %in% c(p$exportCode, "stockChange")],
      na.rm = TRUE
    ),
  by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
]

# used to chack if a parent has processed as utilization
data_tree[,
  proc_Median :=
    median(
      Value[measuredElementSuaFbs == "foodManufacturing" & timePointYears %in% 2000:2017],
      na.rm=TRUE
    ),
  by = c(p$parentVar, p$geoVar)
]

# boolean variable taking TRUE if the parent has only processed as utilization 
data_tree[,
  unique_proc :=
    proc_Median > 0 &
      !is.na(proc_Median) &
      # ... is the only utilization
      all(is.na(Value[!(measuredElementSuaFbs %chin%
                        c('production', 'imports', 'exports',
                          'stockChange','foodManufacturing'))])),
  by = c(p$parentVar, p$geoVar, p$yearVar)
]

sel_vars <-
  c("measuredItemParentCPC", "geographicAreaM49", "timePointYears",
    "measuredElementSuaFbs", "flagObservationStatus", "flagMethod",
    "Value", "Official", "measuredItemChildCPC", "extractionRate",
    "processingLevel")

data_tree<-
  unique(
    data_tree[, c(sel_vars, "availability", "unique_proc"), with = FALSE],
    by = sel_vars
  )


# dataset to calculate the number of parent of each child and the number of children of each parent
# including zeroweight commodities
sel_vars <- c("geographicAreaM49", "measuredItemParentCPC",
              "measuredItemChildCPC","timePointYears")
data_count <- unique(data_tree[, sel_vars, with = FALSE], by = sel_vars)

#Caculate the number of parent of each child
data_count[,
  number_of_parent := .N,
  by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
]

# calculate the number of children of each parent
# we exclude zeroweight to avoid doublecounting of children (processing)
data_count[measuredItemChildCPC %!in% zeroWeight,
  number_of_children := uniqueN(measuredItemChildCPC),
  by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
]

data_tree <-
  dt_left_join(
    data_tree,
    data_count,
    by = c(p$parentVar, p$childVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )

# dataset containing the processed quantity of parents
food_proc <-
  unique(
    data_tree[
      measuredElementSuaFbs == "foodManufacturing",
      c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Value"),
      with = FALSE
    ],
    by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears", "Value")
  )

setnames(food_proc, "Value", "parent_qty_processed")

# avoid recaculation of shareDownUp from 2014 onwards
food_proc[timePointYears > 2013, parent_qty_processed := NA_real_]

data_tree <-
  dt_left_join(
    data_tree,
    food_proc,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )

sel_vars <- 
  c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC",
    "timePointYears", "extractionRate", "parent_qty_processed",
    "processingLevel", "number_of_parent", "number_of_children")

data_tree <-
  unique(
    data_tree[, c(sel_vars, "availability", "unique_proc"), with = FALSE],
    by = sel_vars
  )


# dataset containing the production of child commodities
dataprodchild <- data[measuredElementSuaFbs %chin% c('production')]

setnames(dataprodchild, "measuredItemSuaFbs", "measuredItemChildCPC")

dataprodchild <-
  unique(
    dataprodchild[,
      c("geographicAreaM49", "measuredItemChildCPC",
        "timePointYears", "Value", "flagObservationStatus",
        "flagMethod"),
      with = FALSE
    ]
  )

setnames(dataprodchild, "Value", "production_of_child")


# to avoid resestimation based on estimated data (processed and production of child) from 2014 onwards
dataprodchild[timePointYears > 2013, production_of_child := NA_real_]

data_tree <-
  dt_left_join(
    data_tree,
    dataprodchild,
    by = c(p$geoVar, p$childVar, p$yearVar)
  )

# ShareDownups for zeroweights are calculated  separately
# to avoid double counting when agregating processed quantities of parent

# dataset containing informations of zeroweight commodities
data_zeroweight <- data_tree[measuredItemChildCPC %chin% zeroWeight]

# import data for coproduct relation
zw_coproduct <-
  coproduct_for_sharedownup[,
    .(zeroweight = measured_item_child_cpc, measuredItemChildCPC = branch)
  ]

zw_coproduct <- unique(zw_coproduct, by = c("measuredItemChildCPC", "zeroweight"))

# We subset the zeroweight coproduct reference table by taking only zeroweights and their coproduct
# that are childcommodities in the tree of the country
zw_coproduct <-
  zw_coproduct[
    measuredItemChildCPC %chin% data_tree$measuredItemChildCPC &
      zeroweight %chin% data_tree$measuredItemChildCPC
  ]


# Computing information for non zeroweight commodities
data_tree <- data_tree[measuredItemChildCPC %!in% zeroWeight]

# this dataset will be used when creating processing share and shareUpdown
dataComplete <- copy(data_tree)

# Quantity of parent destined to the production of the given child (only for child with one parent for the moment)
data_tree[, processed_to_child := ifelse(number_of_parent == 1, production_of_child, NA_real_)]

# if a parent has one child, all the production of the child comes from that parent
data_tree[
  number_of_children == 1,
  processed_to_child := parent_qty_processed * extractionRate,
  processed_to_child
]

data_tree[production_of_child == 0, processed_to_child := 0]

# assigning the entired availability to processed for parent having only processed as utilization
data_tree[
  number_of_children == 1 & unique_proc == TRUE,
  processed_to_child := availability * extractionRate
]


# mirror assignment for imputing processed quantity for multple parent children
# 5 loop is sufficient to deal with all the cases

for (k in 1:5) {
  data_tree <- RemainingToProcessedParent(data_tree)
  data_tree <- RemainingProdChildToAssign(data_tree)
}

data_tree <- RemainingToProcessedParent(data_tree)

# proportional allocation of the remaing production of multiple parent children
data_tree[,
  processed_to_child :=
    ifelse(
      number_of_parent > 1 & is.na(processed_to_child),
      (remaining_to_process_child * is.na(processed_to_child) * remaining_processed_parent) / sum((remaining_processed_parent * is.na(processed_to_child)), na.rm = TRUE),
      processed_to_child),
  by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
]

# Update of remaining production to assing ( should be zero for 2000:2013)
data_tree[,
  parent_already_processed :=
    ifelse(
      is.na(parent_qty_processed),
      parent_qty_processed,
      sum(processed_to_child / extractionRate,na.rm = TRUE)
    ),
  by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
]

data_tree[, remaining_processed_parent := round(parent_qty_processed - parent_already_processed)]

data_tree[
  remaining_processed_parent < 0,
  remaining_processed_parent := 0
]


# Impute processed quantity for 2014 onwards using 3 years average
# (this only to imput shareDownUp)
data_tree <-
  data_tree[
    order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears),
    processed_to_child_avg := rollavg(processed_to_child, order = 3),
    by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
  ]

setkey(data_tree, NULL)

data_tree[timePointYears > 2013 & is.na(processed_to_child), processed_to_child := processed_to_child_avg]


# Back to zeroweight cases(we assign to zeroweights the processed quantity of their coproduct(already calculated))

zw_coproduct_bis <-
  merge(
    data_tree,
    zw_coproduct,
    by = "measuredItemChildCPC",
    allow.cartesian = TRUE,
    all.y = TRUE
  )

zw_coproduct_bis[,
  `:=`(
    measuredItemChildCPC = zeroweight,
    processed_to_child = processed_to_child / extractionRate
  )
]

sel_vars <- 
  c("geographicAreaM49", "measuredItemParentCPC",
    "measuredItemChildCPC", "timePointYears")

zw_coproduct_bis <-
  zw_coproduct_bis[, c(sel_vars, "processed_to_child"), with = FALSE]

# Correction to milk tree issue ( zeroweight can be associated with 2 main products from the same parent)
# example: butter of cow milk
zw_coproduct_bis[,
  processed_to_child := sum(processed_to_child, na.rm = TRUE),
  by = sel_vars 
]

zw_coproduct_bis <-
  unique(zw_coproduct_bis, by = colnames(zw_coproduct_bis))

data_zeroweight <-
  dt_left_join(data_zeroweight, zw_coproduct_bis, by = sel_vars)

data_zeroweight[,
  processed_to_child := processed_to_child * extractionRate
]

sel_vars <-
  c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC",
    "timePointYears", "number_of_parent", "parent_qty_processed",
    "production_of_child", "processed_to_child")

data_zeroweight <- data_zeroweight[, sel_vars, with = FALSE]

data_tree <- data_tree[, sel_vars, with = FALSE]

#combining zeroweight and non zero weight commodities
data_tree <- rbind(data_tree, data_zeroweight)

#calculate ShareDownUp
data_tree[,
  shareDownUp := processed_to_child / sum(processed_to_child, na.rm = TRUE),
  by = c("geographicAreaM49", "measuredItemChildCPC", "timePointYears")
]

#some corrections...

data_tree[is.na(shareDownUp) & number_of_parent == 1, shareDownUp := 1]

data_tree[
  (production_of_child == 0 | is.na(production_of_child)) &
    measuredItemChildCPC %!in% zeroWeight &
    timePointYears < 2014,
  shareDownUp := 0
]

data_tree[
  (parent_qty_processed == 0 | is.na(parent_qty_processed)) &
    timePointYears < 2014,
  shareDownUp :=0
]

data_tree[is.na(shareDownUp), shareDownUp := 0]

data_tree <-
  unique(
    data_tree[,
      c("geographicAreaM49", "measuredItemParentCPC",
        "measuredItemChildCPC", "timePointYears", "shareDownUp"),
      with = FALSE
    ]
  )

########################GENERARING ProcessedShare for parent----------------------------------------
######################## ANd ShareUpDown for children ##############################################
####################################################################################################

# Processing share and ShareUpDown
dataProcessingShare <-
  unique(
    dataComplete[,
      c("geographicAreaM49", "timePointYears", "availability",
        "measuredItemParentCPC", "parent_qty_processed"),
      with = FALSE
    ],
    by = c("geographicAreaM49", "timePointYears",
           "measuredItemParentCPC", "parent_qty_processed")
  )

dataProcessingShare[,
  Pshare := parent_qty_processed / availability,
  by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
]

dataProcessingShare[Pshare > 1, Pshare := 1]
dataProcessingShare[Pshare < 0,Pshare := 0]
dataProcessingShare[is.nan(Pshare), Pshare := NA_real_]

dataProcessingShare <-
  dataProcessingShare[
    order(geographicAreaM49, measuredItemParentCPC, timePointYears),
    Pshare_avr := rollavg(Pshare, order = 3),
    by = c("geographicAreaM49", "measuredItemParentCPC")
  ]

setkey(dataProcessingShare, NULL)

# we estimate Pshare for 2014 onwards, even if processed are modified manually because a this stage
# availability of 2014 onwards are known (stocks are not generated yet)
dataProcessingShare[timePointYears > 2013, Pshare := Pshare_avr]
dataProcessingShare[, Pshare_avr := NULL]

data_Pshare <-
  dataProcessingShare[,
    c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
      "parent_qty_processed", "Pshare"),
    with = FALSE
  ]


# calculating shareUpdown for each child
data_ShareUpDoawn <-
  dataComplete[,
    c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
      "availability", "parent_qty_processed", "measuredItemChildCPC",
      "extractionRate", "production_of_child", "number_of_children"),
    with = FALSE
  ]
  
data_ShareUpDoawn <-
  unique(
    data_ShareUpDoawn,
    by = colnames(data_ShareUpDoawn)
  )

data_ShareUpDoawn <- merge(
  data_ShareUpDoawn,
  data_tree,
  by = c("geographicAreaM49", "measuredItemParentCPC",
         "measuredItemChildCPC", "timePointYears"),
  all = TRUE
)

data_ShareUpDoawn <- data_ShareUpDoawn[measuredItemChildCPC %!in% zeroWeight]
data_ShareUpDoawn[, shareUpDown := NA_real_]

data_ShareUpDoawn[
  !is.na(parent_qty_processed) & extractionRate>0,
  shareUpDown := (production_of_child / extractionRate) * shareDownUp / sum(production_of_child / extractionRate * shareDownUp, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
]

data_ShareUpDoawn[is.nan(shareUpDown), shareUpDown := NA_real_]

data_ShareUpDoawn <-
  data_ShareUpDoawn[
    order(geographicAreaM49, measuredItemParentCPC, measuredItemChildCPC, timePointYears),
    shareUpDown_avg := rollavg(shareUpDown, order = 3),
    by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC")
  ]

setkey(data_ShareUpDoawn, NULL)

data_ShareUpDoawn[timePointYears > 2013, shareUpDown := shareUpDown_avg]

data_ShareUpDoawn[, shareUpDown_avg := NULL]

data_ShareUpDoawn[
  timePointYears > 2013,
  shareUpDown := shareUpDown / sum(shareUpDown, na.rm = TRUE),
  by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
]

sel_vars <-
  c("geographicAreaM49", "measuredItemParentCPC",
    "measuredItemChildCPC", "timePointYears", "shareUpDown")

data_ShareUpDoawn_final <- data_ShareUpDoawn[, sel_vars, with = FALSE]

shareUpDown_zeroweight <-
  merge(
    data_ShareUpDoawn_final,
    zw_coproduct,
    by = "measuredItemChildCPC",
    allow.cartesian = TRUE,
    all.y = TRUE
  )

shareUpDown_zeroweight[, measuredItemChildCPC := zeroweight]

shareUpDown_zeroweight <- shareUpDown_zeroweight[, sel_vars, with = FALSE]

# Correction to milk tree issue ( zeroweight can be associated with 2 main products from the same parent)
# example: butter of cow milk
shareUpDown_zeroweight<-
  shareUpDown_zeroweight[,
     shareUpDown := sum(shareUpDown, na.rm = TRUE),
     by = c("geographicAreaM49", "measuredItemParentCPC", "measuredItemChildCPC", "timePointYears")
  ]

shareUpDown_zeroweight <-
  unique(
    shareUpDown_zeroweight,
    by = colnames(shareUpDown_zeroweight)
  )

# /correction


data_ShareUpDoawn_final <- rbind(data_ShareUpDoawn_final, shareUpDown_zeroweight)

# some correction
data_ShareUpDoawn_final[is.na(shareUpDown), shareUpDown := 0]
data_ShareUpDoawn_final[!is.na(shareUpDown), flagObservationStatus := "I"]
data_ShareUpDoawn_final[!is.na(shareUpDown), flagMethod := "c"]


# sAVE DATA TO SWS

shareUpDown_to_save <- copy(data_ShareUpDoawn_final)

shareUpDown_to_save[, measuredElementSuaFbs := "5431"]

setnames(
  shareUpDown_to_save,
  c("measuredItemParentCPC", "measuredItemChildCPC"),
  c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree")
)

setnames(shareUpDown_to_save, "shareUpDown", "Value")

shareUpDown_to_save[, Value := round(Value, 3)]

sessionKey_shareUpDown <- swsContext.datasets[[3]]

CONFIG <- GetDatasetConfig(sessionKey_shareUpDown@domain, sessionKey_shareUpDown@dataset)

data_shareUpDown_sws <- GetData(sessionKey_shareUpDown)

#saving ShareUpDown For the first time #all flage are (i,c) like production of derived
if (nrow(data_shareUpDown_sws) == 0) {
  faosws::SaveData(
    domain = "suafbs",
    dataset = "up_down_share",
    data = shareUpDown_to_save,
    waitTimeout = 20000
  )
} else {
 
  setnames(
    data_shareUpDown_sws,
    c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree", "Value"),
    c("measuredItemParentCPC", "measuredItemChildCPC", "shareUpDown")
  )
  
  data_ShareUpDoawn_to_use <- copy(data_shareUpDown_sws)

  data_ShareUpDoawn_to_use <-
    data_ShareUpDoawn_to_use[, colnames(data_ShareUpDoawn_final), with = FALSE]
  
  
  # rbind with anti_join
  data_ShareUpDoawn_final <-
    rbind(
      data_ShareUpDoawn_to_use,
      data_ShareUpDoawn_final[
        !data_ShareUpDoawn_to_use,
        on = c('geographicAreaM49', 'measuredItemParentCPC',
               'measuredItemChildCPC', 'timePointYears')
      ],
      fill = TRUE
    )
  
}


#consistency check: sum of shareUpDown by parent should exceed 1.

data_ShareUpDoawn_final_invalid <-
  data_ShareUpDoawn_final[
    measuredItemChildCPC %!in% zeroWeight,
    .(sum_shares = round(sum(shareUpDown, na.rm = TRUE)), 2),
    by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
  ][
    !dplyr::near(sum_shares, 1) & !dplyr::near(sum_shares, 0)
  ]


if (nrow(data_ShareUpDoawn_final_invalid) > 0) {
  
  write.csv(
    data_ShareUpDoawn_final_invalid,
    file.path(paste0("ShareUpDown_toCorrect_", COUNTRY, ".csv"))
  )
  
  if (!CheckDebug()) {
    send_mail(
      from = "do-not-reply@fao.org",
      to = swsContext.userEmail,
      subject = "Some shareDownUp are invalid",
      body = c(paste("There are some invalid shareUpDown (they do not sum to 1). See attachment and fix them in the SWS shareUpDown dataset"),
               file.path(paste0("ShareUpDown_toCorrect_", COUNTRY, ".csv")))
    )
  }
  
  stop("Some shareUpDown can create conflict. Check your email.")
}


#/processingShare---------------------



# / Tables that will be required by the co-product issue (fix processingShare)

if (file.exists(shareDownUp_file)) {
  
  SHAREDOWNUP_LOADED <- TRUE
  
  shareDownUp_previous <-
    fread(
      shareDownUp_file,
      colClasses = c(rep("character", 4), "numeric", "logical")
    )
 
  #correction (if some duplicates are already generated)
  shareDownUp_previous <-
    unique(
      shareDownUp_previous,
      by = colnames(shareDownUp_previous)
    )

  shareDownUp_previous[,
    shareDownUp := sum(shareDownUp, na.rm = TRUE),
    by = c("geographicAreaM49", "measuredItemParentCPC",
           "measuredItemChildCPC", "timePointYears")
  ]

  shareDownUp_previous <-
    unique(
      shareDownUp_previous,
      by = colnames(shareDownUp_previous)
    )
  
  #/correction
  
  # Check on consistency of shareDownUp
  shareDownUp_invalid <-
    shareDownUp_previous[,
      .(sum_shares = round(sum(shareDownUp)), 2),
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

#final sharedowmUp

if (nrow(shareDownUp_previous) > 0) {
  data_tree_final <-
    dt_left_join(
      data_tree,
      shareDownUp_previous,
      by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
    )

  data_tree_final[protect_share == TRUE, shareDownUp := shareDownUp_prev]
  data_tree_final[is.na(protect_share), protect_share := FALSE]
  
  ##Automatic update of shareDOwnUp (to discuss with Salar)
  #data_tree_final[,shareDownUp1:=ifelse(protect_share==FALSE & sum(protect_share==TRUE)>0,
  #                                     shareDownUp/sum(shareDownUp[protect_share==FALSE],na.rm = TRUE)*(1-shareDownUp[protect_share==TRUE]),
  #                                     shareDownUp
  #),
  #by=c(p$geoVar,p$yearVar,p$childVar)
  #]
  data_tree_final[, shareDownUp_prev := NULL]
  data_tree_final <- unique(data_tree_final, by = colnames(data_tree_final))
  data_tree_final_save <- copy(data_tree_final)
  data_tree_final[, protect_share := NULL]
} else {
  data_tree_final <- data_tree
  data_tree_final_save <- copy(data_tree_final)
  data_tree_final_save[, protect_share := FALSE]
}

# Check on consistency of shareDownUp
shareDownUp_invalid <-
  data_tree_final[,
    .(sum_shares = round(sum(shareDownUp)), 2),
    by = c("geographicAreaM49", "timePointYears", "measuredItemChildCPC")
  ][
    !dplyr::near(sum_shares, 1) & !dplyr::near(sum_shares, 0)
  ]

if (nrow(shareDownUp_invalid) > 0) {
  
  shareDownUp_invalid <-
    data_tree_final[
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
  data_tree_final_save[,
    c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
      "measuredItemChildCPC", "shareDownUp", "protect_share"),
    with = FALSE
  ]


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

# / ShareDownUp -------------------------------------------------------



# stockable items for which a historical series of at least
# 5 non-missing/non-null data points exist
historical_avail_stocks <-
  data[
    measuredElementSuaFbs == "stockChange" &
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

#keep shareuPdOWN
updated_shareUpDOwn<-list()

fixed_proc_shares <- list()

original_stock_variation <-
  data[
    measuredElementSuaFbs == "stockChange" &
      timePointYears >= 2014 &
      !is.na(Value),
    c(
      "geographicAreaM49", "measuredItemSuaFbs",
      "timePointYears", "Value", "flagObservationStatus", "flagMethod"
    )
  ]

dbg_print("starting derived production loop")

if (length(primaryInvolvedDescendents) == 0) {
  message("No primary commodity involved in this country")
} else {
  
  #USED TO AVOID DUPLICATES
  data[, Processed_estimed := FALSE]
  
  for (lev in sort(unique(tree$processingLevel))) {
    #testing purpose
    # lev=0
    dbg_print(paste("derived production loop, level", lev))

    treeCurrentLevel <-
      tree[
        !is.na(Value) &
          processingLevel == lev &
          measuredElementSuaFbs == 'extractionRate',
        .(
          measuredItemParentCPC,
          geographicAreaM49,
          measuredItemChildCPC,
          timePointYears,
          extractionRate = Value
        )
      ]
    
    #tree containing children of all parent of current level
    tree_parent_Level<-
      tree[
        !is.na(Value) &
          measuredElementSuaFbs == 'extractionRate' &
          measuredItemParentCPC %chin% treeCurrentLevel[[p$parentVar]],
        .(
          measuredItemParentCPC,
          geographicAreaM49,
          measuredItemChildCPC,
          timePointYears,
          extractionRate = Value,
          processingLevel
        )
      ]

    ############# Create stocks, if missing ####################
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
                  - Value[measuredElementSuaFbs %chin% c("exports", "stockChange")],
                  na.rm = TRUE
                ),
              supply_exc =
                sum(
                  Value[measuredElementSuaFbs %chin% c("production", "imports")],
                  - Value[measuredElementSuaFbs == "exports"],
                  na.rm = TRUE
                )
            ),
            keyby = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
          ][,
            trend := seq_len(.N),
            by = c("geographicAreaM49", "measuredItemSuaFbs")
          ]

        dbg_print("coeffs_stocks_mod")

        data_histmod_stocks[,
          c("c_int", "c_sup", "c_trend") := coeffs_stocks_mod(.SD),
          by = c("geographicAreaM49", "measuredItemSuaFbs")
        ][,
          supply_inc_pred := c_int + c_sup * supply_exc + c_trend * trend,
        ][,
          `:=`(
            min_new_supply_inc = min(supply_inc[supply_inc > 0 & timePointYears >= 2009 & timePointYears <= 2013], na.rm = TRUE),
            avg_new_supply_inc = mean(supply_inc[supply_inc > 0 & timePointYears >= 2009 & timePointYears <= 2013], na.rm = TRUE)
          ),
          by = c("geographicAreaM49", "measuredItemSuaFbs")
        ][
          supply_inc_pred <= 0 & timePointYears >= 2014,
          supply_inc_pred := min_new_supply_inc
        ][,
          delta_pred := supply_exc - supply_inc_pred
        ]

        data_histmod_stocks[
          supply_inc > 100,
          abs_stock_perc := abs(supply_exc - supply_inc) / supply_inc
        ]

        data_histmod_stocks[,
          abs_delta_pred_perc := abs(delta_pred) / avg_new_supply_inc
        ]

        # Max without the actual maximum, to avoid extremes
        data_histmod_stocks[,
          #upper_abs_stock_perc := max(abs_stock_perc[-(which(abs_stock_perc == max(abs_stock_perc, na.rm = TRUE)))], na.rm = TRUE),
          upper_abs_stock_perc := max(abs_stock_perc[timePointYears <= 2013], na.rm = TRUE),
          by = c("geographicAreaM49", "measuredItemSuaFbs")
        ]

        # upper_abs_stock_perc can be infinite if abs_stock_perc is always missing
        # (note that abs_stock_perc is calculated only for supply > 100)
        data_histmod_stocks[
          abs_delta_pred_perc > upper_abs_stock_perc &
            timePointYears >= 2014 &
            !is.infinite(upper_abs_stock_perc),
          delta_pred :=
            upper_abs_stock_perc * avg_new_supply_inc * sign(delta_pred)
        ]

        data_for_stocks <-
          data_histmod_stocks[
            timePointYears >= 2014 ,
            .(geographicAreaM49, measuredItemSuaFbs,
              timePointYears, delta = delta_pred)
          ]

        if (nrow(data_for_stocks) > 0) {
          # HERE
          data_for_stocks <-
            dt_left_join(
              data_for_stocks,
              all_opening_stocks[,
                .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua,
                  timePointYears, new_opening = Value)
              ],
              by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
            )

          stockdata <-
            data[
              Protected == TRUE &
                measuredElementSuaFbs == "stockChange" &
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
            dt_left_join(
              data_for_stocks,
              stockdata,
              by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
            )

          data_for_stocks[!is.na(stockvar), delta := stockvar]
          
          data_for_stocks[, stockvar := NULL]

          # Stock withdrawal cannot exceed opening stocks in the first year
          data_for_stocks[
            timePointYears == "2014" & delta < 0 & abs(delta) > new_opening,
            delta := - new_opening
          ]

          # NOTE: Data here should be ordered by country/item/year (CHECK)
          data_for_stocks <- update_opening_stocks(data_for_stocks)

          data <-
            dt_left_join(
              data,
              data_for_stocks,
              by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs')
            )

          data[
            measuredElementSuaFbs == "stockChange" &
              Protected == FALSE &
              !is.na(delta),
            `:=`(
              Value = delta,
              flagObservationStatus = "E",
              flagMethod = "u",
              Protected = FALSE
            )
          ]

          data[, c("delta", "new_opening") := NULL]

          all_opening_stocks <-
            dt_full_join(
              all_opening_stocks,
              data_for_stocks[,
                .(
                  geographicAreaM49,
                  measuredItemFbsSua = measuredItemSuaFbs,
                  timePointYears,
                  new_opening
                )
              ],
              by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
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

    ############# / Create stocks, if missing ##################

    dataMergeTree <- data[measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stockChange')]
    
    setnames(dataMergeTree, "measuredItemSuaFbs", "measuredItemParentCPC")
    
    dbg_print("dataMerge + treeCurrentLevel")

    dataMergeTree <-
      merge(
        dataMergeTree,
        tree_parent_Level,
        by = c(p$parentVar, p$geoVar, p$yearVar),
        allow.cartesian = TRUE
      )

    dbg_print("dataMerge + data_tree_final")

    dataMergeTree <-
      merge(
        dataMergeTree,
        data_tree_final,
        by = c(p$parentVar, p$childVar, p$geoVar, p$yearVar),
        allow.cartesian = TRUE
      )

    setkey(dataMergeTree, NULL)
    
    dbg_print("dataMerge availability")

    dataMergeTree[,
      availability :=
        sum(
          Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
          # XXX p$stockCode is "stockChange", not "stockChange"
          - Value[get(p$elementVar) %in% c(p$exportCode, "stockChange")],
          na.rm = TRUE
        ),
      by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
    ]
    
    dbg_print("negative availability")

    negative_availability[[as.character(lev)]] <-
      unique(
        dataMergeTree[
          availability < -100,
          .(
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

    
    ##########Caculating and updating processed of parent--------------------------
    #amsata
    datamergeNew <- copy(dataMergeTree)
    
    datamergeNew <-
      datamergeNew[,
        c("geographicAreaM49", "measuredItemParentCPC", "availability",
          "measuredItemChildCPC", "timePointYears", "extractionRate",
          "processingLevel", "shareDownUp"),
        with = FALSE
      ]
    
    datamergeNew <- unique(datamergeNew, by = colnames(datamergeNew))
    
    datamergeNew <-
      merge(
        datamergeNew,
        data[
          measuredElementSuaFbs == 'production',
          list(geographicAreaM49, timePointYears,
               measuredItemChildCPC = measuredItemSuaFbs, production = Value,
               Protected, Official, flagObservationStatus, flagMethod)
          ],
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemChildCPC')
      )

    setkey(datamergeNew, NULL)

    datamergeNew[, manual := flagObservationStatus == "E" & flagMethod == "f"]

    datamergeNew[, c("flagObservationStatus", "flagMethod") := NULL]
    
    datamergeNew_zw <- datamergeNew[measuredItemChildCPC %chin% zeroWeight]
    
    datamergeNew <- datamergeNew[measuredItemChildCPC %!in% zeroWeight]
    
    datamergeNew <-
      dt_left_join(
        datamergeNew,
        data_Pshare,
        by = c(p$parentVar, p$geoVar, p$yearVar),
        allow.cartesian = TRUE
      ) 
    
    datamergeNew <-
      dt_left_join(
        datamergeNew,
        data_ShareUpDoawn_final,
        by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
      ) 
    
    datamergeNew <-
      datamergeNew[,
        c("geographicAreaM49", "measuredItemParentCPC", "availability", "Pshare",
          "measuredItemChildCPC", "timePointYears", "extractionRate",
          "processingLevel", "shareDownUp", "shareUpDown", "production",
          "Protected", "flagObservationStatus", "flagMethod", "Official", "manual"),
        with = FALSE
      ]

    datamergeNew <- unique(datamergeNew)
    
    # datamergeNew[,zeroweight:=measuredItemChildCPC %in% zeroWeight]
    
    
    # including processed of parent
    datamergeNew <-
      dt_left_join(
        datamergeNew,
        data[
          measuredElementSuaFbs == 'foodManufacturing',
          list(geographicAreaM49, timePointYears,
               measuredItemParentCPC = measuredItemSuaFbs,
               processedBis = Value, processedProt = Protected)
          ],
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemParentCPC')
      )
    
    # datamergeNew[timePointYears >= 2014 & Protected == FALSE, Value := NA][, Protected := NULL]
    datamergeNew[timePointYears >= 2014 & Protected == FALSE, production := NA]
    datamergeNew[timePointYears >= 2014 & processedProt == FALSE, processedBis := NA]
    
    # Calculate the number of child with protected production for each parent
    datamergeNew[,
      Number_childPro := sum(Protected == TRUE & shareUpDown > 0, na.rm = TRUE),
      by = c(p$parentVar, p$geoVar, p$yearVar)
    ]
    
    # calculate the number of child of each parent where processed used to be send
    datamergeNew[,
      Number_child := sum(shareUpDown > 0, na.rm = TRUE),
      by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
    ]

    # we estimate processed quantity for parent
    # based on the historique trend of processed percentage
    datamergeNew[, Processed := Pshare * availability] 
    
    # However, we may have cases where some production of child commodities are official
    
    # case1: some child production are official but not all
    # in this case we update the shareUpDown of the child commodities for which the production is official
    # if the sum of their ShareUpdown is greater than 1, we update processed using the official production of those
    # children and then we update the shareUpDown of different child commodities
    datamergeNew[, c("shareUpDown_NEW", "Processed_new") := NA_real_]
    
    # A child is converted up to process of parent if it has a protected production 
    # with shareUpDown>0, shareDownUp>0 and extractionRate > 0. this avoid issue caused by multiple parent
    # children with protected production
    
    datamergeNew[,
      child_DownUp :=
        Protected == TRUE &
        shareUpDown > 0 &
        extractionRate > 0 &
        shareDownUp > 0
    ]
    
    # GianLuca suggestion----------------
    
    datamergeNew[Number_childPro > 0, #& extractionRate > 0,
      #sum_shareUD_high == TRUE,
      Processed_new :=
        sum(
          production[child_DownUp == TRUE] / extractionRate[child_DownUp == TRUE] * shareDownUp[child_DownUp == TRUE],
          na.rm = TRUE
        ) /
        sum(shareUpDown[child_DownUp == TRUE], na.rm = TRUE),
      by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
    ]

    datamergeNew[,
      Processed_new := ifelse(is.na(Processed_new), mean(Processed_new, na.rm = TRUE), Processed_new),
      by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
    ]
    
    datamergeNew[,
      processed_down_up := sum(child_DownUp, na.rm = TRUE) > 0,
      by = c("geographicAreaM49", "measuredItemParentCPC", "timePointYears")
    ]
    
    datamergeNew[is.na(Processed_new) & processed_down_up == TRUE, Processed_new := 0]

    datamergeNew[Processed_new == 0 & processed_down_up == FALSE, Processed_new := NA_real_]
    
    datamergeNew[
      Protected == TRUE & manual == FALSE,
      `:=`(flagObservationStatus = "T", flagMethod = "-")
    ]
   
    datamergeNew[
      Protected == TRUE & manual == TRUE,
      `:=`(flagObservationStatus = "E", flagMethod = "f")
    ]
   
    datamergeNew[
      Protected==FALSE,
      `:=`(flagObservationStatus = "I", flagMethod = "c")
    ]
    
    datamergeNew[!is.na(shareUpDown_NEW), shareUpDown := shareUpDown_NEW]
    
    # GianLuca------------------------
    
    
    #Update the processed quantities
    
    dataForProc <-
      datamergeNew[,
        c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
          "availability","Pshare","Processed", "processedBis", "Processed_new"),
        with = FALSE
      ]

    dataForProc <- unique(dataForProc)
    
    dataForProc[
      timePointYears > 2013 & !is.na(Processed_new),
      processedBis := Processed_new
    ]
    
    # cancel the automatic update of Pshare for 2014 onward
    #However this lines can be kept for futur improvement
    
    # dataForProc[,
    #   Pshare := processedBis / availability,
    #   by = c("geographicAreaM49", "timePointYears", "measuredItemParentCPC")
    # ]
    # dataForProc[is.nan(Pshare), Pshare := NA_real_]
    # dataForProc[Pshare > 1, Pshare := 1]
    # dataForProc[Pshare < 0, Pshare := 0]
    # dataForProc <-
    #   dataForProc[
    #     order(geographicAreaM49, measuredItemParentCPC, timePointYears),
    #     Pshare_avr := rollavg(Pshare, order = 3),
    #     by = c("geographicAreaM49", "measuredItemParentCPC")
    #   ]
    # 
    # setkey(dataForProc, NULL)
    # 
    # dataForProc[timePointYears > 2013 & is.na(Pshare), Pshare := Pshare_avr]
    # 
    # dataForProc[, Pshare_avr := NULL]
    # 
    # dataForProc[, Processed := Pshare * availability]

    dataForProc[!is.na(Processed_new), Processed := Processed_new]

    datamergeNew <-
      datamergeNew[,
        c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
          "measuredItemChildCPC", "extractionRate", "processingLevel",
          "shareDownUp", "shareUpDown", "production", "Protected",
          "flagObservationStatus", "flagMethod", "Official", "manual",
          "processed_down_up"),
        with = FALSE
      ]
    
    datamergeNew <-
      merge(
        dataForProc,
        datamergeNew,
        by = c(p$geoVar, p$yearVar, p$parentVar),
        allow.cartesian = TRUE
      ) 
    
    setkey(datamergeNew, NULL)
    
    #if the shareUpDown of a zeroweight coproduct is updated, the shareUpDown of
    #the corresponding zeroweight should also be updated
    
    #merge zeroweight with the tree
    
    datamergeNew_zeroweight <-
      merge(
      datamergeNew,
        zw_coproduct ,
        by = "measuredItemChildCPC",
        allow.cartesian = TRUE
      )

    setkey(datamergeNew_zeroweight, NULL)
    
    datamergeNew_zeroweight[, measuredItemChildCPC := zeroweight]
    
    datamergeNew_zeroweight <-
      datamergeNew_zeroweight[,
        c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
          "measuredItemChildCPC", "Pshare", "shareUpDown", "Processed",
          "flagObservationStatus", "flagMethod", "processed_down_up"),
        with = FALSE
      ]
    
    #correction
    datamergeNew_zeroweight<-
      datamergeNew_zeroweight[,
        shareUpDown := sum(shareUpDown, na.rm = TRUE),
        by = c("geographicAreaM49", "measuredItemParentCPC",
               "measuredItemChildCPC", "timePointYears")
      ]
    
    datamergeNew_zeroweight <- unique(datamergeNew_zeroweight)

    #/correction
    
    datamergeNew_zeroweight <-
      merge(
        datamergeNew_zeroweight,
        datamergeNew_zw,
        by = c("geographicAreaM49", "timePointYears",
               "measuredItemParentCPC", "measuredItemChildCPC")
      )
    
    datamergeNew <-
      datamergeNew[, colnames(datamergeNew_zeroweight), with = FALSE]
    
    datamergeNew_final <- rbind(datamergeNew, datamergeNew_zeroweight)
    
    #some correction
    datamergeNew_final[is.na(shareUpDown), shareUpDown := NA_real_]
    
    updated_shareUpDOwn[[as.character(lev)]] <-
      datamergeNew_final[
        processingLevel == lev,
        c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
          "measuredItemChildCPC", "shareUpDown", "flagObservationStatus",
          "flagMethod"),
        with = FALSE
      ]

    #/processed of parent-----------------
    estimated_processed <-
      datamergeNew_final[,
        list(geographicAreaM49, timePointYears,
             measuredItemSuaFbs = measuredItemParentCPC,
             Processed, processed_down_up)
      ]

    estimated_processed <- unique(estimated_processed)
     
    complete_food_proc <-
      CJ(
        measuredItemSuaFbs    = unique(estimated_processed$measuredItemSuaFbs),
        measuredElementSuaFbs = 'foodManufacturing',
        geographicAreaM49     = unique(estimated_processed$geographicAreaM49),
        timePointYears        = unique(estimated_processed$timePointYears)
      )
    
    # rbind with anti_join
    data <-
      rbind(
        data,
        complete_food_proc[
          !data,
          on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                 'geographicAreaM49', 'timePointYears')
        ],
        fill = TRUE
      )
    
    data <-
      dt_left_join(
        data,
        estimated_processed[timePointYears >= 2014],
        by = c('geographicAreaM49', 'timePointYears', 'measuredItemSuaFbs'),
        allow.cartesian = TRUE
      )
    
    data[is.na(Protected), Protected := FALSE]
    data[is.na(Official), Official := FALSE]
    data[is.na(Processed_estimed), Processed_estimed := FALSE]
    
    data[
      measuredElementSuaFbs == "foodManufacturing" &
        Protected == FALSE & !is.na(Processed) & Processed_estimed == FALSE,
      `:=`(
        Value = Processed,
        flagObservationStatus = "E",
        flagMethod = ifelse(processed_down_up == TRUE, "-", "i"),
        Processed_estimed = TRUE
      )
    ][,
      c("Processed", "processed_down_up") := NULL
    ]
    
    
    ####### estimating production of derived
    data_production <-
      datamergeNew_final[
        processingLevel == lev,
        c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
          "availability", "Pshare", "measuredItemChildCPC", "extractionRate",
          "shareDownUp", "shareUpDown", "production", "Protected"),
        with = FALSE
      ]
    
    data_production <- unique(data_production)
    
    data_production <-
      dt_left_join(
        data_production,
        unique(
          data[
            measuredElementSuaFbs == "foodManufacturing" & !is.na(Value),
            .(geographicAreaM49, timePointYears,
              measuredItemParentCPC = measuredItemSuaFbs,
              Processed_parent = Value)
          ]
        ),
        by = c(p$geoVar, p$yearVar, p$parentVar)
      )
    
    data_production[,
      #new_imputation:=Processed_parent*shareUpDown*extractionRate*shareDownUp
      new_imputation := Processed_parent * shareUpDown * extractionRate #shareDownUp is not part of the formula
    ]
    
    data_production <-
      data_production[,
        c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
          "availability", "Pshare", "measuredItemChildCPC", "production",
          "new_imputation", "extractionRate", "shareDownUp",
          "shareUpDown", "Processed_parent"),
        with = FALSE
      ]
    
    data_production <- unique(data_production, by = c(colnames(data_production))) 
    
    sel_vars <- 
      c("geographicAreaM49", "timePointYears", "measuredItemParentCPC",
        "availability", "Pshare", "measuredItemChildCPC", "extractionRate",
        "shareDownUp", "shareUpDown", "Processed_parent")
    
    computed_shares[[as.character(lev)]] <-
      data_production[, sel_vars, with = FALSE]
    
    # XXX: change this name
    computed_shares_send[[as.character(lev)]] <-
      data_production[, sel_vars, with = FALSE]
    
    dbg_print("sum unique of new_imputation")

    data_production <-
      data_production[,
        .(
          measuredElementSuaFbs = 'production',
          # NOTE: the sum(unique()) is just a temporary HACK: check how to do it properly
          imputed_deriv_value = sum(unique(round(new_imputation, 2)), na.rm = TRUE)
        ),
        .(geographicAreaM49, timePointYears, measuredItemSuaFbs = measuredItemChildCPC)
      ]
    
    
    data_production_complete <-
      CJ(
        measuredItemSuaFbs    = unique(data_production$measuredItemSuaFbs),
        measuredElementSuaFbs = 'production',
        geographicAreaM49     = unique(data_production$geographicAreaM49),
        timePointYears        = unique(data_production$timePointYears)
      )

    setkey(data_production_complete, NULL)

    sel_vars <- 
      c('geographicAreaM49', 'timePointYears',
        'measuredItemSuaFbs', 'measuredElementSuaFbs') 

    # rbind with anti_join
    data <-
      rbind(
        data,
        data_production_complete[!data, on = sel_vars],
        fill = TRUE
      )
    
    data <- dt_left_join(data, data_production, by = sel_vars)

    dbg_print("z data for shares to send")

    z <-
      data[
        measuredElementSuaFbs %chin% c("production", "imports", "exports", "stockChange"),
        c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs",
          "measuredElementSuaFbs", "Value", "Protected", "imputed_deriv_value"),
        with = FALSE
      ]

    z[
      measuredElementSuaFbs == 'production' & Protected == FALSE,
      Value := imputed_deriv_value
    ]

    z[, imputed_deriv_value := NULL]

    # XXX stocks create duplicates
    z <- unique(z)

    dbg_print("dcast z data")

    z <-
      data.table::dcast(
        z,
        geographicAreaM49 + timePointYears + measuredItemSuaFbs ~ measuredElementSuaFbs,
        value.var = c("Value", "Protected")
      )
    
    # XXX stockChange may change below (and also production, if required)

    dbg_print("z data + computed_shares_send")

    tmp <-
      z[computed_shares_send[[as.character(lev)]],
        on = c('geographicAreaM49'  = 'geographicAreaM49',
               'timePointYears'     = 'timePointYears',
               'measuredItemSuaFbs' = 'measuredItemChildCPC')]

    setkey(tmp, NULL)

    # XXX stocks create duplicates
    tmp <- unique(tmp)

    computed_shares_send[[as.character(lev)]] <- tmp
    
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
        flagObservationStatus = "I", flagMethod = "c")
    ]
    
    data[, imputed_deriv_value := NULL]
  }
}

setkey(data, NULL)

computed_shares <- rbindlist(computed_shares)

updated_shareUpDOwn <- rbindlist(updated_shareUpDOwn)


shareUpDown_to_save <- copy(updated_shareUpDOwn)
shareUpDown_to_save[, measuredElementSuaFbs := "5431"]

setnames(
  shareUpDown_to_save,
  c("measuredItemParentCPC", "measuredItemChildCPC", "shareUpDown"),
  c("measuredItemParentCPC_tree", "measuredItemChildCPC_tree", "Value")
)

shareUpDown_to_save <- shareUpDown_to_save[!is.na(Value)]

shareUpDown_to_save[, Value := round(Value, 3)]

faosws::SaveData(
  domain = "suafbs",
  dataset = "up_down_share",
  data = shareUpDown_to_save,
  waitTimeout = 20000
)


dbg_print("end of derived production loop")


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

# save processed data
data_processed <-
  data[
    measuredElementSuaFbs == 'foodManufacturing' &
      timePointYears %in% 2014:2017,
    list(
      geographicAreaM49,
      measuredElementSuaFbs = "5023",
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
    measuredElementSuaFbs == 'stockChange' &
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

data_to_save_unbalanced <-
  rbind(
    data_deriv,
    data_processed,
    data_stock_to_save,
    opening_stocks_to_save
  )

data_to_save_unbalanced <- data_to_save_unbalanced[!is.na(Value)]

#saveRDS(data_to_save_unbalanced, paste0("/tmp/out_", COUNTRY, ".rds"))

out <-
  SaveData(
    domain = "suafbs",
    dataset = "sua_unbalanced",
    data = data_to_save_unbalanced,
    waitTimeout = 20000
  )

if (STOP_AFTER_DERIVED == TRUE) {
  dbg_print("stop after production of derived")

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

computed_shares_send <- rbindlist(computed_shares_send, fill = TRUE)

# XXX
computed_shares_send <- unique(computed_shares_send)

colnames(computed_shares_send) <-
  sub("Value_", "", colnames(computed_shares_send))

setnames(
  computed_shares_send,
  c("timePointYears", "measuredItemSuaFbs", "measuredItemParentCPC"),
  c("year", "measuredItemChildCPC_tree", "measuredItemParentCPC_tree")
)

computed_shares_send <-
  nameData(
    "suafbs",
    "ess_fbs_commodity_tree2",
    computed_shares_send
  )

setnames(
  computed_shares_send,
  c("geographicAreaM49", "geographicAreaM49_description",
    "measuredItemChildCPC_tree", "measuredItemChildCPC_tree_description",
    "measuredItemParentCPC_tree", "measuredItemParentCPC_tree_description"),
  c("Country", "Country_name", "Child", "Child_name", "Parent", "Parent_name")
)

computed_shares_send[, zero_weigth := Child %chin% zeroWeight]

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

  negative_availability <-
    nameData("suafbs", "sua_unbalanced", negative_availability)

  negative_availability[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

}

data <-
  plyr::ddply(
    data,
    .variables = c('geographicAreaM49', 'timePointYears'),
    .fun = function(x) addMissingElements(as.data.table(x), p)
  )

setDT(data)


dbg_print("start outliers")

######################## OUTLIERS #################################
# re-writing of Cristina's outliers plugin with data.table syntax #

if (FIX_OUTLIERS == TRUE) {

  sel_vars <- c('geographicAreaM49', 'timePointYears',
                'measuredItemSuaFbs', 'measuredElementSuaFbs')

  commDef <- ReadDatatable("fbs_commodity_definitions") # XXX: updated?

  stopifnot(nrow(commDef) > 0)

  primaryProxyPrimary_items <-
    commDef[proxy_primary == "X" | primary_commodity == "X"]$cpc

  food_items <- commDef[food_item == "X"]$cpc

  dout <-
    CJ(
      geographicAreaM49 = unique(data$geographicAreaM49),
      timePointYears = unique(data$timePointYears),
      measuredItemSuaFbs = unique(data$measuredItemSuaFbs),
      measuredElementSuaFbs = unique(data$measuredElementSuaFbs)
    )

  dout <- data[dout, on = sel_vars]

  dout[is.na(Protected), Protected := FALSE]

  dout[,
    `:=`(
      production = Value[measuredElementSuaFbs  == "production"],
      supply     = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                       - Value[measuredElementSuaFbs %in% c("exports", "stockChange")],
                       na.rm = TRUE),
      domsupply  = sum(Value[measuredElementSuaFbs %in% c("production", "imports")],
                       na.rm = TRUE)
    ),
    by = c('geographicAreaM49', 'measuredItemSuaFbs', 'timePointYears')
  ]

  dout[measuredElementSuaFbs %chin% c("feed", "industrial"), element_supply := supply]
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
      measuredElementSuaFbs %chin% c("feed", "seed", "loss", "industrial") &
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
      !(measuredItemSuaFbs %chin% food_items &
          measuredItemSuaFbs %chin% primaryProxyPrimary_items),
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
    
    data <- dt_full_join(data, dout, by = sel_vars)
    
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

fbsTree <- ReadDatatable("fbs_tree")

######################## rm new loss in dairy/meat #################

dairy_meat_items <- fbsTree[id3 %chin% c("2948", "2943"), item_sua_fbs]

# TODO: Some of the code below is repeated. It should be rewritten so
# that there is a single computation of new elements.

data_complete_loss <-
  data.table(
    geographicAreaM49 = unique(data$geographicAreaM49),
    timePointYears = sort(unique(data$timePointYears)))[
      unique(
        data[
          measuredElementSuaFbs == "loss" & !is.na(Value),
          c("geographicAreaM49", "measuredItemSuaFbs"),
          with = FALSE
        ]
      ),
      on = "geographicAreaM49",
      allow.cartesian = TRUE
    ]

data_complete_loss <-
  dt_left_join(
    data_complete_loss,
    data[
      measuredElementSuaFbs == "loss" & Value > 0,
      c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears", "Value"),
      with = FALSE
    ],
    by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
  )

data_complete_loss[, y := 1]


data_complete_loss <-
  data_complete_loss[,
    .(
      t_pre   = sum(y[timePointYears <= 2013]),
      t_post  = sum(y[timePointYears >= 2014]),
      na_pre  = sum(is.na(Value[timePointYears <= 2013])),
      na_post = sum(is.na(Value[timePointYears >= 2014]))
    ),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
  ]


new_elements_loss <-
  data_complete_loss[
    na_pre == t_pre & na_post < t_post
  ][
    order(geographicAreaM49, measuredItemSuaFbs),
    c("geographicAreaM49", "measuredItemSuaFbs"),
    with = FALSE
  ]

new_loss_dairy_meat <-
  new_elements_loss[measuredItemSuaFbs %chin% dairy_meat_items]

new_loss_dairy_meat[, new_loss_dm := TRUE]

data <-
  new_loss_dairy_meat[data, on = c("geographicAreaM49", "measuredItemSuaFbs")]

data[
  new_loss_dm == TRUE & timePointYears >= 2014 & measuredElementSuaFbs == "loss",
  `:=`(
    Value = NA_real_,
    flagObservationStatus = NA_character_,
    flagMethod = NA_character_
  )
]

data[, new_loss_dm := NULL]


######################## / rm new loss in dairy/meat #################




# Protect all loss data, to keep it consistent with SDG indicator,
# whatever the flag is.
data[measuredElementSuaFbs == "loss", Protected := TRUE]



data <- dt_left_join(data, itemMap, by = "measuredItemSuaFbs")


## Split data based on the two factors we need to loop over
uniqueLevels <- unique(data[, c("geographicAreaM49", "timePointYears"), with = FALSE])

uniqueLevels <- uniqueLevels[order(geographicAreaM49, timePointYears)]

data[, stockable := measuredItemSuaFbs %chin% stockable_items]


# Remove stocks for non stockable items
data <- data[!(measuredElementSuaFbs == 'stockChange' & stockable == 'FALSE')]

data <- data[measuredElementSuaFbs != 'residual']

# Tourism consumption will be in the data only for selected countries
# and needs to be protected
data[measuredElementSuaFbs == "tourist", Protected := TRUE]


######################### Save UNB for validation #######################

sua_unbalanced <-
  data[,
    c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs",
      "measuredElementSuaFbs", "Value", "flagObservationStatus", "flagMethod"),
    with = FALSE
  ]

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


# Filter elements that appear for the first time

sel_vars <-
  c("geographicAreaM49", "measuredItemSuaFbs", "measuredElementSuaFbs")

data_complete <-
  data.table(
    geographicAreaM49 = unique(data$geographicAreaM49),
    timePointYears = sort(unique(data$timePointYears))
  )[
    unique(data[, sel_vars, with = FALSE]),
    on = "geographicAreaM49",
    allow.cartesian = TRUE
  ]

sel_vars <-
  c("geographicAreaM49", "measuredItemSuaFbs",
    "measuredElementSuaFbs", "timePointYears")

data_complete <-
  dt_left_join(
    data_complete,
    data[Value > 0, c(sel_vars, "Value"), with = FALSE],
    by = sel_vars
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
  ][
    order(geographicAreaM49, measuredElementSuaFbs, measuredItemSuaFbs),
    .(geographicAreaM49, measuredElementSuaFbs, measuredItemFbsSua = measuredItemSuaFbs)
  ]

new_loss <-
  new_elements[
    measuredElementSuaFbs == "loss",
    .(geographicAreaM49, measuredItemSuaFbs = measuredItemFbsSua, new_loss = TRUE)
  ]

new_elements <- nameData("suafbs", "sua_unbalanced", new_elements, except = "measuredElementSuaFbs")

new_elements[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

tmp_file_new <- file.path(TMP_DIR, paste0("NEW_ELEMENTS_", COUNTRY, ".csv"))

write.csv(new_elements, tmp_file_new)

# / Filter elements that appear for the first time


dbg_print("set thresholds")


if (THRESHOLD_METHOD == 'share') {
  
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

  data[,
    supply := ifelse(measuredElementSuaFbs == "seed", Value[measuredElementSuaFbs == "production"], supply),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]

  data[supply < 0, supply := 0]

  data[
    !(measuredElementSuaFbs %chin% c("production", "imports", "exports", "stockChange")),
    util_share := Value / supply
  ]

  data[is.infinite(util_share) | is.nan(util_share), util_share := NA_real_]

  # share < 0 shouldn't happen. Also, tourist can be negative.
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
  dt_left_join(
    data,
    new_loss,
    by = c('geographicAreaM49', 'measuredItemSuaFbs')
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
# protected figures, so that they are not changed
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
    Food_Median       = median(Value[measuredElementSuaFbs == "food" & timePointYears %in% 2000:2013], na.rm=TRUE),
    Feed_Median       = median(Value[measuredElementSuaFbs == "feed" & timePointYears %in% 2000:2013], na.rm=TRUE),
    Industrial_Median = median(Value[measuredElementSuaFbs == "industrial" & timePointYears %in% 2000:2013], na.rm=TRUE)
  ),
  by = c("geographicAreaM49", "measuredItemSuaFbs")
]


z_comp_shares <- copy(computed_shares_send)

z_comp_shares[, Protected_exports := NULL]
z_comp_shares[, Protected_imports := NULL]
z_comp_shares[, Protected_production := NULL]
z_comp_shares[, Protected_stockChange := NULL]

# FIXME: fix above
z_comp_shares[, Parent := sub("'", "", Parent)]
z_comp_shares[, Child := sub("'", "", Child)]


setcolorder(
  z_comp_shares,
  c("Country", "Country_name", "Parent", "Parent_name",
    "Child", "Child_name", "year", "production", "imports", "exports",
    "stockChange", "extractionRate", "shareDownUp", "shareUpDown",
    "availability", "Pshare", "zero_weigth", "Processed_parent")
)

dbg_print("SHARES workbook, create")

wb <- createWorkbook()
addWorksheet(wb, "SHARES")
style_protected <- createStyle(fgFill = "pink")
style_text <- createStyle(numFmt = "TEXT")
style_comma <- createStyle(numFmt = "COMMA")
writeData(wb, "SHARES", z_comp_shares)

for (i in c("exports", "imports", "production", "stockChange")) {
  a_col <- grep(paste0("^", i, "$"), names(z_comp_shares))
  a_rows <- which(computed_shares_send[[paste0("Protected_", i)]] == TRUE) + 1

  if (all(length(a_col) > 0, length(a_rows) > 0)) {
    addStyle(wb, sheet = "SHARES", style_protected, rows = a_rows, cols = a_col)
  }
}

addStyle(wb, sheet = "SHARES", style_text, rows = 1:nrow(z_comp_shares) + 1, cols = grep("^(Parent|Child)$", names(z_comp_shares)), gridExpand = TRUE, stack = TRUE)
addStyle(wb, sheet = "SHARES", style_comma, rows = 1:nrow(z_comp_shares) + 1, cols = grep("^(production|imports|exports|stockChange|availability_parent|processed)$", names(z_comp_shares)), gridExpand = TRUE, stack = TRUE)

addFilter(wb, "SHARES", row = 1, cols = 1:ncol(z_comp_shares))

dbg_print(paste("SHARES workbook, save", getwd()))

saveWorkbook(wb, tmp_file_shares, overwrite = TRUE)


## 1 => year = 2014
i <- 1

dbg_print("starting balancing loop")

# XXX Only from 2004 onwards
uniqueLevels <- uniqueLevels[timePointYears >= 2014][order(timePointYears)]

standData <- vector(mode = "list", length = nrow(uniqueLevels))

for (i in seq_len(nrow(uniqueLevels))) {

  dbg_print(paste("in balancing loop, start", i))

  # For stocks, the first year no need to see back in time. After the first year was done,
  # stocks may have changed, so opening need to be changed in "data".

  if (i > 1) {
    dbg_print(paste("check stocks change in balancing", i))

    items_stocks_changed <-
      unique(standData[[i-1]][!is.na(change_stocks)]$measuredItemSuaFbs)

    if (length(items_stocks_changed) > 0) {

      dbg_print(paste("recalculate stocks changed", i))
    
      stocks_modif <-
        rbind(
          # Previous data (balanced)
          rbindlist(standData)[
            !is.na(Value) &
              measuredElementSuaFbs == 'stockChange' &
              measuredItemSuaFbs %chin% items_stocks_changed,
            list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value)
          ],
          # New data (unbalanced)
          data[
            !is.na(Value) &
              timePointYears > unique(standData[[i-1]]$timePointYears) &
              measuredElementSuaFbs == 'stockChange' &
              measuredItemSuaFbs %chin% items_stocks_changed,
            list(geographicAreaM49, timePointYears, measuredItemSuaFbs, delta = Value)
          ]
        )

      data_for_opening <-
        dt_left_join(
          all_opening_stocks[
            timePointYears >= 2014 &
              measuredItemFbsSua %chin% items_stocks_changed,
            .(geographicAreaM49,  measuredItemSuaFbs =  measuredItemFbsSua,
              timePointYears, new_opening = Value)
          ],
          stocks_modif,
          by = c("geographicAreaM49", "measuredItemSuaFbs", "timePointYears")
        )

      data_for_opening[is.na(delta), delta := 0]

      data_for_opening <- data_for_opening[order(geographicAreaM49, measuredItemSuaFbs, timePointYears)]

      dbg_print(paste("update opening stocks", i))

      data_for_opening <- update_opening_stocks(data_for_opening)

      dbg_print(paste("merge data in opening stocks", i))

      all_opening_stocks <-
        dt_left_join(
          all_opening_stocks,
          data_for_opening[,
            .(
              geographicAreaM49,
              measuredItemFbsSua = measuredItemSuaFbs,
              timePointYears,
              new_opening
            )
          ],
          by = c("geographicAreaM49", "measuredItemFbsSua", "timePointYears")
        )

      dbg_print(paste("new_opening", i))

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

  sel_vars <- c("geographicAreaM49", "timePointYears")

  treeSubset <- tree[filter, on = sel_vars]
  
  treeSubset[, sel_vars := NULL, with = FALSE]
  
  dataSubset <- data[filter, on = sel_vars]

  dbg_print(paste("actual balancing", i))

  standData[[i]] <- 
    newBalancing(
      data = dataSubset,
      #nutrientData = subNutrientData,
      #batchnumber = batchnumber,
      Utilization_Table = Utilization_Table
    )

  # FIXME: we are now assigning the "Protected" flag to ALL processing as
  # after the first loop it should have been computed and that value SHOULD
  # never be touched again.
  standData[[i]][measuredElementSuaFbs == "foodManufacturing", Protected := TRUE]

  dbg_print(paste("in balancing loop, end", i))
}

all_opening_stocks[, measuredElementSuaFbs := "5113"]

dbg_print("end of balancing loop")

standData <- rbindlist(standData)

calculateImbalance(standData)

standData[
  supply > 0,
  imbalance_percent := imbalance / supply * 100
]

# If the imbalance is relatively small (less than 5% in absoulte value)
# a new allocation is done, this time with no limits.

# Here we need to protect seed, because of its lik to production.
standData[measuredElementSuaFbs == "seed", Protected := TRUE]

dbg_print("balancing of imbalances < 5%")

if (nrow(standData[data.table::between(imbalance_percent, -5, 5)]) > 0) {

  standData[, can_balance := FALSE]

  # The following two instructions basically imply to assign the
  # (small) imbalance with no limits (except food, among utilizations)

  sel_vars <- c("production", "imports", "exports", "stockChange", "food")

  standData[
    measuredElementSuaFbs %!in% sel_vars &
      Protected == FALSE &
      !is.na(min_threshold),
    min_threshold := 0
  ]

  standData[
    measuredElementSuaFbs %!in% sel_vars &
      Protected == FALSE &
      !is.na(max_threshold),
    max_threshold := Inf
  ]

  standData[
    !is.na(Value) &
      Protected == FALSE &
      !(data.table::between(Value, min_threshold, max_threshold) %in% FALSE) &
      !(measuredElementSuaFbs %chin%
        c("production", "imports", "exports", "stockChange", "foodManufacturing")),
    can_balance := TRUE
  ]

  standData[,
    elements_balance := any(can_balance),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]
  
  standData[
    data.table::between(imbalance_percent, -5, 5) &
      elements_balance == TRUE,
    adjusted_value := balance_proportional(.SD),
    by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs")
  ]

  standData[
    !is.na(adjusted_value) & adjusted_value != Value,
    `:=`(
      Value = adjusted_value,
      flagObservationStatus = "E",
      flagMethod = "n"
    )
  ]

  standData[, adjusted_value := NULL]
}

calculateImbalance(standData)

standData[
  supply > 0,
  imbalance_percent := imbalance / supply * 100
]


######################### Save BAL for validation #######################

dbg_print("sua_balanced for validation")

sel_vars <-
  c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs",
    "measuredElementSuaFbs", "Value", "flagObservationStatus", "flagMethod")

sua_balanced <-
  rbind(
    data[timePointYears < 2014, sel_vars, with = FALSE],
    standData[, sel_vars, with = FALSE]
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
    !is.na(Value) &
      outside(imbalance, -100, 100) &
      timePointYears >= 2014,
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
  dt_left_join(
    imbalances_to_send,
    unique(standData[, .(country = geographicAreaM49, year = timePointYears,
                         measuredItemSuaFbs, supply, utilizations, imbalance,
                         imbalance_percent)]),
    by = c("country", "year", "measuredItemSuaFbs")
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
  nameData('suafbs', 'sua_unbalanced', imbalances_to_send,
           except = c('measuredElementSuaFbs'))

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

write.csv(imbalances_to_send,          tmp_file_imb)
write.csv(negative_availability,       tmp_file_negative)
write.csv(non_existing_for_imputation, tmp_file_non_exist)
write.csv(fixed_proc_shares,           tmp_file_fix_shares)
write.csv(data_negtrade,               tmp_file_NegNetTrade)

saveRDS(
  computed_shares_send,
  file.path(R_SWS_SHARE_PATH, 'mongeau', paste0('computed_shares_send_', COUNTRY, '.rds'))
)




# FIXME: see also the one done for elementToCodeNames
codes <- as.data.table(tibble::tribble(
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
))

standData <- standData[codes, on = c('measuredElementSuaFbs' = 'name')]


standData <-
  standData[,
    c("geographicAreaM49", "code", "measuredItemSuaFbs", "timePointYears",
      "Value", "flagObservationStatus", "flagMethod", "Protected"),
    with = FALSE
  ]

setnames(
  standData,
  c("code", "measuredItemSuaFbs"),
  c("measuredElementSuaFbs", "measuredItemFbsSua")
)

standData <- standData[!is.na(Value)]

# These cases should not happen (i.e., all flags should already be
# set), but in any case, add specific flags so to check.
standData[is.na(flagObservationStatus), flagObservationStatus := "M"]
standData[is.na(flagMethod), flagMethod := "q"]


# Calculate calories

calories_per_capita <-
  dt_left_join(
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
    by = c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua')
  )

calories_per_capita <-
  dt_left_join(
    calories_per_capita,
    popSWS[, list(geographicAreaM49, timePointYears, population = Value)],
    by = c('geographicAreaM49', 'timePointYears')
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
    measuredElementSuaFbs %chin%
      c("5510", "5023", "5016", "5165", "5520", "5525", "5164", "5141")
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
  dt_left_join(
    standData,
    old_sua_bal,
    by = c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")
  )

new_sua_bal[
  measuredElementSuaFbs %chin%
    c("5510", "5023", "5016", "5165", "5520", "5525", "5164", "5141"),
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
  new_sua_bal[
    measuredElementSuaFbs %chin%
      c("5510", "5023", "5016", "5165", "5520", "5525", "5164", "5141")
  ]

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

  sel_vars <-
    c("geographicAreaM49", "measuredItemFbsSua", "measuredElementSuaFbs")

  out_elems_items <-
    rbind(
      data_suabal[timePointYears <= 2013][unique(new_sua_bal[, sel_vars, with = FALSE]), on = sel_vars],
      standData[unique(new_sua_bal[, sel_vars, with = FALSE]), on = sel_vars]
    )

  out_elems_items <-
    dt_left_join(
      out_elems_items,
      new_sua_bal,
      by = c("geographicAreaM49", "measuredItemFbsSua",
             "measuredElementSuaFbs", "timePointYears")
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
    new_sua_bal[0, .(geographicAreaM49, measuredElementSuaFbs,
                     measuredItemFbsSua, timePointYears, Value,
                     flagObservationStatus, flagMethod,
                     perc.change = NA_real_,
                     Historical_value_2010_2013 = NA_real_, outlier)]
}

out_elems_items <- rbind(combined_calories, out_elems_items, fill = TRUE)

out_elems_items[, Value := round(Value, 2)]
out_elems_items[, perc.change := round(perc.change, 2)]
out_elems_items[, Historical_value_2010_2013 := round(Historical_value_2010_2013, 2)]


tmp_file_outliers <- file.path(TMP_DIR, paste0("OUTLIERS_", COUNTRY, ".csv"))

out_elems_items <-
  nameData("suafbs", "sua_unbalanced", out_elems_items, except = "timePointYears")

write.csv(out_elems_items, tmp_file_outliers)


######### / Combine old and new calories and data, and get outliers ###########


if (nrow(data_suabal[timePointYears >= 2014]) > 0) {

  sel_vars <- 
    c('geographicAreaM49', 'measuredElementSuaFbs',
      'measuredItemFbsSua', 'timePointYears') 

  data_suabal_missing <-
    data_suabal[timePointYears >= 2014][!standData, on = sel_vars]
  
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

sel_vars <- c('geographicAreaM49', 'timePointYears', 'measuredItemFbsSua')

des <- 
  rbind(
    data_suabal[
      measuredElementSuaFbs == '664' & timePointYears %in% 2010:2013,
      .(Value = sum(Value, na.rm = TRUE)),
      by = sel_vars 
    ],
    standData[
      measuredElementSuaFbs == '664' & timePointYears >= 2014,
      .(Value = sum(Value, na.rm = TRUE)),
      by =  sel_vars
    ]
  )

des <-
  rbind(
    des,
    des[,
      .(measuredItemFbsSua = 'S2901', Value = sum(Value)),
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
    order(geographicAreaM49, measuredItemFbsSua, timePointYears),
    .(year = timePointYears, diff = Value - shift(Value)),
    .(geographicAreaM49, item = measuredItemFbsSua)
  ][
    year != min(year)
  ]

des_diff <- des_diff[item %chin% des_diff[abs(diff) > 20]$item]

des_diff[item == "S2901", item := "GRAND TOTAL"]

plot_main_des_diff <-
  ggplot(des_diff, aes(x = year, diff, group = item, color = item)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    ggtitle("Absolute variation of DES and main variations of items",
            subtitle = COUNTRY_NAME)

tmp_file_plot_main_des_diff <-
  file.path(TMP_DIR, paste0("PLOT_MAIN_DES_DIFF_", COUNTRY, ".pdf"))

ggsave(tmp_file_plot_main_des_diff, plot = plot_main_des_diff)

##### / Plot of main DES absolute variations

##### Plot of main DES

main_des_items <-
  des[,
    .(geographicAreaM49, year = timePointYears, item = measuredItemFbsSua, Value)
  ][
    item %chin% des[Value > 100]$measuredItemFbsSua
  ][
    order(geographicAreaM49, item, year)
  ]

main_des_items[item == "S2901", item := "GRAND TOTAL"]

plot_main_des_items <-
  ggplot(main_des_items, aes(x = year, Value, group = item, color = item)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    ggtitle("Main DES items (> 100 Calories)", subtitle = COUNTRY_NAME)

tmp_file_plot_main_des_items <-
  file.path(TMP_DIR, paste0("PLOT_MAIN_DES_ITEMS_", COUNTRY, ".pdf"))

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
  file.path(TMP_DIR, paste0("PLOT_DES_MAIN_DIFF_AVG_", COUNTRY, ".pdf"))

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
des_cast[
  measuredItemFbsSua == "S2901",
  measuredItemFbsSua_description :=
    paste0("000", measuredItemFbsSua_description)
]

des_cast <- des_cast[order(measuredItemFbsSua_description)]

des_cast[,
  measuredItemFbsSua_description :=
    sub("^000", "", measuredItemFbsSua_description)
]


# Main items, more then 90%

des_main_90 <-
  des[
    measuredItemFbsSua != "S2901" & timePointYears >= 2014,
    .(tot = sum(Value)),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ][
    order(-tot),
    cumsum := cumsum(tot)
  ]

des_main_90 <-
  des_main_90[
    des[
      measuredItemFbsSua != "S2901" & timePointYears >= 2014,
      .(maintot = sum(Value)),
      by = "geographicAreaM49"
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

########## create XLSX for main DES items

des_main_90_and_tot <-
  rbind(
    data.table(
      geographicAreaM49 = unique(des_main_90$geographicAreaM49),
      measuredItemFbsSua = "S2901"
    ),
    des_main_90
  )

des_level_diff <-
  des[
    order(geographicAreaM49, measuredItemFbsSua, timePointYears),
    .(
      Value = Value - shift(Value),
      timePointYears
    ),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ]

des_level_diff_cast <-
  data.table::dcast(
    des_level_diff,
    geographicAreaM49 + measuredItemFbsSua ~ timePointYears,
    fun.aggregate = sum,
    value.var = "Value"
  )


des_perc_diff <-
  des[
    order(geographicAreaM49, measuredItemFbsSua, timePointYears),
    .(
      Value = Value / shift(Value) - 1,
      timePointYears = timePointYears
    ),
    by = c("geographicAreaM49", "measuredItemFbsSua")
  ]

des_perc_diff[is.infinite(Value) | is.nan(Value), Value := NA_real_]

des_perc_diff_cast <-
  data.table::dcast(
    des_perc_diff,
    geographicAreaM49 + measuredItemFbsSua ~ timePointYears,
    fun.aggregate = sum,
    value.var = "Value"
  )

sel_vars <-
  c("geographicAreaM49", "geographicAreaM49_description",
    "measuredItemFbsSua", "measuredItemFbsSua_description")

by_vars <- c("geographicAreaM49", "measuredItemFbsSua")

des_level_diff_cast <-
  merge(des_level_diff_cast, des_cast[, sel_vars, with = FALSE ], by = by_vars)

setcolorder(des_level_diff_cast, names(des_cast))

des_perc_diff_cast <-
  merge(des_perc_diff_cast, des_cast[, sel_vars, with = FALSE], by = by_vars)

setcolorder(des_perc_diff_cast, names(des_cast))

des_level_diff_cast <- des_level_diff_cast[des_main_90_and_tot, on = by_vars]

des_perc_diff_cast <- des_perc_diff_cast[des_main_90_and_tot, on = by_vars]


wb <- createWorkbook()

addWorksheet(wb, "DES_MAIN")
addWorksheet(wb, "DES_MAIN_diff")
addWorksheet(wb, "DES_MAIN_diff_perc")

writeData(wb, "DES_MAIN", des_main)
writeData(wb, "DES_MAIN_diff", des_level_diff_cast)
writeData(wb, "DES_MAIN_diff_perc", des_perc_diff_cast)

style_cal_0      <- createStyle(fgFill = "black", fontColour = "white", textDecoration = "bold")
style_cal_gt_10  <- createStyle(fgFill = "lightyellow")
style_cal_gt_20  <- createStyle(fgFill = "yellow")
style_cal_gt_50  <- createStyle(fgFill = "orange")
style_cal_gt_100 <- createStyle(fgFill = "red")
style_percent    <- createStyle(numFmt = "PERCENTAGE")
style_mean_diff  <- createStyle(borderColour = "blue", borderStyle = "double", border = "TopBottomLeftRight")

# All zeros in 2014-2017, that were not zeros in 2010-2013
zeros <-
  des_main[, .SD, .SDcols = as.character(2014:2017)] == 0 &
  des_main[, rowMeans(.SD), .SDcols = as.character(2010:2013)] > 0

for (i in which(names(des_level_diff_cast) %in% 2011:2017)) {
  addStyle(wb, "DES_MAIN", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 10], style = style_cal_gt_10, gridExpand = TRUE)
  addStyle(wb, "DES_MAIN", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 20], style = style_cal_gt_20, gridExpand = TRUE)
  addStyle(wb, "DES_MAIN", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 50], style = style_cal_gt_50, gridExpand = TRUE)
  addStyle(wb, "DES_MAIN", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 100], style = style_cal_gt_100, gridExpand = TRUE)

  if (names(des_level_diff_cast)[i] %in% 2014:2017) {
    j <- names(des_level_diff_cast)[i]
    rows_with_zero <- zeros[, j]
    if (any(rows_with_zero == TRUE)) {
      addStyle(wb, "DES_MAIN", cols = i, rows = 1 + (1:nrow(zeros))[zeros[, j]], style = style_cal_0, gridExpand = TRUE)
    }
  }

  addStyle(wb, "DES_MAIN_diff", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 10], style = style_cal_gt_10, gridExpand = TRUE)
  addStyle(wb, "DES_MAIN_diff", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 20], style = style_cal_gt_20, gridExpand = TRUE)
  addStyle(wb, "DES_MAIN_diff", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 50], style = style_cal_gt_50, gridExpand = TRUE)
  addStyle(wb, "DES_MAIN_diff", cols = i, rows = 1 + (1:nrow(des_level_diff_cast))[abs(des_level_diff_cast[[i]]) > 100], style = style_cal_gt_100, gridExpand = TRUE)
}

high_variation_mean <-
  abs(des_main[, rowMeans(.SD), .SDcols = as.character(2010:2013)] / des_main[, rowMeans(.SD), .SDcols = as.character(2014:2017)] - 1) > 0.30

if (any(high_variation_mean == TRUE)) {
  high_variation_mean <- 1 + (1:nrow(des_main))[high_variation_mean]
  addStyle(wb, sheet = "DES_MAIN", style_mean_diff, rows = high_variation_mean, cols = which(names(des_level_diff_cast) %in% 2014:2017), gridExpand = TRUE, stack = TRUE)
}


addStyle(wb, sheet = "DES_MAIN_diff_perc", style_percent, rows = 1:nrow(des_level_diff_cast) + 1, cols = which(names(des_level_diff_cast) %in% 2011:2017), gridExpand = TRUE, stack = TRUE)

setColWidths(wb, "DES_MAIN", 4, 40)
setColWidths(wb, "DES_MAIN_diff", 4, 40)
setColWidths(wb, "DES_MAIN_diff_perc", 4, 40)

tmp_file_des_main <- file.path(TMP_DIR, paste0("DES_MAIN_ITEMS_", COUNTRY, ".xlsx"))

saveWorkbook(wb, tmp_file_des_main, overwrite = TRUE)


########## / create XLSX for main DES items


tmp_file_des <- file.path(TMP_DIR, paste0("DES_", COUNTRY, ".csv"))

#des_cast[, measuredItemFbsSua := paste0("'", measuredItemFbsSua)]

write.csv(des_cast, tmp_file_des)

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
      I,i: Residual item (identity); stocks as 20%% of supply in 2013
      I,-: Opening stocks as cumulated stocks variations 1961-2013
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

      - DES_MAIN_ITEMS_*.xlsx = as DES_*.csv, but only with items that
          accounted for nearly 90%% on average over 2014-2017

      - DES_*.csv = calculation of DES (total and by items)

      - OUTLIERS_*.csv = outliers in Calories (defined as those that account
          for more than 5 Calories with an increase of more than 15%%)

      - SHARES_*.xlsx = Parents/Children shares, availability, etc.

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
      subject = "Results from SUA_bal_compilation plugin",
      body = c(body_message,
               tmp_file_plot_main_des_items,
               tmp_file_plot_main_des_diff,
               tmp_file_plot_des_main_diff_avg,
               tmp_file_des_main,
               tmp_file_des,
               tmp_file_outliers,
               tmp_file_shares,
               tmp_file_imb,
               tmp_file_extr,
               tmp_file_negative,
               tmp_file_non_exist,
               tmp_file_new,
               tmp_file_fix_shares,
               tmp_file_NegNetTrade
              )
    )
  }
  
  unlink(TMP_DIR, recursive = TRUE)

  print(paste(out$inserted + out$ignored, "observations written and problems with", out$discarded))
  
} else {
  
  print("The SUA_bal_compilation plugin had a problem when saving data.")
  
}

