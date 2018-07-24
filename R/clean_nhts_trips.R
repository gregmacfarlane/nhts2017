#' Clean the trips data frame of an NHTS set
#' 
#' @param df A `data_frame` containing the raw public NHTS data file.
#' @param year A text string to append to the `year` column of the 
#'   data frame identifying which year the data come from.
#'   
#' @details The variables in the NHTS data are distinctly un-tidy. This function
#'   builds factors for data that should be categorical and replaces the data
#'   names with tidy equivalents.
#'   
#' @return A tidy data frame with the following variables:
#' \describe{
#'   \item{houseid}{HOUSEID}
#'   \item{perid}{HOUSEID + PERSONID}
#'   \item{year}{The value of `year`}
#'   \item{time}{Start time of}
#'   \item{purpose}{TRIPPURP}
#'   \item{weight}{WTTRDFIN}
#'   \item{weekend}{Boolean, TRUE if on weekend}
#'   \item{period}{Period of the trip as a factor}
#'   \item{age}{Age of the tripmaker as a factor}
#'   \item{msasize}{Size of the MSA as a factor}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#' }
#' 
#' @export
clean_nhts_trips <- function (df, year = "2009") {
  
  # Define the labels and values of the factor variables
  times <-  c("Early AM", "AM Peak", "Morning", "Afternoon", "PM Peak", "Evening", "Night") 
  ages  <-  c("15 and under", "16 to 25", "26 to 35",  
              "36 to 65", "66 and over")
  msasizes <- c("Less than 250k", "250k - 499k", "500k to 999k",
                "1M to 2.9M", "More than 3M", "Not in MSA/CMSA") 
    
  # Munge the data frame
  df %>%
    transmute(
      hhid = HOUSEID,
      perid = stringr::str_c(hhid, PERSONID),
      year = !!year,
      start_time = as.numeric(STRTTIME),
      end_time = as.numeric(ENDTIME),
      purpose = TRIPPURP,
      weight = WTTRDFIN,
      weekend = ifelse(TDWKND == "01", TRUE, FALSE),
      period = case_when(
        start_time <= 600  ~ times[1],
        start_time <= 900  ~ times[2], 
        start_time <= 1300 ~ times[3], 
        start_time <= 1600 ~ times[4],
        start_time <= 1900 ~ times[5],
        start_time <= 2200 ~ times[6],
        start_time > 2200 | start_time <= 100 ~ times[7],
        TRUE ~ as.character(NA)
      ),
      age = case_when(
        R_AGE < 16  ~ ages[1],
        R_AGE < 26  ~ ages[2],
        R_AGE < 36  ~ ages[3],
        R_AGE < 66  ~ ages[4],
        R_AGE >= 66 ~ ages[5],
        TRUE ~ as.character(NA)
      ),
      msasize = case_when(
        MSASIZE == "01" ~ msasizes[1],
        MSASIZE == "02" ~ msasizes[2],
        MSASIZE == "03" ~ msasizes[3],
        MSASIZE == "04" ~ msasizes[4],
        MSASIZE == "05" ~ msasizes[5],
        MSASIZE == "06" ~ msasizes[6],
        TRUE ~ as.character(NA)
      ), 
      
      # convert factor variables to factors
      period = factor(period, levels = times),
      age = factor(age, levels = ages),
      msasize = factor(msasize, levels = msasizes)
    )
  
  
  
}


#' Build trips-in-motion data from cleaned NHTS trip records
#' 
#' @param trips A cleaned NHTS trips data frame
#' @param bin_size The size of the bin. Defaults to 15 minutes
#' 
build_trips_in_motion <- function(trips, bin_size = 15) {
  
  bins <- seq(0, 1439, bin_size)
  
  trips_with_minutes <- trips %>%
    filter(start_time != -9 | end_time != -9) %>%
    mutate(
      start = citycastr:::get_mins_from_baseline(start_time),
      end = citycastr:::get_mins_from_baseline(end_time)
    )
    
  lapply(bins, function(bin) {
    trips_with_minutes %>%
      filter(
        start <= bin & end > bin
      ) %>%
      mutate( bin = !! bin ) %>%
      group_by(year, purpose, bin) %>%
      summarise(
        trips = sum(weight)
      )
  }) %>%
    bind_rows() %>%
    ungroup() %>%
    mutate(
      bin = citycastr:::get_military_time(bin),
      hours = stringr::str_sub(bin, 1, 2),
      minutes = stringr::str_sub(bin, 3, 4),
      time = lubridate::ymd_hm(paste0("2018-01-01 ", hours, ":", minutes))
    ) %>%
    transmute(
      time, purpose, trips, 
      dataset = paste0(year, " NHTS")
    )
  
  
}


build_tim_from_demand <- function(demand, bin_size = 15) {
  
  bins <- seq(0, 1439, bin_size)
  
  spread_demand <- demand %>%
    spread(time_type, time) %>%
    mutate(
      end = ifelse(is.na(end), 1440, end),
      start = ifelse(is.na(start), 0, start)
    ) %>%
    group_by(per_id) %>%
    arrange(start, .by_group =TRUE) %>%
    mutate(
      from = lead(event_type),
      to = lag(event_type),
      purpose = ifelse(to == "Home" | from == "Home", "HBW", "Other"),
      purpose = ifelse(is.na(purpose), "Other", purpose)
    ) %>%
    select(per_id, from, event_type, to, start, end, purpose) %>%
    filter(event_type == "Travel")
  
  lapply(bins, function(bin) {
    spread_demand %>%
      filter(
        (start <= bin & end > bin) |          # trip ocurring in bin
        (start > bin & end < (bin + 15))     # trip completed in bin
      ) %>%
      mutate(  bin = !! bin ) %>%
      group_by(purpose, bin) %>%
      summarise(trips = n())
      
  }) %>%
    bind_rows() %>%
    mutate(
      bin = citycastr:::get_military_time(bin),
      hours = stringr::str_sub(bin, 1, 2),
      minutes = stringr::str_sub(bin, 3, 4),
      time = lubridate::ymd_hm(paste0("2018-01-01 ", hours, ":", minutes))
    ) %>%
    select(time, purpose, trips)
  
  
}


build_tim_from_supply <- function(supply, bin_size = 15) {
  
  bins <- seq(0, 1439, bin_size)
  
  lapply(bins, function(bin) {
    supply %>%
      filter(
        (start <= bin & end > bin) |          # trip ocurring in bin
        (start > bin & end < (bin + 15))     # trip completed in bin
      ) %>%
      mutate(  bin = !! bin ) %>%
      group_by(purpose, bin) %>%
      summarise(trips = n())
  }) %>%
    bind_rows() %>%
    mutate(
      bin = citycastr:::get_military_time(bin),
      hours = stringr::str_sub(bin, 1, 2),
      minutes = stringr::str_sub(bin, 3, 4),
      time = lubridate::ymd_hm(paste0("2018-01-01 ", hours, ":", minutes))
    ) %>%
    select(time, purpose, trips)
  
}

