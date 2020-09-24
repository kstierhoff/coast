#' Extract vertically integrated backscatter from CSV files
#'
#' @param filename Name or path to CSV file exported from Echoview.
#'
#' @return A data frame containing vertically integrated backscatter data.
#' @export
extract_csv <- function(filename) {
  # Read CSV file
  tmp <- data.table::fread(filename, sep = ",")

  # Summarize NASC by interval
  tmp %>%
    dplyr::group_by(Interval) %>%
    dplyr::summarise(
      long     = Lon_M[1],
      lat      = Lat_M[1],
      date     = Date_M[1],
      time     = as.character(Time_M[1]),
      dist_m   = Dist_M[1],
      NASC.5   = sum(NASC[Layer_depth_max <=   5]),
      NASC.10  = sum(NASC[Layer_depth_max <=  10]),
      NASC.10  = sum(NASC[Layer_depth_max <=  10]),
      NASC.15  = sum(NASC[Layer_depth_max <=  15]),
      NASC.20  = sum(NASC[Layer_depth_max <=  20]),
      NASC.25  = sum(NASC[Layer_depth_max <=  25]),
      NASC.30  = sum(NASC[Layer_depth_max <=  30]),
      NASC.35  = sum(NASC[Layer_depth_max <=  35]),
      NASC.40  = sum(NASC[Layer_depth_max <=  40]),
      NASC.45  = sum(NASC[Layer_depth_max <=  45]),
      NASC.50  = sum(NASC[Layer_depth_max <=  50]),
      NASC.55  = sum(NASC[Layer_depth_max <=  55]),
      NASC.60  = sum(NASC[Layer_depth_max <=  60]),
      NASC.65  = sum(NASC[Layer_depth_max <=  65]),
      NASC.70  = sum(NASC[Layer_depth_max <=  70]),
      NASC.75  = sum(NASC[Layer_depth_max <=  75]),
      NASC.80  = sum(NASC[Layer_depth_max <=  80]),
      NASC.85  = sum(NASC[Layer_depth_max <=  85]),
      NASC.90  = sum(NASC[Layer_depth_max <=  90]),
      NASC.95  = sum(NASC[Layer_depth_max <=  95]),
      NASC.100 = sum(NASC[Layer_depth_max <= 100]),
      NASC.150 = sum(NASC[Layer_depth_max <= 150]),
      NASC.250 = sum(NASC[Layer_depth_max <= 250]),
      NASC.350 = sum(NASC[Layer_depth_max <= 350]),
      NASC     = NASC.250,
      depth    = max(Layer_depth_max) + 3,
      filename = fs::path_file(filename),
      datetime = lubridate::ymd_hms(paste(date, time)))
}
