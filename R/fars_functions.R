#' Reads a  CSV File
#'
#'This is a simple function that takes a file name as a input , check for existence,
#'if yes read that csv file and convert it into a data frame. If file doesnot exist, it will
#'return an error.
#'
#' @param filename a charater string giving the file that function will read
#'
#' @return this function returns the data frame
#' @export
#'
#' @importFrom dplyr tbl_df %>%
#' @importFrom readr read_csv
#'
#'
#' @examples
#' \dontrun{
#' fars_read("student_data.csv")
#' fars_read("accident_2013.csv.bz2")
#' }
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate File name from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System
#' \href{http://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{Fatality Analysis Reporting System (FARS)}.
#' This function generates the accident file name for a particular year taken as input. Entering other
#' than integer may throw a warning
#'
#' @param year a integer representing the year for which the file name to be generated
#'
#' @return this function returns the  accident file name for a particular year given as input
#'
#'  @examples
#'  \dontrun{
#'  make_filename(1992)
#'  make_filename("2012")
#'  }
#' @export
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read Accident files for years
#'
#' This function read files for series of  years and print Month and year column of the data frame
#'
#' @param years a numeric vector or list for which files need to be read
#'
#' @return a data frame containing Month and year of the file for each year, else a warning message
#' for invalid year
#'
#' @export
#' @importFrom dplyr mutate select %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(2014)
#' fars_read_years(1990)
#' }
#'
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#'
#' Summarize Accident over years
#'
#' This function summarize accidents over many years and return a key value pair for years and
#' number of accidents in that year.Throws an error for invalid state number
#'
#' @inheritParams fars_read_years
#'
#' @return returns a key value pair of year(key) and the total number of accidents(value) in that year
#'@export
#'
#' @importFrom dplyr bind_rows group_by summarize  %>%
#' @importFrom tidyr spread
#'
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014))
#' fars_summarize_years(2013)
#' }
#'
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot Accidents for a State
#'
#' This function plot the accidents for a state given as a input
#'
#' @param state.num a numeric code for a state(e.g 5 or 6)
#'
#' @param year accident year
#'
#'
#' @return a map plotting the accidents for the given state in an input year
#'
#' @section Warning:
#' Dont enter the invalid state or year,else function will generate an error.
#'
#' @export
#'
#' @importFrom dplyr filter %>%
#' @importFrom maps map
#' @importFrom graphics points
#'
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2015)
#' fars_map_state(6,2013)
#' fars_map_state(2,2013) #2 is invalid state
#' }
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
