#' Quickly get a D3 js sunburst
#'
#' This function will take 4 columns of data, format the data, then feed it to sunburstR where it will be converted into a D3 plot.
#' @section Bugs:
#' If you find an issue, post it here and we'll try and fix it: \url{http://go.roche.com/RocheRWDSissue}
#' @param dataframe the dataframe holding the data
#' @param id grouping variable that links the long data. Most likely patient id.
#' @param linename The name of the line - or whatever goes in the bars
#' @param linenumber A column with the numeric line number.
#' @param n_common The n most common treatments you want to include. Others will go into no treatment or other.
#' @param return_data FALSE by default. If true, it will return the formatted data for you to pass to the plotting function yourself.
#' @param legend_width Width of the legend in pixels. This will likely need tweaking.
#' @keywords teradata sunburst
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#'           ##### Get data from flatiron ##################
#'              library(dplyr)
#'
#'
#'              # data valid today
#'              valid_date <- Sys.Date()
#'
#'              # If on Windows, life is no fun
#'              if (Sys.info()[["sysname"]] == "Windows"){
#'                Sys.setenv(JAVA_HOME="")  # for java - go to 64bit
#'              }
#'
#'              # connect to data
#'              tdRWDSconnect(
#'                datalab = "RWDS_blackj9",
#'                type = "teradataR"
#'              )
#'
#'              line.of.therapy <- tdQuery(paste0(
#'                "
#'                SELECT
#'                patientid
#'                ,LineName
#'                ,LineNumber
#'                ,IsMaintenanceTherapy
#'                FROM rwd_vdm_flatiron.v_cll_lineoftherapy
#'                WHERE
#'                VALID_START <= CAST('",paste0(valid_date," 00:00:00"),"' AS TIMESTAMP) AND
#'                VALID_END >= CAST('",paste0(valid_date," 00:00:00"),"' AS TIMESTAMP)
#'                ;"
#'              )) %>%
#'                filter(
#'                  # just do lines 0 to 3
#'                  LineNumber < 4 &
#'                    # and ignore maintenence
#'                    IsMaintenanceTherapy == "False"
#'                )
#'
#'              ##### Use function ##################
#'
#'              RWDSsunburst(
#'                dataframe = line.of.therapy,
#'                id = "PatientID",
#'                linename = "LineName",
#'                linenumber = "LineNumber",
#'                n_common = 3,
#'                legend_width = 300
#'              )
#'
#'           }

RWDSsunburst <- function(
  dataframe,
  id,
  linename,
  linenumber,
  n_common = 5, # n most common teeatments to keep per line
  return_data = F,
  legend_width = 300
){
  # define so that within function calls don't create error
  n <- NULL
  line <- NULL
  . <- NULL
  linename_derived <- NULL
  linenumber_derived <- NULL
  nextline <- NULL
  id_factor <- NULL
  id_num <- NULL

  # needed pacakges
  if (!requireNamespace('sunburstR', quietly = T)) {
    stop('You need to install sunburstR::sunburst first. This package maps the data to D3')
  }

  if (!requireNamespace('dplyr', quietly = T)) {
    stop('One cannot live without dplyr. I refuse to go on till you install it.')
  }

  require('dplyr', quietly = T)

  if (!requireNamespace('tidyr', quietly = T)) {
    stop('One cannot live without tidyr. I refuse to go on till you install it.')
  }

  require('tidyr', quietly = T)

  # messages
  message(paste0(
    "Please manually validate the numbers - this function is unvalidated."
  ))

  # dataset to work with
  temp_data <- dataframe %>%
    # cut dataset
    dplyr::select_(id,linename,linenumber) %>%
    # rename to make programming simple!
    dplyr::rename_(
      "id" = id,
      "linename" = linename,
      "linenumber" = linenumber
    )

  # constants
  # max lines to go through
  minlines <- min(temp_data$linenumber, na.rm = T)
  maxlines <- max(temp_data$linenumber, na.rm = T)

  # find the n_common most common treatments in line 1 to three
  # placeholder
  temp_mostcommon <- NULL
  # fill it
  for (i in minlines:maxlines) {
    temp2 <- temp_data %>%
      dplyr::filter(linenumber == i) %>%
      dplyr::group_by(linename) %>%
      dplyr::summarise(
        n = n()
      ) %>%
      dplyr::mutate(line = i) %>%
      dplyr::top_n(n_common, n)
    # merge
    temp_mostcommon <- rbind(
      temp2,
      temp_mostcommon
    )
  }
  # scrub formatting and order
  temp_mostcommon <- temp_mostcommon %>%
    dplyr::arrange(line,n) %>%
    as.data.frame()

  # recode lines into other if not in common list
  # make a loop so adapts to n_common
  temp_clean <- NULL
  for(i in 1:maxlines){
    # most common for this line
    temp_list <- temp_mostcommon %>% dplyr::filter(line == i) %>% .$linename
    # make other if not in list
    temp_loop <- temp_data %>%
      dplyr::filter(linenumber == i) %>%
      dplyr::mutate(
        linename_clean = dplyr::case_when(
          .$linenumber == i & !.$linename %in% temp_list ~ "Other",
          TRUE ~ .$linename
        )
      )

    temp_clean <- rbind(temp_clean,temp_loop)
  }

  temp_clean <- temp_clean %>%
    dplyr::select(id,linenumber,linename_clean)

  temp_clean <- reshape(temp_clean,
                        timevar = "linenumber",
                        idvar = c("id"),
                        direction = "wide")

  temp_clean <- temp_clean %>%
    # merge columns
    tidyr::unite(lines,
          dplyr::starts_with("linename"),
          sep = "-",
          remove = T) %>%
    # remove -NA
    dplyr::mutate(
      sequence = gsub(
        x = lines
        , pattern = "(-NA*)"
        , replacement = ""
        , perl = T
      ),
      sequence = paste0(sequence,"-lines end")
    )

  # how many in each line?
  temp_clean <- temp_clean %>%
    dplyr::group_by(sequence) %>%
    dplyr::summarise(
      freq = n()
    ) %>% as.data.frame()

  # make a plot or give back the data?

  if (return_data == T) {
    return(temp_clean)
  }

  # make the plot
  sunburstR::sunburst(
    temp_clean,
    count = T,
    legend = list(w = legend_width)
  )

}
