#' Quickly get a D3 js parallel sets plot
#'
#' This function will take 4 columns of data, format the data, then feed it to parsetR where it will be converted into a D3 plot.
#' @section Bugs:
#' If you find an issue, post it here and we'll try and fix it: \url{http://go.roche.com/RocheRWDSissue}
#' @param dataframe the dataframe holding the data
#' @param id grouping variable that links the long data. Most likely patient id.
#' @param linename The name of the line - or whatever goes in the bars
#' @param linenumber A column with the numeric line number.
#' @param n_common The n most common treatments you want to include. Others will go into no treatment or other.
#' @param return_data FALSE by default. If true, it will return the formatted data for you to pass to the plotting function yourself.
#' @param ... passdown options
#' @keywords teradata parallel sets
#' @export
#' @importFrom magrittr "%>%"
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
#'                datalab = FALSE,
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
#'                  # just do lines 1 to 3
#'                  LineNumber < 4 & LineNumber > 0 &
#'                    # and ignore maintenence
#'                    IsMaintenanceTherapy == "False"
#'                )
#'
#'              ##### Use function ##################
#'
#'              RWDSparallelsets(
#'                dataframe = line.of.therapy,
#'                id = "PatientID",
#'                linename = "LineName",
#'                linenumber = "LineNumber",
#'                n_common = 3
#'              )
#'
#'           }

parallelsets <- function(
  dataframe,
  id,
  linename,
  linenumber,
  n_common = 5, # n most common teeatments to keep per line
  return_data = F,
  ...
){


  # messages
  message(paste0(
    "Please manually validate the numbers - this function is unvalidated."
  ))

  # ignore use within functions
  . <- NULL
  n <- NULL
  line <- NULL
  linename_clean <- NULL

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
  for(i in minlines:maxlines){
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

  temp_clean <- stats::reshape(temp_clean,
                        timevar = "linenumber",
                        idvar = c("id"),
                        direction = "wide") %>%
    dplyr::select(dplyr::starts_with("linename"))

  temp_clean[is.na(temp_clean)] <- "No line recorded"

  # get frequencies
  temp_clean <- as.data.frame(table(temp_clean))

  # rename columns
  names(temp_clean) <- c(
    paste("Line", minlines:maxlines),
    "Freq")

  # if you want data, return it instead of plotting

  if (return_data == T) {
    return(temp_clean)
  }

  # make the plot
  parsetR::parset(
    temp_clean,
    # dimensions are the categorical columns
    dimensions = colnames(temp_clean)[-ncol(temp_clean)],
    # use some JavaScript to inform parset that Freq has the value
    value = htmlwidgets::JS("function(d){return d.Freq}"),
    width = "100%", height = 400,
    tension = 0.7
  )

}
