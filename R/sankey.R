#' Quickly get a D3 js sankey
#'
#' This function will take 4 columns of data, format the data, then feed it to networkD3 where it will be converted into a D3 plot.
#' @section Bugs:
#' If you find an issue, post it here and we'll try and fix it: \url{http://go.roche.com/RocheRWDSissue}
#' @param dataframe the dataframe holding the data
#' @param id grouping variable that links the long data. Most likely patient id.
#' @param linename The name of the line - or whatever goes in the bars
#' @param linenumber A column with the numeric line number.
#' @param n_common The n most common treatments you want to include. Others will go into no treatment or other.
#' @param return_data FALSE by default. If true, it will return the formatted data for you to pass to the plotting function yourself.
#' @keywords teradata sankey
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
#'              RWDSsankey(
#'                dataframe = line.of.therapy,
#'                id = "PatientID",
#'                linename = "LineName",
#'                linenumber = "LineNumber",
#'                n_common = 3
#'              )
#'
#'           }

RWDSsankey <- function(
  dataframe,
  id,
  linename,
  linenumber,
  n_common = 5, # n most common teeatments to keep per line
  return_data = F
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
  if (!requireNamespace('networkD3', quietly = T)) {
    stop('You need to install networkD3 first. This package maps the data to D3')
  }

  if (!requireNamespace('dplyr', quietly = T)) {
    stop('One cannot live without dplyr. I refuse to go on till you install it.')
  }

  require('dplyr', quietly = T)

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

  # build a dataset of source, target and n!
  # recode into other if not of interest
  temp_data2 <- NULL
  for (i in minlines:maxlines) {
    # what is most common in this line?
    temp_line <- temp_mostcommon %>%
      dplyr::filter(line == i) %>%
      .$linename
    # recode
    temp2 <- temp_data %>%
      dplyr::filter(linenumber == i) %>%
      dplyr::mutate(
        linename = dplyr::if_else(
          linename %in% temp_line,
          linename,
          "Other Tx",
          missing = "No Tx"
        )
      )
    # left join onto original to find missing
    temp2 <- dplyr::left_join(
      unique(temp_data["id"]),
      temp2,
      by = "id")
    temp2$linename_derived <- if_else(
      !is.na(temp2$linename),
      paste0(temp2$linename," ",i,"L"),
      paste0("No Tx ",i,"L")
    )
    # new to check the old is the same
    temp2$linenumber_derived <- i

    # merge
    temp_data2 <- rbind(
      temp2,
      temp_data2
    )
  }

  # in each line, what's the next line?
  temp_data2 <- temp_data2 %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      nextline = dplyr::lead(linename_derived, order_by = linenumber_derived)
    ) %>%
    dplyr::arrange(linenumber_derived)

  # get count of each combo
  temp_data2 <- temp_data2 %>%
    dplyr::group_by(linename_derived,nextline) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::rename(
      source = linename_derived,
      target = nextline,
      value = n
    )

  # remove if line dead
  temp_data2 <- na.omit(temp_data2)

  # nodes (Tx)
  nodes <- data.frame(
    id_char = unique(c(temp_data2$source, temp_data2$target)),
    id_factor = as.factor(unique(c(temp_data2$source, temp_data2$target))),
    stringsAsFactors = F
  ) %>%
    dplyr::mutate(
      id_num = as.numeric(id_factor) - 1
    ) %>%
    # arrange, as order is used to match!
    dplyr::arrange(
      id_num
    ) %>% as.data.frame()

  # give numeric edges
  # will be referenced against nodes, so order crucial
  edges <- dplyr::left_join(
    temp_data2,
    nodes[c("id_char","id_num")],
    by = c("source" = "id_char")
  ) %>% dplyr::rename(
    idnum_source = id_num
  )  %>% as.data.frame()

  edges <- dplyr::left_join(
    edges,
    nodes[c("id_char","id_num")],
    by = c("target" = "id_char")
  ) %>% dplyr::rename(
    idnum_target = id_num
  ) %>% as.data.frame()

  if (return_data == T) {
    return(list(edges,nodes))
  }

  # make the plot
  networkD3::sankeyNetwork(
    Links = edges,
    Nodes = nodes,
    Source = "idnum_source",
    Target = "idnum_target",
    Value = "value",
    NodeID = "id_char",
    units = "people", fontSize = 12, nodeWidth = 30
  )

}
