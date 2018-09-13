#' Quickly get a D3 js sankey
#'
#' This function will take 4 columns of data, format the data, then feed it to networkD3 where it will be converted into a D3 plot.
#' @section Vignette:
#' This function has a vignette - \url{https://pages.github.roche.com/Rpackages/RochePlot/articles/pkgdown_only/sankey.html}
#' @section Bugs:
#' If you find an issue, post it here and we'll try and fix it: \url{http://go.roche.com/RochePlotissue}
#' @param dataframe the dataframe holding the data
#' @param id grouping variable that links the long data. Most likely patient id.
#' @param linename The name of the line - or whatever goes in the bars
#' @param linenumber A column with the numeric line number.
#' @param n_common The n most common treatments you want to include. Others will go into no treatment or other.
#' @param return_data FALSE by default. If true, it will return the formatted data for you to pass to the plotting function yourself.
#' @param postfix What do you want to go on the labels for the Sankey
#' @keywords teradata sankey
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' paste("See https://pages.github.roche.com/Rpackages/RochePlot/articles/pkgdown_only/sankey.html")

sankey <- function(
  dataframe,
  id = NULL,
  linename = NULL,
  linenumber = NULL,
  n_common = 3, # n most common teeatments to keep per line
  return_data = F,
  postfix = " patients"
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

  # messages ------------------------------------------------------------
  message(paste0(
    "Please manually validate the numbers - this function is unvalidated."
  ))

  # check input ------------------------------------------------------------
    # is character
    if (!is.character(id)) stop("id must be a character value")
    if (!is.character(linename)) stop("linename must be character value")
    if (!is.character(linenumber)) stop("linenumber must be character value")
    if (!is.character(postfix)) stop("postfix must be character")

    # is in dataframe
    if (!id %in% names(dataframe)) stop("id not in dataframe")
    if (!linename %in% names(dataframe)) stop("linename not in dataframe")
    if (!linenumber %in% names(dataframe)) stop("linenumber not in dataframe")

  # start ------------------------------------------------------------

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

  # find the n_common most common treatments in line 1 to n
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
          missing = "No recorded"
        )
      )
    # left join onto original to find missing
    temp2 <- dplyr::left_join(
      unique(temp_data["id"]),
      temp2,
      by = "id")
    temp2$linename_derived <- dplyr::if_else(
      !is.na(temp2$linename),
      paste0(temp2$linename," ",i,"L"),
      paste0("No recorded ",i,"L")
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
  temp_data2 <- stats::na.omit(temp_data2)

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
    ) %>% as.data.frame() %>%
    # change colours
    mutate(
      colour = dplyr::case_when(
        grepl("No recorded ",.$id_char) ~ "rgb(165,42,42)",
        grepl("Other Tx ",.$id_char) ~ "rgb(138,43,226)",
        TRUE ~ "rgb(30,144,255)"
      )
    )

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
    return(list(edges = edges,nodes = nodes))
  }

  # make the plot
    plotly::plot_ly(
      type = "sankey",
      domain = list(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valueformat = ".0f",
      valuesuffix = postfix,

      node = list(
        label = nodes$id_char,
        color = nodes$colour,
        pad = 15,
        thickness = 10,
        line = list(
          color = "black",
          width = 0.5
        )
      ),

      link = list(
        source = edges$idnum_source,
        target = edges$idnum_target,
        value =  edges$value
        #label =  d_sankey$source
      )
    ) %>% plotly::config(displayModeBar = F)

}
