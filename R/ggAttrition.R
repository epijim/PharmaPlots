#' Visualise attrition data as a barplot
#'
#' @param data dataframe.
#' @param label Unquoted name of column indicating selection criteria
#' @param n Unquoted name of column indicating cohort size
#' @param sort Either NULL (no sorting), 'a' for ascending, or 'd' for descending
#' @param N_max numeric. Maximum value used in calculating percentages. Defaults to the maximum value observed in the data.
#' @param bar_fill character. Optional bar fill and border color
#' @param bar_col character. Optional bar border color
#' @param bar_alpha numeric. Alpha of bar
#' @param label_pos character. Position of labels, either "b", "m", or "l"
#' @param wrap_width integer. Optional integer indicating how much to wrap selection criteria text
#' @param flip logical. Should axes be flipped?
#' @param title_pos character. Position of title, either "m" for margins, or "l" for left side of plot
#' @param type character. Type of plot. Currently must be 'bar'
#' @param label_pos character. Position of labels, either "t" for top, or "b" for bottom.
#' @param show_p logical. Should percentages be included?
#' @param nudge_title numeric. Nudging of title
#' @param nudge_lab numeric. Nudging of label
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr arrange mutate enquo
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_wrap
#'
#' @export
#'
#' @examples
#'
#'  library(tidyverse)
#'
#'  data <- tibble("criteria" = c("Start",
#'                                "Criteria A ....",
#'                                "Criteria B ....",
#'                                "Criteria C ....",
#'                                "Criteria D ...."),
#'                 "size" = c(1000, 785, 456, 234, 154))
#'
#'
#' ggAttrition(data, criteria, size) +
#'   labs(title = "Attrition Table")
#'
#'
ggAttrition <- function(data,
                        label,
                        n,
                        sort = NULL,
                        show_p = TRUE,
                        N_max = NULL,
                        bar_fill = NULL,
                        bar_col = "black",
                        bar_alpha = .9,
                        label_pos = "b",
                        label_thresh = .7,
                        title_pos = "m",
                        wrap_width = 100,
                        flip = TRUE,
                        nudge_title = 10,
                        nudge_lab = NULL) {

  #
  # #
  #    data <- tibble("criteria" = c("Start",
  #                                  "Criteria A ....",
  #                                  "Criteria B ....",
  #                                  "Criteria C ....",
  #                                  "Criteria D ...."),
  #                   "size" = c(1000, 785, 456, 234, 154))
  #
  #
  #
  #
  #   x <- quo(criteria)
  #   y <- quo(N)
  #
  #
  # sort = NULL
  # bar_fill = NULL
  # bar_col = "black"
  # bar_alpha = .9
  # wrap_width = 100
  # flip = TRUE
  # title_pos = "m"
  # label_pos = "b"
  # type = "bar"
  # show_p = FALSE
  # N_max = NULL
  # nudge_title = 10
  # nudge_lab = NULL
  # label = quo(criteria)
  # n = quo(size)
  #

  # Deal with that crazy quosure stuff

  n <- dplyr::enquo(n)
  label <- dplyr::enquo(label)


  # Define max

  if(is.null(N_max)) {

    N_max <- data %>% pull(!!n) %>% max()

  }


  # nudge_lab

  if(is.null(nudge_lab)) {

    nudge_lab <- N_max / 100

  }



  # Add lab

  data <- data %>%
    mutate(N_max = N_max,
           p = round(!!n / N_max * 100))

  if(show_p) {

    data <- data %>%
      mutate(lab = paste0(scales::number(!!n, big.mark = ","), " (", round(p, 1), "%)"))

  } else {

    data <- data %>%
      mutate(lab = paste0(scales::number(!!n, big.mark = ",")))
  }




  if(is.null(sort)) {

    data <- data %>%
      arrange(-row_number()) %>%
      mutate(label = factor(!!label, ordered = TRUE, levels = !!label))

  }

  if(!is.null(sort) && substr(sort, start = 1, 1) == "a") {

    data <- data %>%
      arrange(desc(!!n)) %>%
      mutate(label = factor(!!x, ordered = TRUE, levels = !!x))


  }

  if(!is.null(sort) && substr(sort, start = 1, 1) == "d") {

    data <- data %>%
      arrange(!!n) %>%
      mutate(label = factor(!!x, ordered = TRUE, levels = !!x))

  }



  # set up plot

  p <- ggplot(data,
              aes(x = label, y = !!n, label = lab)) +
    labs(x = "",
         y = "Size") +
    guides(fill = FALSE) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor.y = element_blank()) +
    ylim(c(0, N_max))

  # Add bar

  if(!is.null(bar_fill)) {

    p <- p + geom_bar(stat = "identity", col = bar_col, fill = bar_fill, alpha = bar_alpha)

  } else {

    p <- p + geom_bar(stat = "identity", col = bar_col, alpha = bar_alpha, aes(fill = !!n))

  }




  if(title_pos == "m") {

    p <- p + scale_x_discrete(labels = function(x) {stringr::str_wrap(x, width = wrap_width)})

  }

  # Add label


  if(label_pos == "r") {

    # right labels

    p <- p + geom_label(data = data, mapping = aes(x = !!label, y = !!n),
                        nudge_y = nudge_lab,
                        hjust = 0)

  }


  if(label_pos == "l") {

    # left labels

    p <- p + geom_label(data = data, mapping = aes(x = !!label, y = 0),
                        nudge_y = nudge_lab,
                        hjust = 0)

  }


  if(label_pos == "m") {

    p <- p + geom_label(mapping = aes(x = !!label, y = !!n),
                        nudge_y = -nudge_lab,
                        hjust = .5)

  }

  if(label_pos == "b") {

    # left labels

    p <- p + geom_label(data = data %>% filter(!!n < label_thresh * N_max), mapping = aes(x = !!label, y = !!n),
                        nudge_y = nudge_lab,
                        hjust = 0)

    # right labels

    p <- p + geom_label(data = data %>% filter(!!n >= label_thresh * N_max), mapping = aes(x = !!label, y = !!n),
                        nudge_y = -nudge_lab,
                        hjust = 1)

  }


  if(flip) {

    p <- p + coord_flip()

  }

  p <- p + scale_fill_continuous(limits = c(0, N_max))

  p

}
