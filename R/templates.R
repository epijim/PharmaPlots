#' Download an RWDS template to your working directory
#'
#' First set your working directory to the root of the project (e.g. rwds_xxx), then run the function.
#' It will download the latest version from github.roche.com of the template you tell it to get.
#' Files will be placed into the correct folder based on how BEE files are laid out. For some templates, a
#' script will also run to link up Jira and Github. Obviously these links will need to be fixed
#' if this is not a RWDS Jira project.
#' @section Bugs:
#' If you find an issue, post it here and we'll try and fix it: \url{http://go.roche.com/RocheRWDSissue}
#' @param template Pick the template. Templates are here: \url{https://pages.github.roche.com/RWDSsme/R/#r-in-rwds-templates}
#' @param overwrite Function will fail if any Rmd or R files are in the programs directory. Set to TRUE to ignore (and overwrite).
#' @keywords templates markdown
#' @export
#' @examples
#' \dontrun{RWDStemplates("script_single")}

RWDStemplates <- function(
  template = NULL,
  overwrite = FALSE
){
  # Message this is deprecated
  message("This function is deprecated - please check out the RocheTemplates package")
  message("Please visit https://pages.github.roche.com/RWDSsme/rwdssme/rwdsverse/")

  # Check if RCurl installed
  rcurlinstalled <- tryCatch(utils::packageVersion("RCurl"), error = function(e) NA)
  if (is.na(rcurlinstalled)) {
    # in interactive ask if can install
    if (interactive()) {
      message("To access github.roche.com I need 'RCurl'. Would you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages("RCurl")
      } else {
        stop("Please run 'install.packages(RCurl)' so I can access github.roche.com.")
      }
    } else {
      # if not inte
      stop("Please run 'install.packages(RCurl)' so I can access github.roche.com.")
    }
  }




  # check if code already there ------
  prevcode <- list.files(
    path = "programs",
    pattern = "\\.rmd$|\\.r$",
    ignore.case = TRUE)

  if (length(prevcode) != 0 & !overwrite) {
    stop("You seem to have code in this project. \nUse the `overwrite = TRUE` option to overwrite existing files.")
  }

  # Check if template exists -------
  currenttemplates <- c(
    "script_single","dashboard_static","script_multiple"
  )

  if (is.null(template)) stop("Please pick one template.")

  if (!template %in% currenttemplates) {
    stop(paste(
      "Template unknown. Options are:",paste(currenttemplates, collapse = ", ")
    ))
  }

  if (length(template) != 1) stop("Please pick one template.")


  # stop if no programs folder present in dir below ------
  if (!dir.exists("programs")) {
    stop(paste0(
      "I can't detect a subfolder called 'programs'",
      "\nIt appears you are here: ",getwd(),
      "\nPlease move to the project root (e.g. rwds_xxx)."
    ))
  }

  # if single script -----

  if (template == "script_single") {
    message("Sourcing template for script_single")

    # repo link
    url4template <- "https://raw.github.roche.com/RWDSsme/R/master/templates/script_single/"

    # text files
    textfiles <- c(
      "programs/roche.css","programs/index.Rmd","programs/build_navigation.R"
    )

    # bindary files
    binaryfiles <- c(
      "programs/logo.png"
    )

  }

  # if multiple script -----

  if (template == "script_multiple") {
    # uses import folder
    if (!dir.exists("import")) {
      stop(paste0(
        "I can't detect a subfolder called 'import'",
        "\nIt appears you are here: ",getwd(),
        "\nPlease make an import folder."
      ))
    }

    # repo link
    url4template <- "https://raw.github.roche.com/RWDSsme/R/master/templates/script_multiple/"

    # text files
    textfiles <- c(
      "programs/roche.css","programs/index.Rmd","programs/01_buildnotes.Rmd",
      "programs/02_pull.Rmd","programs/_site.yml","programs/footer.html",
      "import/environment.R","import/sas.R",'programs/build_navigation.R'
    )

    # bindary files
    binaryfiles <- c(
      "programs/logo.png"
    )
  }

  # if dashboard static -----

  if (template == "dashboard_static") {
    # repo link
    url4template <- "https://raw.github.roche.com/RWDSsme/R/master/templates/dashboard_static/"

    # text files
    textfiles <- c(
      "programs/index.Rmd"
    )

    # bindary files
    binaryfiles <- c(
      "import/roche_log_transperant.png"
    )
  }

  # download files --------
  for (i in textfiles) {
    helper.writeDfile(
      file = paste0(url4template,i),
      output = i
    )

    if (file.exists(i)) {
      message("Downloaded: ",i)
    } else {
      warning("Download of ",i," failed.")
    }
  } # text close

  for (i in binaryfiles) {

    if(!RCurl::url.exists(paste0(url4template,i))) {
      stop(paste0(url4template,i)," not found.")
    }

    download.file(
      paste0(url4template,i),
      i,
      mode = 'wb', # binary image
      quiet = TRUE
    )

    if (file.exists(i)) {
      message("Downloaded: ",i)
    } else {
      warning("Download of ",i," failed.")
    }
  } # binary close

  # build navigation ----------
  if (template %in% c("script_multiple","script_single")) {
    message("I have built a navigation bar for you. See programs/_navbar.html")
    source("programs/build_navigation.R")
    build_nav_bar()
  }
}

#' Helper to get the url
#'
#' Used within RWDStemplates()
#' @section Bugs:
#' If you find an issue, post it here and we'll try and fix it: \url{http://go.roche.com/RocheRWDSissue}
#' @param file files url to downloard
#' @param output where to put it relative to wd
#' @keywords internal

helper.writeDfile <- function(
  file = NA,
  output = NA
){
  # error check
  if (is.na(file) | is.na(output)) stop("Internal error on helper function - please make an issue on github")

  # does the url work?
  if(!RCurl::url.exists(file)) stop(file," not found.")

  # get the script from github
  code <- RCurl::getURL(
    file
  )
  # write to file
  writeLines(
    code,
    output)
}
