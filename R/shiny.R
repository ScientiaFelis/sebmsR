#' Running this function launches a Shiny web application presenting
#' the datasets bundled in this package, a couple of examples are included
#' @param example the name of the bundled Shiny application example
#' @export
runShinyApp <- function(example) {

  PKG <- "sebmsR"
  APPS <- "shiny-apps"

  validExamples <- list.files(system.file(APPS, package = PKG))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  appDir <- system.file(APPS, example, package = PKG)
  if (appDir == "") {
    msg <- paste0("Could not find example directory. Try re-installing `",
      PKG, "`.")
    stop(msg, call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
