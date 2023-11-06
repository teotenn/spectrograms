box::use(
  app/view/spectra
)

box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  bootstrapPage(
    spectra$ui(ns("spectra"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    spectra$server("spectra")
  })
}
