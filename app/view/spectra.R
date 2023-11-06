box::use(
  shiny[NS, h2, h4, tagList, fluidRow, column, moduleServer, verbatimTextOutput,
        p, hr, checkboxGroupInput, actionButton, plotOutput, observeEvent, renderPrint],
  rhandsontable[rHandsontableOutput, renderRHandsontable, rhandsontable]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("SPECTRA"),
    column(
      4,
      fluidRow(h4("Data")),
      fluidRow(
        rHandsontableOutput(NS(id, "counts"))
      )
    ),
    column(
      8,
      fluidRow(
        column(3,
               p("Selected elements"),
               hr(),
               verbatimTextOutput(NS(id, "selected_elements"))
               ),
        column(9,
               checkboxGroupInput(NS(id, "choose_element"), "Select elements",
                           choices = list("Pb", "Fe", "Zn", "Hg", "Au"))
               )
      ),
      fluidRow(
        actionButton(NS(id, "plotBut"), "Plot"),
        plotOutput(NS(id, "plotGraph"))
      )
    )
  )
}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## initialize empty table
    df <- data.frame(Energy = double(50), Counts = double(50))
    output$counts <- renderRHandsontable(rhandsontable(df, readOnly = F))

    ## cast elements
    output$selected_elements <- renderPrint({input$choose_element})
  })
}
