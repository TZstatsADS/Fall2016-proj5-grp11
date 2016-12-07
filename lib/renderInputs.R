#######renderInputs

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             textInput(prefix, label = h3("Text input"), value = "Enter Player Name...")
      )
    ),
    p(actionButton(prefix,
                   "Re-run simulation", icon("random")
    ))
  )
}
