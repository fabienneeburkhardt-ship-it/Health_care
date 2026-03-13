library(shiny)
# Nutzt 'modell_screening' aus dem Environment
ui <- fluidPage(
  titlePanel("KHK-Screening-Tool"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Alter:", value = 50, min = 20, max = 100),
      selectInput("sex", "Geschlecht:", choices = list("Weiblich" = 0, "Männlich" = 1)),
      selectInput("cp", "Brustschmerztyp:", 
                  choices = list("1: Typische Angina" = 1, 
                                 "2: Atypische Angina" = 2, 
                                 "3: Nicht-anginöser Schmerz" = 3, 
                                 "4: Asymptomatisch" = 4), 
                  selected = 1),
      sliderInput("trestbps", "Ruheblutdruck:", min = 90, max = 200, value = 120),
      sliderInput("chol", "Cholesterin:", min = 100, max = 400, value = 200),
      selectInput("fbs", "Blutzucker > 120 mg/dl:", choices = list("Nein" = 0, "Ja" = 1))
    ),
    mainPanel(
      h3("Ergebnis nach ESC-Guidelines"),
      h4("Pre-Test Probability (PTP):"),
      verbatimTextOutput("prob_out"),
      h4("Triage-Empfehlung:"),
      verbatimTextOutput("triage_out")
    )
  )
)

server <- function(input, output) {
  vorhersage <- reactive({
    neuer_patient <- data.frame(
      age = input$age, 
      sex = as.numeric(input$sex), 
      cp = as.numeric(input$cp),
      trestbps = input$trestbps, 
      chol = input$chol, 
      fbs = as.numeric(input$fbs)
    )
    predict(modell_screening, newdata = neuer_patient, type = "response")
  })
  
  output$prob_out <- renderText({ paste0(round(vorhersage() * 100, 1), " %") })
  
  output$triage_out <- renderText({
    p <- vorhersage()
    if(p < 0.15) return("NIEDRIGES RISIKO: Keine weiteren Tests nötig.")
    if(p <= 0.85) return("MITTLERES RISIKO: Weitere Abklärungen (z.B. Echo/MRI).")
    return("HOHES RISIKO: Direkte Überweisung zum Kardiologen.")
  })
}

shinyApp(ui, server)