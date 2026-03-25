library(shiny)

# WICHTIG: Hier laden wir das gespeicherte Modell in die App!
# Die Datei "modell_screening.rds" MUSS im selben Ordner wie diese app.R liegen.
modell_screening <- readRDS("modell_screening.rds")


ui <- fluidPage(
  titlePanel("KHK-Screening-Tool"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Alter (Jahre):", min = 18, max = 99, value = 50, step = 1, width = "100%"),
      selectInput("sex", "Geschlecht:", choices = list("Weiblich" = 0, "Männlich" = 1)),
      selectInput("cp", "Brustschmerztyp:",
                  choices = list("1: Typische Angina" = 1,
                                 "2: Atypische Angina" = 2,
                                 "3: Nicht-anginöser Schmerz" = 3,
                                 "4: Asymptomatisch" = 4),
                  selected = 1),
      sliderInput("trestbps", "Ruheblutdruck (mm Hg):", min = 80, max = 220, value = 120, step = 1),
      sliderInput("chol", "Serumcholesterin (mg/dl):", min = 100, max = 600, value = 200, step = 1),
      selectInput("fbs", "Nüchternblutzucker > 120 mg/dl:", choices = list("Nein" = 0, "Ja" = 1))
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
    
    # Hier nutzt er nun das geladene 'modell_screening'
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