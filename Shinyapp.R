# Falls noch nicht installiert, vorher einmal in der Konsole ausführen:
# install.packages("shiny")
library(shiny)

# --- 1. User Interface (UI) ---
ui <- fluidPage(
  
  # Titel der App
  titlePanel("KHK-Risiko-Prädiktor (Optimiertes Modell)"),
  
  # Seitenleisten-Layout
  sidebarLayout(
    
    # Eingabebereich für die 9 relevanten Parameter
    sidebarPanel(
      h4("Klinische Parameter"),
      
      selectInput("sex", "Geschlecht:", 
                  choices = list("Weiblich" = 0, "Männlich" = 1)),
      
      sliderInput("cp", "Brustschmerztyp (1-4):", 
                  min = 1, max = 4, value = 1, step = 1),
      
      sliderInput("trestbps", "Ruheblutdruck (mm Hg):", 
                  min = 90, max = 200, value = 130),
      
      selectInput("fbs", "Nüchternblutzucker > 120 mg/dl:", 
                  choices = list("Nein" = 0, "Ja" = 1)),
      
      sliderInput("thalach", "Max. Herzfrequenz:", 
                  min = 70, max = 220, value = 150),
      
      selectInput("exang", "Belastungsangina:", 
                  choices = list("Nein" = 0, "Ja" = 1)),
      
      sliderInput("slope", "ST-Strecken-Steigung (1-3):", 
                  min = 1, max = 3, value = 2, step = 1),
      
      sliderInput("ca", "Anzahl gefärbter Gefässe (0-3):", 
                  min = 0, max = 3, value = 0, step = 1),
      
      # Anmerkung: Die typischen Werte im Cleveland-Datensatz für thal sind 3, 6 oder 7
      selectInput("thal", "Thallium-Szintigraphie:", 
                  choices = list("Normal (3)" = 3, "Fixierter Defekt (6)" = 6, "Reversibler Defekt (7)" = 7))
    ),
    
    # Hauptbereich für die Ergebnisse
    mainPanel(
      h3("Diagnostische Auswertung"),
      br(), # Zeilenumbruch
      h4("Berechnete KHK-Wahrscheinlichkeit:"),
      verbatimTextOutput("wahrscheinlichkeit_out"),
      br(),
      h4("Klassifikation des Modells (Cut-off: 50%):"),
      verbatimTextOutput("klassifikation_out")
    )
  )
)

# --- 2. Server-Logik ---
server <- function(input, output) {
  
  # Reaktive Berechnung der Vorhersage bei jeder Regler-Bewegung
  vorhersage <- reactive({
    
    # Neuen Patienten-Datensatz aus den UI-Eingaben zusammenbauen
    # WICHTIG: as.numeric() stellt sicher, dass das Modell Zahlen und keinen Text bekommt
    neuer_patient <- data.frame(
      sex      = as.numeric(input$sex),
      cp       = as.numeric(input$cp),
      trestbps = as.numeric(input$trestbps),
      fbs      = as.numeric(input$fbs),
      thalach  = as.numeric(input$thalach),
      exang    = as.numeric(input$exang),
      slope    = as.numeric(input$slope),
      ca       = as.numeric(input$ca),
      thal     = as.numeric(input$thal)
    )
    
    # Modell anwenden
    prob <- predict(modell_optimiert, newdata = neuer_patient, type = "response")
    return(prob)
  })
  
  # Ausgabe der Prozentzahl
  output$wahrscheinlichkeit_out <- renderText({
    prozent <- round(vorhersage() * 100, 2)
    paste0(prozent, " %")
  })
  
  # Ausgabe der Klassifikation (Krank / Gesund)
  output$klassifikation_out <- renderText({
    if (vorhersage() > 0.5) {
      "ACHTUNG: Herzleiden wahrscheinlich (KHK)"
    } else {
      "UNAUFFÄLLIG: Kein Herzleiden prognostiziert"
    }
  })
}

# --- 3. App starten ---
shinyApp(ui = ui, server = server)