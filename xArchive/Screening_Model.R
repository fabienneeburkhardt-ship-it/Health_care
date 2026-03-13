library(dplyr)
library(caret)

# --- 1. Daten für das Screening-Modell vorbereiten ---
# Wir behalten NUR die einfachen, schnell erhebbaren Basis-Parameter
df_screening <- df %>%
  select(target, age, sex, cp, trestbps, chol, fbs)

# Wir teilen die Daten erneut auf (70% Training, 30% Test), um fair zu evaluieren
set.seed(42)
train_index_scr <- sample(seq_len(nrow(df_screening)), size = 0.7 * nrow(df_screening))
train_data_scr <- df_screening[train_index_scr, ]
test_data_scr  <- df_screening[-train_index_scr, ]

# --- 2. Neues Modell trainieren ---
# Wir trainieren die logistische Regression nur mit diesen 6 Hausarzt-Variablen
modell_screening <- glm(target ~ ., data = train_data_scr, family = "binomial")

# --- 3. Vorhersagen generieren ---
wahrscheinlichkeiten_scr <- predict(modell_screening, newdata = test_data_scr, type = "response")

# --- 4. Schwellenwert für MAXIMALE Sensitivität setzen ---
# Da wir niemanden übersehen wollen, setzen wir den Cut-off extrem tief an.
# "Schon wenn das Modell eine 10%ige Chance sieht, schlagen wir Alarm!"
cutoff_screening <- 0.10

vorhersagen_klassen_scr <- ifelse(wahrscheinlichkeiten_scr > cutoff_screening, "1", "0")
vorhersagen_fakt_scr <- factor(vorhersagen_klassen_scr, levels = levels(test_data_scr$target))

# --- 5. Konfusionsmatrix & Metriken berechnen ---
matrix_screening <- confusionMatrix(data = vorhersagen_fakt_scr, 
                                    reference = test_data_scr$target, 
                                    positive = "1")

# Ausgabe der wichtigsten Metriken
print(matrix_screening$table) # Zeigt dir die genauen Patientenzahlen
print(matrix_screening$byClass[c("Sensitivity", "Specificity")])

# --- ESC-Konforme Risikostratifizierung ---

# 1. Wir nutzen die Wahrscheinlichkeiten aus unserem Hausarzt-Modell
# (wahrscheinlichkeiten_scr aus dem vorherigen Schritt)

# 2. ESC-Triage-Gruppen erstellen
esc_triage <- cut(wahrscheinlichkeiten_scr, 
                  breaks = c(-Inf, 0.15, 0.85, Inf), 
                  labels = c("1. Niedriges Risiko (<15%): Keine Tests", 
                             "2. Mittleres Risiko (15-85%): Nicht-invasive Bildgebung", 
                             "3. Hohes Risiko (>85%): Herzkatheter (Invasiv)"))

# 3. Ergebnisse mit der Realität abgleichen
triage_ergebnisse <- table(Vorhersage_ESC_Klasse = esc_triage, 
                           Tatsaechliche_Diagnose = test_data_scr$target)

# Ausgabe der Verteilung
print("ESC-Triage-System angewendet auf den Testdatensatz:")
print(triage_ergebnisse)