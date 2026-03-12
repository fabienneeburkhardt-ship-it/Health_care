# Lade das Paket (falls du RStudio zwischendurch neu gestartet hast)
library(caret)

# --- 1. Einstellungen für die Cross-Validation festlegen ---
# "cv" steht für Cross-Validation, "number = 10" bedeutet 10 Folds (Blöcke)
train_kontrolle <- trainControl(method = "cv", number = 10)

# --- 2. Das Modell mit Cross-Validation trainieren ---
# Wir geben hier exakt die 9 Variablen an, die bei deiner Optimierung übrig geblieben sind.
# Als Datensatz nutzen wir 'df_modell' (der gesamte Datensatz ohne die Spalte 'num').
cv_modell <- train(
  target ~ sex + cp + trestbps + fbs + thalach + exang + slope + ca + thal, 
  data = df_modell, 
  method = "glm", 
  family = "binomial", 
  trControl = train_kontrolle
)

# --- 3. Ergebnisse der Kreuzvalidierung anzeigen ---
print(cv_modell)k