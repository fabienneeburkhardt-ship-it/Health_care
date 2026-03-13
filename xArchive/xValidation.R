library(caret)

# --- FIX: Daten absolut rigoros bereinigen ---
# 1. Eventuelle Fragezeichen (aus dem UCI-Datensatz) in echte NAs umwandeln
df_modell[df_modell == "?"] <- NA

# 2. Alle Zeilen, die irgendwo ein NA haben, restlos löschen
df_modell <- na.omit(df_modell)

# 3. Variablen, die eventuell noch als "Character" feststecken, in Zahlen umwandeln
# (Besonders 'ca' und 'thal' machen oft Probleme)
df_modell$ca <- as.numeric(as.character(df_modell$ca))
df_modell$thal <- as.numeric(as.character(df_modell$thal))

# Noch einmal sicherheitshalber na.omit anwenden, falls beim Umwandeln neue NAs entstanden sind
df_modell <- na.omit(df_modell)

# Kurzer Check: Das MUSS "0" ausgeben!
print(paste("Anzahl fehlender Werte jetzt:", sum(is.na(df_modell))))


# --- 1. Einstellungen für die Cross-Validation festlegen ---
train_kontrolle <- trainControl(method = "cv", number = 10)

# --- 2. Das Modell mit Cross-Validation trainieren ---
cv_modell <- train(
  target ~ sex + cp + trestbps + fbs + thalach + exang + slope + ca + thal, 
  data = df_modell, 
  method = "glm", 
  family = "binomial", 
  trControl = train_kontrolle
)

# --- 3. Ergebnisse der Kreuzvalidierung anzeigen ---
print(cv_modell)