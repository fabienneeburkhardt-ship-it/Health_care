
# 1. Ursprüngliche Zielvariable 'num' entfernen, um "Data Leakage" zu verhindern
df_modell <- df %>% select(-num)

# 2. Datensplit (70% Training, 30% Test)
set.seed(42) # Setzt einen Startwert, damit das zufällige Aufteilen reproduzierbar bleibt

# Wir ziehen zufällig 70% der Zeilennummern
train_index <- sample(seq_len(nrow(df_modell)), size = 0.7 * nrow(df_modell))

# Datensätze aufteilen
train_data <- df_modell[train_index, ]
test_data  <- df_modell[-train_index, ]

# Kurze Kontrolle der Aufteilung
nrow(train_data)
nrow(test_data)

# Logistische Regression mit allen Variablen
modell_voll <- glm(target ~ ., data = train_data, family = "binomial")

# Zusammenfassung anzeigen (hier siehst du die p-Werte der einzelnen Parameter)
summary(modell_voll)

# Automatische Rückwärts-Selektion durchführen
modell_optimiert <- step(modell_voll, direction = "backward")

# Das finale, bereinigte Modell ansehen
summary(modell_optimiert)


# install.packages("caret")
# install.packages("pROC")

library(caret)
library(pROC)

# --- 1. Vorhersagen generieren ---
wahrscheinlichkeiten <- predict(modell_optimiert, newdata = test_data, type = "response")

# --- 2. Schwellenwert anpassen (auf "1" und "0" statt Text) ---
vorhersagen_klassen <- ifelse(wahrscheinlichkeiten > 0.5, "1", "0")

# Vorhersagen in den exakt gleichen Faktor umwandeln wie die echten Daten
vorhersagen_fakt <- factor(vorhersagen_klassen, levels = levels(test_data$target))

# --- 3. Konfusionsmatrix berechnen ---
# Hier sagen wir R nun, dass die "1" (und nicht das Wort "Herzleiden") unser positiver Fall ist
konfusionsmatrix <- confusionMatrix(data = vorhersagen_fakt, 
                                    reference = test_data$target, 
                                    positive = "1")

# Ausgabe der Ergebnisse
print(konfusionsmatrix)


# --- 4. ROC-Kurve und AUC berechnen ---
roc_kurve <- roc(test_data$target, wahrscheinlichkeiten, levels = c("0", "1"))

# Kurve plotten
plot(roc_kurve, main = "ROC-Kurve für das KHK-Vorhersagemodell", col = "blue", lwd = 2)

# AUC-Wert ausgeben
auc_wert <- auc(roc_kurve)
print(paste("Der AUC-Wert beträgt:", round(auc_wert, 3)))