library(dplyr)
library(caret)
library(pROC)

# Anzahl der Simulationen
n_sim <- 100

# Leere Listen zum Speichern der Ergebnisse aus den 100 Durchläufen
sens_liste <- numeric(n_sim)
spez_liste <- numeric(n_sim)
auc_liste <- numeric(n_sim)
verpasste_kranke_liste <- numeric(n_sim) 

# Daten für das Screening-Modell vorbereiten (df_modell muss existieren)
df_screening <- df_modell %>% select(target, age, sex, cp, trestbps, chol, fbs)

# Schleife: Wir machen das Ganze 100-mal!
for(i in 1:n_sim) {
  
  # 1. Zufälliger Datensplit (neuer Seed in jedem Durchlauf)
  set.seed(i) # Der Seed ändert sich von 1 bis 100
  train_index <- sample(seq_len(nrow(df_screening)), size = 0.7 * nrow(df_screening))
  train_data <- df_screening[train_index, ]
  test_data  <- df_screening[-train_index, ]
  
  # 2. Modell trainieren (mit 6 Variablen)
  modell <- glm(target ~ ., data = train_data, family = "binomial")
  
  # 3. Vorhersagen generieren
  probs <- predict(modell, newdata = test_data, type = "response")
  
  # 4. AUC berechnen (Gesamtgüte des Modells)
  auc_liste[i] <- as.numeric(auc(roc(test_data$target, probs, levels = c("0", "1"), direction="<", quiet=TRUE)))
  
  # 5. Sensitivität & Spezifität beim 15% ESC-Cut-off bewerten
  vorhersagen_15 <- factor(ifelse(probs > 0.15, "1", "0"), levels = c("0", "1"))
  cm <- confusionMatrix(data = vorhersagen_15, reference = test_data$target, positive = "1")
  
  sens_liste[i] <- cm$byClass["Sensitivity"]
  spez_liste[i] <- cm$byClass["Specificity"]
  
  # 6. Wie viele tatsächlich Kranke wurden fälschlicherweise nach Hause geschickt? (<15%)
  verpasste_kranke_liste[i] <- sum(probs < 0.15 & test_data$target == "1")
}

# --- Ergebnisse nach 100 Simulationen berechnen und anzeigen ---
ergebnisse <- data.frame(
  Metrik = c("Durchschnittliche Sensitivität (Kranke erkannt)",
             "Durchschnittliche Spezifität (Gesunde erkannt)",
             "Durchschnittlicher AUC-Wert (Modellgüte)",
             "Ø Übersehene Kranke pro Testset (ca. 40 Kranke pro Set)"),
  Wert = c(round(mean(sens_liste), 3),
           round(mean(spez_liste), 3),
           round(mean(auc_liste), 3),
           round(mean(verpasste_kranke_liste), 2))
)

print("--- ERGEBNISSE NACH 100 SIMULATIONEN (ESC Rule-out bei 15%) ---")
print(ergebnisse)