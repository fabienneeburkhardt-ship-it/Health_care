# install.packages("randomForest")
library(randomForest)
library(caret)

# 1. Gleiche Cross-Validation Einstellungen wie bei der Regression
train_kontrolle <- trainControl(method = "cv", number = 10)

# 2. Random Forest trainieren
# Wir setzen einen "Seed", damit der Zufall reproduzierbar bleibt
set.seed(42) 

# Wir nutzen "target ~ .", um dem Wald ALLE 13 Variablen zur Verfügung zu stellen.
# Er sucht sich selbst die besten heraus!
rf_modell <- train(
  target ~ ., 
  data = df_modell, 
  method = "rf", 
  trControl = train_kontrolle,
  importance = TRUE # Wichtig, um später zu sehen, welche Variablen am nützlichsten waren
)

# 3. Ergebnisse anzeigen
print(rf_modell)

# 4. Die wichtigsten Variablen des Random Forests anzeigen lassen (Feature Importance)
plot(varImp(rf_modell), main = "Wichtigste Variablen im Random Forest")