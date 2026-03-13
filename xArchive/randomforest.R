# install.packages("randomForest")
library(randomForest)
library(caret)

# 1. Clean the dataset by removing rows with missing values
# This prevents the na.fail.default error!
df_modell_clean <- na.omit(df_modell)

# 2. Gleiche Cross-Validation Einstellungen wie bei der Regression
train_kontrolle <- trainControl(method = "cv", number = 10)

# 3. Random Forest trainieren
set.seed(42) 

# Notice we are using df_modell_clean here now
rf_modell <- train(
  target ~ ., 
  data = df_modell_clean, 
  method = "rf", 
  trControl = train_kontrolle,
  importance = TRUE 
)

# 4. Ergebnisse anzeigen
print(rf_modell)

# 5. Die wichtigsten Variablen des Random Forests anzeigen lassen (Feature Importance)
plot(varImp(rf_modell), main = "Wichtigste Variablen im Random Forest")