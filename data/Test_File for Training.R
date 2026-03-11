# ── 1. Datensatz laden ──────────────────────────────────
data <- read.csv("data/heart_disease_clean.csv")

# ── 2. num Spalte entfernen da in Target binarisiert─────
data$num <- NULL   # ← num rausnehmen!

# ── 3. Train/Test Split ─────────────────────────────────
set.seed(42)
train_idx <- sample(1:nrow(data), size = 0.8 * nrow(data))
train <- data[train_idx, ]   # ~237 Patienten
test  <- data[-train_idx, ]  # ~60 Patienten

# ── 4. Prüfen ───────────────────────────────────────────
nrow(train)
nrow(test)

# ── 5. Modell trainieren ────────────────────────────────
modell <- glm(target ~ ., data = train, family = binomial)
summary(modell)

# ── 6. Vorhersagen auf Testdaten ────────────────────────
vorhersagen <- predict(modell, newdata = test, type = "response")
klassen <- ifelse(vorhersagen > 0.5, 1, 0)

# ── 7. Confusion Matrix ─────────────────────────────────
cm <- table(Vorhergesagt = klassen, Tatsächlich = test$target)
print(cm)

# ── 8. Metriken berechnen ───────────────────────────────
TP <- cm[2,2]
TN <- cm[1,1]
FP <- cm[2,1]
FN <- cm[1,2]

sensitivitaet <- TP / (TP + FN)
cat("Sensitivität:", round(sensitivitaet, 3), "\n")

spezifitaet <- TN / (TN + FP)
cat("Spezifität:  ", round(spezifitaet, 3), "\n")

genauigkeit <- (TP + TN) / (TP + TN + FP + FN)
cat("Genauigkeit: ", round(genauigkeit, 3), "\n")

# ── 9. AUC Wert ─────────────────────────────────────────
library(pROC)
roc_kurve <- roc(test$target, vorhersagen)
cat("AUC:         ", round(auc(roc_kurve), 3), "\n")