library(ucimlrepo)
library(dplyr)
library(caret)
library(pROC)

# =============================================================================
# 1. DATEN LADEN (identisch zum Report)
# =============================================================================
heart_disease <- fetch_ucirepo(id = 45)
df <- bind_cols(heart_disease$data$features, heart_disease$data$targets)
df[df == "?"] <- NA
df$ca   <- as.numeric(as.character(df$ca))
df$thal <- as.numeric(as.character(df$thal))
df_modell <- df %>%
  mutate(target = as.factor(ifelse(num > 0, "1", "0"))) %>%
  select(-num)
df_modell <- na.omit(df_modell)

set.seed(42)
train_index <- sample(seq_len(nrow(df_modell)), size = 0.7 * nrow(df_modell))

# =============================================================================
# 2. ZWEI VERSIONEN
# =============================================================================
# Version A: Status quo (alles numerisch, wie im Report)
df_num <- df_modell

# Version B: Kategoriale Variablen als Faktor
df_fak <- df_modell
df_fak$cp      <- as.factor(df_fak$cp)
df_fak$thal    <- as.factor(df_fak$thal)
df_fak$slope   <- as.factor(df_fak$slope)
df_fak$restecg <- as.factor(df_fak$restecg)

# Train/Test-Split für beide Versionen
train_num  <- df_num[train_index, ]
test_num   <- df_num[-train_index, ]
train_fak  <- df_fak[train_index, ]
test_fak   <- df_fak[-train_index, ]

# =============================================================================
# 3a. BASISMODELL (13 Prädiktoren)
# =============================================================================
cat("\n========== BASISMODELL: NUMERISCH ==========\n")
mod_num_voll <- glm(target ~ ., data = train_num, family = "binomial")
print(summary(mod_num_voll))
aic_num_voll <- mod_num_voll$aic

cat("\n========== BASISMODELL: FAKTORISIERT ==========\n")
mod_fak_voll <- glm(target ~ ., data = train_fak, family = "binomial")
print(summary(mod_fak_voll))
aic_fak_voll <- mod_fak_voll$aic

# =============================================================================
# 3b. OPTIMIERTES MODELL (Rückwärts-Selektion)
# =============================================================================
cat("\n========== RÜCKWÄRTS-SELEKTION: NUMERISCH ==========\n")
mod_num_opt <- step(mod_num_voll, direction = "backward", trace = 1)
cat("\nSummary optimiertes Modell (numerisch):\n")
print(summary(mod_num_opt))
aic_num_opt <- mod_num_opt$aic
vars_num    <- names(coef(mod_num_opt))[-1]  # ohne Intercept
n_vars_num  <- length(vars_num)

cat("\n========== RÜCKWÄRTS-SELEKTION: FAKTORISIERT ==========\n")
mod_fak_opt <- step(mod_fak_voll, direction = "backward", trace = 1)
cat("\nSummary optimiertes Modell (faktorisiert):\n")
print(summary(mod_fak_opt))
aic_fak_opt <- mod_fak_opt$aic
# Variablennamen ohne Dummy-Suffixe und ohne Intercept
vars_fak_raw <- names(coef(mod_fak_opt))[-1]
vars_fak     <- unique(gsub("(cp|thal|slope|restecg)[0-9]+", "\\1", vars_fak_raw))
n_vars_fak   <- length(unique(gsub("(cp|thal|slope|restecg)[0-9]+.*", "\\1", vars_fak_raw)))

# =============================================================================
# 3c. TESTSET-EVALUATION (Cut-off 50%)
# =============================================================================
# Numerisch
p_num <- predict(mod_num_opt, newdata = test_num, type = "response")
pred_num <- factor(ifelse(p_num > 0.5, "1", "0"), levels = c("0", "1"))
cm_num   <- confusionMatrix(pred_num, test_num$target, positive = "1")
roc_num  <- roc(test_num$target, p_num, levels = c("0","1"), direction = "<", quiet = TRUE)
auc_num  <- as.numeric(auc(roc_num))
acc_num  <- cm_num$overall["Accuracy"]
sens_num <- cm_num$byClass["Sensitivity"]
spez_num <- cm_num$byClass["Specificity"]

# Faktorisiert
p_fak <- predict(mod_fak_opt, newdata = test_fak, type = "response")
pred_fak <- factor(ifelse(p_fak > 0.5, "1", "0"), levels = c("0", "1"))
cm_fak   <- confusionMatrix(pred_fak, test_fak$target, positive = "1")
roc_fak  <- roc(test_fak$target, p_fak, levels = c("0","1"), direction = "<", quiet = TRUE)
auc_fak  <- as.numeric(auc(roc_fak))
acc_fak  <- cm_fak$overall["Accuracy"]
sens_fak <- cm_fak$byClass["Sensitivity"]
spez_fak <- cm_fak$byClass["Specificity"]

# =============================================================================
# 3d. SCREENING-MODELL (6 Variablen, Cut-off 15%)
# =============================================================================
# Numerisch
df_scr_num <- df_num %>% select(target, age, sex, cp, trestbps, chol, fbs)
train_scr_num <- df_scr_num[train_index, ]
test_scr_num  <- df_scr_num[-train_index, ]
mod_scr_num   <- glm(target ~ ., data = train_scr_num, family = "binomial")
p_scr_num     <- predict(mod_scr_num, newdata = test_scr_num, type = "response")
cm_scr_num    <- confusionMatrix(
  factor(ifelse(p_scr_num > 0.15, "1", "0"), levels = c("0","1")),
  test_scr_num$target, positive = "1")
sens_scr_num  <- cm_scr_num$byClass["Sensitivity"]
spez_scr_num  <- cm_scr_num$byClass["Specificity"]

# Faktorisiert (nur cp wird zum Faktor)
df_scr_fak <- df_fak %>% select(target, age, sex, cp, trestbps, chol, fbs)
train_scr_fak <- df_scr_fak[train_index, ]
test_scr_fak  <- df_scr_fak[-train_index, ]
mod_scr_fak   <- glm(target ~ ., data = train_scr_fak, family = "binomial")
p_scr_fak     <- predict(mod_scr_fak, newdata = test_scr_fak, type = "response")
cm_scr_fak    <- confusionMatrix(
  factor(ifelse(p_scr_fak > 0.15, "1", "0"), levels = c("0","1")),
  test_scr_fak$target, positive = "1")
sens_scr_fak  <- cm_scr_fak$byClass["Sensitivity"]
spez_scr_fak  <- cm_scr_fak$byClass["Specificity"]

# =============================================================================
# 5. p-WERT-VERGLEICH FÜR cp IM SCREENING-MODELL
# =============================================================================
cat("\n========== p-WERT-VERGLEICH: cp IM SCREENING-MODELL ==========\n")

coef_num_scr <- summary(mod_scr_num)$coefficients
cat("\nNumerisch – cp (1 Koeffizient, linearer Trend):\n")
cat(sprintf("  cp:  Schätzer = %+.4f,  p = %.4f\n",
            coef_num_scr["cp", "Estimate"],
            coef_num_scr["cp", "Pr(>|z|)"]))

coef_fak_scr <- summary(mod_scr_fak)$coefficients
cp_rows <- grep("^cp", rownames(coef_fak_scr), value = TRUE)
cat("\nFaktorisiert – cp (3 Dummy-Koeffizienten, Referenz = cp1):\n")
for (r in cp_rows) {
  cat(sprintf("  %-6s  Schätzer = %+.4f,  p = %.4f\n",
              r,
              coef_fak_scr[r, "Estimate"],
              coef_fak_scr[r, "Pr(>|z|)"]))
}

# Richtung und Signifikanz
cat("\nInterpretation:\n")
est_num <- coef_num_scr["cp", "Estimate"]
cat(sprintf("  Numerisch: negativer Koeffizient (%.4f) → höherer cp-Wert = niedrigeres KHK-Risiko\n", est_num))
cat(sprintf("  (Dies entspricht dem klinischen Befund: cp=4 'asymptomatisch' = höchstes KHK-Risiko,\n"))
cat(sprintf("   aber numerisch kodiert als grösster Wert – linearer Trend ist inhaltlich invers.)\n"))
cat(sprintf("  Faktorisiert: Referenzkategorie cp1 (typische Angina); Dummies cp2, cp3, cp4\n"))
cat(sprintf("  zeigen den jeweiligen Unterschied zu cp1 direkt und ohne Linearitätsannahme.\n"))

# =============================================================================
# 4. VERGLEICHSTABELLE
# =============================================================================
cat("\n========== VERGLEICH: NUMERISCH vs. FAKTORISIERT ==========\n\n")

cat("--- BASISMODELL (13 Variablen) ---\n")
cat(sprintf("AIC numerisch:    %.1f\n", aic_num_voll))
cat(sprintf("AIC faktorisiert: %.1f\n", aic_fak_voll))
cat(sprintf("Differenz:        %.1f\n\n", aic_fak_voll - aic_num_voll))

cat("--- OPTIMIERTES MODELL (nach step()) ---\n")
cat(sprintf("AIC numerisch:    %.1f  (%d Variablen)\n", aic_num_opt, n_vars_num))
cat(sprintf("AIC faktorisiert: %.1f  (%d Variablen)\n", aic_fak_opt, n_vars_fak))
cat("Variablen numerisch:    ", paste(vars_num, collapse = ", "), "\n")
cat("Variablen faktorisiert: ", paste(vars_fak, collapse = ", "), "\n\n")

cat("--- TESTSET-EVALUATION (Cut-off 50%) ---\n")
cat(sprintf("AUC numerisch:    %.3f\n", auc_num))
cat(sprintf("AUC faktorisiert: %.3f\n", auc_fak))
cat(sprintf("Accuracy num:     %.1f%%\n", acc_num * 100))
cat(sprintf("Accuracy fak:     %.1f%%\n", acc_fak * 100))
cat(sprintf("Sens num:         %.1f%%\n", sens_num * 100))
cat(sprintf("Sens fak:         %.1f%%\n\n", sens_fak * 100))

cat("--- SCREENING-MODELL (6 Variablen, Cut-off 15%) ---\n")
cat(sprintf("Sens numerisch:    %.1f%%\n", sens_scr_num * 100))
cat(sprintf("Sens faktorisiert: %.1f%%\n", sens_scr_fak * 100))
cat(sprintf("Spez numerisch:    %.1f%%\n", spez_scr_num * 100))
cat(sprintf("Spez faktorisiert: %.1f%%\n", spez_scr_fak * 100))
