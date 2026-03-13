# --- EDA: Chi-Quadrat-Tests ---

# 1. Brustschmerztyp (cp) vs. Zielvariable (target)
# Erstelle zuerst eine Kreuztabelle
tabelle_cp <- table(df$cp, df$target)
# Führe den Test aus
test_cp <- chisq.test(tabelle_cp)
print(test_cp)

# 2. Geschlecht (sex) vs. Zielvariable (target)
tabelle_sex <- table(df$sex, df$target)
test_sex <- chisq.test(tabelle_sex)
print(test_sex)

# --- EDA: Einseitige ANOVA ---

# 1. Maximale Herzfrequenz (thalach) nach Zielvariable (target)
anova_thalach <- aov(thalach ~ target, data = df)
summary(anova_thalach)

# 2. Alter (age) nach Zielvariable (target)
anova_age <- aov(age ~ target, data = df)
summary(anova_age)

# 3. Ruheblutdruck (trestbps) nach Zielvariable (target)
anova_trestbps <- aov(trestbps ~ target, data = df)
summary(anova_trestbps)


# Lade die benötigten Bibliotheken
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

# --- 1. VISUELLE EDA: Kontinuierliche Variablen (Boxplots) ---
# Wir wählen die wichtigsten numerischen Variablen und plotten sie in einem Grid
plot_kontinuierlich <- df %>%
  select(target, age, thalach, trestbps, chol) %>%
  pivot_longer(cols = -target, names_to = "Variable", values_to = "Wert") %>%
  ggplot(aes(x = target, y = Wert, fill = target)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
  facet_wrap(~ Variable, scales = "free_y") + # Erstellt für jede Variable ein eigenes Fenster
  scale_fill_manual(values = c("0" = "#1b9e77", "1" = "#d95f02"), 
                    labels = c("0" = "Gesund", "1" = "Krank")) +
  theme_minimal() +
  labs(title = "Verteilung kontinuierlicher Variablen nach KHK-Diagnose",
       x = "Diagnose (0 = Gesund, 1 = Krank)",
       y = "Gemessener Wert",
       fill = "Status") +
  theme(legend.position = "bottom")

print(plot_kontinuierlich)


# --- 2. VISUELLE EDA: Kategoriale Variablen (Gestapelte Balkendiagramme) ---

# Plot für Brustschmerztyp (cp)
plot_cp <- ggplot(df, aes(x = factor(cp), fill = target)) +
  geom_bar(position = "fill", alpha = 0.8) + # "fill" macht es zu 100%-gestapelten Balken
  scale_fill_manual(values = c("0" = "#1b9e77", "1" = "#d95f02"),
                    labels = c("0" = "Gesund", "1" = "Krank")) +
  theme_minimal() +
  labs(title = "KHK-Anteil nach Brustschmerztyp (cp)",
       x = "Brustschmerztyp (1 bis 4)",
       y = "Prozentualer Anteil",
       fill = "Status")

print(plot_cp)

# Plot für Geschlecht (sex)
plot_sex <- ggplot(df, aes(x = factor(sex), fill = target)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#1b9e77", "1" = "#d95f02"),
                    labels = c("0" = "Gesund", "1" = "Krank")) +
  scale_x_discrete(labels = c("0" = "Weiblich", "1" = "Männlich")) +
  theme_minimal() +
  labs(title = "KHK-Anteil nach Geschlecht",
       x = "Geschlecht",
       y = "Prozentualer Anteil",
       fill = "Status")

print(plot_sex)


# --- 3. VISUELLE EDA: Multikollinearität (Korrelationsmatrix) ---
# Für die Korrelation dürfen wir nur reine Zahlen verwenden, keine Faktoren!
df_numerisch <- df %>%
  # Wandelt alle Spalten in numerische Werte um (auch unsere binarisierte target-Variable)
  mutate(across(everything(), as.numeric)) 

# Korrelationsmatrix berechnen
korrelations_matrix <- cor(df_numerisch, use = "complete.obs")



# Matrix plotten
corrplot(korrelations_matrix, 
         method = "color",       # Farben statt Kreise
         type = "upper",         # Nur das obere Dreieck anzeigen (übersichtlicher)
         addCoef.col = "black",  # Zahlenwerte in die Kästchen schreiben
         number.cex = 0.6,       # Schriftgrösse der Zahlen
         tl.col = "black",       # Schriftfarbe der Variablen-Namen
         tl.srt = 45,            # Variablen-Namen um 45 Grad drehen
         diag = FALSE,           # Die Diagonale (immer 1) ausblenden
         title = "Korrelationsmatrix der klinischen Parameter",
         mar = c(0,0,2,0))       # Rand anpassen, damit der Titel Platz hat