# install.packages("ucimlrepo")   # Datensatz direkt laden
# install.packages("tidyverse")   # ggplot2, dplyr, etc.
# install.packages("corrplot")    # Korrelationsmatrix

library(ucimlrepo)
library(dplyr)

# Datensatz direkt vom UCI Repository laden (ID = 45)
heart_disease <- fetch_ucirepo(id = 45)

# Features (X) und Zielvariable (y) extrahieren
X <- heart_disease$data$features
y <- heart_disease$data$targets

# Zu einem einzigen DataFrame zusammenführen
df <- bind_cols(X, y)

# Erste Zeilen ansehen
head(df)
dim(df)  # Erwartete Ausgabe: 920 Zeilen, 14 Spalten


# Datentypen und fehlende Werte prüfen
str(df)
summary(df)

# Fehlende Werte pro Spalte zählen
colSums(is.na(df))

# Zielvariable binarisieren: 0 = kein Herzleiden, 1 = Herzleiden
df$target <- ifelse(df$num > 0, 1, 0)
df$target <- as.factor(df$target)

# Verteilung der Zielvariable
table(df$target)

library(ggplot2)

# 1. Verteilung Herzleiden (Ja/Nein)
ggplot(df, aes(x = target, fill = target)) +
  geom_bar() +
  labs(title = "Verteilung: Herzleiden", x = "Diagnose", y = "Anzahl Patienten") +
  scale_x_discrete(labels = c("0" = "Kein Herzleiden", "1" = "Herzleiden")) +
  theme_minimal()

# 2. Altersverteilung nach Diagnose
ggplot(df, aes(x = age, fill = target)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
  labs(title = "Altersverteilung nach Diagnose", x = "Alter", y = "Häufigkeit") +
  theme_minimal()

# 3. Maximale Herzfrequenz (thalach) nach Diagnose
ggplot(df, aes(x = target, y = thalach, fill = target)) +
  geom_boxplot() +
  labs(title = "Max. Herzfrequenz nach Diagnose",
       x = "Diagnose", y = "Max. Herzfrequenz (thalach)") +
  scale_x_discrete(labels = c("0" = "Kein Herzleiden", "1" = "Herzleiden")) +
  theme_minimal()

library(corrplot)

# Nur numerische Spalten auswählen
df_num <- df %>% select(where(is.numeric))

# Korrelation berechnen und plotten
corrplot(cor(df_num, use = "complete.obs"),
         method = "color",
         type   = "upper",
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.6,
         title = "Korrelationsmatrix – Heart Disease",
         mar = c(0,0,1,0))

# Als CSV im data/-Ordner speichern
write.csv(df, "data/heart_disease_clean.csv", row.names = FALSE)

# Paket laden 
library(tidyr)

# Die Fälle mit fehlenden Werten ausschliessen
df <- df %>%
  drop_na()


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
