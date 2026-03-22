# 1. Wahrscheinlichkeiten berechnen
wahrscheinlichkeiten <- predict(modell_optimiert, newdata = test_data, type = "response")

# 2. Schwellenwert 50% anwenden
vorhersagen_klassen <- ifelse(wahrscheinlichkeiten > 0.5, "1", "0")
vorhersagen_fakt <- factor(vorhersagen_klassen, levels = levels(test_data$target))

# 3. Konfusionsmatrix berechnen
konfusionsmatrix <- confusionMatrix(data = vorhersagen_fakt, reference = test_data$target, positive = "1")

# 4. Heatmap bauen
cm_daten <- as.data.frame(konfusionsmatrix$table)
colnames(cm_daten) <- c("Vorhersage", "Referenz", "Anzahl")

heatmap_cm <- ggplot(data = cm_daten, aes(x = Referenz, y = Vorhersage, fill = Anzahl)) +
  geom_tile(color = "white", linewidth = 1.5) +
  scale_fill_gradient(low = "#e0f3f8", high = "#313695", name = "Patienten") +
  geom_text(aes(label = Anzahl), 
            color = ifelse(cm_daten$Anzahl > 20, "white", "black"), 
            size = 8, fontface = "bold") +
  theme_minimal() +
  labs(title = "Heatmap der Klassifikationsleistung",
       subtitle = "Vorhersage vs. Tatsächliche Diagnose (Cut-off: 50%)",
       x = "Tatsächliche Diagnose (0 = Gesund, 1 = Krank)",
       y = "Modell-Vorhersage") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA))

# 5. Als PNG mit transparentem Hintergrund im Working Directory speichern
ggsave(
  filename = file.path(getwd(), "heatmap_cm.png"),
  plot = heatmap_cm,
  bg = "transparent",
  width = 6,
  height = 5,
  dpi = 300
)

message("Gespeichert unter: ", file.path(getwd(), "heatmap_cm.png"))

