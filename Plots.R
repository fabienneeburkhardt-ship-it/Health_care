# Lade benötigte Bibliothek
library(ggplot2)

# ==============================================================================
# 1. HEATMAP (Gatekeeper-Modell) MIT TRANSPARENTEM HINTERGRUND EXPORTIEREN
# ==============================================================================

# Daten vorbereiten
cm_daten_scr <- as.data.frame(matrix_screening$table)
colnames(cm_daten_scr) <- c("Vorhersage", "Referenz", "Anzahl")

# Plot bauen
heatmap_scr <- ggplot(data = cm_daten_scr, aes(x = Referenz, y = Vorhersage, fill = Anzahl)) +
  geom_tile(color = "white", linewidth = 1.5) +
  scale_fill_gradient(low = "#e0f3f8", high = "#313695", name = "Patienten") +
  geom_text(aes(label = Anzahl), 
            color = ifelse(cm_daten_scr$Anzahl > 10, "white", "black"), 
            size = 8, fontface = "bold") +
  theme_minimal() +
  labs(title = "Klassifikationsleistung: Gatekeeper-Modell",
       subtitle = "Vorhersage vs. Tatsächliche Diagnose (ESC Cut-off: 15%)",
       x = "Tatsächliche Diagnose (0 = Gesund, 1 = Krank)",
       y = "Modell-Vorhersage") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid = element_blank(),
        
        # MAGIE FÜR TRANSPARENZ:
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA))

# Speichern als High-Res PNG (300 dpi für gestochen scharfe PowerPoint-Bilder)
ggsave("Heatmap_Gatekeeper_Transparent.png", plot = heatmap_scr, 
       width = 7, height = 5, dpi = 300, bg = "transparent")


# ==============================================================================
# 2. HISTOGRAMM (Repeated Random Subsampling) MIT TRANSPARENTEM HINTERGRUND EXPORTIEREN
# ==============================================================================

# Daten vorbereiten
mc_daten <- data.frame(Sensitivitaet = sens_liste * 100)

# Plot bauen
plot_mc <- ggplot(mc_daten, aes(x = Sensitivitaet)) +
  geom_histogram(fill = "#313695", color = "white", bins = 15) +
  geom_vline(aes(xintercept = mean(Sensitivitaet)), color = "red", linetype = "dashed", linewidth = 1.5) +
  theme_minimal() +
  labs(title = "Stabilität der Modell-Sensitivität",
       subtitle = "Verteilung über 100 Repeated-Random-Subsampling-Durchläufe",
       x = "Sensitivität in %", y = "Häufigkeit") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),

        # MAGIE FÜR TRANSPARENZ:
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))

# Speichern als High-Res PNG
ggsave("Histogramm_RepeatedRandomSubsampling_Transparent.png", plot = plot_mc,
       width = 7, height = 5, dpi = 300, bg = "transparent")

print("Erfolg! Beide Plots wurden mit transparentem Hintergrund in deinem Projektordner gespeichert.")