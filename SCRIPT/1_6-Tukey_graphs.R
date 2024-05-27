# Carica le librerie necessarie
library(ggplot2)
library(multcomp)
library(multcompView)
library(dplyr)

############################### Length #########################################

# Estrai i valori p dai risultati del test di Tukey
p_values <- Tukey_length$treatment[, "p adj"]

# Determina i livelli di significatività
significance_levels <- ifelse(p_values < 0.001, "***", 
                              ifelse(p_values < 0.01, "**", 
                                     ifelse(p_values < 0.05, "*", "")))

# Crea una mappa tra le comparazioni e i simboli di significatività
comparison_names <- rownames(Tukey_length$treatment)

significance_map <- data.frame(comparison = comparison_names, significance = significance_levels)

# Funzione per generare le etichette dei trattamenti
generate_treatment_labels <- function(TUKEY, variable) {
  # Estrai i livelli e le etichette dal test post-hoc di Tukey
  Tukey_levels <- TUKEY[[variable]][, 4]
  Tukey_labels <- data.frame(multcompLetters(Tukey_levels)['Letters'])
  
  # Ordina le etichette per allinearle al boxplot
  Tukey_labels$treatment <- rownames(Tukey_labels)
  Tukey_labels <- Tukey_labels[order(Tukey_labels$treatment), ]
  
  return(Tukey_labels)
}

# Applica la funzione al dataset
LABELS <- generate_treatment_labels(Tukey_length, "treatment")

# Crea il dataframe principale
dataframe_length <- result_df[, 1:3]

# Aggiungi le etichette al dataframe
dataframe_length$labels <- LABELS[dataframe_length$treatment, 1]
dataframe_length$labels <- factor(dataframe_length$labels, levels = unique(dataframe_length$labels))

# Crea il boxplot con ggplot2
boxplot_gg <- ggplot(dataframe_length, aes(x = treatment, y = length, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha = 0.5) +
  ylim(c(0, 1.1 * max(dataframe_length$length))) +
  labs(x = "Treatment", y = "Length (cm)", title = "Boxplot") +
  theme_minimal()

# Rimuovi le righe duplicate basate sul trattamento
max_values <- dataframe_length %>%
  group_by(treatment) %>%
  summarize(max_length = max(length[length <= quantile(length, 0.75) + 1.5 * IQR(length)]), labels = first(labels))

# Aggiungi le etichette al grafico
boxplot_gg <- boxplot_gg +
  geom_text(data = max_values, aes(label = labels, y = max_length + 200), 
            position = position_dodge(width = 0.9), vjust = 0, size = 6, colour = "black")

# Stampa il boxplot modificato con le etichette
print(boxplot_gg)

# Specifica il percorso per salvare l'immagine
file_path <- "./GRAPHS/boxPlots_tukey_length.jpg"

# Salva il boxplot come file PNG
ggsave(file_path, boxplot_gg, width = 16, height = 12, dpi = 700)

############################### avgDiam #########################################

# Estrai i valori p dai risultati del test di Tukey
p_values <- Tukey_avgDiam$treatment[, "p adj"]

# Determina i livelli di significatività
significance_levels <- ifelse(p_values < 0.001, "***", 
                              ifelse(p_values < 0.01, "**", 
                                     ifelse(p_values < 0.05, "*", "")))

# Crea una mappa tra le comparazioni e i simboli di significatività
comparison_names <- rownames(Tukey_avgDiam$treatment)

significance_map <- data.frame(comparison = comparison_names, significance = significance_levels)

# Funzione per generare le etichette dei trattamenti
generate_treatment_labels <- function(TUKEY, variable) {
  # Estrai i livelli e le etichette dal test post-hoc di Tukey
  Tukey_levels <- TUKEY[[variable]][, 4]
  Tukey_labels <- data.frame(multcompLetters(Tukey_levels)['Letters'])
  
  # Ordina le etichette per allinearle al boxplot
  Tukey_labels$treatment <- rownames(Tukey_labels)
  Tukey_labels <- Tukey_labels[order(Tukey_labels$treatment), ]
  
  return(Tukey_labels)
}

# Applica la funzione al dataset
LABELS <- generate_treatment_labels(Tukey_avgDiam, "treatment")

# Crea il dataframe principale
dataframe_avgDiam <- result_df[, c(1:2,4)]

# Aggiungi le etichette al dataframe
dataframe_avgDiam$labels <- LABELS[dataframe_avgDiam$treatment, 1]
dataframe_avgDiam$labels <- factor(dataframe_avgDiam$labels, levels = unique(dataframe_avgDiam$labels))

# Crea il boxplot con colori basati sul trattamento
boxplot_gg <- ggplot(dataframe_avgDiam, aes(x = treatment, y = avgDiam, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha = 0.5) +
  ylim(c(0, 1.1 * max(dataframe_avgDiam$avgDiam))) +
  labs(x = "Treatment", y = "Mean Diameter (cm)", title = "Boxplot") +
  theme_minimal() 

# Rimuovi le righe duplicate basate sul trattamento
max_values <- dataframe_avgDiam %>%
  group_by(treatment) %>%
  summarize(max_avgDiam = max(avgDiam[avgDiam <= quantile(avgDiam, 0.75) + 1.5 * IQR(avgDiam)]), labels = first(labels))

# Aggiungi le etichette al grafico
boxplot_gg <- boxplot_gg +
  geom_text(data = max_values, aes(label = labels, y = max_avgDiam + 0.02), 
            position = position_dodge(width = 0.9), vjust = 0, size = 6, colour = "black")

# Stampa il boxplot modificato con le etichette
print(boxplot_gg)

# Specifica il percorso per salvare l'immagine
file_path <- "./GRAPHS/boxPlots_tukey_avgDiam.jpg"

# Salva il boxplot come file PNG
ggsave(file_path, boxplot_gg, width = 16, height = 12, dpi = 700)

############################### VOLT #########################################

# Estrai i valori p dai risultati del test di Tukey
p_values <- Tukey_VOLT$treatment[, "p adj"]

# Determina i livelli di significatività
significance_levels <- ifelse(p_values < 0.001, "***", 
                              ifelse(p_values < 0.01, "**", 
                                     ifelse(p_values < 0.05, "*", "")))

# Crea una mappa tra le comparazioni e i simboli di significatività
comparison_names <- rownames(Tukey_VOLT$treatment)

significance_map <- data.frame(comparison = comparison_names, significance = significance_levels)

# Funzione per generare le etichette dei trattamenti
generate_treatment_labels <- function(TUKEY, variable) {
  # Estrai i livelli e le etichette dal test post-hoc di Tukey
  Tukey_levels <- TUKEY[[variable]][, 4]
  Tukey_labels <- data.frame(multcompLetters(Tukey_levels)['Letters'])
  
  # Ordina le etichette per allinearle al boxplot
  Tukey_labels$treatment <- rownames(Tukey_labels)
  Tukey_labels <- Tukey_labels[order(Tukey_labels$treatment), ]
  
  return(Tukey_labels)
}

# Applica la funzione al dataset
LABELS <- generate_treatment_labels(Tukey_VOLT, "treatment")

# Crea il dataframe principale
dataframe_VOLT <- result_df[, c(1:2,5)]

# Aggiungi le etichette al dataframe
dataframe_VOLT$labels <- LABELS[dataframe_VOLT$treatment, 1]
dataframe_VOLT$labels <- factor(dataframe_VOLT$labels, levels = unique(dataframe_VOLT$labels))

# Crea il boxplot con ggplot2
boxplot_gg <- ggplot(dataframe_VOLT, aes(x = treatment, y = rootVolume, fill = treatment)) +
  geom_boxplot(outliers = FALSE, alpha = 0.5) +
  ylim(c(0, 1.1 * max(dataframe_VOLT$rootVolume))) +
  labs(x = "Treatment", y = "Root Volume (cm3)", title = "Boxplot") +
  theme_minimal()

# Rimuovi le righe duplicate basate sul trattamento
max_values <- dataframe_VOLT %>%
  group_by(treatment) %>%
  summarize(max_volt = max(rootVolume[rootVolume <= quantile(rootVolume, 0.75) + 1.5 * IQR(rootVolume)]), labels = first(labels))

# Aggiungi le etichette al grafico
boxplot_gg <- boxplot_gg +
  geom_text(data = max_values, aes(label = labels, y = max_volt + 0), 
            position = position_dodge(width = 0.9), vjust = 0, size = 6, colour = "black")

# Stampa il boxplot modificato con le etichette
print(boxplot_gg)

# Specifica il percorso per salvare l'immagine
file_path <- "./GRAPHS/boxPlots_tukey_volt.jpg"

# Salva il boxplot come file PNG
ggsave(file_path, boxplot_gg, width = 16, height = 12, dpi = 700)
















