################################################################################

#                      1_5 Statistical analysis

################################################################################
library(readr)

# Load the file
result_df  <- read_csv("./DATAFRAMES/result_df.csv")
result_df$...1 <- NULL

result_df$treatment <- factor(result_df$treatment)

################### Verifica delle assunzioni di anova ################### 
library(forecast)
# Modello ANOVA
modello <- aov(length ~ treatment, data = result_df)

# Residui del modello
residui <- residuals(modello)

# Test di Shapiro-Wilk per la normalità
shapiro_result <- shapiro.test(residui)
print(shapiro_result)

# Q-Q plot
qqnorm(residui)
qqline(residui, col = "red")

# Test di Levene per l'omogeneità delle varianze
library(car)
levene_result <- leveneTest(length ~ treatment, data = result_df)
print(levene_result)

################### avgDiam

# Modello ANOVA
modello <- aov(avgDiam ~ treatment, data = result_df)

# Residui del modello
residui <- residuals(modello)

# Test di Shapiro-Wilk per la normalità
shapiro_result <- shapiro.test(residui)
print(shapiro_result)

# Q-Q plot
qqnorm(residui)
qqline(residui, col = "red")

# Test di Levene per l'omogeneità delle varianze
library(car)
levene_result <- leveneTest(avgDiam ~ treatment, data = result_df)
print(levene_result)

################################################################################

# Modello ANOVA
anova_length <- aov(length ~ treatment, data = result_df)

summary(anova_length)

# > summary(anova_length)
# Df    Sum Sq  Mean Sq F value   Pr(>F)    
# treatment   11 111056871 10096079   6.526 2.04e-07 ***
#   Residuals   72 111389197  1547072                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova_avgDiam <- aov(avgDiam ~ treatment, data = result_df)

summary(anova_avgDiam)

# > summary(anova_avgDiam)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# treatment   11 0.3587 0.03260   9.143 5.55e-10 ***
#   Residuals   72 0.2568 0.00357                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova_VOLT <- aov(rootVolume ~ treatment, data = result_df)
summary(anova_VOLT)

# > summary(anova_VOLT)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# treatment   11  340.9   30.99   5.419 3.28e-06 ***
#   Residuals   72  411.8    5.72                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

################################################################################

Tukey_length <- TukeyHSD(anova_length, conf.level=0.95)

Tukey_length

# Estrai i risultati del test di Tukey
tukey_results <- Tukey_length$treatment

# Estrai i valori delle differenze significative, intervalli di confidenza e valori p-adj
diff <- tukey_results[, "diff"]
lwr <- tukey_results[, "lwr"]
upr <- tukey_results[, "upr"]
p_adj <- tukey_results[, "p adj"]

# Crea il dataframe con i risultati
tukey_df <- data.frame(
  Comparison = rownames(tukey_results),
  Difference = diff,
  Lower_CI = lwr,
  Upper_CI = upr,
  P_Adjusted = p_adj,
  row.names = NULL
)

# Visualizza il dataframe
tukey_df

# Carica il pacchetto
library(xlsx)

# Specifica il percorso del file Excel
file_path <- "./RESULTS/risultati_tukey.xlsx"

# Salva il dataframe in un file Excel
write.xlsx(tukey_df, file_path, row.names = FALSE)

# Cattura il plot
plot_tukey <- base::plot(Tukey_length, las = 1, col="brown")

# Specifica il percorso del file
file_path_plot <- "./GRAPHS/tukey.png"

# Salva il plot come PNG
png(file_path_plot, width = 16, height = 12, units = "in", res = 700)
print(plot_tukey)  # Stampa esplicitamente il plot catturato
dev.off()   # Chiude il dispositivo grafico


library(emmeans)
library(multcomp)

EMM <- emmeans(anova_length, ~ treatment)
EMM    # display the means

contrast(EMM, "pairwise")

cld <- cld(EMM, decreasing = TRUE, Letters = letters)

cld

emmip(EMM, ~ treatment, CIs = T)


################################################################################

GGTukey.2 <- function(Tukey){
  A <- require("tidyverse")
  if (!A) {
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  B <- as.data.frame(Tukey[1])
  colnames(B)[2:4] <- c("min", "max", "p")
  
  # Create dataframe C with adjusted id based on p-value
  C <- data.frame(
    id = row.names(B),
    min = B$min,
    max = B$max,
    idt = ifelse(B$p < 0.05, "Significant", "Not significant")
  )
  
  # Reorder id based on p-value
  C$id <- factor(C$id, levels = C$id[order(B$p, decreasing = TRUE)])
  
  D <- C %>%
    ggplot(aes(id, color = idt)) +
    geom_errorbar(aes(ymin = min, ymax = max), width = 0.5, size = 1.25) +
    geom_hline(yintercept=0, color="red")+
    labs(x = NULL, color = NULL) +
    scale_color_manual(values = c("red", "green")) +
    coord_flip() +
    theme(
      text = element_text(family = "Arial"),
      title = element_text(color = "black", size = 15),
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 10),
      panel.grid = element_line(color = "grey75"),
      axis.line = element_blank(),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.59),
      legend.key = element_rect(color = "white", fill = "white")
    )
  
  return(D)
}

# Esegui la funzione GGTukey.2 con i dati Tukey_length
plot_tukey_length <- GGTukey.2(Tukey_length)

# Specifica il percorso del file
file_path_plot <- "./GRAPHS/tukey_length.png"

# Save the grid as a PNG file.
ggsave(file_path_plot, plot_tukey_length, width = 16, height = 12, dpi = 700)

################################################################################

Tukey_avgDiam <- TukeyHSD(anova_avgDiam, conf.level=0.95)

Tukey_avgDiam

# Esegui la funzione GGTukey.2 con i dati Tukey_length
plot_tukey_avgDiam <- GGTukey.2(Tukey_avgDiam)

# Specifica il percorso del file
file_path_plot <- "./GRAPHS/tukey_avgDiam.png"

# Save the grid as a PNG file.
ggsave(file_path_plot, plot_tukey_avgDiam, width = 16, height = 12, dpi = 700)


################################################################################

Tukey_VOLT <- TukeyHSD(anova_VOLT, conf.level=0.95)

Tukey_VOLT

# Esegui la funzione GGTukey.2 con i dati Tukey_length
plot_tukey_VOLT <- GGTukey.2(Tukey_VOLT)

# Specifica il percorso del file
file_path_plot <- "./GRAPHS/tukey_VOLT.png"

# Save the grid as a PNG file.
ggsave(file_path_plot, plot_tukey_VOLT, width = 16, height = 12, dpi = 700)



