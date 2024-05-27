################################################################################

#                      1_4 cREATE BOXPLOTS

################################################################################
library(ggplot2); library(gridExtra); library(grid); library(readr)
result_df  <- read_csv("./DATAFRAMES/result_df.csv")

result_df$...1 <- NULL

data <- result_df

# Converti la colonna treatment in un fattore
data$treatment <- factor(data$treatment)

# Definizione dei colori desiderati per PC e NI
# colors <- c("PC" = "lightcoral", "NI" = "lightblue")

## This is the plot from which I extract the legend.
# leg <- ggplot(data, aes(x = treatment, y = length, fill = treatment)) +
#   geom_boxplot() +
#   labs(x = "", y = "Length (cm)") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
#   guides(fill = guide_legend(ncol = 3, title = "Treatment")) +
#   # scale_fill_manual(values = colors) +
#   theme(legend.position = "bottom")

# Extract the legend from one of the graphs.
# legend <- cowplot::get_legend(leg)

# Creazione della lista di grafici
plots <- list()

variables <- colnames(data[,3:10])

# Ciclo for per creare i grafici
for (var in variables) {
  plot <- ggplot(data, aes_string(x = "treatment", y = var, fill = "treatment")) +
    geom_boxplot(show.legend = FALSE) +
    labs(x = ifelse(var == "length", "Treatments", ""), 
         y = switch(var,
                    "length" = "Total Length (cm)",
                    "avgDiam" = "AvgDiam (cm)",#
                    "rootVolume" = "Total Roots Volume (cm3)",
                    "FRL" = "Fine Root Length (cm)", #
                    "CRL" = "Coarse Root Length (cm)", #
                    "FRS" = "Fine Root Surface (cm2)", #
                    "CRS" = "Coarse Root Surface (cm2)", #
                    "FVOL" = "Fine Root Volume (cm3)")) + # 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) 
    # scale_fill_manual(values = colors) +
    # theme(legend.position = "none")
  
  plots[[var]] <- plot
}

# Posizionamento della legenda nella griglia
grid1 <- grid.arrange(arrangeGrob(grobs = plots, ncol = 3))

# Aggiunta del titolo al grafico composto
grid1 <- grid.arrange(grobs = list(grid1), top = textGrob("Roots Boxplots - Wild Olive", gp = gpar(fontsize = 16)))

# Specifies the path to save
file_path <- "./GRAPHS/boxPlots_olivastro.png"

# Save the grid as a PNG file.
ggsave(file_path, grid1, width = 16, height = 12, dpi = 700)
