################################################################################

#                            2_1 Disease Index

################################################################################

library(readxl); library(ggplot2)

dataframe <- readxl::read_excel("./DISEASE_INDEX/disease_index_0.xlsx")

# Convertire le colonne dalla quinta alla trentaduesima in numerico
dataframe <- dataframe %>%
  mutate(across(5:32, as.numeric))

# Trasforma il dataframe da wide a long
df_long <- tidyr::pivot_longer(dataframe, 
                               cols = starts_with("10-01-2024"):starts_with("19-04-2024"),
                               names_to = "Date",
                               values_to = "Class")

# Converti la colonna Date in formato data
df_long$Date <- as.Date(df_long$Date, format = "%d-%m-%Y")

# Converti le date in numero di giorni
df_long$Days <- as.numeric(df_long$Date - min(df_long$Date))

# Calcola la media di Class per ogni combinazione di Inoculum, Treatment e Days
df_summary <- df_long %>%
  group_by(Treatment, Days) %>%
  summarize(mean_class = mean(Class)) %>%
  ungroup()

# Scrivi il dataframe sommario in un file CSV
write_csv(df_summary, "./DATAFRAMES/df_summary_modified.csv")

# Ripulisci l'enviroment di R
rm(list=ls())

df_summary <- read_csv("./DATAFRAMES/df_summary_modified.csv")

trattamenti <- unique(df_summary$Treatment)

# Crea il grafico unico con facet_wrap
combined_plot <- ggplot(df_summary, aes(x = Days, y = mean_class, color = Treatment)) +
  geom_step(linewidth = 1) + # Usando geom_step per linee a gradino
  labs(title = "Mean Class over Days by Treatment",
       x = "Days",
       y = "Mean Class") +
  ylim(0, 4) +
  facet_wrap(~ Treatment) + # Crea un pannello per ogni trattamento
  theme_minimal() # Usa un tema minimalista

# Stampa il grafico combinato
print(combined_plot)

# Salva il layout di griglia come file PNG
ggsave("./GRAPHS/mean_DI.jpg", combined_plot, width = 16, height = 12, dpi = 700)
