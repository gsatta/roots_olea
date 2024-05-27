################################################################################

#                            2_1 Disease Index

################################################################################

library(readxl); library(ggplot2)

dataframe <- readxl::read_excel("./DISEASE_INDEX/disease_index_0.xlsx")

# Convertire le colonne dalla quinta alla trentaduesima in numerico
dataframe <- dataframe %>%
  mutate(across(5:32, as.numeric))

# Elimina le colonne dalla 5 alla 12
dataframe <- dataframe[, -c(5:12)]

# Trasforma il dataframe da wide a long
df_long <- tidyr::pivot_longer(dataframe, 
                               cols = starts_with("09-02-2024"):starts_with("19-04-2024"),
                               names_to = "Date",
                               values_to = "Class")

# Filtra le righe dove il valore della colonna "Cod" è diverso da "PH536"
df_long <- df_long %>% 
  filter(Cod != "PH536")

# Converti la colonna Date in formato data
df_long$Date <- as.Date(df_long$Date, format = "%d-%m-%Y")

# Converti le date in numero di giorni
df_long$Days <- as.numeric(df_long$Date - min(df_long$Date))

# Calcola la media di Class per ogni combinazione di Inoculum, Treatment e Days
df_summary <- df_long %>%
  group_by(Cod, Treatment, Days) %>%
  summarize(mean_class = mean(Class)) %>%
  ungroup()

# Scrivi il dataframe sommario in un file CSV
write_csv(df_summary, "./DATAFRAMES/df_summary_modified.csv")

# Ripulisci l'enviroment di R
rm(list=ls())

df_summary <- read_csv("./DATAFRAMES/df_summary_modified.csv")

trattamenti <- unique(df_summary$Treatment)

# Crea il grafico unico con facet_wrap
combined_plot <- ggplot(df_summary, aes(x = Days, y = mean_class, color = Cod)) +
  geom_step(linewidth = 1) + # Usando geom_step per linee a gradino
  labs(title = "",
       x = "Days",
       y = "Mean disease Class") +
  ylim(0, 3) +
  facet_wrap(~ Cod, strip.position = "bottom") + # Create a plot for each species
  theme_minimal() + # Use a minimal teme
  theme(
    plot.title = element_text(size = 18, face = "bold"), # Dimensione del titolo
    axis.title.x = element_text(size = 14), # Dimensione del titolo dell'asse x
    axis.title.y = element_text(size = 14), # Dimensione del titolo dell'asse y
    axis.text = element_text(size = 12), # Dimensione dei numeri sugli assi
    strip.text = element_text(size = 14), # Dimensione del testo dei facet
    legend.position = "none"
  )

# Print the combined graph
print(combined_plot)

# Save the plot as jpg file
ggsave("./GRAPHS/mean_DI.jpg", combined_plot, width = 16, height = 12, dpi = 700)
