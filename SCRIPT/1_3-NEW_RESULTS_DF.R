################################################################################

#                   Create a new results Data Frame

################################################################################

# Load the selected_columns data frame stored previously
selected_columns <- read_csv("./DATAFRAMES/selected_columns.csv")

# Delete undesired column $...1
selected_columns$...1 <- NULL

# Create the result_df dataframe with the new summary columns per sample
result_df <- selected_columns %>%
  group_by(plant, treatment) %>%
  summarise(
    length = sum(length),
    avgDiam = mean(avgDiam),
    rootVolume = sum(VOLT),
    FRL = sum(FRL),
    CRL = sum(CRL),
    FRS = sum(FRS),
    CRS = sum(CRS),
    FVOL = sum(FVOL)
  )

# Convert the 'plant' column to characters (strings)
result_df$plant <- as.character(result_df$plant)

# Filtra le righe dove il valore della colonna "Cod" è diverso da "PH536"
result_df <- result_df %>% 
  filter(treatment != "PH536")

# Save the result_df dataframe in csv format
write_csv(result_df, "./DATAFRAMES/result_df.csv", append = FALSE)

# Clean the R enviroment
rm(list=ls())
