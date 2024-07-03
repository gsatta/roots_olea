################################################################################

#                             1_1 GET THE DATA

################################################################################
library(dplyr); library(readxl); library(readr)

# Get the name of the file xlxs inside the folder
file_names <- list.files(path = "./DATA", pattern = "\\.xlsx$")

# Inizialize a new empty data frame
combined_df <- data.frame()

# Read each file and add it in a new dataframe
for (file in file_names) {
  temp_df <- readxl::read_excel(paste("./Data", file, sep = "/"), sheet = "Global")
  combined_df <- rbind(combined_df, temp_df)
}

# Extract the Treatment column from the "Sanmple id" column and create a new "Treatment" column
combined_df$treatment <- sapply(strsplit(combined_df$`Sample Id`, "_"), `[`, 1)

# Move the "Treatment" column to the second place
combined_df <- combined_df %>%
  select(`Sample Id`, treatment, Plant, everything())

# Store the dataframe in csv format
write.csv(combined_df, "./DATAFRAMES/combined_df.csv", append = FALSE)

# Ripulisci l'enviroment di R
rm(list=ls())

combined_df <- read_csv("./DATAFRAMES/combined_df.csv")
combined_df$...1 <- NULL

# Ripulisci l'enviroment di R
rm(list=ls())

####################   weight data    ###############################

# Load the weight data
weigth <- read_excel("./DATAFRAMES/weigth_0.xlsx", sheet = "weigth")

write_csv(weigth, "./DATAFRAMES/weigth.csv")

# Clean the R environment
rm(list=ls())
