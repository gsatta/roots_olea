################################################################################

#                      1_2 Select the necessary columns

################################################################################

# Load the combined data set previosly stored
combined_df <- read_csv("./DATAFRAMES/combined_df.csv")

# Delete undesired column $...1
combined_df$...1 <- NULL

# Select the necessary columns of the roots parameters
cols_fine_roots_lenght <- combined_df %>%
  select(1:3, 66:69)

cols_fine_roots_lenght <- cols_fine_roots_lenght %>%
  mutate(FRL = rowSums(select(., 4:7)))

cols_coarse_roots_lenght <- combined_df %>%
  select(1:3, 70:75)

cols_coarse_roots_lenght <- cols_coarse_roots_lenght %>%
  mutate(CRL = rowSums(select(., 4:9)))

cols_fine_roots_SA <- combined_df %>%
  select(1:3, 77:80)

cols_fine_roots_SA <- cols_fine_roots_SA %>%
  mutate( FRS = rowSums(select(., 4:7)))

cols_coarse_roots_SA <- combined_df %>%
  select(1:3, 81:86)

cols_coarse_roots_SA <- cols_coarse_roots_SA %>%
  mutate(CRS = rowSums(select(., 4:9)))

cols_fine_roots_VOL <- combined_df %>%
  select(1:3, 99:102)

cols_fine_roots_VOL <- cols_fine_roots_VOL %>%
  mutate(FVOL = rowSums(select(., 4:7)))

# Add the coulumn "FRL" to combined_df
combined_df <- left_join(combined_df, cols_fine_roots_lenght %>% select(treatment, `Sample Id`, Plant, FRL), by = c("treatment", "Sample Id", "Plant"))

# Add the coulumn "CRL" to combined_df
combined_df <- left_join(combined_df, cols_coarse_roots_lenght %>% select(treatment, `Sample Id`, Plant, CRL), by = c("treatment", "Sample Id", "Plant"))

# Add the coulumn "FRS" to combined_df
combined_df <- left_join(combined_df, cols_fine_roots_SA %>% select(treatment, `Sample Id`, Plant, FRS), by = c("treatment", "Sample Id", "Plant"))

# Add the coulumn "CRS" to combined_df
combined_df <- left_join(combined_df, cols_coarse_roots_SA %>% select(treatment, `Sample Id`, Plant, CRS), by = c("treatment", "Sample Id", "Plant"))

# Add the coulumn "FVOL" to combined_df
combined_df <- left_join(combined_df, cols_fine_roots_VOL %>% select(treatment, `Sample Id`, Plant, FVOL), by = c("treatment", "Sample Id", "Plant"))


# Select only the columns number 1:4, 19, 25, 29, 33, 32, 34, 27, 126, 127, 128, 129, 130
selected_columns <- combined_df %>%
  select(1:3, 18, 24, 28, 125:129)

# Assing the new names to the columns
colnames(selected_columns) <- c("sample_Id", "treatment", "plant", "length", "avgDiam", "VOLT", "FRL", "CRL", "FRS", "CRS", "FVOL")

# Check if the columns have a new names 
colnames(selected_columns)

# Store the selected_columns data frame in csv format
write.csv(selected_columns, "./DATAFRAMES/selected_columns.csv", append = FALSE)

# Clean the R enviroment
rm(list=ls())
