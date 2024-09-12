

################################################################################
# About
# data processing script written by JG.Smith jossmith@mbayaq.org




################################################################################
#steps still required

#1. Need to account for subsample kelp on line 179
#2. Check survey date as part of final join
#2. Check final join to identify data inconsistencies

################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape)
gs4_auth()

#read data
quad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 1) %>% clean_names()

urchin_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                         sheet = 2) %>% clean_names()


kelp_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1i9rHc8EAjMcqUqUDwjHtGhytUdG49VTSG9vfKmDPerQ/edit?gid=0#gid=0",
                       sheet = 3) %>% clean_names()

gonad_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4/edit?gid=0#gid=0") %>%
  clean_names()


################################################################################
#Step 1 - process quadrat data

#inspect
View(quad_raw)

quad_build <- quad_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-notes, -windows_ctrl_alt_shift_0_mac_command_option_shift_0, -write_in) %>%
  # Set quadrat to numeric by removing R/L
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(survey_date),
    transect = as.numeric(transect),
    quadrat = as.numeric(quadrat),
    substrate = factor(substrate)
  ) %>%
  # Extrapolate densities for subsamples
  mutate(
    purple_urchin_densitym2 = purple_urchins * (4 / purple_quadrants_sampled),
    purple_urchin_conceiledm2 = purple_conceiled * (4 / purple_quadrants_sampled),
    red_urchin_densitym2 = red_urchins * (4 / red_quadrants_sampled),
    red_urchin_conceiledm2 = red_conceiled * (4 / red_quadrants_sampled),
    tegula_densitym2 = tegula * (4 / tegula_quadrants_sampled),
    pomaulax_densitym2 = pomaulax * (4 / pomaulax_quadrants_sampled)
  ) %>%
  # Drop old columns
  select(-purple_urchins, -purple_conceiled, -purple_quadrants_sampled,
         -red_urchins, -red_conceiled, -red_quadrants_sampled,
         -tegula, -tegula_quadrants_sampled, -pomaulax, -pomaulax_quadrants_sampled) %>%
  #calculate upc
  # Step 1: Convert upc1 through upc8 columns to long format
  pivot_longer(cols = starts_with("upc"), names_to = "upc", values_to = "species") %>%
  # Step 2: Group by all necessary columns, including the key columns
  group_by(name_of_data_enterer, site, site_type, zone, survey_date, observer_buddy,
           transect, quadrat, substrate,relief, risk, species, purple_urchin_densitym2, purple_urchin_conceiledm2,
           red_urchin_densitym2, red_urchin_conceiledm2, tegula_densitym2, pomaulax_densitym2,
           lamr, macr, macj, nerj, ptej, lsetj, eisj) %>%
  # Step 3: Calculate percent cover directly
  summarise(percent_cover = (n() / 8) * 100, .groups = 'drop') %>%
  # Step 4: Reshape back to wide format with species as columns, adding 'upc_' prefix
  pivot_wider(names_from = species, values_from = percent_cover, values_fill = 0,
              names_prefix = "upc_") %>%
  # Clean column names and remove any columns with 'na' in the name
  clean_names() %>%
  #clean up
  mutate(substrate = word(substrate, 1))


################################################################################
#Step 2 - process urchin size data

#inspect
#View(urchin_raw)

#build raw size fq
urch_build <- urchin_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_unit = factor(depth_units),
    species = factor(species),
    size = factor(size),
    count = as.numeric(count)
  ) %>%
  select(-date, depth_unit) %>%
  #convert feet to meters
  mutate(depth_m = ifelse(depth_unit == "Feet",depth*0.3048,depth)) %>%
  select(-depth_units, -depth_unit, -depth) %>%
  select(name_of_data_enterer, survey_date, everything()) %>%
  mutate(size = as.numeric(as.character(size))) %>%
  filter(!is.na(size))

# Expand the data based on counts
urch_expanded <- expandRows(urch_build, "count")


# Calculate mean and standard deviation for each group
urch_size_summary <- urch_expanded %>%
  group_by(site, survey_date, site_type, zone, transect, species) %>%
  summarise(
    mean_size = mean(size),  # Mean size after expanding
    sd_size = sd(size)       # Standard deviation after expanding
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = species, 
    values_from = c(mean_size, sd_size),
    names_glue = "size_{species}_{.value}_cm"
  ) %>% clean_names




################################################################################
#Step 3 - process swath data

#inspect
#View(kelp_raw)

#build kelp
kelp_build <- kelp_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_unit = factor(depth_units),
    species = factor(species),
    count = as.numeric(count)
  ) %>%
  #convert feet to meters
  mutate(depth_m = ifelse(depth_unit == "Feet",depth*0.3048,depth)) %>%
  select(-depth_units, -depth_unit, -depth)
#calculate macro density
macro_density <- kelp_build %>% filter(species == "MACPYR") %>%
  #macro is not subsampled
  select(-subsample_meter, -count) %>%
  group_by(survey_date, site, site_type, zone, 
           transect)%>%
  summarize(density20m2_macro_plants = n(),
            density20m2_macro_stipes = mean(stipe_counts_macrocystis_only)
  )
kelp_density <- kelp_build %>% filter(species != "MACPYR") %>%
  select(-stipe_counts_macrocystis_only) %>%
  #######ACCOUNT FOR SUMSAMPLE HERE############
group_by(survey_date, site, site_type, zone, 
         transect, depth_m, species)%>%
  summarize(density20m2 = mean(count)) %>%
  # Rename species to include "density20m2_" prefix
  mutate(species = paste0("density20m2_", species)) %>%
  # Pivot wider by species
  pivot_wider(
    names_from = species, 
    values_from = density20m2,
    values_fill = 0 #replace NA with true 0
  ) %>% 
  clean_names()


################################################################################
#Step 4 - join everything

#inspect for join
str(quad_build) #base df
str(urch_size_summary)
str(macro_density)
str(kelp_density)


quad_build_combined <- quad_build %>%
  left_join(urch_size_summary, by = c("survey_date", "site", "site_type", "zone", "transect")) %>%
  left_join(macro_density, by = c("survey_date", "site", "site_type", "zone", "transect")) %>%
  left_join(kelp_density, by = c("survey_date", "site", "site_type", "zone", "transect")) %>%
  # Replace NA values in macro plants and macro stipes with 0 for true zeros
  mutate(
    density20m2_macro_plants = replace_na(density20m2_macro_plants, 0),
    density20m2_macro_stipes = replace_na(density20m2_macro_stipes, 0)) 

################################################################################
#Step 5 - process dissection data


gonad_dat <- gonad_raw %>%
  # Replace missing values and clean up variables
  mutate(
    institution = as.factor(institution),
    name_of_data_enterer = as.character(name_of_data_enterer),
    date_collected = ymd(date_collected),
    date_fixed = ymd(date_fixed),
    date_processed = ymd(date_processed),
    site_number = as.factor(site_number),
    transect = as.factor(transect),
    treatment = as.factor(treatment),
    sex = as.factor(sex),
    test_height_mm = as.numeric(test_height_mm),
    test_diameter_mm = as.numeric(test_diameter_mm),
    animal_wet_mass_g = as.numeric(animal_wet_mass_g),
    animal_24hr_mass_g = as.numeric(animal_24hr_mass_g),
    gonad_mass_g = as.numeric(gonad_mass_g),
    soft_tissue_mass_g = as.numeric(soft_tissue_mass_g),
    notes = as.character(notes),
    date_entered = ymd_hms(date_entered)
  ) %>%
  # Select focal variables
  select(date_collected, site_number, transect, species, animal_24hr_mass_g, gonad_mass_g) %>%
  # Filter to recovery sites
  filter(grepl("^REC", site_number)) %>%
  # Clean up site name
  mutate(
    site_number = gsub("-", "_", site_number),
    site_number = gsub("REC_", "REC", site_number)
  ) %>%
  # Separate site_number into two parts and clean remaining underscores
  separate(site_number, into = c("site_number", "site_type"), sep = "_", remove = FALSE) %>%
  mutate(site_number = gsub("_.*", "", site_number)) %>%
  # Fix special cases
  mutate(site_number = case_when(
    site_number == "REC 11" ~ "REC11",
    site_number == "REC3" ~ "REC03",
    site_number == "REC03INCIP" ~ "REC03",
    site_number == "REC4=FOR" ~ "REC04",
    site_number == "REC5=BAR" ~ "REC05",
    site_number == "RECD12" ~ "REC12",
    TRUE ~ site_number  # Keep the original value if no condition matches
  ),
  site_type = case_when(
    site_type == "INCIIP" ~ "INCIP",
    site_type == "INCEP" ~ "INCIP",
    site_type == "INCID" ~ "INCIP",
    TRUE ~ site_type  # Keep the original value if no condition matches
  )) %>%
  mutate(transect = toupper(transect),
         transect = ifelse(transect == "01", NA, transect)) %>%
  # Add small constant to gonad data
  mutate(gonad_mass_g = gonad_mass_g + 0.000001) %>%
  # Calculate gonad index
  mutate(gonad_index = (gonad_mass_g / animal_24hr_mass_g) * 100) %>%
  # Calculate mean gonad index for each site and transect
  group_by(date_collected, site_number, site_type, transect, species) %>%
  summarize(mean_GI = mean(gonad_index, na.rm = TRUE)) %>%
  # Pivot data wider by species
  pivot_wider(names_from = species, values_from = mean_GI) %>%
  mutate(site = factor(site_number),
         zone = factor(transect),
         site_type = factor(site_type),
         site_number = factor(site_number))

################################################################################
#Step 6 - export

# Remove all objects except 'quad_build_combined'
rm(list = setdiff(ls(), c("quad_build_combined","gonad_dat")))

#write to 2024 intern folder
#output_file_path <- "/Users/jossmith/2024_KFI_projects/output/raw/recovery_data.csv"
#write.csv(quad_build_combined, file = output_file_path, row.names = FALSE)




