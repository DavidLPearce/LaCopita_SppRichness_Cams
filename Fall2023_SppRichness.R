# -------------------------------------------------------
#
#                    Load libraries
#
# -------------------------------------------------------

# Install Packages (if needed)
# install.packages("tidyverse")
# install.packages("devtools")
# install.packages("purrr")
# install.packages("fossil")
# devtools::install_github("AnneChao/SpadeR")


# Load Packages
library("tidyverse")
library("devtools")
library("purrr")
library("fossil")
library("SpadeR")



# Set seed, scientific notation options, and working directory
set.seed(123)
options(scipen = 9999)
setwd(".")

# -------------------------------------------------------
#
#                    Load Data
#
# -------------------------------------------------------

# Read in detections
cam_dat <- read.csv("./Data/LaCopita_Cams_Fall2023_SnapShot.csv")

# Head look at first few rows
head(cam_dat)

# Look at data structure
str(cam_dat)


# -------------------------------------------------------
#
#                   Data Wrangling
#
# -------------------------------------------------------



# Formatting time and creating a survey_date column
cam_dat <- cam_dat %>%
  mutate(survey_date = as.Date(start_time, format = "%m/%d/%Y %H:%M"))

# take a look
head(cam_dat)


# Summarize data by site_camera and survey date for each species. If present on that date spp = 1, otherwise = 0
summary_dat <- cam_dat %>%
  filter(!is_blank) %>% # Exclude blank records
  group_by(deployment_id, survey_date, common_name) %>%
  summarize(detection = 1, .groups = "drop") %>% # Mark detection as 1
  pivot_wider(names_from = common_name, values_from = detection, values_fill = 0) # Fill NAs with 0

# Take a look
head(summary_dat)

# Adding a day of year column
summary_dat <- summary_dat %>%
                     mutate(
                       doy = yday(survey_date)) %>% # Add Day of Year column
                       arrange(doy) # Order by Day of Year

# Take a look
View(summary_dat)

# Extract SiteID into new column
summary_dat <- summary_dat %>%
  mutate(
    SiteID = str_extract(deployment_id, "\\d+"),  # Extract the site number
  )

# Take a look
View(summary_dat)
str(summary_dat)

# Changing DOY to Occasion where lowest DOY = Occasion 1
summary_dat <- summary_dat %>%
  mutate(Occasion = dense_rank(doy))

# View the result
str(summary_dat)

# Change site id to numeric
summary_dat$SiteID <- as.numeric(summary_dat$SiteID)

# Arrange the data by SiteID, and Occasion
summary_dat <- summary_dat %>%
  arrange(SiteID, Occasion)

# Reorder columns by moving doy, SiteID, and Occasion before deployment_id
summary_dat <- summary_dat %>%
  select( SiteID,  Occasion, doy, everything())

# View the result
View(summary_dat)
str(summary_dat)

# Subset to only species of interest  
# **** you may want to add some more species or remove some depending on the camera dataset ****  
# use unique(cam_dat$common_name) for a list of all detections
species_columns <- c("White-tailed Deer", "Coyote", "Northern Raccoon", "Southern Plains Woodrat",
                     "American Badger", "Collared Peccary", "Wild Boar", "Bobcat", "Virginia Opossum",
                     "Grey Fox", "Eastern Cottontail", "Nine-banded Armadillo")

# Subsetting
subset_data <- summary_dat %>%
  select(SiteID, Occasion, all_of(species_columns))

# View the result
View(subset_data)
str(subset_data)



# -------------------------------------
# Detection Matrix: Out & Down Cameras
# -------------------------------------

# Create a new column for site_camOren by combining SiteID and CamOren
subset_data <- subset_data %>%
  mutate(site_camOren = paste(SiteID, CamOren, sep = "_"))


# Create a sequence of occasions from the minimum to the maximum
all_occasions <- seq(min(subset_data$Occasion), max(subset_data$Occasion))

# Get the list of species columns (columns 4 to 21) ** if you added or removed species this number will change
# use View(subset_data) and hover over the column to get number
species_columns <- colnames(subset_data)[4:21]

# Create all combinations of site_camOren, species, and occasions
all_combinations <- expand.grid(
  site_camOren = unique(subset_data$site_camOren),
  species = species_columns,  # Now we use only the species columns
  Occasion = all_occasions
) %>% 
  mutate(species_occ = paste(species, Occasion, sep = "."))

# Check the structure of the combinations
str(all_combinations)

# Create an empty dataframe with all combinations
det_OaD_mat <- data.frame(matrix(0, nrow = length(unique(all_combinations$site_camOren)),
                                  ncol = length(unique(all_combinations$species_occ))))
rownames(det_OaD_mat) <- unique(all_combinations$site_camOren)
colnames(det_OaD_mat) <- unique(all_combinations$species_occ)

 
# Loop over subset_data to fill the matrix with detection none detection
for(i in 1:nrow(subset_data)) {
  site <- subset_data$site_camOren[i]
  occ <- subset_data$Occasion[i]
  
  for(species in species_columns) {
    # Construct species_occ key
    species_occ <- paste(species, occ, sep = ".")
    
    # Find the matching species column in subset_data
    count_value <- subset_data[[species]][i]
    
    # Fill the matrix
    if (count_value > 0) {
      det_OaD_mat[site, species_occ] <- count_value
    }
  }
}

# Replace "Down" with "D" and "Out" with "O" in row names
rownames(det_OaD_mat) <- gsub("Down", "D", rownames(det_OaD_mat))
rownames(det_OaD_mat) <- gsub("Out", "O", rownames(det_OaD_mat))

# Replace spaces with underscores in column names
colnames(det_OaD_mat) <- gsub(" ", "_", colnames(det_OaD_mat))


# Take a look
#View(det_OaD_mat)
str(det_OaD_mat)
head(det_OaD_mat[,1:5])




# -------------------------------------
# Detection Matrix: Site
# -------------------------------------

# Create a sequence of occasions from the minimum to the maximum
all_occasions <- seq(min(subset_data$Occasion), max(subset_data$Occasion))

# Get the list of species columns (columns 4 to 21) ** if you added or removed species this number will change
# use View(subset_data) and hover over the column to get number
species_columns <- colnames(subset_data)[-c(1:2)]


# Create all combinations of sites, species, and occasions
all_combinations <- expand.grid(
  site = unique(subset_data$SiteID),
  species = species_columns,  # Now we use only the species columns
  Occasion = all_occasions
) %>% 
  mutate(species_occ = paste(species, Occasion, sep = "."))

# Check the structure of the combinations
str(all_combinations)

# Create an empty dataframe with all combinations
det_site_mat <- data.frame(matrix(0, nrow = length(unique(all_combinations$site)),
                                 ncol = length(unique(all_combinations$species_occ))))


rownames(det_site_mat) <- unique(all_combinations$site_camOren)
colnames(det_site_mat) <- unique(all_combinations$species_occ)

# Loop to fill the  matrix
for(i in 1:nrow(subset_data)) {
  site <- subset_data$SiteID[i]
  occ <- subset_data$Occasion[i]
  
  for(species in species_columns) {
    # Construct species_occ key
    species_occ <- paste(species, occ, sep = ".")
    
    # Find the matching species column in subset_data
    count_value <- subset_data[[species]][i]
    
    # Fill the matrix
    if (count_value > 0) {
      det_site_mat[site, species_occ] <- count_value
    }
  }
}



# Replace spaces with underscores in column names
colnames(det_site_mat) <- gsub(" ", "_", colnames(det_site_mat))

# Take a look
#View(det_site_mat)
str(det_site_mat)
head(det_site_mat[,1:5])
 

# -------------------------------------------------------
#
#               Species Richness: Chao
#
# -------------------------------------------------------

# Get all unique species names by splitting column names before the "."
species_names <- unique(gsub("\\..*", "", colnames(det_site_mat)))

# Initialize the list to store data frames for all sites
samplist <- list()

# Loop through each site
for (i in 1:nrow(det_site_mat)) {
  
  # Initialize a list to store data for each species
  species_list <- list()
  
  # Extract data for each species using dynamic column selection
  for (species in species_names) {
    species_cols <- grep(paste0("^", species, "\\."), colnames(det_site_mat))  # Find columns for the species
    species_data <- det_site_mat[i, species_cols, drop = FALSE]
    colnames(species_data) <- seq_along(species_cols)  # Standardize column names
    species_list[[species]] <- species_data
  }
  
  # Bind all species data
  samp <- do.call(rbind, species_list)
  
  # Remove columns with all NA values
  samp <- samp[, colSums(is.na(samp)) < nrow(samp)]
  
  # Add the resulting data frame to the list with site-specific names
  samplist[[paste0("Site", i)]] <- data.frame(samp)
}


####Run chao estimates 
#use purrr package to iterate through all estimates of chao2
#this needs to be cleared before each run so you have an empty data frame
chao2 = data.frame()
chao2se = data.frame()
chao2lo = data.frame()
chao2up = data.frame()
# i = 14  
# i = 18
# i = 20
for (i in 1:length(samplist)) {
  
  tryCatch({
    
    esite <- as.data.frame(samplist[i])
    
    out1 = tryCatch(ChaoSpecies(esite, datatype = "incidence_raw", k = 12, conf= 0.95))
    

    print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #create list of chao2 estimates [2,1], se [2,2], 95 lower [2,3], 95 upper [2,4]
  chao2 = rbind(chao2,out1$Species_table[2,1])
  chao2se = rbind(chao2se,out1$Species_table[2,2])
  chao2lo = rbind(chao2lo,out1$Species_table[2,3])
  chao2up = rbind(chao2up,out1$Species_table[2,4])
}

result <-cbind(chao2,chao2se,chao2lo,chao2up)
colnames(result) <- c("chao2","chao2se","chao2lo","chao2up")
print(result)

# Export
write.csv(result, "Fall2023_chao2_results.csv")

####Run chao estimates with different package (No Errors)
#use purrr package to iterate through all estimates of chao2
#this needs to be cleared before each run so you have an empty data frame
###FossilPackage
chao2fossil = data.frame()

for (i in 1:length(samplist)) {
  
  esite <- as.data.frame(samplist[i])
  
  out1 = chao2(esite)
  
  #create list of chao2 estimates
  chao2fossil = rbind(chao2fossil,out1)
}
chao2fossil
#write.csv(chao2fossil, "Fall2023_chao2_fossil_results.csv")




































