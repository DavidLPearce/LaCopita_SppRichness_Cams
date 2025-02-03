# -------------------------------------------------------
#
#                    Load libraries
#
# -------------------------------------------------------

# Install Packages (if needed)
# install.packages("tidyverse")


# Load Packages
library("tidyverse")
library(viridis)


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


# Making a Survey Date and Month of Survey Column
cam_dat <- cam_dat %>%
  mutate(survey_date = as.Date(start_time, format = "%m/%d/%Y %H:%M"),
         month = format(survey_date, "%B"))


# Extract SiteID into new column
cam_dat <- cam_dat %>%
  mutate(
    SiteID = str_extract(deployment_id, "\\d+"),  # Extract the site number
  )


# Take a look
head(cam_dat)
str(cam_dat)


# Filter the dataset to include only mammals
cam_dat <- cam_dat %>%
  filter(common_name %in% c("White-tailed Deer", "Coyote", "Northern Raccoon", "Southern Plains Woodrat",
                            "American Badger", "Collared Peccary", "Wild Boar", "Bobcat", "Virginia Opossum",
                            "Grey Fox", "Eastern Cottontail", "Nine-banded Armadillo"))

# Check the output
head(cam_dat)




# Plot of Spp detected by month
ggplot(cam_dat, 
       aes(fill=common_name, y=common_name, x=month)) + 
  geom_bar(position="fill", stat="identity")

# Counts of species at sites
ggplot(cam_dat , aes(x = month, fill = common_name)) + 
  geom_bar()+
  facet_wrap(~SiteID)


# Percentages of species at sites
ggplot(cam_dat , aes(x = month, fill = common_name)) + 
  geom_bar(position="fill")+
  facet_wrap(~SiteID)


# See https://ggplot2.tidyverse.org/index.html for more plot examples










