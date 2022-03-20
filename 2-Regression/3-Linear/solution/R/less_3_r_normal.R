# Load the core Tidyverse packages
library(tidyverse)
library(lubridate)

# Import the pumpkins data
setwd("C:/Users/HP/Documents/repositorio_ML/ML-For-Beginners/2-Regression/data")
pumpkins <- read_csv(file = "US-pumpkins.csv")
# Get a glimpse and dimensions of the data
glimpse(pumpkins)
#?glimpse

# Print the first 15 rows of the data set
pumpkins %>% 
  slice_head(n = 15)

# Return column names
pumpkins %>% 
  names()
