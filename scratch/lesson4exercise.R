library(tidyverse)

# Generate sample data
# Sightings of Black Oystercatcher chicks at Santa Cruz beaches
beaches <- c("Cowell's", "Steamer Lane", "Natural Bridges", "Mitchell's", "Main")
band_colors <- c("B", "G", "K", "W", "Y") # blue, green, black, white, yellow
surveys <- seq(as.Date("2023-06-01"), as.Date("2023-08-31"), by = 7)

set.seed(1538)
# 3 band colors identify a bird. We want 12 birds.
birds <- paste0(
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE)
) %>% 
  unique() %>%
  head(12)
bloy_chicks <- tibble(
  # Randomly generate survey data
  beach = sample(beaches, size = 100, replace = TRUE),
  bird = sample(birds, size = 100, replace = TRUE),
  survey = sample(surveys, size = 100, replace = TRUE)
) %>% 
  # Remove duplicates (see ?distinct)
  distinct() %>% 
  # Sort by survey date and location
  arrange(survey, beach)

# We want to estimate where chicks hatched
# For each bird, where was it seen most often?
# If multiple sites are tied, choose the one with the earliest observation
# If still tied, randomly choose one

# Try it with just tidyverse functions
# Find most frequent beach per bird
beach_freq <- bloy_chicks %>% 
  count(bird, beach) %>% 
  group_by(bird) %>% 
  filter(n == max(n)) %>% 
  ungroup()
# Find first date for each bird+beach
beach_early <- bloy_chicks %>% 
  group_by(bird, beach) %>% 
  summarize(earliest = min(survey),
            .groups = "drop")
# Join the two conditions and retain most frequent beach, only earliest
hatch_beach <- beach_freq %>% 
  left_join(beach_early, by = c("bird", "beach")) %>% 
  # Apply the filter by group
  # What happens if you don't group? Why is that wrong?
  group_by(bird) %>% 
  filter(earliest == min(earliest)) %>% 
  ungroup()
# Uh oh, GYB and KYW still have two beaches. Try again, but choose one at random
hatch_beach <- beach_freq %>% 
  left_join(beach_early, by = c("bird", "beach")) %>% 
  group_by(bird) %>% 
  filter(earliest == min(earliest)) %>% 
  sample_n(1) %>% # Randomly choose 1 row. See ?sample_n
  ungroup()

# Two issues with this approach 
# 1) It's kind of long and we have to make multiple intermediate data frames
# 2) The logic for estimating a hatching beach is spread out across multiple 
# locations in the code. If we choose a different approach then we have to 
# change everything.

# Different approach: functions
# 1) Put all the logic for estimating the hatching beach in one place
# 2) Call that function with summarize()

# This is an example of split-apply-combine
# Use group_by() to split our data frame by bird
# Write a custom function to estimate the hatching beach *for that bird*
#  That's critical: this function works on one part of the whole!
# Use summarize() to apply our function to each bird and combine the results
find_hatching_beach <- function(site, date) {
  # What's the value of `site` for bird YWG? How about WYB? 
  # What's the value of `date` for those birds?
  # For the above ?'s: which part is the parameter and which is the argument?
  
  # Start with a data frame (well, tibble) of site and date *for one bird*
  # Use pipes and dplyr functions to find the (estimated) hatching beach
  bird_observations <- tibble(site, date)
  result <- bird_observations %>% 
    ___ # use as many pipes and dplyr functions as necessary
  # result should end up as a data frame with one row for the hatching beach
  return(result$site) # return the site
}

bloy_chicks %>% 
  # split
  ___ %>% 
  # apply and combine
  ___

# Think carefully how you'll use find_hatching_beach()!
