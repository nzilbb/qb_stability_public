# Anonymise location and high school data.

library(tidyverse)
library(here)

hs_data <- read_csv(here('data', 'private_highschooldata.csv'))
loc_data <- read_csv(here('data', 'private_locationdata.csv'))
correspondence_table <- read_rds(here('data', 'private_correspondence_table.rds'))

anon_speaker <- function(in_data) {
  in_data %>% 
    left_join(
      correspondence_table %>% 
        select(
          -code
        ) %>% 
        rename(
          Speaker = speaker
        )
    ) %>% 
    select(
      -Speaker
    ) %>% 
    rename(
      Speaker = anonymised
    )
}

hs_data <- anon_speaker(hs_data)
loc_data <- anon_speaker(loc_data)

write_rds(hs_data, here('data', 'hs_data.rds'))
write_rds(loc_data, here('data', 'loc_data.rds'))
