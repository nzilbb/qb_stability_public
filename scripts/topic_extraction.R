# This script uses the `nzilbb.labbcat` package to extract 'topic' information 
# for each of our tokens (after the filtering step.)

library(tidyverse)
library(here)
library(nzilbb.labbcat)

labbcat.url <- "https://labbcat.canterbury.ac.nz/quakestudies"

# Load filtered data.
QB1 <- read_rds(here('data', 'QB1.rds'))
QB2 <- read_rds(here('data', 'QB2.rds'))

# Output layer id from labbcat instances
getLayerIds(labbcat.url)

# We want the 'type' layer for both of the data sets.
get_topic_data <- function(in_data) {
  getMatchAlignments(
    labbcat.url,
    match.ids = in_data$MatchId,
    layer.ids = c('type')
  )
}

QB1_topic_data <- get_topic_data(QB1)
QB2_topic_data <- get_topic_data(QB2)

QB1 <- QB1 %>% 
  mutate(
    type = QB1_topic_data$type
  )

QB2 <- QB2 %>% 
  mutate(
    type = QB2_topic_data$type
  )

# Export the data
write_rds(QB1, here('data', 'QB1_topics.rds'))
write_rds(QB2, here('data', 'QB2_topics.rds'))
