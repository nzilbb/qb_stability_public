# This script searches for all vowel tokens in the QB2 corpus, collects relevant
# attributes, and interfaces with Praat (through LaBB-CAT) in order to extract
# formants.

library(tidyverse)
library(glue)
library(here)
library(nzilbb.labbcat)

labbcat.url <- "https://labbcat.canterbury.ac.nz/quakestudies"

labbcatTimeout(seconds=12000)

vowels_pattern <- list(
  columns = list(
    list(
      layers = list(
        segment = list(pattern = "[IEVQU{i#3\\$u]")
      )
    )  
  )
)

matches <- getMatches(
  labbcat.url,
  pattern = vowels_pattern,
  aligned = TRUE,
  transcript.expression =  expressionFromTranscriptTypes(c('QB2')),
  page.length = 1000,
  main.participant = TRUE
)

# Collect QB1 and QB2 participant information.
participant_attributes <- getParticipantAttributes(
  labbcat.url,
  matches$Participant %>% unique(),
  c(
    'participant_gender', 'participant_age_category', 'participant_ethnicity',
    'participant_nz_ethnic', 'participant_grew_up', 'participant_grew_up_region',
    'participant_grew_up_town', 'participant_languages_spoken', 
    'participant_duration', 'participant_speaker articulation rate', 
    'participant_speaker speech rate', 'participant_word count',
    'participant_qb2_age_category', 'participant_qb2_gender',
    'participant_qb2_ethnicity', 'participant_qb2_iwi_hapū',
    'participant_qb2_secondary_school', 'participant_qb2_areas_lived_since_2012',
    'participant_qb2_area_lived_2020', 'participant_qb2_occupation_2012',
    'participant_qb2_occupation_2020', 'participant_qb2_household_2012',
    'participant_qb2_household_2020', 'participant_qb2_recently_seen_original'
  )
)

matches <- matches %>%
  left_join(
    participant_attributes %>%
      rename(
        Participant = participant
      )
  )

# Collect transcript attributes 
transcript_attributes <- getTranscriptAttributes(
  labbcat.url,
  matches$Transcript %>% unique(),
  c(
    'transcript_duration', 
    'transcript_word count', 
    'transcript_speaker articulation rate'
  )
)

matches <- matches %>%
  left_join(
    transcript_attributes %>%
      rename(
        Transcript = transcript
      )
  )

# Collect formants

formants <- processWithPraat(
  labbcat.url,
  matches$MatchId, matches$Target.segment.start, matches$Target.segment.end,
  praatScriptFormants(
    formants = c(1, 2),
    sample.points = c(0.5),
    max.number.formants = 5,
    max.formant = 5500,
    max.formant.male = 5000,
    gender.attribute = "participant_gender",
    value.for.male = "M",
    window.length = 0.025,
    preemphasis.from = 50,
    time.step=0.0025
  ),
  window.offset = 0.5 # process audio 0.5 seconds either side of target.
)

matches <- bind_cols(matches, formants)

# Formants to numeric formant.
matches <- matches %>%
  mutate(
    # Change formant columns to numeric file type.
    across(contains('time_0_5'), as.numeric)
  )

# Get environment
get_environ <- function(in_data, offset) {
  preceeding <- getMatchAlignments(
    labbcat.url, 
    in_data$MatchId, 
    "segment",
    target.offset=offset, 
    anchor.confidence.min=50, #  automatically aligned following or preceeding
    include.match.ids = TRUE
  )
}

preceeding <- get_environ(matches, -1)
following <- get_environ(matches, 1)

matches <- bind_cols(
  matches,
  preceeding %>% select(-x),
  following %>% select(-x)
)

# Get additional useful layers.
layers <- getMatchLabels(
  labbcat.url, 
  matches$MatchId, 
  c(
    "orthography", 
    "stress", 
    "utterance articulation rate", 
    "utterance speech rate"
  )
)

matches <- bind_cols(matches, layers)

# Convert to tibble.
matches <- as_tibble(matches)

# Output
write_rds(matches, here('data', 'private_qb2_data.rds'))
