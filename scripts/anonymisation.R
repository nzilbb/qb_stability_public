# Script to anonymise data. This requires correspondence tables which are, for
# obvious reasons, not made public.

library(tidyverse)
library(glue)
library(here)

QB1 <- read_rds(here('data', 'private_qb1_data.rds'))
QB2 <- read_rds(here('data', "private_qb2_data.rds"))

shared_speakers_count <- length(
  unique(
    QB1$Participant[QB1$Participant %in% QB2$Participant]
  )
)
#103

length(unique(QB2$Participant))
# 109

# Who are the 6 missing?

unique(QB2$Participant[!QB2$Participant %in% QB1$Participant])

# Remove speakers who did not grow up in NZ
QB1 <- QB1 %>%
  filter(
    participant_grew_up != "other",
    participant_grew_up != "",
    !is.na(participant_grew_up)
  )

QB2 <- QB2 %>%
  filter(
    participant_grew_up != "other",
    participant_grew_up != "",
    !is.na(participant_grew_up)
  )

# Data loss: 
# QB1: 707287 -> 511649
# QB2: 231078 -> 149496

# Speakers?
shared_speakers_count <- length(
  unique(
    QB1$Participant[QB1$Participant %in% QB2$Participant]
  )
)
# 73

length(unique(QB2$Participant))
# 74 speakers

# difference?
unique(QB2$Participant[!QB2$Participant %in% QB1$Participant])
# One left.

# Anonymise speakers
correspondence_table <- read_rds(
  here('Data', 'private_correspondence_table.rds')
)

# Add anonymous speaker codes to data:
QB1 <- QB1 %>%
  mutate(
    code = str_extract(Participant, '[A-Z]+[0-9]+')
  ) %>%
  left_join(
    correspondence_table %>%
      mutate(
        code = str_extract(speaker, '[A-Z]+[0-9]+')
      ) %>%
      select(
        code, anonymised
      ),
    by = "code"
  )

# Any missing data in the anonymised column?
QB1 %>%
  filter(
    is.na(anonymised)
  )
# Nope!

# Add anonymous speaker codes to data:
QB2 <- QB2 %>%
  mutate(
    code = str_extract(Participant, '[A-Z]+[0-9]+')
  ) %>%
  left_join(
    correspondence_table %>%
      mutate(
        code = str_extract(speaker, '[A-Z]+[0-9]+')
      ) %>%
      select(
        code, anonymised
      ),
    by = "code"
  )

# Any missing data in the anonymised column?
QB2 %>%
  filter(
    is.na(anonymised)
  )
# Nope!


# Stopword removal:
stopwords <- c(
  'a', 'ah', 'ahh', 'am', 'an', 'and', 'are', "aren't", 'as', 'at',
  'aw', 'because', 'but', 'could', 'do', "don't", 'eh', 'for', 'from', 'gonna',
  'had', 'has', 'have', 'he', "he's", 'her', 'high', 'him', 'huh', 'i', "i'll",
  "i'm", "i've", "i'd", 'in', 'into', 'is', 'it', "it's", 'its', 'just', 'mean',
  'my', 'nah', 'not', 'of', 'oh', 'on', 'or', 'our', 'says', 'she', "she's",
  'should', 'so', 'than', 'that', "that's", 'the', 'them', 'there', "there's",
  'they', 'this', 'to', 'uh', 'um', 'up', 'was', "wasn't", 'we', 'were', 'what',
  'when', 'which', 'who', 'with', 'would', 'yeah', 'you', "you've")


word_filter <- function(in_data) {
  in_data %>%
    rename(word = orthography) %>%
    filter(
      !grepl("~", word), #filter tokens with hesitations
      !is.na(word), #filter tokens which do not have the word transcribed
      !word %in% stopwords # filter stopwords.
    )
}

QB1 <- word_filter(QB1)
# Data loss: 511649 -> 275001

QB2 <- word_filter(QB2)
# Data loss: 149496 -> 96622


# Anonymise words
word_correspondence_table <- read_rds(
  here('Data', 'private_word_correspondence_table.rds')
)

# Join word anonymisation codes.
QB1 <- QB1 %>%
  left_join(
    word_correspondence_table %>%
      rename(word_anon = anonymised),
    by = "word"
  )

QB2 <- QB2 %>%
  left_join(
    word_correspondence_table %>%
      rename(word_anon = anonymised),
    by = "word"
  )

desired_columns <- c(
  "anonymised", "participant_gender", "participant_age_category",
  "participant_nz_ethnic", 
  "participant_grew_up",
  "participant_grew_up_region", 
  "participant_sd_vowel_length", "participant_duration", 
  "participant_word count", "transcript_duration",
  "MatchId", "word_anon", "Target.segment",
  "Target.segment.start", "Target.segment.end", 
  "stress", "time_0_5", "Token.minus.1.segment",
  "Token.plus.1.segment", "utterance.articulation.rate", "utterance.speech.rate",
  "f1_time_0_5", "f2_time_0_5"
)


QB1_out <- QB1 %>%
  select(
    any_of(desired_columns)
  ) %>%
  # We'll rename the variables as well to reduce tidying in the script.
  rename(
    Speaker = anonymised,
    Word = word_anon,
    Gender = participant_gender,
    Age = participant_age_category,
    Ethnicity = participant_nz_ethnic,
    VowelDISC = Target.segment,
    VowelStart = Target.segment.start,
    VowelEnd = Target.segment.end,
    VowelMid = time_0_5,
    previous_segment = Token.minus.1.segment,
    following_segment = Token.plus.1.segment,
    articulation_rate = utterance.articulation.rate,
    speech_rate = utterance.speech.rate,
    F1_50 = `f1_time_0_5`,
    F2_50 = `f2_time_0_5`
  )

QB2_out <- QB2 %>%
  select(
    any_of(desired_columns)
  ) %>%
  # We'll rename the variables as well to reduce tidying in the script.
  rename(
    Speaker = anonymised,
    Word = word_anon,
    Gender = participant_gender,
    Age = participant_age_category,
    Ethnicity = participant_nz_ethnic,
    VowelDISC = Target.segment,
    VowelStart = Target.segment.start,
    VowelEnd = Target.segment.end,
    VowelMid = time_0_5,
    previous_segment = Token.minus.1.segment,
    following_segment = Token.plus.1.segment,
    articulation_rate = utterance.articulation.rate,
    speech_rate = utterance.speech.rate,
    F1_50 = `f1_time_0_5`,
    F2_50 = `f2_time_0_5`
  )

# Speakers at this stage.
shared_speakers_count <- length(
  unique(
    QB1$Participant[QB1$Participant %in% QB2$Participant]
  )
)
# Still 73, as expected.

# Export data
write_rds(QB1_out, here("Data", "QB1_anon.rds"), compress="gz")
write_rds(QB2_out, here("Data", "QB2_anon.rds"), compress="gz")
