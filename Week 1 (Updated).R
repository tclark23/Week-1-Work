# Week 1 - Adding to my repo

# Setting up the editor
library(tidyverse)
setwd("C:/Users/t_cla/OneDrive/Summer Research")

# Load in data and subset it, as instructed
DataSubjects21 <- read_csv("DataSubjects21.csv")

subset <- DataSubjects21 %>%
  filter(`Session ID` == 1) %>%
  select(`DARI ID`, `Subject ID`, `Shoulder External Rotation Max Left`)

# Now, let's look at the psych package
install.packages("psych")
library(psych)
?ICC()

# Reformatting the data
subset_mod <- subset %>%
  arrange(`DARI ID`, `Subject ID`)

subset_1 <- subset_mod[1:21, ]
colnames(subset_1) <- c("DARI ID", "Subject ID", "Shoulder External Rotation Max Left_1")
subset_1 <- subset_1 %>%
  select(`Subject ID`, `Shoulder External Rotation Max Left_1`)

subset_2 <- subset_mod[22: nrow(subset_mod), ]
colnames(subset_2) <- c("DARI ID", "Subject ID", "Shoulder External Rotation Max Left_2")
subset_2 <- subset_2 %>%
  select(`Subject ID`, `Shoulder External Rotation Max Left_2`)

final_df <- merge(subset_1, subset_2, by = "Subject ID") %>%
  select(`Shoulder External Rotation Max Left_1`, `Shoulder External Rotation Max Left_2`)

# Obtaining the ICC
ICC(final_df, lmer = FALSE)

# If the DARI motion capture system was randomly chosen out of commercially
# available markerless motion capture systems, and we plan to generalize the
# reliability study results to all systems with the same characteristics, then
# it would be classified as a Two-Way Random-Effects Model.
# On the other hand, if the results were only meant to be representative of the
# specific raters (i.e. the DARI system) used in the experiment, then we would
# classify it as a Two-Way Mixed-Model.
# In this study, the researchers were testing for absolute agreement between the 
# two DARI systems based on a "single rater" measurement.
# Regardless of whether we would classify this study as random-effects
# or mixed-effects, the corresponding ICC is ICC(2,1), which in the output is 
# labeled as ICC2.

# Therefore, the ICC for our inter-rater reliability study for this specific metric
# in session 1 was 0.96, and the 95% confidence interval is given by (0.91, 0.99).

################################################################################
# Let's try this a simpler way, using pivot_wider()
library(tidyverse)
library(psych)
setwd("C:/Users/t_cla/OneDrive/Summer Research")

DataSubjects21 <- read_csv("DataSubjects21.csv")

new_subset <- DataSubjects21 %>%
  filter(`Session ID` == 1) %>%
  select(`DARI ID`, `Subject ID`, `Shoulder External Rotation Max Left`)

new_final_df <- new_subset %>%
  pivot_wider(names_from = `DARI ID`, values_from = `Shoulder External Rotation Max Left`) %>%
  select("1", "2")

colnames(new_final_df) <- c("DARI 1", "DARI 2")

ICC(new_final_df, lmer = FALSE)

# This gives us the same result with a lot fewer steps.