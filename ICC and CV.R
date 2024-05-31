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

################################################################################
# Coefficient of variation function: end goal is one function that takes a 
# dataframe like DataSubjects21, transforms it, and spits out the coefficient of 
# variation for an inter or intra reliability study based on specified DARI/session
# number and measurement variable.

# First let's make a dataframe transformation function for an inter-rater 
# reliability study, that takes a df like DataSubjects21 and produces a df in the
# same format as needed for the ICC function.

df_inter_transform <- function(df, session, variable) {
  new_subset <- df %>%
    filter(`Session ID` == session) %>%
    select(`DARI ID`, `Subject ID`, all_of(variable))
  
  new_final_df <- new_subset %>%
    pivot_wider(names_from = `DARI ID`, values_from = variable) %>%
    select("1", "2")
  
  colnames(new_final_df) <- c("DARI 1", "DARI 2")
  
  return(new_final_df)
}


# Example of how we could use this:
ICC(df_inter_transform(DataSubjects21, session = "1", variable = "Shoulder External Rotation Max Left"), lmer = FALSE)


# Now, we can create a function that calculates the coefficient of variation for
# an inter-rater reliability study, based on the specified session number and 
# variable name.
cv_inter <- function(df, session, variable) {
  new_df <- df_inter_transform(df, session, variable)
  
  coefs <- c()
  for (i in 1:nrow(new_df)) {
    sdev <- sd(as.numeric(new_df[i, ]))
    avg <- mean(as.numeric(new_df[i, ]))
    cov <- sdev*100/avg
    coefs <- append(coefs, cov)
  }
  return(mean(coefs))
}

# For example:
cv_inter(DataSubjects21, "1", "Shoulder External Rotation Max Left")

# Now, let's make a function to reformat the data for an INTRA-rater reliability 
# study instead, based on DARI ID and variable name.
df_intra_transform <- function(df, DARI, variable) {
  new_subset <- df %>%
    filter(`DARI ID` == DARI) %>%
    select(`Session ID`, `Subject ID`, all_of(variable))
  
  new_final_df <- new_subset %>%
    pivot_wider(names_from = `Session ID`, values_from = variable) %>%
    select("1", "2")
  
  colnames(new_final_df) <- c("Session 1", "Session 2")
  
  return(new_final_df)
}

# An example of how we would use THIS function:
ICC(df_intra_transform(DataSubjects21, "2", "Shoulder External Rotation Max Left"), lmer = FALSE)

# Now, we can create a function that calculates the coefficient of variation for
# an INTRA-rater reliability study, dependent on the user's inputted DARI ID and
# variable name.
cv_intra <- function(df, DARI, variable) {
  new_df <- df_intra_transform(df, DARI, variable)
  
  coefs <- c()
  for (i in 1:nrow(new_df)) {
    sdev <- sd(as.numeric(new_df[i, ]))
    avg <- mean(as.numeric(new_df[i, ]))
    cov <- sdev*100/avg
    coefs <- append(coefs, cov)
  }
  return(mean(coefs))
}

# Let's use it to calculate a CV:
cv_intra(DataSubjects21, "2", "Shoulder External Rotation Max Left")

################################################################################
# Finally, we make one general CV function, in which the user specifies the data
# frame, the type of study (inter or intra), a value 1 or 2 (representing either
# the DARI ID or the Session ID, depending on the type of study), and a variable 
# name.

general_cv <- function(df, type, value, variable) {
  if (type == "inter") {
    return(cv_inter(df, value, variable))
  }
  else if (type == "intra") {
    return(cv_intra(df, value, variable))
  }
}

# Let's try it out:
general_cv(DataSubjects21, "inter", "1", "Shoulder External Rotation Max Left")
general_cv(DataSubjects21, "intra", "2", "Knee Flexion Max Overhead Squat Right")
