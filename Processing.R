##########
# Project: Analyzing early online support for Justin Amash
# Contributor: Suzie Mulesky
# Date: May 8, 2020
#
# Task: data cleaning/processing
##########

# Load packages
library(dplyr)
library(tidyr)

# Import data
df <- read.csv("Amash - Survey Questionnaire_May 8, 2020_10.43.csv")

# Delete first two rows
df <- df[-(1:2),]

# Delete empty columns
df <- df[, -(10:13)]

# Delete survey previews and those who didn't finish
df <- df[df$Status != "Survey Preview", ]
df <- df[df$Finished == "True", ]

# Function to handle factor-to-numeric conversions
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# Change factor variables to numeric
names(df)
df[,c(6, 17:21)] <- sapply(df[ ,c(6, 17:21)], as.numeric.factor)

# Separate responses from Q3 into two variables
df <- df %>%
  separate(Q3, c("Q3_1", "Q3_2"), ",")

# Adjust factor levels
levels(df$Q1)
df$Q1 <- droplevels(df$Q1)

levels(df$Q2)
df$Q2 <- droplevels(df$Q2)

df$Q3_1 <- as.factor(df$Q3_1)
levels(df$Q3_1)

df$Q3_2 <- as.factor(df$Q3_2)
levels(df$Q3_2)

df$Q4_1 <- as.factor(df$Q4_1)
df$Q4_2 <- as.factor(df$Q4_2)
df$Q4_3 <- as.factor(df$Q4_3)
df$Q4_4 <- as.factor(df$Q4_4)
df$Q4_5 <- as.factor(df$Q4_5)

# Save
write.csv(df, file = "amash.csv", row.names = FALSE)


