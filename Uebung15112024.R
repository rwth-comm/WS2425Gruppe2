# Pakete installieren

# Pakete aktivieren
library(tidyverse)
library(psych)
library(plotrix)
library(dataforsocialscience)

# Daten einlesen
df <- robo_care
table(df$gender)
qplot(df$gender)
median(df$age)
quantile(df$age)
boxplot(df$age)
table(df$job_type)

describe(df)
