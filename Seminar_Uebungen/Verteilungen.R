library(tidyverse)
library(dataforsocialscience)
library(psych)

df <- robo_care

## Übung 1: Boxplot und Histogram ----

meine_variable <- df$privacy_concerns
hist(meine_variable)
hist(meine_variable, breaks = 19)

## Übung 2: Z-Transformation ----

meine_z_standardisierte_variable <- (meine_variable - mean(meine_variable)) / sd(meine_variable)

hist(meine_variable)
hist(meine_z_standardisierte_variable)

## Übung 3: Dichtefunktion ----

plot(density(meine_variable))
plot(density(meine_z_standardisierte_variable))

## Übung 4: Dichtefunktion mehrerer Variablen ----

plot(density(meine_variable), col = "red")
lines(density(df$technical_knowledge), col = "blue")
lines(density(df$cse), col = "green")

## Übung 5: Binomialverteilung, Poissonverteilung, Normalverteilung ----

mybinom <- rbinom( n = 293, size = 10, prob = 0.5)
mybinom
hist(mybinom)

mypoiss <- rpois( n = 293, lambda = 3)
mypoiss
hist(mypoiss)

mynorm <- rnorm( n = 293, mean = mean(meine_variable), sd = sd(meine_variable))
mynorm
hist(meine_variable)
hist(mynorm)

## Übung 6: Visueller Vergleich Variable und Normalverteilung ----

plot(density(meine_variable), col = "red")
lines(density(mynorm), col = "blue")

## Übung 7: Binärer Geschlechtervergleich im Densityplot ----

df_male <- filter(df, df$gender == "male")
df_female <- filter(df, df$gender == "female")

plot(density(df_male$cse), col = "blue")
lines(density(df_female$cse), col = "red")
