library(tidyverse)
library(dataforsocialscience)
library(psych)

df <- robo_care

## Übung 1: Boxplot und Histogram ----

meine_variable <- df$privacy_concerns
hist(meine_variable)
hist(meine_variable, breaks = 19)

## Übung 2: Dichtefunktion ----

plot(density(meine_variable))
plot(density(meine_z_standardisierte_variable))

## Übung 3: Dichtefunktion mehrerer Variablen ----

plot(density(meine_variable), col = "red")
lines(density(df$technical_knowledge), col = "blue")
lines(density(df$cse), col = "green")

## Übung 4: Binomialverteilung, Poissonverteilung, Normalverteilung ----

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

## Übung 5: Z-Transformation ----

meine_z_standardisierte_variable <- (meine_variable - mean(meine_variable)) / sd(meine_variable)

hist(meine_variable)
hist(meine_z_standardisierte_variable)

## Übung 6: Visueller Vergleich Variable und Normalverteilung ----

plot(density(meine_variable), col = "red")
lines(density(mynorm), col = "blue")

## Übung 7: Binärer Geschlechtervergleich im Densityplot ----

df_male <- filter(df, df$gender == "male")
df_female <- filter(df, df$gender == "female")

plot(density(df_male$cse), col = "blue")
lines(density(df_female$cse), col = "red")


## Übung 8: Flugzeuge ----

# Show-Up Wahrscheinlichkeit der Passagiere sei 97%
# Sie verkaufen immer 165 Tickets; haben aber nur 160 Plätze im Flugzeug 
# Wir simulieren für N=100 Flüge und die Anzahl der Passagiere am Gate
passengers <- rbinom(n = 100, size = 165, prob = 0.97)
passengers        # Passagiere am Gate in unsere 100 Simulationen
table(passengers) # Häufigkeitstabelle – Was fällt auf? => Wenig richtige Ausreißer

ueberbuchung <- ifelse(passengers > 160, passengers - 160, 0)
ueberbuchung        # Wie viele Plätze sind im jeweiligen Flug überbucht worden?
table(ueberbuchung) # Häufigkeitstabelle – Was fällt auf? => Wenig richtige Ausreißer
100*165							 # Verkaufte Tickets insgesamt
sum(passengers)     # Gesamtzahl der Passagiere am Gate (weniger als verkaufte Tickets!)
sum(ueberbuchung)   # Anzahl an Überbuchungen
sum(ueberbuchung)*250 # und Strafzahlung nötig: 250€ pro Fall

# Wie können wir das visuell darstellen?
barplot(tosses, main="1000x 11 Münzwürfe")
# Ufts. Da kann man ja nichts mit anfangen... Warum?

# Wie können wir das sinnvoller darstellen? Als Histogram!
hist(tosses, breaks=4)

# Huch? Zuwenig Bins im Histogramm? 
hist(tosses, breaks=10, xlim=c(0,10))

# Wahrscheinlichkeit statt Häufigkeit? freq=FALSE
hist(tosses, breaks=10, xlim=c(0,10), freq=FALSE)

# Farbe und Titel dran!
hist(tosses, main="1000x 11 Würfe einer fairen Münze", breaks=11, xlim=c(0,10), col="chocolate")


