    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(ggthemes)
    library(AachenColorPalette)

    df <- readRDS("data/data.rds")

# Teammitglieder

-   Emilia
-   Monique
-   Zehra
-   Elcin-Havva Konar

# Forschungsfrage

- Welche Faktoren beeinflussen die Akzeptanz und die
Privatsphären-Wahrnehmung bei der Nutzung eines KI-Chatbots in der
Kommunalverwaltung zur Unterstützung bei Formularprozessen?

# Faktorenraum

<figure>
<img src="Readme_files/Faktorenraum.jpeg"
alt="Faktorenraum" />
<figcaption aria-hidden="true">Faktorenraum</figcaption>
</figure>


# Operationalisierung

- **Technikaffinität:** Affinity towards technology interaction (ATI) nach
[Franke et
al. (2019)](10.1080/10447318.2018.1456150 "Franke, T., Attig, C., & Wessel, D. (2019). A Personal Resource for Technology Interaction: Development and Validation of the Affinity for Technology Interaction (ATI) Scale. International Journal of Human–Computer Interaction, 35(6), 456-467, DOI: 10.1080/10447318.2018.1456150")

- **Alter:** Wie alt sind Sie? (in Jahren)

- **Bildungsabschluss:** Was ist ihr höchster Bildungsabschluss? ((Noch) kein
Schulabschluss Hauptschulabschluss Realschulabschluss Abitur Höher als
Abitur)

- **Nutzungsintention:** User Acceptance of Information Technology: Toward a
Unified View (UTAUT) Venkatesh, V., Morris, M. G., Davis, G. B., &
Davis, F. D. (2003). User Acceptance of Information Technology: Toward a
Unified View. MIS Quarterly, 27(3), 425–478.
<https://doi.org/10.2307/30036540>

- **Vertrauen:** Trust\_in\_Automation\_Questionnaire (TiA):
<https://github.com/moritzkoerber/TiA_Trust_in_Automation_Questionnaire/blob/master/Vertrauen-in-Automation_TiA_Fragebogen.pdf>

- **Nutzung Digitaler Medien:** (Häufigkeit der Nutzung) Wie oft nutzen Sie
digitale Medien? (nie, einmal im Monat, mehrmals im Monat, einmal pro
Woche, mehrmals in der Woche, täglich)

- **Datenschutzbedenken:** Development of measures of online privacy concern
and protection for use on the Internet (IUIPC); (Buchanan, Tom &
Joinson, Adam & Paine Schofield, Carina & Reips, Ulf-Dietrich. (2007).
Development of measures of online privacy concern and protection for use
on the Internet. Journal of the American Society for Information Science
and Technology. <http://dx.doi.org/10.1002/asi.20459>)

# Hypothesen

Einfache Zusammenhangshypothesen:

- **H1:** Es gibt einen positiven Zusammenhang zwischen der Technikaffinität
und digitaler Mediennutzung (Spearman-Korrelation: Zwischen
Technikaffinität UV und Digitale Mediennutzung AV)

    cor.test(df$ATI,as.numeric(df$Mn), method = "spearman")

    ## Warning in cor.test.default(df$ATI, as.numeric(df$Mn), method = "spearman"):
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  df$ATI and as.numeric(df$Mn)
    ## S = 55998485, p-value = 0.5894
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## 0.02043267

- **H2:** Je höher das Alter der Nutzenden, desto höher die
Privatsphäredisposition. (Pearson-Korrelation: Zwischen Alter UV und
Privatsphäredisposition AV)

    cor.test(df$Age,df$PD, method = "pearson")

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$Age and df$PD
    ## t = -1.6285, df = 698, p-value = 0.1039
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.13501110  0.01263667
    ## sample estimates:
    ##         cor 
    ## -0.06152378

- **H3:** Je höher das Vertrauen in den KI-Chatbot, desto niedrieger sind die
Datenschutzbedenken. (Pearson-Korrelation: Zwischen Vertrauen in die
Technologie UV und Datenschutzbedenken AV)

    cor.test(df$NZV,df$PW, method = "pearson")

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  df$NZV and df$PW
    ## t = -0.26674, df = 698, p-value = 0.7897
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.08413555  0.06405511
    ## sample estimates:
    ##         cor 
    ## -0.01009565

Komplexe Zusammenhangshypothesen:

- **H4:** Je höher die usability und die wahrgenommene Privatsphäredisposition
des Chatbots sind, desto positiver ist die Privatsphären-Wahrnehmung.
(Multiple lineare Regression: Zwischen usability UV1 sowie
Privatsphäredisposition UV2 und Privatsphären-Wahrnehmung AV)

Einfache Unterschiedshypothesen:

- **H5:** Nutzende mit höherem Bildungsabschluss haben eine höhere
Technikaffinität als Nutzende mit niedrigem Bildungsabschluss.
(Unverbundener T-Test: Zwischen Bildungsniveau UV und Technikaffinität
AV)

    t.test( filter(df, Bildungsabschluss > "Abitur")$ATI , filter(df, Bildungsabschluss < "Abitur")$ATI )

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df, Bildungsabschluss > "Abitur")$ATI and filter(df, Bildungsabschluss < "Abitur")$ATI
    ## t = -0.73326, df = 210.03, p-value = 0.4642
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.15667915  0.07172207
    ## sample estimates:
    ## mean of x mean of y 
    ##  3.528192  3.570671

- **H6:** Frauen haben eine höheres Empfinden der Privatsphäre bei der Nutzung
eines KI-Chatbots als Männer. (Unverbundener T-Test: Zwischen Geschlecht
(UV) und wahrgenommener Privatsphäre (AV)

    t.test( filter(df, Gender == "Weiblich")$PW , filter(df, Gender == "Männlich")$PW )

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  filter(df, Gender == "Weiblich")$PW and filter(df, Gender == "Männlich")$PW
    ## t = 0.82049, df = 343.45, p-value = 0.4125
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.09599944  0.23341334
    ## sample estimates:
    ## mean of x mean of y 
    ##  3.487654  3.418947

- **H7:** Die Nutzungsintention der Stichprobe, gemessen auf einer Skala von
1-6, ist höher als 3. (Einfacher T-Test: Nutzungsintention (AV))

    t.test(df$BIATT,mu=3)

    ## 
    ##  One Sample t-test
    ## 
    ## data:  df$BIATT
    ## t = 20.212, df = 699, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 3
    ## 95 percent confidence interval:
    ##  3.478302 3.581222
    ## sample estimates:
    ## mean of x 
    ##  3.529762

Komplexe Unterschiedshypothesen:

- **H8:** Jüngere und technikaffine Nutzende haben eine höhere
Nutzungsintention und eine positivere Privatsphären-Wahrnehmung des
Chatbots als ältere und technikavers Nutzende. (F-Test MANOVA: Zwischen
Alter UV1 & Technikaffinität UV2 und Nutzungsintention AV1 &
wahrgenommene Privatsphäre und Datensicherheit AV2)
