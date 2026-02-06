# Introduksjon til R

# Her leser du inn de pakken vi får bruk for i kurset
library(tidyverse)
library(haven)
library(summarytools)
library(expss)
library(gmodels)
library(car)
library(psych)
library(jtools)
library(sjPlot)
# Hvis du mangler noen av pakkene, så må du installere dem

# Hvis du har PC: Les inn datasettet du har hentet fra ESS fra din PC-katalog
# ess <- read_dta("C:/katalognavn/ESS11-subset.dta")
# Hvis du har Mac: Les inn datasettet du har hentet fra ESS fra din Mac-katalog
# ess <- read_dta("/Volumes/navn/katalognavn/ESS11-subset.dta")

# Lag en enkel tabell for variabelen polintr
table(ess$polintr)
# Lag en vakrere tabell med pakken summarytools
freq(ess$polintr)

# Hvordan presenterer vi den kontinuerlige aldersvariabelen agea?
freq(ess$agea)
# Kontinuerlige variabler bør heller presenteres med statistiske mål
descr(ess$agea)


# Klargjøring av data
freq(ess$happy)
ess$happy <- recode(ess$happy, "NA=8")
freq(ess$happy)

# Tabell med variabelen bosted (domicil)
freq(ess$domicil)
# Denne kan kodes om til den nye dummyen by, der 1=by og 0=landsbygd
ess$by <- recode(ess$domicil, "1:3=1; 4:5=0")
fre(ess$by)

# Tabell med kjønnsvariabelen gndr
freq(ess$gndr)
# Denne variabelen kan også kodes om til dummyen kvinne
ess$kvinne <- recode(ess$gndr, "1=0; 2=1")
freq(ess$kvinne)
fre(ess$kvinne)

# Tabell med partibarometer
freq(ess$prtvtcno)
# Partibarometeret kan gjøres om til en dummy for FrP
ess$frp <- recode(ess$prtvtcno, "1:7=0; 8=1; 9:11=0")
fre(ess$frp)

# Tabell for samlet husholdsinntekt i desiler
freq(ess$hinctnta)
# Slik kan du interpolere inntektsvariabelen fra ordinal- til forholdstallsnivå
ess$inntekt <- recode(ess$hinctnta, "1=148;2=358;3=475;4=583;5=692;6=808;7=930;8=1073;9=1288;10=1423")
descr(ess$inntekt)

# Men denne variabelen er nå kodet slik at den kan tolke gjennomsnittsverdien
descr(ess$inntekt)

# Lage en indeks for politisk tillit
freq(ess$trstprl)
freq(ess$trstplt)
freq(ess$trstprt)
matrise <- data.frame(ess$trstprl, ess$trstplt, ess$trstprt)
cor_matrix <- cor(matrise, use = "complete.obs")
print(cor_matrix)
# Her er alle korrelasjonene over 0,3 og vi kan slå de tre variablene sammen til en indeks
ess$poltillit <- (ess$trstprl+ess$trstplt+ess$trstprt)
freq(ess$poltillit)
hist(ess$poltillit)

# Nå kan du også bruke det lagrede objektet matrise til å lage objekter for Cronbachs alfa og for en faktoranalyse
# Cronbachs alfa
alfa <- alpha(matrise)
print(alfa)
# Faktoranalyse
faktor <- fa(matrise)
print(faktor)
# Også disse to analysene bekrefter at de tre variablene kan slås sammen til en indeks
ess$politisktillit <- (ess$trstprl+ess$trstplt+ess$trstprt)/3
freq(ess$politisktillit)
descr(ess$politisktillit)
hist(ess$politisktillit)

# Sammenheng mellom variablene
# Lage en bivariat krysstabell med politisk interesse fordelt etter kjønn
ctable(ess$polintr, ess$gndr, prop = "c", useNA = "no")
# En enklere tabell med kjikvadrattest
ctable(ess$polintr, ess$gndr, prop = "c", useNA = "no", chisq = TRUE)

# Lineær regresjon
# Bivariat regresjon
freq(ess$wrclmch)
freq(ess$eduyrs)
modell1 <- lm(wrclmch ~ eduyrs, data=ess)
summary(modell1)
summ(modell1)
# Hvis du vil vise resultatene med tre desimaler skriver du:
summ(modell1, digits=3)

# Multippel regresjon
modell2 <- lm(wrclmch ~ eduyrs+kvinne+inntekt+agea+by, data=ess)
summ(modell2, digits=3)

# I modell1 er N=1330 og i modell2 er N=1251. Vi må derfor lage et datasett med lik N i begge modellene?
utvalg <- subset(ess, select = c(wrclmch, eduyrs, kvinne, inntekt, agea, by))
utvalg=na.omit(utvalg)
modell1 <- lm(wrclmch ~ eduyrs, data=utvalg)
summ(modell1, digits=3)
modell2 <- lm(wrclmch ~ eduyrs+kvinne+inntekt+agea+by, data=utvalg)
summ(modell2, digits=3)

# Regresjonsmodeller med dummysett
ess$bosted <- as.factor(ess$domicil)
modell3 <- lm(wrclmch ~ bosted, data=ess)
summ(modell3)
# Hvis du vil endre referansekategori fra 1 til 5 kan du skrive:
ess$bosted2 <- relevel(ess$bosted, ref=5)
modell3b <- lm(wrclmch ~ bosted2, data=ess)
summ(modell3b, digits=3)

# Kurvelegresjonsmodeller med andregradsledd
# Ny avhengig variabel
freq(ess$imbgeco)
modell4 <- lm(imbgeco ~ agea+I(agea^2), data=ess)
summ(modell4, digits=5)
# Grafisk fremstilling av kurvelineariteten
effect_plot(modell4, pred = agea)

# Regresjonsmodell med samspillseffekt
# Omkoding av variablene homoaksept og storby
freq(ess$freehms)
ess$homoaksept <- recode(ess$freehms, "1=5; 2=4; 3=3; 4=2; 5=1")
ess$storby <- recode(ess$domicil, "1:2=1; 3:5=0")
# Estimering av lineær modell uten samspill
modell5 <- lm(homoaksept ~ agea+storby, data=ess)
summ (modell5, digit=3)
# Estimering av lineær modell med samspill
modell6 <- lm(homoaksept ~ agea+storby+agea:storby, data=ess)
summ (modell6, digit=3)
# Grafisk fremstilling av samspillet
plot_model(modell6, type = "pred", terms = c("agea", "storby"))

# logistisk regresjon
# Bivariate logistisk regresjonsmodell
modell_logistisk1 <- glm(frp ~ eduyrs, data = ess, family = binomial)
summ(modell_logistisk1, digit=3)
# Oddsratio
oddsratio1 <- exp(coef(modell_logistisk1))
print(oddsratio1)

# Utdanningseffekten vist grafisk som sannsynligheter
effect_plot(modell_logistisk1, pred = eduyrs)

# Multippel logistisk regresjonsmodell
modell_logistisk2 <- glm(frp ~ eduyrs + kvinne + inntekt + agea + by, data = ess, family = binomial)
summ(modell_logistisk2, digit=3)
# Oddsratio
oddsratio2 <- exp(coef(modell_logistisk2))
print(oddsratio2)
# Utdanningseffekten kontrollert for andre variabler vist grafisk som sannsynligheter
effect_plot(modell_logistisk2, pred = eduyrs)

# Estimerer den logistiske regresjonsmodellen med kurvelineær utddaningseffekt
modell_logistisk3 <- glm(frp ~ eduyrs + I(eduyrs^2) + kvinne + inntekt + agea + by, data = ess, family = binomial)
summ(modell_logistisk3, digits=3)

# Plotte den kurvelineære effekten av eduyrs på frp
effect_plot(modell_logistisk3, pred = eduyrs)

# AME-analyse av modell_logistisk2
summary(margins(modell_logistisk3))
