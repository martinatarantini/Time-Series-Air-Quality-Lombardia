#ESAME INTRODUZIONE ALLE SERIE STORICHE TARANTINI MARTINA

# 1. Preparazione dataset -------------------------------------------------

### Gestione delle date
library(lubridate)  # https://raw.githubusercontent.com/rstudio/cheatsheets/main/lubridate.pdf
library(tsbox)      # https://cran.r-project.org/web/packages/tsbox/vignettes/tsbox.html
### TS modelling from Forecasting Principles and Practice (R.J. Hyndman)
library(forecast)
library(tsibble)
library(tseries)
### Tidyverse
library(tidyverse)
### Tidyverse plot
library(ggplot2)
library(ggpubr)

### Funzioni ausiliarie
source('G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/FN - Sequence of dates.R', encoding = 'UTF-8')
source('G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/FN - PerfMetr_SARIMA.R', encoding = 'UTF-8')
source('G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/FN - AutoPortmanteauQ.R', encoding = 'UTF-8')
source('G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/FN - ARCHTest.R', encoding = 'UTF-8')
source('G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/FN - KSmoothGCV e LOESSGCV.R', encoding = 'UTF-8')
source('G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/FN - minmax_rescale.R', encoding = 'UTF-8')
source('G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/FN - TS_custom_aggregate.R', encoding = 'UTF-8')

rm(list = setdiff(ls(), lsf.str()))

### Working directory 
setwd("G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/Progetto")

#Carico dataset
Dataset <- read_csv("G:/Altri computer/Il mio laptop/Magistrale/Introduzione alle Serie Storiche (1)/Progetto/Data1_AQ_W_mensile_2016_2022.txt")

#Ribadisco che la colonna "Data" deve essere di tipo Date, rinomino le centraline di mio interesse con nomi più rapidi e le seleziono.
#Seleziono anche solo le variabili di mio interesse, che sono NO2 e PM10, oltre al nome della stazione e alla data.
Dati <- Dataset %>%
  mutate(Data = yearmonth(ymd(Data)),
         Nome_staz = case_when(Nome_staz == "Ferno Via Di Dio" ~ "Malpensa",
                               Nome_staz == "Bergamo Via Meucci"~ "Bergamo",
                               TRUE ~ Nome_staz)) %>%
  filter(Nome_staz %in% c("Malpensa", "Bergamo")) %>%
  select(Data, Nome_staz, NO2, PM10) %>%
  as_tsibble(index = Data, key = Nome_staz)

#Trasformo i dati in una serie storica
y <- Dati %>%
  ts_ts()
str(y)
y
#Ho 4 serie storiche: due riguardanti i valori di NO2 e PM10 di Bergamo e due riguardanti i valori di NO2 e PM10 di Malpensa.


# 2. Analisi esplorativa --------------------------------------------------


# 2.1. EDA: Grafico serie storiche ----------------------------------------

#Rappresentazione dell'inquinante NO2:
NO2 <- Dati %>%
  select(NO2) %>%
  ts_ts()
#Sono due serie storiche: una è il valore di NO2 a Bergamo, l'altra di NO2 a Malpensa.

autoplot(NO2, facets = FALSE) + labs(title = "Concentrazione di NO2: Malpensa Aeroporto vs Bergamo Città", subtitle = "Dati ARPA", x = "Anno", y = "NO2 (µg/m³)", color = "Stazione")

autoplot(NO2, facets = TRUE) + labs(title = "Concentrazione di NO2: Malpensa Aeroporto vs Bergamo Città", subtitle = "Dati ARPA", x = "Anno", y = "NO2 (µg/m³)")


#Serie storiche singole
NO2_Bergamo <- Dati %>%
  filter(Nome_staz == "Bergamo") %>%
  select(NO2) 

NO2_Bergamo_ts <- NO2_Bergamo %>%
  ts_ts()

y1 <- autoplot(NO2_Bergamo_ts) + labs(title = "Concentrazione di NO2 a Bergamo Città", subtitle = "Dati ARPA", y = "NO2 (µg/m³)", x = "Anno")
#Serie storica dell'inquinante NO2 a Bergamo città.

NO2_Malpensa <- Dati %>%
  filter(Nome_staz == "Malpensa") %>%
  select(NO2) 

NO2_Malpensa_ts <- NO2_Malpensa %>%
  ts_ts()

y2 <- autoplot(NO2_Malpensa_ts) + labs(title = "Concentrazione di NO2 a Malpensa Aeroporto", subtitle = "Dati ARPA", y = "NO2 (µg/m³)", x = "Anno")
#Serie storica dell'inquinante NO2 a Milano Malpensa.

#Livelli medi NO2 per anno
Media_NO2_Bergamo <- Dati %>%
  filter(Nome_staz == "Bergamo") %>%
  select(c(Data, NO2)) %>%
  group_by(year(Data)) %>%
  mutate(media_annua = mean(NO2, na.rm = T))

g1 <- ggplot(data = Media_NO2_Bergamo, mapping = aes(x = Data)) + 
  geom_line(mapping = aes(y = NO2)) +
  geom_line(mapping = aes(y = media_annua), col = "red3", size = 1.1) + 
  labs(title = "Concentrazione di NO2 a Bergamo Città", subtitle = "Dati ARPA", x = "Anno", y = "NO2 (µg/m³)")
g1

Media_NO2_Malpensa <- Dati %>%
  filter(Nome_staz == "Malpensa") %>%
  select(c(Data, NO2)) %>%
  group_by(year(Data)) %>%
  mutate(media_annua = mean(NO2, na.rm = T))

g2 <- ggplot(data = Media_NO2_Malpensa, mapping = aes(x = Data)) + 
  geom_line(mapping = aes(y = NO2)) +
  geom_line(mapping = aes(y = media_annua), col = "cyan3", size = 1.1) + 
  labs(title = "Concentrazione di NO2 a Malpensa Aeroporto", subtitle = "Dati ARPA", x = "Anno", y = "NO2 (µg/m³)")
g2

ggarrange(g1, g2, ncol = 1, nrow = 2)

#Rappresentazione dell'inquinante PM10:
PM10 <- Dati %>%
  select(PM10) %>%
  ts_ts()

autoplot(PM10, facets = FALSE) + labs(title = "Concentrazione di PM10: Malpensa Aeroporto vs Bergamo Città", subtitle = "Dati ARPA", x = "Anno", y = "PM10 (µg/m³)", color = "Stazione")

autoplot(PM10, facets = TRUE) + labs(title = "Concentrazione di PM10: Malpensa Aeroporto vs Bergamo Città", subtitle = "Dati ARPA", x = "Anno", y = "PM10 (µg/m³)")

#Serie storiche singole
PM10_Bergamo <- Dati %>%
  filter(Nome_staz == "Bergamo") %>%
  select(PM10) 

PM10_Bergamo_ts <- PM10_Bergamo %>%
  ts_ts()

y3 <- autoplot(PM10_Bergamo_ts) + labs(title = "Concentrazione di PM10 a Bergamo Città", subtitle = "Dati ARPA", y = "PM10 (µg/m³)", x = "Anno")
#Serie storica dell'inquinante PM10 a Bergamo città.

PM10_Malpensa <- Dati %>%
  filter(Nome_staz == "Malpensa") %>%
  select(PM10) 

PM10_Malpensa_ts <- PM10_Malpensa %>%
  ts_ts()

y4 <- autoplot(PM10_Malpensa_ts) + labs(title = "Concentrazione di PM10 a Malpensa", subtitle = "Dati ARPA", y = "PM10 (µg/m³)", x = "Anno")
#Serie storica dell'inquinante PM10 a Milano Malpensa.

#Livelli medi PM10 per anno
Media_PM10_Bergamo <- Dati %>%
  filter(Nome_staz == "Bergamo") %>%
  select(c(Data, PM10)) %>%
  group_by(year(Data)) %>%
  mutate(media_annua = mean(PM10, na.rm = T))

g3 <- ggplot(data = Media_PM10_Bergamo, mapping = aes(x = Data)) + 
  geom_line(mapping = aes(y = PM10)) +
  geom_line(mapping = aes(y = media_annua), col = "red3", size = 1.1) + 
  labs(title = "Concentrazione di PM10 a Bergamo Città", subtitle = "Dati ARPA", x = "Data", y = "PM10 (µg/m³)")
g3

Media_PM10_Malpensa <- Dati %>%
  filter(Nome_staz == "Malpensa") %>%
  select(c(Data, PM10)) %>%
  group_by(year(Data)) %>%
  mutate(media_annua = mean(PM10, na.rm = T))

g4 <- ggplot(data = Media_PM10_Malpensa, mapping = aes(x = Data)) + 
  geom_line(mapping = aes(y = PM10)) +
  geom_line(mapping = aes(y = media_annua), col = "cyan3", size = 1.1) + 
  labs(title = "Concentrazione di PM10 a Malpensa Aeroporto", subtitle = "Dati ARPA", x = "Anno", y = "PM10 (µg/m³)")
g4

ggarrange(g3, g4, ncol = 1, nrow = 2)


# 2.2. EDA: Distribuzione dei dati (normalità) ----------------------------


# 2.2.1. Istogramma e Box-Plot --------------------------------------------

color <- c("Dens"="orange2", "Norm"="darkviolet")

#Variabile NO2:
#Bergamo
hist_NO2B <- ggplot(data = NO2_Bergamo, aes(x = NO2)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(NO2_Bergamo$NO2,na.rm=T),
                            sd = sd(NO2_Bergamo$NO2,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm
hist_NO2B
#Istogramma di NO2 a Bergamo Città con la densità kernel e la curva teorica normale

bp_NO2B <- ggplot(data = NO2_Bergamo, aes(x = NO2)) + 
  geom_boxplot(outlier.colour="red4",    #evidenziami in rosso gli outlier
               outlier.shape=8,     
               outlier.size=4,
               notch=FALSE) + 
  labs(title = "Box-plot",
       subtitle = "Concentrazione di NO2 a Bergamo Città",
       x = "NO2 (µg/m³)")
bp_NO2B
#Box-Plot di NO2 a Bergamo Città

ggarrange(hist_NO2B, bp_NO2B, nrow = 1, ncol = 2)

#Malpensa
hist_NO2M <-  ggplot(data = NO2_Malpensa, aes(x = NO2)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(NO2_Malpensa$NO2,na.rm=T),
                            sd = sd(NO2_Malpensa$NO2,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Milano Malpensa",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", 
                     values = color, 
                     breaks = c("Dens","Norm"), 
                     labels = c("KDE","Gaussiana"))
hist_NO2M
#Istogramma di NO2 a Milano Malpensa con la densità kernel e la curva teorica normale

bp_NO2M <- ggplot(data = NO2_Malpensa, aes(x = NO2)) + 
  geom_boxplot(outlier.colour="red4",
               outlier.shape=8,     
               outlier.size=4,
               notch=FALSE) + 
  labs(title = "Box-plot",
       subtitle = "Concentrazione di NO2 a Milano Malpensa",
       x = "NO2 (µg/m³)")
bp_NO2M
#Box-Plot di NO2 a Milano Malpensa

ggarrange(hist_NO2M, bp_NO2M, nrow = 1, ncol = 2)

#Variabile PM10:
#Bergamo
hist_PM10B <- ggplot(data = PM10_Bergamo, aes(x = PM10)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(PM10_Bergamo$PM10,na.rm=T),
                            sd = sd(PM10_Bergamo$PM10,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di PM10 a Bergamo Città",
       x = "PM10 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", 
                     values = color, 
                     breaks = c("Dens","Norm"), 
                     labels = c("KDE","Gaussiana")) 
hist_PM10B
#Istogramma della variabile PM10 a Bergamo Città.

bp_PM10B <- ggplot(data = PM10_Bergamo, aes(x = PM10)) + 
  geom_boxplot(outlier.colour="red4",   
               outlier.shape=8,     
               outlier.size=4,
               notch=FALSE) + 
  labs(title = "Box-plot",
       subtitle = "Concentrazione di PM10 a Bergamo Città",
       x = "PM10 (µg/m³)")
bp_PM10B
#Box-Plot della variabile PM10 a Bergamo Città.

ggarrange(hist_PM10B, bp_PM10B, nrow = 1, ncol = 2)

#Milano Malpensa
hist_PM10M <- ggplot(data = PM10_Malpensa, aes(x = PM10)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(PM10_Malpensa$PM10,na.rm=T),
                            sd = sd(PM10_Malpensa$PM10,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di PM10 a Milano Malpensa",
       x = "PM10 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", 
                     values = color, 
                     breaks = c("Dens","Norm"), 
                     labels = c("KDE","Gaussiana")) 
hist_PM10M
#Istogramma della variabile PM10 a Milano Malpensa.

bp_PM10M <- ggplot(data = PM10_Malpensa, aes(x = PM10)) + 
  geom_boxplot(outlier.colour="red4",    
               outlier.shape=8,     
               outlier.size=4,
               notch=FALSE) + 
  labs(title = "Box-plot",
       subtitle = "Concentrazione di PM10 a Milano Malpensa",
       x = "PM10 (µg/m³)")
bp_PM10M
#Box-Plot della variabile PM10 a Milano Malpensa.

ggarrange(hist_PM10M, bp_PM10M, nrow = 1, ncol = 2)


# 2.2.2. Test di normalità ------------------------------------------------
#Test di Bera-Jarque + Test di Shapiro-Wilk + Test di Kolmogorov-Smirnov
#H0: normalità dei dati
#H1: non normalità dei dati

library(tseries)

#Variabile NO2
#Bergamo
NO2_Bergamo %>% pull(NO2) %>% jarque.bera.test()
#P-Value = 0.04964 -> Rifiuto H0 al 5%

NO2_Bergamo %>% pull(NO2) %>% shapiro.test()
#P-value = 0.0002915 -> Rifiuto H0

NO2_Bergamo %>% pull(NO2) %>% ks.test(y = "pnorm")
#P-value < 2.2e-16 -> Rifiuto H0


#Malpensa
NO2_Malpensa %>% pull(NO2) %>% jarque.bera.test()
#P-value = 0.01871 -> Rifiuto H0 al 5%

NO2_Malpensa %>% pull(NO2) %>% shapiro.test()
#P-value = 6.686e-05 -> Rifiuto H0

NO2_Malpensa %>% pull(NO2) %>% ks.test(y = "pnorm")
#P-value < 2.2e-16 -> Rifiuto H0

#Variabile PM10:
#Bergamo
PM10_Bergamo %>% pull(PM10) %>% jarque.bera.test()
#P-value = 0.01975 -> Rifiuto H0 al 5%

PM10_Bergamo %>% pull(PM10) %>% shapiro.test()
#P-value = 5.978e-05 -> Rifiuto H0

PM10_Bergamo %>% pull(PM10) %>% ks.test(y = "pnorm")
#P-value = < 2.2e-16 -> Rifiuto H0


#Malpensa
PM10_Malpensa %>% pull(PM10) %>% jarque.bera.test()
#P-value = 0.00879 -> Rifiuto H0

PM10_Malpensa %>% pull(PM10) %>% shapiro.test()
#P-value = 5.333e-06 -> Rifiuto H0

PM10_Malpensa %>% pull(PM10) %>% ks.test(y = "pnorm")
#P-value = < 2.2e-16 -> Rifiuto H0


# 2.3. EDA: Analisi della persistenza -------------------------------------


# 2.3.1. Funzione di autocorrelazione e autocorrelazione parziale ---------

#NO2
#Bergamo
ACF_NO2B <- ggAcf(NO2_Bergamo_ts, lag.max = 36) + 
  labs(title = "ACF",
       subtitle = "Concentrazione di NO2 a Bergamo Città")
#Avendo dati mensili, conviene usare un lag.max pari a 36 mesi.

PACF_NO2B <- ggPacf(NO2_Bergamo_ts, lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Concentrazione di NO2 a Bergamo Città")

ggarrange(ACF_NO2B, PACF_NO2B, ncol = 2, nrow = 1)

#Malpensa
ACF_NO2M <- ggAcf(NO2_Malpensa_ts, lag.max = 36) + 
  labs(title = "ACF",
       subtitle = "Concentrazione di NO2 a Milano Malpensa")
#Avendo dati mensili, conviene usare un lag.max pari a 36 mesi.

PACF_NO2M <- ggPacf(NO2_Malpensa_ts, lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Concentrazione di NO2 a Milano Malpensa")

ggarrange(ACF_NO2M, PACF_NO2M, ncol = 2, nrow = 1)

#PM10
#Bergamo
ACF_PM10B <- ggAcf(PM10_Bergamo_ts, lag.max = 36) + 
  labs(title = "ACF",
       subtitle = "Concentrazione di PM10 a Bergamo Città")
#Avendo dati mensili, conviene usare un lag.max pari a 36 mesi.

PACF_PM10B <- ggPacf(PM10_Bergamo_ts, lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Concentrazione di PM10 a Bergamo Città")

ggarrange(ACF_PM10B, PACF_PM10B, ncol = 2, nrow = 1)

#Malpensa
ACF_PM10M <- ggAcf(PM10_Malpensa_ts, lag.max = 36) + 
  labs(title = "ACF",
       subtitle = "Concentrazione di PM10 a Milano Malpensa")
#Avendo dati mensili, conviene usare un lag.max pari a 36 mesi.

PACF_PM10M <- ggPacf(PM10_Malpensa_ts, lag.max = 36) + 
  labs(title = "PACF",
       subtitle = "Concentrazione di PM10 a Milano Malpensa")

ggarrange(ACF_PM10M, PACF_PM10M, ncol = 2, nrow = 1)


# 2.3.2. Lag plot ---------------------------------------------------------

#NO2
#Bergamo
lp_NO2B <- gglagplot(NO2_Bergamo_ts,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  labs(title = "Lag-Plot",
       subtitle = "Concentrazione di NO2 a Bergamo Città") 
lp_NO2B

#Malpensa
lp_NO2M <- gglagplot(NO2_Malpensa_ts,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  labs(title = "Lag-Plot",
       subtitle = "Concentrazione di NO2 a Milano Malpensa") 
lp_NO2M

#PM10
#Bergamo
lp_PM10B <- gglagplot(PM10_Bergamo_ts,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  labs(title = "Lag-Plot",
       subtitle = "Concentrazione di PM10 a Bergamo Città") 
lp_PM10B

#Malpensa
lp_PM10M <- gglagplot(PM10_Malpensa_ts,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  labs(title = "Lag-Plot",
       subtitle = "Concentrazione di PM10 a Milano Malpensa") 
lp_PM10M


# 2.3.3. Test di Portmanteau ----------------------------------------------

library(feasts)

#Test di Ljung-Box
#Variabile NO2
#Bergamo
ljung_box(x = NO2_Bergamo_ts, lag = 1) #test con k=1
ljung_box(x = NO2_Bergamo_ts, lag = 3) #test con k=3
ljung_box(x = NO2_Bergamo_ts, lag = 12) #test con k=12
#Rifiuto sempre

#Malpensa
ljung_box(x = NO2_Malpensa_ts, lag = 1) #test con k=1
ljung_box(x = NO2_Malpensa_ts, lag = 3) #test con k=3
ljung_box(x = NO2_Malpensa_ts, lag = 12) #test con k=12
#Rifiuto sempre

#Automatizzo Ljung-Box per tutte e 2 le centraline a vari lags, per NO2
lags <- c(1,2,3,6,12,18,24,36)
LBQ_NO2 <- matrix(data = NA, nrow = length(lags), ncol = 3)
LBQ_NO2[,1] <- lags
for (i in 1:length(lags)) {
  LBQ_NO2[i,2] <- ljung_box(x = NO2_Bergamo_ts, lag = lags[i])[2]
  LBQ_NO2[i,3] <- ljung_box(x = NO2_Malpensa_ts, lag = lags[i])[2]
}
LBQ_NO2 <- data.frame(LBQ_NO2)
colnames(LBQ_NO2) <- c("lag","NO2_Bergamo","NO2_Malpensa")
LBQ_NO2

#Variabile PM10
#Bergamo
ljung_box(x = PM10_Bergamo_ts, lag = 1) #test con k=1
ljung_box(x = PM10_Bergamo_ts, lag = 3) #test con k=3
ljung_box(x = PM10_Bergamo_ts, lag = 12) #test con k=12
#Rifiuto sempre

#Malpensa
ljung_box(x = PM10_Malpensa_ts, lag = 1) #test con k=1
ljung_box(x = PM10_Malpensa_ts, lag = 3) #test con k=3
ljung_box(x = PM10_Malpensa_ts, lag = 12) #test con k=12
#Rifiuto sempre

#Automatizzo Ljung-Box per tutte e 2 le centraline a vari lags, per PM10
lags <- c(1,2,3,6,12,18,24,36)
LBQ_PM10 <- matrix(data = NA, nrow = length(lags), ncol = 3)
LBQ_PM10[,1] <- lags
for (i in 1:length(lags)) {
  LBQ_PM10[i,2] <- ljung_box(x = PM10_Bergamo_ts, lag = lags[i])[2]
  LBQ_PM10[i,3] <- ljung_box(x = PM10_Malpensa_ts, lag = lags[i])[2]
}
LBQ_PM10 <- data.frame(LBQ_PM10)
colnames(LBQ_PM10) <- c("lag","PM10_Bergamo","PM10_Malpensa")
LBQ_PM10

#Test di Box-Pierce
#Variabile NO2
#Bergamo
box_pierce(x = NO2_Bergamo_ts, lag = 1) #test con k=1
box_pierce(x = NO2_Bergamo_ts, lag = 3) #test con k=3
box_pierce(x = NO2_Bergamo_ts, lag = 12) #test con k=12

#Malpensa
box_pierce(x = NO2_Malpensa_ts, lag = 1) #test con k=1
box_pierce(x = NO2_Malpensa_ts, lag = 3) #test con k=3
box_pierce(x = NO2_Malpensa_ts, lag = 12) #test con k=12

#Automatizzo Box-Pierce per tutte e 2 le centraline a vari lags, per NO2
lags <- c(1,2,3,6,12,18,24,36)
BPQ_NO2 <- matrix(data = NA, nrow = length(lags), ncol = 3)
BPQ_NO2[,1] <- lags
for (i in 1:length(lags)) {
  BPQ_NO2[i,2] <- box_pierce(x = NO2_Bergamo_ts, lag = lags[i])[2]
  BPQ_NO2[i,3] <- box_pierce(x = NO2_Malpensa_ts, lag = lags[i])[2]
}
BPQ_NO2 <- data.frame(BPQ_NO2)
colnames(BPQ_NO2) <- c("lag","NO2_Bergamo","NO2_Malpensa")
BPQ_NO2

#Variabile PM10
#Bergamo
box_pierce(x = PM10_Bergamo_ts, lag = 1) #test con k=1
box_pierce(x = PM10_Bergamo_ts, lag = 3) #test con k=3
box_pierce(x = PM10_Bergamo_ts, lag = 12) #test con k=12
#Rifiuto sempre

#Malpensa
box_pierce(x = PM10_Malpensa_ts, lag = 1) #test con k=1
box_pierce(x = PM10_Malpensa_ts, lag = 3) #test con k=3
box_pierce(x = PM10_Malpensa_ts, lag = 12) #test con k=12
#Rifiuto sempre

#Automatizzo Box-Pierce per tutte e 2 le centraline a vari lags, per PM10
lags <- c(1,2,3,6,12,18,24,36)
BPQ_PM10 <- matrix(data = NA, nrow = length(lags), ncol = 3)
BPQ_PM10[,1] <- lags
for (i in 1:length(lags)) {
  BPQ_PM10[i,2] <- box_pierce(x = PM10_Bergamo_ts, lag = lags[i])[2]
  BPQ_PM10[i,3] <- box_pierce(x = PM10_Malpensa_ts, lag = lags[i])[2]
}
BPQ_PM10 <- data.frame(BPQ_PM10)
colnames(BPQ_PM10) <- c("lag","PM10_Bergamo","PM10_Malpensa")
BPQ_PM10

# 2.4. Trasformata di Box-Cox ---------------------------------------------

#NO2_Bergamo

#Metodo di Guerrero 
lambda_guer <- forecast::BoxCox.lambda(NO2_Bergamo_ts,method = "guerrero",lower = -3,upper = 3) 
lambda_guer #Lambda ottimo = 0.1317

# Metodo massima verosimiglianza 
lambda_loglik <- forecast::BoxCox.lambda(NO2_Bergamo_ts,method = "loglik",lower = -3,upper = 3)
lambda_loglik #0

#Bisogna capire se lambda = 0.1317 sia statisticamente uguale a 0, e quindi si può applicare la trasformazione logaritmica.
m_NO2B_free <- tslm(NO2_Bergamo_ts ~ 1)       # Dati grezzi 
m_NO2B_trend <- tslm(NO2_Bergamo_ts ~ trend)  # Dati detrendizzati
m_NO2B_seas <- tslm(NO2_Bergamo_ts ~ fourier(NO2_Bergamo_ts,K = 6))   # Dati destagionalizzati 
m_NO2B_trendseas <- tslm(NO2_Bergamo_ts ~ trend + fourier(NO2_Bergamo_ts,K = 6))  # Dati detrend e destag

par(mfrow=c(2,2))

bc_free <- MASS::boxcox(m_NO2B_free,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (no model)") 
bc_trend <- MASS::boxcox(m_NO2B_trend,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (LM con trend)") 
bc_seas <- MASS::boxcox(m_NO2B_seas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (LM con armoniche stagionali)") 
bc_trendseas <- MASS::boxcox(m_NO2B_trendseas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (LM con trend e armoniche stagionali)")

par(mfrow=c(1,1)) 

#Creo una nuova serie storica, dell'NO2 a Bergamo, usando il log.
log_NO2B <- NO2_Bergamo %>%
  mutate(log_NO2 = log(NO2)) %>%
  select(log_NO2) 

log_NO2B_ts <- ts_ts(log_NO2B) 

#Ora costruisco l'istogramma per vedere se è normale.
ggplot(data = log_NO2B, aes(x = log_NO2)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(log_NO2B$log_NO2,na.rm=T),
                            sd = sd(log_NO2B$log_NO2,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di log(NO2) a Bergamo Città",
       x = "log(NO2) (log(µg/m³))",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

log_NO2B %>% pull(log_NO2) %>% jarque.bera.test()
log_NO2B %>% pull(log_NO2) %>% shapiro.test()
log_NO2B %>% pull(log_NO2) %>% ks.test(y = "pnorm")

y1 <- autoplot(NO2_Bergamo_ts) + labs(title = "Concentrazione di NO2 a Bergamo Città", subtitle = "Dati ARPA", y = "NO2 (µg/m³)", x = "Anno")
log_y1 <- autoplot(log_NO2B_ts) + labs(title = "Concentrazione di log(NO2) a Bergamo Città", subtitle = "Dati ARPA", y = "log(NO2) log(µg/m³)", x = "Anno")
ggarrange(y1, log_y1, nrow = 2)
#Anche dai grafici emerge che non è cambiato niente

#NO2_Malpensa

#Metodo di Guerrero 
lambda_guer <- forecast::BoxCox.lambda(NO2_Malpensa_ts,method = "guerrero",lower = -3,upper = 3) 
lambda_guer #Lambda ottimo = -0.2626928

# Metodo massima verosimiglianza 
lambda_loglik <- forecast::BoxCox.lambda(NO2_Malpensa_ts,method = "loglik",lower = -3,upper = 3)
lambda_loglik #Lambda ottimo = 0.2

#Bisogna capire se i lambda ottenuti siano statisticamente uguali a 0, e quindi si può applicare la trasformazione logaritmica.
m_NO2M_free <- tslm(NO2_Malpensa_ts ~ 1)       # Dati grezzi 
m_NO2M_trend <- tslm(NO2_Malpensa_ts ~ trend)  # Dati detrendizzati
m_NO2M_seas <- tslm(NO2_Malpensa_ts ~ fourier(NO2_Malpensa_ts,K = 6))   # Dati destagionalizzati 
m_NO2M_trendseas <- tslm(NO2_Malpensa_ts ~ trend + fourier(NO2_Malpensa_ts,K = 6))  # Dati detrend e destag

par(mfrow=c(2,2))

bc_free <- MASS::boxcox(m_NO2M_free,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (no model)") 
bc_trend <- MASS::boxcox(m_NO2M_trend,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (LM con trend)") 
bc_seas <- MASS::boxcox(m_NO2M_seas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (LM con armoniche stagionali)") 
bc_trendseas <- MASS::boxcox(m_NO2M_trendseas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for NO2 (LM con trend e armoniche stagionali)")

par(mfrow=c(1,1)) 

#Creo una nuova serie storica, dell'NO2 a Malpensa, usando il log.
log_NO2M <- NO2_Malpensa %>%
  mutate(log_NO2 = log(NO2)) %>%
  select(log_NO2) 

log_NO2M_ts <- ts_ts(log_NO2M) 

#Ora costruisco l'istogramma per vedere se è normale.
ggplot(data = log_NO2M, aes(x = log_NO2)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(log_NO2M$log_NO2,na.rm=T),
                            sd = sd(log_NO2M$log_NO2,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di log(NO2) a Milano Malpensa",
       x = "log(NO2) (log(µg/m³))",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

log_NO2M %>% pull(log_NO2) %>% jarque.bera.test()
log_NO2M %>% pull(log_NO2) %>% shapiro.test()
log_NO2M %>% pull(log_NO2) %>% ks.test(y = "pnorm")

y2 <- autoplot(NO2_Malpensa_ts) + labs(title = "Concentrazione di NO2 a Milano Malpensa", subtitle = "Dati ARPA", y = "NO2 (µg/m³)", x = "Anno")
log_y2 <- autoplot(log_NO2M_ts) + labs(title = "Concentrazione di log(NO2) a Milano Malpensa", subtitle = "Dati ARPA", y = "log(NO2) log(µg/m³)", x = "Anno")
ggarrange(y2, log_y2, nrow = 2)
#Anche dai grafici emerge che non è cambiato niente


#PM10_Bergamo

#Metodo di Guerrero 
lambda_guer <- forecast::BoxCox.lambda(PM10_Bergamo_ts,method = "guerrero",lower = -3,upper = 3) 
lambda_guer #Lambda ottimo = -0.1318654

# Metodo massima verosimiglianza 
lambda_loglik <- forecast::BoxCox.lambda(PM10_Bergamo_ts,method = "loglik",lower = -3,upper = 3)
lambda_loglik #Lambda = -0.15

#Bisogna capire se i lambda siano statisticamente uguali a 0, e quindi si può applicare la trasformazione logaritmica.
m_PM10B_free <- tslm(PM10_Bergamo_ts ~ 1)       # Dati grezzi 
m_PM10B_trend <- tslm(PM10_Bergamo_ts ~ trend)  # Dati detrendizzati
m_PM10B_seas <- tslm(PM10_Bergamo_ts ~ fourier(PM10_Bergamo_ts,K = 6))   # Dati destagionalizzati 
m_PM10B_trendseas <- tslm(PM10_Bergamo_ts ~ trend + fourier(PM10_Bergamo_ts,K = 6))  # Dati detrend e destag

par(mfrow=c(2,2))

bc_free <- MASS::boxcox(m_PM10B_free,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (no model)") 
bc_trend <- MASS::boxcox(m_PM10B_trend,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (LM con trend)") 
bc_seas <- MASS::boxcox(m_PM10B_seas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (LM con armoniche stagionali)") 
bc_trendseas <- MASS::boxcox(m_PM10B_trendseas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (LM con trend e armoniche stagionali)")

par(mfrow=c(1,1)) 

#Creo una nuova serie storica, del PM10 a Bergamo, usando il log.
log_PM10B <- PM10_Bergamo %>%
  mutate(log_PM10 = log(PM10)) %>%
  select(log_PM10) 

log_PM10B_ts <- ts_ts(log_PM10B) 

#Ora costruisco l'istogramma per vedere se è normale.
ggplot(data = log_PM10B, aes(x = log_PM10)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(log_PM10B$log_PM10,na.rm=T),
                            sd = sd(log_PM10B$log_PM10,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di log(PM10) a Bergamo Città",
       x = "log(NO2) (log(µg/m³))",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

log_PM10B %>% pull(log_PM10) %>% jarque.bera.test()
log_PM10B %>% pull(log_PM10) %>% shapiro.test()
log_PM10B %>% pull(log_PM10) %>% ks.test(y = "pnorm")


#PM10_Malpensa

#Metodo di Guerrero 
lambda_guer <- forecast::BoxCox.lambda(PM10_Malpensa_ts,method = "guerrero",lower = -3,upper = 3) 
lambda_guer #Lambda ottimo = -0.5700564

# Metodo massima verosimiglianza 
lambda_loglik <- forecast::BoxCox.lambda(PM10_Malpensa_ts,method = "loglik",lower = -3,upper = 3)
lambda_loglik #Lambda ottimo = -0.35

#Bisogna capire se i lambda ottenuti siano statisticamente uguali e uguali a 0, e quindi si può applicare la trasformazione logaritmica.
m_PM10M_free <- tslm(PM10_Malpensa_ts ~ 1)       # Dati grezzi 
m_PM10M_trend <- tslm(PM10_Malpensa_ts ~ trend)  # Dati detrendizzati
m_PM10M_seas <- tslm(PM10_Malpensa_ts ~ fourier(PM10_Malpensa_ts,K = 6))   # Dati destagionalizzati 
m_PM10M_trendseas <- tslm(PM10_Malpensa_ts ~ trend + fourier(PM10_Malpensa_ts,K = 6))  # Dati detrend e destag

par(mfrow=c(2,2))

bc_free <- MASS::boxcox(m_PM10M_free,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (no model)") 
bc_trend <- MASS::boxcox(m_PM10M_trend,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (LM con trend)") 
bc_seas <- MASS::boxcox(m_PM10M_seas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (LM con armoniche stagionali)") 
bc_trendseas <- MASS::boxcox(m_PM10M_trendseas,lambda=seq(-3,3,by=.01)) 
title(main = "Log-lik for PM10 (LM con trend e armoniche stagionali)")

par(mfrow=c(1,1)) 

#Creo una nuova serie storica, del PM10 a Malpensa, usando il log.
log_PM10M <- PM10_Malpensa %>%
  mutate(log_PM10 = log(PM10)) %>%
  select(log_PM10) 

log_PM10M_ts <- ts_ts(log_PM10M) 

#Ora costruisco l'istogramma per vedere se è normale.
ggplot(data = log_PM10M, aes(x = log_PM10)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(log_PM10M$log_PM10,na.rm=T),
                            sd = sd(log_PM10M$log_PM10,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di log(PM10) a Milano Malpensa",
       x = "log(PM10) (log(µg/m³))",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

log_PM10M %>% pull(log_PM10) %>% jarque.bera.test()
log_PM10M %>% pull(log_PM10) %>% shapiro.test()
log_PM10M %>% pull(log_PM10) %>% ks.test(y = "pnorm")


# 2.5. EDA: Analisi della stazionarietà -----------------------------------

# 2.5.1. Destagionalizzazione ---------------------------------------------

library(fable)
library(urca)

#Variabile NO2 Bergamo

#Test ADF sulla serie originale

#NO2 a Bergamo

#Si trend, Si costante
ADF_NO2B_const_trend <- urca::ur.df(y = NO2_Bergamo_ts, type = "trend",selectlags = "AIC")
summary(ADF_NO2B_const_trend)
#Emerge che la serie è stazionaria attorno a un trend deterministico

#No trend, Si costante
ADF_NO2B_const <- urca::ur.df(y = NO2_Bergamo_ts, type = "drift",selectlags = "AIC")
summary(ADF_NO2B_const)

#No trend, no costante
ADF_NO2B <- urca::ur.df(y = NO2_Bergamo_ts, type = "none",selectlags = "AIC")
summary(ADF_NO2B)


#Regressione armonica
k1 <- tslm(NO2_Bergamo_ts ~ fourier(NO2_Bergamo_ts,K = 1))
k2 <- tslm(NO2_Bergamo_ts ~ fourier(NO2_Bergamo_ts,K = 2))
k3 <- tslm(NO2_Bergamo_ts ~ fourier(NO2_Bergamo_ts,K = 3))
k4 <- tslm(NO2_Bergamo_ts ~ fourier(NO2_Bergamo_ts,K = 4))
k5 <- tslm(NO2_Bergamo_ts ~ fourier(NO2_Bergamo_ts,K = 5))
k6 <- tslm(NO2_Bergamo_ts ~ fourier(NO2_Bergamo_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)] 
#Indecisione tra M1 e M2

NO2B_deseas <- NO2_Bergamo %>%
  mutate(NO2B_deseas_m1 = k1$residuals, seas_m1 = k1$fitted.values,
         NO2B_deseas_m2 = k2$residuals, seas_m2 = k2$fitted.values)

#Rappresentazione grafica TS con stagionalità
NO2B_deseas %>%
  pivot_longer(cols = c(seas_m1, seas_m2),
               names_to = "Model", values_to = "seas") %>%
  ggplot(aes(x = Data)) +
  geom_line(aes(y = NO2, color = "NO2 Originale"), alpha = 0.7) +
  geom_line(aes(y = seas, color = Model), size = 0.8) +
  labs(
    title = "NO2 con stagionalità", subtitle = "Bergamo Città",
    y = "NO2 (µg/m³)",
    color = "Legenda" 
  ) +
  scale_color_manual(
    values = c("NO2 Originale" = "black", "seas_m1" = "blue", "seas_m2" = "red"),
    labels = c("NO2 Originale" = "NO2 Originale", "seas_m1" = "Stagionalità Modello 1", "seas_m2" = "Stagionalità Modello 2")
  ) +
  facet_wrap(~ Model, nrow = 1)

#Si sceglie il modello 1
summary(k1)
NO2B_destag <- k1$residuals 
autoplot(NO2B_destag) +  
  labs(title = "Concentrazione di NO2 destagionalizzata", subtitle = "Bergamo Città", y = "NO2 (µg/m³)", x = "Anno") 

#Istogramma della serie di NO2 a Bergamo destagionalizzata
hist_NO2B_destag <- as_tsibble(NO2B_destag) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(NO2B_destag,na.rm=T),
                            sd = sd(NO2B_destag,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma della concentrazione di NO2 destagionalizzata", subtitle = "Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = color,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

#Box-Plot della serie destagionalizzata di NO2 a Bergamo
boxplot_NO2B_destag <- as_tsibble(NO2B_destag) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "NO2 (µg/m³)")

ggarrange(hist_NO2B_destag,boxplot_NO2B_destag, nrow = 1)

NO2B_destag %>% jarque.bera.test()
#P-Value = 0.739 -> Accetto H0

#Lagplot
gglagplot(NO2B_destag,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")

#ACF
ACF_NO2B_destag <- ggAcf(NO2B_destag) + 
  labs(title = "ACF")
#PACF
PACF_NO2B_destag <- ggPacf(NO2B_destag) + 
  labs(title = "PACF")
ggarrange(ACF_NO2B_destag, PACF_NO2B_destag, nrow = 1)

library(urca)
#Test ADF sulla serie destagionalizzata NO2 a Bergamo
# Si trend, Si costante
summary(urca::ur.df(y = NO2B_destag, type = "trend",selectlags = "AIC"))
#Emerge che la serie è stazionaria attorno a un trend deterministico

# No trend, Si costante
summary(urca::ur.df(y = NO2B_destag, type = "drift",selectlags = "AIC"))

#No trend, no costante
summary(urca::ur.df(y = NO2B_destag, type = "none",selectlags = "AIC"))

#Proviamo per curiosità a detrendizzare la serie già destagionalizzata
NO2B_de <- tslm(NO2B_destag ~ trend)
summary(NO2B_de)

NO2B_destag %>% autoplot()+ 
    geom_line(aes(y=NO2B_de$fitted.values), col="blue", size=1.1)+
    labs(title = "Concentrazione di NO2 a Bergamo Città", subtitle = "Dati ARPA",
         y = "µg/m³", x = "Anno")

autoplot(NO2B_de$residuals) +  
  labs(title = "Concentrazione di NO2 detrendizzata e destagionalizzata", subtitle = "Bergamo Città", y = "NO2 (µg/m³)", x = "Anno") 
#Effettivamente il risultato non cambia, quindi è un passaggio inutile.


#Variabile NO2 Malpensa

#NO2 a Malpensa
#No trend, no costante
summary(urca::ur.df(y = NO2_Malpensa_ts, type = "none",selectlags = "AIC"))
# No trend, Si costante
summary(urca::ur.df(y = NO2_Malpensa_ts, type = "drift",selectlags = "AIC"))
# Si trend, Si costante
summary(urca::ur.df(y = NO2_Malpensa_ts, type = "trend",selectlags = "AIC"))

#Regressione armonica
k1 <- tslm(NO2_Malpensa_ts ~ fourier(NO2_Malpensa_ts,K = 1))
k2 <- tslm(NO2_Malpensa_ts ~ fourier(NO2_Malpensa_ts,K = 2))
k3 <- tslm(NO2_Malpensa_ts ~ fourier(NO2_Malpensa_ts,K = 3))
k4 <- tslm(NO2_Malpensa_ts ~ fourier(NO2_Malpensa_ts,K = 4))
k5 <- tslm(NO2_Malpensa_ts ~ fourier(NO2_Malpensa_ts,K = 5))
k6 <- tslm(NO2_Malpensa_ts ~ fourier(NO2_Malpensa_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)] 

#Si sceglie il modello 2
summary(k2)
NO2M_destag <- k2$residuals 
autoplot(NO2M_destag) +  
  labs(title = "Concentrazione di NO2 destagionalizzata", subtitle = "Milano Malpensa", y = "NO2 (µg/m³)", x = "Anno") 

#Test ADF sulla serie destagionalizzata NO2 a Malpensa
# Si trend, Si costante
summary(urca::ur.df(y = NO2M_destag, type = "trend",selectlags = "AIC"))
#Emerge che la serie è stazionaria attorno a un trend deterministico

# No trend, Si costante
summary(urca::ur.df(y = NO2M_destag, type = "drift",selectlags = "AIC"))

#No trend, no costante
summary(urca::ur.df(y = NO2M_destag, type = "none",selectlags = "AIC"))

#Detrendizziamo la serie già destagionalizzata
NO2M_de <- tslm(NO2M_destag ~ trend)
summary(NO2M_de)

gg1 <- NO2M_destag %>% autoplot()+ 
  geom_line(aes(y=NO2M_de$fitted.values), col="blue", size=1.1)+
  labs(title = "Concentrazione di NO2 a Milano Malpensa", subtitle = "Dati ARPA",
       y = "µg/m³", x = "Anno")

gg2 <- autoplot(NO2M_de$residuals) +  
  labs(title = "Concentrazione di NO2 detrendizzata e destagionalizzata", subtitle = "Milano Malpensa", y = "NO2 (µg/m³)", x = "Anno") 
#La serie sembra ora stazionaria.
ggarrange(gg1, gg2, nrow = 1)

#Serie detrendizzata e destagionalizzata
NO2M_de <- NO2M_de$residuals

#Istogramma della serie di NO2 a Malpensa destagionalizzata e detrendizzata
hist_NO2M_de <- as_tsibble(NO2M_de) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(NO2M_de,na.rm=T),
                            sd = sd(NO2M_de,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma della concentrazione di NO2 destagionalizzata e detrendizzata", subtitle = "Milano Malpensa",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = color,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

#Box-Plot della serie destagionalizzata e detrendizzata di NO2 a Malpensa
boxplot_NO2M_de <- as_tsibble(NO2M_de) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "NO2 (µg/m³)")

ggarrange(hist_NO2M_de,boxplot_NO2M_de, nrow = 1)

NO2M_de %>% jarque.bera.test()
#P-Value = 0.0612 -> Accetto H0

#Lagplot
gglagplot(NO2M_de,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")

#ACF
ACF_NO2M_de <- ggAcf(NO2M_de) + 
  labs(title = "ACF")
#PACF
PACF_NO2M_de <- ggPacf(NO2M_de) + 
  labs(title = "PACF")
ggarrange(ACF_NO2M_de, PACF_NO2M_de, nrow = 1)

#Test ADF di conferma sulla serie destagionalizzata e detrendizzata
summary(urca::ur.df(y = NO2M_de, type = "none",selectlags = "AIC"))
summary(urca::ur.df(y = NO2M_de, type = "drift",selectlags = "AIC"))
summary(urca::ur.df(y = NO2M_de, type = "trend",selectlags = "AIC"))


#Provo per curiosità a verificare cosa accade con trend polinomiali
m1 <- tslm(NO2M_destag ~ trend)
m2 <- tslm(NO2M_destag ~ trend + I(trend^2))
m3 <- tslm(NO2M_destag ~ trend + I(trend^2) + I(trend^3))
m4 <- tslm(NO2M_destag ~ trend + I(trend^2) + I(trend^3) + I(trend^4))

a <- data.frame(cbind(Model=c("M1","M2","M3","M4"),
                             rbind(round(CV(m1),2),round(CV(m2),2),round(CV(m3),2),round(CV(m4),2))))
a

a$Model[which.min(a$AIC)] 
a$Model[which.min(a$AICc)] 
a$Model[which.min(a$BIC)] 
a$Model[which.max(a$AdjR2)] 

#Il modello migliore sembra essere il terzo con il trend cubico
summary(m3)
m3$residuals #Serie detrendizzata con trend cubico e destagionalizzata
autoplot(m3$residuals) +  
  labs(title = "Concentrazione di NO2 destagionalizzata e detrendizzata con un trend cubico", subtitle = "Milano Malpensa", y = "NO2 (µg/m³)", x = "Anno") 

gg3 <- NO2M_destag %>% autoplot()+ 
  geom_line(aes(y=m3$fitted.values), col="blue", size=1.1)+
  labs(title = "Concentrazione di NO2 a Milano Malpensa", subtitle = "Dati ARPA",
       y = "µg/m³", x = "Anno")

gg4 <- autoplot(m3$residuals) +  
  labs(title = "Concentrazione di NO2 detrendizzata e destagionalizzata", subtitle = "Milano Malpensa", y = "NO2 (µg/m³)", x = "Anno") 
#Rischio di far passare il break del 2020 come dipeso dal trend. Non va bene.
ggarrange(gg3, gg4, nrow = 1)


#PM10 a Bergamo trasformata Box-Cox
autoplot(log_PM10B_ts) + labs(title = "Concentrazione di log(PM10)", subtitle = "Bergamo Città", y = "log(NO2) log(µg/m³)", x = "Anno")

#Test ADF sulla serie originale
#Si trend, Si costante
summary(urca::ur.df(y = log_PM10B_ts, type = "trend",selectlags = "AIC"))
#Emerge che la serie è stazionaria attorno a un trend deterministico

#No trend, Si costante
summary(urca::ur.df(y = log_PM10B_ts, type = "drift",selectlags = "AIC"))

#No trend, no costante
summary(urca::ur.df(y = log_PM10B_ts, type = "none",selectlags = "AIC"))
#Da questi risultati sembra che la serie non presenti una radice unitaria, altrimenti avrei rifiutato H0 anche con il "drift" e con "trend".

k1 <- tslm(log_PM10B_ts ~ fourier(log_PM10B_ts,K = 1))
k2 <- tslm(log_PM10B_ts ~ fourier(log_PM10B_ts,K = 2))
k3 <- tslm(log_PM10B_ts ~ fourier(log_PM10B_ts,K = 3))
k4 <- tslm(log_PM10B_ts ~ fourier(log_PM10B_ts,K = 4))
k5 <- tslm(log_PM10B_ts ~ fourier(log_PM10B_ts,K = 5))
k6 <- tslm(log_PM10B_ts ~ fourier(log_PM10B_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)] 
#Il modello migliore sembra essere M4.

summary(k4)
log_PM10B_destag <- k4$residuals #Serie destagionalizzata 
autoplot(log_PM10B_destag) +  
  labs(title = "Concentrazione di log(PM10) destagionalizzata", subtitle = "Bergamo Città", y = "PM10 (µg/m³)", x = "Anno") 

#Istogramma della serie di log(PM10) a Bergamo destagionalizzata
hist_log_PM10B_destag <- as_tsibble(log_PM10B_destag) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(log_PM10B_destag,na.rm=T),
                            sd = sd(log_PM10B_destag,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma della concentrazione di log(PM10) destagionalizzata", subtitle = "Bergamo Città",
       x = "PM10 (µg/m³)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = color,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

#Box-Plot della serie destagionalizzata di PM10 a Bergamo
boxplot_log_PM10B_destag <- as_tsibble(log_PM10B_destag) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "log(PM10) log(µg/m³)")

ggarrange(hist_log_PM10B_destag,boxplot_log_PM10B_destag, nrow = 1)

log_PM10B_destag %>% jarque.bera.test()

#Lagplot
gglagplot(log_PM10B_destag,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")

#ACF
ACF_log_PM10B_destag <- ggAcf(log_PM10B_destag) + 
  labs(title = "ACF")
#PACF
PACF_log_PM10B_destag <- ggPacf(log_PM10B_destag) + 
  labs(title = "PACF")
ggarrange(ACF_log_PM10B_destag, PACF_log_PM10B_destag, nrow = 1)

library(urca)
#Test ADF sulla serie destagionalizzata log(PM10) a Bergamo
# Si trend, Si costante
summary(urca::ur.df(y = log_PM10B_destag, type = "trend",selectlags = "AIC"))
#Emerge che la serie è stazionaria attorno a un trend deterministico

# No trend, Si costante
summary(urca::ur.df(y = log_PM10B_destag, type = "drift",selectlags = "AIC"))

#No trend, no costante
summary(urca::ur.df(y = log_PM10B_destag, type = "none",selectlags = "AIC"))

#Sembra esserci un trend decrescente lineare.
#Detrendizziamo la serie già destagionalizzata
PM10B_de <- tslm(log_PM10B_destag ~ trend) 
summary(PM10B_de)

gg5 <- log_PM10B_destag %>% autoplot()+ 
  geom_line(aes(y=PM10B_de$fitted.values), col="blue", size=1.1)+
  labs(title = "Concentrazione di log(PM10) a Bergamo Città", subtitle = "Dati ARPA",
       y = "log(µg/m³)", x = "Anno")

gg6 <- autoplot(PM10B_de$residuals) +  
  labs(title = "Concentrazione di log(PM10) detrendizzata e destagionalizzata", subtitle = "Bergamo Città", y = "log(PM10 (µg/m³))", x = "Anno") 
#La serie ora sembra essere stazionaria.

ggarrange(gg5, gg6, ncol = 2)

#Serie detrendizzata e destagionalizzata
PM10B_de <- PM10B_de$residuals 

#Istogramma della serie di PM10 a Bergamo destagionalizzata e detrendizzata
hist_PM10B_de <- as_tsibble(PM10B_de) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(PM10B_de,na.rm=T),
                            sd = sd(PM10B_de,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma della concentrazione di log(PM10) destagionalizzata e detrendizzata", subtitle = "Bergamo Città",
       x = "log(PM10 (µg/m³))",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = color,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

#Box-Plot della serie destagionalizzata e detrendizzata di PM10 a Bergamo
boxplot_PM10B_de <- as_tsibble(PM10B_de) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "log(PM10 (µg/m³))")

ggarrange(hist_PM10B_de,boxplot_PM10B_de, nrow = 1)

PM10B_de %>% jarque.bera.test()
#P-Value = 0.5678 -> Accetto H0

#Lagplot
gglagplot(PM10B_de,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")

#ACF
ACF_PM10B_de <- ggAcf(PM10B_de) + 
  labs(title = "ACF")
#PACF
PACF_PM10B_de <- ggPacf(PM10B_de) + 
  labs(title = "PACF")
ggarrange(ACF_PM10B_de, PACF_PM10B_de, nrow = 1)

#Test ADF di conferma sulla serie destagionalizzata e detrendizzata
summary(urca::ur.df(y = PM10B_de, type = "none",selectlags = "AIC"))
summary(urca::ur.df(y = PM10B_de, type = "drift",selectlags = "AIC"))
summary(urca::ur.df(y = PM10B_de, type = "trend",selectlags = "AIC"))

#Provo per curiosità a verificare cosa accade con trend polinomiali
n1 <- tslm(log_PM10B_destag ~ trend)
n2 <- tslm(log_PM10B_destag ~ trend + I(trend^2))
n3 <- tslm(log_PM10B_destag ~ trend + I(trend^2) + I(trend^3))
n4 <- tslm(log_PM10B_destag ~ trend + I(trend^2) + I(trend^3) + I(trend^4))

b <- data.frame(cbind(Model=c("M1","M2","M3","M4"),
                      rbind(round(CV(n1),2),round(CV(n2),2),round(CV(n3),2),round(CV(n4),2))))
b

b$Model[which.min(b$AIC)] 
b$Model[which.min(b$AICc)] 
b$Model[which.min(b$BIC)] 
b$Model[which.max(b$AdjR2)] 

#Il modello migliore sembra essere il quarto, con un trend polinomiale di grado quarto
summary(n4)
n4$residuals #Serie detrendizzata con trend di grado quarto e destagionalizzata
autoplot(n4$residuals) +  
  labs(title = "Concentrazione di log(PM10) destagionalizzata e detrendizzata con un trend di ordine quarto", subtitle = "Bergamo Città", y = "log(PM10 (µg/m³))", x = "Anno") 

gg7 <- log_PM10B_destag %>% autoplot()+ 
  geom_line(aes(y=n4$fitted.values), col="blue", size=1.1)+
  labs(title = "Concentrazione di log(PM10) a Bergamo Città", subtitle = "Dati ARPA",
       y = "log(µg/m³)", x = "Anno")

gg8 <- autoplot(n4$residuals) +  
  labs(title = "Concentrazione di log(PM10) detrendizzata e destagionalizzata", subtitle = "Bergamo Città", y = "log(PM10 (µg/m³))", x = "Anno") 
#Rischio di far modellare il break del 2017 come dipeso dal trend. Non va bene.
ggarrange(gg7, gg8, nrow = 1)


#PM10 a Malpensa
#Variabile PM10 Malpensa

#PM10 a Malpensa
#No trend, no costante
summary(urca::ur.df(y = PM10_Malpensa_ts, type = "none",selectlags = "AIC"))
# No trend, Si costante
summary(urca::ur.df(y = PM10_Malpensa_ts, type = "drift",selectlags = "AIC"))
# Si trend, Si costante
summary(urca::ur.df(y = PM10_Malpensa_ts, type = "trend",selectlags = "AIC"))

#Regressione armonica
k1 <- tslm(PM10_Malpensa_ts ~ fourier(PM10_Malpensa_ts,K = 1))
k2 <- tslm(PM10_Malpensa_ts ~ fourier(PM10_Malpensa_ts,K = 2))
k3 <- tslm(PM10_Malpensa_ts ~ fourier(PM10_Malpensa_ts,K = 3))
k4 <- tslm(PM10_Malpensa_ts ~ fourier(PM10_Malpensa_ts,K = 4))
k5 <- tslm(PM10_Malpensa_ts ~ fourier(PM10_Malpensa_ts,K = 5))
k6 <- tslm(PM10_Malpensa_ts ~ fourier(PM10_Malpensa_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)] 

#Si sceglie il modello 2
summary(k2)
PM10M_destag <- k2$residuals 
autoplot(PM10M_destag) +  
  labs(title = "Concentrazione di PM10 destagionalizzata", subtitle = "Milano Malpensa", y = "PM10 (µg/m³)", x = "Anno") 

#Istogramma della serie di PM10 a Malpensa destagionalizzata
hist_PM10M_destag <- as_tsibble(PM10M_destag) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(PM10M_destag,na.rm=T),
                            sd = sd(PM10M_destag,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma della concentrazione di PM10 destagionalizzata", subtitle = "Milano Malpensa",
       x = "PM10 (µg/m³)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = color,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

#Box-Plot della serie destagionalizzata di PM10 a Malpensa
boxplot_PM10M_destag <- as_tsibble(PM10M_destag) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "PM10 (µg/m³)")

ggarrange(hist_PM10M_destag,boxplot_PM10M_destag, nrow = 1)

#Rimuoviamo gli outlier
tsoutliers(PM10M_destag)

PM10M_destag <- tsclean(PM10M_destag)

autoplot(PM10M_destag) +  
  labs(title = "Concentrazione di PM10 destagionalizzata", subtitle = "Milano Malpensa", y = "PM10 (µg/m³)", x = "Anno") 

#Istogramma della serie di PM10 a Malpensa destagionalizzata
hist_PM10M_destag <- as_tsibble(PM10M_destag) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(PM10M_destag,na.rm=T),
                            sd = sd(PM10M_destag,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma della concentrazione di PM10 destagionalizzata", subtitle = "Milano Malpensa",
       x = "PM10 (µg/m³)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = color,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))

#Box-Plot della serie destagionalizzata di PM10 a Malpensa
boxplot_PM10M_destag <- as_tsibble(PM10M_destag) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "PM10 (µg/m³)")

ggarrange(hist_PM10M_destag,boxplot_PM10M_destag, nrow = 1)

PM10M_destag %>% jarque.bera.test()
#P-Value = 0.2271 -> Accetto H0

#Lagplot
gglagplot(PM10M_destag,set.lags = c(1,2,3,6,12,18,24,30,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")

#ACF
ACF_PM10M_destag <- ggAcf(PM10M_destag) + 
  labs(title = "ACF")
#PACF
PACF_PM10M_destag <- ggPacf(PM10M_destag) + 
  labs(title = "PACF")
ggarrange(ACF_PM10M_destag, PACF_PM10M_destag, nrow = 1)

#Test ADF sulla serie destagionalizzata PM10 a Malpensa
# Si trend, Si costante
summary(urca::ur.df(y = PM10M_destag, type = "trend",selectlags = "AIC"))
#Emerge che la serie è stazionaria attorno a un trend deterministico

# No trend, Si costante
summary(urca::ur.df(y = PM10M_destag, type = "drift",selectlags = "AIC"))

#No trend, no costante
summary(urca::ur.df(y = PM10M_destag, type = "none",selectlags = "AIC"))

#Proviamo a vedere se ha senso detrendizzare

#Detrendizziamo la serie già destagionalizzata
PM10M_de <- tslm(PM10M_destag ~ trend)
summary(PM10M_de)

gg1 <- PM10M_destag %>% autoplot()+ 
  geom_line(aes(y=PM10M_de$fitted.values), col="blue", size=1.1)+
  labs(title = "Concentrazione di PM10 a Milano Malpensa", subtitle = "Dati ARPA",
       y = "µg/m³", x = "Anno")

gg2 <- autoplot(PM10M_de$residuals) +  
  labs(title = "Concentrazione di PM10 detrendizzata e destagionalizzata", subtitle = "Milano Malpensa", y = "PM10 (µg/m³)", x = "Anno") 
ggarrange(gg1, gg2, nrow = 1)

#Il trend è impercettibile, come ci aspettavamo non ha senso.


# 3. Decomposizione -------------------------------------------------------

colori <- c("Destag"="orange2",
            "Detrend"="green3",
            "Original"="black")

#Decomposizione classica
#NO2 a Bergamo
autoplot(decompose(NO2_Bergamo_ts, type = "multiplicative")) + xlab("Anno") +
  ggtitle("Decomposizione moltiplicativa classica per NO2") 
autoplot(decompose(NO2_Bergamo_ts, type = "additive")) + xlab("Anno") +
  ggtitle("Decomposizione additiva classica per NO2")
#Non sembrano essere la scelta adatta perchè il break strutturale viene modellato nel trend


# 3.1. Variabile NO2 Bergamo ----------------------------------------------

#Decomposizione X11
library(seasonal)

( dec_NO2B_X11 <- NO2_Bergamo_ts %>% 
  seas(x11 = "") )
#Dall'output emerge che è una componente AR(1) non stagionale e poi una componente stagionale sulla parte a media mobile

#Estraiamo le tre componenti
trend_NO2B_X11 <- trendcycle(dec_NO2B_X11)
seas_NO2B_X11 <- seasonal(dec_NO2B_X11)
res_NO2B_X11 <- remainder(dec_NO2B_X11)

#Istogramma residui
ggplot(data = res_NO2B_X11, aes(x = res_NO2B_X11)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_NO2B_X11,na.rm=T),
                            sd = sd(res_NO2B_X11,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_NO2B_X11)

NO2B_X11_destag <- seasadj(dec_NO2B_X11) #serie destagionalizzata
NO2B_X11_detrend <- NO2_Bergamo_ts - trend_NO2B_X11 #serie detrendizzata

q1 <- autoplot(dec_NO2B_X11) + 
  xlab("Anno") +
  ggtitle("Decomposizione X-11 per NO2 a Bergamo Città")

autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_X11_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione X11 per NO2 a Bergamo",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_X11_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione X11 per NO2 a Bergamo",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione SEATS
dec_NO2B_SEATS <- NO2_Bergamo_ts %>% 
  seas()
dec_NO2B_SEATS

trend_NO2B_SEATS <- trendcycle(dec_NO2B_SEATS)
seas_NO2B_SEATS <- seasonal(dec_NO2B_SEATS)
res_NO2B_SEATS <- remainder(dec_NO2B_SEATS)

#Istogramma residui
ggplot(data = res_NO2B_SEATS, aes(x = res_NO2B_SEATS)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_NO2B_SEATS,na.rm=T),
                            sd = sd(res_NO2B_SEATS,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_NO2B_SEATS)

NO2B_SEATS_destag <- seasadj(dec_NO2B_SEATS)
NO2B_SEATS_detrend <- NO2_Bergamo_ts - trend_NO2B_SEATS

q2 <- autoplot(dec_NO2B_SEATS) + 
  xlab("Anno") +
  ggtitle("Decomposizione SEATS per NO2 a Bergamo")

#Serie destagionalizzata
autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_SEATS_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione SEATS per NO2 a Bergamo",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_SEATS_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione SEATS per NO2 a Bergamo",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione STL
dec_NO2B_STL <- NO2_Bergamo_ts %>% 
  stl(t.window=25, s.window="periodic")
dec_NO2B_STL

trend_NO2B_STL <- trendcycle(dec_NO2B_STL)
seas_NO2B_STL <- seasonal(dec_NO2B_STL)
res_NO2B_STL <- remainder(dec_NO2B_STL)

#Istogramma residui
ggplot(data = res_NO2B_STL, aes(x = res_NO2B_STL)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_NO2B_STL,na.rm=T),
                            sd = sd(res_NO2B_STL,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_NO2B_STL)

NO2B_STL_destag <- seasadj(dec_NO2B_STL)
NO2B_STL_detrend <- NO2_Bergamo_ts - trend_NO2B_STL

q3 <- autoplot(dec_NO2B_STL) + 
  xlab("Anno") +
  ggtitle("Decomposizione STL per NO2 a Bergamo")

#Serie destagionalizzata
autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_STL_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione STL per NO2 a Bergamo",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_STL_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione STL per NO2 a Bergamo",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Confronto tra i metodi di decomposizione
q4 <- autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_X11_destag,series = "X11", size=1.01) + 
  autolayer(NO2B_SEATS_destag,series = "SEATS", size=1.01) + 
  autolayer(NO2B_STL_destag,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Destagionalizzazione con vari metodi di decomposizione", subtitle = "NO2 a Bergamo Città")

autoplot(NO2_Bergamo_ts,series = "Original") + 
  autolayer(NO2B_X11_detrend,series = "X11", size=1.01) + 
  autolayer(NO2B_SEATS_detrend,series = "SEATS", size=1.01) + 
  autolayer(NO2B_STL_detrend,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Detrendizzazione con vari metodi di decomposizione", subtitle = "NO2 a Bergamo Città")

ggarrange(q1,q2,q3,q4, nrow = 2, ncol = 2)

# 3.2. Variabile NO2 a Malpensa -------------------------------------------

#Decomposizione X11
library(seasonal)

( dec_NO2M_X11 <- NO2_Malpensa_ts %>% 
  seas(x11 = "") )
#Dall'output emerge che è una componente MA(1) non stagionale e poi una componente stagionale sulla parte a media mobile

#Estraiamo le tre componenti
trend_NO2M_X11 <- trendcycle(dec_NO2M_X11)
seas_NO2M_X11 <- seasonal(dec_NO2M_X11)
res_NO2M_X11 <- remainder(dec_NO2M_X11)

#Istogramma residui
ggplot(data = res_NO2M_X11, aes(x = res_NO2M_X11)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_NO2M_X11,na.rm=T),
                            sd = sd(res_NO2M_X11,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_NO2M_X11)

NO2M_X11_destag <- seasadj(dec_NO2M_X11) #serie destagionalizzata
NO2M_X11_detrend <- NO2_Malpensa_ts - trend_NO2M_X11 #serie detrendizzata

q1 <- autoplot(dec_NO2M_X11) + 
  xlab("Anno") +
  ggtitle("Decomposizione X-11 per NO2 a Milano Malpensa")

autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_X11_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione X11 per NO2 a Malpensa",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_X11_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione X11 per NO2 a Malpensa",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione SEATS
dec_NO2M_SEATS <- NO2_Malpensa_ts %>% 
  seas()
dec_NO2M_SEATS

trend_NO2M_SEATS <- trendcycle(dec_NO2M_SEATS)
seas_NO2M_SEATS <- seasonal(dec_NO2M_SEATS)
res_NO2M_SEATS <- remainder(dec_NO2M_SEATS)

#Istogramma residui
ggplot(data = res_NO2M_SEATS, aes(x = res_NO2M_SEATS)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_NO2M_SEATS,na.rm=T),
                            sd = sd(res_NO2M_SEATS,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Milano Malpensa",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_NO2M_SEATS)

NO2M_SEATS_destag <- seasadj(dec_NO2M_SEATS)
NO2M_SEATS_detrend <- NO2_Malpensa_ts - trend_NO2M_SEATS

q2 <- autoplot(dec_NO2M_SEATS) + 
  xlab("Anno") +
  ggtitle("Decomposizione SEATS per NO2 a Malpensa")

#Serie destagionalizzata
autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_SEATS_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione SEATS per NO2 a Malpensa",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_SEATS_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione SEATS per NO2 a Malpensa",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione STL
dec_NO2M_STL <- NO2_Malpensa_ts %>% 
  stl(t.window=25, s.window="periodic")
dec_NO2M_STL

trend_NO2M_STL <- trendcycle(dec_NO2M_STL)
seas_NO2M_STL <- seasonal(dec_NO2M_STL)
res_NO2M_STL <- remainder(dec_NO2M_STL)

#Istogramma residui
ggplot(data = res_NO2M_STL, aes(x = res_NO2M_STL)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_NO2M_STL,na.rm=T),
                            sd = sd(res_NO2M_STL,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di NO2 a Milano Malpensa",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_NO2M_STL)

NO2M_STL_destag <- seasadj(dec_NO2M_STL)
NO2M_STL_detrend <- NO2_Malpensa_ts - trend_NO2M_STL

q3 <- autoplot(dec_NO2M_STL) + 
  xlab("Anno") +
  ggtitle("Decomposizione STL per NO2 a Malpensa")

#Serie destagionalizzata
autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_STL_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione STL per NO2 a Malpensa",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_STL_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione STL per NO2 a Malpensa",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Confronto tra i metodi di decomposizione
q4 <- autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_X11_destag,series = "X11", size=1.01) + 
  autolayer(NO2M_SEATS_destag,series = "SEATS", size=1.01) + 
  autolayer(NO2M_STL_destag,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Destagionalizzazione con vari metodi di decomposizione", subtitle = "NO2 a Milano Malpensa")

autoplot(NO2_Malpensa_ts,series = "Original") + 
  autolayer(NO2M_X11_detrend,series = "X11", size=1.01) + 
  autolayer(NO2M_SEATS_detrend,series = "SEATS", size=1.01) + 
  autolayer(NO2M_STL_detrend,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Detrendizzazione con vari metodi di decomposizione", subtitle = "NO2 a Milano Malpensa")

ggarrange(q1,q2,q3,q4, nrow = 2, ncol = 2)

# 3.3. Variabile log(PM10) Bergamo ----------------------------------------------

#Decomposizione X11
library(seasonal)

( dec_PM10B_X11 <- log_PM10B_ts %>% 
  seas(x11 = "") )
#Dall'output emerge che c'è una componente stagionale sulla parte a media mobile

#Estraiamo le tre componenti
trend_PM10B_X11 <- trendcycle(dec_PM10B_X11)
seas_PM10B_X11 <- seasonal(dec_PM10B_X11)
res_PM10B_X11 <- remainder(dec_PM10B_X11)

#Istogramma residui
ggplot(data = res_PM10B_X11, aes(x = res_PM10B_X11)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_PM10B_X11,na.rm=T),
                            sd = sd(res_PM10B_X11,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di PM10 a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_PM10B_X11)

PM10B_X11_destag <- seasadj(dec_PM10B_X11) #serie destagionalizzata
PM10B_X11_detrend <- log_PM10B_ts - trend_PM10B_X11 #serie detrendizzata

q1 <- autoplot(dec_PM10B_X11) + 
  xlab("Anno") +
  ggtitle("Decomposizione X-11 per log(PM10) a Bergamo Città")

autoplot(log_PM10B_ts,series = "Original") + 
  autolayer(PM10B_X11_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione X11 per log(PM10) a Bergamo",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

autoplot(log_PM10B_ts,series = "Original") + 
  autolayer(PM10B_X11_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione X11 per log(PM10) a Bergamo",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione SEATS
dec_PM10B_SEATS <- log_PM10B_ts %>% 
  seas()
dec_PM10B_SEATS

trend_PM10B_SEATS <- trendcycle(dec_PM10B_SEATS)
seas_PM10B_SEATS <- seasonal(dec_PM10B_SEATS)
res_PM10B_SEATS <- remainder(dec_PM10B_SEATS)

#Istogramma residui
ggplot(data = res_PM10B_SEATS, aes(x = res_PM10B_SEATS)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_PM10B_SEATS,na.rm=T),
                            sd = sd(res_PM10B_SEATS,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di PM10 a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_PM10B_SEATS)

PM10B_SEATS_destag <- seasadj(dec_PM10B_SEATS)
PM10B_SEATS_detrend <- log_PM10B_ts - trend_PM10B_SEATS

q2 <- autoplot(dec_PM10B_SEATS) + 
  xlab("Anno") +
  ggtitle("Decomposizione SEATS per log(PM10) a Bergamo")

#Serie destagionalizzata
autoplot(log_PM10B_ts,series = "Original") + 
  autolayer(PM10B_SEATS_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione SEATS per log(PM10) a Bergamo",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(log_PM10B_ts,series = "Original") + 
  autolayer(PM10B_SEATS_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione SEATS per log(PM10) a Bergamo",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione STL
dec_PM10B_STL <- log_PM10B_ts %>% 
  stl(t.window=25, s.window="periodic")
dec_PM10B_STL

trend_PM10B_STL <- trendcycle(dec_PM10B_STL)
seas_PM10B_STL <- seasonal(dec_PM10B_STL)
res_PM10B_STL <- remainder(dec_PM10B_STL)

#Istogramma residui
ggplot(data = res_PM10B_STL, aes(x = res_PM10B_STL)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_PM10B_STL,na.rm=T),
                            sd = sd(res_PM10B_STL,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di log(PM10) a Bergamo Città",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_PM10B_STL)

PM10B_STL_destag <- seasadj(dec_PM10B_STL)
PM10B_STL_detrend <- log_PM10B_ts - trend_PM10B_STL

q3 <- autoplot(dec_PM10B_STL) + 
  xlab("Anno") +
  ggtitle("Decomposizione STL per log(PM10) a Bergamo")

#Serie destagionalizzata
autoplot(PM10_Bergamo_ts,series = "Original") + 
  autolayer(PM10B_STL_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione STL per log(PM10) a Bergamo",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(log_PM10B_ts,series = "Original") + 
  autolayer(PM10B_STL_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione STL per log(PM10) a Bergamo",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Confronto tra i metodi di decomposizione
q4 <- autoplot(log_PM10B_ts,series = "Original") + 
  autolayer(PM10B_X11_destag,series = "X11", size=1.01) + 
  autolayer(PM10B_SEATS_destag,series = "SEATS", size=1.01) + 
  autolayer(PM10B_STL_destag,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Destagionalizzazione con vari metodi di decomposizione", subtitle = "log(PM10) a Bergamo Città")

autoplot(log_PM10B_ts,series = "Original") + 
  autolayer(PM10B_X11_detrend,series = "X11", size=1.01) + 
  autolayer(PM10B_SEATS_detrend,series = "SEATS", size=1.01) + 
  autolayer(PM10B_STL_detrend,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Detrendizzazione con vari metodi di decomposizione", subtitle = "log(PM10) a Bergamo Città")

ggarrange(q1,q2,q3,q4, nrow = 2, ncol = 2)


# 3.4. Variabile PM10 a Malpensa -------------------------------------------

#Decomposizione X11
library(seasonal)

( dec_PM10M_X11 <- PM10_Malpensa_ts %>% 
    seas(x11 = "") )
#Dall'output emerge che è una componente MA(1) non stagionale e poi una componente stagionale sulla parte a media mobile

#Estraiamo le tre componenti
trend_PM10M_X11 <- trendcycle(dec_PM10M_X11)
seas_PM10M_X11 <- seasonal(dec_PM10M_X11)
res_PM10M_X11 <- remainder(dec_PM10M_X11)

#Istogramma residui
ggplot(data = res_PM10M_X11, aes(x = res_PM10M_X11)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_PM10M_X11,na.rm=T),
                            sd = sd(res_PM10M_X11,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di PM10 a Milano Malpensa",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_PM10M_X11)

PM10M_X11_destag <- seasadj(dec_PM10M_X11) #serie destagionalizzata
PM10M_X11_detrend <- PM10_Malpensa_ts - trend_PM10M_X11 #serie detrendizzata

q1 <- autoplot(dec_PM10M_X11) + 
  xlab("Anno") +
  ggtitle("Decomposizione X-11 per PM10 a Milano Malpensa")

autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_X11_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione X11 per PM10 a Malpensa",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_X11_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione X11 per PM10 a Malpensa",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione SEATS
dec_PM10M_SEATS <- PM10_Malpensa_ts %>% 
  seas()
dec_PM10M_SEATS

trend_PM10M_SEATS <- trendcycle(dec_PM10M_SEATS)
seas_PM10M_SEATS <- seasonal(dec_PM10M_SEATS)
res_PM10M_SEATS <- remainder(dec_PM10M_SEATS)

#Istogramma residui
ggplot(data = res_PM10M_SEATS, aes(x = res_PM10M_SEATS)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_PM10M_SEATS,na.rm=T),
                            sd = sd(res_PM10M_SEATS,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di PM10 a Milano Malpensa",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_PM10M_SEATS)

PM10M_SEATS_destag <- seasadj(dec_PM10M_SEATS)
PM10M_SEATS_detrend <- PM10_Malpensa_ts - trend_PM10M_SEATS

q2 <- autoplot(dec_PM10M_SEATS) + 
  xlab("Anno") +
  ggtitle("Decomposizione SEATS per PM10 a Malpensa")

#Serie destagionalizzata
autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_SEATS_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione SEATS per PM10 a Malpensa",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_SEATS_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione SEATS per PM10 a Malpensa",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Decomposizione STL
dec_PM10M_STL <- PM10_Malpensa_ts %>% 
  stl(t.window=25, s.window="periodic")
dec_PM10M_STL

trend_PM10M_STL <- trendcycle(dec_PM10M_STL)
seas_PM10M_STL <- seasonal(dec_PM10M_STL)
res_PM10M_STL <- remainder(dec_PM10M_STL)

#Istogramma residui
ggplot(data = res_PM10M_STL, aes(x = res_PM10M_STL)) +
  geom_histogram(aes(y =..density..), 
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) +  
  stat_function(fun = dnorm,
                args = list(mean = mean(res_PM10M_STL,na.rm=T),
                            sd = sd(res_PM10M_STL,na.rm = T)), 
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       subtitle = "Concentrazione di PM10 a Milano Malpensa",
       x = "NO2 (µg/m³)",
       y = "Densità") + 
  #legenda
  scale_color_manual("Curve", #titolo legenda
                     values = color, #valori da prendere dal vettore color
                     breaks = c("Dens","Norm"), #indico i colori da considerare del vettore
                     labels = c("KDE","Gaussiana")) #etichetta associata a Dens e Norm

jarque.bera.test(res_PM10M_STL)

PM10M_STL_destag <- seasadj(dec_PM10M_STL)
PM10M_STL_detrend <- PM10_Malpensa_ts - trend_PM10M_STL

q3 <- autoplot(dec_PM10M_STL) + 
  xlab("Anno") +
  ggtitle("Decomposizione STL per PM10 a Malpensa")

#Serie destagionalizzata
autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_STL_destag,series = "Destag", size=1.01) + 
  labs(x="Anno",
       title = "Decomposizione STL per PM10 a Malpensa",
       subtitle = "Serie destagionalizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Destag"))

#Serie detrendizzata
autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_STL_detrend,series = "Detrend") + 
  labs(x="Anno",
       title = "Decomposizione STL per PM10 a Malpensa",
       subtitle = "Serie detrendizzata") + 
  scale_color_manual("Series",values=colori,breaks = c("Original","Detrend"))

#Confronto tra i metodi di decomposizione
q4 <- autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_X11_destag,series = "X11", size=1.01) + 
  autolayer(PM10M_SEATS_destag,series = "SEATS", size=1.01) + 
  autolayer(PM10M_STL_destag,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Destagionalizzazione con vari metodi di decomposizione", subtitle = "PM10 a Milano Malpensa")

autoplot(PM10_Malpensa_ts,series = "Original") + 
  autolayer(PM10M_X11_detrend,series = "X11", size=1.01) + 
  autolayer(PM10M_SEATS_detrend,series = "SEATS", size=1.01) + 
  autolayer(PM10M_STL_detrend,series = "STL", size=1.01) + 
  labs(x="Anno",
       title = "Detrendizzazione con vari metodi di decomposizione", subtitle = "PM10 a Milano Malpensa")

ggarrange(q1,q2,q3,q4, nrow = 2, ncol = 2)

# 4. Regressione ----------------------------------------------------------
library(GGally)

#Preparo il dataset per fare la regressione
Dati_regr <- Dataset %>%
  mutate(Data = yearmonth(ymd(Data)),
         Nome_staz = case_when(Nome_staz == "Ferno Via Di Dio" ~ "Malpensa",
                               Nome_staz == "Bergamo Via Meucci"~ "Bergamo",
                               TRUE ~ Nome_staz)) %>%
  filter(Nome_staz %in% c("Malpensa", "Bergamo")) %>%
  select(Data, Nome_staz, NO2, PM10, Temperatura, Umidita_relativa, Pioggia_cum) %>%
  as_tsibble(index = Data, key = Nome_staz)

# Scatterplot
fn_add_lm_loess <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

Dati_regr %>% 
  ggpairs(columns = c("NO2","PM10","Temperatura", "Pioggia_cum", "Umidita_relativa"),
          lower = list(continuous = fn_add_lm_loess))


# 4.1. Variabile NO2 ------------------------------------------------------

#Costruiamo i tre diversi modelli di regressione per NO2
m1 <- lm(formula = NO2 ~ Temperatura + Pioggia_cum, data = Dati_regr) 
m2 <- lm(formula = NO2 ~ Temperatura, data = Dati_regr) 
m3 <- lm(formula = NO2 ~ Pioggia_cum, data = Dati_regr)

perf_cv_m1 <- CV(m1) 
perf_cv_m2 <- CV(m2)
perf_cv_m3 <- CV(m3)
perf_cv <- as.data.frame(rbind(perf_cv_m1,perf_cv_m2,perf_cv_m3))
cbind(Model = c("M1","M2","M3"),perf_cv)

summary(m1)
#Analisi residui
checkresiduals(m1)

# 4.2. Variabile PM10 -----------------------------------------------------

#Costruiamo i tre diversi modelli di regressione per il PM10
n1 <- lm(formula = PM10 ~ Temperatura + Pioggia_cum, data = Dati_regr) 
n2 <- lm(formula = PM10 ~ Temperatura, data = Dati_regr) 
n3 <- lm(formula = PM10 ~ Pioggia_cum, data = Dati_regr)

perf_cv_n1 <- CV(n1) 
perf_cv_n2 <- CV(n2)
perf_cv_n3 <- CV(n3)
perf_cv <- as.data.frame(rbind(perf_cv_n1,perf_cv_n2,perf_cv_n3))
cbind(Model = c("M1","M2","M3"),perf_cv)

summary(n1)

#Analisi residui
checkresiduals(n1)

# 5. Modellistica ---------------------------------------------------------

colore <- c("Dens"="#FF0000","Norm"="blue",
        "Original"="black","fit_2stage" = "blue","fit_SARIMA"="green","fit_regARIMA" = "orange",
        "fit_con_COVID" = "blue", "fit_senza_COVID" = "red")

Dati_regARIMA <- Dataset %>%
  mutate(Data = yearmonth(ymd(Data)),
         Nome_staz = case_when(Nome_staz == "Ferno Via Di Dio" ~ "Malpensa",
                               Nome_staz == "Bergamo Via Meucci"~ "Bergamo",
                               TRUE ~ Nome_staz)) %>%
  filter(Nome_staz %in% c("Malpensa", "Bergamo")) %>%
  select(Data, Nome_staz, NO2, PM10, Temperatura, Pioggia_cum) %>%
  as_tsibble(index = Data, key = Nome_staz) %>%
  ts_ts()

Bergamo_Temperatura <- Dati_regARIMA[,5]
Bergamo_Piogge <- Dati_regARIMA[,7]
Malpensa_Temperatura <- Dati_regARIMA[,6]
Malpensa_Piogge <- Dati_regARIMA[,8]

# 5.1. Variabile NO2 Bergamo Città ----------------------------------------

# 5.1.1. ARIMA ------------------------------------------------------------

#Destagionalizzazione
k1 <- tslm(NO2_Bergamo_ts ~ trend + fourier(NO2_Bergamo_ts,K = 1))
k2 <- tslm(NO2_Bergamo_ts ~ trend + fourier(NO2_Bergamo_ts,K = 2))
k3 <- tslm(NO2_Bergamo_ts ~ trend + fourier(NO2_Bergamo_ts,K = 3))
k4 <- tslm(NO2_Bergamo_ts ~ trend + fourier(NO2_Bergamo_ts,K = 4))
k5 <- tslm(NO2_Bergamo_ts ~ trend + fourier(NO2_Bergamo_ts,K = 5))
k6 <- tslm(NO2_Bergamo_ts ~ trend + fourier(NO2_Bergamo_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)]
#Il miglior modello è M1.

#Residui
k1$residuals

#Scelta del modello ottimo
ARIMA_opt <- auto.arima(y = k1$residuals, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T)
ARIMA_opt

#Significatività del coefficiente
ARIMA_opt$coef/(sqrt(diag(vcov(ARIMA_opt))))

#Calcolo R2
(cor(ARIMA_opt$fitted, NO2_Bergamo_ts))^2


# 5.1.2. SARIMA -----------------------------------------------------------

#Modello ottimo
SARIMA_opt <- auto.arima(y = NO2_Bergamo_ts, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                         stationary = F,ic = "aicc")
SARIMA_opt

#Significatività coefficienti
SARIMA_opt$coef/((sqrt(diag(vcov(SARIMA_opt)))))

#R2
(cor(SARIMA_opt$fitted, NO2_Bergamo_ts))^2


# 5.1.3. regARIMA ---------------------------------------------------------

regARIMA_opt <- auto.arima(y = NO2_Bergamo_ts, xreg = cbind(Bergamo_Temperatura, Bergamo_Piogge),
                           max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                           stationary = F,ic = "aicc")
regARIMA_opt

#Significatività coefficienti
regARIMA_opt$coef/((sqrt(diag(vcov(regARIMA_opt)))))

#R2
(cor(regARIMA_opt$fitted, NO2_Bergamo_ts))^2

#Modello migliore: SARIMA
#Dal momento che il modello migliore è il SARIMA, proseguiamo con l'analisi delle innovazioni su quello.

innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "NO2 (μg/m3)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = colore,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "NO2 (μg/m3)")
p4 <- gglagplot(innov,set.lags = c(1,12,24,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))

#Calcolo i valori previsti/fittati
SARIMA_opt$fitted

#Valuto fitting ai dati originali
autoplot(NO2_Bergamo_ts,series = "Original", size=1.01) + 
  autolayer(SARIMA_opt$fitted,series = "fit_SARIMA", size=1.01) + 
  labs(title = "Serie storica") + 
  scale_color_manual("Series",values=colore,breaks = c("Original","fit_SARIMA"))


# 5.2. Variabile NO2 a Milano Malpensa ------------------------------------


# 5.2.1. ARIMA ------------------------------------------------------------

#Destagionalizzazione
k1 <- tslm(NO2_Malpensa_ts ~ trend + fourier(NO2_Malpensa_ts,K = 1))
k2 <- tslm(NO2_Malpensa_ts ~ trend + fourier(NO2_Malpensa_ts,K = 2))
k3 <- tslm(NO2_Malpensa_ts ~ trend + fourier(NO2_Malpensa_ts,K = 3))
k4 <- tslm(NO2_Malpensa_ts ~ trend + fourier(NO2_Malpensa_ts,K = 4))
k5 <- tslm(NO2_Malpensa_ts ~ trend + fourier(NO2_Malpensa_ts,K = 5))
k6 <- tslm(NO2_Malpensa_ts ~ trend + fourier(NO2_Malpensa_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)]
#Il miglior modello è M2.

#Residui
k2$residuals

#Scelta del modello ottimo
ARIMA_opt <- auto.arima(y = k2$residuals, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T)
ARIMA_opt

#Significatività del coefficiente
ARIMA_opt$coef/(sqrt(diag(vcov(ARIMA_opt))))

#Calcolo R2
(cor(ARIMA_opt$fitted, NO2_Malpensa_ts))^2

# 5.2.2. SARIMA -----------------------------------------------------------

#Modello ottimo
SARIMA_opt <- auto.arima(y = NO2_Malpensa_ts, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                         stationary = F,ic = "aicc")
SARIMA_opt

#Significatività coefficienti
SARIMA_opt$coef/((sqrt(diag(vcov(SARIMA_opt)))))

#R2
(cor(SARIMA_opt$fitted, NO2_Malpensa_ts))^2


# 5.2.3. regARIMA ---------------------------------------------------------

regARIMA_opt <- auto.arima(y = NO2_Malpensa_ts, xreg = cbind(Malpensa_Temperatura, Malpensa_Piogge),
                           max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                           stationary = F,ic = "aicc")
regARIMA_opt

#Significatività coefficienti
regARIMA_opt$coef/((sqrt(diag(vcov(regARIMA_opt)))))

#R2
(cor(regARIMA_opt$fitted, NO2_Malpensa_ts))^2

#Modello migliore: SARIMA
#Dal momento che il modello migliore è il SARIMA, proseguiamo con l'analisi delle innovazioni su quello.

innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "NO2 (μg/m3)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = colore,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "NO2 (μg/m3)")
p4 <- gglagplot(innov,set.lags = c(1,12,24,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))

#Calcolo i valori previsti/fittati
SARIMA_opt$fitted

#Valuto fitting ai dati originali
autoplot(NO2_Malpensa_ts,series = "Original", size=1.01) + 
  autolayer(SARIMA_opt$fitted,series = "fit_SARIMA", size=1.01) + 
  labs(title = "Serie storica") + 
  scale_color_manual("Series",values=colore,breaks = c("Original","fit_SARIMA"))


# 5.3. Variabile PM10 Bergamo Città ---------------------------------------


# 5.3.1. ARIMA ------------------------------------------------------------

#Destagionalizzazione
k1 <- tslm(PM10_Bergamo_ts ~ trend + fourier(PM10_Bergamo_ts,K = 1))
k2 <- tslm(PM10_Bergamo_ts ~ trend + fourier(PM10_Bergamo_ts,K = 2))
k3 <- tslm(PM10_Bergamo_ts ~ trend + fourier(PM10_Bergamo_ts,K = 3))
k4 <- tslm(PM10_Bergamo_ts ~ trend + fourier(PM10_Bergamo_ts,K = 4))
k5 <- tslm(PM10_Bergamo_ts ~ trend + fourier(PM10_Bergamo_ts,K = 5))
k6 <- tslm(PM10_Bergamo_ts ~ trend + fourier(PM10_Bergamo_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)]
#Il miglior modello è M3.

#Residui
k3$residuals

#Scelta del modello ottimo
ARIMA_opt <- auto.arima(y = k3$residuals, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T)
ARIMA_opt

#Calcolo R2
(cor(ARIMA_opt$fitted, PM10_Bergamo_ts))^2 #è un white noise

# 5.3.2. SARIMA -----------------------------------------------------------

#Modello ottimo
SARIMA_opt <- auto.arima(y = PM10_Bergamo_ts, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                         stationary = F,ic = "aicc")
SARIMA_opt

#Significatività coefficienti
SARIMA_opt$coef/((sqrt(diag(vcov(SARIMA_opt)))))

#R2
(cor(SARIMA_opt$fitted, PM10_Bergamo_ts))^2


# 5.3.3. regARIMA ---------------------------------------------------------

regARIMA_opt <- auto.arima(y = PM10_Bergamo_ts, xreg = cbind(Bergamo_Temperatura,Bergamo_Piogge),
                           max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                           stationary = F,ic = "aicc")
regARIMA_opt

#Significatività coefficienti
regARIMA_opt$coef/((sqrt(diag(vcov(regARIMA_opt)))))

#R2
(cor(regARIMA_opt$fitted, PM10_Bergamo_ts))^2


#Modello migliore: SARIMA
#Dal momento che il modello migliore è il SARIMA, proseguiamo con l'analisi delle innovazioni su quello.


innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "NO2 (μg/m3)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = colore,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "NO2 (μg/m3)")
p4 <- gglagplot(innov,set.lags = c(1,12,24,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))

#Calcolo i valori previsti/fittati
SARIMA_opt$fitted

#Valuto fitting ai dati originali
autoplot(PM10_Bergamo_ts,series = "Original", size=1.01) + 
  autolayer(SARIMA_opt$fitted,series = "fit_SARIMA", size=1.01) + 
  labs(title = "Serie storica") + 
  scale_color_manual("Series",values=colore,breaks = c("Original","fit_SARIMA"))



# 5.4. Variabile PM10 a Malpensa ------------------------------------------


# 5.4.1. ARIMA ------------------------------------------------------------

#Destagionalizzazione
k1 <- tslm(PM10_Malpensa_ts ~ trend + fourier(PM10_Malpensa_ts,K = 1))
k2 <- tslm(PM10_Malpensa_ts ~ trend + fourier(PM10_Malpensa_ts,K = 2))
k3 <- tslm(PM10_Malpensa_ts ~ trend + fourier(PM10_Malpensa_ts,K = 3))
k4 <- tslm(PM10_Malpensa_ts ~ trend + fourier(PM10_Malpensa_ts,K = 4))
k5 <- tslm(PM10_Malpensa_ts ~ trend + fourier(PM10_Malpensa_ts,K = 5))
k6 <- tslm(PM10_Malpensa_ts ~ trend + fourier(PM10_Malpensa_ts,K = 6))
arm_comp <- data.frame(cbind(Model=c("M1","M2","M3","M4","M5","M6"),
                             rbind(round(CV(k1),2),round(CV(k2),2),round(CV(k3),2),round(CV(k4),2),round(CV(k5),2),round(CV(k6),2))))
arm_comp

arm_comp$Model[which.min(arm_comp$AIC)] 
arm_comp$Model[which.min(arm_comp$AICc)] 
arm_comp$Model[which.min(arm_comp$BIC)] 
arm_comp$Model[which.max(arm_comp$AdjR2)]
#Il miglior modello è M2.

#Residui
k2$residuals

#Scelta del modello ottimo
ARIMA_opt <- auto.arima(y = k2$residuals, max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,stationary = T)
ARIMA_opt

#Calcolo R2
(cor(ARIMA_opt$fitted, PM10_Malpensa_ts))^2 #White Noise


# 5.4.2. SARIMA -----------------------------------------------------------

#Modello ottimo
SARIMA_opt <- auto.arima(y = PM10_Malpensa_ts, max.p = 3, max.q = 3, seasonal = T, parallel = F, stepwise = F,
                         stationary = F,ic = "aicc")
SARIMA_opt

#Significatività coefficienti
SARIMA_opt$coef/((sqrt(diag(vcov(SARIMA_opt)))))

#R2
(cor(SARIMA_opt$fitted, PM10_Malpensa_ts))^2


# 5.4.3. regARIMA ---------------------------------------------------------

regARIMA_opt <- auto.arima(y = PM10_Malpensa_ts, xreg = cbind(Malpensa_Temperatura, Malpensa_Piogge),
                           max.p = 3, max.q = 3, seasonal = F, parallel = F, stepwise = F,
                           stationary = F,ic = "aicc")
regARIMA_opt

#Significatività coefficienti
regARIMA_opt$coef/((sqrt(diag(vcov(regARIMA_opt)))))

#R2
(cor(regARIMA_opt$fitted, PM10_Malpensa_ts))^2


#Modello migliore: SARIMA
#Dal momento che il modello migliore è il SARIMA, proseguiamo con l'analisi delle innovazioni su quello.

innov <- SARIMA_opt$residuals
p1 <- autoplot(innov) + 
  labs(title = "Serie storica")
p2 <- as_tsibble(innov) %>%
  ggplot(data = ., aes(x = value)) + 
  geom_histogram(aes(y =..density..),
                 colour="white",
                 fill = "black") +
  geom_density(aes(col="Dens"),size=1.1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(innov,na.rm=T),
                            sd = sd(innov,na.rm = T)),
                aes(col="Norm"),
                size=1.1) + 
  labs(title = "Istogramma",
       x = "NO2 (μg/m3)",
       y = "Densità") + 
  scale_color_manual("Curve",
                     values = colore,
                     breaks = c("Dens","Norm"),
                     labels = c("KDE","Gaussiana"))
p3 <- as_tsibble(innov) %>%
  ggplot(aes(x = value)) + 
  geom_boxplot(outlier.colour="red",
               outlier.shape=8,
               outlier.size=4,
               notch=F) + 
  labs(title = "Box-plot",
       x = "NO2 (μg/m3)")
p4 <- gglagplot(innov,set.lags = c(1,12,24,36)) +
  theme(legend.position = "") + 
  labs(title = "Lag-plots")
p5 <- ggAcf(innov) + 
  labs(title = "ACF")
p6 <- ggPacf(innov) + 
  labs(title = "PACF")
p <- ggarrange(p1,p2,p3,p4,p5,p6,ncol = 2,nrow = 3)
print(annotate_figure(p,
                      top = text_grob("Innovazioni del modello SARIMA ottimo",
                                      color = "red", face = "bold", size = 14)))

#Calcolo i valori previsti/fittati
SARIMA_opt$fitted 

#Valuto fitting ai dati originali
autoplot(PM10_Malpensa_ts,series = "Original", size=1.01) + 
  autolayer(SARIMA_opt$fitted,series = "fit_SARIMA", size=1.01) + 
  labs(title = "Serie storica") + 
  scale_color_manual("Series",values=colore,breaks = c("Original","fit_SARIMA"))

