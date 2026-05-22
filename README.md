# Time-Series-Air-Quality-Lombardia
# Analisi Comparativa dei Livelli di Inquinamento: Bergamo Città vs Milano Malpensa

Progetto realizzato per l'esame di **Introduzione alle Serie Storiche Economiche** (A.A. 2024-2025) presso l'Università degli Studi di Milano-Bicocca.

## 📝 Descrizione del Progetto
L'obiettivo dello studio è analizzare e confrontare l'andamento temporale (2016-2022) dei livelli di Biossido di Azoto ($NO_2$) e Polveri Sottili ($PM_{10}$) in due contesti differenti della Lombardia: una zona urbana (Bergamo) e una zona aeroportuale (Ferno - Milano Malpensa), valutando anche l'impatto del lockdown del 2020 e delle variabili meteorologiche.

## 🛠️ Tecnologie Utilizzate
* **Linguaggio:** R
* **Librerie principali:** `tseries`, `forecast`, `tsibble`, `lubridate`, `tsbox`, `tidyverse`, `ggplot2`, `ggpubr`
* **Modellistica:** Decomposizione classica (SEATS/X-11), Regressione con covariate, Modelli ARIMA, SARIMA e regARIMA.

## 📂 Contenuto della Repository
* `Codice_Analisi.R`: Script R completo con le procedure di pre-processing, test di stazionarietà e modellistica.
* `Report_Finale.pdf`: Il saggio accademico completo con l'interpretazione statistica e i grafici.
* `Slide_Presentazione.html`: Le slide riassuntive del progetto.
