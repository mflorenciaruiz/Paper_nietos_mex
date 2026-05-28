install.packages(
  "synthdid",
  repos = c("https://skranz.r-universe.dev", "https://cloud.r-project.org")
)

library(foreign)
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(sf)
library(ggplot2)
library(psych)
library(stringr)
library(writexl)
library(fixest)
library(zoo)
library(modelsummary)
library(synthdid)

# -------------------- #
# 0. Data
# -------------------- #

setwd("/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/México/Paper_nietos_mex")

estimacion_yr     <- read_csv("Data Out/estimacion_remesas_yr2.csv")
estimacion_tri    <- read_csv("Data Out/estimacion_remesas_yr.csv")
estimacion_yr_eb  <- read_dta("Data Out/estimacion_remesas_yr_coh_eb.dta")
estimacion_yr_coh <- read_csv("Data Out/estimacion_remesas_yr_coh2.csv")

# -------------------- #
# 1. Estimaciones
# -------------------- #
{
### Remesas ###

## Anual ##

# Estimación con año base 2022, todos los años pre completos, total remesas, share spanish born
feols(total_remesas ~ share_spanish_born:post | inegi + year, data = estimacion_yr)
event0 <- feols(total_remesas ~ i(year, share_spanish_born, "2022") | 
                inegi + year, data = estimacion_yr)
iplot(event0) 

png("Output/Mexico/event0.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event0, main ="Effect on remittances (USD)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2022, todos los años pre completos, total remesas, log spanish born
feols(total_remesas ~ log_spanish_born_avg:post | inegi + year, data = estimacion_yr)
event1 <- feols(total_remesas ~ i(year, log_spanish_born_avg, "2022") | 
                inegi + year, data = estimacion_yr)
iplot(event1) 

png("Output/Mexico/event1.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1, main ="Effect on remittances (USD)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2022, todos los años pre completos, log remesas, log spanish born
att2 <- feols(log_remesas ~ log_spanish_born_avg:post | inegi + year, data = estimacion_yr)
event2 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2022") | 
                inegi + year, data = estimacion_yr)
summary(event2)
iplot(event2) 

png("Output/Mexico/event2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, todos los años pre completos, log remesas, log spanish born
att3 <- feols(log_remesas ~ log_spanish_born_avg :post21 | inegi + year, data = estimacion_yr)
event3 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                inegi + year, data = estimacion_yr)
summary(event3)
iplot(event3) # mejor 

png("Output/Mexico/event3_v2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    #yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event3, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, cortando en 2018, log remesas, log spanish born
att4 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                inegi + year, data = estimacion_yr %>% filter(year>=2018))
summary(att4)
iplot(att4) # mejor (si no cortamos en 2018 hay diferencias)

# Estimación con año base 2022, cortando en 2018, log remesas, log spanish born
att5 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2022") | 
                inegi + year, data = estimacion_yr %>% filter(year>=2018))
summary(att5)
iplot(att5) # negativo consistente

# Estimación con año base 2021, todos los años pre completos, total remesas, log spanish born
event6 <- feols(total_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                inegi + year, data = estimacion_yr)
summary(event6)
iplot(event6) 

png("Output/Mexico/event6.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event6, main ="Effect on remittances (USD)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, todos los años pre completos, total remesas, share spanish born
event7 <- feols(total_remesas ~ i(year, share_spanish_born, "2021") | 
                inegi + year, data = estimacion_yr)
summary(event7)
iplot(event7) 

png("Output/Mexico/event7.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event7, main ="Effect on remittances (USD)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2022, cortando en 2018, log remesas, spanish born
event8 <- feols(log_remesas ~ i(year, spanish_born, "2022") | 
        inegi + year, data = estimacion_yr %>% filter(year>=2018))
summary(event8)
iplot(event8) # me gusta 

png("Output/Mexico/event8.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event8, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, todos los años pre completos, FE interactuados (mig US 2010 x Year), log remesas, log spanish born
att9 <- feols(log_remesas ~ log_spanish_born_avg :post21 | inegi + year + viv_emig_10[year], data = estimacion_yr)
event9 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                inegi + year + viv_emig_10[year], data = estimacion_yr)
summary(att9)
summary(event9)
iplot(event9) 

png("Output/Mexico/event9_v2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    #yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event9, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, cortando en 2016, FE interactuados (mig US 2010 x Year), log remesas, log spanish born
att10 <- feols(log_remesas ~ log_spanish_born_avg :post21 | inegi + year + viv_emig_10[year], data = estimacion_yr%>% filter(year>=2016))
event10 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                inegi + year + viv_emig_10[year], data = estimacion_yr %>% filter(year>=2016))
summary(att10)
summary(event10)
iplot(event10) # mejor

png("Output/Mexico/event10.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event10, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()


modelsummary(
  att10,
  output = "Output/Mexico/att10.docx",
  stars = TRUE,
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "log_spanish_born_avg:post21" = "Post × Log Spanish-born (average)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Migration × Time FE"),
    att8b = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses. * p<0.1, ** p<0.05, *** p<0.01."
)
             
# Estimación con año base 2021, cortando en 2016, FE interactuados (mig US 2000 x Year), log remesas, log spanish born
att11 <- feols(log_remesas ~ log_spanish_born_avg :post21 | inegi + year + viv_emig_00[year], data = estimacion_yr%>% filter(year>=2016))
event11 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                 inegi + year + viv_emig_00[year], data = estimacion_yr %>% filter(year>=2016))
summary(event11)
iplot(event11) # igual que con 10

png("Output/Mexico/event11.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event11, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, todos los años completos, FE interactuados (mig US 2000 x Year), log remesas, log spanish born
event12 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                 inegi + year + viv_emig_00[year], data = estimacion_yr)
summary(event12)
iplot(event12) # 3 años significativos en el pre

png("Output/Mexico/event12_v2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event12, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, todos los años completos, FE interactuados (intensidad mig 2010 x Year), log remesas, log spanish born
att13 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                 inegi + year + iaim_10[year], data = estimacion_yr)
summary(att13)
iplot(att13)

# Estimación con año base 2021, todos los años completos, FE interactuados (intensidad mig 2000 x Year), log remesas, log spanish born
att14 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                 inegi + year + iaim_00[year], data = estimacion_yr)
summary(att14)
iplot(att14) # ic muy grandes en el pre, dudoso

# Estimación con año base 2021, cortando en 2016, FE interactuados (remesas 2010 x Year), log remesas, log spanish born
att15 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                 inegi + year + viv_rem_10[year], data = estimacion_yr
                  %>% filter(year>=2016)
               )
summary(att15)
iplot(att15)

# Estimación con año base 2021, cortando en 2016, FE interactuados (remesas 2000 x Year), log remesas, log spanish born
att16 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                 inegi + year + viv_rem_00[year], data = estimacion_yr%>% filter(year>=2016))
summary(att16)
iplot(att16)

# Estimación con año base 2022, todos los años complejots, FE interactuados (mig US 2010 x Year), log remesas, log spanish born
event17 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2022") | 
                 inegi + year + viv_emig_10[year], data = estimacion_yr)
summary(event17)
iplot(event17) # negativo significativo pero mas o menos consistente

png("Output/Mexico/event17.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event17, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2022, todos los años complejots, FE interactuados (mig US 2000 x Year), log remesas, log spanish born
event18 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2022") | 
                 inegi + year + viv_emig_00[year], data = estimacion_yr)
summary(event18)
iplot(event18)

png("Output/Mexico/event18.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event18, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, cortando en 2016, log remesas, log spanish born
att19 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                inegi + year, data = estimacion_yr %>% filter(year>=2016))
summary(att19)
iplot(att19) 

# Estimación con año base 2021, todos los años completos, FE interactuados (mig US 2000 x Year), log remesas, spanish born
att20 <- feols(log_remesas ~ i(year, spanish_born, "2022") | 
                 inegi + year  +
                 viv_emig_00[year] , 
               data = estimacion_yr)
summary(att20)
iplot(att20)

# Estimación con año base 2021, cortando en 2016 y sacando 2022, FE interactuados (mig US 2010 x Year), log remesas, spanish born
att21 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                 inegi + year + viv_emig_10[year], data = estimacion_yr %>% 
                 filter(year>=2016 & year !=2022))
summary(att21)
iplot(att21) # validacion del 10?

# Estimación con año base 2021, log remesas, spanish born
event22 <- feols(log_remesas ~ i(year, spanish_born, "2021") | 
                  inegi + year, data = estimacion_yr)
summary(event22)
iplot(event22) 

png("Output/Mexico/event22_v2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event22, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2022, todos los años complejots, log remesas, spanish born
event23 <- feols(log_remesas ~ i(year, spanish_born, "2022") | 
                   inegi + year, data = estimacion_yr)
summary(event23)
iplot(event23) 

png("Output/Mexico/event23.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event23, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

feols(total_remesas ~ log_spanish_born_pooled :post | inegi + year, data = estimacion_yr)
feols(total_remesas ~ i(year, log_spanish_born_pooled, "2022") | 
        inegi + year, data = estimacion_yr)

# Sin estados frontera, estimación con año base 2021, todos los años pre completos, FE interactuados (mig US 2010 x Year), log remesas, log spanish born
estimacion_yr_nofrontera <- estimacion_yr %>%
  filter(!substr(inegi, 1, 2) %in% c("02", "26", "08", "05", "19", "28"))

att24 <- feols(log_remesas ~ log_spanish_born_avg :post21 | inegi + year + viv_emig_10[year], data = estimacion_yr_nofrontera)
event24 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                  inegi + year + viv_emig_10[year], data = estimacion_yr_nofrontera)
summary(att24)
summary(event24)
iplot(event24) 

png("Output/Mexico/event24_v2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event24, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Sin estados frontera, estimación con año base 2021, todos los años pre completos, FE interactuados (mig US 2000 x Year), log remesas, log spanish born
event25 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                   inegi + year + viv_emig_00[year], data = estimacion_yr_nofrontera)
summary(event25)
iplot(event25) 

# Sin estados frontera, estimación con año base 2021, todos los años pre completos, log remesas, log spanish born
event26 <- feols(log_remesas ~ i(year, log_spanish_born_avg, "2021") | 
                  inegi + year, data = estimacion_yr_nofrontera)
summary(event26)
iplot(event26)

# Exporto algunas especificaciones que me gustaron
modelsummary(
  list("(1)" = att3, "(2)" = att9, "(3)"=att24),
  output = "Output/Mexico/att_3_9_24.docx",
  stars = TRUE,
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "log_spanish_born_avg:post21" = "Post × Log Spanish-born (average)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Migration × Time FE", "Sample"),
    att3 = c("Yes", "Yes", "No", "Full"),
    att9 = c("Yes", "Yes", "Yes", "Full"),
    att24 = c("Yes", "Yes", "Yes", "No border states")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses. * p<0.1, ** p<0.05, *** p<0.01."
)

# Estimación con año base 2021, todos los años completos, FE interactuados (mig US 2010 x Year), log remesas, spanish born
att27 <- feols(log_remesas ~ i(year, spanish_born, "2021") | 
                 inegi + year  +
                 viv_emig_10[year] , 
               data = estimacion_yr)
summary(att27)
iplot(att27)

png("Output/Mexico/event27_v2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final
par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    #yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(att27, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Estimación con año base 2021, todos los años completos, FE interactuados (mig US 2000 x Year), log remesas, spanish born
att28 <- feols(log_remesas ~ i(year, spanish_born, "2021") | 
                 inegi + year  +
                 viv_emig_00[year] , 
               data = estimacion_yr)
summary(att28)
iplot(att28)

png("Output/Mexico/event28_v2.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final
par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    #yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(att28, main ="Effect on log(remittances)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

## Trimestral ##

# Estimacion trimestral, log remesas, cortando en 2018, trimestre Q4 2021
ref_q <- zoo::as.yearqtr("2021 Q4")

event1b <- feols(log_remesas ~ i(tq, log_spanish_born_avg, ref_q) | 
                inegi + tq, data = estimacion_tri %>% filter(year>=2018))
summary(event1b)
iplot(event1b)

png("Output/Mexico/event1b.png", width = 6.5, height = 4.5, units = "in", res = 300)
par(mar = c(3, 5, 3, 1), 
    cex.lab = 1.1, 
    cex.axis = 1.1, 
    bty = "l",
    yaxs = "i", 
    las = 1, 
    mgp = c(4, 1, 0)) # Aumentamos el primer valor de mgp (distancia del título)

iplot(event1b, 
      main = "Effect on log(remittances)", 
      xlab = "",      # Dejamos vacío el título aquí
      ci.col = "grey50",
      grid = FALSE,
      xaxt = "n")
# Añadimos el título del eje manualmente. 'line' controla qué tan lejos está del eje
title(xlab = "Quarters", line = 1.5, cex.lab = 1.1)
dev.off()

# Estimacion trimestral, total remesas, cortando en 2018, trimestre Q4 2021
att2b <- feols(total_remesas ~ i(tq, log_spanish_born_avg, ref_q) | 
                inegi + tq, data = estimacion_tri %>% filter(year>=2018))
summary(att2b)
iplot(att2b)

# Estimacion trimestral, log remesas, cortando en 2018, trimestre Q4 2021, log_spanish_born_pooled
att3b <- feols(log_remesas ~ i(tq, log_spanish_born_pooled, ref_q) | 
                inegi + tq, data = estimacion_tri %>% filter(year>=2018))
summary(att3b)
iplot(att3b)

# Estimacion trimestral, log remesas, cortando en 2018, trimestre Q4 2021, spanish born
att4b <- feols(log_remesas ~ i(tq, spanish_born, ref_q) | 
                inegi + tq, data = estimacion_tri %>% filter(year>=2018))
summary(att4b)
iplot(att4b)

estimacion_tri <- estimacion_tri %>%
  mutate(
    t = year*4 + (trimestre - 1)  # índice trimestral
  )
q3_21 <- 2021*4 + (3 - 1)
q4_21 <- 2021*4 + (4 - 1)
q3_22 <- 2022*4 + (3 - 1)

# Estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2000 x Year), trimestre Q3 2021, log spanish born, 
att5b <- feols(
  log_remesas ~ i(t, log_spanish_born_avg, ref = q3_21) |
    inegi + t + viv_emig_00[t],
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(att5b)
iplot(att5b)

# Estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2010 x Year), trimestre Q3 2021, log spanish born, 
att6b <- feols(
  log_remesas ~ i(t, log_spanish_born_avg, ref = q3_21) |
    inegi + t + viv_emig_10[t],
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(att6b)
iplot(att6b)

# Estimacion trimestral, log remesas, cortando en 2018, FE interactuados (intensidad mig 2010 x Year), trimestre Q3 2021, log spanish born, 
att7b <- feols(
  log_remesas ~ i(t, log_spanish_born_avg, ref = q3_21) |
    inegi + t + iaim_10[t] ,
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(att7b)
iplot(att7b)

# Estimacion trimestral, log remesas, cortando en 2018, trimestre Q4 2021, FE interactuados con migracion a eeuu 2000
event8b <- feols(
  log_remesas ~ i(tq, log_spanish_born_avg, ref = ref_q) |
    inegi + tq + viv_emig_00[t],
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(event8b)
iplot(event8b) # mejor

png("Output/Mexico/event8b.png", width = 6.5, height = 4.5, units = "in", res = 300)
par(mar = c(3, 5, 3, 1), 
    cex.lab = 1.1, 
    cex.axis = 1.1, 
    bty = "l",
    yaxs = "i", 
    las = 1, 
    mgp = c(4, 1, 0)) # Aumentamos el primer valor de mgp (distancia del título)

iplot(event8b, 
      main = "Effect on log(remittances)", 
      xlab = "",      # Dejamos vacío el título aquí
      ci.col = "grey50",
      grid = FALSE,
      xaxt = "n")
# Añadimos el título del eje manualmente. 'line' controla qué tan lejos está del eje
title(xlab = "Quarters", line = 1.5, cex.lab = 1.1)
dev.off()

# Estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2010 x Year), trimestre Q3 2022, log spanish born, 
q3_22 <- zoo::as.yearqtr("2022 Q3")
event9b <- feols(
  log_remesas ~ i(tq, log_spanish_born_avg, ref = q3_22) |
    inegi + tq + viv_emig_10[t],
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(event9b)
iplot(event9b)

png("Output/Mexico/event9b.png", width = 6.5, height = 4.5, units = "in", res = 300)
par(mar = c(3, 5, 3, 1), 
    cex.lab = 1.1, 
    cex.axis = 1.1, 
    bty = "l",
    yaxs = "i", 
    las = 1, 
    mgp = c(4, 1, 0)) # Aumentamos el primer valor de mgp (distancia del título)

iplot(event9b, 
      main = "Effect on log(remittances)", 
      xlab = "",      # Dejamos vacío el título aquí
      ci.col = "grey50",
      grid = FALSE,
      xaxt = "n")
# Añadimos el título del eje manualmente. 'line' controla qué tan lejos está del eje
title(xlab = "Quarters", line = 1.5, cex.lab = 1.1)
dev.off()

# Estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2000 quintiles x Year), trimestre Q4 2021, log spanish born
att10b <- feols(
  log_remesas ~ i(t, log_spanish_born_avg, ref = q4_21) |
    inegi + t + viv_emig_00_q5[t],
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
iplot(att10b)

# Estimacion trimestral, log remesas, cortando en 2018, FE mig US 2010, trimestre Q4 2021, log spanish born
att11b <- feols(
  log_remesas ~ i(t, log_spanish_born_avg, ref = q4_21) |
    inegi + t + viv_emig_10,
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(att11b)
iplot(att11b)

# Estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2010 x Year), trimestre Q4 2021, log spanish born
event12b <- feols(
  log_remesas ~ i(tq, log_spanish_born_avg, ref = ref_q) |
    inegi + tq + viv_emig_10[t],
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(event12b)
iplot(event12b)

png("Output/Mexico/event12b.png", width = 6.5, height = 4.5, units = "in", res = 300)
par(mar = c(3, 5, 3, 1), 
    cex.lab = 1.1, 
    cex.axis = 1.1, 
    bty = "l",
    yaxs = "i", 
    las = 1, 
    mgp = c(4, 1, 0)) # Aumentamos el primer valor de mgp (distancia del título)

iplot(event12b, 
      main = "Effect on log(remittances)", 
      xlab = "",      # Dejamos vacío el título aquí
      ci.col = "grey50",
      grid = FALSE,
      xaxt = "n")
# Añadimos el título del eje manualmente. 'line' controla qué tan lejos está del eje
title(xlab = "Quarters", line = 1.5, cex.lab = 1.1)
dev.off()

# sin estados frontera, estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2010 x Year), trimestre Q4 2021, log spanish born
estimacion_tri_nofrontera <- estimacion_tri %>%
  filter(!substr(inegi, 1, 2) %in% c("02", "26", "08", "05", "19", "28"))

event13b <- feols(
  log_remesas ~ i(tq, log_spanish_born_avg, ref = ref_q) |
    inegi + tq + viv_emig_10[t] ,
  data = estimacion_tri_nofrontera %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(event13b)
iplot(event13b)

png("Output/Mexico/event13b.png", width = 6.5, height = 4.5, units = "in", res = 300)
par(mar = c(3, 5, 3, 1), 
    cex.lab = 1.1, 
    cex.axis = 1.1, 
    bty = "l",
    yaxs = "i", 
    las = 1, 
    mgp = c(4, 1, 0)) # Aumentamos el primer valor de mgp (distancia del título)

iplot(event13b, 
      main = "Effect on log(remittances)", 
      xlab = "",      # Dejamos vacío el título aquí
      ci.col = "grey50",
      grid = FALSE,
      xaxt = "n")
# Añadimos el título del eje manualmente. 'line' controla qué tan lejos está del eje
title(xlab = "Quarters", line = 1.5, cex.lab = 1.1)
dev.off()

# sin estados frontera, estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2000 x Year), trimestre Q4 2021, log spanish born
event14b <- feols(
  log_remesas ~ i(tq, log_spanish_born_avg, ref = ref_q) |
    inegi + tq + viv_emig_00[t] ,
  data = estimacion_tri_nofrontera %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(event14b)
iplot(event14b)

png("Output/Mexico/event14b.png", width = 6.5, height = 4.5, units = "in", res = 300)
par(mar = c(3, 5, 3, 1), 
    cex.lab = 1.1, 
    cex.axis = 1.1, 
    bty = "l",
    yaxs = "i", 
    las = 1, 
    mgp = c(4, 1, 0)) # Aumentamos el primer valor de mgp (distancia del título)

iplot(event14b, 
      main = "Effect on log(remittances)", 
      xlab = "",      # Dejamos vacío el título aquí
      ci.col = "grey50",
      grid = FALSE,
      xaxt = "n")
# Añadimos el título del eje manualmente. 'line' controla qué tan lejos está del eje
title(xlab = "Quarters", line = 1.5, cex.lab = 1.1)
dev.off()

# Estimacion trimestral, log remesas, cortando en 2018, FE interactuados (mig US 2000 x Year), trimestre Q3 2022, log spanish born, 
event15b <- feols(
  log_remesas ~ i(tq, log_spanish_born_avg, ref = q3_22) |
    inegi + tq + viv_emig_00[t],
  data = estimacion_tri %>% filter(year >= 2018),
  cluster = ~inegi
)
summary(event15b)
iplot(event15b)
}

# ---------------------------- #
# 2. Estimaciones por cohorte
# ---------------------------- #
{
### Remesas ###

## Anual ##

# Estimación con año base 2021, todos los años pre completos, log remesas
att1_c <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                     inegi + year, data = estimacion_yr_coh)

event1_c_36 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                  inegi + year, data = estimacion_yr_coh)
iplot(event1_c_36) 

event1_c_55 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                    inegi + year, data = estimacion_yr_coh)
iplot(event1_c_55) 

# Estimación con año base 2021, todos los años completos, log remesas, FE interactuados (mig US 2010 x Year)
att2_c <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                     inegi + year + viv_emig_10[year], data = estimacion_yr_coh) # al 10% en ambos

event2_c_36 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                       inegi + year +  + viv_emig_10[year], data = estimacion_yr_coh)
iplot(event2_c_36) 

event2_c_55 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                       inegi + year +  + viv_emig_10[year], data = estimacion_yr_coh)
iplot(event2_c_55) 

# Estimación con año base 2021, todos los años completos, log remesas, FE interactuados (mig US 2000 x Year)
att3_c <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                  inegi + year + viv_emig_00[year], data = estimacion_yr_coh) # no da siginficativo

event3_c_36 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                       inegi + year +  + viv_emig_00[year], data = estimacion_yr_coh)
iplot(event3_c_36) 

event3_c_55 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                       inegi + year +  + viv_emig_00[year], data = estimacion_yr_coh)
iplot(event3_c_55) 

# controles: 
  # Migracion a eeuu anual o en un año dado (“dejá que los municipios que ya eran migratorios a EE. UU. en 2000 tengan una dinámica temporal distinta de las remesas.”)
  # reveer controles trimestrales, los datos de emig eeuu mex son anuales y los interactuo con fe trimestrañes

# Media de remesas pre tratamiento
estimacion_yr_coh_pre <- estimacion_yr_coh %>% 
  filter(year < 2021 , spanish_presence_1956_1978 == 0 , spanish_presence_1936_1955 == 0) 
Hmisc::describe(estimacion_yr_coh_pre$log_remesas)
Hmisc::describe(estimacion_yr_coh_pre$total_remesas)
  
}

# ---------------------------- #
# 3. Estimaciones EB por cohorte
# ---------------------------- #

## 3.1 Estrategia 1 - Alternativa de pesos 8 ##
{
# Estimación con año base 2021, todos los años pre completos, log remesas
att1_c_w118 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                  inegi + year, data = estimacion_yr_eb, weights = ~w11_8)
summary(att1_c_w118)

att1_c_w128 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                        inegi + year, data = estimacion_yr_eb, weights = ~w12_8)
summary(att1_c_w128)

  # Event Study
event1_c_36_w118 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                       inegi + year, data = estimacion_yr_eb, weights = ~w11_8)
iplot(event1_c_36_w118) 

event1_c_55_w128 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                       inegi + year, data = estimacion_yr_eb, weights = ~w12_8)
iplot(event1_c_55_w128) 

# Estimación con año base 2021, todos los años completos, log remesas, FE interactuados (mig US 2010 x Year)
att2_c_w118 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                  inegi + year + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w11_8)
summary(att2_c_w118)

att2_c_w128 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                       inegi + year + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w12_8)
summary(att2_c_w128)

  # Eevent Study
event2_c_36_w118 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                       inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w11_8)
iplot(event2_c_36_w118) 

event2_c_55_w128 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                       inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w12_8)
iplot(event2_c_55_w128) 

}

## 3.2. Estrategia 1 - Alternativa de pesos 10 ##
{
# Estimación con año base 2021, todos los años pre completos, log remesas
att1_c_w1110 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                        inegi + year, data = estimacion_yr_eb, weights = ~w11_10)
summary(att1_c_w1110)

att1_c_w1210 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                        inegi + year, data = estimacion_yr_eb, weights = ~w12_10)
summary(att1_c_w1210)

  # Event Study
event1_c_36_w1110 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                         inegi + year, data = estimacion_yr_eb, weights = ~w11_10)
iplot(event1_c_36_w1110) 

event1_c_55_w1210 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                         inegi + year, data = estimacion_yr_eb, weights = ~w12_10)
iplot(event1_c_55_w1210) 

# Estimación con año base 2021, todos los años completos, log remesas, FE interactuados (mig US 2010 x Year)
att2_c_w1110 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                       inegi + year + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w11_10)
summary(att2_c_w1110)

att2_c_w1210 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                       inegi + year + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w12_10)
summary(att2_c_w1210)

  # Event Study
event2_c_36_w1110 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                            inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w11_10)
iplot(event2_c_36_w1110) 

event2_c_55_w1210 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                            inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w12_10)
iplot(event2_c_55_w1210) 
}

## 3.3. Estrategia 2 - Alternativa de pesos 8 ##
{
# Estimación con año base 2021, todos los años pre completos, log remesas
att1_c_w28 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                        inegi + year, data = estimacion_yr_eb, weights = ~w2_8)
summary(att1_c_w28)

  # Event Study
event1_c_36_w28 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                         inegi + year, data = estimacion_yr_eb, weights = ~w2_8)
iplot(event1_c_36_w28) 

event1_c_55_w28 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                         inegi + year, data = estimacion_yr_eb, weights = ~w2_8)
iplot(event1_c_55_w28) 

# Estimación con año base 2021, todos los años completos, log remesas, FE interactuados (mig US 2010 x Year)
att2_c_w28 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                        inegi + year + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w2_8)
summary(att2_c_w28)

  # Event Study
event2_c_36_w28 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                             inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w2_8)
iplot(event2_c_36_w28) 

event2_c_55_w28 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                             inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w2_8)
iplot(event2_c_55_w28)
}

## 3.4. Estrategia 2 - Alternativa de pesos 10 ##
{
# Estimación con año base 2021, todos los años pre completos, log remesas
att1_c_w210 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                      inegi + year, data = estimacion_yr_eb, weights = ~w2_10)
summary(att1_c_w210)

# Event Study
event1_c_36_w210 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                           inegi + year, data = estimacion_yr_eb, weights = ~w2_10)
iplot(event1_c_36_w210) 

event1_c_55_w210 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                           inegi + year, data = estimacion_yr_eb, weights = ~w2_10)
iplot(event1_c_55_w210) 

# Estimación con año base 2021, todos los años completos, log remesas, FE interactuados (mig US 2010 x Year)
att2_c_w210 <- feols(log_remesas ~ spanish_presence_1936_1955:post21 + spanish_presence_1956_1978:post21 |
                      inegi + year + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w2_10)
summary(att2_c_w210)

# Event Study
event2_c_36_w210 <- feols(log_remesas ~ i(year, spanish_presence_1936_1955, "2021") | 
                           inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w2_10)
iplot(event2_c_36_w210) 

event2_c_55_w210 <- feols(log_remesas ~ i(year, spanish_presence_1956_1978, "2021") | 
                           inegi + year +  + viv_emig_10[year], data = estimacion_yr_eb, weights = ~w2_10)
iplot(event2_c_55_w210)
}

## Exporto los resultados de 2 y 3 ##
{
# --- ATTs --- #
  
models <- list(
  "ATT1: No weights"             = att1_c,
  "ATT2: No weights"             = att2_c,
  "ATT1: w1.8 (1936–1955)"       = att1_c_w118,
  "ATT1: w1.8 (1956–1978)"       = att1_c_w128,
  "ATT2: w1.8 (1936–1955)"       = att2_c_w118,
  "ATT2: w1.8 (1956–1978)"       = att2_c_w128,
  "ATT1: w1.10 (1936–1955)"      = att1_c_w1110,
  "ATT1: w1.10 (1956–1978)"      = att1_c_w1210,
  "ATT2: w1.10 (1936–1955)"      = att2_c_w1110,
  "ATT2: w1.10 (1956–1978)"      = att2_c_w1210,
  "ATT1: w2.8"                   = att1_c_w28,
  "ATT2: w2.8"                   = att2_c_w28,
  "ATT1: w2.10"                  = att1_c_w210,
  "ATT2: w2.10"                  = att2_c_w210
)

mig_time_fe <- c(
  "No",  "Yes",
  "No",  "No",  "Yes", "Yes",
  "No",  "No",  "Yes", "Yes",
  "No",  "Yes",
  "No",  "Yes"
)

add_rows <- data.frame(
  term = c("Municipality FE", "Time FE", "Migration × Time FE"),
  rbind(
    rep("Yes", length(models)),
    rep("Yes", length(models)),
    mig_time_fe
  ),
  check.names = FALSE
)

names(add_rows) <- c("term", names(models))

modelsummary(
  models,
  output = "Output/Estimaciones/att_cohorts_eb.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "spanish_presence_1936_1955:post21" = "Post × Spanish Presence (1936–1955)",
    "post21:spanish_presence_1956_1978" = "Post × Spanish Presence (1956–1978)"
  ),
  add_rows = add_rows,
  notes = "Standard errors clustered at the municipality level in parentheses. * p<0.1, ** p<0.05, *** p<0.01."
)

# --- Event Studies --- #

 # Event Studies sin pesos

  # Exporto el gráfico del event study para la cohorte 1936-1955 sin pesos, sin controles de migración, con año base 2021
png("Output/Estimaciones/event1_c_36.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_36, main = "Post × Spanish Presence (1936–1955)",  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

  # Exporto el gráfico del event study para la cohorte 1956-1978 sin pesos, sin controles de migración, con año base 2021
png("Output/Estimaciones/event1_c_55.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_55, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

  # Exporto el gráfico del event study para la cohorte 1936-1955 sin pesos, con controles de migración, con año base 2021
png("Output/Estimaciones/event2_c_36.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_36, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

  # Exporto el gráfico del event study para la cohorte 1956-1978 sin pesos, con controles de migración, con año base 2021
png("Output/Estimaciones/event2_c_55.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_55, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

 # Event studies con pesos

  # Exporto los gráficos del event study para la cohorte 1936-1955 con pesos, sin controles de migración, con año base 2021
png("Output/Estimaciones/event1_c_36_w118.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_36_w118, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event1_c_36_w1110.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_36_w1110, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event1_c_36_w28.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_36_w28, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event1_c_36_w210.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_36_w210, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

  # Exporto el gráfico del event study para la cohorte 1956-1978 con pesos, sin controles de migración, con año base 2021
png("Output/Estimaciones/event1_c_55_w128.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_55_w128, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event1_c_55_w1210.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_55_w1210, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event1_c_55_w28.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_55_w28, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event1_c_55_w210.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_55_w210, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

  # Exporto los gráficos del event study para la cohorte 1936-1955 con pesos, con controles de migración, con año base 2021
png("Output/Estimaciones/event2_c_36_w118.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_36_w118, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event2_c_36_w1110.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_36_w1110, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event2_c_36_w28.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_36_w28, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event2_c_36_w210.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_36_w210, main ="Post × Spanish Presence (1936–1955)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

  # Exporto el gráfico del event study para la cohorte 1956-1978 con pesos, sin controles de migración, con año base 2021
png("Output/Estimaciones/event2_c_55_w128.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_55_w128, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event2_c_55_w1210.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event1_c_55_w1210, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event2_c_55_w28.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_55_w28, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

png("Output/Estimaciones/event2_c_55_w210.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, cex.axis = 1.1, 
    #family = "Times New Roman", 
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event2_c_55_w210, main ="Post × Spanish Presence (1956–1978)" ,  xlab    = "Year", 
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

}
# ---------------------------- #
# 4. Synthetic DiD
# ---------------------------- #
{
# Creo los grupos
estimacion_yr_coh <- estimacion_yr_coh %>%
  mutate(
    # Grupos mutuamente excluyentes para la especificación de cohortes
    early_any = spanish_presence_1936_1955 == 1,
    late_only = spanish_presence_1936_1955 == 0 & spanish_presence_1956_1978 == 1,
    never = spanish_presence_1936_1955 == 0 & spanish_presence_1956_1978 == 0,
    
    # Dummy general
    any_presence = spanish_presence_1936_1955 == 1 | spanish_presence_1956_1978 == 1
  )

# Chequeo balance del panel
  # Para el grupo early any vs never
panel_check <- estimacion_yr_coh %>%
  filter(early_any == 1 | never == 1) %>% 
  ungroup() %>%
  count(inegi, name = "n_periods") %>%
  summarise(
    min_T = min(n_periods),
    max_T = max(n_periods),
    n_units = n(),
    balanced = min_T == max_T,
    .groups = "drop"
  )

panel_check

bad_units <- estimacion_yr_coh %>%
  filter(early_any == 1 | never == 1) %>% 
  count(inegi, name = "n_periods") %>%
  filter(n_periods < max(n_periods)) %>%
  arrange(n_periods)

bad_units

estimacion_yr_coh %>% 
  filter(inegi== "20316"| inegi == "20562") %>% 
  select(inegi, admin_name, year, log_remesas, spanish_presence_1936_1955, spanish_presence_1956_1978) %>% 
  View() # son dos municipios que no tienen data de remesas

  # Para el grupo late only vs never
panel_check <- estimacion_yr_coh %>%
  filter(late_only == 1 | never == 1) %>% 
  ungroup() %>%
  count(inegi, name = "n_periods") %>%
  summarise(
    min_T = min(n_periods),
    max_T = max(n_periods),
    n_units = n(),
    balanced = min_T == max_T,
    .groups = "drop"
  )

panel_check

bad_units <- estimacion_yr_coh %>%
  filter(late_only == 1 | never == 1) %>% 
  count(inegi, name = "n_periods") %>%
  filter(n_periods < max(n_periods)) %>%
  arrange(n_periods)

bad_units

estimacion_yr_coh %>% 
  filter(inegi== "20316"| inegi == "20562") %>% 
  select(inegi, admin_name, year, log_remesas, spanish_presence_1936_1955, spanish_presence_1956_1978) %>% 
  View() # son dos municipios que no tienen data de remesas


### 3.1 Con tres grupos ###

data_sdid <- estimacion_yr_coh %>%
  filter(
    early_any == 1 | never == 1
  ) %>%
  filter(inegi != "20316" & inegi != "20562") %>% # filtro los municipios con data faltante
  mutate(
    treatment = as.integer((early_any == 1) & (post21 == 1))
  ) %>%
  select(
    unit = inegi,
    time = year,
    outcome = log_remesas,
    treatment
  ) %>%
  arrange(unit, time) %>% 
  as.data.frame()
table(data_sdid$treatment)
pm <- panel.matrices(data_sdid)




run_sdid <- function(data,
                     treated_var,
                     outcome_var = "log_remesas",
                     unit_var = "inegi",
                     time_var = "year",
                     post_var = "post21") {
  
  treated_var <- rlang::ensym(treated_var)
  outcome_var <- rlang::ensym(outcome_var)
  unit_var <- rlang::ensym(unit_var)
  time_var <- rlang::ensym(time_var)
  post_var <- rlang::ensym(post_var)
  
  data_sdid <- data %>%
    filter(
      !!treated_var == 1 | never == 1
    ) %>%
    filter(!!unit_var != "20316" & !!unit_var != "20562") %>% # filtro los municipios con data faltante
    mutate(
      treatment = as.integer((!!treated_var == 1) & (!!post_var == 1))
    ) %>%
    select(
      unit = !!unit_var,
      time = !!time_var,
      outcome = !!outcome_var,
      treatment
    ) %>%
    arrange(unit, time) %>% 
    as.data.frame()
  
  print(table(data_sdid$treatment))
  
  # Crear matrices para synthdid
  pm <- panel.matrices(
    data_sdid
    #unit = "unit",
    #time = "time",
    #outcome = "outcome",
    #treatment = "treatment"
  )

  # Estimar Synthetic DiD
  tau <- synthdid_estimate(
    Y = pm$Y,
    N0 = pm$N0,
    T0 = pm$T0
  )
  
  # Error estándar por placebo, recomendado cuando hay pocas tratadas
  se_placebo <- sqrt(vcov(tau, method = "placebo"))
  
  list(
    estimate = tau,
    se_placebo = se_placebo,
    data = data_sdid,
    matrices = pm
  )
}

# Corro los sinteticos
sdid_early_any <- run_sdid(
  data = estimacion_yr_coh,
  treated_var = early_any,
  outcome_var = "log_remesas",
  unit_var = "inegi",
  time_var = "year",
  post_var = "post21"
)
sdid_early_any$estimate
plot(sdid_early_any$estimate)

sdid_late_only <- run_sdid(
  data = estimacion_yr_coh,
  treated_var = late_only,
  outcome_var = "log_remesas",
  unit_var = "inegi",
  time_var = "year",
  post_var = "post21"
)
sdid_late_only$estimate
plot(sdid_late_only$estimate)

sdid_any_presence <- run_sdid(
  data = estimacion_yr_coh,
  treated_var = any_presence,
  outcome_var = "log_remesas",
  unit_var = "inegi",
  time_var = "year",
  post_var = "post21"
)
sdid_any_presence$estimate
plot(sdid_any_presence$estimate)
  
}



