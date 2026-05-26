install.packages("fuzzyjoin")
install.packages("psych")

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
library(stringi)
library(fuzzyjoin)
library(readxl)

# -------------------- #
#    Censos México
# -------------------- #
{
#### 1. Presencia española por municipio ####

censos <- read_dta("Data Raw/Censos/Mexico/censos_mx.dta")

# Españoles por municipio
class(censos$perwt)
unique(censos$bplcountry)
class(censos$bplcountry)

length(unique(censos$geo2_mx))
length(unique(censos$geo2_mx1960))
length(unique(censos$geo2_mx1970))
length(unique(censos$geolev2))

# Construyo la data con presencia española anual
spanish <- censos %>%
  mutate(
    spanish_born = (bplcountry == 43120),
    inegi  = str_sub(as.character(geo2_mx), -5, -1)
  ) %>%
  group_by(inegi, year) %>%
  summarise(
    n_total = sum(perwt, na.rm = TRUE),
    n_spanish_born = sum(perwt[spanish_born], na.rm = TRUE),
    share_spanish_born = (n_spanish_born / n_total)*100,
    log_spanish_born = log1p(n_spanish_born),
    
    # chequeos de tamaño muestral (sin pesos)
    unweighted_n = n(),
    unweighted_spanish = sum(spanish_born, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(inegi)

max(spanish$share_spanish_born, na.rm = TRUE)
min(spanish$share_spanish_born, na.rm = TRUE)

psych::describe(spanish$share_spanish_born)
psych::describe(spanish$log_spanish_born)

Hmisc::describe(spanish %>% 
                  filter(inegi != 21119) %>% 
                  pull(share_spanish_born))

# Data de 1960
spanish_60 <- spanish %>% 
  filter(year == 1960)
# Data de 1970
spanish_70 <- spanish %>% 
  filter(year == 1970)

# Construyo la data con prsencia historica de españoles promedio (intensidad historica promedio)
spanish_historical <- censos %>%
  mutate(
    spanish_born = (bplcountry == 43120),
    inegi  = str_sub(as.character(geo2_mx), -5, -1)
  ) %>%
  group_by(inegi) %>%
  summarise(
    n_total = sum(perwt, na.rm = TRUE),
    n_spanish_born = sum(perwt[spanish_born], na.rm = TRUE),
    share_spanish_born = (n_spanish_born / n_total)*100,
    log_spanish_born_pooled = log1p(n_spanish_born),  # log(1 + (S_60 + S_70))
    log_spanish_born_avg = log1p(n_spanish_born / 2), # log(1 + (S_60 + S_70)/2)
    spanish_born = if_else(n_spanish_born>0, 1, 0), # indicador de presencia histórica de españoles
    
    # chequeos de tamaño muestral (sin pesos)
    unweighted_n = n(),
    unweighted_spanish = sum(spanish_born, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(inegi)

#### 2. Estimación del año de llegada ####

unique(censos$migctryp)
unique(censos$migyrs1)

## 2.1 Creo el año de llegada
censos <- censos %>%
  mutate(
    inegi  = str_sub(as.character(geo2_mx), -5, -1),
    # Personas nacidas en España
    spanish_born = (bplcountry == 43120),
    # Personas nacidas en España con residencia previa en España
    spanish_inm = spanish_born & (migctryp == 43120),
    # Año en el que llegaron a México (numeric). Reemplazo por NA los valores mayores a 95 (son otras categorias)
    year_inm = case_when(
      spanish_inm & migyrs1 <= 95 ~ year - as.numeric(migyrs1),
      spanish_inm & migyrs1 > 95 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    # Años residiendo en la localidad actual (numeric)
    migyrs = case_when(
      migyrs1 <= 95 ~ migyrs1,
      migyrs1 > 95 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    # Chqueo para cuantos Españoles inmigrantes hay año de llegada
    has_year_inm = if_else(!is.na(year_inm), 1, 0)
  ) %>% 
  filter(!str_ends(inegi, "999"))

table(censos$year_inm, useNA = "ifany")
class(censos$year_inm)
table(censos$spanish_born)

# Cuantos españoles no tienen como última residencia España? (movers internos)
Hmisc::wtd.table(censos$has_year_inm, weights = censos$perwt) # 41163 españoles que tienen año estimado
Hmisc::wtd.table(censos$spanish_born, weights = censos$perwt) # 58800 españoles totales

table(censos$spanish_born)
table(censos$spanish_inm)
table(censos$has_year_inm) # hay menos españoles con año de llegada estimado que españoles con ultima residencia en España
censos %>% # Chequeo missings en el año de llegada
  filter(spanish_inm == TRUE) %>%
  summarise(n_NA = sum(is.na(migyrs))) # hay exactamente 27 NA, es la diferencia entre 534-507 (españoles inmigrantes menos españoles inmigrantes con año de llegada estimado)

# Españoles que SÍ tienen año de llegada estimado
  # Distribución de años de llegada
ggplot(data = censos %>% filter(has_year_inm == 1))+
  geom_histogram(aes(x = year_inm), bins = 30, fill = "grey60",color = "black")

Hmisc::describe(censos$year_inm)
psych::describe(censos$year_inm)

# Ambos censos
ggplot(data = censos %>% filter(has_year_inm == 1), 
       aes(x = year_inm, weight = perwt)) +
  geom_histogram(
    binwidth = 5,
    boundary = 1900, 
    fill = "grey70",
    color = "white"
  ) +
  # líneas verticales indicando los años 1936 y 1975
  geom_vline(xintercept = 1936, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +
  # anotación del período de exposición
  annotate("text", x = 1955, y = 6000, label = "Exposure years", color = "red", size = 3.5, fontface = "bold") +
  labs(
    title = "Year of arrival of Spanish-born population in México (Census 1960 and 1970)",
    subtitle = "Weighted counts using census sampling weights",
    x = "Year of arrival",
    y = "Weighted count"
  ) +
  scale_x_continuous(breaks = seq(1900, 1970, by = 10)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )
ggsave("Output/Mexico/hist_yrimm_mx.png", width = 8, height = 6, dpi = 300, bg = "white")

# Censo 1960
ggplot(data = censos %>% filter(year == 1960, has_year_inm == 1), 
       aes(x = year_inm, weight = perwt)) +
  geom_histogram(
    binwidth = 5,
    boundary = 1900, 
    fill = "grey70",
    color = "white"
  ) +
  labs(
    title = "Year of arrival of Spanish-born population in México (Census 1960)",
    subtitle = "Weighted counts using census sampling weights",
    x = "Year of arrival",
    y = "Weighted count"
  ) +
  scale_x_continuous(breaks = seq(1900, 1970, by = 10)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )
# Censo 1970
ggplot(data = censos %>% filter(year == 1970, has_year_inm == 1), 
       aes(x = year_inm, weight = perwt)) +
  geom_histogram(
    binwidth = 5,
    boundary = 1900, 
    fill = "grey70",
    color = "white"
  ) +
  labs(
    title = "Year of arrival of Spanish-born population in México (Census 1970)",
    subtitle = "Weighted counts using census sampling weights",
    x = "Year of arrival",
    y = "Weighted count"
  ) +
  scale_x_continuous(breaks = seq(1900, 1970, by = 10)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

## 2.2 Cuento la cantidad de españoles por municipio en cada ventana de llegada
spanish_counts_mx <- censos %>%
  mutate(
    spanish_born = ifelse(bplcountry == 43120, 1, 0),
    inegi = str_sub(as.character(geo2_mx), -5, -1) # últimos 3 dígitos municipios, primeros 2 estado 
  ) %>%
  filter(
    (year == 1960 & year_inm >= 1936 & year_inm <= 1955) |
      (year == 1970 & year_inm >= 1956 & year_inm <= 1978)
  ) %>%
  mutate(
    cohort = case_when(
      year == 1960 & year_inm >= 1936 & year_inm <= 1955 ~ "arrived_1936_1955",
      year == 1970 & year_inm >= 1956 & year_inm <= 1978 ~ "arrived_1956_1978"
    )
  ) %>%
  group_by(inegi, cohort) %>%
  summarise(
    n_spanish = sum(perwt, na.rm = TRUE),
    unweighted_n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = cohort,
    values_from = c(n_spanish, unweighted_n),
    values_fill = 0
  ) %>%
  arrange(inegi)

unique(spanish_counts_mx$inegi)

## 2.3 Hago el merge con todos los municipios

# Data de municipios harmonizada de todos los municipios
municipios_sf <- st_read("Data Raw/Cartografia/geo2_mx1960_2020/geo2_mx1960_2020.shp",  options = "ENCODING=LATIN1")
municipios_sf <- municipios_sf %>%
  mutate(
    inegi  = str_sub(as.character(GEOLEVEL2), -5, -1),
    admin_name_clean = stri_trans_general(ADMIN_NAME, "Latin-ASCII"),
    admin_name_clean = toupper(admin_name_clean),
    admin_name_clean = str_squish(admin_name_clean) # elimino espacios duplicados, saltos de linea, espacios antes y despues
  ) %>%
  filter(!str_ends(inegi, "999"))

unique(municipios_sf$inegi)

# Mergeo
spanish_cohorts_mx <- municipios_sf %>%
  left_join(spanish_counts_mx, by = "inegi") %>%
  mutate(
    across(starts_with("n_spanish"), ~replace_na(., 0)),
    across(starts_with("unweighted_n"), ~replace_na(., 0))
  ) %>%
  arrange(inegi) %>% 
  janitor::clean_names()

# Población total municipal en 1970 y 1980
total_pop_mx <- censos %>%
  mutate(
    inegi = str_sub(as.character(geo2_mx), -5, -1)
  ) %>%
  group_by(inegi, year) %>%
  summarise(
    total_pop = sum(perwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cohort = case_when(
      year == 1960 ~ "arrived_1936_1955",
      year == 1970 ~ "arrived_1956_1978"
    )
  ) %>%
  select(-year) %>%
  pivot_wider(
    names_from = cohort,
    values_from = total_pop,
    names_prefix = "total_pop_"
  )

# Unir y calcular shares
spanish_cohorts_mx <- spanish_cohorts_mx %>%
  left_join(total_pop_mx, by = "inegi") %>%
  mutate(
    share_1936_1955 = 100 * n_spanish_arrived_1936_1955 / total_pop_arrived_1936_1955,
    share_1956_1978 = 100 * n_spanish_arrived_1956_1978 / total_pop_arrived_1956_1978
  ) %>% 
  select(inegi, admin_name_clean, n_spanish_arrived_1936_1955, n_spanish_arrived_1956_1978, 
         unweighted_n_arrived_1936_1955, unweighted_n_arrived_1956_1978,
         total_pop_arrived_1936_1955, total_pop_arrived_1956_1978,
         share_1936_1955, share_1956_1978, everything()) %>% 
  rename(state_code = parent)

Hmisc::describe(spanish_cohorts_mx$share_1936_1955)
Hmisc::describe(spanish_cohorts_mx$share_1956_1978)
summary(spanish_cohorts_mx$share_1956_1978)
class(spanish_cohorts_mx$share_1936_1955)
class(spanish_cohorts_mx$share_1956_1978)
# la presencia española es muy ruidosa, son muy pocos por municipios, en muchos casos 1 o 2. 
# solo en un municipio hay mas de 100

# Explorar el margen extensivo: creo una dummy de presencia española
spanish_cohorts_mx <- spanish_cohorts_mx %>% 
  mutate(
    spanish_presence_1936_1955 = if_else(n_spanish_arrived_1936_1955 > 0, 1, 0),
    spanish_presence_1956_1978 = if_else(n_spanish_arrived_1956_1978 > 0, 1, 0)
  )

table(spanish_cohorts_mx$spanish_presence_1936_1955)
table(spanish_cohorts_mx$spanish_presence_1956_1978)
table(spanish_cohorts_mx$spanish_presence_1936_1955, spanish_cohorts_mx$spanish_presence_1956_1978)

chequeo_spanish <- spanish_cohorts_mx %>%
  select(inegi, share_1936_1955, share_1956_1978, n_spanish_arrived_1936_1955, 
         n_spanish_arrived_1956_1978, unweighted_n_arrived_1936_1955, 
         unweighted_n_arrived_1956_1978, spanish_presence_1936_1955,
         spanish_presence_1956_1978) %>%
  filter(spanish_presence_1936_1955 == 1 | spanish_presence_1956_1978 ==1) 

chequeo_spanish <- st_drop_geometry(chequeo_spanish) 
write_xlsx(chequeo_spanish, "Presentations/Presentacion_220426/chequeo_spanish.xlsx")
}
# -------------------- #
#  Cartografía México
# -------------------- #
{
# Chequeo que no haya NA en variables clave
any(is.na(municipios_sf$GEOLEVEL2))
any(is.na(spanish$year))
any(is.na(spanish_60$year))
any(is.na(spanish_70$year))

# Uno la data de españoles con la cartografía por año (para los gráficos)
spanish_60 <- spanish_60 %>%
  full_join(municipios_sf, by = "inegi") %>% 
  # chequeo
  mutate(solo_df1 = if_else(is.na(GEOLEVEL2), 1, 0),
         solo_df2 = if_else(is.na(year), 1, 0)
         )
class(spanish_60)
spanish_60 <- sf::st_as_sf(spanish_60)

spanish_70 <- spanish_70 %>%
  full_join(municipios_sf, by = "inegi") %>% 
  # chequeo
  mutate(solo_df1 = if_else(is.na(GEOLEVEL2), 1, 0),
         solo_df2 = if_else(is.na(year), 1, 0))
class(spanish_70)
spanish_70 <- sf::st_as_sf(spanish_70)

# Chequeos
table(spanish_60$solo_df1) 
table(spanish_60$solo_df2) # hay NA, puede que el censo no tenga todos los municipios dado que es una muestra
#spanish_60 <- spanish_60 %>%
#  select(-solo_df1, -solo_df2)
spanish_60 %>% filter(solo_df2 == 1) %>% View()
censos %>% 
  filter(geo2_mx == 484007058) %>% 
  select(serial, year, geo2_mx, nativity, bplcountry) %>% 
  View() # son de 1970
censos %>% 
  filter(geo2_mx == 484015027) %>% 
  select(serial, year, geo2_mx, nativity, bplcountry) %>% 
  View() # son de 1970
spanish_60 %>% 
  filter(inegi == 15027) %>% 
  View() 

censo60 <- censos %>% filter(year == 1960)
n_distinct(censo60$geo2_mx)
censo70 <- censos %>% filter(year == 1970)
n_distinct(censo70$geo2_mx) # el censo de 1960 efectivamente tiene menos municipios
n_distinct(municipios_sf$GEOLEVEL2) # el censo de 1970 tampoco tiene todos pero se acerca mas

table(spanish_70$solo_df1) 
table(spanish_70$solo_df2) # hay NA, puede que el censo no tenga todos los municipios dado que es una muestra
#spanish_70 <- spanish_70 %>%
#  select(-solo_df1, -solo_df2)

# Uno la data de españoles para los dos años con la cartografía
any(is.na(spanish_historical$n_total)) # chequeo que no haya NA para construir solo_df2

spanish_historical <- spanish_historical %>%
  full_join(municipios_sf, by = "inegi") %>% 
  # chequeo
  mutate(solo_df1 = if_else(is.na(GEOLEVEL2), 1, 0),
         solo_df2 = if_else(is.na(n_total), 1, 0)
  )
table(spanish_historical$solo_df1) 
table(spanish_historical$solo_df2)
spanish_historical %>% filter(solo_df2 == 1) %>% View() # solo 1 municipio no esta en ningun censo
spanish_historical %>% filter(solo_df1 == 1) %>% View() # son los inegi que terminan en 999, por eso no tienen cartografía

class(spanish_historical)
spanish_historical <- sf::st_as_sf(spanish_historical)
}
# -------------------- #
#    Distribuciones
# -------------------- #
{
## Distribución de españoles por municipio en 1960 ##

# con %
psych::describe(spanish_60$share_spanish_born)

ggplot(spanish_60) +
  geom_sf(aes(fill = share_spanish_born), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 0.7), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1, suffix = "%"),
    name = "% spanish born",
    na.value = "grey80" 
  ) + 
  labs(title = "Spanish-born population by municipality in 1960") +
  theme_void()+
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/map_share_spanish_1960.png", width = 8, height = 6, dpi = 300, bg = "white")

# con log
psych::describe(spanish_60$log_spanish_born)

ggplot(spanish_60) +
  geom_sf(aes(fill = log_spanish_born), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 10), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1),
    name = "log(1 + Spanish-born)",
    na.value = "grey80" 
  ) + 
  labs(title = "Spanish-born population by municipality in 1960") +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/map_log_spanish_1960.png", width = 8, height = 6, dpi = 300, bg = "white")

## Distribución de españoles por municipio en 1970 ##

# con %
psych::describe(spanish_70$share_spanish_born) # hay un outlier de 5%

ggplot(spanish_70) +
  geom_sf(aes(fill = share_spanish_born), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 0.8), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1, suffix = "%"),
    name = "% spanish born",
    na.value = "grey80" 
  ) + 
  labs(title = "Spanish-born population by municipality in 1970 (excluding outlier)") +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/map_share_spanish_1970.png", width = 8, height = 6, dpi = 300, bg = "white")

# con log
psych::describe(spanish_70$log_spanish_born)

ggplot(spanish_70) +
  geom_sf(aes(fill = log_spanish_born), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 10), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1),
    name = "log(1 + Spanish-born)",
    na.value = "grey80" 
  ) + 
  labs(title = "Spanish-born population by municipality in 1970") +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/map_log_spanish_1970.png", width = 8, height = 6, dpi = 300, bg = "white")

## Distribución de españoles por municipio promedio ambos años ##

# con %
psych::describe(spanish_historical$share_spanish_born)

ggplot(spanish_historical) +
  geom_sf(aes(fill = share_spanish_born), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 0.6), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1, suffix = "%"),
    name = "% spanish born",
    na.value = "grey80" 
  ) + 
  labs(title = "Share of Spanish-born pop. by municipality (pooled 1960–1970; excluding the outlier)") +
  theme_void()+
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/map_share_spanish_ave.png", width = 8, height = 6, dpi = 300, bg = "white")

# con log
psych::describe(spanish_historical$log_spanish_born_avg)

ggplot(spanish_historical) +
  geom_sf(aes(fill = log_spanish_born_avg), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 10), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1),
    name = "log(1 + avg. Spanish-born stock)",
    na.value = "grey80" 
  ) + 
  labs(title = "Spanish-born pop. by municipality (log average stock, 1960–1970)") +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/map_log_spanish_ave.png", width = 8, height = 6, dpi = 300, bg = "white")

## Distribución por bins ##
spanish_60 <- spanish_60 %>%
  mutate(share_bin = cut(
    share_spanish_born,
    breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08),
    include.lowest = TRUE
  ))

spanish_70 <- spanish_70 %>%
  mutate(share_bin = cut(
    share_spanish_born,
    breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08),
    include.lowest = TRUE
  ))

ggplot(spanish_60) +
  geom_sf(aes(fill = share_bin), color = "grey85", linewidth = 0.01) +
  scale_fill_brewer(palette = "Blues", name = "% share Spanish-born") +
  theme_void()

ggplot(spanish_70) +
  geom_sf(aes(fill = share_bin), color = "grey85", linewidth = 0.01) +
  scale_fill_brewer(palette = "Blues", name = "% share Spanish-born") +
  theme_void()

## Distribución de valores no nulos 1960 ##
spanish_60_filter <- spanish_60 %>% 
  filter(!is.na(share_spanish_born) & share_spanish_born > 0) %>% 
  select(inegi, share_spanish_born) %>% 
  arrange(desc(share_spanish_born))
Hmisc::describe(spanish_60_filter$share_spanish_born) 
pdf("Output/hist_share_spanish_1960.pdf", width = 8, height = 6)
hist(spanish_60_filter$share_spanish_born, main = "Distribution of the Spanish-born share (1960, excluding zero values)", 
     xlab = "% Spanish-born")
dev.off()

# Guardo la tabla de valores positivos
spanish_60_filter <- spanish_60_filter %>% 
  #left_join(municipios_sf %>% as.data.frame() %>% select(ADMIN_NAME, inegi), by = "inegi") %>% 
  as.data.frame(spanish_60_filter) %>% 
  select(-geometry) %>% 
  arrange(desc(share_spanish_born))
write_csv(spanish_60_filter, "Output/spanish_born_share_1960_positive.csv")

## Distribución de valores no nulos 1970 ##
spanish_70_filter <- spanish_70 %>% 
  filter(!is.na(share_spanish_born) & share_spanish_born > 0) %>% 
  select(inegi, share_spanish_born) %>% 
  arrange(desc(share_spanish_born))
psych::describe(spanish_70_filter$share_spanish_born) 
hist(spanish_70_filter$share_spanish_born)

# Guardo la tabla de valores positivos
spanish_70_filter <- spanish_70_filter %>% 
  #left_join(municipios_sf %>% as.data.fram() %>% select(ADMIN_NAME, inegi), by = "inegi") %>% 
  as.data.frame(spanish_70_filter) %>% 
  select(-geometry) %>% 
  arrange(desc(share_spanish_born))
write_csv(spanish_70_filter, "Output/spanish_born_share_1970_positive.csv")

# Chequeo outlier de San Andrés Cholula que tiene como 5%
municipios_sf %>% filter(GEOLEVEL2 == 484021119) %>% View()
spanish_70 %>% filter(inegi == 21119) %>% 
  View()
chequeo <- censo70 %>% filter(geo2_mx == 484021119) %>% 
  select(serial, year, geo2_mx, nativity, bplcountry, age) 
table(chequeo$nativity)
table(chequeo$bplcountry)
chequeo %>% filter(bplcountry == 43120) %>% View()
# Elimino el outlier por ahora
spanish_70_filter <- spanish_70_filter %>%
  filter(inegi != 21119)

# Grafico de nuevo 
pdf("Output/hist_share_spanish_1970.pdf", width = 8, height = 6)
hist(spanish_70_filter$share_spanish_born, xlab = "% Spanish-born", 
     main = "Distribution of the Spanish-born share (1970, excluding outlier and zero values)")
dev.off()
}
# -------------------- #
#      Remesas
# -------------------- #
{
remesas <- read_csv("Data Raw/Remesas/remesas_2013-2024.csv")
unique(remesas$reg_mig)
length(unique(remesas$cve_mun)) # 2488 municipios
class(remesas$cve_mun)
class(remesas$rem)
any(is.na(remesas$rem))
unique(remesas$trim)

# Cambio el nombre de 4 municipios que están cortados (asumo que el código más chico es el distrito con menor número)
# Aprovecho para cambiar el nombre de los municipios a mayúscula e igual que en el censo
remesas <- remesas %>% 
  mutate(
    nom_mun = case_when(
      cve_mun == 20208 ~ "SAN JUAN MIXTEPEC DISTRITO 08",
      cve_mun == 20209 ~ "SAN JUAN MIXTEPEC DISTRITO 26",
      cve_mun == 20317 ~ "SAN PEDRO MIXTEPEC - DISTR. 22",
      cve_mun == 20318 ~ "SAN PEDRO MIXTEPEC - DISTR. 26",
      TRUE ~ nom_mun
    )
  )

# remesas por trimestre
remesas_tri <- remesas %>% 
  mutate(
    trimestre = case_when(
      trim == "Ene-Mar" ~ 1,
      trim ==  "Abr-Jun" ~ 2,
      trim ==  "Jul-Sep" ~ 3,
      trim == "Oct-Dic" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>% 
  rename(year=aaaa, total_remesas = rem) %>% 
  mutate(
    # relleno con 5 digitos
    inegi = str_pad(as.character(cve_mun), width = 5, side = "left", pad = "0"),
    tq = as.yearqtr(paste0(year, " Q", trimestre), format = "%Y Q%q")
  ) %>% 
  select(inegi, year, trimestre, tq, total_remesas)

# remesas por año
remesas_yr <- remesas %>% 
  group_by(cve_mun, nom_mun, aaaa) %>%
  summarise(
    total_remesas = sum(rem, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  rename(year=aaaa) %>% 
  mutate(
    # relleno con 5 digitos
    inegi = str_pad(as.character(cve_mun), width = 5, side = "left", pad = "0"),
    # Limpio los nombres de los municipios
    nom_mun_clean = stri_trans_general(nom_mun, "Latin-ASCII"),
    nom_mun_clean = toupper(nom_mun_clean),
    nom_mun_clean = str_squish(nom_mun_clean) # elimino espacios duplicados, saltos de linea, espacios antes y despues
  ) %>% 
  select(inegi, nom_mun_clean, year, total_remesas)

table(remesas_exp$year)
table(remesas$aaaa)

#### Diagnostico de municipios en la data de remesas #### 

# Municipios únicos en la data de remesas
municipios_remesas <- remesas_yr %>% 
  select(inegi, nom_mun_clean) %>% 
  #creo el código de estado
  mutate(
    state_code_remesas = str_sub(inegi, 1, 2)
  ) %>%
  distinct() %>% 
  arrange(inegi)

# Municipios únicos en la data del censo
municipios_censo <- municipios_sf %>% 
  st_drop_geometry() %>%
  select(inegi, admin_name_clean) %>% 
  #creo el código de estado
  mutate(
    state_code_censo = str_sub(inegi, 1, 2)
  ) %>%
  distinct() %>% 
  arrange(inegi)

# Merge por INEGI
diag_merge <- municipios_censo %>%
  full_join(municipios_remesas, by = "inegi") %>%
  mutate(
    # match exacto
    exact_match = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      admin_name_clean == nom_mun_clean,
    
    # uno contiene al otro
    name_contained = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      (
        str_detect(admin_name_clean, fixed(nom_mun_clean)) |
          str_detect(nom_mun_clean, fixed(admin_name_clean))
      ),
    
    # el nombre del censo parece agregar varios municipios
    census_aggregation = !is.na(admin_name_clean) &
      str_detect(admin_name_clean, ","),
    
    # reemplazo a mano el municipio que tiene una coma en el nombre eliminando la coma
    census_aggregation = ifelse(admin_name_clean == "VILLA TEZOATLAN DE SEGURA Y LUNA, CUNA DE LA INDEPENDENCIA DE OAXACA, HEROICA",
                                FALSE, census_aggregation),
    
    # clasificación final (toma el primer TRUE)
    merge_status = case_when(
      is.na(admin_name_clean) ~ "only_remesas",
      is.na(nom_mun_clean) ~ "only_censo",
      exact_match ~ "exact",
      census_aggregation & name_contained ~ "part of aggregation",
      name_contained ~ "close",
      TRUE ~ "mismatch"
    )
  )

table(diag_merge$merge_status)

# A. Corrijo los casos de mismatch (hay que asignar el inegi correcto a los municipios de remesas)
mismatch_remesas <- diag_merge %>% 
  filter(merge_status == "mismatch") %>% 
  select(nom_mun_clean, state_code_remesas) 

mismatch_censo <- diag_merge %>% 
  filter(merge_status == "mismatch") %>% 
  select(inegi, admin_name_clean, state_code_censo) 

crosswalk_A <- crossing(mismatch_remesas, mismatch_censo)

correct_A <- crosswalk_A %>% 
  filter(str_detect(admin_name_clean, fixed(nom_mun_clean)) & state_code_censo == state_code_remesas) %>%
  select(inegi, nom_mun_clean, state_code_remesas) # con esto me aseguro que el nombre es el nombre de la data de las remesas

length(unique(correct_A$inegi)) # 834
length(unique(correct_A$nom_mun_clean)) # 828
length(unique(mismatch_censo$inegi)) # 897

setdiff(mismatch_remesas$nom_mun_clean, correct_A$nom_mun_clean) 
setdiff(mismatch_censo$admin_name_clean, correct_A$nom_mun_clean) 

# chequeo que se hayan unido todos
any(is.na(correct_A$inegi)) # no hay inegi con NA
any(is.na(correct_A$nom_mun_clean)) # No hay NA

# chequeo inegi duplicados
correct_A %>% 
  count(inegi) %>%
  filter(n > 1)

# corrijo a mano los casos duplicados que se asignaron mal (porque el nombre esta dentro del nombre de otro municipio: p.e. GRAN MORELOS y MORELOS)
correct_A <- correct_A %>% 
  filter(!(inegi == "08025" & nom_mun_clean == "MORELOS")) %>% 
  filter(!(inegi == "08027" & nom_mun_clean == "GUADALUPE")) %>% 
  filter(!(inegi == "08048" & nom_mun_clean == "CASAS GRANDES")) %>%
  filter(!(inegi == "08051" & nom_mun_clean == "GUERRERO")) %>%
  filter(!(inegi == "15039" & nom_mun_clean == "EL ORO")) %>%
  filter(!(inegi == "16032" & nom_mun_clean == "ZAMORA")) %>%
  filter(!(inegi == "30078" & nom_mun_clean == "LOS REYES")) %>%
  filter(!(inegi == "30102" & nom_mun_clean == "OTEAPAN")) %>%
  filter(!(inegi == "30127" & nom_mun_clean == "SOCHIAPA"))

length(unique(correct_A$inegi)) # 834
correct_A %>%
  count(nom_mun_clean, state_code_remesas) %>%
  filter(n > 1) # cada municipio tiene una unica corrección

# Hasta acá se arreglaron 834 nom_mun_clean que eran mismatch 
# Incluso si el municipio es parte de una agregado, siempre que el agregado tambien tenga mismatch
# También se corrigen los casos de mismatch en los que el nombre en la data de remesas aparece cortado, siempre que no se corten las palabras en el nombre

# Los mismatch que faltan corregir son:
# Municipios que salen de only remesas y tienen que tener otro inegi que ahora es mismatch --> B y E
# Municipios con un inegi que tiene asignarse a una agregación (y la agregación NO tiene mismatch) --> C
# Municipios que tienen las palabras cortadas (si es que hay)
# Municipios que tienen mismatch y el inegi correcto es de only censo --> D

# B. Corrijo los only remesas (tengo que meter los only remesas en los grupos agregados)
only_remesas <- diag_merge %>%
  filter(merge_status == "only_remesas") %>%
  select(inegi, state_code_remesas, nom_mun_clean) %>%
  distinct() %>% 
  # elimino los inegi que terminan en 999 (no identificado)
  filter(!str_ends(inegi, "999")) %>% 
  select(-inegi)

agrupados_censo <- diag_merge %>%
  filter(census_aggregation) %>%
  select(inegi, state_code_censo, admin_name_clean) %>%
  distinct() %>% 
  mutate(
    admin_name_nocoma = admin_name_clean %>%
      str_replace_all(",", " ") %>% # reemplaza comas por espacios
      str_squish() # elimina espacios dobles
  )

crosswalk_B <- crossing(only_remesas, agrupados_censo) %>%
  mutate(
    admin_pad = paste0(" ", admin_name_nocoma, " "),
    nom_pad   = paste0(" ", nom_mun_clean, " ")
  ) 

correct_B <- crosswalk_B %>% 
  filter(str_detect(admin_pad, fixed(nom_pad)) & state_code_censo == state_code_remesas) %>%
  select(inegi, nom_mun_clean, state_code_remesas) # con esto me aseguro que el nombre es el nombre de la data de las remesas

setdiff(correct_B$nom_mun_clean, only_remesas$nom_mun_clean) # 0
setdiff(only_remesas$nom_mun_clean, correct_B$nom_mun_clean) # son casos donde el inegi correspondiente del censo es mismatch o casos dond el nombre de los municipios esta cortado

# Corrijo a mano LAZARO CARDENAS que se unió con SANCTORUM DE LAZARO CARDENAS, BENITO JUAREZ (es otro municipio agregado que se une bien con su propio inegi, 29020)
# elimino cuando el inegi es 29020 y el municipio es LAZARO CARDENAS
correct_B <- correct_B %>% 
  filter(!(inegi == "29020" & nom_mun_clean == "LAZARO CARDENAS"))
# 86 de 133 municipios que eran only remesas ya tienen un grupo asignado

# C. Corrijo los municipios mismatch que tienen que ir para agregados 
# Acá se corrigen los inegi de los municipios en los casos en los que el agregado no tiene mismatch y también en el caso que tenga mismatch (despues eliminamos las correcciones dobles que ya se hicieron en A)
crosswalk_C <- crossing(mismatch_remesas, agrupados_censo) %>%
  mutate(
    admin_pad = paste0(" ", admin_name_nocoma, " "),
    nom_pad   = paste0(" ", nom_mun_clean, " ") # con esto me aseguro que el nombre es el nombre de la data de las remesas
  ) 

correct_C <-  crosswalk_C %>%
  filter(str_detect(admin_pad, fixed(nom_pad)) & state_code_censo == state_code_remesas) %>%
  select(inegi, nom_mun_clean, state_code_remesas)
# 64 municipios que eran mismatch ahora tienen un grupo asignado

# D. Corrijo los only censo (paso los mismatches a los only censo)
only_censo <- diag_merge %>%
  filter(merge_status == "only_censo") %>%
  select(inegi, state_code_censo, admin_name_clean) %>%
  mutate(
    admin_name_nocoma = admin_name_clean %>%
      str_replace_all(",", " ") %>% # reemplaza comas por espacios
      str_squish() # elimina espacios dobles
  )

crosswalk_D <- crossing(only_censo, mismatch_remesas) %>%
  mutate(
    admin_pad = paste0(" ", admin_name_nocoma, " "),
    nom_pad   = paste0(" ", nom_mun_clean, " ")
  ) 

correct_D <- crosswalk_D %>% 
  filter(str_detect(admin_pad, fixed(nom_pad)) & state_code_censo == state_code_remesas) %>%
  select(inegi, nom_mun_clean, state_code_remesas)

# E. Corrijo los municipios que salen de only remesas y tienen una contrapartida con un mismatch del censo
crosswalk_E <- crossing(only_remesas, mismatch_censo) %>% 
  mutate(
    admin_name_nocoma = admin_name_clean %>%
      str_replace_all(",", " ") %>%  # reemplaza comas por espacios
      str_squish(),                  # elimina espacios dobles
    admin_pad = paste0(" ", admin_name_nocoma, " "),
    nom_pad   = paste0(" ", nom_mun_clean, " ")
  ) 

correct_E <- crosswalk_E %>% 
  filter(str_detect(admin_name_clean, fixed(nom_mun_clean)) & state_code_censo == state_code_remesas) %>%
  select(inegi, nom_mun_clean, state_code_remesas)

correct_E %>% 
  count(inegi, nom_mun_clean, state_code_remesas) %>%
  filter(n > 1) # no hay casos con más de una coincidencia

corregidos_only_remesas <- c(correct_E$nom_mun_clean, correct_B$nom_mun_clean)
corregidos_only_remesas_df <- rbind(correct_E, correct_B)
corregidos_only_remesas_df %>% 
  count(inegi, nom_mun_clean, state_code_remesas) %>%
  filter(n > 1) # los que se repiten son los que van a agregados
corregidos_only_remesas_df <- corregidos_only_remesas_df %>% distinct()
setdiff(only_remesas$nom_mun_clean, corregidos_only_remesas) # 129 del total de only remesas corregidos + 4 es 133. Los 4 que faltan están mal escritos o cortados
setdiff(corregidos_only_remesas, only_remesas$nom_mun_clean)

# Agrego a mano los códigos de los 4 que faltan
correct_E <- correct_E %>% 
  rbind(data.frame(
    inegi = c("07059", "07008", "15025", "26048"),
    nom_mun_clean = c("BENEMERITO DE LAS AMERI","MONTECRISTO DE GUERRERO","VALLE DE CHALCO SOLIDARID","GENERAL PLUTARCO ELIAS C"),
    state_code_remesas = c("07", "07", "15", "26") 
  )) 

# Conteo de los corregidos
corregidos <- rbind(correct_A, 
                    correct_B, 
                    correct_C, 
                    correct_D, 
                    correct_E) %>%
  distinct() # 1010

# F. Corrijo los close
close <- diag_merge %>% 
  filter(merge_status == "close") %>% 
  select(inegi, admin_name_clean, nom_mun_clean, state_code_remesas) %>% 
  mutate(state_code_censo = str_sub(inegi, 1, 2),
         coinciden = ifelse(state_code_remesas == state_code_censo, "si", "no"))
table(close$coinciden) # todos parecen ser los mismos 

# Hay algunos municipios que tienen coincidencia exacta con otros más que con los de close, los corrijo
# Caso a: el municipio de close es de remesas y su contraparte es un municipio del censo con mismatch
correct_Fa <- close %>% 
  inner_join(mismatch_censo, by = c("nom_mun_clean" = "admin_name_clean", "state_code_censo")) %>% 
  select(-inegi.x) %>% 
  rename(inegi = inegi.y) %>% 
  select(inegi, nom_mun_clean, state_code_remesas)

# Caso b: el municipio de close es del censo y su contraparte es un municipio de remesas con mismatch
correct_Fb <- close %>% 
  select(inegi, admin_name_clean, state_code_remesas) %>%
  inner_join(mismatch_remesas, by = c( "admin_name_clean" = "nom_mun_clean" , "state_code_remesas")) %>% 
  rename(nom_mun_clean = admin_name_clean)

correct_F <- rbind(correct_Fa, correct_Fb)

# G. CHEQUEOS Y CORREGIDOS TOTALES

corregidos <- rbind(corregidos, correct_F) %>% distinct()
length(unique(corregidos$inegi)) 
# 940 inegis únicos corregidos
# pero si los municipios de remesas se mapean con agregados del censo, los inegis únicos corregidos van a ver menos que la cantidad a corregir de inegis

# Cuento municipios de remesas que necesitan corrección (porque su inegi se unio con otro incorrectamente -mismatch- o porque no se unio con ninguno -only_remesas-)
a_corregir <- diag_merge %>% 
  filter(merge_status == "mismatch" | merge_status == "only_remesas") %>%
  # filtro inegis que terminan en 999
  filter(!str_ends(inegi, "999"))
length(unique(a_corregir$inegi)) # 1030 inegi únicos a corregir (más los 4 que decian close pero eran mismatch)

a_corregir %>% 
  count(nom_mun_clean, state_code_remesas) %>%
  filter(n > 1) 

# Chequeo duplicados por llave
corregidos %>%
  count(nom_mun_clean, state_code_remesas) %>%
  filter(n > 1)
# corrijo a mano el duplicado
corregidos <- corregidos %>%
  filter(!(nom_mun_clean == "MORELOS" & inegi == "15011"))

# Municipios que faltan corregir
anti_join(
  a_corregir %>% distinct(nom_mun_clean, state_code_remesas),
  corregidos %>% distinct(nom_mun_clean, state_code_remesas),
  by = c("nom_mun_clean", "state_code_remesas")
)

# Les asigno el inegi correcto a mano (hay dos municipios que no tienen: SAN PEDRO MIXTEPEC - DIST, ZAPOTITLAN DEL RIO)
manual_corrections <- anti_join(
  a_corregir %>% distinct(nom_mun_clean, state_code_remesas),
  corregidos %>% distinct(nom_mun_clean, state_code_remesas),
  by = c("nom_mun_clean", "state_code_remesas")
) 

# write_xlsx(manual_corrections, "Data out/manual_corrections.xlsx")

manual_corrections <- read_excel("Data out/manual_corrections.xlsx") %>% 
  select(inegi, nom_mun_clean, state_code_remesas)

# Agrego los corregidos a mano a los corregidos totales
corregidos <- rbind(corregidos, manual_corrections) 

# Chequeo correcciones extra
anti_join(
  corregidos %>% distinct(nom_mun_clean, state_code_remesas),
  a_corregir %>% distinct(nom_mun_clean, state_code_remesas),
  by = c("nom_mun_clean", "state_code_remesas")
) # son los 4 close que estaban mal

# Corregidos tiene 1034 municipios (state_code_remesas + nom_mun_clean unicos) --> coincide con los 1030 inegi a corregir + los 4 que eran close

#### Uno los iengi corregidos con la data de remesas  #### 

# Remesas anual
remesas_yr <- remesas_yr %>% 
  # creo el codigo de estado como los dos primeros dígitos del inegi
  mutate(state_code_remesas = str_sub(inegi, 1, 2)) %>%
  # agrego los municipios corregidos
  left_join(corregidos %>% rename(inegi_corregido = inegi), 
            by = c("nom_mun_clean", "state_code_remesas"))

# Unifico los inegi
remesas_yr <- remesas_yr %>%
  mutate(
    inegi_final = if_else(!is.na(inegi_corregido), inegi_corregido, inegi)
  ) %>% 
  select(-inegi, -inegi_corregido) %>% 
  rename(inegi = inegi_final) %>% 
  arrange(inegi) %>% 
  select(inegi, nom_mun_clean, year, total_remesas, state_code_remesas)

any(is.na(remesas_yr$inegi)) # no hay NA

# Reemplazo por NA en los municipios que no tenían un inegi en el censo
remesas_yr <- remesas_yr %>%
  mutate(
    inegi = if_else(nom_mun_clean=="SAN PEDRO MIXTEPEC - DIST" | nom_mun_clean=="ZAPOTITLAN DEL RIO",
                    NA_character_, inegi)
  )
}
# -------------------- #
#  Migración Mex-EEUU
# -------------------- #
{
mig_eeuu_2010 <- read_csv("Data Raw/Migracion EEUU/05_iim_mex_eeuu_2010_municipio.csv")
mig_eeuu_2000 <- read_csv("Data Raw/Migracion EEUU/04_iim_mex_eeuu_2000_municipio.csv")

length(unique(mig_eeuu_2010$cve_mun)) # 2456 municipios
length(unique(mig_eeuu_2000$cve_mun)) # 2456 municipios

# viv_emig: Porcentaje de viviendas con emigrantes con destino a Estados Unidos y/o residentes en Estados Unidos

mig_eeuu_2010 <- mig_eeuu_2010 %>% 
  rename(
    inegi = cve_mun,
    tot_viv_10 = tot_viv,
    viv_rem_10 = viv_rem,
    viv_emig_10 = viv_emig,
    viv_circ_10 = viv_circ,
    viv_ret_10 = viv_ret,
    iaim_10 = iaim,
    gaim_10 = gaim,
    pos_nal_10 = pos_nal
  ) %>% 
  mutate(
    #inegi de 5 digitos
    inegi = str_pad(as.character(inegi), width = 5, side = "left", pad = "0")
  )

mig_eeuu_2000 <- mig_eeuu_2000 %>% 
  rename(
    inegi = cve_mun,
    tot_viv_00 = tot_viv,
    viv_rem_00 = viv_rem,
    viv_emig_00 = viv_emig,
    viv_circ_00 = viv_circ,
    viv_ret_00 = viv_ret,
    iaim_00 = iaim,
    gaim_00 = gaim,
    pos_nal_00 = pos_nal
  ) %>% 
  mutate(
    #inegi de 5 digitos
    inegi = str_pad(as.character(inegi), width = 5, side = "left", pad = "0")
  )

#chequeos
any(is.na(mig_eeuu_2000$tot_viv_00))
any(is.na(mig_eeuu_2000$viv_emig_00))
mig_eeuu_2000 %>% filter(is.na(tot_viv_00) | is.na(viv_emig_00)) %>% View()

any(is.na(mig_eeuu_2010$tot_viv_10))
any(is.na(mig_eeuu_2010$viv_emig_10))

#### Diagnostico de municipios en la data de migración #### 

mig_eeuu_2000 <- mig_eeuu_2000 %>% 
  # Limpio los nombres de los municipios para poder unir
  mutate(
    nom_mun_clean = stri_trans_general(nom_mun, "Latin-ASCII"),
    nom_mun_clean = toupper(nom_mun_clean),
    state_code_mig = str_sub(inegi, 1, 2)
  ) 

# chequeo si hay municipios de migración que no estén en remesas
anti_join(mig_eeuu_2000, remesas_yr %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
          by = c("nom_mun_clean", "state_code_mig" = "state_code_remesas")) %>% 
  View()
# chequeo si hay municipios de remesas que no están en migración
anti_join(remesas_yr %>% select(nom_mun_clean, state_code_remesas) %>% distinct(), mig_eeuu_2000,
          by = c("nom_mun_clean", "state_code_remesas" = "state_code_mig")) %>% 
  View()

# chequeo si los municipios de 2010 de migración son iguales a los de 2000
mig_eeuu_2010 <- mig_eeuu_2010 %>% 
  # Limpio los nombres de los municipios para poder unir
  mutate(
    nom_mun_clean = stri_trans_general(nom_mun, "Latin-ASCII"),
    nom_mun_clean = toupper(nom_mun_clean),
    state_code_mig = str_sub(inegi, 1, 2)
  ) 

anti_join(mig_eeuu_2000 %>% select(nom_mun_clean, state_code_mig) %>% distinct(), 
           mig_eeuu_2010 %>% select(nom_mun_clean, state_code_mig) %>% distinct(),
          by = c("nom_mun_clean", "state_code_mig")) %>% 
  View() # coinciden todos, puedo usar una sola data de base para la corrección

# Municipios únicos en la data de migración
municipios_mig <- mig_eeuu_2000 %>% 
  select(inegi, nom_mun_clean, state_code_mig) %>% 
  distinct() %>% 
  arrange(inegi)

# Merge por INEGI
diag_merge_mig <- municipios_censo %>%
  full_join(municipios_mig, by = "inegi") %>%
  mutate(
    # match exacto
    exact_match = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      admin_name_clean == nom_mun_clean,
    
    # uno contiene al otro
    name_contained = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      (
        str_detect(admin_name_clean, fixed(nom_mun_clean)) |
          str_detect(nom_mun_clean, fixed(admin_name_clean))
      ),
    
    # el nombre del censo parece agregar varios municipios
    census_aggregation = !is.na(admin_name_clean) &
      str_detect(admin_name_clean, ","),
    
    # reemplazo a mano el municipio que tiene una coma en el nombre eliminando la coma
    census_aggregation = ifelse(admin_name_clean == "VILLA TEZOATLAN DE SEGURA Y LUNA, CUNA DE LA INDEPENDENCIA DE OAXACA, HEROICA",
                                FALSE, census_aggregation),
    
    # clasificación final (toma el primer TRUE)
    merge_status = case_when(
      is.na(admin_name_clean) ~ "only_mig",
      is.na(nom_mun_clean) ~ "only_censo",
      exact_match ~ "exact",
      census_aggregation & name_contained ~ "part of aggregation",
      name_contained ~ "close",
      TRUE ~ "mismatch"
    )
  )

table(diag_merge_mig$merge_status)

# Municipios a corregir
a_corregir_mig <- diag_merge_mig %>% 
  filter(merge_status == "mismatch" | merge_status == "only_mig") %>%
  select(inegi, nom_mun_clean, state_code_mig)

# Cuantos de los a corregir no están ya en los corregidos de las remesas
a_corregir_mig %>% 
  anti_join(corregidos %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
            by = c("nom_mun_clean", "state_code_mig" = "state_code_remesas")) %>% 
  View() 

# Hay municipios en corregidos que ahora no haya que corregir?
corregidos %>% 
  anti_join(a_corregir_mig %>% select(nom_mun_clean, state_code_mig) %>% distinct(),
            by = c("nom_mun_clean", "state_code_remesas" = "state_code_mig")) %>% 
  View()

# Los que no coinciden son los que tienen nombre truncado + los 2 que no tienen inegi + los close (no estan en a corregir)

# Cruzo los a corregir que faltan con el crosswalk de municipios de mirgacion
faltan_corregir_mig <- a_corregir_mig %>% 
  anti_join(corregidos %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
            by = c("nom_mun_clean", "state_code_mig" = "state_code_remesas")) %>% 
  select(-inegi)

faltan_corregir_mig %>% count(nom_mun_clean, state_code_mig) %>% filter(n > 1) # no hay repeticiones
  
croswalk_mig <- crossing(faltan_corregir_mig, municipios_censo) %>% 
  mutate(
    admin_name_nocoma = admin_name_clean %>%
      str_replace_all(",", " ") %>% # reemplaza comas por espacios
      str_squish(), # elimina espacios dobles
    admin_pad = paste0(" ", admin_name_nocoma, " "),
    nom_pad   = paste0(" ", nom_mun_clean, " ")
  )

correct_mig <- croswalk_mig %>% 
  filter(str_detect(admin_pad, fixed(nom_pad)) & state_code_censo == state_code_mig) %>%
  select(inegi, nom_mun_clean, state_code_mig)

# chequeo que no haya duplicados en los municipios
correct_mig %>% 
  count(state_code_mig, nom_mun_clean) %>%
  filter(n>1) # no hay

# Se corrigieron 53, veo cuales faltan
anti_join(faltan_corregir_mig, correct_mig, by = c("nom_mun_clean", "state_code_mig")) %>%
  View() # son 5 que tienen nombre distinto escrito al censo y a la data de remesas, los corrijo a mano

correct_mig <- correct_mig %>% 
  rbind(anti_join(faltan_corregir_mig, correct_mig, by = c("nom_mun_clean", "state_code_mig")) %>% 
          mutate(inegi = case_when(
            nom_mun_clean == "SAN JUAN MIXTEPEC -DTO. 08 -" ~ "20207",
            nom_mun_clean == "SAN JUAN MIXTEPEC -DTO. 26 -" ~ "20208",
            nom_mun_clean == "SAN PEDRO MIXTEPEC -DTO. 22 -" ~ "20317",
            nom_mun_clean == "SAN PEDRO MIXTEPEC -DTO. 26 -" ~ "20318",
            nom_mun_clean == "SAN MATEO YUCUTINDO" ~ "20562",
            TRUE ~ NA_character_
            )
          )
  )

correct_mig_totales <- rbind(
  correct_mig, corregidos %>% rename(state_code_mig = state_code_remesas)
)
correct_mig_totales %>% count(state_code_mig, nom_mun_clean) %>%
  filter(n>1) 
length(unique(correct_mig_totales$inegi))

a_corregir_mig %>% count(state_code_mig, nom_mun_clean) %>%
  filter(n>1) 

# Hay correcciones de mas?
anti_join(correct_mig_totales %>% select(nom_mun_clean, state_code_mig) %>% distinct(),
          a_corregir_mig %>% select(nom_mun_clean, state_code_mig) %>% distinct(), 
          by = c("nom_mun_clean", "state_code_mig")) %>% 
  View()
  # son los 60 que tenían nombre cortado en remesas y estaban en "corregidos" + los close
  # 1092-60 = 1032 que es el total de municipios a corregir en migración
  # los de nombre cortado no son un problema porque no se van a mergear por el nombre con la data de migración en el proximo paso
  # los close se unen bien después con la data de migración y se corrigen

#### Uno los iengi corregidos con la data de migración  #### 

# Migración 2000
mig_eeuu_2000 <- mig_eeuu_2000 %>% 
  # agrego los municipios corregidos
  left_join(correct_mig_totales %>% rename(inegi_corregido = inegi), 
            by = c("nom_mun_clean", "state_code_mig"))
# Migración 2010
mig_eeuu_2010 <- mig_eeuu_2010 %>% 
  # agrego los municipios corregidos
  left_join(correct_mig_totales %>% rename(inegi_corregido = inegi), 
            by = c("nom_mun_clean", "state_code_mig"))

# Unifico los inegi
  # Migración 2000
mig_eeuu_2000_c <- mig_eeuu_2000 %>%
  mutate(
    inegi_final = if_else(!is.na(inegi_corregido), inegi_corregido, inegi)
  ) %>% 
  select(-inegi, -inegi_corregido) %>% 
  rename(inegi = inegi_final) %>% 
  arrange(inegi) %>% 
  select(inegi, nom_mun_clean, state_code_mig, everything())
  
# Migración 2010
mig_eeuu_2010_c <- mig_eeuu_2010 %>%
  mutate(
    inegi_final = if_else(!is.na(inegi_corregido), inegi_corregido, inegi)
  ) %>% 
  select(-inegi, -inegi_corregido) %>% 
  rename(inegi = inegi_final) %>% 
  arrange(inegi) %>% 
  select(inegi, nom_mun_clean, state_code_mig, everything())

any(is.na(mig_eeuu_2000_c$inegi)) # no hay NA
any(is.na(mig_eeuu_2010_c$inegi)) # no hay NA

# Agrego la data a nivel de inegi
  # Migración del 2000
mig_eeuu_2000_c <- mig_eeuu_2000_c %>% 
  mutate(
    # Cantidad de viviendas con emigrantes a EEUU
    viv_emig_cant_00 = (viv_emig_00 / 100) * tot_viv_00,
    # Cantidad de viviendas que reciben remesas de EEUU
    viv_rem_cant_00 = (viv_rem_00 / 100) * tot_viv_00,
    # Cantidad de viviendas con migrantes circulares
    viv_circ_cant_00 = (viv_circ_00 / 100) * tot_viv_00,
    # Cantidad de viviendas con migrantes retornados
    viv_ret_cant_00 = (viv_ret_00 / 100) * tot_viv_00
  ) %>% 
  select(inegi,tot_viv_00, viv_emig_cant_00, viv_rem_cant_00, viv_circ_cant_00, viv_ret_cant_00) %>% 
  group_by(inegi) %>% 
  summarise(
    # Calculo los totales
    tot_viv_00 = sum(tot_viv_00, na.rm = TRUE),
    viv_emig_cant_00 = sum(viv_emig_cant_00, na.rm = TRUE),
    viv_rem_cant_00 = sum(viv_rem_cant_00, na.rm = TRUE),
    viv_circ_cant_00 = sum(viv_circ_cant_00, na.rm = TRUE),
    viv_ret_cant_00 = sum(viv_ret_cant_00, na.rm = TRUE)
  ) %>% 
  mutate(
    # Creo los porcentajes
    viv_emig_00 = (viv_emig_cant_00 / tot_viv_00) * 100,
    viv_rem_00 = (viv_rem_cant_00 / tot_viv_00) * 100,
    viv_circ_00 = (viv_circ_cant_00 / tot_viv_00) * 100,
    viv_ret_00 = (viv_ret_cant_00 / tot_viv_00) * 100
  )

  # Migración del 2010
mig_eeuu_2010_c <- mig_eeuu_2010_c %>%
  mutate(
    # Cantidad de viviendas con emigrantes a EEUU
    viv_emig_cant_10 = (viv_emig_10 / 100) * tot_viv_10,
    # Cantidad de viviendas que reciben remesas de EEUU
    viv_rem_cant_10 = (viv_rem_10 / 100) * tot_viv_10,
    # Cantidad de viviendas con migrantes circulares
    viv_circ_cant_10 = (viv_circ_10 / 100) * tot_viv_10,
    # Cantidad de viviendas con migrantes retornados
    viv_ret_cant_10 = (viv_ret_10 / 100) * tot_viv_10
  ) %>% 
  select(inegi,tot_viv_10, viv_emig_cant_10, viv_rem_cant_10, viv_circ_cant_10, viv_ret_cant_10) %>% 
  group_by(inegi) %>% 
  summarise(
    # Calculo los totales
    tot_viv_10 = sum(tot_viv_10, na.rm = TRUE),
    viv_emig_cant_10 = sum(viv_emig_cant_10, na.rm = TRUE),
    viv_rem_cant_10 = sum(viv_rem_cant_10, na.rm = TRUE),
    viv_circ_cant_10 = sum(viv_circ_cant_10, na.rm = TRUE),
    viv_ret_cant_10 = sum(viv_ret_cant_10, na.rm = TRUE)
  ) %>% 
  mutate(
    # Creo los porcentajes
    viv_emig_10 = (viv_emig_cant_10 / tot_viv_10) * 100,
    viv_rem_10 = (viv_rem_cant_10 / tot_viv_10) * 100,
    viv_circ_10 = (viv_circ_cant_10 / tot_viv_10) * 100,
    viv_ret_10 = (viv_ret_cant_10 / tot_viv_10) * 100
  )
}
# -------------------- #
# Covariables población  
# -------------------- #
{
# Población por edades  
pob_edades <- read_excel("Data Raw/Población México/Censo 2020/tabulado_pob_edades.xlsx") 
  
colnames(pob_edades)[2] <- "municipio"
colnames(pob_edades)[1] <- "inegi"

pob_edades <- pob_edades %>% 
  mutate(
    inegi = str_replace_all(inegi, " ", ""),  # elimina todos los espacios
    cve_ent = str_sub(inegi, 1, 2),
    cve_mun = str_sub(inegi, 3, 5),
  ) %>% 
  filter(!is.na(inegi) & str_length(inegi) > 2) %>% 
  janitor::clean_names()

# Variables población, por sexo, y edad mediana
composicion_edad_sexo <- read_excel("Data Raw/Población México/Censo 2020/tabla_indicadores_entidad_clean.xlsx",
                                    sheet = "composicion_edad_sexo") 
composicion_edad_sexo <- composicion_edad_sexo %>% 
  janitor::clean_names() %>%
  # Elimino obs de los totales de cada estado
  filter(!str_starts(municipio, "000 ")) %>%  
  mutate(
    # Creo el inegi
    cve_ent = str_sub(entidad_federativa, 1, 2),
    cve_mun = str_sub(municipio, 1, 3),
    inegi   = paste0(cve_ent, cve_mun),
    # Limpio los nombres de los municipios y estados
    municipio = str_remove(municipio, "^\\d{3}\\s+"),
    entidad_federativa = str_remove(entidad_federativa, "^\\d{2}\\s+")
    ) %>% 
  select(inegi, cve_ent, cve_mun, everything()) %>% 
  arrange(inegi, cve_ent, cve_mun)

# Distribución territorial
distribucion_territorial <- read_excel("Data Raw/Población México/Censo 2020/tabla_indicadores_entidad_clean.xlsx",
                                        sheet = "distribucion_territorial")
distribucion_territorial <- distribucion_territorial %>%
  janitor::clean_names() %>%
  rename(densidad_pob = densidad_de_poblacion_hab_km2) %>% 
  # Elimino obs de los totales de cada estado
  filter(!str_starts(municipio, "000 ")) %>%  
  mutate(
    # Creo el inegi
    cve_ent = str_sub(entidad_federativa, 1, 2),
    cve_mun = str_sub(municipio, 1, 3),
    inegi   = paste0(cve_ent, cve_mun),
    # Limpio los nombres de los municipios y estados
    municipio = str_remove(municipio, "^\\d{3}\\s+"),
    entidad_federativa = str_remove(entidad_federativa, "^\\d{2}\\s+")
  ) %>% 
  select(inegi, cve_ent, cve_mun, everything()) %>% 
  arrange(inegi, cve_ent, cve_mun)

# Carcterísticas económicas
car_economicas <- read_excel("Data Raw/Población México/Censo 2020/tabla_indicadores_entidad_clean.xlsx",
                             sheet = "car_economicas")
car_economicas <- car_economicas %>%
  janitor::clean_names() %>%
  # Elimino obs de los totales de cada estado
  filter(!str_starts(municipio, "000 ")) %>%  
  mutate(
    # Creo el inegi
    cve_ent = str_sub(entidad_federativa, 1, 2),
    cve_mun = str_sub(municipio, 1, 3),
    inegi   = paste0(cve_ent, cve_mun),
    # Limpio los nombres de los municipios y estados
    municipio = str_remove(municipio, "^\\d{3}\\s+"),
    entidad_federativa = str_remove(entidad_federativa, "^\\d{2}\\s+")
  ) %>% 
  select(inegi, cve_ent, cve_mun, everything()) %>% 
  arrange(inegi, cve_ent, cve_mun)

# Características educativas
car_educativas <- read_excel("Data Raw/Población México/Censo 2020/tabla_indicadores_entidad_clean.xlsx",
                             sheet = "car_educativas")
car_educativas <- car_educativas %>%
  janitor::clean_names() %>%
  # Elimino obs de los totales de cada estado
  filter(!str_starts(municipio, "000 ")) %>%  
  mutate(
    # Creo el inegi
    cve_ent = str_sub(entidad_federativa, 1, 2),
    cve_mun = str_sub(municipio, 1, 3),
    inegi   = paste0(cve_ent, cve_mun),
    # Limpio los nombres de los municipios y estados
    municipio = str_remove(municipio, "^\\d{3}\\s+"),
    entidad_federativa = str_remove(entidad_federativa, "^\\d{2}\\s+")
  ) %>% 
  select(inegi, cve_ent, cve_mun, everything()) %>% 
  arrange(inegi, cve_ent, cve_mun)

# Mergeo la data de las covariables de población
cov_censo2020 <- composicion_edad_sexo %>% 
  full_join(distribucion_territorial, by = c("inegi", "cve_ent", "cve_mun", "entidad_federativa", "municipio")) %>% 
  full_join(car_economicas, by = c("inegi", "cve_ent", "cve_mun", "entidad_federativa", "municipio")) %>% 
  full_join(car_educativas, by = c("inegi", "cve_ent", "cve_mun", "entidad_federativa", "municipio")) %>% 
  select(inegi, cve_ent, cve_mun, everything()) %>% 
  arrange(inegi, cve_ent, cve_mun)

# Chqueo coincidencias entre los 4 datasets de covariables
any(is.na(cov_censo2020$poblacion_total)) # no hay NA de composicion_edad_sexo
any(is.na(cov_censo2020$densidad_pob))    # no hay NA de distribucion_territorial
any(is.na(cov_censo2020$pea))             # no hay NA de car_economicas
any(is.na(cov_censo2020$alfb_15_24))      # no hay NA de car_educativas

# Mergeo con la población por edades. Lo hago por separado para chequar mejor poque no viene de la misma data (aunque debería venir con el mismo inegi porque son del censo 2020)
cov_censo2020 <- cov_censo2020 %>% 
  full_join(pob_edades %>% select(-total, -municipio), by = c("inegi", "cve_ent", "cve_mun"))

# Chequeo que se haya unido todo
any(is.na(cov_censo2020$de_0_a_4_anos)) # false
any(is.na(cov_censo2020$poblacion_total)) # false

#### Diagnostico de municipios ####

cov_censo2020 <- cov_censo2020 %>% 
  # Limpio los nombres de los municipios para poder unir
  mutate(
    nom_mun_clean = stri_trans_general(municipio, "Latin-ASCII"),
    nom_mun_clean = toupper(nom_mun_clean)
  ) %>% 
  rename(state_code_cov = cve_ent)

# chequeo si hay municipios de covariables que no estén en remesas
anti_join(cov_censo2020, remesas_yr %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
          by = c("nom_mun_clean", "state_code_cov" = "state_code_remesas")) %>% 
  View() # esto puede ser porque no hay data de remesas para esos municipios
# chequeo si hay municipios de remesas que no están en covariables
anti_join(remesas_yr %>% select(nom_mun_clean, state_code_remesas) %>% distinct(), cov_censo2020,
          by = c("nom_mun_clean", "state_code_remesas" = "state_code_cov")) %>% 
  View() # son nombres cortados y no identificados

# Municipios únicos en la data de covariables
municipios_cov <- cov_censo2020 %>% 
  select(inegi, nom_mun_clean, state_code_cov) %>% 
  distinct() %>% 
  arrange(inegi)

# Merge por INEGI
diag_merge_cov <- municipios_censo %>%
  full_join(municipios_cov, by = "inegi") %>%
  mutate(
    # match exacto
    exact_match = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      admin_name_clean == nom_mun_clean,
    
    # uno contiene al otro
    name_contained = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      (
        str_detect(admin_name_clean, fixed(nom_mun_clean)) |
          str_detect(nom_mun_clean, fixed(admin_name_clean))
      ),
    
    # el nombre del censo parece agregar varios municipios
    census_aggregation = !is.na(admin_name_clean) &
      str_detect(admin_name_clean, ","),
    
    # reemplazo a mano el municipio que tiene una coma en el nombre eliminando la coma
    census_aggregation = ifelse(admin_name_clean == "VILLA TEZOATLAN DE SEGURA Y LUNA, CUNA DE LA INDEPENDENCIA DE OAXACA, HEROICA",
                                FALSE, census_aggregation),
    
    # clasificación final (toma el primer TRUE)
    merge_status = case_when(
      is.na(admin_name_clean) ~ "only_cov",
      is.na(nom_mun_clean) ~ "only_censo",
      exact_match ~ "exact",
      census_aggregation & name_contained ~ "part of aggregation",
      name_contained ~ "close",
      TRUE ~ "mismatch"
    )
  )

table(diag_merge_cov$merge_status)

# Municipios a corregir
a_corregir_cov <- diag_merge_cov %>% 
  filter(merge_status == "mismatch" | merge_status == "only_cov") %>%
  select(inegi, nom_mun_clean, state_code_cov)

# Cuantos de los a corregir no están ya en los corregidos de las remesas
a_corregir_cov %>% 
  anti_join(corregidos %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
            by = c("nom_mun_clean", "state_code_cov" = "state_code_remesas")) %>% 
  View() 

# Hay municipios en corregidos que ahora no haya que corregir?
corregidos %>% 
  anti_join(a_corregir_cov %>% select(nom_mun_clean, state_code_cov) %>% distinct(),
            by = c("nom_mun_clean", "state_code_remesas" = "state_code_cov")) %>% 
  View()
# Los que no coinciden son los que tienen nombre truncado + los 2 que no tienen inegi + los close (no estan en a corregir)

# Cruzo los a-corregir que faltan con el crosswalk de municipios de covariables
faltan_corregir_cov <- a_corregir_cov %>% 
  anti_join(corregidos %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
            by = c("nom_mun_clean", "state_code_cov" = "state_code_remesas")) %>% 
  select(-inegi)

faltan_corregir_cov %>% count(nom_mun_clean, state_code_cov) %>% filter(n > 1) # no hay repeticiones

croswalk_cov <- crossing(faltan_corregir_cov, municipios_censo) %>% 
  mutate(
    admin_name_nocoma = admin_name_clean %>%
      str_replace_all(",", " ") %>% # reemplaza comas por espacios
      str_squish(), # elimina espacios dobles
    admin_pad = paste0(" ", admin_name_nocoma, " "),
    nom_pad   = paste0(" ", nom_mun_clean, " ")
  )

correct_cov <- croswalk_cov %>% 
  filter(str_detect(admin_pad, fixed(nom_pad)) & state_code_censo == state_code_cov) %>%
  select(inegi, nom_mun_clean, state_code_cov)

# chequeo que no haya duplicados en los municipios
correct_cov %>% 
  count(state_code_cov, nom_mun_clean) %>%
  filter(n>1) # no hay

# Se corrigieron 66, veo cuales faltan
anti_join(faltan_corregir_cov, correct_cov, by = c("nom_mun_clean", "state_code_cov")) %>%
  View() # son 8 que tienen nombre distinto escrito al censo y a la data de remesas, los corrijo a mano

correct_cov <- correct_cov %>% 
  rbind(anti_join(faltan_corregir_cov, correct_cov, by = c("nom_mun_clean", "state_code_cov")) %>% 
          mutate(inegi = case_when(
            nom_mun_clean == "SAN JUAN MIXTEPEC - DTO. 08" ~ "20207",
            nom_mun_clean == "SAN JUAN MIXTEPEC - DTO. 26" ~ "20208",
            nom_mun_clean == "SAN PEDRO MIXTEPEC - DTO. 22" ~ "20317",
            nom_mun_clean == "SAN PEDRO MIXTEPEC - DTO. 26" ~ "20318",
            nom_mun_clean == "BATOPILAS DE MANUEL GOMEZ MORIN" ~ "08007",
            nom_mun_clean == "VILLA DE SANTIAGO CHAZUMBA" ~ "20458",
            nom_mun_clean == "MEDELLIN DE BRAVO" ~ "30103",
            nom_mun_clean == "HEROICA VILLA TEZOATLAN DE SEGURA Y LUNA, CUNA DE LA INDEPENDENCIA DE OAXACA" ~ "20545",
            TRUE ~ NA_character_
          )
          )
  )

correct_cov_totales <- rbind(
  correct_cov, corregidos %>% rename(state_code_cov = state_code_remesas)
)
correct_cov_totales %>% count(state_code_cov, nom_mun_clean) %>%
  filter(n>1) 
length(unique(correct_cov_totales$inegi)) # 965

a_corregir_mig %>% count(state_code_mig, nom_mun_clean) %>%
  filter(n>1) 

# Hay correcciones de mas?
anti_join(correct_cov_totales %>% select(nom_mun_clean, state_code_cov) %>% distinct(),
          a_corregir_cov %>% select(nom_mun_clean, state_code_cov) %>% distinct(), 
          by = c("nom_mun_clean", "state_code_cov")) %>% 
  View()
# son los 72 que tenían nombre cortado en remesas y estaban en "corregidos" + los close
# 1108-72 = 1036 que es el total de municipios a corregir en covariables
# los de nombre cortado no son un problema porque no se van a mergear por el nombre con la data de covariables en el proximo paso
# los close se unen bien después con el nombre y se corrigen

#### Uno los iengi corregidos con la data de covariables  #### 

cov_censo2020 <- cov_censo2020 %>% 
  # agrego los municipios corregidos
  left_join(correct_cov_totales %>% rename(inegi_corregido = inegi), 
            by = c("nom_mun_clean", "state_code_cov"))

# Unifico los inegi
cov_censo2020_c <- cov_censo2020 %>%
  mutate(
    inegi_final = if_else(!is.na(inegi_corregido), inegi_corregido, inegi)
  ) %>% 
  select(-inegi, -inegi_corregido) %>% 
  rename(inegi = inegi_final) %>% 
  arrange(inegi) %>% 
  select(inegi, nom_mun_clean, state_code_cov, everything())

any(is.na(cov_censo2020_c$inegi)) # no hay NA
any(is.na(cov_censo2020_c$inegi)) # no hay NA

cov_censo2020_c <- cov_censo2020_c %>% 
  # elimino variables que no voy a poder agregar
  select(-pea_h, -pea_m, -ocupados_h, -ocupados_m, -densidad_pob, -edad_mediana, 
         -relacion_hombres_mujeres, -razon_de_dependencia)

# Reconstruyo variables que sí puedo agregar a nivel de inegi
sapply(cov_censo2020_c %>% select(starts_with("de_")), class)
cov_censo2020_c <- cov_censo2020_c %>%
  mutate(
    across(starts_with("de_"), ~ as.numeric(str_replace_all(.x, ",", "")))
  )

cov_censo2020_agg <- cov_censo2020_c %>% 
  mutate(
    hombres = porcentaje_de_hombres * poblacion_total,
    mujeres = porcentaje_de_mujeres * poblacion_total,
    
    pob_10mas = rowSums(cbind(
      de_10_a_14_anos, de_15_a_19_anos, de_20_a_24_anos, de_25_a_29_anos,
      de_30_a_34_anos, de_35_a_39_anos, de_40_a_44_anos, de_45_a_49_anos,
      de_50_a_54_anos, de_55_a_59_anos, de_60_a_64_anos, de_65_a_69_anos,
      de_70_a_74_anos, de_75_a_79_anos, de_80_a_84_anos
    ), na.rm = TRUE),
    
    pob_15mas = rowSums(cbind(
      de_15_a_19_anos, de_20_a_24_anos, de_25_a_29_anos,
      de_30_a_34_anos, de_35_a_39_anos, de_40_a_44_anos, de_45_a_49_anos,
      de_50_a_54_anos, de_55_a_59_anos, de_60_a_64_anos, de_65_a_69_anos,
      de_70_a_74_anos, de_75_a_79_anos, de_80_a_84_anos
    ), na.rm = TRUE),
    
    pea_cantidad = pea * pob_15mas,
    ocupados_cantidad = ocupados * pea_cantidad,
    
    sin_escolaridad_cant = sin_escolaridad * pob_15mas,
    esc_basica_cant = esc_basica * pob_15mas,
    esc_media_superior_cant = esc_media_superior * pob_15mas,
    esc_superior_cant = esc_superior * pob_15mas,
    
    pob_15_24 = rowSums(cbind(de_15_a_19_anos, de_20_a_24_anos), na.rm = TRUE),
    
    pob_25mas = rowSums(cbind(
      de_25_a_29_anos, de_30_a_34_anos, de_35_a_39_anos, de_40_a_44_anos,
      de_45_a_49_anos, de_50_a_54_anos, de_55_a_59_anos, de_60_a_64_anos,
      de_65_a_69_anos, de_70_a_74_anos, de_75_a_79_anos, de_80_a_84_anos
    ), na.rm = TRUE),
    
    alfb_15_24_cant = alfb_15_24 * pob_15_24,
    alfb_25mas_cant = alfb_25mas * pob_25mas
  ) %>% 
  group_by(inegi) %>%
  summarise(
    state_code_cov = first(state_code_cov),
    entidad_federativa = first(entidad_federativa),
    
    across(starts_with("de_"), ~sum(.x, na.rm = TRUE)),
    
    poblacion_total = sum(poblacion_total, na.rm = TRUE),
    sup_km2_total = sum(superficie_km2, na.rm = TRUE),
    densidad = poblacion_total / sup_km2_total,
    
    hombres = sum(hombres, na.rm = TRUE),
    mujeres = sum(mujeres, na.rm = TRUE),
    hombres_pct = (hombres / poblacion_total) * 100,
    mujeres_pct = (mujeres / poblacion_total) * 100,
    relacion_hm = hombres / mujeres,
    
    pea_total = sum(pea_cantidad, na.rm = TRUE),
    pob_10mas = sum(pob_10mas, na.rm = TRUE),
    pea_pct = (pea_total / pob_10mas) * 100,
    
    ocupados_total = sum(ocupados_cantidad, na.rm = TRUE),
    ocupados_pct = (ocupados_total / pea_total) * 100,
    
    pob_15mas = sum(pob_15mas, na.rm = TRUE),
    sin_escolaridad_total = sum(sin_escolaridad_cant, na.rm = TRUE),
    esc_basica_total = sum(esc_basica_cant, na.rm = TRUE),
    esc_media_superior_total = sum(esc_media_superior_cant, na.rm = TRUE),
    esc_superior_total = sum(esc_superior_cant, na.rm = TRUE),
    
    sin_escolaridad_pct = (sin_escolaridad_total / pob_15mas) * 100,
    esc_basica_pct = (esc_basica_total / pob_15mas) * 100,
    esc_media_superior_pct = (esc_media_superior_total / pob_15mas) * 100,
    esc_superior_pct = (esc_superior_total / pob_15mas) * 100,
    
    pob_15_24 = sum(pob_15_24, na.rm = TRUE),
    pob_25mas = sum(pob_25mas, na.rm = TRUE),
    alfb_15_24_total = sum(alfb_15_24_cant, na.rm = TRUE),
    alfb_25mas_total = sum(alfb_25mas_cant, na.rm = TRUE),
    
    alfb_15_24_pct = (alfb_15_24_total / pob_15_24) * 100,
    alfb_25mas_pct = (alfb_25mas_total / pob_25mas) * 100,
    .groups = "drop"
  )
}
# -------------------- #
#     Marginación  
# -------------------- #
{
# Data de marginación
marginacion <- read_csv("Data Raw/Marginación/Base_Indice_de_marginacion_municipal_90-15.csv",
                        locale = locale(encoding = "Latin1"))

# chequeo la clase de las variables
sapply(marginacion, class)

tail(marginacion, n=10)

marginacion <- marginacion %>% 
  janitor::clean_names() %>% 
  filter(cve_ent!="-") %>% 
  mutate(
    inegi = str_pad(cve_mun, width = 5, pad = "0"),
    cve_ent = str_pad(cve_ent, width = 2, pad = "0"),
    # Solo reemplaza "-" por NA en columnas de texto
    across(where(is.character), ~na_if(., "-"))
  ) %>% 
  select(inegi, everything()) %>% 
  arrange(inegi)
  
marginacion <- marginacion %>% 
  mutate(across(
    where(is.character) & !c(ent, inegi, mun, cve_ent, cve_mun, gm),
    as.numeric
  ))

#### Diagnostico de municipios ####

marginacion <- marginacion %>% 
  # Limpio los nombres de los municipios para poder unir
  mutate(
    nom_mun_clean = stri_trans_general(mun, "Latin-ASCII"),
    nom_mun_clean = toupper(nom_mun_clean)
  ) %>% 
  rename(state_code_mar = cve_ent)

# Hay muncipios que les pusieron distinto nombre pero mismo inegi en distintos años, por ejemplo: HUILOAPAN 
# Para evitar estos problemas me quedo con el año más reciente (2015) para hacer la corrección y despues la aplico a nivel de inegi
marginacion15 <- marginacion %>% 
  filter(ano==2015)
# Cambio el nombre de 2 municipios que en 2015 solamente están cortados
marginacion15 <- marginacion15 %>% 
  mutate(
    nom_mun_clean = case_when(
      cve_mun == 20208 & ano ==2015 ~ "SAN JUAN MIXTEPEC -DTO. 08 -",
      cve_mun == 20209 & ano ==2015 ~ "SAN JUAN MIXTEPEC -DTO. 26 -",
      cve_mun == 20318 & ano ==2015 ~ "SAN PEDRO MIXTEPEC -DTO. 22 -",
      cve_mun == 20319 & ano ==2015 ~ "SAN PEDRO MIXTEPEC -DTO. 26 -",
      TRUE ~ nom_mun_clean
    )
  )

# chequeo si hay municipios de marginación que no estén en remesas
anti_join(marginacion15, remesas_yr %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
          by = c("nom_mun_clean", "state_code_mar" = "state_code_remesas")) %>% 
  View() # esto puede ser porque no hay data de remesas para esos municipios o los nombres no coinciden exactamente
# chequeo si hay municipios de remesas que no están en marginación
anti_join(remesas_yr %>% select(nom_mun_clean, state_code_remesas) %>% distinct(), marginacion15,
          by = c("nom_mun_clean", "state_code_remesas" = "state_code_mar")) %>% 
  View() # son nombres cortados y no identificados

# Municipios únicos en la data de covariables
municipios_mar <- marginacion15 %>% 
  select(inegi, nom_mun_clean, state_code_mar) %>% 
  distinct() %>% 
  arrange(inegi)

# Merge por INEGI
diag_merge_mar <- municipios_censo %>%
  full_join(municipios_mar, by = "inegi") %>%
  mutate(
    # match exacto
    exact_match = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      admin_name_clean == nom_mun_clean,
    
    # uno contiene al otro
    name_contained = !is.na(admin_name_clean) &
      !is.na(nom_mun_clean) &
      (
        str_detect(admin_name_clean, fixed(nom_mun_clean)) |
          str_detect(nom_mun_clean, fixed(admin_name_clean))
      ),
    
    # el nombre del censo parece agregar varios municipios
    census_aggregation = !is.na(admin_name_clean) &
      str_detect(admin_name_clean, ","),
    
    # reemplazo a mano el municipio que tiene una coma en el nombre eliminando la coma
    census_aggregation = ifelse(admin_name_clean == "VILLA TEZOATLAN DE SEGURA Y LUNA, CUNA DE LA INDEPENDENCIA DE OAXACA, HEROICA",
                                FALSE, census_aggregation),
    
    # clasificación final (toma el primer TRUE)
    merge_status = case_when(
      is.na(admin_name_clean) ~ "only_mar",
      is.na(nom_mun_clean) ~ "only_censo",
      exact_match ~ "exact",
      census_aggregation & name_contained ~ "part of aggregation",
      name_contained ~ "close",
      TRUE ~ "mismatch"
    )
  )

table(diag_merge_mar$merge_status)
  
# Municipios a corregir
a_corregir_mar <- diag_merge_mar %>% 
  filter(merge_status == "mismatch" | merge_status == "only_mar") %>%
  select(inegi, nom_mun_clean, state_code_mar)

# Cuantos de los a corregir no están ya en los corregidos de las remesas
a_corregir_mar %>% 
  anti_join(corregidos %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
            by = c("nom_mun_clean", "state_code_mar" = "state_code_remesas")) %>% 
  View() 

# Hay municipios en corregidos que ahora no haya que corregir?
corregidos %>% 
  anti_join(a_corregir_mar %>% select(nom_mun_clean, state_code_mar) %>% distinct(),
            by = c("nom_mun_clean", "state_code_remesas" = "state_code_mar")) %>% 
  View()
# Los que no coinciden son los que tienen nombre truncado + los dos que no tienen inegi + los close (no estan en a corregir)

# Cruzo los a-corregir que faltan con el crosswalk de municipios de covariables
faltan_corregir_mar <- a_corregir_mar %>% 
  anti_join(corregidos %>% select(nom_mun_clean, state_code_remesas) %>% distinct(),
            by = c("nom_mun_clean", "state_code_mar" = "state_code_remesas")) %>% 
  select(-inegi)

faltan_corregir_mar %>% count(nom_mun_clean, state_code_mar) %>% filter(n > 1) # no hay repeticiones

croswalk_mar <- crossing(faltan_corregir_mar, municipios_censo) %>% 
  mutate(
    admin_name_nocoma = admin_name_clean %>%
      str_replace_all(",", " ") %>% # reemplaza comas por espacios
      str_squish(), # elimina espacios dobles
    admin_pad = paste0(" ", admin_name_nocoma, " "),
    nom_pad   = paste0(" ", nom_mun_clean, " ")
  )

correct_mar <- croswalk_mar %>% 
  filter(str_detect(admin_pad, fixed(nom_pad)) & state_code_censo == state_code_mar) %>%
  select(inegi, nom_mun_clean, state_code_mar)

# chequeo que no haya duplicados en los municipios
correct_mar %>% 
  count(state_code_mar, nom_mun_clean) %>%
  filter(n>1) # no hay

# Se corrigieron 66, veo cuales faltan
anti_join(faltan_corregir_mar, correct_mar, by = c("nom_mun_clean", "state_code_mar")) %>%
  View() # son 6 que tienen nombre distinto escrito al censo y a la data de remesas, los corrijo a mano

correct_mar <- correct_mar %>% 
  rbind(anti_join(faltan_corregir_mar, correct_mar, by = c("nom_mun_clean", "state_code_mar")) %>% 
          mutate(inegi = case_when(
            nom_mun_clean == "SAN JUAN MIXTEPEC -DTO. 08 -" ~ "20207",
            nom_mun_clean == "SAN JUAN MIXTEPEC -DTO. 26 -" ~ "20208",
            nom_mun_clean == "SAN PEDRO MIXTEPEC -DTO. 22 -" ~ "20317",
            nom_mun_clean == "SAN PEDRO MIXTEPEC -DTO. 26 -" ~ "20318",
            nom_mun_clean == "MEDELLIN DE BRAVO" ~ "30103",
            nom_mun_clean == "HEROICA VILLA TEZOATLAN DE SEGURA Y LUNA, CUNA DE LA INDEPENDENCIA DE OAXACA" ~ "20545",
            TRUE ~ NA_character_
          )
          )
  )

correct_mar_totales <- rbind(
  correct_mar, corregidos %>% rename(state_code_mar = state_code_remesas)
)
correct_mar_totales %>% count(state_code_mar, nom_mun_clean) %>%
  filter(n>1) 
length(unique(correct_mar_totales$inegi)) # 960

a_corregir_mig %>% count(state_code_mig, nom_mun_clean) %>%
  filter(n>1) 

# Hay correcciones de mas?
anti_join(correct_mar_totales %>% select(nom_mun_clean, state_code_mar) %>% distinct(),
          a_corregir_mar %>% select(nom_mun_clean, state_code_mar) %>% distinct(), 
          by = c("nom_mun_clean", "state_code_mar")) %>% 
  View()
# son los que tenían nombre cortado en remesas y estaban en "corregidos" + los close
# los de nombre cortado no son un problema porque no se van a mergear por el nombre con la data de covariables en el proximo paso
# los close se unen bien después con el nombre y se corrigen

#### Uno los iengi corregidos con la data de marginación  #### 

marginacion15 <- marginacion15 %>% 
  # agrego los municipios corregidos
  left_join(correct_mar_totales %>% rename(inegi_corregido = inegi), 
            by = c("nom_mun_clean", "state_code_mar"))

# Uno con el panel completo de marginación
marginacion_c <- marginacion %>% 
  left_join(marginacion15 %>% select(inegi, inegi_corregido),
            by = c("inegi"))

# Unifico los inegi
marginacion_c <- marginacion_c %>%
  mutate(
    inegi_final = if_else(!is.na(inegi_corregido), inegi_corregido, inegi)
  ) %>% 
  select(-inegi, -inegi_corregido) %>% 
  rename(inegi = inegi_final) %>% 
  arrange(inegi) %>% 
  select(inegi, nom_mun_clean, state_code_mar, everything())

any(is.na(marginacion_c$inegi)) # no hay NA
any(is.na(marginacion_c$inegi)) # no hay NA

# Agrego las variables
marginacion_agg <- marginacion_c %>% 
  group_by(inegi, ano) %>%
  summarise(
    poblacion_agg = sum(pob_tot, na.rm = TRUE),
    im_agg = weighted.mean(im, w = pob_tot, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  rename(
    poblacion = poblacion_agg,
    im = im_agg
  )

# Paso a formato wide
marginacion_wide <- marginacion_agg %>% 
  pivot_wider(names_from = ano, values_from = c(poblacion, im), names_sep = "_")

}
# -------------------- #
#     Data completa 
# -------------------- #
{
#### 1. Creo la data para la estimación con stock de españoles #### 

# chequeo si hay inegi-years repetidos en la data de remesas
remesas_yr %>% 
  filter(!str_ends(inegi, "999")) %>% 
  count(inegi, year) %>% 
  filter(n > 1) # hay algunos municipios que se repiten en el tiempo porque hay municipios agrupados bajo el mismo inegi harmonizado, los voy a sumar al nivel de inegi-year
remesas_yr %>% arrange(inegi, year) %>% View()
  
# Data de remesas anual con exposición histórica
estimacion_yr <- remesas_yr %>% 
  # Elimino las obs con inegi que termina en 999
  filter(!str_ends(inegi, "999")) %>%
  # Sumo las remesas al nivel inegi-year (porque hay municipios agrupados bajo el mismo inegi harmonizado)
  group_by(inegi, year) %>%
  summarise(total_remesas = sum(total_remesas, na.rm = TRUE), .groups = "drop") %>%
  # Uno con la data de españoles
  right_join(spanish_historical %>% 
               st_drop_geometry() %>%
               as.data.frame() %>% 
               select(-solo_df1, -solo_df2),
            by = "inegi"
  ) %>%
  # agrego la migración a eeuu en 2000
  left_join(mig_eeuu_2000_c, by = "inegi") %>%
  # agrego la migración a eeuu en 2010
  left_join(mig_eeuu_2010_c, by = "inegi") %>%
  # agrego los controles de población
  left_join(cov_censo2020_agg, by = "inegi") %>%
  rename(poblacion_2020 = poblacion_total) %>% 
  # agrego la data de marginación
  left_join(marginacion_wide, by = "inegi") %>%
  # creo post y el log de las remesas
  mutate(
    post22 = if_else(year > 2022, 1, 0), # la ley es de octubre del 2022
    post21 = if_else(year > 2021, 1, 0),
    log_remesas = log1p(total_remesas)
  ) %>% 
  arrange(inegi, year) %>% 
  janitor::clean_names() %>% 
  select(inegi, year, admin_name, admin_name_clean, entidad_federativa, state_code_cov, 
         total_remesas, post22, post21, spanish_born, share_spanish_born, log_spanish_born_pooled, 
         log_spanish_born_avg, unweighted_n, unweighted_spanish, n_spanish_born, n_total,
         everything()) %>% 
  rename(state_code = state_code_cov)

#chequeos
names(estimacion_yr)
any(is.na(estimacion_yr$tot_viv_00)) # sí hay na
any(is.na(estimacion_yr$tot_viv_10)) # sí hay na
estimacion_yr %>% filter(is.na(tot_viv_00) | is.na(tot_viv_10)) %>% View()   # son inegi que terminan con 999, vienen de la data de españoles 
estimacion_yr %>% filter(is.na(viv_emig_00) | is.na(viv_emig_10)) %>% View() # son inegi que terminan con 999, vienen de la data de españoles 

estimacion_yr %>%
  filter(
    !is.na(viv_emig_10),
    !is.na(viv_emig_00),
    !is.na(log_spanish_born_avg),
    !is.na(log_remesas)
  ) %>%
  summarise(n_inegi = n_distinct(inegi)) # 2322 

# Data de remesas trimestral con exposición histórica. FIX ME: PARA PODER HACER ESTO TENGO QUE CORREGIR LOS INEGI DE LAS REMESAS TRIMESTRALES
{ 
 # chequeo si hay inegi-years repetidos en la data de remesas
remesas_tri %>% 
  filter(!str_ends(inegi, "999")) %>% 
  count(inegi, tq) %>% 
  filter(n > 1) 

estimacion_tri <- remesas_tri %>% 
  # Elimino las obs con inegi que termina en 999
  filter(!str_ends(inegi, "999")) %>%
  # Sumo las remesas al nivel inegi-year (porque hay municipios agrupados bajo el mismo inegi harmonizado)
  group_by(inegi, year) %>%
  summarise(total_remesas = sum(total_remesas, na.rm = TRUE), .groups = "drop") %>%
  
  
  left_join(spanish_historical %>% 
              as.data.frame() %>% 
              select(inegi, share_spanish_born, log_spanish_born_pooled, log_spanish_born_avg, spanish_born),
            by = "inegi"
  ) %>% 
  # agrego la migración a eeuu en 2000
  left_join(mig_eeuu_2000 %>% 
              select(-cve_ent, -nom_mun),
            by = "inegi"
  ) %>%
  # agrego la migración a eeuu en 2010
  left_join(mig_eeuu_2010,
            by = "inegi"
  ) %>%
  mutate(
    post = if_else(year > 2022, 1, 0), # la ley es de octubre del 2022
    log_remesas = log1p(total_remesas)
  ) %>% 
  arrange(inegi, year) %>% 
  janitor::clean_names() %>% 
  select(inegi, year, admin_name, total_remesas, post22, post21, 
         spanish_born, share_spanish_born, log_spanish_born_pooled, log_spanish_born_avg,
         unweighted_n, unweighted_spanish, n_spanish_born, n_total,
         everything())

# Creo grupos de quintiles para la migracion a eeuu
estimacion_tri <- estimacion_tri %>%
  mutate(
    viv_emig_10_q5 = ntile(viv_emig_10, 5),
    viv_emig_00_q5 = ntile(viv_emig_00, 5),
    viv_emig_10_q10 = ntile(viv_emig_10, 10),
    viv_emig_00_q10 = ntile(viv_emig_00, 10)
  )

write_csv(estimacion_tri, "Data Out/estimacion_remesas_tri.csv")
}

write_csv(estimacion_yr, "Data Out/estimacion_remesas_yr2.csv")
write_dta(estimacion_yr, "Data Out/estimacion_remesas_yr2.dta")
  # la versión 2 agrega las covariables de población y de marginación

#### 2. Creo la data para la estimación con cohortes de llegada #### 

length(unique(spanish_cohorts_mx$inegi)) # 2325
length(unique(remesas_yr$inegi)) # 2356
setdiff(spanish_cohorts_mx$inegi, remesas_yr$inegi)
setdiff(remesas_yr$inegi, spanish_cohorts_mx$inegi) # son todos no identificados y NA

# Data de remesas anual con exposición histórica
estimacion_yr_coh <- remesas_yr %>% 
  # Elimino las obs con inegi que termina en 999
  filter(!str_ends(inegi, "999")) %>%
  # Sumo las remesas al nivel inegi-year (porque hay municipios agrupados bajo el mismo inegi harmonizado)
  group_by(inegi, year) %>%
  summarise(total_remesas = sum(total_remesas, na.rm = TRUE), .groups = "drop") %>%
  # Uno con la data de españoles
  right_join(spanish_cohorts_mx, by = "inegi") %>%
  # Agrego la migración a eeuu en 2000
  left_join(mig_eeuu_2000_c, by = "inegi") %>%
  # Agrego la migración a eeuu en 2010
  left_join(mig_eeuu_2010_c, by = "inegi") %>%
  # Agrego los controles de población
  left_join(cov_censo2020_agg, by = "inegi") %>%
  rename(poblacion_2020 = poblacion_total) %>% 
  # agrego la data de marginación
  left_join(marginacion_wide, by = "inegi") %>%
  # Creo post y el lo de las remesas
  mutate(
    post22 = if_else(year > 2022, 1, 0), # la ley es de octubre del 2022
    post21 = if_else(year > 2021, 1, 0),
    log_remesas = log1p(total_remesas)
  ) %>% 
  arrange(inegi, year) %>% 
  select(inegi, year, admin_name, spanish_presence_1936_1955, spanish_presence_1956_1978, 
         total_remesas, post22, post21, everything())

length(unique(estimacion_yr_coh$inegi)) # 2325

# Chequeo que los municipios tratados se hayan unido bien
municipios_tratados_df <- spanish_cohorts_mx %>%
  st_drop_geometry() %>%
  filter(spanish_presence_1956_1978==1 | spanish_presence_1936_1955 ==1) %>%
  select(inegi, admin_name) %>% 
  left_join(remesas_yr %>% 
              select(inegi, nom_mun_clean) %>% distinct(), by = c("inegi")) # bien

estimacion_yr_coh_dta <- st_drop_geometry(estimacion_yr_coh) %>% 
  select(-geometry)
  
# Exporto la data
write_csv(estimacion_yr_coh, "Data Out/estimacion_remesas_yr_coh2.csv")
write_dta(estimacion_yr_coh_dta, "Data Out/estimacion_remesas_yr_coh2.dta")
  # la versión 2 agrega las covariables de población y de marginación
}
# 1. LISTO. agregar las nievas covariables a estimacion_yr_coh
# 2. LISTO. agregar a estimacion_yr las nuevas convariables par el entropy balancing. En esta data la variable de tratamiento principal es la continua
# 3. opcional: corregir los inegi de la data de remesas trimestral para poder correr estimacion_tri
# 4. Hacer entropy balancing en opcion discreta (estimacion_yr_coh)
# 5. Hacer entropy balancing en opcion continua (estimacion_yr)
# 6. un Sinth did puede ser mejor

