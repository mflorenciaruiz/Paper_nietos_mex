############################################################
# PPML / Poisson Event-Study en R
# Equivalente a ppmlhdfe en Stata
############################################################

# Paquetes
library(haven)
library(dplyr)
library(fixest)
library(ggplot2)
library(broom)
library(stringr)
library(readr)

############################################################
# 0. Rutas
############################################################

main <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_mex-main"

data_path   <- file.path(main, "Data Out")
output_path <- file.path(main, "Output")
graphs_path <- file.path(output_path, "Estimaciones")
tables_path <- file.path(output_path, "Estimaciones")

dir.create(output_path, showWarnings = FALSE)
dir.create(graphs_path, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_path, showWarnings = FALSE, recursive = TRUE)

############################################################
# 1. Cargar las dos bases
############################################################

# Base para exposición continua: log Spanish-born average
estimacion_yr <- read_csv(file.path(data_path, "estimacion_remesas_yr2.csv"))

# Base para especificaciones de cohortes / presencia
estimacion_yr_coh <- read_csv(file.path(data_path, "estimacion_remesas_yr_coh2.csv"))

############################################################
# 2. Funciones auxiliares
############################################################

clean_ppml_data <- function(data,
                            outcome = "total_remesas",
                            id = "inegi",
                            time = "year",
                            drop_p99 = TRUE) {
  
  data_clean <- data %>%
    mutate(
      year = as.numeric(year),
      post21 = if_else(year >= 2022, 1, 0)
    ) %>%
    filter(
      !is.na(.data[[id]]),
      !is.na(.data[[time]]),
      !is.na(.data[[outcome]]),
      .data[[outcome]] >= 0
    )
  
  if (drop_p99) {
    p99 <- quantile(data_clean[[outcome]], 0.99, na.rm = TRUE)
    
    data_clean <- data_clean %>%
      filter(.data[[outcome]] <= p99)
    
    cat("P99 de", outcome, "=", p99, "\n")
    cat("Observaciones luego de eliminar top 1%:", nrow(data_clean), "\n\n")
  }
  
  return(data_clean)
}


make_event_plot <- function(model,
                            title,
                            subtitle,
                            output_name,
                            ref_year = 2021) {
  
  event_df <- tidy(model, conf.int = TRUE) %>%
    filter(str_detect(term, "year::")) %>%
    mutate(
      year = as.numeric(str_extract(term, "\\d{4}")),
      beta = estimate,
      se = std.error,
      pct = 100 * (exp(beta) - 1),
      ci_low = 100 * (exp(conf.low) - 1),
      ci_high = 100 * (exp(conf.high) - 1)
    ) %>%
    select(year, beta, se, pct, ci_low, ci_high)
  
  event_df <- bind_rows(
    event_df,
    tibble(
      year = ref_year,
      beta = 0,
      se = 0,
      pct = 0,
      ci_low = 0,
      ci_high = 0
    )
  ) %>%
    arrange(year)
  
  write_csv(
    event_df,
    file.path(tables_path, paste0(output_name, ".csv"))
  )
  
  g <- ggplot(event_df, aes(x = year, y = pct)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = ref_year, linetype = "dashed") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
    geom_point(size = 2) +
    geom_line() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year",
      y = "Percent effect on expected remittances"
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(graphs_path, paste0(output_name, ".png")),
    plot = g,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  return(list(results = event_df, plot = g))
}

############################################################
# 3. Limpiar cada base por separado
############################################################

drop_outliers_p99 <- TRUE

df_main <- clean_ppml_data(
  data = estimacion_yr,
  outcome = "total_remesas",
  drop_p99 = drop_outliers_p99
)

df_coh <- clean_ppml_data(
  data = estimacion_yr_coh,
  outcome = "total_remesas",
  drop_p99 = drop_outliers_p99
)


############################################################
# 4. GRAFICO 1:
#    Base: estimacion_yr
#    Exposición continua: log_spanish_born_avg
############################################################

df_cont <- df_main %>%
  filter(!is.na(log_spanish_born_avg))

m_ppml_log_spanish <- fepois(
  total_remesas ~ i(year, log_spanish_born_avg, ref = 2021) |
    inegi + year,
  data = df_cont,
  cluster = ~ inegi
)

summary(m_ppml_log_spanish)

plot_log_spanish <- make_event_plot(
  model = m_ppml_log_spanish,
  title = "PPML event-study: historical Spanish exposure",
  subtitle = "Base: estimacion_remesas_yr2; outcome in levels; reference year: 2021",
  output_name = "ppml_event_log_spanish",
  ref_year = 2021
)

############################################################
# 5. GRAFICO 1B:
#    Misma especificación, con tendencias migratorias
#    Equivalente a viv_emig_10 × year
############################################################

if ("viv_emig_10" %in% names(df_cont)) {
  
  df_cont_mig <- df_cont %>%
    filter(!is.na(viv_emig_10))
  
  m_ppml_log_spanish_mig <- fepois(
    total_remesas ~ i(year, log_spanish_born_avg, ref = 2021) |
      inegi + year + year[viv_emig_10],
    data = df_cont_mig,
    cluster = ~ inegi
  )
  
  summary(m_ppml_log_spanish_mig)
  
  plot_log_spanish_mig <- make_event_plot(
    model = m_ppml_log_spanish_mig,
    title = "PPML event-study: historical Spanish exposure",
    subtitle = "With U.S. migration × year trends; reference year: 2021",
    output_name = "ppml_event_log_spanish_migtrends",
    ref_year = 2021
  )
}

############################################################
# 6. GRAFICO 2:
#    Base: estimacion_yr_coh
#    Exposición no continua: spanish_presence_1956_1978
############################################################

df_presence_1956 <- df_coh %>%
  filter(!is.na(spanish_presence_1956_1978)) %>%
  mutate(
    spanish_presence_1956_1978 = as.numeric(spanish_presence_1956_1978)
  )

m_ppml_presence_1956 <- fepois(
  total_remesas ~ i(year, spanish_presence_1956_1978, ref = 2021) |
    inegi + year,
  data = df_presence_1956,
  cluster = ~ inegi
)

summary(m_ppml_presence_1956)

plot_presence_1956 <- make_event_plot(
  model = m_ppml_presence_1956,
  title = "PPML event-study: Spanish presence 1956–1978",
  subtitle = "Base: estimacion_remesas_yr_coh2; treated relative to controls; reference year: 2021",
  output_name = "ppml_event_presence_1956_1978",
  ref_year = 2021
)

############################################################
# 7. GRAFICO 2B:
#    Presencia 1956-1978 con tendencias migratorias
############################################################

if ("viv_emig_10" %in% names(df_presence_1956)) {
  
  df_presence_1956_mig <- df_presence_1956 %>%
    filter(!is.na(viv_emig_10))
  
  m_ppml_presence_1956_mig <- fepois(
    total_remesas ~ i(year, spanish_presence_1956_1978, ref = 2021) |
      inegi + year + year[viv_emig_10],
    data = df_presence_1956_mig,
    cluster = ~ inegi
  )
  
  summary(m_ppml_presence_1956_mig)
  
  plot_presence_1956_mig <- make_event_plot(
    model = m_ppml_presence_1956_mig,
    title = "PPML event-study: Spanish presence 1956–1978",
    subtitle = "With U.S. migration × year trends; reference year: 2021",
    output_name = "ppml_event_presence_1956_1978_migtrends",
    ref_year = 2021
  )
}

############################################################
# 8. Alternativa: spanish_presence_1936_1955
#    Misma base: estimacion_yr_coh
############################################################

if ("spanish_presence_1936_1955" %in% names(df_coh)) {
  
  df_presence_1936 <- df_coh %>%
    filter(!is.na(spanish_presence_1936_1955)) %>%
    mutate(
      spanish_presence_1936_1955 = as.numeric(spanish_presence_1936_1955)
    )
  
  m_ppml_presence_1936 <- fepois(
    total_remesas ~ i(year, spanish_presence_1936_1955, ref = 2021) |
      inegi + year,
    data = df_presence_1936,
    cluster = ~ inegi
  )
  
  summary(m_ppml_presence_1936)
  
  plot_presence_1936 <- make_event_plot(
    model = m_ppml_presence_1936,
    title = "PPML event-study: Spanish presence 1936–1955",
    subtitle = "Base: estimacion_remesas_yr_coh2; treated relative to controls; reference year: 2021",
    output_name = "ppml_event_presence_1936_1955",
    ref_year = 2021
  )
}

############################################################
# 9. Modelos agregados post
############################################################

m_post_log_spanish <- fepois(
  total_remesas ~ post21:log_spanish_born_avg |
    inegi + year,
  data = df_cont,
  cluster = ~ inegi
)

summary(m_post_log_spanish)

b_log <- coef(m_post_log_spanish)["post21:log_spanish_born_avg"]
cat(
  "Post × log Spanish-born average, percent effect:",
  100 * (exp(b_log) - 1),
  "\n"
)


m_post_presence_1956 <- fepois(
  total_remesas ~ post21:spanish_presence_1956_1978 |
    inegi + year,
  data = df_presence_1956,
  cluster = ~ inegi
)

summary(m_post_presence_1956)

b_presence_1956 <- coef(m_post_presence_1956)["post21:spanish_presence_1956_1978"]
cat(
  "Post × Spanish presence 1956-1978, percent effect:",
  100 * (exp(b_presence_1956) - 1),
  "\n"
)

############################################################
# 10. Nota final
############################################################

cat("\n")
cat("Graficos generados en:", graphs_path, "\n")
cat("Tablas generadas en:", tables_path, "\n")
cat("Interpretacion: los puntos son 100*(exp(beta)-1), es decir,\n")
cat("efectos porcentuales sobre remesas esperadas.\n")

tabla_deciles_remesas <- estimacion_yr %>%
  filter(!is.na(total_remesas)) %>%
  mutate(
    decil = ntile(total_remesas, 10)
  ) %>%
  group_by(decil) %>%
  summarise(
    min = min(total_remesas, na.rm = TRUE),
    max = max(total_remesas, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    decil = paste0("Decile ", decil),
    min = round(min, 3),
    max = round(max, 3)
  )

tabla_deciles_remesas
