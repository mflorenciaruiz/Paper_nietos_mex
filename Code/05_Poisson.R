############################################################
# 05_Poisson_adjusted.R
# PPML equivalente a las especificaciones OLS principales
############################################################

library(haven)
library(dplyr)
library(fixest)
library(ggplot2)
library(broom)
library(stringr)
library(readr)
library(tibble)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(gt)

############################################################
# 0. Rutas
############################################################

#main <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_mex"
main <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/México/Paper_nietos_mex"

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
estimacion_yr <- read_csv(file.path(data_path, "estimacion_remesas_yr3.csv"))

# Base para especificaciones de cohortes / presencia
estimacion_yr_coh <- read_csv(file.path(data_path, "estimacion_remesas_yr_coh3.csv"))

############################################################
# 2. Opciones
############################################################

# Para comparar con OLS, usar "none".
# Alternativas para robustez: "drop_p99" o "winsor_p99".
outlier_method <- "none"

# Si querés replicar tu especificación preferida event10, poner TRUE.
# event10 en tu script OLS usa year >= 2016 y viv_emig_10 x year.
use_2016_sample <- FALSE

ref_year <- 2021

############################################################
# 3. Funciones auxiliares
############################################################

clean_ppml_data <- function(data,
                            outcome = "total_remesas",
                            id = "inegi",
                            time = "year",
                            outlier_method = c("none", "drop_p99", "winsor_p99")) {
  
  outlier_method <- match.arg(outlier_method)
  
  data_clean <- data %>%
    mutate(
      year = as.numeric(year),
      post21 = if_else(year > 2021, 1, 0)
    ) %>%
    filter(
      !is.na(.data[[id]]),
      !is.na(.data[[time]]),
      !is.na(.data[[outcome]]),
      .data[[outcome]] >= 0
    )
  
  p99 <- quantile(data_clean[[outcome]], 0.99, na.rm = TRUE)
  
  if (outlier_method == "none") {
    
    data_clean <- data_clean %>%
      mutate(total_remesas_ppml = .data[[outcome]])
    
    cat("Outlier method: none\n")
    
  } else if (outlier_method == "drop_p99") {
    
    n_drop <- sum(data_clean[[outcome]] > p99, na.rm = TRUE)
    
    data_clean <- data_clean %>%
      filter(.data[[outcome]] <= p99) %>%
      mutate(total_remesas_ppml = .data[[outcome]])
    
    cat("Outlier method: drop p99\n")
    cat("P99 =", p99, "\n")
    cat("Observaciones eliminadas:", n_drop, "\n")
    
  } else if (outlier_method == "winsor_p99") {
    
    n_win <- sum(data_clean[[outcome]] > p99, na.rm = TRUE)
    
    data_clean <- data_clean %>%
      mutate(
        total_remesas_ppml = if_else(
          .data[[outcome]] > p99,
          as.numeric(p99),
          .data[[outcome]]
        )
      )
    
    cat("Outlier method: winsor p99\n")
    cat("P99 =", p99, "\n")
    cat("Observaciones winsorizadas:", n_win, "\n")
  }
  
  cat("Observaciones finales:", nrow(data_clean), "\n\n")
  
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
  
  g <- ggplot(event_df %>%
                mutate(year_lab = as.character(as.integer(year))), aes(x = year, y = pct)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = ref_year, linetype = "dashed") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, color = "grey65") +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(2013, 2025, by = 1)) +
    #geom_line() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year",
      y = "Percent effect on expected remittances"
    ) +
    theme_minimal(base_family = "Times New Roman", base_size = 12)+
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11)
    )
  
  ggsave(
    filename = file.path(graphs_path, paste0(output_name, ".png")),
    plot = g,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  return(list(results = event_df, plot = g))
}


sample_check <- function(raw_data, clean_data, model, name) {
  cat("\n============================\n")
  cat("Sample check:", name, "\n")
  cat("============================\n")
  cat("Raw rows:", nrow(raw_data), "\n")
  cat("Rows after cleaning:", nrow(clean_data), "\n")
  cat("Model nobs:", nobs(model), "\n")
  cat("Dropped in cleaning:", nrow(raw_data) - nrow(clean_data), "\n")
  cat("Dropped by model:", nrow(clean_data) - nobs(model), "\n")
  cat("============================\n\n")
}

############################################################
# 4. Limpiar bases
############################################################

df_main <- clean_ppml_data(
  data = estimacion_yr,
  #outcome = "remesas_pc",
  outcome = "total_remesas",
  outlier_method = outlier_method
) # FIX ME: no hace falta, pero faltaría limpiar las obs con NA en el treatment para que este completa la limpieza

df_coh <- clean_ppml_data(
  data = estimacion_yr_coh,
  outcome = "remesas_pc",
  #outcome = "total_remesas",
  outlier_method = outlier_method
)

if (use_2016_sample) {
  df_main <- df_main %>% filter(year >= 2016)
  df_coh  <- df_coh  %>% filter(year >= 2016)
}

suffix <- paste0(
  outlier_method,
  ifelse(use_2016_sample, "_from2016", "_allyears")
)

############################################################
# 5.1. CONTINUA: equivalente PPML de OLS con log_spanish_born_avg
############################################################

df_cont <- df_main %>%
  filter(!is.na(log_spanish_born_avg))

# OLS comparable en misma muestra, para chequear contra tu modelo original
m_ols_log_spanish <- feols(
  log_remesas ~ i(year, log_spanish_born_avg, ref = ref_year) |
    inegi + year,
  data = df_cont,
  cluster = ~ inegi
)

# PPML equivalente
m_ppml_log_spanish <- fepois(
  total_remesas_ppml ~ i(year, log_spanish_born_avg, ref = ref_year) |
    inegi + year,
  data = df_cont,
  cluster = ~ inegi
)

summary(m_ols_log_spanish)
summary(m_ppml_log_spanish)

sample_check(
  raw_data = estimacion_yr,
  clean_data = df_cont,
  model = m_ppml_log_spanish,
  name = "PPML continuous exposure"
)

plot_log_spanish <- make_event_plot(
  model = m_ppml_log_spanish,
  title = "PPML event-study: historical Spanish exposure",
  subtitle = paste0(
    "Equivalent to OLS event-study; outcome in levels; ref. year: ",
    ref_year,
    "; sample: ",
    ifelse(use_2016_sample, "2016–2024", "all years")
    #, "; outliers: ",
    #outlier_method
  ),
  output_name = paste0("ppml_event_log_spanish_", suffix),
  ref_year = ref_year
)

############################################################
# 5.2. CONTINUA + tendencias migratorias
# Equivalente a OLS preferido, pero corrigiendo sintaxis:
# year[viv_emig_10] = year x viv_emig_10
############################################################

if ("viv_emig_10" %in% names(df_cont)) {
  
  df_cont_mig <- df_cont %>%
    filter(!is.na(viv_emig_10))
  
  # OLS comparable
  m_ols_log_spanish_mig <- feols(
    log_remesas ~ i(year, log_spanish_born_avg, ref = ref_year) |
      inegi + year + year[viv_emig_10],
    data = df_cont_mig,
    cluster = ~ inegi
  )
  
  # PPML equivalente
  m_ppml_log_spanish_mig <- fepois(
    total_remesas_ppml ~ i(year, log_spanish_born_avg, ref = ref_year) |
      inegi + year + year[viv_emig_10],
    data = df_cont_mig,
    cluster = ~ inegi
  )
  
  summary(m_ols_log_spanish_mig)
  summary(m_ppml_log_spanish_mig)
  
  sample_check(
    raw_data = estimacion_yr,
    clean_data = df_cont_mig,
    model = m_ppml_log_spanish_mig,
    name = "PPML continuous exposure with migration trends"
  )
  
  plot_log_spanish_mig <- make_event_plot(
    model = m_ppml_log_spanish_mig,
    title = "PPML event-study: historical Spanish exposure",
    subtitle = paste0(
      "With U.S. migration × year trends; ref. year: ",
      ref_year,
      "; sample: ",
      ifelse(use_2016_sample, "2016–2024", "all years"),
      "; outliers: ",
      outlier_method
    ),
    output_name = paste0("ppml_event_log_spanish_migtrends_", suffix),
    ref_year = ref_year
  )
}

############################################################
# 5.3. ATTs del modelo continuo (con y sin mig trends)
############################################################

m_post_log_spanish <- fepois(
  total_remesas_ppml ~ post21:log_spanish_born_avg |
    inegi + year,
  data = df_cont,
  cluster = ~ inegi
)

summary(m_post_log_spanish)

b_log <- coef(m_post_log_spanish)["post21:log_spanish_born_avg"]

cat(
  "PPML Post x log Spanish-born, percent effect:",
  100 * (exp(b_log) - 1),
  "\n"
)

if ("viv_emig_10" %in% names(df_cont)) {
  
  m_post_log_spanish_mig <- fepois(
    total_remesas_ppml ~ post21:log_spanish_born_avg |
      inegi + year + year[viv_emig_10],
    data = df_cont %>% filter(!is.na(viv_emig_10)),
    cluster = ~ inegi
  )
  
  summary(m_post_log_spanish_mig)
  
  b_log_mig <- coef(m_post_log_spanish_mig)["post21:log_spanish_born_avg"]
  
  cat(
    "PPML Post x log Spanish-born with migration trends, percent effect:",
    100 * (exp(b_log_mig) - 1),
    "\n"
  )
}

############################################################
# 6. COHORTES / presencia: equivalente PPML de OLS por cohortes
############################################################

df_presence <- df_coh %>%
  filter(
    !is.na(spanish_presence_1936_1955),
    !is.na(spanish_presence_1956_1978)
  ) %>%
  mutate(
    spanish_presence_1936_1955 = as.numeric(spanish_presence_1936_1955),
    spanish_presence_1956_1978 = as.numeric(spanish_presence_1956_1978)
  )

is.numeric(estimacion_yr_coh$spanish_presence_1936_1955)
is.numeric(estimacion_yr_coh$spanish_presence_1956_1978)
any(is.na((estimacion_yr_coh$spanish_presence_1936_1955)))
any(is.na((estimacion_yr_coh$spanish_presence_1956_1978)))

############################################################
# 6.1. Event-study: presencia 1936-1955
############################################################

m_ols_presence_1936 <- feols(
  log_remesas ~ i(year, spanish_presence_1936_1955, ref = ref_year) |
    inegi + year,
  data = df_presence,
  cluster = ~ inegi
)

m_ppml_presence_1936 <- fepois(
  total_remesas_ppml ~ i(year, spanish_presence_1936_1955, ref = ref_year) |
    inegi + year,
  data = df_presence,
  cluster = ~ inegi
)

summary(m_ols_presence_1936)
summary(m_ppml_presence_1936)

sample_check(
  raw_data = estimacion_yr_coh,
  clean_data = df_presence,
  model = m_ppml_presence_1936,
  name = "PPML Spanish presence 1936-1955"
)

plot_presence_1936 <- make_event_plot(
  model = m_ppml_presence_1936,
  title = "PPML event-study: Spanish presence 1936–1955",
  subtitle = paste0(
    "Treated relative to controls; ref. year: ",
    ref_year,
    "; sample: ",
    ifelse(use_2016_sample, "2016–2024", "all years")
    #,"; outliers: ",
    #outlier_method
  ),
  output_name = paste0("ppml_event_presence_1936_1955_", suffix),
  ref_year = ref_year
)

############################################################
# 6.2. Event-study: presencia 1956-1978
############################################################

m_ols_presence_1956 <- feols(
  log_remesas ~ i(year, spanish_presence_1956_1978, ref = ref_year) |
    inegi + year,
  data = df_presence,
  cluster = ~ inegi
)

m_ppml_presence_1956 <- fepois(
  total_remesas_ppml ~ i(year, spanish_presence_1956_1978, ref = ref_year) |
    inegi + year,
  data = df_presence,
  cluster = ~ inegi
)

summary(m_ols_presence_1956)
summary(m_ppml_presence_1956)

sample_check(
  raw_data = estimacion_yr_coh,
  clean_data = df_presence,
  model = m_ppml_presence_1956,
  name = "PPML Spanish presence 1956-1978"
)

plot_presence_1956 <- make_event_plot(
  model = m_ppml_presence_1956,
  title = "PPML event-study: Spanish presence 1956–1978",
  subtitle = paste0(
    "Treated relative to controls; ref. year: ",
    ref_year,
    "; sample: ",
    ifelse(use_2016_sample, "2016–2024", "all years")
    #, "; outliers: ",
    #outlier_method
  ),
  output_name = paste0("ppml_event_presence_1956_1978_", suffix),
  ref_year = ref_year
)

############################################################
# 6.3. Event-studies con tendencias migratorias
############################################################

if ("viv_emig_10" %in% names(df_presence)) {
  
  df_presence_mig <- df_presence %>%
    filter(!is.na(viv_emig_10))
  
  m_ppml_presence_1936_mig <- fepois(
    total_remesas_ppml ~ i(year, spanish_presence_1936_1955, ref = ref_year) |
      inegi + year + year[viv_emig_10],
    data = df_presence_mig,
    cluster = ~ inegi
  )
  
  m_ppml_presence_1956_mig <- fepois(
    total_remesas_ppml ~ i(year, spanish_presence_1956_1978, ref = ref_year) |
      inegi + year + year[viv_emig_10],
    data = df_presence_mig,
    cluster = ~ inegi
  )
  
  summary(m_ppml_presence_1936_mig)
  summary(m_ppml_presence_1956_mig)
  
  plot_presence_1936_mig <- make_event_plot(
    model = m_ppml_presence_1936_mig,
    title = "PPML event-study: Spanish presence 1936–1955",
    subtitle = paste0(
      "With U.S. migration × year trends; ref. year: ",
      ref_year,
      "; sample: ",
      ifelse(use_2016_sample, "2016–2024", "all years"),
      "; outliers: ",
      outlier_method
    ),
    output_name = paste0("ppml_event_presence_1936_1955_migtrends_", suffix),
    ref_year = ref_year
  )
  
  plot_presence_1956_mig <- make_event_plot(
    model = m_ppml_presence_1956_mig,
    title = "PPML event-study: Spanish presence 1956–1978",
    subtitle = paste0(
      "With U.S. migration × year trends; ref. year: ",
      ref_year,
      "; sample: ",
      ifelse(use_2016_sample, "2016–2024", "all years"),
      "; outliers: ",
      outlier_method
    ),
    output_name = paste0("ppml_event_presence_1956_1978_migtrends_", suffix),
    ref_year = ref_year
  )
}

############################################################
# 6.4. ATTs del modelo por cohortes
# Equivalente al OLS:
# log_remesas ~ spanish_presence_1936_1955:post21 +
#               spanish_presence_1956_1978:post21 | inegi + year
############################################################

m_post_coh_ppml <- fepois(
  total_remesas_ppml ~ post21:spanish_presence_1936_1955 +
    post21:spanish_presence_1956_1978 |
    inegi + year,
  data = df_presence,
  cluster = ~ inegi
)

summary(m_post_coh_ppml)

b_1936 <- coef(m_post_coh_ppml)["post21:spanish_presence_1936_1955"]
b_1956 <- coef(m_post_coh_ppml)["post21:spanish_presence_1956_1978"]

cat(
  "PPML Post x Spanish presence 1936-1955, percent effect:",
  100 * (exp(b_1936) - 1),
  "\n"
)

cat(
  "PPML Post x Spanish presence 1956-1978, percent effect:",
  100 * (exp(b_1956) - 1),
  "\n"
)

if ("viv_emig_10" %in% names(df_presence)) {
  
  m_post_coh_ppml_mig <- fepois(
    total_remesas_ppml ~ post21:spanish_presence_1936_1955 +
      post21:spanish_presence_1956_1978 |
      inegi + year + year[viv_emig_10],
    data = df_presence %>% filter(!is.na(viv_emig_10)),
    cluster = ~ inegi
  )
  
  summary(m_post_coh_ppml_mig)
}


############################################################
# 6.5. Event-study:presencia 1936-1955 (remesas PC)
############################################################

m_ols_presence_1936 <- feols(
  remesas_pc ~ i(year, spanish_presence_1936_1955, ref = ref_year) |
    inegi + year,
  data = df_presence,
  cluster = ~ inegi
)

m_ppml_presence_1936 <- fepois(
  remesas_pc ~ i(year, spanish_presence_1936_1955, ref = ref_year) |
    inegi + year,
  data = df_presence,
  cluster = ~ inegi
)

summary(m_ols_presence_1936)
summary(m_ppml_presence_1936)

sample_check(
  raw_data = estimacion_yr_coh,
  clean_data = df_presence,
  model = m_ppml_presence_1936,
  name = "PPML Spanish presence 1936-1955"
)

plot_presence_1936 <- make_event_plot(
  model = m_ppml_presence_1936,
  title = "PPML event-study: Spanish presence 1936–1955",
  subtitle = paste0(
    "Treated relative to controls; ref. year: ",
    ref_year,
    "; sample: ",
    ifelse(use_2016_sample, "2016–2024", "all years"),
    "; outliers: ",
    outlier_method,
    "; outcome: remittances per capita"
  ),
  output_name = paste0("ppml_event_presence_1936_1955_", suffix, "_pc"),
  ref_year = ref_year
)


############################################################
# 7. Comparación de nobs OLS vs PPML
############################################################

cat("\n")
cat("NOBS continuous OLS:", nobs(m_ols_log_spanish), "\n")
cat("NOBS continuous PPML:", nobs(m_ppml_log_spanish), "\n")

if (exists("m_ols_log_spanish_mig") & exists("m_ppml_log_spanish_mig")) {
  cat("NOBS continuous OLS + mig:", nobs(m_ols_log_spanish_mig), "\n")
  cat("NOBS continuous PPML + mig:", nobs(m_ppml_log_spanish_mig), "\n")
}

cat("NOBS presence 1936 OLS:", nobs(m_ols_presence_1936), "\n")
cat("NOBS presence 1936 PPML:", nobs(m_ppml_presence_1936), "\n")
cat("NOBS presence 1956 OLS:", nobs(m_ols_presence_1956), "\n")
cat("NOBS presence 1956 PPML:", nobs(m_ppml_presence_1956), "\n")

cat("\n")
cat("Gráficos generados en:", graphs_path, "\n")
cat("Tablas generadas en:", tables_path, "\n")
cat("Interpretación PPML: 100*(exp(beta)-1), efecto porcentual sobre remesas esperadas.\n")


### 

# Exporto m_post_log_spanish y m_post_coh_ppml

modelsummary(
  list(
    "Log Spanish-born avg" = m_post_log_spanish, 
    "Cohort PPML" = m_post_coh_ppml),
  output = "Output/Estimaciones/att_poisson.docx",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "post21:log_spanish_born_avg" = "Post × log Spanish-born average",
    "post21:spanish_presence_1936_1955" = "Post × Spanish share (1936-1955)",
    "post21:spanish_presence_1956_1978" = "Post × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE"),
    "Log Spanish-born avg" = c("Yes", "Yes"),
    "Cohort PPML" = c("Yes", "Yes")
  ),
  notes = "Poisson pseudo-maximum likelihood estimates. Standard errors clustered at the municipality level in parentheses."
)


############################################################
# Histograma de remesas totales y tabla de deciles
############################################################

# Elegir base y variable
df_hist <- estimacion_yr %>%
  filter(!is.na(total_remesas))

############################################################
# 1. Histograma de total_remesas
############################################################


df_hist <- estimacion_yr %>%
  filter(!is.na(total_remesas))

g_hist_remesas <- ggplot(df_hist, aes(x = total_remesas)) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "white"
  ) +
  scale_x_continuous(
    labels = scales::comma
  ) +
  labs(
    title = "Distribution of total remittances",
    subtitle = "Municipality-year observations",
    x = "Total remittances",
    y = "Number of observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

g_hist_remesas

ggsave(
  filename = file.path(graphs_path, "hist_total_remesas.png"),
  plot = g_hist_remesas,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


############################################################
# 2. Tabla lista para PowerPoint
############################################################

tabla_deciles_remesas <- df_hist %>%
  mutate(
    decil = ntile(total_remesas, 10)
  ) %>%
  group_by(decil) %>%
  summarise(
    min_remesas = min(total_remesas, na.rm = TRUE),
    max_remesas = max(total_remesas, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

tabla_deciles_remesas

gtsave(
  tabla_deciles_gt,
  filename = file.path(tables_path, "table_deciles_total_remesas.png")
)
