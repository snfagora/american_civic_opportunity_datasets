# Function to summarize linear regressions
summary_class_civic_org <- function(data) {
  variable_labels <- c(
    POV150 = "Below 150% of the poverty level",
    SNGPNT = "Single-parent households",
    BROAD = "No broadband internet",
    NOHSDP = "No high school diploma",
    UNEMP = "Unemployment",
    REMNRTY = "Racial or ethnic minority status"
  )
  
  data %>%
    pivot_longer(cols = POV150:REMNRTY) %>%
    group_by(name) %>%
    do(tidy(lm(re_or_so_freq ~ value, data = .), conf.int = TRUE)) %>%
    filter(term == "value") %>%
    mutate(name = variable_labels[as.character(name)])
}

# Function to normalize a vector
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Function to create county-level prediction plot
cnty_pred_plot <- function(var, var_name) {
  var <- enquo(var)
  formula <- as.formula(glue("civic_opp_sum_normalized ~ {quo_name(var)}"))
  model <- lm(formula, data = cnty_counts_cov)
  
  tidy_model <- tidy(model, conf.int = TRUE)
  coeff_info <- tidy_model %>%
    filter(term == quo_name(var)) %>%
    select(estimate, conf.low, conf.high) %>%
    unlist() %>%
    as.numeric()
  
  coeff <- coeff_info[1]
  conf_low <- coeff_info[2]
  conf_high <- coeff_info[3]
  coeff_label <- glue("Coefficient: {round(coeff, 2)} [95% CI: {round(conf_low, 2)}, {round(conf_high, 2)}]")
  
  ci95 <- predict(model, cnty_counts_cov, interval = "confidence", level = 0.95) %>%
    as.data.frame()
  
  cnty_counts_cov %>%
    bind_cols(ci95) %>%
    ggplot(aes(x = !!var, y = civic_opp_sum_normalized)) +
    geom_jitter(alpha = 0.2) +
    geom_line(aes(y = fit, col = "OLS fits")) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "95% CIs"), alpha = 0.2) +
    annotate(
      "text", x = Inf, y = Inf, label = coeff_label,
      hjust = 1.1, vjust = 1.1, size = 4, col = "black"
    ) +
    labs(
      y = "Civic opportunity scores",
      x = var_name
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_minimal() +
    guides(
      color = guide_legend(title = NULL),
      fill = guide_legend(title = NULL)
    )
  
}

# Function to create ZIP-code-level prediction plot
zcta_pred_plot <- function(var, var_name) {
  zcta_counts_cov <- zcta_counts_cov %>%
    mutate(civic_opp_sum_normalized = civic_opp_sum_normalized + 1)
  
  var <- enquo(var)
  formula <- as.formula(glue("log10(civic_opp_sum_normalized + 1) ~ {quo_name(var)}"))
  model <- lm(formula, data = zcta_counts_cov)
  
  tidy_model <- tidy(model, conf.int = TRUE)
  coeff_info <- tidy_model %>%
    filter(term == quo_name(var)) %>%
    select(estimate, conf.low, conf.high) %>%
    unlist() %>%
    as.numeric()
  
  coeff <- coeff_info[1]
  conf_low <- coeff_info[2]
  conf_high <- coeff_info[3]
  coeff_label <- glue("Coefficient: {round(coeff, 2)} [95% CI: {round(conf_low, 2)}, {round(conf_high, 2)}]")
  
  ci95 <- predict(model, zcta_counts_cov, interval = "confidence", level = 0.95) %>%
    as.data.frame()
  
  zcta_counts_cov %>%
    bind_cols(ci95) %>%
    ggplot(aes(x = !!var, y = log10(civic_opp_sum_normalized + 1))) +
    geom_jitter(alpha = 0.2) +
    geom_line(aes(y = fit, col = "OLS fits")) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "95% CIs"), alpha = 0.2) +
    annotate(
      "text", x = Inf, y = Inf, label = coeff_label,
      hjust = 1.1, vjust = 1.1, size = 4, col = "black"
    ) +
    labs(
      y = "Log10(Civic opportunity scores)",
      x = var_name
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_minimal() +
    guides(
      color = guide_legend(title = NULL),
      fill = guide_legend(title = NULL)
    )
  
}

# Helper functions
mean_no_na <- function(x) mean(x, na.rm = TRUE)
std_no_na <- function(x) sd(x, na.rm = TRUE) / sqrt(length(x))

# Define a helper function to compute mean and CI
summary_ci <- function(x) {
  m <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  lower <- m - 1.96 * se
  upper <- m + 1.96 * se
  sprintf("%.2f [%.2f, %.2f]", m, lower, upper)
}

# Custom theme for plots
custom_theme <- function(size = 13) {
  theme_bw(base_size = size) +
    theme(
      aspect.ratio = 1.2,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(margin = margin(t = 6)),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(colour = "grey50", hjust = 0),
      legend.position = "bottom"
    )
}

# Correlation summary function
cor_fun <- function(x) {
  out <- correlation(x) %>%
    summary() %>%
    format(2) %>%
    as.matrix()
  row.names(out) <- out[, 1]
  out <- out[, -1]
  return(out)
}

# Function to bind county and ZIP-code summaries
bind_cnty_zcta_summaries <- function(var, var_name) {
  cnty_state_avg_se <- cnty_counts_cov %>%
    group_by(state) %>%
    summarize(avg = mean_no_na({{var}}),
              se = std_no_na({{var}})) %>%
    mutate(unit = "County")
  
  zcta_state_avg_se <- zcta_counts_cov %>%
    group_by(state) %>%
    summarize(avg = mean_no_na({{var}}),
              se = std_no_na({{var}})) %>%
    mutate(unit = "Zipcode")
  
  bind_rows(cnty_state_avg_se, zcta_state_avg_se) %>%
    filter(state != "DC") %>%
    mutate(var = var_name)
}

# Function to compute percent + 95% CI from a dataframe
get_predicted_percent_ci <- function(df, label) {
  df %>%
    filter(!is.na(predicted)) %>%
    count(predicted) %>%
    mutate(total = sum(n)) %>%
    rowwise() %>%
    mutate(ci = list(binom.confint(x = n, n = total, method = "wilson"))) %>%
    ungroup() %>%
    unnest_wider(ci, names_sep = "_") %>%
    transmute(
      predicted,
      count = n,
      percent = round(ci_mean * 100, 2),
      lower_percent = round(ci_lower * 100, 2),
      upper_percent = round(ci_upper * 100, 2),
      group = label
    )
}

# Function to bootstrap median and compute 95% CI
bootstrap_median_ci <- function(x, R = 1000) {
  x <- x[x > 0 & !is.na(x)]
  if (length(x) < 2) return(c(NA, NA, NA))
  
  # Use all available cores minus one
  n_cores <- max(1, parallel::detectCores() - 1)
  
  boot_out <- boot(
    data = x,
    statistic = function(data, i) median(data[i]),
    R = R,
    parallel = "multicore",
    ncpus = n_cores
  )
  
  ci <- tryCatch(
    boot.ci(boot_out, type = "perc")$percent[4:5],
    error = function(e) c(NA, NA)
  )
  
  c(median = median(x), ci_low = ci[1], ci_high = ci[2])
}