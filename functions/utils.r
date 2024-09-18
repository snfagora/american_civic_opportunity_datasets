
summary_primary_civic_org <- function(data, var, var_name) {
  
  data %>%
    group_by(primary_org_cat) %>%
    summarize(avg = mean_no_na({{var}}),
              se = std_no_na({{var}})) %>%
    mutate(low_ci = avg - 1.96 * se,
           hi_ci = avg + 1.96 * se) %>%
    mutate(variable = var_name)
}

normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

cnty_pred_plot <- function(var, var_name) {
  # Capture the variable
  var <- enquo(var)
  
  # Create the formula
  formula <- as.formula(glue("civic_opp_sum_normalized ~ {quo_name(var)}"))
  
  # Create the linear model
  model <- lm(formula, data = cnts_counts_cov)
  
  # Extract regression coefficient and confidence intervals
  tidy_model <- tidy(model, conf.int = TRUE)
  
  coeff_info <- tidy_model %>%
    filter(term == quo_name(var)) %>%
    select(estimate, conf.low, conf.high) %>%
    unlist() %>%
    as.numeric()
  
  coeff <- coeff_info[1]
  conf_low <- coeff_info[2]
  conf_high <- coeff_info[3]
  
  # Create label for annotation with confidence intervals
  coeff_label <- glue("Coefficient: {round(coeff, 2)} [95% CI: {round(conf_low, 2)}, {round(conf_high, 2)}]")
  
  # Get the confidence intervals for the prediction
  ci95 <- predict(model, cnts_counts_cov, interval = "confidence", level = 0.95) %>%
    as.data.frame() %>%
    rename(fit = fit, lwr = lwr, upr = upr)
  
  # Plot
  cnts_counts_cov %>%
    bind_cols(ci95) %>%
    ggplot(aes(x = !!var, y = civic_opp_sum_normalized)) +
    geom_jitter(alpha = 0.5) +
    geom_line(aes(y = fit, col = "OLS fit")) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "95% CIs"), alpha = 0.2) +
    annotate(
      "text", 
      x = Inf, y = Inf, 
      label = coeff_label,
      hjust = 1.1, vjust = 1.1, 
      size = 4, col = "black"
    ) +
    labs(
      y = "Civic opportunity scores",
      x = var_name,
      col = "Lines",
      fill = "Error bars"
    ) +
    scale_x_continuous(label = scales::percent) +
    theme_minimal()
}

zcta_pred_plot <- function(var, var_name) {

  # Capture the variable
  var <- enquo(var)
  
  # Create the formula
  formula <- as.formula(glue("civic_opp_sum_normalized ~ {quo_name(var)}"))
  
  # Create the linear model
  model <- lm(formula, data = zcta_counts_cov)
  
  # Extract regression coefficient and confidence intervals
  tidy_model <- tidy(model, conf.int = TRUE)
  
  coeff_info <- tidy_model %>%
    filter(term == quo_name(var)) %>%
    select(estimate, conf.low, conf.high) %>%
    unlist() %>%
    as.numeric()
  
  coeff <- coeff_info[1]
  conf_low <- coeff_info[2]
  conf_high <- coeff_info[3]
  
  # Create label for annotation with confidence intervals
  coeff_label <- glue("Coefficient: {round(coeff, 2)} [95% CI: {round(conf_low, 2)}, {round(conf_high, 2)}]")
  
  # Get the confidence intervals for the prediction
  ci95 <- predict(model, zcta_counts_cov, interval = "confidence", level = 0.95) %>%
    as.data.frame() %>%
    rename(fit = fit, lwr = lwr, upr = upr)
  
  # Plot
  zcta_counts_cov %>%
    bind_cols(ci95) %>%
    ggplot(aes(x = !!var, y = civic_opp_sum_normalized)) +
    geom_jitter(alpha = 0.5) +
    geom_line(aes(y = fit, col = "OLS fit")) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = "95% CIs"), alpha = 0.2) +
    annotate(
      "text", 
      x = Inf, y = Inf, 
      label = coeff_label,
      hjust = 1.1, vjust = 1.1, 
      size = 4, col = "black"
    ) +
    labs(
      y = "Civic opportunity scores",
      x = var_name,
      col = "Lines",
      fill = "Error bars"
    ) +
    scale_x_continuous(label = scales::percent) +
    theme_minimal()
}


mean_no_na <- function(x) mean(x, na.rm = T)

std_no_na <- function(x) sd(x, na.rm = T)/sqrt(length(x))

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

# the following function is from here: https://modelsummary.com/vignettes/datasummary.html

cor_fun <- function(x) {
  out <- correlation(x) |>
    summary() |>
    format(2) |> 
    as.matrix()
  row.names(out) <- out[, 1]
  out <- out[, 2:ncol(out)]
  return(out)
}

bind_cnty_zcta_summaries <- function(var, var_name) {
  
  cnts_state_avg_se <- cnts_counts_cov %>%
    group_by(state) %>%
    summarize(avg = mean_no_na({{var}}),
              se = std_no_na({{var}})) %>%
    mutate(unit = "County")
  
  zcta_state_avg_se <- zcta_counts_cov %>%
    group_by(state) %>%
    summarize(avg = mean_no_na({{var}}),
              se = std_no_na({{var}})) %>%
    mutate(unit = "Zipcode")
  
  binded_out <- bind_rows(cnts_state_avg_se, zcta_state_avg_se) %>%
    filter(state != "DC") %>%
    mutate(var = var_name)  # Capture variable name as a string
  
  return(binded_out)
  
}
