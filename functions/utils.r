
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
