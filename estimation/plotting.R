################################################################################
# Plotting Functions for Political Poll Analysis
# 
# This file contains functions for creating visualizations of political polling data,
# including posterior predictive checks and prior-posterior comparisons.
#
# Dependencies:
#   - tidyverse
#   - posterior
#   - bayesplot
#   - ggthemes
#
# Author: [Your Name]
# Last Modified: [Date]
################################################################################

# Load required packages
library(tidyverse)
library(posterior)
library(bayesplot)
library(ggthemes)

################################################################################
# Posterior Predictive Checks
################################################################################

#' Create Posterior Predictive Check Plots for Vote Shares
#'
#' @param fit Fitted model object containing posterior draws
#' @param data_list List containing observed data
#' @param n_draws Number of posterior draws to use in visualization (default: 100)
#' @return List containing plots and transformed data:
#'   - plot_dens: Density overlay plot
#'   - plot_hist: Histogram comparison plot
#'   - y_long: Long-format observed data
#'   - y_rep_transformed: Transformed posterior predictions
create_vote_share_ppc <- function(fit, data_list, n_draws = 100) {
  # Extract posterior predictive draws
  survey_y_rep <- fit$draws("survey_y_rep") %>%
    posterior::as_draws_matrix()
  
  # Transform observed data to long format with vote shares
  survey_y_long <- data_list$survey_y %>%
    as.data.frame() %>%
    mutate(i = 1:n()) %>%
    pivot_longer(
      c(-i),
      names_to = "party",
      values_to = "count"
    ) %>%
    group_by(i) %>%
    mutate(
      vote_share = count / sum(count)
    )
  
  # Helper function to transform y_rep into vote shares
  transform_survey_y_rep <- function(survey_y_rep) {
    survey_y_rep %>%
      as.data.frame() %>%
      mutate(
        draw = 1:n()
      ) %>%
      pivot_longer(
        c(-draw),
        names_to = "var",
        values_to = "val"
      ) %>%
      mutate(i = str_match(var, "(\\d+),")[,2]) %>%
      group_by(i, draw) %>%
      mutate(val = val / sum(val)) %>%
      ungroup() %>%
      select(-i) %>%
      pivot_wider(
        id_cols = draw,
        values_from = val,
        names_from = var
      ) %>%
      select(-draw) %>%
      as.matrix()
  }
  
  # Transform y_rep
  survey_y_rep_transformed <- transform_survey_y_rep(survey_y_rep)
  
  # Create plot
  plt_dens <- bayesplot::ppc_dens_overlay(
    survey_y_long$vote_share, 
    survey_y_rep_transformed[sample(1:nrow(survey_y_rep_transformed),n_draws),]
  ) + 
    labs(
      title = "Posterior Predictive Check (density): Vote Shares",
      x = "Vote Share",
      y = "Density"
    )
  
  ggsave(filename = "estimation/plt/ppc/ppc_yrep_density.png",
         plt_dens, 
         width = 7, height = 7)
  
  plt_hist = bayesplot::ppc_hist(survey_y_long$vote_share, 
                                 survey_y_rep_transformed[sample(1:nrow(survey_y_rep_transformed),11),]) + 
    labs(
      title = "Posterior Predictive Check (histogram): Vote Shares",
      x = "Vote Share",
      y = "Density"
    )
  
  ggsave(filename = "estimation/plt/ppc/ppc_yrep_histogram.png",
         plt_hist, 
         width = 7, height = 7)
  
  # Return both the plot and the transformed data
  return(list(
    plot_dens = plt_dens,
    plot_hist = plt_hist,
    survey_y_long = survey_y_long,
    survey_y_rep_transformed = survey_y_rep_transformed
  ))
}


################################################################################
# Prior-Posterior Comparison Plots
################################################################################


#' Compare Prior and Posterior Alpha Parameters
#'
#' Creates a comparison plot showing prior and posterior distributions of alpha 
#' parameters, which represent party-specific volatility scales.
#'
#' @param fit Fitted model object containing draws
#' @param party_names Vector of party names
#' @param prior_mean Mean for the prior normal distribution (default: 0)
#' @param prior_sd Standard deviation for the prior normal distribution (default: 1)
#' @return ggplot object showing prior-posterior comparisons of alpha parameters
compare_alpha_parameters <- function(fit, party_names, prior_mean = 0, prior_sd = 1) {
  #' Create a comparison plot of prior and posterior alpha parameter distributions
  #' 
  #' @param fit The fitted model object containing draws
  #' @param party_names Vector of party names in order
  #' @param prior_mean Mean for the prior normal distribution (default 0)
  #' @param prior_sd Standard deviation for the prior normal distribution (default 1)
  #' @return A ggplot object showing prior and posterior comparisons of alpha parameters
  
  # Extract posterior draws for alpha and convert to data frame
  posterior_draws <- fit$draws("alpha") %>%
    posterior::as_draws_df() %>%
    # Add draw number
    mutate(draw = 1:n()) %>%
    # Remove unnecessary columns (those containing ".")
    select(!contains(".")) %>%
    # Convert to long format
    pivot_longer(
      cols = c(-draw),
      values_to = "val",
      names_to = "var"
    )
  
  # Combine posterior and prior distributions
  combined_distributions <- bind_rows(
    # Posterior distribution
    posterior_draws %>%
      mutate(kind = "Posterior"),
    # Prior distribution - simulate from normal distribution and exponentiate
    posterior_draws %>%
      mutate(
        kind = "Prior",
        val = exp(rnorm(n(), prior_mean, prior_sd)),
        # Ensure reference category remains 1
        val = ifelse(var == "alpha[1]", 1, val)
      )
  ) %>%
    # Add party information
    mutate(
      # Extract party index from variable name
      ix_party = 1 + as.integer(str_match(var, "(\\d+)")[, 2]),
      # Map indices to party names
      party = party_names[ix_party]
    )
  
  # Calculate summary statistics and create plot
  plot <- combined_distributions %>%
    # Calculate quantiles for each party and distribution type
    group_by(party, kind) %>%
    summarize(
      q50 = quantile(val, 0.5),  # median
      q25 = quantile(val, 0.25), # lower quartile
      q75 = quantile(val, 0.75)  # upper quartile
    ) %>%
    # Create the visualization
    ggplot(aes(x = party, y = q50, color = kind)) + 
    # Add points for medians
    geom_point(position = position_dodge(width = 0.5)) +
    # Add error bars for quartiles
    geom_errorbar(
      aes(ymin = q25, ymax = q75), 
      width = 0, 
      position = position_dodge(width = 0.5)
    ) +
    # Customize labels and theme
    labs(
      y = "Ratio of average change rate relative to first party",
      caption = "The plot assesses how the change rates differ between parties. 
                 The overall trend is the same but some parties are expected to have a higher/lower change rate than others"
    ) + 
    theme_light() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    ggthemes::scale_color_colorblind()
  
  
  ggsave(
    filename = "estimation/plt/prior_posterior/plot_volatility_party_scales.png",
    plot = plot,
    width = 7,
    height = 7
  )
  
  return(plot)
}

#' Compare Prior and Posterior Length Scale Distributions
#'
#' Creates a comparison plot showing prior and posterior distributions of the trend
#' length scale parameter, which represents the temporal correlation structure in
#' the model.
#'
#' @param fit Fitted model object containing draws
#' @param prior_mean Mean for the prior normal distribution
#' @param prior_sd Standard deviation for the prior normal distribution (default: 10)
#' @param save_path Directory path for saving plots (default: "estimation/plt/prior_posterior/")
#' @return ggplot object showing prior-posterior comparison of length scale parameter
plot_length_scale_comparison <- function(fit, prior_mean, prior_sd = 10, 
                                         save_path = "estimation/plt/prior_posterior/") {
  # Create directory if it doesn't exist
  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  
  # Extract posterior draws and prepare for plotting
  posterior_draws <- fit$draws("trend_length_scale") %>%
    posterior::as_draws_df() %>%
    select(!contains(".")) %>%
    pivot_longer(
      cols = everything(),
      values_to = "value",
      names_to = "parameter"
    ) %>%
    mutate(distribution = "Posterior")
  
  # Generate prior draws
  prior_draws <- bind_rows(
    data.frame(
      value = abs(rnorm(nrow(posterior_draws), 5, 20)),
      distribution = "Prior",
      parameter = "trend_length_scale[1]"
    ),
    data.frame(
      value = abs(rnorm(nrow(posterior_draws), 365.0 / 7.0, 10)),
      distribution = "Prior",
      parameter = "trend_length_scale[2]"
    )
  )
  
  # Combine draws and ensure factor ordering
  all_draws <- bind_rows(posterior_draws, prior_draws) %>%
    mutate(distribution = factor(distribution, levels = c("Prior", "Posterior")))
  
  # Calculate summary statistics for annotations
  medians <- all_draws %>%
    group_by(distribution, parameter) %>%
    summarize(
      median = median(value),
      mad = mad(value)
    )
  
  # Create visualization
  plot <- ggplot(all_draws, aes(x = value, fill = distribution)) +
    facet_grid(parameter ~ .) +
    # Add histograms with transparency
    geom_histogram(
      position = "identity",
      alpha = 0.5,
      bins = 50,
      color = NA
    ) +
    # Add vertical lines for distribution medians
    geom_vline(
      data = medians,
      aes(xintercept = median, color = distribution),
      linetype = "dashed"
    ) +
    # Add statistical annotations
    geom_text(
      data = medians,
      aes(
        x = median,
        y = Inf,
        label = sprintf("Median: %.2f\nMAD: %.2f", median, mad),
        color = distribution
      ),
      vjust = 1.2,
      hjust = -0.05,
      size = 3
    ) +
    # Customize appearance
    theme_light() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.spacing.x = unit(0.2, "cm"),
      panel.grid.minor = element_blank()
    ) +
    # Use colorblind-friendly palette
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    # Add informative labels
    labs(
      title = "Prior and Posterior Distributions of Trend Length Scale",
      subtitle = "Histogram comparison with medians",
      x = "Length Scale",
      y = "Count",
      caption = "Dashed lines indicate distribution medians.\nMAD = Median Absolute Deviation"
    )
  
  # Save plot
  ggsave(
    file.path(save_path, "comparison_length_scale.png"),
    plot,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  return(plot)
}




#' Create and Save Party Correlation Plots
#'
#' Creates two visualizations of party correlations from posterior draws:
#' 1. Density plots showing the distribution of correlations between each party pair
#' 2. Heatmap plots showing correlation matrices for selected posterior draws
#'
#' @param fit Fitted model object containing posterior draws
#' @param party_names Vector of party names
#' @param n_draws Number of draws to show in heatmap visualization (default: 25)
#' @param save_path Directory path for saving plots (default: "estimation/plt/prior_posterior/")
#' @return List containing both plot objects (density_plot and heatmap_plot)
plot_party_correlations <- function(fit, party_names, n_draws = 25, 
                                    save_path = "estimation/plt/prior_posterior/") {
  # Create directory if it doesn't exist
  dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
  
  # Common data preparation
  omega_draws <- fit$draws("trend_party_correlation") %>%
    posterior::as_draws_df() %>%
    select(!contains("."))
  
  # --- Density Plot ---
  density_data <- omega_draws %>%
    pivot_longer(
      cols = everything(),
      names_to = "pair",
      values_to = "correlation"
    ) %>%
    mutate(
      row = as.integer(str_match(pair, "trend_party_correlation\\[(\\d+),(\\d+)\\]")[,2]),
      col = as.integer(str_match(pair, "trend_party_correlation\\[(\\d+),(\\d+)\\]")[,3]),
      party_row = factor(party_names[row + 1], levels = party_names),
      party_col = factor(party_names[col + 1], levels = party_names)
    )
  
  # Calculate medians for annotations
  medians <- density_data %>%
    group_by(party_row, party_col) %>%
    summarize(
      median_corr = median(correlation),
      .groups = 'drop'
    )
  
  # Create density plot
  density_plot <- ggplot(density_data, aes(x = correlation)) +
    geom_density(fill = "steelblue", alpha = 0.4) +
    geom_vline(
      data = medians,
      aes(xintercept = median_corr),
      color = "red",
      linetype = "dashed"
    ) +
    geom_vline(xintercept = 0, color = "black", alpha = 0.3) +
    geom_text(
      data = medians,
      aes(x = -0.8, y = Inf, label = sprintf("Median: %.2f", median_corr)),
      vjust = 2,
      size = 3
    ) +
    facet_grid(
      party_row ~ party_col,
      switch = "y"
    ) +
    theme_light() +
    theme(
      strip.text = element_text(size = 9),
      strip.text.y.left = element_text(angle = 0),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      strip.placement = "outside"
    ) +
    labs(
      title = "Posterior Correlation Distributions",
      subtitle = "Density Plots with Median Values",
      x = "Correlation",
      y = "Density"
    ) +
    xlim(-1, 1)
  
  # Save density plot
  ggsave(
    file.path(save_path, "party_correlations_density.png"),
    density_plot,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  # --- Heatmap Plot ---
  # Prepare data for heatmap
  omega_draws_with_draw <- omega_draws %>%
    mutate(draw = 1:n())
  
  selected_draws <- sample(1:nrow(omega_draws_with_draw), n_draws)
  
  heatmap_data <- omega_draws_with_draw[selected_draws, ] %>%
    pivot_longer(
      cols = -draw,
      names_to = "pair",
      values_to = "correlation"
    ) %>%
    mutate(
      row = as.integer(str_match(pair, "Omega\\[(\\d+),(\\d+)\\]")[,2]),
      col = as.integer(str_match(pair, "Omega\\[(\\d+),(\\d+)\\]")[,3]),
      party1 = party_names[row + 1],
      party2 = party_names[col + 1]
    )
  
  # Create heatmap plot
  heatmap_plot <- ggplot(heatmap_data, aes(x = party1, y = party2, fill = correlation)) +
    geom_tile() +
    geom_text(
      aes(label = sprintf("%.2f", correlation)),
      color = ifelse(abs(heatmap_data$correlation) > 0.5, "white", "black")
    ) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    facet_wrap(~draw, ncol = 5) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    ) +
    labs(
      title = "Posterior Draws of Party Correlation Matrix",
      x = NULL,
      y = NULL,
      fill = "Correlation"
    )
  
  # Save heatmap plot
  ggsave(
    file.path(save_path, "party_correlations_heatmap.png"),
    heatmap_plot,
    width = 15,
    height = 12,
    dpi = 300
  )
  
  # Return both plots in a list
  return(list(
    density_plot = density_plot,
    heatmap_plot = heatmap_plot
  ))
}


#' Plot Party Vote Share Trends Over Time
#'
#' Creates visualizations of predicted vote shares over time for each party,
#' including overall trends, long-term trends, and short-term trends.
#' Uses traditional German party colors for visualization.
#'
#' @param fit Fitted model object containing posterior draws
#' @param input_data Data frame containing dates and index mapping
#' @param df Original data frame with observed vote shares
#' @param save_path Directory path for saving plots (default: "estimation/plt/trends/")
#' @return List of ggplot objects showing temporal trends with uncertainty bands
plot_vote_share_trends <- function(fit, input_data, 
                                   index_date,
                                   df, 
                                   election_dates, 
                                   cutoff_date,
                                   save_path = "estimation/plt/trends/") {
  
  # Traditional German party colors
  party_colors <- c(
    "CDU/CSU" = "#000000",  # Black
    "SPD"     = "#E3000F",  # Red
    "GRÜNE"   = "#46962b",  # Green
    "FDP"     = "#FFED00",  # Yellow
    "LINKE"   = "#BE3075",  # Purple/Pink
    "Sonstige" = "#A4A4A4"  # Grey
  )
  
  # Helper function to prepare trend data
  prepare_trend_data <- function(trend_var) {
    fit$summary(trend_var, ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))) %>%
      mutate(
        layer1_aggregate_idx = as.integer(str_match(variable, "(\\d+),")[, 2]),
        ix_party = as.integer(str_match(variable, ",(\\d+)")[, 2]),
        party = factor(
          c("CDU/CSU", "FDP", "GRÜNE", "LINKE", "Sonstige", "SPD")[ix_party],
          levels = names(party_colors)
        )
      ) %>%
      right_join(index_date %>% 
                   select(date, layer1_aggregate_idx))
  }
  
  # Helper function to filter relevant election dates
  get_relevant_elections <- function(trend_data) {
    election_dates %>%
      filter(
        date >= min(trend_data$date),
        date <= max(trend_data$date)
      )
  }
  
  # Prepare observed data (shared across all plots)
  observed_data <- df %>% 
    mutate(party = ifelse(party == "REP", "Sonstige", party)) %>%
    group_by(uuid, party, date) %>%
    summarize(vote_share = sum(vote_share)) %>%
    ungroup() %>%
    mutate(vote_share = vote_share / 100)
  
  # Helper function to create trend plot
  create_trend_plot <- function(trend_data, title_prefix, trend_description, trend_var) {
    relevant_elections <- get_relevant_elections(trend_data)
    
    # Define legend settings based on plot type
    legend_settings <- if (grepl("short_term", trend_var)) {
      theme(legend.position = "none")
    } else {
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      )
    }
    
    ggplot(trend_data, aes(x = date, y = `50%`, color = party, fill = party)) +
      # Add uncertainty bands
      geom_ribbon(
        aes(ymin = `2.5%`, ymax = `97.5%`),
        alpha = 0.125,
        color = NA
      ) +
      geom_ribbon(
        aes(ymin = `25%`, ymax = `75%`),
        alpha = 0.25,
        color = NA
      ) +
      # Add trend lines and observed points (if applicable)
      geom_line(linewidth = 1) +
      {if (!grepl("short_term", trend_var)) 
        geom_point(
          data = observed_data,
          aes(y = vote_share),
          size = 2
        )
      } +
      # Customize appearance
      scale_color_manual(values = party_colors) +
      scale_fill_manual(values = party_colors) +
      {if (grepl("short_term", trend_var)) {
        facet_wrap(~party, ncol = 2)
      }} + 
      {if (grepl("short_term", trend_var)) {
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          name = "Percentage Difference from Long-term Average",
          # Let the limits be determined by the data
          expand = expansion(mult = 0.05)
        )
      } else {
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          limits = c(0, 0.5),
          name = "Vote Share"
        )
      }} +
      theme_light() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()
      ) +
      legend_settings +
      # Add 5% threshold line and annotation for non-short-term plots
      {if (!grepl("short_term", trend_var)) {
        list(
          geom_hline(
            yintercept = 0.05,
            linetype = "dashed",
            color = "black",
            alpha = 0.5
          ),
          annotate(
            "label",
            x = min(trend_data$date, na.rm = TRUE),
            y = 0.05,
            label = "5% electoral threshold",
            hjust = 0,
            size = 3,
            fill = "white",
            alpha = 0.8,
            label.r = unit(0.15, "lines")
          )
        )
      }} +
      # Add labels
      labs(
        title = paste0(title_prefix, " Vote Shares Over Time"),
        subtitle = paste0(trend_description, "\nLines show median predictions with 50% and 95% credible intervals"),
        x = "Date",
        y = "Vote Share",
        caption = "Points show observed data. Bands show 50% (darker) and 95% (lighter) credible intervals."
      ) +
      # Add election date markers and labels
      geom_vline(
        data = relevant_elections,
        aes(xintercept = date),
        linetype = "dashed",
        color = "darkred",
        alpha = 0.5
      ) +
      geom_text(
        data = relevant_elections,
        aes(
          x = date,
          y = max(trend_data$`97.5%`) -0.01,
          label = format(date, "%b %Y")
        ),
        angle = 90,
        hjust = 0.25,
        vjust = -0.45,
        size = 3,
        color = "darkred",
        inherit.aes = FALSE
      ) +
      # Add cutoff date marker and label
      geom_vline(
        data = data.frame(date = cutoff_date),
        aes(xintercept = date),
        linetype = "twodash",
        alpha = 0.75
      ) +
      geom_text(
        data = data.frame(date = cutoff_date),
        aes(
          x = date,
          y = max(trend_data$`97.5%`) -0.01,
          label = "Data cutoff"
        ),
        angle = 90,
        hjust = 0.25,
        vjust = -0.45,
        size = 3,
        inherit.aes = FALSE
      )
  }
  
  # Define trend components
  trend_components <- list(
    list(
      var = "trend_shares",
      prefix = "Overall",
      desc = "Combined long-term and short-term trends",
      filename = "overall_trend.png"
    ),
    list(
      var = "trend_long_term_shares",
      prefix = "Long-term",
      desc = "Underlying long-term trends excluding short-term fluctuations",
      filename = "long_term_trend.png"
    ),
    list(
      var = "trend_short_term_shares",
      prefix = "Short-term",
      desc = "Short-term fluctuations in voting intentions",
      filename = "short_term_trend.png"
    )
  )
  
  # Create and save all plots
  plots <- list()
  
  for (component in trend_components) {
    # Prepare trend data
    trend_data <- prepare_trend_data(component$var)
    
    # Create plot
    plot <- create_trend_plot(
      trend_data = trend_data,
      title_prefix = component$prefix,
      trend_description = component$desc,
      trend_var = component$var
    )
    
    # Save plot
    ggsave(
      filename = file.path(save_path, component$filename),
      plot = plot,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    plots[[component$var]] <- plot
  }
  
  return(plots)
}

#' Plot Trend Volatility Over Time
#'
#' Creates a visualization of the sigma parameter (trend volatility) over time,
#' with vertical lines marking federal elections that fall within the data range.
#'
#' @param fit Fitted model object containing posterior draws
#' @param index_date Data frame mapping weeks to dates
#' @param save_path Directory path for saving plots (default: "estimation/plt/uncertainty/")
#' @return ggplot object showing sigma trend with election markers
plot_trend_volatility <- function(fit, index_date, 
                                  election_dates,
                                   save_path = "estimation/plt/trends") {
  # Prepare trend uncertainty data
  trend_sigma <- fit$summary("volatility_short_term", ~quantile(., c(0.025, 0.25, 0.5, 0.75, 0.975))) %>%
    mutate(
      layer2_aggregate_idx = as.integer(str_match(variable, "(\\d+)")[,2])
    ) %>%
    right_join(
      index_date %>%
        distinct(date, layer2_aggregate_idx)
    )
  
  # Filter election dates to those within data range
  relevant_elections <- election_dates %>%
    filter(
      date >= min(trend_sigma$date),
      date <= max(trend_sigma$date)
    )
  
  # Create plot
  plot <- ggplot(trend_sigma, aes(x = date, y = `50%`)) +
    # Add uncertainty bands
    geom_ribbon(
      aes(ymin = `2.5%`, ymax = `97.5%`),
      alpha = 0.125,
      fill = "steelblue"
    ) +
    geom_ribbon(
      aes(ymin = `25%`, ymax = `75%`),
      alpha = 0.25,
      fill = "steelblue"
    ) +
    # Add main trend line
    geom_line(
      color = "steelblue",
      linewidth = 1
    ) +
    # Add election date markers
    geom_vline(
      data = relevant_elections,
      aes(xintercept = date),
      linetype = "dashed",
      color = "darkred",
      alpha = 0.5
    ) +
    # Add election date labels
    geom_text(
      data = relevant_elections,
      aes(
        x = date,
        y = max(trend_sigma$`97.5%`),
        label = format(date, "%b %Y")
      ),
      angle = 90,
      hjust = 0.25,
      vjust = -0.5,
      size = 3,
      color = "darkred"
    ) +
    # Customize appearance
    theme_light() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()
    ) +
    # Add labels
    labs(
      title = "Trend Volatility Over Time",
      subtitle = "Median and credible intervals with federal election markers",
      y = "Volatility",
      caption = "Bands show 50% (darker) and 95% (lighter) credible intervals.\nVertical lines indicate federal elections."
    )
  
  # Save plot
  ggsave(
    filename = file.path(save_path, "trend_uncertainty.png"),
    plot = plot,
    width = 12,
    height = 6,
    dpi = 300
  )
  
  return(plot)
}

################################################################################
# End of File
################################################################################