################################################################################
# Shared Theme & Party Colors
#
# Purpose: Single source of truth for party colors, labels, and ggplot theme.
#          Source this file from any script that produces plots.
#
# Usage:   source("estimation/theme.R")
################################################################################

library(ggplot2)

################################################################################
# Party Colors (canonical)
################################################################################

PARTY_COLORS <- c(
  "CDU/CSU"  = "#000000",
  "SPD"      = "#E3000F",
  "GRÜNE"    = "#1AA037",
  "FDP"      = "#FFCC00",
  "AfD"      = "#009EE0",
  "LINKE"    = "#BE3075",
  "BSW"      = "#572B81",
  "Sonstige" = "#AAAAAA"
)

## Mapping from canonical column names to display names
PARTY_LABELS <- c(
  cdu_csu  = "CDU/CSU",
  spd      = "SPD",
  gruene   = "GRÜNE",
  fdp      = "FDP",
  afd      = "AfD",
  linke    = "LINKE",
  bsw      = "BSW",
  sonstige = "Sonstige"
)


################################################################################
# Minimal ggplot theme
################################################################################

theme_forecast <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Text
      plot.title       = element_text(size = rel(1.2), face = "bold",
                                      margin = margin(b = 8)),
      plot.subtitle    = element_text(size = rel(0.85), color = "grey40",
                                      margin = margin(b = 12)),
      plot.caption     = element_text(size = rel(0.7), color = "grey50",
                                      hjust = 0, margin = margin(t = 10)),

      # Axes
      axis.title.x     = element_blank(),
      axis.title.y     = element_text(size = rel(0.85), color = "grey30",
                                      margin = margin(r = 8)),
      axis.text         = element_text(size = rel(0.8), color = "grey30"),
      axis.ticks        = element_line(color = "grey80", linewidth = 0.3),

      # Grid
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),

      # Legend
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = rel(0.8)),
      legend.key.width  = unit(1.2, "cm"),
      legend.key.height = unit(0.4, "cm"),

      # Facets
      strip.text        = element_text(size = rel(0.85), face = "bold",
                                       margin = margin(b = 4, t = 4)),

      # Margins
      plot.margin       = margin(12, 12, 12, 12)
    )
}


################################################################################
# Scale helpers
################################################################################

scale_color_parties <- function(...) {
  scale_color_manual(values = PARTY_COLORS, ...)
}

scale_fill_parties <- function(...) {
  scale_fill_manual(values = PARTY_COLORS, ...)
}
