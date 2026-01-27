#' @title RMB Plotting Themes
#' @description Custom ggplot2 themes for publication-quality figures
#' @name themes

#' RMB Default Theme
#'
#' A clean, publication-ready theme for RMB plots
#'
#' @param base_size Base font size (default 12)
#' @param base_family Base font family (default "")
#' @param grid Show grid lines ("none", "major", "minor", "both") (default "major")
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + theme_rmb()
theme_rmb <- function(base_size = 12, base_family = "", grid = "major") {

  # Base theme
  theme_base <- ggplot2::theme_bw(base_size = base_size, base_family = base_family)

  # Customizations
  theme_base +
    ggplot2::theme(
      # Text
      plot.title = ggplot2::element_text(
        size = base_size * 1.3,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 1.0,
        color = "gray40",
        hjust = 0,
        margin = ggplot2::margin(b = 15)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.8,
        color = "gray50",
        hjust = 1
      ),

      # Axes
      axis.title = ggplot2::element_text(
        size = base_size * 1.0,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        size = base_size * 0.9,
        color = "gray20"
      ),
      axis.ticks = ggplot2::element_line(color = "gray70"),

      # Legend
      legend.title = ggplot2::element_text(
        size = base_size * 0.95,
        face = "bold"
      ),
      legend.text = ggplot2::element_text(
        size = base_size * 0.85
      ),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),

      # Panel
      panel.border = ggplot2::element_rect(color = "gray30", fill = NA, linewidth = 0.5),
      panel.background = ggplot2::element_rect(fill = "white"),

      # Grid
      panel.grid.major = if (grid %in% c("major", "both")) {
        ggplot2::element_line(color = "gray90", linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = if (grid %in% c("minor", "both")) {
        ggplot2::element_line(color = "gray95", linewidth = 0.2)
      } else {
        ggplot2::element_blank()
      },

      # Strip (for facets)
      strip.background = ggplot2::element_rect(fill = "gray95", color = "gray70"),
      strip.text = ggplot2::element_text(
        size = base_size * 0.95,
        face = "bold",
        margin = ggplot2::margin(5, 5, 5, 5)
      ),

      # Plot margins
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
}

#' RMB Dark Theme
#'
#' A dark theme for presentations
#'
#' @param base_size Base font size (default 14)
#' @param base_family Base font family (default "")
#'
#' @return A ggplot2 theme object
#' @export
theme_rmb_dark <- function(base_size = 14, base_family = "") {

  ggplot2::theme_dark(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#1a1a2e", color = NA),
      panel.background = ggplot2::element_rect(fill = "#16213e", color = NA),
      panel.border = ggplot2::element_rect(color = "#0f3460", fill = NA, linewidth = 0.5),

      text = ggplot2::element_text(color = "#eaeaea"),
      axis.text = ggplot2::element_text(color = "#c0c0c0"),
      axis.title = ggplot2::element_text(color = "#eaeaea", face = "bold"),

      legend.background = ggplot2::element_rect(fill = "#1a1a2e", color = NA),
      legend.text = ggplot2::element_text(color = "#c0c0c0"),
      legend.title = ggplot2::element_text(color = "#eaeaea", face = "bold"),

      panel.grid.major = ggplot2::element_line(color = "#0f3460", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      strip.background = ggplot2::element_rect(fill = "#0f3460"),
      strip.text = ggplot2::element_text(color = "#eaeaea", face = "bold"),

      plot.title = ggplot2::element_text(color = "#e94560", face = "bold", size = base_size * 1.3),
      plot.subtitle = ggplot2::element_text(color = "#c0c0c0"),

      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
}

#' RMB Color Palettes
#'
#' Custom color palettes for Drosophila activity data
#'
#' @name rmb_palettes
#' @return A vector of colors
NULL

#' @rdname rmb_palettes
#' @export
rmb_colors <- c(
  "wildtype" = "#2E86AB",
  "mutant" = "#E94560",
  "control" = "#4A5568",
  "treatment" = "#48BB78",
  "male" = "#3182CE",
  "female" = "#D53F8C",
  "light" = "#F6E05E",
  "dark" = "#1A202C"
)

#' @rdname rmb_palettes
rmb_palette_main <- c("#2E86AB", "#E94560", "#48BB78", "#F6AD55", "#9F7AEA", "#38B2AC")

#' @rdname rmb_palettes
rmb_palette_sequential <- c("#EBF8FF", "#BEE3F8", "#90CDF4", "#63B3ED", "#4299E1", "#3182CE", "#2B6CB0", "#2C5282")

#' @rdname rmb_palettes
rmb_palette_diverging <- c("#E53E3E", "#FC8181", "#FEB2B2", "#FFFFFF", "#BEE3F8", "#63B3ED", "#3182CE")

#' RMB Fill Scale
#'
#' Discrete fill scale using RMB colors
#'
#' @param ... Arguments passed to discrete_scale
#'
#' @return A ggplot2 scale object
#' @export
scale_fill_rmb <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "rmb",
    palette = function(n) {
      if (n <= length(rmb_palette_main)) {
        rmb_palette_main[1:n]
      } else {
        grDevices::colorRampPalette(rmb_palette_main)(n)
      }
    },
    ...
  )
}

#' RMB Color Scale
#'
#' Discrete color scale using RMB colors
#'
#' @param ... Arguments passed to discrete_scale
#'
#' @return A ggplot2 scale object
#' @export
scale_color_rmb <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "rmb",
    palette = function(n) {
      if (n <= length(rmb_palette_main)) {
        rmb_palette_main[1:n]
      } else {
        grDevices::colorRampPalette(rmb_palette_main)(n)
      }
    },
    ...
  )
}

#' Light/Dark Annotation
#'
#' Add light/dark period shading to a time series plot
#'
#' @param light_hours Vector of two values: c(lights_on, lights_off) in ZT hours
#' @param n_days Number of days to annotate
#' @param minutes_per_day Minutes per day (default 1440)
#' @param alpha Transparency of shading (default 0.2)
#'
#' @return A list of ggplot2 annotation layers
#' @export
annotate_light_dark <- function(light_hours = c(0, 12), n_days = 1,
                                 minutes_per_day = 1440, alpha = 0.2) {

  lights_on <- light_hours[1] * 60
  lights_off <- light_hours[2] * 60

  annotations <- list()

  for (day in 0:(n_days - 1)) {
    offset <- day * minutes_per_day

    # Dark period (after lights off until end of day + before lights on)
    if (lights_off < minutes_per_day) {
      annotations <- c(annotations, list(
        ggplot2::annotate(
          "rect",
          xmin = offset + lights_off,
          xmax = offset + minutes_per_day,
          ymin = -Inf, ymax = Inf,
          fill = "gray30", alpha = alpha
        )
      ))
    }

    if (lights_on > 0 && day > 0) {
      annotations <- c(annotations, list(
        ggplot2::annotate(
          "rect",
          xmin = offset,
          xmax = offset + lights_on,
          ymin = -Inf, ymax = Inf,
          fill = "gray30", alpha = alpha
        )
      ))
    }
  }

  return(annotations)
}
