# install.packages("ggplot2", "patchwork", "dplyr", "readr", "svglite", "ragg")

options(warn = 1)

local_lib <- Sys.getenv("NATURE_FIGURE_RLIB", unset = "")
if (nzchar(local_lib)) .libPaths(c(local_lib, .libPaths()))

required_packages <- c("ggplot2", "patchwork", "svglite", "ragg")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "),
       ". Install them before rendering this figure.")
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg) == 1) {
  dirname(normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(script_dir)

source_file <- "Fig1_source_data.csv"
stopifnot(file.exists(source_file))
dat <- read.csv(source_file, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(nrow(dat) == 15L, all(is.finite(dat$estimate)), all(is.finite(dat$se)))
dat$ci_low <- dat$estimate - 1.96 * dat$se
dat$ci_high <- dat$estimate + 1.96 * dat$se

profile_levels <- c(
  "High Activity-High Resilience",
  "Low Activity-Low Resilience",
  "Light Activity-Moderate Resilience"
)

profile_colors <- c(
  "High Activity-High Resilience" = "#2F6F9F",
  "Low Activity-Low Resilience" = "#C5544D",
  "Light Activity-Moderate Resilience" = "#6C8F7B"
)
profile_shapes <- c(
  "High Activity-High Resilience" = 16,
  "Low Activity-Low Resilience" = 17,
  "Light Activity-Moderate Resilience" = 15
)
profile_linetypes <- c(
  "High Activity-High Resilience" = "solid",
  "Low Activity-Low Resilience" = "longdash",
  "Light Activity-Moderate Resilience" = "dotted"
)
legend_labels <- c(
  "High Activity-High Resilience" = "High Activity-High Resilience (n = 7,151; 40.7%)",
  "Low Activity-Low Resilience" = "Low Activity-Low Resilience (n = 2,183; 12.5%)",
  "Light Activity-Moderate Resilience" = "Light Activity-Moderate Resilience (n = 8,198; 46.8%)"
)
short_labels <- c(
  "High Activity-High Resilience" = "High activity-\nhigh resilience",
  "Low Activity-Low Resilience" = "Low activity-\nlow resilience",
  "Light Activity-Moderate Resilience" = "Light activity-\nmoderate resilience"
)

theme_nature <- function(base_size = 7.2, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "#1F1F1F"),
      axis.ticks = element_line(linewidth = 0.35, colour = "#1F1F1F"),
      axis.ticks.length = grid::unit(1.4, "mm"),
      axis.title = element_text(size = 8, colour = "#1F1F1F"),
      axis.text = element_text(size = 7.2, colour = "#1F1F1F"),
      plot.title = element_text(size = 8.6, face = "bold", hjust = 0, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 6.2, colour = "#4A4A4A", hjust = 0,
                                   margin = margin(b = 5)),
      plot.tag = element_text(size = 8.6, face = "bold", colour = "#111111"),
      plot.tag.position = c(0, 1),
      plot.margin = margin(7, 7, 6, 7),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 6.2, colour = "#1F1F1F"),
      legend.key.width = grid::unit(7, "mm"),
      legend.key.height = grid::unit(3.4, "mm"),
      legend.spacing.x = grid::unit(2.2, "mm"),
      legend.margin = margin(t = 2),
      panel.grid = element_blank()
    )
}

indicator_dat <- dat[dat$panel == "a", , drop = FALSE]
indicator_dat$profile <- factor(indicator_dat$profile, levels = profile_levels)
indicator_dat$measure <- factor(
  indicator_dat$measure,
  levels = c("Psychological resilience", "MVPA", "LPA"),
  labels = c("Psychological\nresilience", "MVPA", "LIPA")
)

p_a <- ggplot(
  indicator_dat,
  aes(x = measure, y = estimate, group = profile, colour = profile,
      shape = profile, linetype = profile)
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#777777") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.08,
                linewidth = 0.42, show.legend = FALSE) +
  geom_line(linewidth = 0.72) +
  geom_point(size = 2.15, stroke = 0.25) +
  scale_colour_manual(values = profile_colors, breaks = profile_levels,
                      labels = legend_labels) +
  scale_shape_manual(values = profile_shapes, breaks = profile_levels,
                     labels = legend_labels) +
  scale_linetype_manual(values = profile_linetypes, breaks = profile_levels,
                        labels = legend_labels) +
  scale_y_continuous(breaks = seq(-2.5, 1.0, 0.5), limits = c(-2.7, 1.25),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Mind-body profile patterns",
    x = NULL,
    y = "Estimated mean (z score)"
  ) +
  theme_nature() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 6.4, lineheight = 0.9),
    plot.margin = margin(7, 8, 6, 5)
  )

make_outcome_panel <- function(panel_id, title, subtitle, x_limits, x_breaks) {
  panel_dat <- dat[dat$panel == panel_id, , drop = FALSE]
  panel_dat$profile <- factor(panel_dat$profile, levels = rev(profile_levels))
  panel_dat$value_label <- sprintf("%.2f", panel_dat$estimate)

  label_offset <- diff(x_limits) * 0.025

  ggplot(panel_dat, aes(y = profile, x = estimate, colour = profile, shape = profile)) +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.17,
                  linewidth = 0.52, show.legend = FALSE) +
    geom_point(size = 2.25, stroke = 0.25, show.legend = FALSE) +
    geom_text(aes(x = ci_high + label_offset, label = value_label),
              hjust = 0, size = 2.15, colour = "#2A2A2A", family = "Arial",
              show.legend = FALSE) +
    scale_colour_manual(values = profile_colors, guide = "none") +
    scale_shape_manual(values = profile_shapes, guide = "none") +
    scale_y_discrete(labels = short_labels) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits,
                       expand = expansion(mult = c(0, 0))) +
    labs(title = title, subtitle = subtitle, x = "Mean (95% CI)", y = NULL) +
    theme_nature() +
    theme(
      legend.position = "none",
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 6.1, lineheight = 0.88, hjust = 1),
      plot.margin = margin(7, 8, 6, 10)
    )
}

p_b <- make_outcome_panel(
  "b",
  "Smartphone addiction",
  "",
  c(29.15, 33.50),
  c(30, 31, 32, 33)
)

p_c <- make_outcome_panel(
  "c",
  "Anxiety",
  "",
  c(6.80, 10.60),
  c(7, 8, 9, 10)
)

layout_design <- "
AB
AC
"

fig1 <- p_a + p_b + p_c +
  plot_layout(design = layout_design, widths = c(1.22, 1.0),
              heights = c(1, 1), guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

width_mm <- 183
height_mm <- 122
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite("Fig1.svg", width = width_in, height = height_in,
                 bg = "white", system_fonts = list(sans = "Arial"))
print(fig1)
dev.off()

ragg::agg_tiff("Fig1.tiff", width = width_in, height = height_in,
               units = "in", res = 600, background = "white", compression = "lzw")
print(fig1)
dev.off()

ragg::agg_png("Fig1_preview.png", width = width_in, height = height_in,
              units = "in", res = 300, background = "white")
print(fig1)
dev.off()

message("Rendered Fig. 1 at ", width_mm, " x ", height_mm, " mm.")
