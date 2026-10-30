options(warn = 1)

local_lib <- Sys.getenv("NATURE_FIGURE_RLIB", unset = "")
if (nzchar(local_lib)) .libPaths(c(local_lib, .libPaths()))

required_packages <- c("ggplot2", "patchwork", "svglite", "ragg")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0) {
  stop(
    "Missing R packages: ", paste(missing_packages, collapse = ", "),
    ". Install them before rendering this figure."
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

source_file <- "Fig2_source_data.csv"
stopifnot(file.exists(source_file))
dat <- read.csv(source_file, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(
  all(c("slope", "prediction", "contrast") %in% dat$record_type),
  all(is.finite(dat$estimate[dat$record_type != "contrast"]))
)

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
  "High Activity-High Resilience" = "High Activity-High Resilience",
  "Low Activity-Low Resilience" = "Low Activity-Low Resilience",
  "Light Activity-Moderate Resilience" = "Light Activity-Moderate Resilience"
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
      axis.title = element_text(size = 7.2, colour = "#1F1F1F"),
      axis.text = element_text(size = 6.6, colour = "#1F1F1F"),
      plot.title = element_text(size = 7.6, face = "bold", hjust = 0,
                                margin = margin(b = 3)),
      plot.subtitle = element_text(size = 6.2, colour = "#4A4A4A", hjust = 0,
                                   lineheight = 1.05, margin = margin(b = 5)),
      plot.tag = element_text(size = 8.5, face = "bold", colour = "#111111"),
      plot.tag.position = c(0, 1),
      plot.margin = margin(7, 7, 6, 7),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 6.1, colour = "#1F1F1F"),
      legend.key.width = grid::unit(7, "mm"),
      legend.key.height = grid::unit(3.4, "mm"),
      legend.spacing.x = grid::unit(2.0, "mm"),
      legend.margin = margin(t = 2),
      panel.grid = element_blank()
    )
}

slope_dat <- dat[dat$record_type == "slope", , drop = FALSE]
slope_dat$profile <- factor(slope_dat$profile, levels = rev(profile_levels))
slope_dat$value_label <- sprintf("%.3f (%.3f-%.3f)",
                                 slope_dat$estimate, slope_dat$ci_low, slope_dat$ci_high)

p_a <- ggplot(slope_dat, aes(y = profile, x = estimate, colour = profile, shape = profile)) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "#777777") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.18,
                linewidth = 0.58, show.legend = FALSE) +
  geom_point(size = 2.35, stroke = 0.25, show.legend = FALSE) +
  geom_text(aes(label = value_label), hjust = 0.5, vjust = -1.25, size = 2.15,
            family = "Arial", colour = "#252525", show.legend = FALSE) +
  scale_colour_manual(values = profile_colors, guide = "none") +
  scale_shape_manual(values = profile_shapes, guide = "none") +
  scale_y_discrete(labels = short_labels) +
  scale_x_continuous(breaks = seq(0, 0.4, 0.1), limits = c(0, 0.405),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Profile-specific association",
    x = "Adjusted unstandardized slopes (95% CI)",
    y = NULL
  ) +
  theme_nature() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 6.1, lineheight = 0.88, hjust = 1),
    plot.margin = margin(7, 10, 6, 5)
  )

pred_dat <- dat[dat$record_type == "prediction", , drop = FALSE]
pred_dat$profile <- factor(pred_dat$profile, levels = profile_levels)

p_b <- ggplot(
  pred_dat,
  aes(x = x, y = estimate, colour = profile, fill = profile, linetype = profile)
) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.10, colour = NA,
              show.legend = FALSE) +
  geom_line(linewidth = 0.82) +
  scale_colour_manual(values = profile_colors, breaks = profile_levels,
                      labels = legend_labels) +
  scale_fill_manual(values = profile_colors, breaks = profile_levels,
                    labels = legend_labels) +
  scale_linetype_manual(values = profile_linetypes, breaks = profile_levels,
                        labels = legend_labels) +
  scale_x_continuous(breaks = c(13, 20, 30, 40, 48), limits = c(13, 48),
                     expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(breaks = seq(2, 16, 2), limits = c(2, 16),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Covariate-standardized predictions (95% CI)",
    x = "Smartphone addiction score",
    y = "Predicted anxiety score"
  ) +
  theme_nature() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(7, 6, 6, 9)
  )

fig2 <- p_a + p_b +
  plot_layout(widths = c(1.02, 1.18), guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

width_mm <- 183
height_mm <- 90
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite("Fig2.svg", width = width_in, height = height_in,
                 bg = "white", system_fonts = list(sans = "Arial"))
print(fig2)
dev.off()

ragg::agg_tiff("Fig2.tiff", width = width_in, height = height_in,
               units = "in", res = 600, background = "white", compression = "lzw")
print(fig2)
dev.off()

ragg::agg_png("Fig2_preview.png", width = width_in, height = height_in,
              units = "in", res = 300, background = "white")
print(fig2)
dev.off()

message("Rendered Fig. 2 at ", width_mm, " x ", height_mm, " mm.")
