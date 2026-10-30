options(warn = 1)

local_lib <- Sys.getenv("NATURE_FIGURE_RLIB", unset = "")
if (nzchar(local_lib)) .libPaths(c(local_lib, .libPaths()))

required_packages <- c("ggplot2", "patchwork", "svglite", "ragg")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

source_file <- "Fig3_source_data.csv"
stopifnot(file.exists(source_file))
dat <- read.csv(source_file, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(
  all(c("profile_mean", "slope", "prediction", "observation") %in% dat$record_type),
  sum(dat$record_type == "observation") == 1928L
)

profile_levels <- c(
  "Low LIPA-Low Resilience",
  "MVPA-Oriented-Low Resilience",
  "Broadly Active-Average Resilience",
  "LIPA-Oriented-Average Resilience"
)
profile_colors <- c(
  "Low LIPA-Low Resilience" = "#C5544D",
  "MVPA-Oriented-Low Resilience" = "#C58B3E",
  "Broadly Active-Average Resilience" = "#2F6F9F",
  "LIPA-Oriented-Average Resilience" = "#6F7F79"
)
profile_shapes <- c(
  "Low LIPA-Low Resilience" = 17,
  "MVPA-Oriented-Low Resilience" = 18,
  "Broadly Active-Average Resilience" = 16,
  "LIPA-Oriented-Average Resilience" = 15
)
profile_linetypes <- c(
  "Low LIPA-Low Resilience" = "longdash",
  "MVPA-Oriented-Low Resilience" = "dotdash",
  "Broadly Active-Average Resilience" = "solid",
  "LIPA-Oriented-Average Resilience" = "dotted"
)

profile_summary <- unique(dat[dat$record_type == "slope", c("profile", "n", "percent")])
profile_summary <- profile_summary[match(profile_levels, profile_summary$profile), ]
legend_labels <- setNames(
  sprintf(
    "%s (n = %s; %.1f%%)",
    profile_summary$profile,
    trimws(format(profile_summary$n, big.mark = ",", scientific = FALSE)),
    profile_summary$percent
  ),
  profile_summary$profile
)

theme_nature <- function(base_size = 7.2, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      axis.line = element_line(linewidth = 0.35, colour = "#1F1F1F"),
      axis.ticks = element_line(linewidth = 0.35, colour = "#1F1F1F"),
      axis.ticks.length = grid::unit(1.4, "mm"),
      axis.title = element_text(size = 8, colour = "#1F1F1F"),
      axis.text = element_text(size = 7.2, colour = "#1F1F1F"),
      plot.title = element_text(size = 8.6, face = "bold", hjust = 0,
                                margin = margin(b = 3)),
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

mean_dat <- dat[dat$record_type == "profile_mean", , drop = FALSE]
mean_dat$profile <- factor(mean_dat$profile, levels = profile_levels)
mean_dat$variable <- factor(
  mean_dat$variable,
  levels = c("Psychological resilience", "MVPA", "LIPA"),
  labels = c("Psychological\nresilience", "MVPA", "LIPA")
)

p_a <- ggplot(
  mean_dat,
  aes(
    x = variable, y = estimate, group = profile,
    colour = profile, shape = profile, linetype = profile
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.38, colour = "#777777") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.055,
                linewidth = 0.46, show.legend = FALSE) +
  geom_line(linewidth = 0.78) +
  geom_point(size = 2.15, stroke = 0.25) +
  scale_colour_manual(values = profile_colors, breaks = profile_levels,
                      labels = legend_labels) +
  scale_shape_manual(values = profile_shapes, breaks = profile_levels,
                     labels = legend_labels) +
  scale_linetype_manual(values = profile_linetypes, breaks = profile_levels,
                        labels = legend_labels) +
  scale_y_continuous(breaks = seq(-2.5, 0.5, 0.5), limits = c(-2.72, 0.82),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Student-athlete mind-body profiles",
    x = NULL,
    y = "Estimated mean (z score)"
  ) +
  theme_nature() +
  theme(
    axis.text.x = element_text(size = 6.1, lineheight = 0.92),
    legend.position = "bottom",
    plot.margin = margin(7, 9, 6, 5)
  )

slope_dat <- dat[dat$record_type == "slope", , drop = FALSE]
slope_dat$profile <- factor(slope_dat$profile, levels = rev(profile_levels))
slope_dat$value_label <- sprintf(
  "%.3f (%.3f-%.3f)", slope_dat$estimate, slope_dat$ci_low, slope_dat$ci_high
)
slope_axis_labels <- c(
  "Low LIPA-Low Resilience" = "Low LIPA\nLow Resilience",
  "MVPA-Oriented-Low Resilience" = "MVPA-Oriented\nLow Resilience",
  "Broadly Active-Average Resilience" = "Broadly Active\nAverage Resilience",
  "LIPA-Oriented-Average Resilience" = "LIPA-Oriented\nAverage Resilience"
)

p_b <- ggplot(slope_dat, aes(y = profile, x = estimate, colour = profile, shape = profile)) +
  geom_vline(xintercept = 0, linewidth = 0.35, linetype = "dashed", colour = "#777777") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), orientation = "y", width = 0.18,
                linewidth = 0.55, show.legend = FALSE) +
  geom_point(size = 2.25, stroke = 0.25, show.legend = FALSE) +
  geom_text(aes(label = value_label), hjust = 0.5, vjust = -1.25, size = 2.02,
            family = "Arial", colour = "#252525", show.legend = FALSE) +
  scale_colour_manual(values = profile_colors, guide = "none") +
  scale_shape_manual(values = profile_shapes, guide = "none") +
  scale_y_discrete(labels = slope_axis_labels) +
  scale_x_continuous(breaks = seq(0, 0.16, 0.04), limits = c(0, 0.18),
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
    axis.text.y = element_text(size = 5.7, lineheight = 0.90),
    plot.margin = margin(7, 6, 5, 7)
  )

obs_dat <- dat[dat$record_type == "observation", , drop = FALSE]
pred_dat <- dat[dat$record_type == "prediction", , drop = FALSE]
obs_dat$profile <- factor(obs_dat$profile, levels = profile_levels)
pred_dat$profile <- factor(pred_dat$profile, levels = profile_levels)

p_c <- ggplot() +
  geom_point(
    data = obs_dat,
    aes(x = x, y = observed_y, colour = profile),
    size = 0.42, alpha = 0.14,
    position = position_jitter(width = 0.08, height = 0.08, seed = 20260901),
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = pred_dat,
    aes(x = x, ymin = ci_low, ymax = ci_high, fill = profile),
    alpha = 0.10, colour = NA, show.legend = FALSE
  ) +
  geom_line(
    data = pred_dat,
    aes(x = x, y = estimate, colour = profile, linetype = profile),
    linewidth = 0.78, show.legend = FALSE
  ) +
  annotate(
    "text", x = 0.5, y = 10.5, label = "Anxiety score",
    angle = 90, size = 8 / 2.845276, family = "Arial"
  ) +
  scale_colour_manual(values = profile_colors) +
  scale_fill_manual(values = profile_colors) +
  scale_linetype_manual(values = profile_linetypes) +
  scale_x_continuous(breaks = c(5, 15, 25, 35, 45, 55),
                     expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20),
                     expand = expansion(mult = c(0, 0))) +
  coord_cartesian(
    xlim = c(5, 56.28), ylim = c(-0.28, 21.28),
    expand = FALSE, clip = "off"
  ) +
  labs(
    title = "Observed distribution and adjusted trends",
    x = "Smartphone addiction score",
    y = NULL
  ) +
  theme_nature() +
  theme(plot.margin = margin(5, 6, 6, 7))

# The panel-c y-axis title is drawn in data coordinates so it remains
# attached to its own axis when patchwork aligns this column with panel b.
right_column <- p_b / p_c + plot_layout(heights = c(0.95, 1.05))
fig3 <- p_a | right_column
fig3 <- fig3 +
  plot_layout(widths = c(1.02, 1.28), guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "center"
  ) &
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(alpha = 1)),
    shape = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

width_mm <- 183
height_mm <- 112
width_in <- width_mm / 25.4
height_in <- height_mm / 25.4

svglite::svglite("Fig3.svg", width = width_in, height = height_in,
                 bg = "white", system_fonts = list(sans = "Arial"))
print(fig3)
dev.off()

ragg::agg_tiff("Fig3.tiff", width = width_in, height = height_in,
               units = "in", res = 600, background = "white", compression = "lzw")
print(fig3)
dev.off()

ragg::agg_png("Fig3_preview.png", width = width_in, height = height_in,
              units = "in", res = 300, background = "white")
print(fig3)
dev.off()

message("Rendered Fig. 3 at ", width_mm, " × ", height_mm, " mm.")
