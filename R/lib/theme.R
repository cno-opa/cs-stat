#
# A theme for CS STAT charts
#
#
# This is what is changed:
# ==============================
#
#
# theme_opa <- function() {
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = .97),
#     axis.line = element_line(colour = "grey50"),
#     axis.line.y = element_blank(),
#     panel.background = element_rect(colour = NA, fill = "white"),
#     panel.border = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.major.y = element_line(colour = "grey50"),
#     legend.key = element_rect(colour = NA, fill = "white"),
#     legend.position = "top",
#     complete = FALSE
#     )
# }
#
#
# And here is the default gray theme, with changes made, so theme is complete and can be set by theme_set(opa_theme())
# ===============================

#load 'grid' package

library("grid")

theme_opa <- function (base_size = 12, base_family = "")
{
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),

    axis.text = element_text(size = rel(0.8), colour = "grey50"),
    strip.text = element_text(size = rel(0.8)),
    axis.line = element_line(colour = "grey50"),
    axis.line.y = element_blank(),
    axis.text.x = element_text(vjust = 1, angle = 45, hjust = .97),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_line(colour = "grey50"),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(colour = NA, fill = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.8), face = "bold", hjust = 0),
    legend.title.align = NULL,
    legend.position = "top",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey50"),
    panel.grid.minor = element_blank(),
    panel.margin = unit(0.25, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,

    strip.background = element_rect(fill = "grey80", colour = NA),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2)),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), complete = TRUE)
}
