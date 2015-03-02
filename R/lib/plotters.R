#
# A theme for CS STAT charts and a function to build it
#
# Important: Either use the function here, buildChart, to cut up the chart elements are arrange them properly, or include three line breaks ('\n\n\n') in the chart title of your ggplot call
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

library("grid")
library("gridExtra")
library("gtable")

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
    legend.position = c(0,1),
    legend.direction = "horizontal",
    legend.justification = c(0.03, 0),
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
    plot.title = element_text(size = rel(1.2), hjust = 0),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    complete = TRUE)
}

buildChart <- function(p) {

  gt <- ggplot_gtable(ggplot_build(p))

  t <- gtable_filter(gt, "title")[[1]]

  l <- gtable_filter(gt, "guide-box")[[1]]

  p <- p + theme(legend.position = "none", plot.title = element_blank())

  built <- grid.arrange(
             arrangeGrob(
              t[[1]],
              l[[1]],
              p,
              nrow = 3,
              heights = c(1, 1.2, 10)
             )
           )

  return(built)
}

lineOPA <- function(data, x, y, title = "Need a title", xlab = "Month", ylab = "Need axis label", group = 1, ...) {
  args <- eval(substitute(alist(...)))

  base <- ggplot(data, aes_string(x = x, y = y, group = group, colour = group)) +
            geom_line(size = 1) +
            labs(title = title, x = xlab, y = ylab) +
            scale_colour_brewer(palette = "Blues")

  if( !is.null(args$labels) ) {
    base <- base + geom_text(size = 4, colour = "grey33", vjust = -.5, aes_string(label = args$labels, y = y))
  }

  return(base)
}
