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
# ===============================

library("grid")
library("gridExtra")
library("gtable")

theme_opa <- function (base_size = 14, base_family = "")
{
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),

    axis.text = element_text(size = rel(0.8), colour = "grey50"),
    strip.text = element_text(size = rel(0.8)),
    axis.line = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(vjust = 1, angle = 45, hjust = .97),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_blank(),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.1, "cm"),

    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(colour = NA, fill = "white"),
    legend.key.size = unit(.8, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.75)),
    legend.text.align = NULL,
    legend.title = element_blank(),
    legend.title.align = NULL,
    legend.position = c(0,1),
    legend.direction = "horizontal",
    legend.justification = c(-0.02, 0.8),
    legend.box = NULL,

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey40"),
    panel.grid.minor = element_blank(),
    panel.margin = unit(0.25, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,

    strip.background = element_rect(fill = "grey80", colour = NA),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), hjust = 0.05),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    complete = TRUE)
}

#aesthetic variables
lightBlue <- "#23b6ff"
darkBlue <- "#002537"
red <- "tomato"

buildChart <- function(p) {

  gt <- ggplot_gtable(ggplot_build(p))

    gtl <- gt[[1]]

  t <- gtable_filter(gt, "title")[[1]]

  p <- p + theme(legend.position = "none", plot.title = element_blank())

  if( TRUE %in% grepl("guide-box", gtl) ) {
    l <- gtable_filter(gt, "guide-box")[[1]]

    built <- arrangeGrob(
              t[[1]],
              l[[1]],
              p,
              nrow = 3,
              heights = c(1, 1.2, 10)
             )
  } else {
    built <- arrangeGrob(
              t[[1]],
              p,
              nrow = 2,
              heights = c(1, 10)
             )
  }

  return(built)
}

lineOPA <- function(data, x, y, title = "Title!", group = 1, percent = FALSE, ...) {
  # set labels with `labels = "label_column"`
  # set highlight with `highlight = "group_to_highlight"`
  # set y axis label with `ylab = "label"`
  # set legend labels with `legend.labels = character vector`
  # percent = FALSE refers to whether or not y-axis should be in percent

  dots <- eval(substitute(alist(...)))

  #make continuous var type
  data[y] <- as.numeric(data[y][[1]])

  #get max y val
  ymax <- max(data[y], na.rm = TRUE)

  remap <- function(input, matcher, value) {
    matcher <- paste0(matcher, "(?! .)")
    i <- grep(matcher, names(input), perl = TRUE)
    input[i] <- value
    return(input)
  }

  if(group == 1) {
    blues <- darkBlue
  } else {
    blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[group])))
    names(blues) <- as.matrix(unique(data[group]))[,1]
  }

  if( !is.null(dots$highlight) ) {
    blues <- remap(blues, dots$highlight, red)
  }

  base <- ggplot(data, aes_string(x = x, y = y, group = group, colour = group)) +
          geom_line(size = 1.5) +
          labs(title = title, x = "", y = "")

  if( !is.null(dots$ylab) ) {
    base + labs(y = dots$ylab)
  }

  if(group == 1) {
    base <- base + geom_line(colour = blues) + guides(colour = FALSE)
  } else {
    base <- base + scale_colour_manual( values = blues )
  }

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_colour_manual( values = blues, labels = legend.labels )
  }

  if( !is.null(dots$labels) ) {
    base <- base +
            geom_text(size = 4, colour = "grey33", vjust = -.5, aes_string(label = dots$labels, y = y))
  }

  if(percent == FALSE) {
    brks <- pretty_breaks(4)(0:ymax)
    yul  <- brks[length(brks)]
    base <- base + scale_y_continuous(breaks = brks) + expand_limits(y = c(0, yul))
  } else {
    yul  <- ymax + 0.05
    incr <- yul/4
    brks <- seq(0, yul, incr)
    base <- base + scale_y_continuous(breaks = brks, labels = percent(brks)) + expand_limits(y = c(0, yul))
  }

  return(base)
}

barOPA <- function(data, x, y, title = "Title", stat = "identity", position = "identity", ...) {
  # set fill with `fill = "variable"`
  # set labels with `labels = "label_column"`

  dots <- eval(substitute(alist(...)))

  base <- ggplot(data, aes_string(x = x, y = y, fill = dots$fill)) +
          geom_bar(stat = stat, position = position) +
          labs(title = title, y = "", x = "") +
          expand_limits(y = 0)

  if( !is.null(dots$ylab) ) {
    base + labs(y = dots$ylab)
  }

  if( !is.null(dots$labels) ) {
    base <- base +
            geom_text(size = 4, colour = "grey33", vjust = -.5, aes_string(label = dots$labels, y = y))
  }

  if( !is.null(dots$fill) ) {
    blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[dots$fill])))
    base <- base + geom_bar(stat = stat, position = position) + scale_fill_manual(values = blues)
  } else {
    base <- base + geom_bar(stat = stat, position = position, fill = darkBlue)
  }

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_fill_manual( values = blues, labels = legend.labels )
  }

  return(base)
}

schigoda <- function(data, x, y, fill, title = "Schigoda!", ...) {
  #
  #

  dots <- eval(substitute(alist(...)))

  blues <- colorRampPalette( c(darkBlue, lightBlue) )(nrow(unique(data[fill])))

  #area
  data[, x] <- as.Date(as.yearmon(data[, x]))
  base <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
          geom_area(position = "identity") +
          labs(title = title, x = "", y = "") +
          scale_fill_manual(values = blues) +
          scale_x_date(breaks = pretty_breaks(9)(data[, x]), labels = date_format("%b %Y"))

  if( !is.null(dots$legend.labels) ) {
    legend.labels <- eval(dots$legend.labels)
    base <- base + scale_fill_manual( values = blues, labels = legend.labels )
  }

  return(base)
}
