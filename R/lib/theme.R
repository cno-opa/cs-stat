#
# A theme for CS STAT charts
#

theme_opa <- function() {
  theme(
    axis.text.x = element_text(angle = 45, hjust = .97),
    axis.line = element_line(colour = "grey50"),
    axis.line.y = element_blank(),
    panel.background = element_rect(colour = NA, fill = "white"),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey50"),
    legend.key = element_rect(colour = NA, fill = "white"),
    complete = FALSE
    )
}
