theme_set(theme_classic())
theme_update(
  plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
  panel.background = element_rect(fill = "#ffffff", color = NA),
  panel.grid.major.x = element_line(colour="#e3dfda"),
  # panel.grid.minor = element_blank(),
  axis.text.x = element_text(face = "bold", color = "black", size = 10, angle = 0, hjust = 1),
  axis.text.y = element_text(face = "plain", color = "black", size = 10, angle = 0, hjust = 1),
  axis.ticks = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_text(face = "bold", color = "black", size = 11, angle = 0, vjust = -2),
  plot.title = element_text(size = 13, face = "bold", hjust = .5),
  strip.text = element_text(face = "bold", color = "black", size = 12), #--facet_wrap theme
  strip.background = element_blank(),
  panel.spacing.x=unit(.7, "lines"),
  panel.spacing.y=unit(.7, "lines"),
  legend.text=element_text(size=10, face="plain"),
  legend.title = element_text(size=10,face="bold"),
  legend.position="none",
  legend.background = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  plot.caption = element_text(
    family = "Arial",
    size = 12,
    color = "grey70",
    face = "bold",
    hjust = .5,
    margin = margin(5, 0, 20, 0)
  ),
  plot.margin = margin(rep(15, 4))
)
