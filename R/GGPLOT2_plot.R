# Plotting data in ggplot2
library(ggplot2)

# Inputs
save_figure <- T
plot_title <- bquote('COPS vs OLI')
legend_title <- ""
legend_position <- "right"
xmin <- 0; xmax <- 0.0032;  xstp <- 0.001; xlbl <- bquote('COPS '*R[rs]* '('*sr^{-1}*')')
ymin <- 0; ymax <- 0.0045; ystp <- 0.001; ylbl <- bquote('OLI '*R[rs]* '('*sr^{-1}*')')
vnames <- c("B1", "B2", "B3", "B4")
vcolor <- rainbow(length(vnames))
filename <- "COPSvsOLI"

# Making aspect ratio of the figure to be 1 (square)
asp_rat <- (xmax-xmin)/(ymax-ymin)

# Creating tif file is we need to save the figure
if (save_figure)
{  if (!(grepl(".tif", filename, fixed = TRUE)))
  filename <- paste0(filename,".tif")
tiff(filename, width = 17, height = 17, units = "cm", res = 300, compression = "lzw")
}

# Plotting the data
g <- ggplot(DF) +
  geom_point(aes(x = DF$aer, y = DF$Rrs_443, colour ='B1'), na.rm = FALSE, show.legend = TRUE) +
  geom_point(aes(x = DF$blue, y = DF$Rrs_482, colour ='B2'), na.rm = FALSE, show.legend = TRUE) +
  geom_point(aes(x = DF$green, y = DF$Rrs_561, colour ='B3'), na.rm = FALSE, show.legend = TRUE) +
  geom_point(aes(x = DF$red, y = DF$Rrs_655, colour ='B4'), na.rm = FALSE, show.legend = TRUE) +
  labs(title = plot_title, color = legend_title) +
  scale_colour_manual(labels = vnames, values = vcolor) +
  coord_fixed(ratio = asp_rat, xlim = c(xmin, xmax),
              ylim = c(ymin, ymax), expand = FALSE, clip = "on") +
  scale_x_continuous(name = xlbl, limits = c(xmin, xmax),
                     breaks = seq(xmin, xmax, xstp))  +
  scale_y_continuous(name = ylbl, limits = c(ymin, ymax),
                     breaks = seq(ymin, ymax, ystp))  +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 15, color = 'black', angle = 0),
        axis.text.y = element_text(size = 15, color = 'black', angle = 0),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.ticks.length = unit(.25, "cm"),
        legend.position = legend_position,
        legend.direction = "vertical",
        legend.title = element_text(colour = "black", size = 15, face = "plain"),
        legend.text = element_text(colour = "black", size = 15, face = "plain"),
        legend.background = element_rect(fill = NA, size = 0.5,
                                         linetype = "solid", colour = 0),
        legend.key = element_blank(),
        legend.justification = c("left", "top"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey",
                                        size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.5))
g

# Saving the tiff file, if we want to save figure
if (save_figure)
  dev.off()
