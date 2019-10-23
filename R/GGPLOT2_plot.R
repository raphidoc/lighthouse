# Plotting data in ggplot2
library(ggplot2)

# Inputs
save_figure <- FALSE
plot_title <- bquote('COPS vs OLI')
legend_title <- ""
legend_position <- c(0.75, 1)
xmin <- 400; xmax <- 800;  xstp <- 100; xlbl <- bquote('Wavelength, ' *lambda*' (nm)')
ymin <- 0; ymax <- 0.01; ystp <- 0.002; ylbl <- bquote(''*R[rs]* '('*sr^{-1}*')')
vnames <- c("OLI", "COPS")
vcolor <- rainbow(length(vnames))
filename <- "PARvsTheta"

# Making aspect ratio of the figure to be 1 (square)
asp_rat <- (xmax-xmin)/(ymax-ymin)

# Creating tif file is we need to save the figure
if (save_figure)
{  if (!(grepl(".tif", filename, fixed = TRUE)))
  filename <- paste0(filename,".tif")
tiff(filename, width = 17, height = 17, units = "cm", res = 300, compression = "lzw")
}

# Plotting the data
g <- ggplot(dfnew, aes(x = lambda)) +
  geom_line(aes(y = dfnew$Station_002, colour ='y1'), na.rm = FALSE, show.legend = TRUE) +
  geom_line(aes(y = DFcops$Station_002, colour ='y2'), na.rm = FALSE, show.legend = TRUE) +
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
