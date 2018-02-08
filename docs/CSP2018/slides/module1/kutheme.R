library(extrafont)
loadfonts()
library(ggplot2)
library(ggthemr)


cLAUs <- define_palette(swatch = c('#eeeeee', # White
                                   "#8282ff", # Blue b9b9ff
                                   "#ff7979", # Red
                                   '#f2f200', # Yellow
                                   "#0bff06", # Green
                                   "#eca8e5", # Magenta
                                   "#ffad5b"  # Brownish
                                  ),
                         gradient = c(lower = 'red', upper = 'green'),
                         background = "transparent",
                         text = c("#eeeeee", "#eeeeee"),
                         line = c("#ee1e1e", "#6e6e6e"), # Axis lines
                         gridline = "#434343") # Gridlines

ggthemr(cLAUs, layout="scientific", type = 'outer', spacing=1.2)



theme_xkcd <- function() {
    theme(text=element_text(size=28, family="Unmasked BB"),   # Humor Sans
          panel.background = element_rect(fill = "transparent", colour = NA), # bg of the panel
          plot.background = element_rect(fill = "transparent", colour = NA) # bg of the plot
                                        # change stuff here
         )
}
