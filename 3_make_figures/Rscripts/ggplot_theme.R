## ggplot2 settings and helpers

library(ggplot2)
theme_set(theme_bw())
theme_update(strip.background = element_blank(),
             panel.border = element_rect(colour = "black", fill=NA, size=.2),
             # axis.title.x = element_text(),
             axis.title.y = element_text(angle = 90, vjust=.5))
library(gridExtra)
library(viridis)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(colorspace)

geom_world <- function(color="black",...){
    map_data("world") %>%
        mutate(lon=long) %>%
        geom_path(mapping=aes(x=lon, y=lat, group=group, fill=NULL), color=color, ...)
}

barheight <- 6
barwidth <- .8
