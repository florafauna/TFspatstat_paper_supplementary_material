## generate Fig. 3 of manuscript

rm(list=ls())
source("Rscripts/ggplot_theme.R")
library(fields)
library(FNN)
library(reticulate); np <- import("numpy")

load("../1_common_data/rda/slope.rda")


pixel_width <- diff(range(lon)) / (length(lon)-1)
pixel_height <- diff(range(lat)) / (length(lat)-1)

map_layers <- list(coord_cartesian(xlim = range(lon) + c(-1,1)*pixel_width/2,
                                   ylim = range(lat) + c(-1,1)*pixel_height/2,
                                   expand = FALSE),
                   geom_tile(),
                   geom_world(),
                   theme(axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.y=element_blank()),
                   facet_wrap(what ~. ))



rect1 <- c(45, 95)
rect2 <- c(95, 65)

data.frame(x=lon, y=rep(lat, each=length(lon)), z=c(slope_x[,,1]),
           what = "slope (1 of 30 replicates)") %>%
    ggplot(mapping=aes(x=x, y=y, fill=z)) +
    map_layers +
    geom_rect(xmin=lon[rect1[1]]-.7, xmax=lon[rect1[1]+15]+.7,
              ymin=lat[rect1[2]]-.5, ymax=lat[rect1[2]+15]+.5,
              fill=NA, color="black") + 
    geom_rect(xmin=lon[rect1[1]]-.7, xmax=lon[rect1[1]+15]+.7,
              ymin=lat[rect1[2]]-.5, ymax=lat[rect1[2]+15]+.5,
              fill=NA, color="violetred1", linetype="bb") + 
    geom_rect(xmin=lon[rect2[1]]-.7, xmax=lon[rect2[1]+15]+.7,
              ymin=lat[rect2[2]]-.5, ymax=lat[rect2[2]+15]+.5,
              fill=NA, color="black") + 
    geom_rect(xmin=lon[rect2[1]]-.7, xmax=lon[rect2[1]+15]+.7,
              ymin=lat[rect2[2]]-.5, ymax=lat[rect2[2]+15]+.5, fill=NA,
              color="orange", linetype="bb") + 
    scale_fill_gradientn(colors=viridis(256)) +
    guides(fill = guide_colorbar(title = "",
            barwidth = barwidth, barheight = barheight, label.position = "left", 
            legend.position = c(0, 0),  ticks.colour = "black",
            frame.colour="black"))+ theme(legend.position="left") +
    xlab(" ") -> map_s1


im1 <- slope_x[rect1[1]:(rect1[1]+15),rect1[2]:(rect1[2]+15),]
im2 <- slope_x[rect2[1]:(rect2[1]+15),rect2[2]:(rect2[2]+15),]

vg1 <- lapply(1:30, function(i) vgram.matrix(im1[,,i], R=16))
dvg1 <- data.frame(d=vg1[[1]]$d, vg=unlist(lapply(vg1, function(x) x$vgram)),
           id=rep(1:30, each=length(vg1[[1]]$d)), type="variograms for left subset")
vg2 <- lapply(1:30, function(i) vgram.matrix(im2[,,i], R=16))
dvg2 <- data.frame(d=vg2[[1]]$d, vg=unlist(lapply(vg2, function(x) x$vgram)),
           id=rep(1:30, each=length(vg2[[1]]$d)), type="variograms for right subset")

ggplot(rbind(dvg1, dvg2),
       aes(x=d, y=vg, group=interaction(id, type), color=type)) + 
    geom_point(alpha=1, size=.1) +
    geom_smooth(se=FALSE, size=.3, color="black") +
    facet_wrap(type~., ncol=2) +
    scale_color_manual(values=c("violetred1", "orange"), guide=FALSE) +
    coord_cartesian(ylim=c(0.1, 4.4)) +
    xlab("distance") +
    ylab("variogram") +
    theme(strip.background = element_blank()) -> plot_vgrams

dir.create("figs", showWarnings=FALSE)
png("figs/fig_3.png", width=10, height=3.4, units="in", res=600)
grid.arrange(map_s1+
             theme(plot.margin = theme_get()$plot.margin*c(1,1,3,1)),
             plot_vgrams +
             theme(plot.margin = theme_get()$plot.margin*c(1,1,1,5),
                   axis.title=element_text(size=8)),
             ncol=2, widths=c(1.2,2),
             padding = unit(10.5, "line"))
dev.off()
