## generate Fig. 4 and 5 of manuscript
rm(list=ls())
source("Rscripts/ggplot_theme.R")
library(ggh4x)
library(FNN)
library(reticulate); np <- import("numpy")
load("../1_common_data/rda/slope.rda")
load("../2_model_ML_ML30/rda/slope_mle.rda")
load("../2_model_NV30/rda/slope_mvg.rda")
train_y <- np$load("../1_common_data/npy/training_201_200_y.npy")
test_y <- np$load("../1_common_data/npy/test_y.npy")

dir.create("figs", showWarnings=FALSE)

logLambda_mle <- slope_mle[,1]
logTheta_mle <- slope_mle[,2]
theta_mle <- exp(slope_mle[,2])

logLambda_mvg <- slope_mvg[,1]
theta_mvg <- slope_mvg[,2]
logTheta_mvg <- log(theta_mvg)

longi <- lon[1:(128-15) + 8]
lati <- lat[1:(128-15) + 8]

pixel_width <- diff(range(longi)) / (length(longi)-1)
pixel_height <- diff(range(lati)) / (length(lati)-1)
map_layers <- list(coord_cartesian(xlim = range(longi) + c(-1,1)*pixel_width/2,
                                   ylim = range(lati) + c(-1,1)*pixel_height/2,
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
barheight <- 6
barwidth <- .8


## clip to train grid for better comparison
max_dist_to_test_grid <- .8
logLambda_mle_test <- slope_mle[,1]
theta_mle_test <- exp(slope_mle[,2])

myscale <- function(test_y){
    test_y_mean <- apply(test_y, 2, mean, na.rm=TRUE)
    test_y_sd <- apply(test_y, 2, sd, na.rm=TRUE)
    function(x){
        o <- cbind((x[,1]-test_y_mean[1])/test_y_sd[1],
        (x[,2]-test_y_mean[2])/test_y_sd[2])
        o[!apply(is.na(o), 1, any),]
    }
}
myscale <- myscale(test_y=test_y)
project_to_test_y_mle <- get.knnx(myscale(test_y),
                                  myscale(cbind(logLambda_mle_test, theta_mle_test)), k=1)
logLambda_mle_test[project_to_test_y_mle$nn.dist>max_dist_to_test_grid] <- NA
theta_mle_test[project_to_test_y_mle$nn.dist>max_dist_to_test_grid] <- NA
logTheta_mle_test <- log(theta_mle_test)

project_to_test_y_mvg <- get.knnx(myscale(test_y), myscale(slope_mvg), k=1)
logLambda_mvg_test <- slope_mvg[,1]
theta_mvg_test <- slope_mvg[,2]
logLambda_mvg_test[project_to_test_y_mvg$nn.dist>max_dist_to_test_grid] <- NA
theta_mvg_test[project_to_test_y_mvg$nn.dist>max_dist_to_test_grid] <- NA
logTheta_mvg_test <- log(theta_mvg_test)




## fig. 4 ----------------------------------------------------------------
dtmp2 <- data.frame(logLambda=c(logLambda_mvg, logLambda_mle,
                                logLambda_mvg_test, logLambda_mle_test),
                    theta=c(theta_mvg, theta_mle,
                            theta_mvg_test, theta_mle_test),
                    model=rep(c("NV30", "ML30", "NV30", "ML30"),
                             c(length(theta_mvg), length(theta_mle),
                               length(theta_mvg_test), length(theta_mle_test))),
                    type=rep(c("raw", "clipped"),
                             c(length(theta_mvg)+ length(theta_mle),
                               length(theta_mvg_test)+ length(theta_mle_test)))) 
dtmp2$model <- factor(dtmp2$model,
                      levels = c("NV30", "ML30"),
                      labels=c("__________________________________ NV30 __________________________________",
                               "__________________________________ ML30 __________________________________"))
dtmp2$type <- factor(dtmp2$type, levels=c("raw","clipped"))

ggplot(data=data.frame(logLambda=train_y[,1], theta=train_y[,2]),
             mapping=aes(x=logLambda, y=theta)) +
    geom_point(shape=19, color='gray', size=.02) +
    geom_point(data=data.frame(logLambda=test_y[,1], theta=test_y[,2]),
               shape=19, color='lightpink', size=.02) +
    facet_nested(.~model + type, scales="free_x") +
    stat_binhex(data=dtmp2 , binwidth=c(1,2), aes(fill=log(..count..))) +
    scale_fill_viridis() +
    labs(fill = "log\ncounts") +
    xlab(expression(log(lambda))) + ylab(expression(theta)) +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust=.5)) -> p_resgrid


png("figs/fig_4.png", width=12, height=3.5, units="in", res=600)
p_resgrid
dev.off()



## fig. 5 ----------------------------------------------
data.frame(x=longi, y=rep(lati, each=length(longi)),
           z=c(logLambda_mle_test,logLambda_mvg_test),
           what=rep(c("ML30", "NV30"),
                    each=length(logLambda_mvg_test))) %>%
    ggplot(mapping=aes(x=x, y=y, fill=z)) +
    map_layers +
    scale_fill_gradientn(colors=viridis(256), na.value="gray") +
    guides(fill = guide_colorbar(title = expression(log(widehat(lambda))),
            barwidth = barwidth, barheight = barheight, label.position = "right", 
            legend.position = c(0, 0),  ticks.colour = "black",
            frame.colour="black")) -> p_geo_lambda


data.frame(x=longi, y=rep(lati, each=length(longi)),
           z=c(logTheta_mle_test,logTheta_mvg_test),
           what=rep(c("ML30", "NV30"),
                    each=length(logLambda_mvg_test))) %>%
    ggplot(mapping=aes(x=x, y=y, fill=z)) +
    map_layers + 
    scale_fill_gradientn(colors=viridis(256), na.value="gray") +
    guides(fill = guide_colorbar(title = expression(log(widehat(theta))),
            barwidth = barwidth, barheight = barheight, label.position = "right", 
            legend.position = c(0, 0),  ticks.colour = "black",
            frame.colour="black")) -> p_geo_theta

logLambda_maxdiff <- max(abs(logLambda_mvg_test-logLambda_mle_test), na.rm=TRUE)
data.frame(x=longi, y=rep(lati, each=length(longi)),
           mvg=logLambda_mvg_test, mle=logLambda_mle_test,
           what="difference: NV30 - ML30") %>%
    mutate(z = mvg-mle) %>%
    ggplot(mapping=aes(x=x, y=y, fill=z)) +
    map_layers + 
    scale_fill_stepsn(colors=rev(brewer.pal(5, "RdBu")), na.value="gray",
                      limits=c(-logLambda_maxdiff,logLambda_maxdiff),
                      breaks=c(seq(-2.7, 3, 1.8))) +
    guides(fill = guide_colorbar(title = expression(log(widehat(lambda))),
            barwidth = barwidth, barheight = barheight, label.position = "right", 
            legend.position = c(0, 0),  ticks.colour = "black",
            frame.colour="black")) -> p_geo_lambda_diff




logTheta_maxdiff <- max(abs(logTheta_mvg_test-logTheta_mle_test), na.rm=TRUE)
data.frame(x=longi, y=rep(lati, each=length(longi)),
           mvg=logTheta_mvg_test, mle=logTheta_mle_test,
           what="difference: NV30 - ML30") %>%
    mutate(z = mvg-mle) %>%
    ggplot(mapping=aes(x=x, y=y, fill=z)) +
    map_layers + 
    scale_fill_stepsn(colors=rev(brewer.pal(5, "RdBu")), na.value="gray",
                      limits=c(-logTheta_maxdiff,logTheta_maxdiff),
                      breaks=c(seq(-.6, .6, .4))) +
    guides(fill = guide_colorbar(title = expression(log(widehat(theta))),
            barwidth = barwidth, barheight = barheight, label.position = "right", 
            legend.position = c(0, 0),  ticks.colour = "black",
            frame.colour="black")) -> p_geo_theta_diff


scatter_lambda_range <- range(apply(cbind(logLambda_mle_test, logLambda_mvg_test), 1, range, na.rm=TRUE, finite=TRUE), na.rm=TRUE, finite=TRUE)

data.frame(x=logLambda_mle_test, y=logLambda_mvg_test, what="log(widehat(lambda))") %>%
    ggplot(mapping=aes(x=x, y=y)) +
    facet_wrap(what~., labeller = label_parsed) +
    geom_point(size=.5, alpha=.1) +
    geom_abline(slope=1, intercept=0, color="green") +
    coord_fixed(xlim=scatter_lambda_range, ylim=scatter_lambda_range) +
    xlab("ML30") +
    ylab("NV30") +
    theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=.5)) -> p_scatter_lambda



scatter_theta_range <- range(apply(cbind(logTheta_mle_test, logTheta_mvg_test), 1, range, na.rm=TRUE, finite=TRUE), na.rm=TRUE, finite=TRUE)
data.frame(x=logTheta_mle_test, y=logTheta_mvg_test, what="log(widehat(theta))") %>%
    ggplot(mapping=aes(x=x, y=y)) +
    facet_wrap(what~., labeller = label_parsed) +
    geom_point(size=.5, alpha=.1) +
    geom_abline(slope=1, intercept=0, color="green") +
    coord_fixed(xlim=scatter_theta_range, ylim=scatter_theta_range) +
    xlab("ML30") +
    ylab("NV30")  +
    theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=.5)) -> p_scatter_theta


png("figs/fig_5.png", width=12, height=5.7, units="in", res=600)
grid.arrange(grid.arrange(p_geo_lambda +
                          theme(plot.margin = theme_get()$plot.margin*c(1,1,0,1)),
                          p_geo_lambda_diff +
                          theme(plot.margin = theme_get()$plot.margin*c(1,1,0,1)),
                          p_scatter_lambda +
                          theme(plot.margin = theme_get()$plot.margin*c(1,1,0,1)),
                          nrow=1, widths=c(1.71,1,.8), newpage=FALSE),
             grid.arrange(p_geo_theta +
                          theme(strip.text = element_text(color="white"),
                                plot.margin = theme_get()$plot.margin*c(0,1,1,1)),
                          p_geo_theta_diff +
                          theme(strip.text = element_text(color="white"),
                                plot.margin = theme_get()$plot.margin*c(0,1,1,1)),
                          p_scatter_theta +
                          theme(plot.margin = theme_get()$plot.margin*c(0,1,1,1)),
                          nrow=1, widths=c(1.71,1,.8), newpage=FALSE), nrow=2)
dev.off()



