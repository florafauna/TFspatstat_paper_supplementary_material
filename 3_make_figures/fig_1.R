## generate Fig. 1 of manuscript

rm(list=ls())
source("Rscripts/ggplot_theme.R")
library(reticulate); np <- import("numpy")


test_y <- np$load("../1_common_data/npy/test_y.npy")
train_y <- np$load("../1_common_data/npy/training_201_200_y.npy")
train_df <- np$load("../1_common_data/npy/training_201_200_df.npy")

aa <- rbind(data.frame(logLambda=train_y[,1], theta=train_y[,2],
                       df=rep(train_df, 20), type="train parameters /\nML grid search"),
            data.frame(logLambda=test_y[,1], theta=test_y[,2], df=NA,
                       type="test parameters"))
aa %>% filter(!is.na(df),
              df %in% unique(df)[c(TRUE,rep(FALSE,99))],
              theta==max(theta)) -> aa2

aa %>% filter(!is.na(df),
              round(df) %in% c(40,216),
              theta==max(theta)) -> aa3

aa$type <- factor(aa$type, c("train parameters /\nML grid search", "test parameters"))


dir.create("figs", showWarnings=FALSE)
png("figs/fig_1.png", width=7, height=3.5, units="in", res=600)
ggplot(aa, mapping=aes(x=logLambda, y=theta, color=type, size=type)) +
    geom_point(shape=19) +
    scale_size_manual(values=.01*c(.1, 1)) +
    scale_color_manual(values=c("gray40", "pink")) +
    labs(color="") +
    guides(size = FALSE, color = guide_legend(override.aes = list(size = 4, shape=19))) +
    geom_text(data=aa2, mapping=aes(label=round(df),size=NULL), nudge_y=2, color="black") +
    geom_text(data=aa3, mapping=aes(label=round(df),size=NULL), nudge_y=2, color=lighten("pink", -.4)) +
    xlab(expression(log(lambda)))+
    ylab(expression(theta)) +
    theme(axis.title.y = element_text(angle=0, vjust=.5)) + 
    annotate("text", x = -15.7, y = 52, label = "df:", color="black")
dev.off()
