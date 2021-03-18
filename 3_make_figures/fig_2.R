## generate Fig. 2 of manuscript

rm(list=ls())
library(reticulate); np <- import("numpy")
source("Rscripts/ggplot_theme.R")
mae <- function(x, y) mean(abs(x-y))
rmspe <- function(x, y) sqrt(mean((x-y)^2))
lighten <- .8



## testObj with true values
test_y <- np$load("../1_common_data/npy/test_y.npy")

## 'test_mle_c' (channelwise MLE) and ' mleComb' combined MLE 
load("../2_model_ML_ML30/rda/test_mle.rda")
ml_pred_y <- aperm(test_mle_c, c(1,3,2)) # 10000, 16, 2
ml_pred_y[,,2] <- exp(ml_pred_y[,,2]) 
mc_pred_y <- test_mle
mc_pred_y[,2] <- exp(mc_pred_y[,2])

np <- import("numpy")
im_pred_y <- np$load("../2_model_NI_NI30/npy/test_pred_model_NI.npy")
vg_pred_y <- np$load("../2_model_NV/npy/test_pred_model_NV.npy")
vg_rep_pred_y <- np$load("../2_model_NV30/npy/test_pred_model_NV30.npy")

n <- dim(im_pred_y)[1]
m <- dim(im_pred_y)[2]



## comparison of one replicate case -------------------------

bg_colors <- brewer.pal(8, "Set1")[c(2,1)]
dd_box_lambda <- rbind(data.frame(true=test_y[,1], est=im_pred_y[,1,1],
                                  var="lambda", method="NF"),
                       data.frame(true=test_y[,1], est=vg_pred_y[,1,1],
                                  var="lambda", method="NV"),
                       data.frame(true=test_y[,1], est=ml_pred_y[,1,1],
                                  var="lambda", method="ML"))
dd_box_lambda$method <- factor(dd_box_lambda$method, levels=c("NF","NV", "ML"))
dd_box_lambda$true_bin <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "",
                   as.character(cut_interval(dd_box_lambda$true, 10))), ","),
                                        function(x) mean(as.numeric(x))))
lambda_color_bin <- sort(unique(unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "",
                   as.character(cut_interval(dd_box_lambda$true, 10))), ","),
                                        function(x) (as.numeric(x)[2])))))
dd_box_lambda$color_bin <- cut_interval(dd_box_lambda$true, 2)
dd_box_lambda %>%
   ggplot(aes(x=true_bin, y=est-true, group=true_bin)) +
    geom_rect(xmin=-Inf, xmax=lambda_color_bin[5], ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[1], lighten)) +
    geom_rect(xmin=lambda_color_bin[5], xmax=Inf, ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[2], lighten)) +
    geom_vline(xintercept = seq(-7.5,0,2.5), color="gray", size=.5) +
    geom_hline(yintercept = seq(-4,2,2), color="gray", size=.5) +
    scale_y_continuous(breaks = seq(-4,2,2)) +
    geom_boxplot(width=.5) +
    xlab(expression(log(lambda)~phantom(log(hat(lambda))))) +
    ylab(expression(log(hat(lambda))-log(lambda))) +
      facet_wrap(method~.) +
    geom_abline(intercept=0, slope=0, color="red", linetype=2) -> p_box_lambda

dd_box_theta <- rbind(
    data.frame(true=test_y[,2], est=im_pred_y[,1,2], var="theta", method="NF"),
    data.frame(true=test_y[,2], est=vg_pred_y[,1,2], var="theta", method="NV"),
    data.frame(true=test_y[,2], est=ml_pred_y[,1,2], var="theta", method="ML")) 
dd_box_theta$method <- factor(dd_box_theta$method, levels=c("NF","NV","ML"))
dd_box_theta$true_bin <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "", as.character(cut_interval(dd_box_theta$true, 10))), ","), function(x) mean(as.numeric(x))))
theta_color_bin <- sort(unique(unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "",
                   as.character(cut_interval(dd_box_theta$true, 10))), ","),
                   function(x) (as.numeric(x)[2])))))
dd_box_theta %>%
   ggplot(aes(x=true_bin, y=est-true, group=true_bin) ) +
    geom_rect(xmin=-Inf, xmax=theta_color_bin[5], ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[1], lighten)) +
    geom_rect(xmin=theta_color_bin[5], xmax=Inf, ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[2], lighten)) +
    geom_vline(xintercept = seq(5,25,5), color="gray", size=.5) +
    geom_hline(yintercept = seq(-10,20,10), color="gray", size=.5) +
    geom_boxplot() +
    xlab(expression(theta~phantom(hat(theta)))) +
    ylab(expression(hat(theta)-theta)) +
    facet_wrap(method~.) +
    geom_abline(intercept=0, slope=0, color="red", linetype=2) -> p_box_theta






dd_box_lambda$true_bin2 <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "", as.character(cut_interval(dd_box_lambda$true, 2))), ","), function(x) mean(as.numeric(x))))
dd_box_lambda$true_bin2_label <- factor(dd_box_lambda$true_bin2,
                                        levels=sort(unique(dd_box_lambda$true_bin2), decreasing=TRUE),
                                        labels=c("large", "small"))

dd_box_theta$true_bin2 <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "", as.character(cut_interval(dd_box_theta$true, 2))), ","), function(x) mean(as.numeric(x))))
dd_box_theta$true_bin2_label <- factor(dd_box_theta$true_bin2,
                                        levels=sort(unique(dd_box_theta$true_bin2), decreasing=TRUE),
                                        labels=c("large", "small"))


dd_box_lambda %>%
    mutate(diff = est - true) %>%
    group_by(method, var, true_bin2_label) %>%
    summarize(bias = mean(diff), variance = sd(diff)) %>%
    mutate(var = factor(var, levels=c("lambda", "theta"),
         labels=c(expression(log(lambda)),expression(theta))))  %>% 
    ggplot(mapping=aes(x=bias, y=variance, shape=method, label=method,
                       color=as.factor(true_bin2_label))) +
    facet_wrap(~var, scales="free", labeller = label_parsed) +
    geom_vline(xintercept=0, linetype=2, color="gray25") +
    geom_text(size=5, key_glyph = "point") +
    scale_color_manual(values=rev(bg_colors)) +
    scale_x_continuous(expand = expansion(mult = .15)) +
    scale_y_continuous(expand = expansion(mult = .15)) +
    xlab(expression(phantom(log(widehat(lambda)))~"bias"~phantom(log(widehat(lambda))))) +
    ylab(expression("standard deviation")) +
    theme(legend.position="none") -> p_bias_lambda

dd_box_theta %>%
    mutate(diff = est - true) %>%
    group_by(method, var, true_bin2_label) %>%
    summarize(bias = mean(diff), variance = sd(diff)) %>%
    mutate(var = factor(var, levels=c("lambda", "theta"),
           labels=c(expression(expression(log(lambda))),expression(theta)))) %>%
    ggplot(mapping=aes(x=bias, y=variance, shape=method, label=method,
                       color=as.factor(true_bin2_label))) +
    facet_wrap(~var, scales="free", labeller = label_parsed) +
    geom_vline(xintercept=0, linetype=2, color="gray25") +
    geom_text(size=5, key_glyph = "point") +
    scale_shape_manual(values=c(1,3,4)) +
    scale_color_manual(values=rev(bg_colors)) +
    scale_x_continuous(expand = expansion(mult = .15)) +
    scale_y_continuous(expand = expansion(mult = .15)) +
    xlab(expression(phantom(widegat(theta))~"bias"~phantom(widegat(theta)))) +
    ylab(expression("standard deviation")) +
    theme(legend.position="none") -> p_bias_theta

png("figs/fig_2a.png", width=11, height=6, units="in", res=600)
grid.arrange(grid.arrange(p_box_lambda, p_bias_lambda, ncol=2, widths=c(2.7,1), newpage=FALSE),
             grid.arrange(p_box_theta, p_bias_theta, ncol=2, widths=c(2.7,1), newpage=FALSE))
dev.off()






## comparison of 16 replicate case -------------------------
## boxplot and bias
im_m_pred_y <- cbind(apply(im_pred_y[,,1], c(1), mean),
                     exp(apply(log(im_pred_y[,,2]), c(1), mean)))

bg_colors <- brewer.pal(8, "Set1")[c(2,1)]
mm_box_lambda <- rbind(data.frame(true=test_y[,1], est=im_m_pred_y[,1],
                                  var="lambda", method="NF30"),
                       data.frame(true=test_y[,1], est=vg_rep_pred_y[,1],
                                  var="lambda", method="NV30"),
                       data.frame(true=test_y[,1], est=mc_pred_y[,1],
                                  var="lambda", method="ML30"))
mm_box_lambda$method <- factor(mm_box_lambda$method,
                               levels=c("NF30","NV30","ML30"))
mm_box_lambda$true_bin <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "",
                   as.character(cut_interval(mm_box_lambda$true, 10))), ","),
                                        function(x) mean(as.numeric(x))))
lambda_color_bin <- sort(unique(unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "",
                   as.character(cut_interval(mm_box_lambda$true, 10))), ","),
                                        function(x) (as.numeric(x)[2])))))
mm_box_lambda$color_bin <- cut_interval(mm_box_lambda$true, 2)
mm_box_lambda %>%
   ggplot(aes(x=true_bin, y=est-true, group=true_bin) ) +
    geom_rect(xmin=-Inf, xmax=lambda_color_bin[5], ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[1], lighten)) +
    geom_rect(xmin=lambda_color_bin[5], xmax=Inf, ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[2], lighten))+
    geom_vline(xintercept = seq(-7.5,0,2.5), color="gray", size=.5) +
    geom_hline(yintercept = seq(-2,1,1), color="gray", size=.5) +
    geom_boxplot(width=.5) +
    xlab(expression(log(lambda)~phantom(log(hat(lambda))))) +
    ylab(expression(log(hat(lambda))-log(lambda))) +
      facet_wrap(method~.) +
    geom_abline(intercept=0, slope=0, color="red", linetype=2) -> p_box_lambda
mm_box_theta <- rbind(data.frame(true=test_y[,2], est=im_m_pred_y[,2], var="theta", method="NF30"),
      data.frame(true=test_y[,2], est=vg_rep_pred_y[,2], var="theta", method="NV30"),
      data.frame(true=test_y[,2], est=mc_pred_y[,2], var="theta", method="ML30")) 
mm_box_theta$method <- factor(mm_box_theta$method, levels=c("NF30","NV30","ML30"))
mm_box_theta$true_bin <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "", as.character(cut_interval(mm_box_theta$true, 10))), ","), function(x) mean(as.numeric(x))))
theta_color_bin <- sort(unique(unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "",
                   as.character(cut_interval(mm_box_theta$true, 10))), ","),
                   function(x) (as.numeric(x)[2])))))
mm_box_theta %>%
   ggplot(aes(x=true_bin, y=est-true, group=true_bin) ) +
        geom_rect(xmin=-Inf, xmax=theta_color_bin[5], ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[1], lighten))+
    geom_rect(xmin=theta_color_bin[5], xmax=Inf, ymin=-Inf, ymax=Inf,
              fill=lighten(bg_colors[2], lighten))+
    geom_vline(xintercept = seq(5,25,5), color="gray", size=.5) +
    geom_hline(yintercept = seq(-2,4,2), color="gray", size=.5) +
    geom_boxplot() +
      xlab(expression(theta~phantom(hat(theta)))) + ylab(expression(hat(theta)-theta)) +
      facet_wrap(method~.) +
    geom_abline(intercept=0, slope=0, color="red", linetype=2) -> p_box_theta

mm_box_lambda2 <- rbind(data.frame(true=test_y[,1], est=c(im_m_pred_y[,1]), var="lambda",
                                   method="NF30"),
                        data.frame(true=test_y[,1], est=c(vg_rep_pred_y[,1]), var="lambda",
                                   method="NV30"),
                        data.frame(true=test_y[,1], est=c(mc_pred_y[,1]), var="lambda",
                                   method="ML30"))
mm_box_lambda2$true_bin <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "", as.character(cut_interval(mm_box_lambda2$true, 2))), ","), function(x) mean(as.numeric(x))))
mm_box_lambda2$true_bin_label <- factor(mm_box_lambda2$true_bin,
                                        levels=sort(unique(mm_box_lambda2$true_bin), decreasing=TRUE),
                                        labels=c("large", "small"))
mm_box_lambda2$method <- factor(mm_box_lambda2$method, levels=c("NF30", "NV30", "ML30"))
mm_box_theta2 <- rbind(data.frame(true=test_y[,2], est=c(im_m_pred_y[,2]), var="theta",
                                  method="NF30"),
                       data.frame(true=test_y[,2], est=c(vg_rep_pred_y[,2]), var="theta",
                                  method="NV30"),
                       data.frame(true=test_y[,2], est=c(mc_pred_y[,2]), var="theta",
                                  method="ML30"))
mm_box_theta2$method <- factor(mm_box_theta2$method, levels=c("NF30", "NV30", "ML30"))
mm_box_theta2$true_bin <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "", as.character(cut_interval(mm_box_theta2$true, 2))), ","), function(x) mean(as.numeric(x))))
mm_box_theta2$true_bin_label <- factor(mm_box_theta2$true_bin,
                                       levels=sort(unique(mm_box_theta2$true_bin), decreasing=TRUE),
                                       labels=c("large", "small"))
mm_box_lambda2 %>%
    mutate(diff = est - true) %>%
    group_by(method, var, true_bin_label) %>%
    summarize(bias = mean(diff), variance = sd(diff))  %>%
    mutate(var = factor(var, levels=c("lambda"), labels=c(expression("log(lambda)")))) %>%
    ggplot(mapping=aes(x=bias, y=variance, shape=method, color=as.factor(true_bin_label), label=method)) +
    facet_wrap(~var, scales="free", labeller = label_parsed) +
    geom_vline(xintercept=0, linetype=2, color="gray25") +
    geom_text(size=5, key_glyph = "point") +
    scale_shape_manual(values=c(1,3,4)) +
    scale_color_manual(values=rev(bg_colors)) +
    xlab(expression(phantom(log(widehat(lambda)))~"bias"~phantom(log(widehat(lambda))))) +
    ylab(expression("standard deviation")) +
    scale_x_continuous(expand = expansion(mult = .15)) +
    scale_y_continuous(expand = expansion(mult = .05)) +
    theme(legend.position="none") -> p_bias_lambda

mm_box_theta2 %>%
    mutate(diff = est - true) %>%
    group_by(method, var, true_bin_label) %>%
    summarize(bias = mean(diff), variance = sd(diff))  %>%
    mutate(var = factor(var, levels=c("theta"), labels=c(expression(theta)))) %>%
    ggplot(mapping=aes(x=bias, y=variance, shape=method, color=as.factor(true_bin_label), label=method)) +
    facet_wrap(~var, scales="free", labeller = label_parsed) +
    geom_vline(xintercept=0, linetype=2, color="gray25") +
    geom_text(size=5, key_glyph = "point") +
    scale_shape_manual(values=c(1,3,4)) +
    scale_color_manual(values=rev(bg_colors)) +
    xlab(expression(phantom(widegat(theta))~"bias"~phantom(widegat(theta)))) +
    ylab(expression("standard deviation")) +
    scale_x_continuous(expand = expansion(mult = .15)) +
    scale_y_continuous(expand = expansion(mult = .05)) +
    theme(legend.position="none") -> p_bias_theta


png("figs/fig_2b.png", width=11, height=6, units="in", res=600)
grid.arrange(grid.arrange(p_box_lambda, p_bias_lambda, ncol=2, widths=c(2.7,1), newpage=FALSE),
             grid.arrange(p_box_theta, p_bias_theta, ncol=2, widths=c(2.7,1), newpage=FALSE))
dev.off()

