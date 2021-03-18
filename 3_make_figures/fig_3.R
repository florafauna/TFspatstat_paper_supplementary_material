rm(list=ls())
library(reticulate); np <- import("numpy")
source("Rscripts/ggplot_theme.R")
mae <- function(x, y) mean(abs(x-y))
rmspe <- function(x, y) sqrt(mean((x-y)^2))
lighten <- .8



## testObj with true values
test_y <- np$load("../1_common_data/npy/test_y.npy")

## 'test_mle_c' (channelwise MLE) and ' mleComb' combined MLE 
gs_rep_pred_y <- np$load("../2_model_GS30/npy/test_pred_model_gstat_perfect_start_values.npy")
cl_rep_pred_y <- np$load("../2_model_CL30/npy/test_pred_model_composite_perfect_start_values_lbfgsb.npy")
gs_rep_pred_y[,1] <- log(gs_rep_pred_y[,1])
mean(!is.finite(gs_rep_pred_y[,1]))
gs_rep_pred_y[,1][!is.finite(gs_rep_pred_y[,1])] <- min(gs_rep_pred_y[,1][is.finite(gs_rep_pred_y[,1])])
cl_rep_pred_y[,1] <- log(cl_rep_pred_y[,1])
cl_rep_pred_y[,1][!is.finite(cl_rep_pred_y[,1])] <- min(cl_rep_pred_y[,1][is.finite(cl_rep_pred_y[,1])])

bg_colors <- brewer.pal(8, "Set1")[c(2,1)]
mm_box_lambda <- rbind(data.frame(true=test_y[,1], est=gs_rep_pred_y[,1],
                                  var="lambda", method="GS30"),
                       data.frame(true=test_y[,1], est=cl_rep_pred_y[,1],
                                  var="lambda", method="CL30"))
mm_box_lambda$method <- factor(mm_box_lambda$method,
                               levels=c("GS30","CL30"))
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
    geom_abline(intercept=0, slope=0, color="red", linetype=2) +
    coord_cartesian(ylim = c(-2.01, 1.01)) -> p_box_lambda

mm_box_theta <- rbind(data.frame(true=test_y[,2], est=gs_rep_pred_y[,2],
                                 var="theta", method="GS30"),
                      data.frame(true=test_y[,2], est=cl_rep_pred_y[,2],
                                 var="theta", method="CL30")) 
mm_box_theta$method <- factor(mm_box_theta$method, levels=c("GS30","CL30"))
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
    geom_abline(intercept=0, slope=0, color="red", linetype=2) +
    coord_cartesian(ylim=c(-2.4, 4.01)) -> p_box_theta

mm_box_lambda2 <- rbind(data.frame(true=test_y[,1], est=c(gs_rep_pred_y[,1]), var="lambda",
                                   method="GS30"),
                        data.frame(true=test_y[,1], est=c(cl_rep_pred_y[,1]), var="lambda",
                                   method="CL30"))
mm_box_lambda2$true_bin <- unlist(lapply(strsplit(gsub("(\\()|(\\])|\\[", "", as.character(cut_interval(mm_box_lambda2$true, 2))), ","), function(x) mean(as.numeric(x))))
mm_box_lambda2$true_bin_label <- factor(mm_box_lambda2$true_bin,
                                        levels=sort(unique(mm_box_lambda2$true_bin), decreasing=TRUE),
                                        labels=c("large", "small"))
mm_box_lambda2$method <- factor(mm_box_lambda2$method, levels=c("GS30", "CL30"))

mm_box_theta2 <- rbind(data.frame(true=test_y[,2], est=c(gs_rep_pred_y[,2]), var="theta",
                                  method="GS30"),
                       data.frame(true=test_y[,2], est=c(cl_rep_pred_y[,2]), var="theta",
                                  method="CL30"))
mm_box_theta2$method <- factor(mm_box_theta2$method, levels=c("GS30", "CL30"))
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
    scale_x_continuous(expand = expansion(mult = .25)) +
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
    scale_x_continuous(expand = expansion(mult = .25)) +
    scale_y_continuous(expand = expansion(mult = .05)) +
    theme(legend.position="none") -> p_bias_theta


#pdf("figs/16replicate_boxplot_bias.pdf", width=11, height=6)
png("figs/fig_3.png", width=8, height=6, units="in", res=600)
grid.arrange(grid.arrange(p_box_lambda, p_bias_lambda, ncol=2, widths=c(1.8,1), newpage=FALSE),
             grid.arrange(p_box_theta, p_bias_theta, ncol=2, widths=c(1.8,1), newpage=FALSE))
dev.off()
