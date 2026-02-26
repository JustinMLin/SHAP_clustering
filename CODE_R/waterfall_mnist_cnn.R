library(tidyverse)
library(ggplot2)

shap_dt <- read.csv("DATA/model comparison/MNIST/mnist_shap_dt.csv")
shap_xgb <- read.csv("DATA/model comparison/MNIST/mnist_shap_xgb.csv")
shap_cnn <- read.csv("DATA/model comparison/MNIST/mnist_shap_cnn.csv")

cluster <- read.csv("DATA/model comparison/MNIST/mnist_target.csv")[,1]
cluster_cnn <- read.csv("DATA/model comparison/MNIST/mnist_target_cnn.csv")[,1]

stack_df <- function(data, n_feat=784, n_class=10) {
  data.frame(ID = rep(data$ID, each=n_feat),
             cluster = rep(data$cluster, each=n_feat),
             feature = rep(paste0("X", 0:(n_feat-1)), nrow(data)),
             Class0 = unlist(c(t(data %>% select(ends_with("_0"))))),
             Class1 = unlist(c(t(data %>% select(ends_with("_1"))))),
             Class2 = unlist(c(t(data %>% select(ends_with("_2"))))),
             Class3 = unlist(c(t(data %>% select(ends_with("_3"))))),
             Class4 = unlist(c(t(data %>% select(ends_with("_4"))))),
             Class5 = unlist(c(t(data %>% select(ends_with("_5"))))),
             Class6 = unlist(c(t(data %>% select(ends_with("_6"))))),
             Class7 = unlist(c(t(data %>% select(ends_with("_7"))))),
             Class8 = unlist(c(t(data %>% select(ends_with("_8"))))),
             Class9 = unlist(c(t(data %>% select(ends_with("_9"))))))
}

waterfall_plot <- function(shap, cluster, n_feat, title, adj, vjust=1.5, hjust=1.5, labels=TRUE, legend=TRUE) {
  data <- cbind(cluster, shap) %>%
    mutate(ID = 1:nrow(shap), .before=cluster) %>%
    filter(cluster != -1)

  long_data <- stack_df(data)

  mean_dist <- long_data %>%
    mutate(dist=sqrt(Class0^2 + Class1^2 + Class2^2 + Class3^2 + Class4^2 + Class5^2 + Class6^2 + Class7^2 + Class8^2 + Class9^2)) %>%
    group_by(feature) %>%
    dplyr::summarize(mean_dist = mean(dist)) %>%
    arrange(desc(mean_dist))

  feat_order <- mean_dist$feature

  means_data <- long_data %>%
    group_by(cluster, feature) %>%
    dplyr::summarize(mean0 = mean(Class0),
                     mean1 = mean(Class1),
                     mean2 = mean(Class2),
                     mean3 = mean(Class3),
                     mean4 = mean(Class4),
                     mean5 = mean(Class5),
                     mean6 = mean(Class6),
                     mean7 = mean(Class7),
                     mean8 = mean(Class8),
                     mean9 = mean(Class9),
                     .groups="keep") %>%
    ungroup(feature) %>%
    arrange(factor(feature, levels = feat_order), .by_group=TRUE) %>%
    mutate(cs0 = cumsum(mean0),
           cs1 = cumsum(mean1),
           cs2 = cumsum(mean2),
           cs3 = cumsum(mean3),
           cs4 = cumsum(mean4),
           cs5 = cumsum(mean5),
           cs6 = cumsum(mean6),
           cs7 = cumsum(mean7),
           cs8 = cumsum(mean8),
           cs9 = cumsum(mean9)) %>%
    ungroup()

  pca <- prcomp(means_data %>% select(cs0:cs9), rank.=2, center=FALSE)
  pca_x = pca$x

  total_var = cumsum(pca$sdev^2)[2]/sum(pca$sdev^2)

  pca_data <- means_data %>%
    select(c(cluster, feature)) %>%
    mutate(PC1 = pca_x[,1],
           PC2 = pca_x[,2])

  top_feats <- feat_order[1:n_feat]

  rest_data <- pca_data %>%
    filter(!(feature %in% top_feats)) %>%
    group_by(cluster) %>%
    dplyr::summarize(PC1 = mean(PC1),
                     PC2 = mean(PC2)) %>%
    mutate(feature = "rest", .after=cluster)

    pca_data <- rbind(data.frame(cluster=unique(data$cluster), feature="start", PC1=0, PC2=0),
                      pca_data %>% filter(feature %in% top_feats),
                      rest_data) %>%
      arrange(cluster)

    pca_data <- pca_data %>%
      mutate(color = rep(c(unique(pca_data$feature)[-1], top_feats[1]),
                         length(unique(pca_data$cluster))),
             label = ifelse(feature == "rest", cluster, NA))

    label_data <- data.frame(x=Inf, y=Inf, vjust=vjust, hjust=hjust,
                             label=format(total_var, digits=2, nsmall=2))

    ggplot(pca_data, aes(x=PC1, y=PC2, group=cluster, label=label)) +
      geom_point() +
      geom_path(aes(color=factor(color, levels=c(feat_order[1:n_feat], "rest")))) +
      {if (labels) geom_text(aes(x = PC1 + adj*PC1/sqrt(PC1^2+PC2^2),
                                 y = PC2 + adj*PC2/sqrt(PC1^2+PC2^2)))} +
      geom_text(data=label_data, aes(x=x, y=y, vjust=vjust, hjust=hjust, label=label), inherit.aes=FALSE) +
      geom_hline(yintercept = 0, size=0.2) +
      geom_vline(xintercept = 0, size=0.2) +
      scale_x_continuous(limits = ggpmisc::symmetric_limits) +
      scale_y_continuous(limits = ggpmisc::symmetric_limits) +
      labs(title=title, color="Feature") +
      theme_bw() +
      {if (!legend) theme(legend.position="none")}
}

waterfall_plot(shap_dt, cluster, 100, "Decision Tree", adj=0.05, labels=T, legend=F)
waterfall_plot(shap_xgb, cluster, 100, "XGBoost", adj=1, labels=T, legend=F)
waterfall_plot(shap_cnn, cluster_cnn, 100, "Neural Network", adj=0.1, labels=T, legend=F)

