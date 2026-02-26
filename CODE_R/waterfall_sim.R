library(tidyverse)
library(ggplot2)
library(gridExtra)

shap_dt <- read.csv("DATA/model comparison/simulated/sim_shap_dt.csv")
shap_xgb <- read.csv("DATA/model comparison/simulated/sim_shap_xgb.csv")
shap_nn <- read.csv("DATA/model comparison/simulated/sim_shap_nn.csv")

cluster <- read.csv("DATA/model comparison/simulated/sim_hdbscan.csv")[,1]

waterfall_plot <- function(shap, cluster, n_feat, title, adj, vjust=1.5, hjust=1.5, labels=TRUE) {
  data <- cbind(cluster, shap) %>%
    mutate(ID = 1:nrow(shap), .before=cluster) %>%
    filter(cluster != -1)

  long_data <- data %>%
    pivot_longer(-c(ID, cluster)) %>%
    separate(name, sep="_", into=c("feature", "class")) %>%
    pivot_wider(names_from=class, names_prefix = "Class", values_from=value)

  mean_dist <- long_data %>%
    mutate(dist=sqrt(Class0^2 + Class1^2 + Class2^2)) %>%
    group_by(feature) %>%
    dplyr::summarize(mean_dist = mean(dist)) %>%
    arrange(desc(mean_dist))

  feat_order <- mean_dist$feature

  means_data <- long_data %>%
    group_by(cluster, feature) %>%
    dplyr::summarize(mean0 = mean(Class0),
                     mean1 = mean(Class1),
                     mean2 = mean(Class2),
                     .groups="keep") %>%
    ungroup(feature) %>%
    arrange(factor(feature, levels = feat_order), .by_group=TRUE) %>%
    mutate(cs0 = cumsum(mean0),
           cs1 = cumsum(mean1),
           cs2 = cumsum(mean2)) %>%
    ungroup()

  pca <- prcomp(means_data %>% select(cs0:cs2), rank.=2, center=FALSE)
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
             label = ifelse(feature == "rest", paste("Cluster", cluster+1), NA))

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
      scale_x_continuous(limits = ggpmisc::symmetric_limits, expand = expansion(mult = 0.1)) +
      scale_y_continuous(limits = ggpmisc::symmetric_limits, expand = expansion(mult = 0.1)) +
      labs(title=title, color="Feature") +
      theme_bw()
}

waterfall_plot(shap_dt, cluster, 4, "Decision Tree", 0.1, labels=T)
waterfall_plot(shap_xgb, cluster, 4, "XGBoost", 1, labels=T)
waterfall_plot(shap_nn, cluster, 4, "Neural Network", 0.1, labels=T)
