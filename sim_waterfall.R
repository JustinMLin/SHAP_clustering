library(tidyverse)
library(ggplot2)
library(gridExtra)

data <- read.csv("DATA/SHAP_groups_sim.csv") %>%
  rename(ID = X)

data <- data %>% filter(Cluster != -1)

long_data = data %>%
  pivot_longer(-c(ID, Cluster)) %>%
  separate(name, sep="_", into=c("feature", "condition")) %>%
  pivot_wider(names_from=condition, values_from=value)

mean_dist <- long_data %>%
  mutate(dist=sqrt(Class.0^2 + Class.1^2 + Class.2^2)) %>%
  group_by(feature) %>%
  dplyr::summarize(mean_dist = mean(dist)) %>%
  arrange(desc(mean_dist))

feat_order <- mean_dist$feature

means_data <- long_data %>%
  group_by(Cluster, feature) %>%
  dplyr::summarize(mean0 = mean(Class.0),
                   mean1 = mean(Class.1),
                   mean2 = mean(Class.2),
                   .groups="keep") %>%
  ungroup(feature) %>%
  arrange(factor(feature, levels = feat_order), .by_group=TRUE) %>%
  mutate(cs0 = cumsum(mean0),
         cs1 = cumsum(mean1),
         cs2 = cumsum(mean2)) %>%
  ungroup()

pca <- prcomp(means_data %>% select(cs0:cs2), rank.=2, center=FALSE)$x

pca_data <- means_data %>%
  select(c(Cluster, feature)) %>%
  mutate(PC1 = pca[,1],
         PC2 = pca[,2])

top_feats <- feat_order[1:4]

rest_data <- pca_data %>%
  filter(!(feature %in% top_feats)) %>%
  group_by(Cluster) %>%
  dplyr::summarize(PC1 = mean(PC1),
                   PC2 = mean(PC2)) %>%
  mutate(feature = "rest", .after=Cluster)

pca_data <- rbind(data.frame(Cluster=unique(data$Cluster), feature="start", PC1=0, PC2=0),
                  pca_data %>% filter(feature %in% top_feats),
                  rest_data) %>%
  arrange(Cluster)

pca_data <- pca_data %>%
  mutate(color = rep(c(unique(pca_data$feature)[-1], top_feats[1]),
                     length(unique(pca_data$Cluster))),
         label = ifelse(feature == "rest", paste("Cluster", Cluster), NA))

ggplot(pca_data, aes(x=PC1, y=PC2, group=Cluster, label=label)) +
  geom_point() +
  geom_path(aes(color=factor(color, levels=c("Feature.0",
                                             "Feature.1",
                                             "Feature.8",
                                             "Feature.5",
                                             "rest")))) +
  geom_text(aes(y = PC2 + sign(PC2)*0.3)) +
  geom_hline(yintercept = 0, size=0.2) +
  geom_vline(xintercept = 0, size=0.2) +
  scale_color_hue(labels=c("Feature.0" = "0",
                           "Feature.1" = "1",
                           "Feature.8" = "8",
                           "Feature.5" = "5",
                           "rest" = "Rest")) +
  scale_x_continuous(limits = ggpmisc::symmetric_limits) +
  scale_y_continuous(limits = ggpmisc::symmetric_limits) +
  labs(color="Feature")
