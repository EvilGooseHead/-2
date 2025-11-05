
shap1013 <- shap_feature_importance(model1013,name = '(2010和2013年合并数据集,lightGBM)',seed =123)
shap1821 <- shap_feature_importance(model1821_1,name = '(2018和2021年合并数据集,lightGBM)',seed =231)
shapviz1013 <- shap1013$shap_values
shapviz1821 <- shap1821$shap_values
names(shapviz1821)
library(tidyverse)
library(tidyr)
shap_imp_1013 <- shapviz1013$S %>% 
  as.data.frame() %>% 
  summarise(across(everything(), ~mean(abs(.x)))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "mean_abs_shap") %>% 
  arrange(desc(mean_abs_shap)) %>% 
  mutate(rank_1013 = row_number())

shap_imp_1821 <- shapviz1821$S %>% 
  as.data.frame() %>% 
  summarise(across(everything(), ~mean(abs(.x)))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "mean_abs_shap") %>% 
  arrange(desc(mean_abs_shap)) %>% 
  mutate(rank_1821 = row_number())

combined_ranks <- full_join(
  select(shap_imp_1013, feature, rank_1013),
  select(shap_imp_1821, feature, rank_1821),
  by = "feature"
) %>% 
  mutate(
    rank_change = rank_1821 - rank_1013,      # 正数=排名上升
    abs_rank_change = abs(rank_change),
    rel_change = abs_rank_change / nrow(.)
  ) %>% 
  arrange(desc(abs_rank_change))
  
threshold <- nrow(combined_ranks) * 0.3
significant_changed <- combined_ranks %>% 
  filter(abs_rank_change > threshold)

library(ggrepel)
change_shap <- ggplot(combined_ranks, aes(x = rank_1013, y = rank_1821, color = rank_change)) +
  geom_point(size = 2.6) +
  geom_abline(slope = 1, linetype = "dashed") +
  geom_text_repel(
    aes(label = ifelse(abs_rank_change >= threshold, feature, "")),
    size = 5, max.overlaps = 30
  ) +
  scale_color_gradient2(low = "red", mid = "grey", high = "blue") +
  labs(x = "2010-2013排名", y = "2018-2021排名", 
       title = "SHAP特征重要性排名变化(分组数据)") +
  theme(title = element_text(size = 18),
        plot.background = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        plot.margin = margin(              # 动态边距（上,右,下,左）
          t = 30,                          # 上边距
          r = 30,         # 右边距
          b = 20,                          # 下边距
          l = 40                           # 左边距
        ))
change_shap
