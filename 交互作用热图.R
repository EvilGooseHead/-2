# 1. 数据处理：提取下三角部分（含对角线）
interaction_matrix <- shap1021_XGB_inter$interaction_matrix
features_to_keep <- head(rownames(interaction_matrix))  # 移除最后4个
interaction_matrix <- interaction_matrix[features_to_keep, features_to_keep]
interaction_matrix[upper.tri(interaction_matrix, diag = FALSE)] <- NA
showtext_auto()  
class(interaction_matrix)

non_diag_mask <- !diag(nrow(interaction_matrix))

# 2. 将矩阵对角线元素设为NA（保留原始矩阵结构）
mat_no_diag <- interaction_matrix * non_diag_mask
mat_no_diag[mat_no_diag == 0] <- NA  # 将对角线0转为NA

# 3. 计算每行非对角线元素的最大绝对值
row_max_abs <- apply(abs(mat_no_diag), 1, max, na.rm = TRUE)

# 4. 按最大值排序并取前20名（降序排列）
top20_indices <- order(row_max_abs, decreasing = TRUE)[1:20]
top20_names <- rownames(interaction_matrix)[top20_indices]

# 5. 创建结果数据框
result_df <- data.frame(
  row_name = top20_names,
  max_abs_value = row_max_abs[top20_indices],
  rank = 1:20,
  stringsAsFactors = FALSE
)

# 输出结果
print(result_df)

plot_data <- as.data.frame(as.table(interaction_matrix)) %>%
  filter(!is.na(Freq)) %>%
  rename(Feature1 = Var1, Feature2 = Var2, Interaction = Freq) %>%
  mutate(
    Feature1 = factor(Feature1, levels = rev(levels(Feature1))),  # 反转Y轴顺序
    Feature2 = factor(Feature2)
  )

# 2. 绘制热力图
heatmap <- ggplot(plot_data, aes(x = Feature2, y = Feature1, fill = Interaction)) +
  geom_tile(color = "white", linewidth = 0.5) +
  # 添加数值标签（保留2位小数）
  geom_text(
    aes(label = sprintf("%.2f", Interaction)),
    color = "black",  # 统一白色文字
    size = 3.6,
    fontface = "bold"
  ) +
  # 指定颜色梯度
  scale_fill_gradient2(
    low = "skyblue",  # 浅蓝绿
    high = "#984EA3", # 紫色
    midpoint = 0.001,
    na.value = NA      # 隐藏NA值
  ) +
  labs(
    fill = "交互强度"
  ) +
  theme(
    # 坐标轴设置
    axis.title = element_blank(),
    text = element_text(size = 18 ,family = "HT"),
    axis.text.x = element_text(angle = 30, size = 14,face = "bold",vjust = 0.6,hjust = 0.7),
    # 隐藏X轴标签
    axis.text.y = element_text(face = "bold", size = 14),  # Y轴加粗
    # 网格和背景
    panel.grid = element_blank(),
    panel.background = element_blank(),
    # 图例设置
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  ) +
  coord_fixed()  # 保持单元格为正方形

# 显示图形
print(heatmap)