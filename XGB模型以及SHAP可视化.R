library(shapviz)             
library(xgboost)
library(caret)                
library(pROC)
library(reshape2)
library(pheatmap)
library(ggplotify)
library(patchwork)


#封装好的XGB训练函数
xgb_final_train <- function(
    Data, 
    target_var = "expectation",
    test_ratio = 0.3,
    # XGBoost 主要参数
    max_depth = 6,
    gamma = 1,
    eta = 0.1,
    colsample_bytree = 0.8,
    colsample_bylevel = 1,
    lambda = 0.6,
    alpha = 0.8,
    subsample = 1,
    min_child_weight = 20,
    max_cat_to_onehot = 4,
    max_delta_step = 0.1,
    n_estimators = 300,
    # 其他选项
    early_stopping_rounds = 100,
    eval_metric = "auc",
    # 分类阈值
    threshold = 0.5,
    # 交叉验证
    nfold = 5,
    seed = 42
) {
  
  # 1. 数据预处理
  features <- Data[, -which(names(Data) == target_var), drop = FALSE]
  labels <- as.numeric(as.character(Data[[target_var]]))  # 转换为0/1数值
  
  # 2. 划分训练集和测试集
  set.seed(seed)
  train_idx <- createDataPartition(labels, p = 1 - test_ratio, list = FALSE)
  train_data <- features[train_idx, ]
  train_labels <- labels[train_idx]
  test_data <- features[-train_idx, ]
  test_labels <- labels[-train_idx]
  
  # 3. 创建XGBoost数据集
  dtrain <- xgb.DMatrix(data = data.matrix(train_data), label = train_labels)
  dtest <- xgb.DMatrix(data = data.matrix(test_data), label = test_labels)
  dall <- xgb.DMatrix(data = data.matrix(features), label = labels)
  
  # 4. 处理类别不平衡
  scale_pos_weight <- sum(train_labels == 0) / sum(train_labels == 1)
  
  # 5. 模型参数
  params <- list(
    objective = "binary:logistic",
    eval_metric = eval_metric,
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    colsample_bylevel = colsample_bylevel,
    lambda = lambda,
    alpha = alpha,
    subsample = subsample,
    min_child_weight = min_child_weight,
    max_cat_to_onehot = max_cat_to_onehot,
    max_delta_step = max_delta_step,
    scale_pos_weight = scale_pos_weight,
    verbosity = 1,
    seed = seed
  )
  
  # 6. 交叉验证
  cv_result <- xgb.cv(
    params = params,
    data = dall,
    nfold = nfold,
    nrounds = n_estimators,
    early_stopping_rounds = early_stopping_rounds,
    verbose = 1
  )
  
  # 7. 绘制交叉验证图
  plot_data <- cv_result$evaluation_log
  cvp <- ggplot(plot_data, aes(iter, test_auc_mean)) +
    geom_line(color = "blue") +
    geom_ribbon(
      aes(
        ymin = test_auc_mean - test_auc_std,  
        ymax = test_auc_mean + test_auc_std
      ),
      alpha = 0.2,
      fill = "skyblue"
    ) +
    labs(
      title = "Cross-Validation AUC Performance",
      x = "Boosting Rounds (iter)",
      y = "AUC"
    ) +
    theme_bw()
  
  # 8. 训练模型
  model <- xgb.train(
    params = params,
    data = dtrain,
    evals = list(train = dtrain, test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    nrounds = n_estimators,
    verbose = 1
  )
  
  # 9. 获取最佳迭代轮次
  best_iter <- model$best_iteration
  
  # 10. 预测概率
  test_pred_prob <- predict(model, data.matrix(test_data), iterationrange  = best_iter)
  train_pred_prob <- predict(model, data.matrix(train_data), iterationrange  = best_iter)
  
  # 11. 计算指标
  calc_metrics <- function(true_labels, pred_prob, name = "test") {
    pred_class <- ifelse(pred_prob > threshold, 1, 0)
    conf_matrix <- caret::confusionMatrix(
      data = as.factor(pred_class),
      reference = as.factor(true_labels),
      positive = "1"
    )
    
    # 提取混淆矩阵数值
    cm <- conf_matrix$table
    colnames(cm) <- rownames(cm) <- c("Pred_0", "Pred_1")
    
    list(
      accuracy = conf_matrix$overall["Accuracy"],
      recall = conf_matrix$byClass["Recall"],
      precision = conf_matrix$byClass["Precision"],
      f1 = conf_matrix$byClass["F1"],
      auc = pROC::auc(pROC::roc(true_labels, pred_prob)),
      confusion_matrix = cm
    )
  }
  
  test_metrics <- calc_metrics(test_labels, test_pred_prob, "test")
  train_metrics <- calc_metrics(train_labels, train_pred_prob, "train")
  
  # 12. 返回结果
  list(
    model = model,
    params = params,
    cvresult = cv_result,
    # 数据集输出
    test_features = test_data,
    test_labels = test_labels,
    raw_train_data = Data[train_idx, ], 
    raw_test_data = Data[-train_idx, ],
    features = features,
    labels = labels,
    # 图片输出
    cvp = cvp,
    # 测试集指标
    test_accuracy = test_metrics$accuracy,
    test_recall = test_metrics$recall,
    test_precision = test_metrics$precision,
    test_f1 = test_metrics$f1,
    test_auc = test_metrics$auc,
    test_confusion_matrix = test_metrics$confusion_matrix,
    test_pred_prob = test_pred_prob,
    # 训练集指标
    train_accuracy = train_metrics$accuracy,
    train_recall = train_metrics$recall,
    train_precision = train_metrics$precision,
    train_f1 = train_metrics$f1,
    train_auc = train_metrics$auc,
    train_confusion_matrix = train_metrics$confusion_matrix,
    train_pred_prob = train_pred_prob,
    # 其他信息
    threshold = threshold,
    best_iteration = best_iter
  )
}

xgb_final_train2 <- function(
    Data, 
    target_var = "expectation",
    test_ratio = 0.3,
    # XGBoost 主要参数
    max_depth = 6,
    gamma = 1,
    eta = 0.1,
    colsample_bytree = 0.8,
    colsample_bylevel = 1,
    lambda = 0.6,
    alpha = 0.8,
    subsample = 1,
    min_child_weight = 20,
    max_cat_to_onehot = 8,
    max_delta_step = 0.1,
    n_estimators = 300,
    # 其他选项
    early_stopping_rounds = 100,
    eval_metric = "auc",
    # 分类阈值
    threshold = 0.5,
    # 交叉验证
    nfold = 5,
    seed = 42
) {
  
  # 1. 数据预处理
  features <- Data[, -which(names(Data) == target_var), drop = FALSE]
  labels <- as.numeric(as.character(Data[[target_var]]))  # 转换为0/1数值
  
  # 2. 划分训练集和测试集
  set.seed(seed)
  train_idx <- createDataPartition(labels, p = 1 - test_ratio, list = FALSE)
  train_data <- features[train_idx, ]
  train_labels <- labels[train_idx]
  test_data <- features[-train_idx, ]
  test_labels <- labels[-train_idx]
  
  # 3. 创建XGBoost数据集
  dtrain <- xgb.DMatrix(data = data.matrix(train_data), label = train_labels)
  dtest <- xgb.DMatrix(data = data.matrix(test_data), label = test_labels)
  dall <- xgb.DMatrix(data = data.matrix(features), label = labels)
  
  # 4. 处理类别不平衡
  scale_pos_weight <- sum(train_labels == 0) / sum(train_labels == 1)
  
  # 5. 模型参数
  params <- list(
    objective = "binary:logistic",
    eval_metric = eval_metric,
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    colsample_bylevel = colsample_bylevel,
    lambda = lambda,
    alpha = alpha,
    subsample = subsample,
    min_child_weight = min_child_weight,
    max_cat_to_onehot = max_cat_to_onehot,
    max_delta_step = max_delta_step,
    scale_pos_weight = scale_pos_weight,
    verbosity = 1,
    seed = seed
  )
  
  # 8. 训练模型
  model <- xgb.train(
    params = params,
    data = dtrain,
    evals = list(train = dtrain, test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    nrounds = n_estimators,
    verbose = 1
  )
  
  # 9. 获取最佳迭代轮次
  best_iter <- model$best_iteration
  
  # 10. 预测概率
  test_pred_prob <- predict(model, data.matrix(test_data), iterationrange  = best_iter)
  train_pred_prob <- predict(model, data.matrix(train_data), iterationrange  = best_iter)
  
  # 11. 计算指标
  calc_metrics <- function(true_labels, pred_prob, name = "test") {
    pred_class <- ifelse(pred_prob > threshold, 1, 0)
    conf_matrix <- caret::confusionMatrix(
      data = as.factor(pred_class),
      reference = as.factor(true_labels),
      positive = "1"
    )
    
    # 提取混淆矩阵数值
    cm <- conf_matrix$table
    colnames(cm) <- rownames(cm) <- c("Pred_0", "Pred_1")
    
    list(
      accuracy = conf_matrix$overall["Accuracy"],
      recall = conf_matrix$byClass["Recall"],
      precision = conf_matrix$byClass["Precision"],
      f1 = conf_matrix$byClass["F1"],
      auc = pROC::auc(pROC::roc(true_labels, pred_prob)),
      confusion_matrix = cm
    )
  }
  
  test_metrics <- calc_metrics(test_labels, test_pred_prob, "test")
  train_metrics <- calc_metrics(train_labels, train_pred_prob, "train")
  
  # 12. 返回结果
  list(
    model = model,
    params = params,
    # 数据集输出
    test_features = test_data,
    test_labels = test_labels,
    raw_train_data = Data[train_idx, ], 
    raw_test_data = Data[-train_idx, ],
    features = features,
    labels = labels,
    # 测试集指标
    test_accuracy = test_metrics$accuracy,
    test_recall = test_metrics$recall,
    test_precision = test_metrics$precision,
    test_f1 = test_metrics$f1,
    test_auc = test_metrics$auc,
    test_confusion_matrix = test_metrics$confusion_matrix,
    test_pred_prob = test_pred_prob,
    # 训练集指标
    train_accuracy = train_metrics$accuracy,
    train_recall = train_metrics$recall,
    train_precision = train_metrics$precision,
    train_f1 = train_metrics$f1,
    train_auc = train_metrics$auc,
    train_confusion_matrix = train_metrics$confusion_matrix,
    train_pred_prob = train_pred_prob,
    # 其他信息
    threshold = threshold,
    best_iteration = best_iter
  )
}

shap_importance_inter <- function(model, 
                                  sample_size = 1000, 
                                  seed = 123,
                                  name = '***年和***年合并数据',
                                  max_display = 8L) {
  
  # 参数校验
  if (!all(c("model", "test_features", "test_labels") %in% names(model))) {
    stop("模型对象必须包含 'model', 'test_features' 和 'test_labels' 元素")
  }
  
  # 设置随机种子
  set.seed(seed)
  
  # 1. 数据抽样
  test_data <- model$test_features
  test_labels <- model$test_labels
  
  if (nrow(test_data) < sample_size) {
    warning(paste("测试集样本量(", nrow(test_data), ") 小于指定抽样量，使用全量数据"))
    sample_size <- nrow(test_data)
  }
  
  sample_idx <- sample(nrow(test_data), sample_size)
  X_explain <- data.matrix(test_data[sample_idx, ])
  X <- test_data[sample_idx, ]
  sampled_labels <- test_labels[sample_idx]
  
  # 2. 计算SHAP值（含交互作用）
  shap_obj <- shapviz(
    object = model$model, 
    X_pred = X_explain,
    X = X,
    interactions = TRUE
  )
  
  # 3. 变量名映射
  var_label_df <- read.csv("C:/Users/USER/Desktop/毕业设计/变量与标签.csv")
  feature_mapping <- setNames(var_label_df$label, var_label_df$variable)
  feature_names <- colnames(shap_obj$X)
  new_feature_names <- sapply(feature_names, function(x) {
    ifelse(x %in% names(feature_mapping), feature_mapping[x], x)
  })
  colnames(shap_obj$X) <- new_feature_names
  colnames(shap_obj$S) <- new_feature_names
  dimnames(shap_obj$S_inter) <- list(NULL, new_feature_names, new_feature_names)
  
  # 4. 计算平均交互强度矩阵
  interaction_matrix <- sv_interaction(shap_obj, max_display = max_display,kind= 'no')
  n_display <- max_display
  interaction_matrix<-interaction_matrix[1:n_display,1:n_display]
  # 准备下三角交互矩阵（保留对角线）
  interaction_matrix[upper.tri(interaction_matrix, diag = FALSE)] <- NA
  
  
  list(
    shap_object = shap_obj,
    interaction_matrix = interaction_matrix
  )
}



model1021_xgb <-  xgb_final_train(CGSS1021,eta = 0.05,n_estimators = 1600,max_depth = 6,gamma = 0.4)
model1021_xgb_2 <-  xgb_final_train2(CGSS1021,test_ratio = 0.2,eta = 0.03,n_estimators = 1800,max_depth = 8,gamma = 1)
model1021_xgb_3 <-  xgb_final_train2(CGSS1021,test_ratio = 0.4,eta = 0.1,n_estimators = 1200,max_depth = 6,gamma = 0.6)
show_eval(model1021_xgb)
model1021_xgb$

shap1021_XGB_1 <- shap_feature_importance(model1021_xgb,sample_size = 2000)
C1021_XGB_1 <- shap1021_XGB_1$shap_values
cshap1021_XGB_1$beeswarm<-shap1021_XGB_1$beeswarm  + 
  theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))
shap1021_XGB_1$beeswarm


shap1021_XGB_1$FPlot 
shap1021_XGB_inter <-shap_importance_inter(model1021_xgb, name = '(2010至2021年合并数据集)',max_display = 10)
shap1021_XGB_interbee<-shap1021_XGB_inter$hex_plot
shap1021_XGB_interm <- shap1021_XGB_inter$heat_gtable
shap1021_XGB_interbee

shap1021_XGB_2 <- shap_feature_importance(model1021_xgb_2)
C1021_XGB_2 <- shap1021_XGB_2$shap_values
shap1021_XGB_3 <- shap_feature_importance(model1021_xgb_3)
C1021_XGB_3 <- shap1021_XGB_3$shap_values
CGSS1013_XGB_1 <- xgb_final_train2(CGSS1013)
shap1013_XGB_1 <- shap_feature_importance(CGSS1013_XGB_1)
shap1013_XGB_1$beeswarm
C1013_XGB_1 <- shap1013_XGB_1$shap_values

CGSS1013_XGB_2 <- xgb_final_train2(CGSS1013,max_depth = 6,gamma = 0.02,eta = 0.05,n_estimators = 400,max_cat_to_onehot = 3)
shap1013_XGB_2 <- shap_feature_importance(CGSS1013_XGB_2)
C1013_XGB_2 <- shap1013_XGB_2$shap_values

CGSS1013_XGB_3 <- xgb_final_train2(CGSS1013,max_depth = 8,gamma = 0.1,eta = 0.03,n_estimators = 800,max_cat_to_onehot = 3)
shap1013_XGB_3 <- shap_feature_importance(CGSS1013_XGB_3)
C1013_XGB_3 <- shap1013_XGB_3$shap_values

CGSS1821_XGB_1 <- xgb_final_train2(CGSS1821)
shap1821_XGB_1 <- shap_feature_importance(CGSS1821_XGB_1)
C1821_XGB_1 <- shap1821_XGB_1$shap_values
shap1821_XGB_1$beeswarm


CGSS1821_XGB_2 <- xgb_final_train2(CGSS1821,max_depth = 6,gamma = 0.02,eta = 0.05,n_estimators = 500,max_cat_to_onehot = 3)
shap1821_XGB_2 <- shap_feature_importance(CGSS1821_XGB_2)
C1821_XGB_2 <- shap1821_XGB_2$shap_values

CGSS1821_XGB_3 <- xgb_final_train2(CGSS1821,max_depth = 8,gamma = 0.1,eta = 0.03,n_estimators = 800,max_cat_to_onehot = 3)
shap1821_XGB_3 <- shap_feature_importance(CGSS1821_XGB_3)
C1821_XGB_3 <- shap1821_XGB_3$shap_values

#计算SHAP相关性
  # 1. 提取SHAP值
shap_list <- list(C1013_LGB_1$S, C1013_LGB_2$S, C1013_LGB_3$S,
                  C1013_XGB_1$S, C1013_XGB_2$S, C1013_XGB_3$S, 
                  C1821_LGB_1$S, C1821_LGB_2$S, C1821_LGB_3$S,
                  C1821_XGB_1$S, C1821_XGB_2$S, C1821_XGB_3$S,
                  C1021_LGB_1$S, C1021_LGB_2$S, C1021_LGB_3$S,
                  C1021_XGB_1$S, C1021_XGB_2$S, C1021_XGB_3$S)


# 2. 动态生成模型名称（与shap_list顺序严格对应）
model_names <- c(
  "C1013_LGB_1", "C1013_LGB_2", "C1013_LGB_3",
  "C1013_XGB_1", "C1013_XGB_2", "C1013_XGB_3",
  "C1821_LGB_1", "C1821_LGB_2", "C1821_LGB_3",
  "C1821_XGB_1", "C1821_XGB_2", "C1821_XGB_3",
  "C1021_LGB_1", "C1021_LGB_2", "C1021_LGB_3",
  "C1021_XGB_1", "C1021_XGB_2", "C1021_XGB_3"
)

# 3. 计算特征重要性（绝对值均值）并合并为18×131矩阵
shap_matrix <- do.call(rbind, lapply(shap_list, function(x) {
  colMeans(abs(as.matrix(x)[, drop=FALSE]))
}))
rownames(shap_matrix) <- model_names

# 4. 计算模型间相关性（18×18矩阵）
cor_matrix <- cor(t(shap_matrix))
  # 计算显著性p值矩阵（使用cor.test）
p_matrix <- matrix(NA, nrow=18, ncol=18)
rownames(p_matrix) <- colnames(p_matrix) <- model_names

for (i in 1:18) {
  for (j in 1:18) {
    test_result <- cor.test(shap_matrix[i, ], shap_matrix[j, ])
    p_matrix[i, j] <- test_result$p.value
  }
}

# 5. 准备热图数据（自动适配18个模型）
plot_data <- as.data.frame(as.table(cor_matrix))
colnames(plot_data) <- c("Model1", "Model2", "Correlation")
plot_data$p_value <- as.data.frame(as.table(p_matrix))$Freq
  # 添加显著性标记
plot_data$signif <- ifelse(
  plot_data$p_value < 0.001, "***",
  ifelse(plot_data$p_value < 0.01, "**",
         ifelse(plot_data$p_value < 0.05, "*", ""))
)


#反转Y轴因子水平确保对角线方向正确
plot_data$Model1 <- factor(plot_data$Model1, levels = model_names)
plot_data$Model2 <- factor(plot_data$Model2, levels = model_names)
#添加显著性水平
plot_data$p_value <- as.data.frame(as.table(p_matrix))$Freq
plot_data$signif <- ifelse(plot_data$p_value < 0.001, "***",
                           ifelse(plot_data$p_value < 0.01, "**",
                                  ifelse(plot_data$p_value < 0.05, "*", "")))
#对角线
plot_data_lower <- plot_data %>%
  filter(as.numeric(Model2) <= as.numeric(Model1))

# 6. 可视化（适配18个模型的热图）
library(ggplot2)
max_label_width <- max(nchar(model_names)) * 0.2  # 动态计算标签宽度

cor_valid <- ggplot(plot_data_lower, aes(x = Model2, y = Model1, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.3) +
  
  geom_text(
    aes(label = sprintf("%.2f%s", Correlation, signif)),  # 简化显示，去掉显著性标记
    color = "white", size = 4, 
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#E0F7FA", mid = "#E41A1C", high = "#984EA3",
    midpoint = 0.85, limits = c(0.65, 1),  # 调整颜色范围
    guide = guide_colorbar(
      title = "Pearson相关系数",
      title.position = "top",
      title.theme = element_text(size = 14, lineheight = 1),
      barwidth = unit(0.8, "cm"),
      barheight = unit(8, "cm")  # 加长色条以适应18个模型
    )
  ) +
  
  labs(
    title = "不同年份与不同参数下模型间SHAP相关性", 
    x = "年份、模型与参数", 
    y = NULL,
    caption = "显著性水平：* p<0.05  ** p<0.01  *** p<0.001"
  ) +
  theme_minimal(base_size = 13) +  # 缩小基础字体
  theme(
    plot.caption = element_text(size = 12),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    axis.text.x = element_text(angle = 30,face = "bold", hjust = 1, size = 12),
    axis.text.y = element_text(size = 12,face = "bold"),
    legend.position = "right",
    axis.title.y = element_blank(), 
    axis.title.x = element_text(size = 18,face = 'bold',margin = margin(t = 50)),
    guide.title = element_text(size = 24, face = "bold", lineheight = 1.2),
    legend.text = element_text(size = 12), plot.margin = margin(              # 动态边距（上,右,下,左）
      t = 30,                          # 上边距
      r = max_label_width + 30,         # 右边距
      b = 20,                          # 下边距
      l = 40                           # 左边距
    )
  )
cor_valid

model1021_xgb$cvresult
cvp1021 <- model1021_xgb$cvp +
  ggtitle("2010-2021年合并数据集最优参数K折交叉验证AUC曲线(XGboost)") +
  scale_y_continuous(
    breaks = sort(c(0.60,0.65,0.70,0.7048)), # 确保包含0.70
    labels = function(x) ifelse(x == 0.7048, "0.7048", sprintf("%.2f", x))
  ) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 18,face ='bold'), 
        axis.title.x = element_text(size = 18, face ='bold'),
        plot.title = element_text(
          size = 16,                # 标题字体大小
          face = "bold",           # 加粗
          color = "black",         # 字体颜色
          hjust = 0.5,             # 水平居中
          margin = margin(b = 5)  # 底部边距
        )
  )+ 
  plot_annotation(
    caption = "阴影：±1个标准差，基于n折交叉验证",
    theme = theme(plot.caption = element_text(size=14,hjust = 0.5))
  )
cvp1021

sv_dependence(shap1013_XGB_1$shap_values,
              v="居住面积")  + geom_smooth(method = "loess",   # 拟合方法
                                       formula = y ~ x,    # 公式
                                       se = TRUE, 
                                       span = 0.9,# 显示置信区间
                                       color = "skyblue",      # 曲线颜色
                                       linewidth = 1.2) +   # 线宽
  ggtitle("住房面积特征贡献") +
  theme_bw(base_size = 16)

sense <- sv_dependence(shap1021_XGB_1$shap_values, v = "流动感知") +
  theme_bw(base_size = 16)
sense 

subjective_fide <- sv_dependence(shap1021_XGB_1$shap_values, v = '自评14岁时家庭社会地位' ,color_var = '流动感知') + 
  theme_bw(base_size = 16)
subjective_fide


age <- sv_dependence(shap1021_XGB_1$shap_values, v = "年龄",color_var = 'auto') + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, linewidth = 1.2,aes(group = 流动感知, color = 流动感知))+
  theme_bw(base_size = 16) 
age

subjective_ide <- sv_dependence(shap1021_XGB_1$shap_values, v = '主观阶层认同' ,color_var = '人均GDP') + 
  ggtitle("主观阶层认同") +
  theme_bw(base_size = 16)
subjective_ide

city_worlk_time <- sv_dependence(shap1021_XGB_1$shap_values, v = "非农工作的工作时长",color_var = '年龄') + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, color = "skyblue", linewidth = 1.2)  +  # 限制 SHAP 值显示范围
  ggtitle("非农工作的工作时长(年)") +
  theme_bw(base_size = 16)
city_worlk_time

gender_attitude <- sv_dependence(shap1021_XGB_1$shap_values, v = "是否同意“如需要，应优先解雇女性员工”",color_var = '年龄') + 
  ggtitle("性别观念") +
  theme_bw(base_size = 16)
gender_attitude

old <- sv_dependence(shap1021_XGB_1$shap_values, v = "养老主要应该由谁负责",color_var = NULL) + 
  ggtitle("养老观念") +
  theme_bw(base_size = 16)
old

learn <- sv_dependence(shap1021_XGB_1$shap_values, v = "空闲时间学习充电的频率",color_var = 'auto')+  # 限制 SHAP 值显示范围
  ggtitle("空闲时间学习充电") +
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.4, linewidth = 1.2,aes(group = 空闲时间拜访亲戚的频率, color = 空闲时间拜访亲戚的频率))  + 
  theme_bw(base_size = 16)
learn

music <- sv_dependence(shap1021_XGB_1$shap_values, v = "空闲时间在家听音乐的频率",color_var = 'auto')+
  theme_bw(base_size = 16)
music

Year_effect <- sv_dependence(shap1021_XGB_1$shap_values, v = "调查年份",color_var = 'auto') + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, linewidth = 1.2)  +  # 限制 SHAP 值显示范围)+
  theme_bw(base_size = 16)
Year_effect

birth_will <- sv_dependence(shap1021_XGB_1$shap_values, v = "无限制的情况下希望生几个孩子" ) + 
  ggtitle("生育意愿") +
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0, linewidth = 1.2,color ='skyblue')+
  theme_bw(base_size = 16)
birth_will

Lnincome <- sv_dependence(shap1021_XGB_1$shap_values, v = "个人去年收入" ,color_var = NULL) + 
  ggtitle("个人去年收入") +
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.4, linewidth = 1.2,color='skyblue')+
  labs(subtitle = "个人去年收入与生育意愿\n（颜色表示生育意愿）")+
  coord_cartesian(xlim = c(6, 15))+
  theme_bw(base_size = 16)
Lnincome



politic_attitude <- sv_dependence(shap1021_XGB_1$shap_values, v = "政府应不应该干预生育" ) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, linewidth = 1.2,color = "skyblue")+
  ggtitle("政治态度(生育)") +
  theme_bw(base_size = 16)
politic_attitude

madarain <- sv_dependence(shap1021_XGB_1$shap_values, v = "听普通话的能力" ) +
  theme_bw(base_size = 16)
madarain

English <- sv_dependence(shap1021_XGB_1$shap_values, v = "听英语的能力" ) +
  theme_bw(base_size = 16)
English

living_space <- sv_dependence(shap1021_XGB_1$shap_values, v = "居住面积",color_var = '地级市') + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, linewidth = 1.2, color = "skyblue") +
  coord_cartesian(xlim = c(0, 200)) +  # 限制 SHAP 值显示范围
  theme_bw(base_size = 16)
living_space

city <-  sv_dependence(shap1021_XGB_1$shap_values, v = "城乡(1=城市,0=乡村)",color_var = NULL) + 
  theme_bw(base_size = 16)
city

visit_relative <- sv_dependence(na.omit(shap1021_XGB_1$shap_values), v = "空闲时间拜访亲戚的频率" ) + 
     geom_smooth(method = "loess", formula = y ~ x, 
     se = TRUE, span = 0.6, color = "skyblue", linewidth = 1.2) +
     geom_smooth(method = "loess", formula = y ~ x, 
     se = TRUE, span = 0.8, degree = 1,linewidth = 1.2,aes(group = 空闲时间学习充电的频率, color = 空闲时间学习充电的频率))  + 
     ggtitle("空闲时间拜访亲戚的频率") +
     theme_bw(base_size = 16)
visit_relative

union_membership <- sv_dependence(shap1021_XGB_1$shap_values, v = "是否是工会会员" ,color_var = NULL) + 
  ggtitle('是否是工会会员') +
  theme_bw(base_size = 16)
union_membership

work_time <- sv_dependence(shap1021_XGB_1$shap_values, v = "每周工作时长" ,color_var = NULL) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, color = "skyblue", linewidth = 1.2) + 
  ggtitle("每周工作时长") +
  theme_bw(base_size = 16)
work_time

Happiness <- sv_dependence(shap1021_XGB_1$shap_values, v = "幸福感" )  + 
  ggtitle("幸福感") +
  theme_bw(base_size = 16)
Happiness

F_ISEI <- sv_dependence(shap1021_XGB_1$shap_values, v = "感到抑郁和沮丧的频率" ,color_var = NULL) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, color = "skyblue", linewidth = 1.2) +
  theme_bw(base_size = 16)
F_ISEI

ISEI <- sv_dependence(shap1021_XGB_1$shap_values, v = "受教育年限",color_var = NULL) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, color = "skyblue", linewidth = 1.2) + 
  theme_bw(base_size = 16)
ISEI

house_ownership <- sv_dependence(shap1021_XGB_1$shap_values, v = '居住房屋是否本人所有',color_var = "auto" ) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, color = "skyblue", linewidth = 1.2) + 
  ggtitle("房屋所有权(是否是自己)") +
  theme_bw(base_size = 16)
house_ownership

fincome <- sv_dependence(shap1021_XGB_1$shap_values, v = '家庭去年收入',color_var = NULL ) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.6, color = "skyblue", linewidth = 1.2) +
  coord_cartesian(xlim = c(1, 15)) + 
  ggtitle("家庭去年收入（对数+CPI加权）") +
  theme_bw(base_size = 16)
fincome

income <- sv_dependence(shap1021_XGB_1$shap_values, v = '个人去年收入',color_var = NULL) + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.4, color = "skyblue", linewidth = 1.2) +
  coord_cartesian(xlim = c(6, 15)) + 
  ggtitle("个人去年收入（CPI加权+对数）") +
  theme_bw(base_size = 16)
income
 
interaction_sense_age <-Year_effect/(sense + age)
interaction_sense_age                            

interaction_relative_learn <- (visit_relative + learn)
interaction_relative_learn
interaction_relative_l_year <- (music + Happiness)
interaction_non <- (subjective_ide + city_worlk_time)/(union_membership + work_time)
non_liner <- living_space
fertilization <-  (birth_will + gender_attitude)/politic_attitude
fertilization
left <- old + madarain
left
interaction_non

house <- sv_dependence(shap1021_XGB_1$shap_values, v = '居住面积',color_var = 'auto') + 
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.8, color = "skyblue", linewidth = 1.2) +
  theme_bw(base_size = 16)
house



income_related <- fincome + income
income_related

ISEI_COMBIND <- F_ISEI+ISEI 
ISEI_COMBIND

saveRDS(model1021_xgb$model, "xgboost_model.rds")
xgb.save(model1021_xgb$model, "xgboost_model.model") 

shap_matrix <- C1021_XGB_1$S

# 计算特征平均绝对SHAP值（按贡献量降序排列）
feature_importance <- colMeans(abs(shap_matrix)) %>% 
  sort(decreasing = FALSE)  # 升序排列便于取后50%

# 确定截断位置
n_features <- length(feature_importance)
cutoff <- ceiling(n_features * 0.5)  # 取50%分位点

# 提取后50%变量名
bottom_50_vars <- names(feature_importance)[cutoff:n_features] %>% 
  na.omit()

# 查看结果
print(bottom_50_vars)
