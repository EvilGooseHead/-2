library(shapviz)             
library(xgboost)
library(caret)                
library(pROC)
library(tibble)
library(ROCit)
library(dplyr)
library(broom)
library(ggplot2)
library(dplyr)
library(lightgbm)
library(catboost)
library(DMwR2)
library(progress)
library(showtext)
library(patchwork)
showtext_auto()   

shap_feature_importance <- function(model, 
                                    sample_size = 1000, 
                                    seed = 123
                                    ) {
   
  # 参数校验
  if (!all(c("model", "test_features") %in% names(model))) {
    stop("模型对象必须包含 'model' 和 'test_features' 元素")
  }
  
  # 设置随机种子保证可重复性
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
  
  # 2. 计算SHAP值
  shap_obj <- shapviz::shapviz(
    object = model$model, 
    X_pred = X_explain,
    X = X
  )
  
  
  #读取变量名映射表，将变量名替换为标签
  var_label_df <- read.csv("C:/Users/USER/Desktop/毕业设计/变量与标签.csv")
  feature_mapping <- setNames(var_label_df$label, var_label_df$variable)
  feature_names <- colnames(shap_obj$X)
  new_feature_names <- sapply(feature_names, function(x) {
    ifelse(x %in% names(feature_mapping), feature_mapping[x], x)
  })
  colnames(shap_obj$X) <- new_feature_names
  colnames(shap_obj$S) <- new_feature_names
  
  #定义特征
  feature_default <- list(
                         plot.title = element_text(size = 32, face = "bold", family = 'HT'),
                         plot.subtitle = element_text(size = 26, family = 'HT'),
                         axis.title.x = element_text(size = 28, face = "bold", family = 'HT'),
                         axis.title.y = element_text(size = 24, face = "bold", family = 'HT'),
                         legend.text = element_text(size = 22, family = 'ST'),
                         legend.title = element_text(size = 24, face = "bold", family = 'HT'),
                         axis.text.x = element_text(size = 22, family = 'ST'),
                         axis.text.y = element_text(size = 24, family = 'ST'),
                         panel.grid.major = element_blank(),  # 去除主网格线（粗线）
                         panel.grid.minor = element_blank(),   # 去除次网格线（细线）
                         # 可选设置：
                         panel.background = element_blank()
                          )
  
  #蜂群图
  beeswarm_plot <- sv_importance(shap_obj, kind = "both", max_display = 30L,show_numbers = TRUE,fill = "#B0E0E6",bar_width = 3/4,bee_width = 0.3) + 
    ggtitle(label = "SHAP值蜂群/条形堆叠图") + 
    theme_bw(base_size = 16)+
    theme(feature_default)
  
  # 预测结果计算
  predicted <- predict(model$model, X_explain, type = "response")
  predicted_labels <- ifelse(predicted > 0.51, 1, 0)
  
  # 计算TP/TN案例
  tp_cases <- which(sampled_labels == 1 & predicted_labels == 1)
  tn_cases <- which(sampled_labels == 0 & predicted_labels == 0)
  
  # 生成瀑布图
  tp_plot <- if(length(tp_cases) >= 1) {
    sv_waterfall(shap_obj, tp_cases[38], max_display = 10) + 
      ggtitle(label ="真正(TP)个案瀑布图(前10个重要特征)")+
      theme_bw(base_size = 16)+
      theme(feature_default)
  } else NULL
  
  tn_plot <- if(length(tn_cases) >= 1) {
    sv_waterfall(shap_obj, tn_cases[28], max_display = 10) + 
      ggtitle(label ="真反(TN)个案瀑布图(前10个重要特征)")+
      theme_bw(base_size = 16)+
      theme(feature_default)
  } else NULL
  
  # 生成力图
  feature_default <- list(
    text = element_text(family = "HT"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    panel.grid.major = element_blank()
  )
  
  # 生成子图
  TPF <- sv_force(shap_obj, tp_cases[50], max_display = 6) + 
    labs(title = "真正(TP)个案特征贡献（前6）") +
    theme_bw(base_size = 16) +
    theme(feature_default)
  
  TNF <- sv_force(shap_obj, tn_cases[28], max_display = 6) + 
    labs(title = "真反(TN)个案特征贡献（前6）") +
    theme_bw(base_size = 16) +
    theme(feature_default)
  
  # 合并并添加标题
  final_plot <- (TPF / TNF) +
    plot_annotation(
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      )
    ) 
  # 返回
  list(
    shap_values = shap_obj,
    beeswarm = beeswarm_plot,
    TP = tp_plot,
    TN = tn_plot,
    FPlot = final_plot,
    X_explain = X_explain,
    sample_idx = sample_idx
  )
}
show_eval <- function(model) {
  # 检查模型是否包含所需的指标
  required_metrics <- c("train_auc", "train_precision", "train_recall", 
                        "train_accuracy", "train_confusion_matrix", 
                        "test_auc", "test_recall", "test_accuracy", 
                        "test_precision", "test_f1", "test_confusion_matrix")
  
  missing_metrics <- setdiff(required_metrics, names(model))
  if (length(missing_metrics) > 0) {
    stop(paste("模型中缺少以下指标:", paste(missing_metrics, collapse = ", ")))
  }
  
  # 打印训练集指标
  cat("\n=== 训练集 (Train) 指标 ===\n")
  cat(sprintf("AUC: %.4f\n", model$train_auc))
  cat(sprintf("Precision: %.4f\n", model$train_precision))
  cat(sprintf("Recall: %.4f\n", model$train_recall))
  cat(sprintf("Accuracy: %.4f\n", model$train_accuracy))
  
  # 打印训练集混淆矩阵
  cat("\n训练集 Confusion Matrix:\n")
  print(model$train_confusion_matrix)
  
  # 打印测试集指标
  cat("\n=== 测试集 (Test) 指标 ===\n")
  cat(sprintf("AUC: %.4f\n", model$test_auc))
  cat(sprintf("Precision: %.4f\n", model$test_precision))
  cat(sprintf("Recall: %.4f\n", model$test_recall))
  cat(sprintf("Accuracy: %.4f\n", model$test_accuracy))
  cat(sprintf("F1 Score: %.4f\n", model$test_f1))
  
  # 打印测试集混淆矩阵
  cat("\n测试集 Confusion Matrix:\n")
  print(model$test_confusion_matrix)
}

lgb_final_train <- function(
    Data, 
    test_ratio = 0.3,
    # LightGBM 主要参数
    num_leaves = 6,       
    max_depth = 4,          
    learning_rate = 0.05,
    lambda_l1 = 5,           
    lambda_l2 = 10,          
    n_estimators = 300,
    drop_rate= 0.1,
    feature_fraction = 0.8,  
    bin_construct_sample_cnt = 200000, 
    min_data_in_leaf = 10,
    cat_l2 = 10,
    cat_smooth = 10,
    path_smooth = 0,
    feature_fraction_bynode = 1,
    # 其他选项
    early_stopping_rounds = 100,
    eval_metric = "auc",
    # 分类阈值
    threshold = 0.5,
    # 交叉验证阈值
    nfold = 4
) {
  
  # 1. 数据预处理
  features <- Data[, -which(names(Data) == "expectation"), drop = FALSE]
  labels <- as.numeric(as.character(Data$expectation))  # 转换为0/1数值
  
  # 2. 识别类别特征
  categorical_cols <- names(features)[sapply(features, function(x) {
    is.factor(x) | is.character(x)
  })]
  
  # 3. 划分训练集和测试集
  set.seed(42)
  train_idx <- createDataPartition(labels, p = 1 - test_ratio, list = FALSE)
  train_data <- features[train_idx, ]
  train_labels <- labels[train_idx]
  test_data <- features[-train_idx, ]
  test_labels <- labels[-train_idx]
  
  
  # 4. 创建LightGBM数据集
  dtrain <- lgb.Dataset(
    data = data.matrix(train_data),
    label = train_labels,
    categorical_feature =  categorical_cols
  )
  
  dtest <- lgb.Dataset(
    data = data.matrix(test_data),
    label = test_labels,
    categorical_feature =  categorical_cols
  )
  
  dcv <- lgb.Dataset(data = data.matrix(features),
                     label = labels,
                     categorical_feature =  categorical_cols)
  # 5. 模型参数
  params <- list(
    objective = "binary",
    metric = eval_metric,
    learning_rate = learning_rate,
    num_leaves = num_leaves,
    max_depth = max_depth,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    feature_fraction = feature_fraction,
    tree_learner = 'feature',
    boosting_type = "dart",
    drop_rate=drop_rate,
    feature_fraction_bynode = feature_fraction_bynode,
    max_drop = -1,
    is_unbalance =TRUE,  # 处理类别不平衡
    verbosity = 1,
    seed = 42,
    cat_l2 = cat_l2,
    cat_smooth =  cat_smooth,
    path_smooth = path_smooth,
    min_data_in_leaf = min_data_in_leaf,
    num_threads = parallel::detectCores(),
    bin_construct_sample_cnt = bin_construct_sample_cnt
  )
  
  #5.交叉验证并绘制AUC图
  cv_result <- lgb.cv(
    params = params,
    data = dcv,
    nfold = nfold,
    nrounds = n_estimators,
    eval = "auc",
    early_stopping_rounds = early_stopping_rounds,
    verbose = 1
  )
  
  #cv结果整理 
  plot_data <- data.frame(
    iter = seq_along(unlist(cv_result$record_evals$valid$auc$eval)),
    valid_auc_mean = unlist(cv_result$record_evals$valid$auc$eval),
    valid_auc_stdv = unlist(cv_result$record_evals$valid$auc$eval_err)
  )
  
  
  library(ggplot2)
  cvp <- ggplot(plot_data, aes(iter, valid_auc_mean)) +
    geom_line(color = "blue") +
    geom_ribbon(
      aes(
        ymin = valid_auc_mean - valid_auc_stdv,  
        ymax = valid_auc_mean + valid_auc_stdv
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
  
  # 6. 训练模型
  model <- lgb.train(
    params = params,
    data = dtrain,
    valids = list(test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    nrounds = n_estimators,
    eval_freq = 5
  )
  
  # 7. 获取评估结果
  eval_log <- model$record_evals
  best_iter <- model$best_iter
  
  # 8. 预测概率
  test_pred_prob <- predict(
    model, 
    data.matrix(test_data),
    num_iteration = best_iter
  )
  
  train_pred_prob <- predict(
    model, 
    data.matrix(train_data),
    num_iteration = best_iter
  )
  
  # 9. 计算指标
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
  
  # 10. 返回结果
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
    #图片输出
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
    # 特征信息
    categorical_cols = categorical_cols,
    feature_types = sapply(features, function(x) class(x)[1]),
    threshold = threshold,
    best_iteration = best_iter
  )
}

lgb_final_train2 <- function(
    Data, 
    test_ratio = 0.3,
    # LightGBM 主要参数
    num_leaves = 6,       
    max_depth = 4,          
    learning_rate = 0.05,
    lambda_l1 = 5,           
    lambda_l2 = 10,          
    n_estimators = 300,
    drop_rate= 0.1,
    feature_fraction = 0.8,  
    bin_construct_sample_cnt = 200000, 
    min_data_in_leaf = 10,
    cat_l2 = 10,
    cat_smooth = 10,
    path_smooth = 0,
    feature_fraction_bynode = 1,
    # 其他选项
    early_stopping_rounds = 100,
    eval_metric = "auc",
    # 分类阈值
    threshold = 0.5
    # 交叉验证阈值
) {
  
  # 1. 数据预处理
  features <- Data[, -which(names(Data) == "expectation"), drop = FALSE]
  labels <- as.numeric(as.character(Data$expectation))  # 转换为0/1数值
  
  # 2. 识别类别特征
  categorical_cols <- names(features)[sapply(features, function(x) {
    is.factor(x) | is.character(x)
  })]
  
  # 3. 划分训练集和测试集
  set.seed(42)
  train_idx <- createDataPartition(labels, p = 1 - test_ratio, list = FALSE)
  train_data <- features[train_idx, ]
  train_labels <- labels[train_idx]
  test_data <- features[-train_idx, ]
  test_labels <- labels[-train_idx]
  
  
  # 4. 创建LightGBM数据集
  dtrain <- lgb.Dataset(
    data = data.matrix(train_data),
    label = train_labels,
    categorical_feature =  categorical_cols
  )
  
  dtest <- lgb.Dataset(
    data = data.matrix(test_data),
    label = test_labels,
    categorical_feature =  categorical_cols
  )
  
  dcv <- lgb.Dataset(data = data.matrix(features),
                     label = labels,
                     categorical_feature =  categorical_cols)
  # 5. 模型参数
  params <- list(
    objective = "binary",
    metric = eval_metric,
    learning_rate = learning_rate,
    num_leaves = num_leaves,
    max_depth = max_depth,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    feature_fraction = feature_fraction,
    tree_learner = 'feature',
    boosting_type = "dart",
    drop_rate=drop_rate,
    feature_fraction_bynode = feature_fraction_bynode,
    max_drop = -1,
    is_unbalance =TRUE,  # 处理类别不平衡
    verbosity = 1,
    seed = 42,
    cat_l2 = cat_l2,
    cat_smooth =  cat_smooth,
    path_smooth = path_smooth,
    min_data_in_leaf = min_data_in_leaf,
    num_threads = parallel::detectCores(),
    bin_construct_sample_cnt = bin_construct_sample_cnt
  )
  
  # 6. 训练模型
  model <- lgb.train(
    params = params,
    data = dtrain,
    valids = list(test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    nrounds = n_estimators,
    eval_freq = 5
  )
  
  # 7. 获取评估结果
  eval_log <- model$record_evals
  best_iter <- model$best_iter
  
  # 8. 预测概率
  test_pred_prob <- predict(
    model, 
    data.matrix(test_data),
    num_iteration = best_iter
  )
  
  train_pred_prob <- predict(
    model, 
    data.matrix(train_data),
    num_iteration = best_iter
  )
  
  # 9. 计算指标
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
  
  # 10. 返回结果
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
    # 特征信息
    categorical_cols = categorical_cols,
    feature_types = sapply(features, function(x) class(x)[1]),
    threshold = threshold,
    best_iteration = best_iter
  )
}

model2010 <-  lgb_final_train(CGSS2010)
shap2010 <- shap_feature_importance(model2010)
shap2010$beeswarm

model2021 <-  lgb_final_train(CGSS2021)
shap2021 <- shap_feature_importance(model2021)
shap2021$beeswarm


model1021 <- lgb_final_train(CGSS1021,
                             test_ratio = 0.2,
                             # LightGBM 主要参数
                             num_leaves = 50,       
                             max_depth = 8,      
                             learning_rate = 0.03,
                             lambda_l1 = 14,           
                             lambda_l2 = 10,          
                             n_estimators = 1200,
                             feature_fraction = 0.6,    
                             bin_construct_sample_cnt = 20000, 
                             cat_l2 = 10,
                             cat_smooth = 10,
                             path_smooth = 0.1,
                             feature_fraction_bynode = 0.6,
                             # 其他选项
                             early_stopping_rounds = 200,
                             min_data_in_leaf = 20,
                             eval_metric = "AUC",
                             # 分类阈值
                             threshold = 0.502,
                             nfold = 5)

show_eval(model1021)
cv1021_XGB <- model1021$cvresult
shap1021_1 <- shap_feature_importance(model1021)
C1021_LGB_1 <- shap1021_1$shap_values
shap1021_1$beeswarm


model1021_2 <- lgb_final_train2(CGSS1021,
                             test_ratio = 0.3,
                             # LightGBM 主要参数
                             num_leaves = 40,       
                             max_depth = 6,      
                             learning_rate = 0.05,
                             lambda_l1 = 16,           
                             lambda_l2 = 12,          
                             n_estimators = 1200,
                             feature_fraction = 0.8,    
                             bin_construct_sample_cnt = 20000, 
                             cat_l2 = 10,
                             cat_smooth = 10,
                             path_smooth = 0.1,
                             feature_fraction_bynode = 0.8,
                             # 其他选项
                             early_stopping_rounds = 200,
                             min_data_in_leaf = 30,
                             eval_metric = "AUC",
                             # 分类阈值
                             threshold = 0.502)
shap1021_2 <- shap_feature_importance(model1021_2)
C1021_LGB_2 <- shap1021_2$shap_values

model1021_3 <- lgb_final_train2(CGSS1021,
                               test_ratio = 0.3,
                               # LightGBM 主要参数
                               num_leaves = 80,       
                               max_depth = 8,      
                               learning_rate = 0.02,
                               lambda_l1 = 12,           
                               lambda_l2 = 8,          
                               n_estimators = 1600,
                               feature_fraction = 0.6,    
                               bin_construct_sample_cnt = 20000, 
                               cat_l2 = 10,
                               cat_smooth = 10,
                               path_smooth = 0.1,
                               feature_fraction_bynode = 0.6,
                               # 其他选项
                               early_stopping_rounds = 200,
                               min_data_in_leaf = 30,
                               eval_metric = "AUC",
                               # 分类阈值
                               threshold = 0.502)
shap1021_3 <- shap_feature_importance(model1021_3)
C1021_LGB_3 <- shap1021_3$shap_values

model1013 <- lgb_final_train(CGSS1013,
                               test_ratio = 0.2,
                               # LightGBM 主要参数
                               num_leaves = 40,       
                               max_depth = 8,      
                               learning_rate = 0.03,
                               lambda_l1 = 8,           
                               lambda_l2 = 14,          
                               n_estimators = 1200,
                               feature_fraction = 0.8,    
                               bin_construct_sample_cnt = 20000, 
                               cat_l2 = 10,
                               cat_smooth = 10,
                               path_smooth = 0.5,
                               feature_fraction_bynode = 0.8,
                              # 其他选项
                               early_stopping_rounds = 200,
                               min_data_in_leaf = 20,
                               eval_metric = "AUC",
                               # 分类阈值
                               threshold = 0.505,
                               nfold = 5)

model1013$train_auc
model1013$train_precision
model1013$train_recall
model1013$train_accuracy
model1013$train_confusion_matrix
model1013$test_auc
model1013$test_recall
model1013$test_accuracy
model1013$test_precision
model1013$test_f1
model1013$test_auc
model1013$test_confusion_matrix
model1013$cvresult
shap1013 <- shap_feature_importance(model1013)
C1013_LGB_1<- shap1013$shap_values
shap_beewarm_1013 <- shap1013$beeswarm
C1013TN <- shap1013$TN
C1013TP <- shap1013$TP
C1013FPlot<- shap1013$FPlot

sv_dependence(shap1013$shap_values,
              v="居住面积") + geom_smooth(method = "loess",   
                                             formula = y ~ x,    
                                             se = TRUE,          
                                             color = "skyblue",     
                                             linewidth = 1.2) +   
              ggtitle("住房面积特征贡献（2010和2013年合并数据集）") +
              theme_bw(base_size = 16)
sv_dependence(shap1013$shap_values,
              v="流动感知",
             ) 

model1013_2 <- lgb_final_train2(CGSS1013,
                             test_ratio = 0.2,
                             # LightGBM 主要参数
                             num_leaves = 40,       
                             max_depth = 6,      
                             learning_rate = 0.02,
                             lambda_l1 = 14,           
                             lambda_l2 = 12,          
                             n_estimators = 1400,
                             feature_fraction = 0.6,    
                             bin_construct_sample_cnt = 20000, 
                             cat_l2 = 10,
                             cat_smooth = 10,
                             path_smooth = 0.1,
                             feature_fraction_bynode = 0.8,
                             # 其他选项
                             early_stopping_rounds = 200,
                             min_data_in_leaf = 20,
                             eval_metric = "AUC",
                             # 分类阈值
                             threshold = 0.51)
shap1013_2 <- shap_feature_importance(model1013_2)
C1013_LGB_2 <- shap1013_2$shap_values

model1013_3 <- lgb_final_train2(CGSS1013,
                               test_ratio = 0.2,
                               # LightGBM 主要参数
                               num_leaves = 30,       
                               max_depth = 4,      
                               learning_rate = 0.02,
                               lambda_l1 = 14,           
                               lambda_l2 = 12,          
                               n_estimators = 1200,
                               feature_fraction = 0.8,    
                               bin_construct_sample_cnt = 20000, 
                               cat_l2 = 10,
                               cat_smooth = 10,
                               path_smooth = 0.1,
                               feature_fraction_bynode = 1,
                               # 其他选项
                               early_stopping_rounds = 200,
                               min_data_in_leaf = 20,
                               eval_metric = "AUC",
                               # 分类阈值
                               threshold = 0.505)
shap1013_3 <- shap_feature_importance(model1013_3)
C1013_LGB_3 <- shap1013_3$shap_values

model1821_1 <- lgb_final_train(CGSS1821,
                               test_ratio = 0.2,
                               # LightGBM 主要参数
                               num_leaves = 50,       
                               max_depth = 8,      
                               learning_rate = 0.03,
                               lambda_l1 = 12,           
                               lambda_l2 = 10,          
                               n_estimators = 1000,
                               feature_fraction = 0.4,    
                               bin_construct_sample_cnt = 20000, 
                               cat_l2 = 10,
                               cat_smooth = 10,
                               path_smooth = 0.2,
                               feature_fraction_bynode = 0.8,
                               # 其他选项
                               early_stopping_rounds = 1000,
                               min_data_in_leaf = 18,
                               eval_metric = "AUC",
                               # 分类阈值
                               threshold = 0.505,
                               nfold = 5)

model1821_1$train_precision
model1821_1$test_accuracy
model1821_1$test_confusion_matrix
model1821_1$train_auc
model1821_1$test_precision
model1821_1$test_accuracy
model1821_1$train_accuracy

C1821_1auc <- model1821_1$test_auc
C1821_1recall <- model1821_1$test_recall
C1821_1accuracy <- model1821_1$test_accuracy
C1821_1acprec <- model1821_1$test_precision
C1821_1af1 <- model1821_1$test_f1
C1821_1afconfm <- model1821_1$test_confusion_matrix
cv1821<- model1821_1$cvp
shap1821 <- shap_feature_importance(model1821_1,seed =231)
C1821_LGB_1 <- shap1821$shap_values
shap_beewarm_1821 <- shap1821$beeswarm 

model1821_1$cvresult
sv_dependence(shap1821$shap_values,
              v="居住面积")  + geom_smooth(method = "loess",   # 拟合方法
                                       formula = y ~ x,    # 公式
                                       se = TRUE, 
                                       span = 0.9,# 显示置信区间
                                       color = "skyblue",      # 曲线颜色
                                       linewidth = 1.2) +   # 线宽
  ggtitle("住房面积特征贡献（2018和2021年合并数据集）") +
  theme_bw(base_size = 16)

sv_dependence(shap1821$shap_values,
              v="家庭去年收入")  + geom_smooth(method = "loess",   # 拟合方法
                                       formula = y ~ x, 
                                       span = 0.9,# 公式
                                       se = TRUE,          # 显示置信区间
                                       color = "skyblue",      # 曲线颜色
                                       linewidth = 1.2) +   # 线宽
  ggtitle("家庭去年收入（对数）特征贡献（2018和2021年合并数据集）") +
  theme_bw(base_size = 16)

model1821_2 <- lgb_final_train2(CGSS1821,
                               test_ratio = 0.2,
                               # LightGBM 主要参数
                               num_leaves = 40,       
                               max_depth = 6,      
                               learning_rate = 0.05,
                               lambda_l1 = 14,           
                               lambda_l2 = 8,          
                               n_estimators = 1000,
                               feature_fraction = 0.6,    
                               bin_construct_sample_cnt = 20000, 
                               cat_l2 = 10,
                               cat_smooth = 8,
                               path_smooth = 0.1,
                               feature_fraction_bynode = 0.8,
                               # 其他选项
                               early_stopping_rounds = 1000,
                               min_data_in_leaf = 18,
                               eval_metric = "AUC",
                               # 分类阈值
                               threshold = 0.51)

shap1821_2 <- shap_feature_importance(model1821_2)
C1821_LGB_2 <- shap1821_2$shap_values

model1821_3 <- lgb_final_train2(CGSS1821,
                               test_ratio = 0.3,
                               # LightGBM 主要参数
                               num_leaves = 30,       
                               max_depth = 5,      
                               learning_rate = 0.08,
                               lambda_l1 = 16,           
                               lambda_l2 = 10,          
                               n_estimators = 1000,
                               feature_fraction = 0.4,    
                               bin_construct_sample_cnt = 20000, 
                               cat_l2 = 10,
                               cat_smooth = 8,
                               path_smooth = 0.1,
                               feature_fraction_bynode = 0.8,
                               # 其他选项
                               early_stopping_rounds = 1000,
                               min_data_in_leaf = 20,
                               eval_metric = "AUC",
                               # 分类阈值
                               threshold = 0.51)

shap1821_3 <- shap_feature_importance(model1821_2)
C1821_LGB_3 <- shap1821_3$shap_values

cvp1013 <- model1013$cvp +
  ggtitle("2010-2013年合并数据集最优参数K折交叉验证AUC曲线(LightGBM)") +
  scale_y_continuous(
    breaks = sort(c(0.60,0.64,0.68,0.6907)), # 确保包含0.70
    labels = function(x) ifelse(x == 0.6907, "0.6907", sprintf("%.2f", x))
  ) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 18,face ='bold'), 
        axis.title.x = element_text(size = 18, face ='bold'),
        plot.title = element_text(
          size = 16,                # 标题字体大小
          face = "bold",           # 加粗
          color = "black",         # 字体颜色
          hjust = 0,             # 水平居中
          margin = margin(b = 5)  # 底部边距
        )
  )
cvp1013

cvp1821 <- model1821_1$cvp +
  ggtitle("2018-2021年合并数据集最优参数K折交叉验证AUC曲线(LightGBM)") +
  scale_y_continuous(
    breaks = sort(c(0.60,0.64,0.68,0.7136)), # 确保包含0.70
    labels = function(x) ifelse(x == 0.7136, "0.7136", sprintf("%.2f", x))
  ) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 18,face ='bold'), 
        axis.title.x = element_text(size = 18, face ='bold'),
        plot.title = element_text(
          size = 16,                # 标题字体大小
          face = "bold",           # 加粗
          color = "black",         # 字体颜色
          hjust = 0,             # 水平居中
          margin = margin(b = 5)  # 底部边距
        )
  )
cvp1821

combined_1018t <- wrap_plots(cvp1013, cvp1821, ncol = 2)+ 
  plot_annotation(
    caption = "阴影：±1个标准差，基于n折交叉验证",
    theme = theme(plot.caption = element_text(size=14,hjust = 0.5))
  )

combined_1018t
shap_beewarm_1013 <- shap_beewarm_1013 +
  theme(
    plot.title = element_text(hjust = 0),    # 主标题左对齐（hjust=0）
    plot.subtitle = element_text(hjust = 0),
    plot.margin = margin(              # 动态边距（上,右,下,左）
      t = 20,
      r = 50,         # 右边距
      b = 20,                          # 下边距
      l = 5                           # 左边距
    )# 副标题左对齐
  )

shap_beewarm_1821 <- shap_beewarm_1821 +
  theme(
    plot.title = element_text(hjust = 0),    # 主标题左对齐（hjust=0）
    plot.subtitle = element_text(hjust = 0),
    plot.margin = margin(              # 动态边距（上,右,下,左）
      t = 20,
      r = 50,         # 右边距
      b = 20,                          # 下边距
      l = 5                         # 左边距
    )# 副标题左对齐
  )

combined_plot <- shap_beewarm_1013 + shap_beewarm_1821
combined_plot

Lnincome2 <- sv_dependence(shap1013$shap_values, v = "个人去年收入" ,color_var = NULL) + 
  ggtitle("个人去年收入") +
  geom_smooth(method = "loess", formula = y ~ x, 
              se = TRUE, span = 0.4, linewidth = 1.2,color='skyblue')+
  labs(title = "个人去年收入与生育意愿\n（颜色表示生育意愿）")+
  coord_cartesian(xlim = c(6, 15))+
  theme_bw(base_size = 16)
Lnincome2

