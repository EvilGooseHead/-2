library(haven)
library(dplyr)

data <- read_sav("C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据/CGSS2018重编码.sav")

# 计算加权基尼系数（使用reldist包）
if (!require(reldist)) install.packages("reldist")
library(reldist)

# 计算各社区加权基尼系数
gini_results <- data %>%
  filter(!is.na(A62), !is.na(weight), weight > 0) %>%
  group_by(city) %>%
  summarise(
    Gini = gini(A62, weight),  # 使用reldist包的加权基尼函数
    Obs = n()
  )

# 合并结果到原数据
data_with_gini <- data %>% 
  left_join(gini_results, by = "city")

# 覆盖原文件
write_sav(data_with_gini, "C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据/CGSS2018重编码.sav")


library(readxl)
library(dplyr)
library(openxlsx)
file_path <- "C:/Users/USER/Desktop/毕业设计/数据/CGSS/最低工资筛选后.xlsx"
data <- read_excel(file_path, sheet = "Sheet1")

# 2. 检查数据维度
cat("原始数据行数:", nrow(data), "\n")

# 3. 正确的问题数据筛选方法
problem_data <- data %>%
  mutate(
    最低月工资_数值 = as.numeric(最低月工资),  # 尝试转换为数值
    问题类型 = case_when(
      is.na(最低月工资) ~ "缺失值",
      最低月工资 == "" ~ "空字符串",
      is.na(最低月工资_数值) ~ "非数值",
      TRUE ~ "有效"
    )
  ) %>%
  filter(问题类型 != "有效")  # 筛选出有问题的记录

# 4. 查看问题数据摘要
cat("\n=== 数据质量问题汇总 ===\n")
cat("总记录数:", nrow(data), "\n")
cat("有效记录数:", nrow(data) - nrow(problem_data), "\n")
cat("问题记录数:", nrow(problem_data), "\n")
cat("问题类型分布:\n")
print(table(problem_data$问题类型))

# 5. 处理有效数据
valid_data <- data %>%
  mutate(最低月工资 = as.numeric(最低月工资)) %>%
  filter(!is.na(最低月工资))  # 只保留有效数值

# 6. 按年度和区县统计
result <- valid_data %>%
  group_by(统计年度, 省份名称, 城市名称) %>%
  summarise(
    月最低工资均值 = round(mean(最低月工资, na.rm = TRUE), 2),
    记录数 = n(),
    .groups = "drop"
  ) %>%
  arrange(统计年度, 省份名称, 城市名称)

# 7. 保存结果
output_path <- "C:/Users/USER/Desktop/毕业设计/数据/CGSS/最低工资统计结果（地级市）.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "统计结果")
writeData(wb, "统计结果", result)
addWorksheet(wb, "问题数据")
writeData(wb, "问题数据", problem_data)
saveWorkbook(wb, output_path, overwrite = TRUE)

cat("\n结果已保存至:", output_path)

#区县名称转换
excel_data <- read_excel("C:/Users/USER/Desktop/毕业设计/数据/CGSS/地级市对应关系.xlsx")  # 替换为你的Excel文件路径

# 2. 读取SPSS文件
spss_data <- read_sav("C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据/CGSS2018重编码.sav")  # 替换为你的SPSS文件路径

excel_data_unique <- excel_data %>%
  distinct(S42, .keep_all = TRUE)

# 3. 仅保留与 SPSS 匹配的 S42
excel_matched <- excel_data_unique %>%
  semi_join(spss_data, by = "S42")

# 4. 合并数据
merged_data <- spss_data %>%
  left_join(excel_matched, by = "S42")
# 4. 覆盖原SPSS文件
write_sav(merged_data, "C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据/CGSS2018重编码.sav")  # 使用相同路径覆盖原文件


#计算区域经济指标
data <- read_excel("C:/Users/USER/Desktop/毕业设计/数据/CGSS/地区经济发展指标.xlsx", sheet = "Sheet1")

# 按年份和城市分组，计算除'区县'外变量的和
grouped_data <- data %>%
  group_by(年份, 城市) %>%
  summarise(
    户籍人口数_万人 = sum(户籍人口数_万人, na.rm = TRUE),
    地区生产总值_万元 = sum(地区生产总值_万元, na.rm = TRUE),
    第二产业增加值_万元 = sum(第二产业增加值_万元, na.rm = TRUE),
    医院卫生院床位数_床 = sum(`医院、卫生院床位数_床`, na.rm = TRUE),
    乡村人口_万人 = sum(乡村人口_万人, na.rm = TRUE)
  ) %>%
  ungroup()

# 计算新增指标
result_data <- grouped_data %>%
  mutate(
    地区人均生产总值 = 地区生产总值_万元 / 户籍人口数_万人,
    人均医院卫生院床位数 = 医院卫生院床位数_床 / 户籍人口数_万人
  )

# 将结果保存为新的Excel文件
write_xlsx(result_data, "C:/Users/USER/Desktop/毕业设计/数据/CGSS/地区经济发展指标计算结果.xlsx")

#合并地区经济发展数据
data1 <- read_excel("C:/Users/USER/Desktop/毕业设计/数据/CGSS/地区经济发展指标计算结果.xlsx") %>%
  mutate(across(everything(), as.character)) # 统一为字符类型避免合并问题

# 读取第二个Excel文件
data2 <- read_excel("C:/Users/USER/Desktop/毕业设计/数据/CGSS/最低工资统计结果（地级市）.xlsx") %>%
  mutate(across(everything(), as.character))

# 根据统计年度和城市名称合并两个数据集
merged_data <- full_join(data1, data2, 
                         by = c("统计年度" = "统计年度", "城市名称" = "城市名称"))

# 或者使用left_join/right_join/inner_join根据需求选择合并方式
# merged_data <- left_join(data1, data2, by = c("统计年度", "城市名称"))

# 将合并后的数据保存为新的Excel文件
write_xlsx(merged_data, "C:/Users/USER/Desktop/毕业设计/数据/CGSS/最低工资统计结果（地级市）.xlsx")

#合并到sav中
spss_data <- read_sav('C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据/CGSS2021重编码.sav')

# 2. 读取Excel文件
excel_data <- read_excel("C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据/经济社会发展.xlsx") %>%
  # 筛选2010年的数据
  filter(Year == 2021) %>%
  # 统一城市名称变量名（确保与SPSS中的city变量对应）
  rename(city = 城市名称)

# 3. 合并数据（左连接，保留SPSS原有数据）
merged_data <- spss_data %>%
  left_join(excel_data, by = "city")

# 4. 覆盖保存到原SPSS文件
write_sav(merged_data, "C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据/CGSS2021重编码.sav")


