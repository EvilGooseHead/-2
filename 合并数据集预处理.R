library(tibble)
library(dplyr)
library(broom)
#library(haven)
library(ggplot2)
library(dplyr)
library(lattice)
library(shapper)
library(occupar)

#setwd('C:/Users/ZLY/Desktop/学习/毕业设计/数据/CGSS/可以用的数据')
setwd('C:/Users/USER/Desktop/毕业设计/数据/CGSS/可以用的数据')
#setwd('C:/Users/admin/Desktop/bbyの文件')
rm(list = ls())
CGSS1821 <- haven::read_sav("CGSS1821.sav")
CGSS1013 <- haven::read_sav("CGSS1013.sav")

CGSS1821 <- haven::zap_labels(CGSS1821)
# 移除CGSS1013数据框中所有变量的值标签
CGSS1013 <- haven::zap_labels(CGSS1013)

ctiy_to_factor <- function(data){
 city_level <- list(levels =c("上海市", "大理白族自治州", "昆明市", "昭通市", "玉溪市", 
                        "呼和浩特市", "北京市", "吉林市", "松原市", "白城市", 
                        "长春市", "乐山市", "南充市", "宜宾市", "成都市", "泸州市", 
                        "眉山市", "天津市", "吴忠市", "宣城市", "蚌埠市", "阜阳市",
                        "黄山市", "临沂市", "日照市", "泰安市", "济宁市", "烟台市", 
                        "太原市", "运城市", "长治市", "广州市", "汕头市", "深圳市", 
                        "崇左市", "柳州市", "玉林市", "喀什市", "南京市", "常州市", 
                        "徐州市", "扬州市", "连云港市", "上饶市", "宜春市", "赣州市", 
                        "鹰潭市", "唐山市", "沧州市", "邯郸市", "信阳市", "周口市", 
                        "商丘市", "洛阳市", "漯河市", "宁波市", "杭州市", "温州市", 
                        "湖州市", "海口市", "孝感市", "恩施州", "武汉市", "襄樊市", 
                        "怀化市", "邵阳市", "郴州市", "兰州市", "庆阳市", "三明市", 
                        "漳州市", "福州市", "拉萨市", "安顺市", "贵阳市", "遵义市", 
                        "朝阳市", "沈阳市", "葫芦岛市", "铁岭市", "重庆市", "延安市",
                        "汉中市", "铜川市", "西宁市", "七台河市", "哈尔滨市", "黑河市", 
                        "齐齐哈尔市") ,
              labels = 1:89)
 data$city <- factor(data$city,  
                     levels = city_level$levels,
                     labels = city_level$labels)
 
 city_level2 <- list(levels =1:89,
                    labels = c("上海市", "大理州", "昆明市", "昭通市", "玉溪市", 
                               "呼和浩特市", "北京市", "吉林市", "松原市", "白城市", 
                               "长春市", "乐山市", "南充市", "宜宾市", "成都市", "泸州市", 
                               "眉山市", "天津市", "吴忠市", "宣城市", "蚌埠市", "阜阳市",
                               "黄山市", "临沂市", "日照市", "泰安市", "济宁市", "烟台市", 
                               "太原市", "运城市", "长治市", "广州市", "汕头市", "深圳市", 
                               "崇左市", "柳州市", "玉林市", "喀什市", "南京市", "常州市", 
                               "徐州市", "扬州市", "连云港市", "上饶市", "宜春市", "赣州市", 
                               "鹰潭市", "唐山市", "沧州市", "邯郸市", "信阳市", "周口市", 
                               "商丘市", "洛阳市", "漯河市", "宁波市", "杭州市", "温州市", 
                               "湖州市", "海口市", "孝感市", "恩施州", "武汉市", "襄樊市", 
                               "怀化市", "邵阳市", "郴州市", "兰州市", "庆阳市", "三明市", 
                               "漳州市", "福州市", "拉萨市", "安顺市", "贵阳市", "遵义市", 
                               "朝阳市", "沈阳市", "葫芦岛市", "铁岭市", "重庆市", "延安市",
                               "汉中市", "铜川市", "西宁市", "七台河市", "哈尔滨市", "黑河市", 
                               "齐齐哈尔市") )
 data$city <- factor(data$city,  
                     levels = city_level2$levels,
                     labels = city_level2$labels)
 return(data)
}

#考虑少数类问题，省份转换为4大经济地理分区
CGSS1013$provinces <- as.numeric(CGSS1013$provinces)
CGSS1013 <- CGSS1013 %>%
  mutate(
    region = case_when(
      provinces %in% c(1,4,7,10,12,15,17,19,20,24) ~ 1,
      provinces %in% c(9,11,16,18,21,22)             ~ 2,
      provinces %in% c(3,13,28,26,6,2,25,29,23,30,8,14)~ 3,  
      provinces %in% c(5,27,31)                      ~ 4,
      TRUE                                           ~ NA 
    )
  )

CGSS1821$provinces <- as.numeric(CGSS1821$provinces)
CGSS1821 <- CGSS1821 %>%
  mutate(
    region = case_when(
      provinces %in% c(1,4,7,10,12,15,17,19,20,24) ~ 1,
      provinces %in% c(9,11,16,18,21,22)             ~ 2,
      provinces %in% c(3,13,28,26,6,2,25,29,23,30,8,14)~ 3,  
      provinces %in% c(5,27,31)                      ~ 4,
      TRUE                                           ~ NA 
    )
  )

#ISCO转换为ISEI08
CGSS1821$isco08_self<-isco08toISEI08(CGSS1821$isco08_self, display.nas = FALSE)
CGSS1821$isco08_sp <-isco08toISEI08(CGSS1821$isco08_sp, display.nas = FALSE)
CGSS1821$isco08_f <- isco08toISEI08(CGSS1821$isco08_f, display.nas = FALSE)
CGSS1821$isco08_m <-isco08toISEI08(CGSS1821$isco08_m, display.nas = FALSE)
CGSS1821$isco08_self_combine<-isco08toISEI08(CGSS1821$isco08_self_combine, display.nas = FALSE)

CGSS1013$isco88_self <- isco08toISEI08(isco88to08(CGSS1013$isco88_self, display.nas = T), display.nas = FALSE)
CGSS1013$isco88_sp <- isco08toISEI08(isco88to08(CGSS1013$isco88_sp, display.nas = T), display.nas = FALSE)
CGSS1013$isco88_f <- isco08toISEI08(isco88to08(CGSS1013$isco88_f, display.nas = T), display.nas = FALSE)
CGSS1013$isco88_m <- isco08toISEI08(isco88to08(CGSS1013$isco88_m, display.nas = T), display.nas = FALSE)
CGSS1013$isco88_self_combine <- isco08toISEI08(isco88to08(CGSS1013$isco88_self_combine, display.nas = T), display.nas = FALSE)

names(CGSS1821)[names(CGSS1821) %in% c("isco08_self", "isco08_sp", "isco08_f", "isco08_m",'isco08_self_combine')] <- 
  c("ISEI_self", "ISEI_sp", "ISEI_f", "ISEI_m",'ISEI_self_combine')
names(CGSS1013)[names(CGSS1013) %in% c("isco88_self", "isco88_sp", "isco88_f", "isco88_m",'isco88_self_combine')] <- 
  c("ISEI_self", "ISEI_sp", "ISEI_f", "ISEI_m",'ISEI_self_combine')
#收入收入进行对数处理
CGSS1821$lincome <- ln(CGSS1821$A8a) 
CGSS1013$lincome <- ln(CGSS1013$A8a) 
CGSS1821$sp__income <- ln(CGSS1821$A75) 
CGSS1013$sp__income <- ln(CGSS1013$A75) 
CGSS1821$fincome <- ln(CGSS1821$A62) 
CGSS1013$fincome <- ln(CGSS1013$A62) 


#按照年龄和预期分布筛选样本
CGSS1013 <- filter(CGSS1013,age >=18 & age<= 45)
CGSS1821 <- filter(CGSS1821,age >=18 & age<= 45)

CGSS1013 <- filter(CGSS1013,A43_a >=1 & A43_a<= 9)
CGSS1821 <- filter(CGSS1821,A43_a >=1 & A43_a<= 9)

#根据户籍以及现居住地等生成新变量“居民身份”，1为城市土著，2为农村居民，3为流动人口，4为户籍移民
CGSS1821$identity <- ifelse(CGSS1821$type == 1,1,0)
CGSS1821$identity <- ifelse(CGSS1821$identity == 1 & (CGSS1821$A18 == 1 | CGSS1821$A18 == 3),3,CGSS1821$identity)
CGSS1821$identity <- ifelse(CGSS1821$identity == 1 & CGSS1821$A19a == 1,1.1,ifelse(CGSS1821$identity == 1,4,CGSS1821$identity))
CGSS1821$identity[CGSS1821$identity == 0 ] <- 2
CGSS1821$identity[CGSS1821$identity == 1.1 ] <- 1

CGSS1013$identity <- ifelse(CGSS1013$type == 1,1,0)
CGSS1013$identity <- ifelse(CGSS1013$identity == 1 & (CGSS1013$A18 == 1| CGSS1013$A18 == 3 ),3,CGSS1013$identity)
CGSS1013$identity <- ifelse(CGSS1013$identity == 1 & CGSS1013$A19a == 1,1.1,ifelse(CGSS1013$identity == 1,4,CGSS1013$identity))
CGSS1013$identity[CGSS1013$identity == 0 ] <- 2
CGSS1013$identity[CGSS1013$identity == 1.1 ] <- 1


#根据A37系列问题生成生育性别偏好,1为偏好儿子，2为偏好女儿，0为无偏好
CGSS1821$A37b <- ifelse((CGSS1821$A37_2 > CGSS1821$A37_3),1,ifelse(CGSS1821$A37_2 == CGSS1821$A37_3,0,2))
CGSS1013$A37b <- ifelse((CGSS1013$A37_2 > CGSS1013$A37_3),1,ifelse(CGSS1013$A37_2 == CGSS1013$A37_3,0,2))

#生成因变量expectation
CGSS1013$expectation <- CGSS1013$A43_c - CGSS1013$A43_a
CGSS1821$expectation <- CGSS1821$A43_c - CGSS1821$A43_a
table(CGSS1013$expectation)
table(CGSS1821$expectation)

#生成自变量流动感知sense
CGSS1013$sense <- CGSS1013$A43_a - CGSS1013$A43_d
CGSS1821$sense <- CGSS1821$A43_a - CGSS1821$A43_d

#根据A53生成自变量工作情况,1为有工作，2为学习，3为居家（全职在家、退休），4为无业（失业、无法劳动）
CGSS1013$job <- ifelse(CGSS1013$A53 == 1,1.1,1)
CGSS1013$job <- ifelse(CGSS1013$job == 1.1 & CGSS1013$A54 == 1,2,CGSS1013$job)
CGSS1013$job <- ifelse(CGSS1013$job == 1.1 & (CGSS1013$A54 == 7|CGSS1013$A54 == 8),3,CGSS1013$job)
CGSS1013$job <- ifelse(CGSS1013$job == 1.1,4,CGSS1013$job)

CGSS1821$job <- ifelse(CGSS1821$A53 == 1,1.1,1)
CGSS1821$job <- ifelse(CGSS1821$job == 1.1 & CGSS1821$A54 == 1,2,CGSS1821$job)
CGSS1821$job <- ifelse(CGSS1821$job == 1.1 & (CGSS1821$A54 == 7|CGSS1821$A54 == 8),3,CGSS1821$job)
CGSS1821$job <- ifelse(CGSS1821$job == 1.1,4,CGSS1821$job)

#根据A76生成自变量工作情况,1为有工作，2为学习，3为居家（全职在家、退休），4为无业（失业、无法劳动）
CGSS1013$spjob <- ifelse(CGSS1013$A76 == 1,1.1,1)
CGSS1013$spjob <- ifelse(CGSS1013$spjob == 1.1 & CGSS1013$A77 == 1,2,CGSS1013$spjob)
CGSS1013$spjob <- ifelse(CGSS1013$spjob == 1.1 & (CGSS1013$A77 == 7|CGSS1013$A77 == 8),3,CGSS1013$spjob)
CGSS1013$spjob <- ifelse(CGSS1013$spjob == 1.1,4,CGSS1013$spjob)

CGSS1821$spjob <- ifelse(CGSS1821$A76 == 1,1.1,1)
CGSS1821$spjob <- ifelse(CGSS1821$spjob == 1.1 & CGSS1821$A77 == 1,2,CGSS1821$spjob)
CGSS1821$spjob <- ifelse(CGSS1821$spjob == 1.1 & (CGSS1821$A77 == 7|CGSS1821$A77 == 8),3,CGSS1821$spjob)
CGSS1821$spjob <- ifelse(CGSS1821$spjob == 1.1,4,CGSS1821$spjob)
table(CGSS1821$spjob)
table(CGSS1013$spjob)

#删除高缺失值变量
get_high_missing_vars <- function(data, 
                                  threshold = 0.5, 
                                  show_pct = FALSE, 
                                  sort = TRUE) {
  
  # 参数验证
  if (!is.data.frame(data)) {
    stop("输入必须是数据框")
  }
  if (threshold < 0 | threshold > 1) {
    stop("阈值必须在0-1之间")
  }
  
  # 计算缺失比例
  missing_ratio <- colMeans(is.na(data))
  
  # 筛选变量
  high_missing <- missing_ratio[missing_ratio > threshold]
  
  # 转换为数据框
  result <- data.frame(
    Variable = names(high_missing),
    MissingRatio = unname(high_missing),
    row.names = NULL
  )
  
  # 排序处理
  if (sort && nrow(result) > 0) {
    result <- result[order(result$MissingRatio, decreasing = TRUE), ]
  }
  
  # 百分比格式化
  if (show_pct && nrow(result) > 0) {
    result$MissingRatio <- sprintf("%.1f%%", result$MissingRatio * 100)
  }
  
  # 添加阈值属性
  attr(result, "threshold") <- threshold
  
  return(result)
}
a<-get_high_missing_vars(CGSS1013,threshold = 0.3)
b<-get_high_missing_vars(CGSS1821,threshold = 0.3)

vars_a <- a$Variable
vars_b <- b$Variable
unique_vars <- unique(c(vars_a, vars_b))
unique_vars <- unique_vars[!unique_vars %in% c("ISEI_self", "ISEI_sp", "ISEI_f", "ISEI_m",'A72','A73','A74','A76')]
unique_vars_dataframe <- data.frame(unique_vars)



#剔除不需要的变量
CGSS1013 <- CGSS1013[, -which(names(CGSS1013) %in% c('id','A19','A24','A43_c','A43_b','A37_2','A37_3','weight',
                                                     'A8a','A62','A75','provinces',unique_vars))]
CGSS1821 <- CGSS1821[, -which(names(CGSS1821) %in% c('id','A19','A24','A43_c','A43_b','A37_2','A37_3','weight',
                                                     'A8a','A62','A75','provinces',unique_vars))]


#定义函数（二分类）
process1 <- function(data) {
  ifelse(data > 0, 1, 0)
}
#定义函数（三分类）
process2 <- function(data){
  ifelse(data >0, 2, ifelse(data == 0,1,0))
}

CGSS1013_con <- CGSS1013
CGSS1821_con <- CGSS1821
CGSS1013$expectation <- process1(CGSS1013$expectation)
CGSS1821$expectation <- process1(CGSS1821$expectation)

CGSS1013$sense <- process2(CGSS1013$sense)
CGSS1821$sense <- process2(CGSS1821$sense)
CGSS1013_con$sense <- process2(CGSS1013_con$sense)
CGSS1821_con$sense <- process2(CGSS1821_con$sense)

table(CGSS1821$sense)
table(CGSS1013$sense)

#删除因变量缺失样本
CGSS1821 <- CGSS1821[!is.na(CGSS1821$expectation),]
CGSS1013 <- CGSS1013[!is.na(CGSS1013$expectation),]
CGSS1821_con <- CGSS1821_con[!is.na(CGSS1821_con$expectation),]
CGSS1013_con <- CGSS1013_con[!is.na(CGSS1013_con$expectation),]

summary(CGSS1821$expectation)
summary(CGSS1013$expectation)


#因子类变量定义，以及年份变量类别化
convert_to_factors <- function(data) {
  # 检查数据框是否为空
  if (is.null(data)) {
    stop("输入的数据框为空")
  }
  
  # 定义各变量的因子水平和标签
  factor_specs <- list(
    A01 = list(levels = c(0,1),
               labels = c('否','是')),
    
    A02 = list(levels = c(0,1),
               labels = c('否','是')),
    
    A03 = list(levels = c(0,1),
               labels = c('否','是')),
    
    A04 = list(levels = c(0,1),
               labels = c('否','是')),
    
    A05 = list(levels = c(0,1),
               labels = c('否','是')),
    
    type = list(levels = c(0,1),
                labels = c('村委会','居委会')),
    
    region = list(levels = 1:4, 
                  labels = c('东部地区','中部地区','西部地区','东北地区')),
    
    A2 = list(levels = 0:1,
              labels = c('女','男')),
    
    A4 = list(levels = 0:1,
              labels = c('少数民族','汉族')),
    
    A5 = list(levels = 0:1,
              labels = c('不信教','信教')),
    
    A7b = list(levels = 1:3, 
               labels = c('正在读','辍学/退学/肄业','毕业')),
    
    A9 = list(levels = 0:1,
              labels = c('未递交过','递交过')),
    
    A10 = list(levels = 0:1, 
               labels = c('非党员','党员')),
    
    A12 = list(levels = 0:1,
               labels = c('否','是')),
    
    A12a = list(levels = 0:1,
                labels = c('否','是')),
    
    A12b = list(levels = 0:1,
               labels = c('否','是')),
    
    A19a = list(levels = 0:1,
                labels = c('否','是')),
    
    A21 = list(levels = 1:3, 
               labels = c('本乡（镇、街道）','本县（市、区）其他乡（镇、街道）',
                          '本区/县/县级市以外')),
    
    A24a = list(levels = 0:1,
                labels = c('否','是')),
    
    A29 = list(levels = 0:1, 
               labels = c('传统媒介','数字媒介')),
    
    A41 = list(levels = 1:3, 
               labels = c('主要由子女负责','政府/子女/老人均摊','主要其他主体负责'
               )),
    
    A45 = list(levels = 1:0, 
               labels = c('是','不是')),
    
    A53 = list(levels = 1:3, 
               labels = c('未从事任何以获得经济收入为目的的工作','休假','是')),
    
    A54 = list(levels = 1:9, 
               labels = c('在校学习','丧失劳动能力','毕业后未工作','因单位原因失去原工作',
                          '因个人原因失去原工作','承包土地被征用','离/退休','料理家务','其他')),
    A56 = list(levels = 0:1,
               labels = c('否','是')),
    
    A57 = list(levels = 0:1,
               labels = c('不能','能')),
    
    A58 = list(levels = 1:6, 
               labels = c('目前从事非农工作','目前务农，曾经有过非农工作','目前务农，没有过非农工作','目前没有工作，而且只务过农','目前没有工作，曾经有过非农工作','从未工作过')),
    
    A59a = list(levels = 1:8, 
                labels = c('自己是老板','个体工商户','受雇于他人(有固定雇主)',
                           '劳务工/劳务派遣人员','零工,散工(无固定雇主的受雇者)',
                           '在自己家的生意/企业中工作/帮忙',
                           '自由职业者','其他')),
    
    A59b = list(levels = 1:3, 
                labels = c('没有签订劳动合同','签有无固定期限劳动合同',
                           '签有固定期限劳动合同')),
    
    A59f = list(levels = 1:4, 
                labels = c('只管别人，不受别人管理','既管理别人，又受别人管理',
                           '只受别人管理，不管理别人','既不管理别人，又不受别人管理')),
    
    A59j = list(levels = 1:6, 
                labels = c('党政机关','企业','事业单位','社会团体、居/村委会',
                           '无单位/自雇（包括个体户）','其他')),
    
    A59k = list(levels = 1:6, 
                labels = c('国有或国有控股','集体所有或集体控股','私有/民营或私有/民营控股',
                           '港澳台资或港澳台资控股','外资所有或外资控股','其他')),
    
    A69 = list(levels = 1:3, 
               labels = c('未婚','同居/已婚','分居/离婚/丧偶')),
    
    A611 = list(levels = 0:1,
                labels = c('否','是')),
    
    A612 = list(levels = 0:1,
                labels = c('否','是')),
    
    A66 = list(levels = 0:1,
               labels = c('没有','有')),
    
    A67_1 = list(levels = 0:1,
                 labels = c('未选择','选择')),
    A67_2 = list(levels = 0:1,
                 labels = c('未选择','选择')), 
    A67_3 = list(levels = 0:1,
                 labels = c('未选择','选择')),
    
    A70a = list(levels = 0:1,
                labels = c('否','是')),
    A73 = list(levels = 0:1, 
               labels = c('非党员','党员')),
    
    A76 = list(levels = 1:4, 
               labels = c('未从事任何以获得经济收入为目的的工作','带薪休假',
                          '停薪休假','是')),
    
    A77 = list(levels = 1:9, 
               labels = c('在校学习','丧失劳动能力','毕业后未工作','因单位原因失去原工作',
                          '因个人原因失去原工作','承包土地被征用','离/退休','料理家务','其他')),
    
    A79 = list(levels = 0:1,
               labels = c('否','是')),
    
    A80 = list(levels = 0:1,
               labels = c('不能','能')),
    
    A81 = list(levels = 1:6, 
               labels = c('目前从事非农工作','目前务农,曾经有过非农工作',
                          '目前务农,没有过非农工作','目前没有工作,而且只务过农',
                          '目前没有工作,曾经有过非农工作','从未工作过')),
    
    A82 = list(levels = 1:9, 
               labels = c('自己是老板','个体工商户','受雇于他人(有固定雇主)',
                          '劳务工/劳务派遣人员','零工,散工(无固定雇主的受雇者)',
                          '在自己家的生意/企业中工作/帮忙,不领工资',
                          '在自己家的生意/企业中工作/帮忙,领取工资',
                          '自由职业者','其他')),
    
    A83 = list(levels = 1:3, 
               labels = c('没有签订劳动合同','签有无固定期限劳动合同',
                          '签有固定期限劳动合同')),
    
    A87 = list(levels = 1:7, 
               labels = c('党政机关','企业','事业单位','社会团体、居/村委会',
                          '无单位/自雇（包括个体户）','军队','其他')),
    
    A88 = list(levels = 1:6, 
               labels = c('国有或国有控股','集体所有或集体控股','私有/民营或私有/民营控股',
                          '港澳台资或港澳台资控股','外资所有或外资控股','其他（请注明）')),
    
    A89c = list(levels = 0:1, 
                labels = c('非党员','党员')),
    
    A89d = list(levels = 0:1, 
                labels = c('没有工作','有工作/上学')),
    
    
    A89g = list(levels = 0:1, 
                labels = c('无单位/自雇（包括个体户）','单位（党政机关、军队、企事业单位）')),
    
    A89h = list(levels = 1:6, 
                labels = c('国有或国有控股','集体所有或集体控股','私有/民营或私有/民营控股',
                           '港澳台资或港澳台资控股','外资所有或外资控股','其他（请注明）')),
    
    A90c = list(levels = 0:1, 
                labels = c('非党员','党员')),
    
    A90d = list(levels = 0:1, 
                labels = c('没有工作','有工作/上学')),
    
    A90g = list(levels = 0:1, 
                labels = c('无单位/自雇（包括个体户）','单位（党政机关、军队、企事业单位）')),
    
    A90h = list(levels = 1:6, 
                labels = c('国有或国有控股','集体所有或集体控股','私有/民营或私有/民营控股',
                           '港澳台资或港澳台资控股','外资所有或外资控股','其他（请注明）')),
    identity = list(levels = 1:4,
                    labels = c('城市土著','农村居民','流动人口','户籍移民')),
    
    A37b = list(levels = 0:2,
                labels = c('无偏好','偏好儿子','偏好女儿')),
    
    job = list(levels= 1:4,
               labels = c('有工作','学习','居家','无业/失业')),
    
    spjob = list(levels= 1:4,
                 labels = c('有工作','学习','居家','无业/失业')),
    
    sense = list(levels= 0:2,
                 labels =c('向下流动','平行流动','向上流动')),
    
    A18 = list(levels = 1:7, 
                labels = c('农业户口',
                           '非农业户口',
                           '居民户口（以前是农业户口）',
                           '居民户口（以前是非农业户口）',
                           '军籍',
                           '没有户口',
                           '其它')),
    
    Year = list(levels =c(2010,2013,2015,2018,2021),
                 labels = c("2010年","2013年","2015年",'2018年','2021年'))
    
    
  )
  for (var_name in names(factor_specs)) {
    if (var_name %in% names(data)) {
      # 检查标签和水平长度是否匹配
      if (length(factor_specs[[var_name]]$levels) != length(factor_specs[[var_name]]$labels)) {
        warning(paste("变量", var_name, "的levels和labels长度不匹配，跳过转换"))
        next
      }
      # 如果数据框中有该变量，则转换为因子
      data[[var_name]] <- factor(data[[var_name]],
                                 levels = factor_specs[[var_name]]$levels,
                                 labels = factor_specs[[var_name]]$labels)
    }
  }
  
  # 返回转换后的数据框
  return(data)
}
CGSS1013 <- convert_to_factors(CGSS1013)
CGSS1821 <- convert_to_factors(CGSS1821)
CGSS1013_con <- convert_to_factors(CGSS1013_con)
CGSS1821_con <- convert_to_factors(CGSS1821_con)


#少数类处理
detect_rare_categories <- function(data, threshold = 0.01, min_samples = NULL) {
  # 参数校验
  if (!is.data.frame(data)) stop("输入必须是数据框")
  if (threshold <= 0 || threshold >= 1) stop("阈值需在(0,1)区间")
  
  # 自动处理min_samples参数
  if (is.null(min_samples)) {
    min_samples <- max(1, round(threshold * nrow(data)))
  } else {
    if (min_samples < 1) stop("min_samples需大于等于1")
  }
  
  # 筛选需要检查的列（因子或字符型）
  cat_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
  if (!any(cat_cols)) return(list(rare_vars = character(0), details = data.frame()))
  
  # 初始化结果存储
  result_details <- data.frame(
    variable = character(0),
    category = character(0),
    count = integer(0),
    percent = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # 遍历所有分类变量
  for (col in names(data)[cat_cols]) {
    # 计算频数（排除NA）
    freq_table <- table(data[[col]], useNA = "no")
    
    # 计算相对频率
    total <- sum(freq_table)
    percent <- prop.table(freq_table) * 100
    
    # 识别稀有类
    rare_mask <- percent < threshold | freq_table < min_samples
    rare_categories <- names(freq_table[rare_mask])
    
    # 如果存在稀有类则记录
    if (length(rare_categories) > 0) {
      new_rows <- data.frame(
        variable = rep(col, length(rare_categories)),
        category = rare_categories,
        count = as.vector(freq_table[rare_mask]),
        percent = as.vector(percent[rare_mask]),
        stringsAsFactors = FALSE
      )
      result_details <- rbind(result_details, new_rows)
    }
  }
  
  # 返回结果
  list(
    rare_vars = unique(result_details$variable),
    details = result_details
  )
}

rare1013 <-detect_rare_categories(CGSS1013)
rare1821 <-detect_rare_categories(CGSS1821)
rare1013
rare1821

#奇异值处理
detect_numeric_outliers <- function(data, 
                                    method = "sd", 
                                    iqr_multiplier = 1.5,
                                    sd_threshold = 3,
                                    lower_percentile = 0.01,
                                    upper_percentile = 0.99,
                                    return_details = TRUE) {
  # 参数校验
  if (!is.data.frame(data)) stop("输入必须是数据框")
  if (!method %in% c("iqr", "sd", "percentile")) {
    stop("方法必须是: 'iqr', 'sd', 或 'percentile'")
  }
  if (method == "iqr" && iqr_multiplier <= 0) {
    stop("iqr_multiplier必须大于0")
  }
  if (method == "sd" && sd_threshold <= 0) {
    stop("sd_threshold必须大于0")
  }
  
  # 筛选数值型列
  numeric_cols <- sapply(data, is.numeric)
  if (!any(numeric_cols)) {
    message("没有数值型变量")
    return(invisible(list()))
  }
  
  # 初始化结果存储
  result <- list(
    summary = data.frame(
      variable = character(),
      method = character(),
      lower_bound = numeric(),
      upper_bound = numeric(),
      outlier_count = integer(),
      outlier_ratio = numeric(),
      stringsAsFactors = FALSE
    ),
    details = list()
  )
  
  # 遍历数值型列
  for (col in names(data)[numeric_cols]) {
    x <- na.omit(data[[col]])  # 排除缺失值
    if (length(x) == 0) next    # 跳过全NA列
    
    # 计算边界
    bounds <- switch(
      method,
      "iqr" = {
        q <- quantile(x, c(0.25, 0.75))
        iqr <- q[2] - q[1]
        c(q[1] - iqr_multiplier*iqr, 
          q[2] + iqr_multiplier*iqr)
      },
      "sd" = {
        mu <- mean(x)
        sigma <- sd(x)
        c(mu - sd_threshold*sigma, 
          mu + sd_threshold*sigma)
      },
      "percentile" = {
        quantile(x, c(lower_percentile, upper_percentile))
      }
    )
    
    # 识别异常值
    is_outlier <- x < bounds[1] | x > bounds[2]
    n_outliers <- sum(is_outlier)
    
    # 记录结果
    result$summary <- rbind(result$summary, data.frame(
      variable = col,
      method = method,
      lower_bound = bounds[1],
      upper_bound = bounds[2],
      outlier_count = n_outliers,
      outlier_ratio = n_outliers / length(x)
    ))
    
    # 记录详细信息
    if (return_details && n_outliers > 0) {
      outlier_data <- data[which(data[[col]] %in% x[is_outlier]), ]
      result$details[[col]] <- list(
        bounds = setNames(bounds, c("lower", "upper")),
        outliers = outlier_data
      )
    }
  }
  
  # 排序结果
  result$summary <- result$summary[order(result$summary$outlier_ratio, 
                                         decreasing = TRUE), ]
  
  return(result)
}
outl1013 <-detect_numeric_outliers(CGSS1013)
outl1821 <-detect_numeric_outliers(CGSS1013)
outl1013$summary
outl1821$summary

table(CGSS1013$expectation)
table(CGSS1821$expectation)



#导入CGSS1015数据
CGSS1015<-haven::read_sav('CGSS1015.sav')
CGSS1015<-haven::zap_labels(CGSS1015)
CGSS1015$provinces <- as.numeric(CGSS1015$provinces)
CGSS1015 <- CGSS1015 %>%
  mutate(
    region = case_when(
      provinces %in% c(1,4,7,10,12,15,17,19,20,24) ~ 1,
      provinces %in% c(9,11,16,18,21,22)             ~ 2,
      provinces %in% c(3,13,28,26,6,2,25,29,23,30,8,14)~ 3,  
      provinces %in% c(5,27,31)                      ~ 4,
      TRUE                                           ~ NA 
    )
  )

CGSS1015$isco88_self <- isco08toISEI08(isco88to08(CGSS1015$isco88_self, display.nas = T), display.nas = FALSE)
CGSS1015$isco88_sp <- isco08toISEI08(isco88to08(CGSS1015$isco88_sp, display.nas = T), display.nas = FALSE)
CGSS1015$isco88_f <- isco08toISEI08(isco88to08(CGSS1015$isco88_f, display.nas = T), display.nas = FALSE)
CGSS1015$isco88_m <- isco08toISEI08(isco88to08(CGSS1015$isco88_m, display.nas = T), display.nas = FALSE)
CGSS1015$isco88_self_combine <- isco08toISEI08(isco88to08(CGSS1015$isco88_self_combine, display.nas = T), display.nas = FALSE)
names(CGSS1015)[names(CGSS1015) %in% c("isco88_self", "isco88_sp", "isco88_f", "isco88_m",'isco88_self_combine')] <- 
  c("ISEI_self", "ISEI_sp", "ISEI_f", "ISEI_m",'ISEI_self_combine')

CGSS1015$lincome <- CGSS1015$A8a 
CGSS1015$sp__income <- CGSS1015$A75 
CGSS1015$fincome <- CGSS1015$A62 
CGSS1015$A75



#按照年龄和预期分布筛选样本
CGSS1015 <- filter(CGSS1015,age >=18 & age<= 45)

CGSS1015 <- filter(CGSS1015,A43_a >=1 & A43_a<= 9)


#根据户籍以及现居住地等生成新变量“居民身份”，1为城市土著，2为农村居民，3为流动人口，4为户籍移民
CGSS1015$identity <- ifelse(CGSS1015$type == 1,1,0)
CGSS1015$identity <- ifelse(CGSS1015$identity == 1 & (CGSS1015$A18 == 1 | CGSS1015$A18 == 3),3,CGSS1015$identity)
CGSS1015$identity <- ifelse(CGSS1015$identity == 1 & CGSS1015$A19a == 1,1.1,ifelse(CGSS1015$identity == 1,4,CGSS1015$identity))
CGSS1015$identity[CGSS1015$identity == 0 ] <- 2
CGSS1015$identity[CGSS1015$identity == 1.1 ] <- 1

#根据A37系列问题生成生育性别偏好,1为偏好儿子，2为偏好女儿，0为无偏好
CGSS1015$A37b <- ifelse((CGSS1015$A37_2 > CGSS1015$A37_3),1,ifelse(CGSS1015$A37_2 == CGSS1015$A37_3,0,2))

#生成因变量expectation
CGSS1015$expectation <- CGSS1015$A43_c - CGSS1015$A43_a
table(CGSS1015$expectation)

#生成自变量流动感知sense
CGSS1015$sense <- CGSS1015$A43_a - CGSS1015$A43_d

#根据A53生成自变量工作情况,1为有工作，2为学习，3为无业（失业、无法劳动、全职在家、退休）
CGSS1015$job <- ifelse(CGSS1015$A53 == 1,1.1,1)
CGSS1015$job <- ifelse(CGSS1015$job == 1.1 & CGSS1015$A54 == 1,2,CGSS1015$job)
CGSS1015$job <- ifelse(CGSS1015$job == 1.1,3,CGSS1015$job)

#根据A53生成自变量工作情况,1为有工作，2为学习，3为居家（全职在家、退休），4为无业（失业、无法劳动）
CGSS1015$job <- ifelse(CGSS1015$A53 == 1,1.1,1)
CGSS1015$job <- ifelse(CGSS1015$job == 1.1 & CGSS1015$A54 == 1,2,CGSS1015$job)
CGSS1015$job <- ifelse(CGSS1015$job == 1.1 & (CGSS1015$A54 == 7|CGSS1015$A54 == 8),3,CGSS1015$job)
CGSS1015$job <- ifelse(CGSS1015$job == 1.1,4,CGSS1015$job)

#根据A76生成自变量工作情况,1为有工作，2为学习，3为居家（全职在家、退休），4为无业（失业、无法劳动）
CGSS1015$spjob <- ifelse(CGSS1015$A76 == 1,1.1,1)
CGSS1015$spjob <- ifelse(CGSS1015$spjob == 1.1 & CGSS1015$A77 == 1,2,CGSS1015$spjob)
CGSS1015$spjob <- ifelse(CGSS1015$spjob == 1.1 & (CGSS1015$A77 == 7|CGSS1015$A77 == 8),3,CGSS1015$spjob)
CGSS1015$spjob <- ifelse(CGSS1015$spjob == 1.1,4,CGSS1015$spjob)
table(CGSS1015$spjob)
table(CGSS1013$A37b)

CGSS1015 <- CGSS1015[, -which(names(CGSS1015) %in% c('id','A19','A24','A37_2','A43_c','A43_b','A37_3','weight','A8a','A62','A75','provinces',unique_vars))]
CGSS1015_con <- CGSS1015
CGSS1015$expectation <- process1(CGSS1015$expectation)
CGSS1015$sense <- process2(CGSS1015$sense)
CGSS1015_con$sense <- process2(CGSS1015_con$sense)

table(CGSS1015$sense)

#CGSS1015<- as.data.frame(lapply(CGSS1015, as.numeric,na.rm = FALSE))
#删除因变量缺失样本
CGSS1015 <- CGSS1015[!is.na(CGSS1015$expectation),]
CGSS1015_con <- CGSS1015_con[!is.na(CGSS1015_con$expectation),]

CGSS1015 <- convert_to_factors(CGSS1015)
CGSS1015_con <- convert_to_factors(CGSS1015_con)

CGSS1021<-bind_rows(CGSS1015, CGSS1821)
CGSS1021_con <- bind_rows(CGSS1015_con, CGSS1821_con)

CGSS1021 <- CGSS1021 %>% 
  mutate(A37_1 = ifelse(A37_1 == 97, -1, A37_1))

CGSS1021_con <- CGSS1021_con %>% 
  mutate(A37_1 = ifelse(A37_1 == 97, -1, A37_1))

CGSS1013 <- CGSS1013 %>% 
  mutate(A37_1 = ifelse(A37_1 == 97, -1, A37_1))

CGSS1013_con <- CGSS1013_con %>% 
  mutate(A37_1 = ifelse(A37_1 == 97, -1, A37_1))



CGSS1821 <- ctiy_to_factor(CGSS1821)
CGSS1013 <- ctiy_to_factor(CGSS1013)
CGSS1021 <- ctiy_to_factor(CGSS1021)

CGSS1821_con <- ctiy_to_factor(CGSS1821_con)
CGSS1013_con <- ctiy_to_factor(CGSS1013_con)
CGSS1021_con <- ctiy_to_factor(CGSS1021_con)

# 处理完的数据导出为sav，方便后续使用
write_sav(
  data = CGSS1021,            
  path = "CGSS1021.sav",      # 输出文件路径
  compress = FALSE            # 是否压缩（FALSE 保持兼容性）
)