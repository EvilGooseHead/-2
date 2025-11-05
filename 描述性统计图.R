library(dplyr)        
library(haven) 
library(Matrix)
library(ggplot2)
library(Hmisc)
library(plyr) 
library(ggpubr)

rm(list=ls())
CGSS2010 <- read_sav("C:/Users/USER/Desktop/毕业设计/数据/CGSS/2010/CGSS2010_recode.sav")
CGSS2013 <- read_sav("C:/Users/USER/Desktop/毕业设计/数据/CGSS/2013/CGSS2013.sav")
CGSS2015 <- read_sav("C:/Users/USER/Desktop/毕业设计/数据/CGSS/2015/CGSS2015.sav")
CGSS2018 <- read_sav("C:/Users/USER/Desktop/毕业设计/数据/CGSS/2018/CGSS2018.sav")
CGSS2021 <- read_sav("C:/Users/USER/Desktop/毕业设计/数据/CGSS/CGSS2021/CGSS2021_20240607_recode.sav")

CGSS2010$age =2010 - CGSS2010$a3a
CGSS2013$age =2013 - CGSS2013$a3a
CGSS2015$age =2015 - CGSS2015$a301
CGSS2018$age =2018 - CGSS2018$a31
CGSS2021$age =2021 - CGSS2021$A3_1
CGSS2010 <- CGSS2010[!is.na(CGSS2010$a43a),]
CGSS2013 <- CGSS2013[!is.na(CGSS2013$a43a),]
CGSS2015 <- CGSS2015[!is.na(CGSS2015$a431),]         
CGSS2018 <- CGSS2018[!is.na(CGSS2018$a43a),]
CGSS2021 <- CGSS2021[!is.na(CGSS2021$A43_a),]

CGSS2010 <- CGSS2010[!is.na(CGSS2010$a43c),]
CGSS2013 <- CGSS2013[!is.na(CGSS2013$a43c),]
CGSS2015 <- CGSS2015[!is.na(CGSS2015$a433),]         
CGSS2018 <- CGSS2018[!is.na(CGSS2018$a43c),]
CGSS2021 <- CGSS2021[!is.na(CGSS2021$A43_c),]

CGSS2010 <- filter(CGSS2010,age >= 18 & age<= 45)
CGSS2013 <- filter(CGSS2013,age >=18 & age<= 45)
CGSS2015 <- filter(CGSS2015,age >=18 & age<= 45)
CGSS2018 <- filter(CGSS2018,age >=18 & age<= 45)
CGSS2021 <- filter(CGSS2021,age >=18 & age<= 45)

id_expectation2010 <- as.data.frame(CGSS2010$a43c)
id_expectation2013 <- as.data.frame(CGSS2013$a43c)
id_expectation2015 <- as.data.frame(CGSS2015$a433)
id_expectation2018 <- as.data.frame(CGSS2018$a43c)
id_expectation2021 <- as.data.frame(CGSS2021$A43_c)
names(id_expectation2010) <- "2010"
names(id_expectation2013) <- "2013"
names(id_expectation2015) <- "2015"
names(id_expectation2018) <- "2018"
names(id_expectation2021) <- "2021"
id_expectation <- rbind.fill(id_expectation2010,id_expectation2013,id_expectation2015,id_expectation2018,id_expectation2021)
id_expectation_long <- reshape2::melt(id_expectation, variable.name = "Year", value.name = "id_expectation")
id_expectation_long <- na.omit(id_expectation_long)
id_expectation_long2 <- id_expectation_long
id_expectation_long2$id_expectation <- as.factor(id_expectation_long2$id_expectation)
cdata_id_expectation <- ddply(id_expectation_long, "Year", summarise,
                            N    = length(id_expectation),
                            mean = mean(id_expectation),
                            sd   = sd(id_expectation),
                            se   = sd / sqrt(N)
)
cdata_id_expectation
cdata_id_expectation$Year<-as.factor(cdata_id_expectation$Year)

identication2010 <- as.data.frame(CGSS2010$a43a)
identication2013 <- as.data.frame(CGSS2013$a43a)
identication2015 <- as.data.frame(CGSS2015$a431)
identication2018 <- as.data.frame(CGSS2018$a43a)
identication2021 <- as.data.frame(CGSS2021$A43_a)
names(identication2010) <- "2010"
names(identication2013) <- "2013"
names(identication2015) <- "2015"
names(identication2018) <- "2018"
names(identication2021) <- "2021"
identication <- rbind.fill(identication2010,identication2013,identication2015,identication2018,identication2021)
identication_long <- reshape2::melt(identication, variable.name = "Year", value.name = "identication")
identication_long <- na.omit(identication_long)
identication_long2 <- identication_long
identication_long2$identication <- as.factor(identication_long2$identication)
cdata_identication <- ddply(identication_long, "Year", summarise,
               N    = length(identication),
               mean = mean(identication),
               sd   = sd(identication),
               se   = sd / sqrt(N)
)
cdata_identication
cdata_identication$Year<-as.factor(cdata_identication$Year)

age = bind_rows(
  data.frame(age = CGSS2010$age, year = 2010),
  data.frame(age = CGSS2013$age, year = 2013),
  data.frame(age = CGSS2015$age, year = 2015),
  data.frame(age = CGSS2018$age, year = 2018),
  data.frame(age = CGSS2021$age, year = 2021)
)
age$year <- as.factor(age$year)

age2<-age 
for ( j in seq_along(age2$age)){
  if(age2$age[j]>18 & age2$age[j]<=25){age2$age[j] <- 1
  }else if(age2$age[j]>25 & age2$age[j]<=30){age2$age[j] <- 2
  }else if(age2$age[j]>30 & age2$age[j]<=35){age2$age[j] <- 3
  }else if(age2$age[j]>35& age2$age[j]<=40){age2$age[j] <- 4
  }else {age2$age[j] <- 5
  }
}

CGSS2010 <- filter(CGSS2010,a43a >=1 & a43a<= 9)
CGSS2013 <- filter(CGSS2013,a43a >=1 & a43a<= 9)
CGSS2015 <- filter(CGSS2015,a431 >=1 & a431<= 9)  
CGSS2018 <- filter(CGSS2018,a43a >=1 & a43a<= 9)
CGSS2021 <- filter(CGSS2021,A43_a >=1 & A43_a<= 9)

expectation2010 <- CGSS2010$a43c - CGSS2010$a43a
expectation2013 <- CGSS2013$a43c - CGSS2013$a43a
expectation2015 <- CGSS2015$a433 - CGSS2015$a431
expectation2018 <- CGSS2018$a43c - CGSS2018$a43a
expectation2021 <- CGSS2021$A43_c - CGSS2021$A43_a



#分类图需要作如下变换
# process_expectation <- function(data) {
#   data <- na.omit(data)
#   ifelse(data > 0, 1, ifelse(data == 0, 0, -1))
# }
# 
#   #应用函数处理各年份数据
# expectation2010 <-process_expectation(expectation2010)
# expectation2013 <-process_expectation(expectation2013)
# expectation2015 <-process_expectation(expectation2015)
# expectation2018 <-process_expectation(expectation2018)
# expectation2021 <-process_expectation(expectation2021)

expectation2010 <- as.data.frame(expectation2010)
expectation2013 <- as.data.frame(expectation2013)
expectation2015 <- as.data.frame(expectation2015)
expectation2018 <- as.data.frame(expectation2018)
expectation2021 <- as.data.frame(expectation2021)

expectation <- rbind.fill(expectation2010,expectation2013,expectation2015,expectation2018,expectation2021)
expectation_long <- reshape2::melt(expectation, variable.name = "Year", value.name = "expectation")
expectation_long$Year <- gsub("expectation", "", expectation_long$Year)
expectation_long <- na.omit(expectation_long)
expectation_long2 <- expectation_long
for(j in seq_along(expectation_long2$expectation)){
  if(expectation_long2$expectation[j]>0){expectation_long2$expectation[j] <- 1
  }else if(expectation_long2$expectation[j]==0){expectation_long2$expectation[j] <- 0
  }else{expectation_long2$expectation[j]<- -1
  }
}

expectation_long2$expectation <- factor(expectation_long2$expectation,levels = c("-1", "0", "1"),labels = c("向下流动", "不变", "向上流动"))

cdata <- ddply(expectation_long, "Year", summarise,
               N    = length(expectation),
               mean = mean(expectation),
               sd   = sd(expectation),
               se   = sd / sqrt(N)
)
cdata
cdata$Year<-as.factor(cdata$Year)
# 使用ggplot绘制均值置信区间图
year_colors <- c("2010" = "#70B48F", 
                 "2013" = "#5861AC",
                 "2015" = "#FEA040",
                 "2018" = "#979998",
                   "2021" = "#B54764")

plot_expectation_mean <- function(cdata, 
                                  x = "年份", 
                                  y = "预期均值") {
  ggplot(cdata, aes(x = Year, y = mean, group = Year)) +
    geom_point(aes(color = factor(Year)), size = 3) +
    geom_errorbar(
      aes(ymin = mean - 2 * se, 
          ymax = mean + 2 * se, 
          color = Year),
      width = 0.2,
      linewidth = 1
    ) +
    geom_text(aes(label = sprintf("%.2f", mean), color = factor(Year)), vjust = -3, 
              size = 5, show.legend = FALSE) +
    labs(x = x, y = y) +
    coord_cartesian(ylim = c(min(cdata$mean - 3*cdata$se), max(cdata$mean + 6*cdata$se))) +
    theme_minimal(base_size = 16) + 
    scale_color_manual(
      name = "年份",  # 图例标题
      values = year_colors,  # 使用定义的颜色映射
      labels = c("2010年", "2013年", "2015年", "2018年", "2021年")  # 可选：修改图例标签
    ) +
    theme(
      axis.line = element_line(color = "black", linewidth = 1),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text.x = element_text(size = 14, hjust = 0.5),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 16, margin = margin(t = 15)),
      axis.title.y = element_text(size = 16, margin = margin(r = 15)),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),  
      panel.grid.minor = element_blank()
    )
}

plot_ratio <- function(expectation_long2, 
                             expectation = "expectation" ,
                             xlab = "年份", 
                             ylab = "累计百分比例",
                             legend_title="主观阶层认同") {
  # 数据预处理
  expectation_long2$number <- 1
  expectation_long2 <- ddply(expectation_long2, 'Year', transform, 
                             percent = 1/sum(number)*100)
  
  # 绘图
  ggplot(expectation_long2, aes(x = Year, y = percent, fill = as.factor(.data[[expectation]]))) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = xlab, y = ylab, fill = legend_title) +
    theme_minimal(base_size = 16) +
    theme(
      axis.line = element_line(color = "black", linewidth = 1),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, 
                                vjust = -1, margin = margin(b = 50)),
      axis.text.x = element_text(size = 14, hjust = 0.5),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 14, margin = margin(t = 15)),
      axis.title.y = element_text(size = 14, margin = margin(r = 15)),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

expectation_mean_plot <- plot_expectation_mean(cdata)
expectation_mean_plot

identication_mean_plot <- plot_expectation_mean(cdata_identication)
identication_mean_plot
identication_ratio_plot <- plot_ratio(identication_long2, expectation = 'identication')
identication_ratio_plot

expectation_plot <- plot_ratio(expectation_long, expectation = 'expectation',legend_title="社会流动预期")
expectation_plot
expectation_long$expectation

ide_expectation_mean_plot <- plot_expectation_mean(cdata_id_expectation)
ide_expectation_mean_plot
ide_expectation_ratio_plot <- plot_ratio(id_expectation_long2, expectation = 'id_expectation',legend_title='地位期待')
ide_expectation_ratio_plot
class(identication_long2$identication)

expectation_mean_plot <- ggplot(cdata, aes(x=Year, y = mean, group = Year)) +
  geom_point(aes(color = factor(Year)), size = 3) +
  geom_errorbar(data=cdata, aes(ymin=mean-2*se, ymax=mean+2*se, 
                                color=Year), width=0.2,linewidth = 1)  +
  labs(title = "各年份社会流动预期均值(95%置信区间)", x = "年份", y = "预期均值",fill = "") +
  theme_minimal(base_size = 16) + 
  scale_color_manual(
    name = "年份",  # 图例标题
    values = year_colors,  # 使用定义的颜色映射
    labels = c("2010年", "2013年", "2015年", "2018年", "2021年")  # 可选：修改图例标签
  ) +
  theme(
    axis.line = element_line(color = "black", linewidth = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # 标题居中加粗
    axis.text.x = element_text(size = 14, hjust = 0.5), 
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16, margin = margin(t = 15)),  # 调整X轴标题间距
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),  # 调整Y轴标题间距
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),  
    panel.grid.minor = element_blank()) 
expectation_mean_plot
# 使用ggplot绘制均值直方图并添加折线
expectation_mean <- aggregate(expectation ~ Year, data = expectation_long, mean)

ggplot(expectation_mean, aes(x = factor(Year), y = expectation)) +
  geom_col(aes(fill = factor(Year)), width = 0.7, show.legend = TRUE) +
  geom_line(aes(y = expectation * 1.06,group = 1), color = "#000", linewidth = 1.5) +  # 折线
  geom_point(aes(y = expectation * 1.06),size = 4) +  # 数据点
  coord_cartesian(ylim = c(1.0, max(expectation_mean$expectation) * 1.05)) +  
  geom_text(aes(label = round(expectation, 2)), 
            vjust = -1, size = 5, color = "black") +
  labs(title = "各年份社会流动预期均值", x = "年份", y = "预期均值",fill = "") +
  theme_minimal(base_size = 16) + 
  scale_fill_manual(
    name = "年份",  # 图例标题
    values = year_colors,  # 使用定义的颜色映射
    labels = c("2010年", "2013年", "2015年", "2018年", "2021年")  # 可选：修改图例标签
  ) +
  theme(
    axis.line = element_line(color = "black", linewidth = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # 标题居中加粗
    axis.text.x = element_text(size = 14, hjust = 0.5), 
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  # 调整X轴标题间距
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  # 调整Y轴标题间距
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),  
    panel.grid.minor = element_blank()) 

# 使用ggplot绘制直方图2
expectation_long2$number <- 1
expectation_long2 <- ddply(expectation_long2,'Year',transform,percent = 1/sum(number)*100)
ggplot(expectation_long2, aes(x = Year, y = percent, fill = expectation)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +  # 调整柱宽
  scale_fill_manual(values = c("向上流动" = "#70B48F", 
                               "不变" = "#FEA040",
                               "向下流动" = "#B54764"), name = "预期情况") + # 设置填充色和图例标题  # Y轴显示百分比
  scale_y_continuous(
    expand = c(0, 0)  
  ) +
  labs(
    x = "年份",
    y = "累计百分比例"
  ) +
  theme_minimal(base_size = 16) +  # 基础字号增大
  theme(, 
    axis.line =  element_line(color = "black", linewidth = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5,vjust = -1,margin = margin(b = 50)),  # 标题居中加粗
    axis.text.x = element_text(size = 14, hjust = 0.5),  # X轴文字倾斜45度
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  # 调整X轴标题间距
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  # 调整Y轴标题间距
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),  
    panel.grid.minor = element_blank())  # 移除垂直网格线

#年龄
age2$age <- as.factor(age2$age)
cage <- ddply(age, "year", summarise,
               N    = length(age),
               mean = mean(age),
               sd   = sd(age),
               se   = sd / sqrt(N)
)

#年龄均值图
age_plot1 <- ggplot(cage, aes(x=year, y = mean, group = year)) +
  geom_point(aes(color = factor(year)), size = 3) +
  geom_errorbar(data=cage, aes(ymin=mean-2*se, ymax=mean+2*se, 
                                color=year), width=0.2,linewidth = 1)  +
  geom_text(aes(label = sprintf("%.2f", mean), color = factor(year)), vjust = -3, 
            size = 5, show.legend = FALSE) +
  labs(x = "年份", y = "年龄均值",fill = "") +
  theme_minimal(base_size = 16) + 
  scale_color_manual(
    name = "年份",  # 图例标题
    values = year_colors,  # 使用定义的颜色映射
    labels = c("2010年", "2013年", "2015年", "2018年", "2021年")  # 可选：修改图例标签
  ) +
  scale_y_continuous(breaks = seq(floor(min(cage$mean - 2*cage$se)), 
                                  ceiling(max(cage$mean + 2*cage$se)), 
                                  by = 0.5)) +
  coord_cartesian(ylim = c(min(cage$mean - 4*cage$se), max(cage$mean + 8*cage$se))) +
  theme(
    axis.line = element_line(color = "black", linewidth = 1),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题居中加粗
    axis.text.x = element_text(size = 14, hjust = 0.5), 
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  # 调整X轴标题间距
    axis.title.y = element_text(size = 14, margin = margin(r = 15)),  # 调整Y轴标题间距
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),  
    panel.grid.minor = element_blank()) 
age_plot1

#各年龄段分布图
age_colors <- c("18-25岁" = "#70B48F", 
                 "25-30岁" = "#5861AC",
                 "30-35岁" = "#FEA040",
                 "35-40岁" = "#979998",
                 "40-45岁" = "#B54764")
age2$age <- factor(age2$age,levels = c('1','2','3','4','5'),labels = c('18-25岁','25-30岁','30-35岁','35-40岁','40-45岁'))
age2$number <- 1
age2 <- ddply(age2,'year',transform,percent = 1/sum(number)*100)
age_plot2 <- ggplot(age2 , aes(x = year, y = percent, fill = age)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +  # 调整柱宽
  scale_fill_manual(values = age_colors, name = "年龄组") + # 设置填充色和图例标题  # Y轴显示百分比
  scale_y_continuous(
    expand = c(0, 0)  
  ) +
  labs(
    x = "年份",
    y = "累计百分比"
  ) +
  theme_minimal(base_size = 16) +  # 基础字号增大
  theme(
        axis.line =  element_line(color = "black", linewidth = 1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题居中加粗
        axis.text.x = element_text(size = 14, hjust = 0.5), 
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 14, margin = margin(t = 15)),  # 调整X轴标题间距
        axis.title.y = element_text(size = 14, margin = margin(r = 15)),  # 调整Y轴标题间距
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),  
        panel.grid.minor = element_blank()) 
age_plot2

identication_long$identication <- as.numeric(identication_long$identication)
id_expectation_long$id_expectation <- as.numeric(id_expectation_long$id_expectation)
expect_interaction <- cbind(identication_long,id_expectation_long$id_expectation)
names(expect_interaction)[3] <- 'expectation'
ggplot(expect_interaction, aes(x = identication, y = expectation)) +
  geom_point(alpha = 0.6) +  # 散点图显示分布
  geom_density_2d() +        # 添加二维密度曲线
  facet_wrap(~ Year) +       # 按年份分面
  labs(x = "主观阶层认同", y = "期望阶层") +
  theme_minimal()
ggplot(expect_interaction, aes(x = identication, y = expectation)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_viridis_c() +
  facet_wrap(~ Year) +
  labs(x = "主观阶层认同", y = "地位期望")+
  coord_cartesian(
    xlim = c(0, 10),  # 强制x轴范围0-10
    ylim = c(0, 10),  # 强制y轴范围0-10
    expand = FALSE     # 关闭坐标轴扩展
  ) +
  scale_x_continuous(breaks = seq(0, 8, 2)) +  # 设置x轴刻度
  scale_y_continuous(breaks = seq(0, 10, 2))    # 设置y轴刻度
ggplot(expect_interaction, aes(x = identication, y = expectation)) +
  geom_hex(bins = 20) +
  scale_fill_viridis_c() +
  facet_wrap(~ Year) +
  coord_cartesian(
    xlim = c(0, 10),  # 强制x轴范围0-10
    ylim = c(0, 10),  # 强制y轴范围0-10
    expand = FALSE     # 关闭坐标轴扩展
  ) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +  # 设置x轴刻度
  scale_y_continuous(breaks = seq(0, 10, 2))    # 设置y轴刻度
expect_interaction <- as.data.frame(expect_interaction)
