################################################################
#话语动员何以可能？——数字缩略语的修辞风格、认知机制与动员效果#

#Yuxuan Su#
#Rixin College#
#Tsinghua University#

#Created on November 17, 2023#
################################################################

################################
#图1：1949-2003年人民日报中的数字缩略语云图#
################################

# 载入需要的包  
library(readxl)  
library(wordcloud2)  

# 定义数据路径和sheet名称  
data_path <- "C:/Users/SYX/Desktop/论文版本/论文-11.13版/附件3：数据库.xlsx"  
sheet_name <- "词云"  

# 从Excel文件中读取数据  
data <- read_excel(data_path, sheet = sheet_name)  

# 将数据汇总为数据框  
df <- data.frame(data)  

p <- my_graph <- wordcloud2(df, #数据
                       size = 15,
                       fontFamily = "微软雅黑",  
                       fontWeight = "600",
                       color = "random-light", 
                       backgroundColor = "white",
                       rotateRatio = 0.5,
)
p
################################
#图2：1949-2003年政治运动次数的变化趋势#
################################
  
library(readxl)    
library(ggplot2)    

# 从Excel文件中读取数据    
data <- read_excel("C:/Users/SYX/Desktop/论文版本/论文-11.13版/附件3：数据库.xlsx", sheet = "政治动员强度")  

# 使用ggplot2包创建柱状图    
p2 <- ggplot(data, aes(x = 年份, y = 政治动员强度)) +     
  geom_bar(stat = "identity", fill = "grey", color = "black", width = 0.7, position = "identity") +  # 设置填充颜色、边框颜色和柱子宽度
  theme_minimal() +     
  labs(x = "年份", y = "政治运动次数") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 14, by = 2), labels = scales::number_format(scale = 1, big.mark = ",", decimal.mark = ".", suffix = "")) +  # 设置纵轴刻度间隔和标签大小
  scale_x_continuous(labels = function(x) paste0(x)) +  # 设置横轴标签为连续数字
  theme(axis.line = element_line(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.text.x = element_text(size = 16),  # 横轴标签字号大小
        axis.text.y = element_text(size = 16),  # 纵轴标签字号大小
        axis.title = element_text(size = 18))  # 坐标轴标题字号大小

# 指定保存图片的宽度和高度  
width <- 10  # 宽度（以英寸为单位）  
height <- 6  # 高度（以英寸为单位）
p2
p2 <- ggsave("图2.png", p2, width = width, height = height, dpi=1000, device = "png")

################################
#图3：不同类型的数字缩略语总量#
################################

library(ggplot2)  

# 创建数据  
categories <- factor(c("数词+量词+语素", "数词+语素", "数词1+语素1+数词2+语素2"))  
values <- c(475, 655, 284)  

# 创建柱状图  
p3 <- ggplot(data = data.frame(categories, values), aes(x = categories, y = values)) +  
  geom_bar(stat = "identity", fill = "darkgrey", colour = "black", width = 0.4) + # 设置柱子宽度为0.4  
  theme_minimal() +  
  labs(x = "缩略类型", y = "话语总量") + # 设置x轴和y轴标题  
  geom_text(aes(label = values), position = position_dodge(width = 0.5), vjust = -0.5, size = 5) +  
  scale_y_continuous(limits = c(0, 700), expand = c(0, 0)) + # 设置纵轴范围并调整空白区域  
  theme(axis.line = element_line(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.text.x = element_text(size = 16),  # 横轴标签字号大小
        axis.text.y = element_text(size = 16),  # 纵轴标签字号大小
        axis.title = element_text(size = 18))  # 坐标轴标题字号大小
width <- 12 # 宽度（以像素为单位）  
height <- 6 # 高度（以像素为单位）  
p3
p3 <- ggsave("图3.png", p3, width = width, height = height, dpi=1000, device = "png")

################################
#图4：不同尾字声调的数字缩略语总量#
################################

library(ggplot2)  

categories <- factor(c("一声", "二声", "三声", "四声"))  
values <- c(580, 262, 311, 261)  

p4 <- ggplot(data = data.frame(categories, values), aes(x = categories, y = values)) +  
  geom_bar(stat = "identity", fill = "darkgrey", colour = "black", width = 0.4) +  
  theme_minimal() +  
  labs( x = "尾字声调", y = "话语总量") +  
  geom_text(aes(label = values), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +  
  scale_y_continuous(limits = c(0, 700), expand = c(0, 0)) +  
  theme(axis.line = element_line(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.text.x = element_text(size = 16),  # 横轴标签字号大小
        axis.text.y = element_text(size = 16),  # 纵轴标签字号大小
        axis.title = element_text(size = 18))  # 坐标轴标题字号大小
width <- 10 # 宽度（以像素为单位）  
height <- 6 # 高度（以像素为单位）  
p4
p4 <- ggsave("图4.png", p4, width = width, height = height, dpi=1000, device = "png")

################################
#图5：不同字符长度的数字缩略语总量#
################################

     
p5 <- ggsave("图5.png", p5, width = width, height = height, dpi=1000, device = "png")

################################
#图6：数字缩略语频数与政治动员强度变化趋势对比#
################################

#数据准备

years <- seq(from=1949, to=2003, by=1)
political_mobilization <- c(2,9,12,9,4,1,4,2,3,8,7,8,2,0,4,4,3,8,8,5,5,6,6,5,4,4,7,7,3,4,2,2,4,3,5,4,1,2,1,1,2,1,1,1,0,1,2,5,6,6,7,8,8,8,9)
discourse_quantity <- c(132,98,224,6322,924,452,1720,830,1378,2948,3152,7888,3040,448,542,1242,2052,1674,7020,5150,3860,3426,3500,2536,1754,2284,1892,7442,5138,3318,2924,2434,3134,3062,3462,3426,2774,2388,2978,3040,3070,3796,1376,958,4394,4926,3527,3952,5110,3481,7006,9162,11574,13095,16295)

# 转换为数据框格式

mobilization_discourse <- data.frame(年份 = years, 政治动员强度 = political_mobilization, 数字缩略语频数 = discourse_quantity)

# 开始画图

library(ggplot2)

p <- ggplot(mobilization_discourse, aes(x = 年份))

# 两条折线，线宽为0.8

p <- p + geom_line(aes(y = 数字缩略语频数/800, color = "数字缩略语频数", linetype = "数字缩略语频数"), size = 0.8)

p <- p + geom_line(aes(y = 政治动员强度, color = "政治动员强度", linetype = "政治动员强度"), size = 0.8)

# 添加图例，政治动员强度为实现，数字缩略语频数为虚线

p <- p + scale_linetype_manual(values = c("政治动员强度" = "solid", "数字缩略语频数" = "dashed"), 
                               name = "图例",
                               labels = c("数字缩略语频数", "政治动员强度")) +
  scale_color_manual(values = c("政治动员强度" = "black", "数字缩略语频数" = "black"),
                     name = "图例",
                     labels = c("数字缩略语频数", "政治动员强度"))

# 设置右y轴的刻度

p <- p + scale_y_continuous(sec.axis = sec_axis(~. *800, name = "数字缩略语频数"))
p <- p + scale_y_continuous(name = "政治动员强度", sec.axis = sec_axis(~. * 800, name = "数字缩略语频数"))

# 坐标轴与图例美化
p <- p + theme(
  
               #绘制坐标轴
               axis.line = element_line(color = "black"), # 坐标轴颜色为黑色
               axis.text.x = element_text(size = 16), # x轴文本字体大小为16
               axis.text.y = element_text(size = 16), # y轴文本字体大小为16
               axis.title.x = element_text(margin = margin(t = 20)), # 标签与x轴距离为20
               axis.title.y = element_text(margin = margin(r = 20)), # 标签与y轴距离为20
               axis.text.y.right = element_text(margin = margin(r = 20)), # 标签与右边y轴距离为20
               axis.title = element_text(size = 18), # 坐标轴标签字体大小为18
               
               #绘制图例
               legend.position = "left",  # 图例放在图片的左边
               legend.margin = margin(r = 30), # 图例距离右边图片的距离为30
               legend.spacing.y = unit(2, "lines"), #　图例垂直方向的间距为2
               axis.ticks.length = unit(0.2, "cm"), # 坐标轴刻度线的长度为0.2cm
               legend.title = element_text(size = 15), # 图例标题的字号为15
               legend.text = element_text(size = 15),  # 图例文本的字号为15
               
              )

# 创建矩形的数据框
shadow_df <- data.frame(
  xmin = c(1950, 1958, 1966, 1975, 2000),
  xmax = c(1954, 1960, 1968, 1977, 2003),
  ymin = -Inf,
  ymax = Inf
)

# 添加阴影条带,分别为1950-1954，1958-1960，1966-1968，1975-1977，2000-2003
p6 <- p + geom_rect(data = shadow_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                   fill = "gray80", alpha = 0.6, inherit.aes = FALSE)
p6

# 保存图片

width <- 10 # 宽度（以像素为单位）  
height <- 5 # 高度（以像素为单位）  
p6 <- ggsave("图6.png", p6, width = width, height = height, dpi=1000, device = "png")

