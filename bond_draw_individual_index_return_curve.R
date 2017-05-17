#### Author Comment Part
# modified on 2017-4-23

#### File Descriptiong Part
# ����Ŀ�ģ�����ϸ������������������Ϣ

###Clear environment
rm(list = ls())

#### Library Quoting Part
##library(forecast)

#### Function Definition Part


######Execution Part


setwd("d:/MyR/jijin")
source("input_and_preprocess_data.R")


##Specify the year and month range to draw plots
##Usually only numeric_Specied_Month need to be changed

numeric_Specied_Year <- 2017
numeric_Specied_Month <- 3:5  ## change here every time!
draw.label.month <- 2:4  ## change here every time!

ls_value <- InputData("bondsimujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

zhishu <- read.csv(file = "bondzhongyaozhishu2017.csv", header = TRUE)

ts.result.huaanmeiyuan002393 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                        numeric_Specied_Month,
                                                        "huaanmeiyuan002393",
                                                        ls_value, 0)


ts.result.guotaishuangli020020 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                          numeric_Specied_Month,
                                                          "guotaishuangli020020",
                                                          ls_value, 0)

ts.result.zhaoshangzhizao001869 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                           numeric_Specied_Month,
                                                           "zhaoshangzhizao001869",
                                                           ls_value, 0)

ts.result.hushen300 <- zhishu[zhishu$zhishu == "hushen300",  "zhangfu"]
ts.result.zhongzhengquanzhai <- zhishu[zhishu$zhishu == "zhongzhengquanzhai",  "zhangfu"]

ylim.upper <- max(c(ts.result.huaanmeiyuan002393,
                    ts.result.guotaishuangli020020,
                    ts.result.zhaoshangzhizao001869)) + 1

ylim.lower <- min(c(ts.result.huaanmeiyuan002393,
                    ts.result.guotaishuangli020020,
                    ts.result.zhaoshangzhizao001869)) - 2
xlim.upper <- max(draw.label.month) + 0.3

xlim.lower <- min(draw.label.month)



par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines

plot(ts.result.huaanmeiyuan002393, type = "n", 
     ylim = c(ylim.lower, ylim.upper),
     xlim = c(xlim.lower, xlim.upper),
     axes = FALSE,
     xlab = "�·�", ylab = "2017��������", main = "��ծָ�����������¶�����(2017�������)")

box()

y.axis.grid.points <- seq(from = sign(ylim.lower) * (abs(ylim.lower) %/% 5) * 5 , 
                          to = sign(ylim.upper) * (abs(ylim.upper) %/% 5) * 5, 
                          by = 5)

abline(h = y.axis.grid.points, 
       v = numeric_Specied_Month - 1,
       col = "white", 
       lty = "solid",
       lwd = par("lwd"))

axis(1, at = numeric_Specied_Month - 1, labels = numeric_Specied_Month - 1,
     cex.axis = 0.8)

axis(2, las = 1, at = y.axis.grid.points, 
     labels = paste0(y.axis.grid.points,"%"), cex.axis = 0.8)

#################
lines(draw.label.month,
      ts.result.huaanmeiyuan002393, 
      type = "o", col = "blue")

text(4.1, tail(ts.result.huaanmeiyuan002393,1), "������Ԫ002393", cex = 0.7, col = "blue")

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

text(draw.label.month,
     ts.result.huaanmeiyuan002393 - 0.25,
     paste0(round(ts.result.huaanmeiyuan002393, digits = 2),"%"),
     cex = 0.7,
     col = "blue")


##########################

#############
lines(draw.label.month,
      ts.result.guotaishuangli020020, 
      type = "o", col = "purple")

text(4.1, 0.45, "��̩˫��020020", cex = 0.7, col = "purple")
text(draw.label.month,
     ts.result.guotaishuangli020020 + 0.25,
     paste0(round(ts.result.guotaishuangli020020, digits = 2),"%"),
     cex = 0.7, col = "purple")

#############
lines(draw.label.month,
      ts.result.zhaoshangzhizao001869, 
      type = "o", col = "darkgreen")

text(4.11, 5.5, "����������001869", cex = 0.7, col = "darkgreen")
text(draw.label.month,
     ts.result.zhaoshangzhizao001869 + 0.25,
     paste0(round(ts.result.zhaoshangzhizao001869, digits = 2),"%"),
     cex = 0.7, col = "darkgreen")

########################

lines(draw.label.month,
      ts.result.hushen300, 
      type = "o", 
      col = "darkorange", 
      lty = "dashed", 
      lwd = 2)
text(4.08, tail(ts.result.hushen300,1), "����300ָ��", col = "darkorange", cex = 0.7)
text(draw.label.month,
     ts.result.hushen300 - 0.25,
     paste0(round(ts.result.hushen300, digits = 2),"%"),
     col = "darkorange", cex = 0.7)

########################

lines(draw.label.month,
      ts.result.zhongzhengquanzhai, 
      type = "o", 
      col = "red", 
      lty = "dashed", 
      lwd = 2)
text(4.08, tail(ts.result.zhongzhengquanzhai,1), "��֤ȫծָ��", col = "red", cex = 0.7)
text(draw.label.month,
     ts.result.zhongzhengquanzhai - 0.25,
     paste0(round(ts.result.zhongzhengquanzhai, digits = 2),"%"),
     col = "red", cex = 0.7)
