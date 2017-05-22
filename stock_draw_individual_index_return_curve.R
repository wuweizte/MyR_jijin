#### Author Comment Part
# modified on 2017-4-23

#### File Descriptiong Part
# 代码目的：用于细化分析基金收益率信息

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
numeric_Specied_Month <- 2:5  ## change here every time!
draw.label.month <- 1:4  ## change here every time!

# ls_value <- InputData(numeric_Specied_Year,numeric_Specied_Month)
ls_value <- InputData("simujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

zhishu <- read.csv(file = "stockzhongyaozhishu2017.csv", header = TRUE)

ts.result.chi <- GetIndividualFundReturn(numeric_Specied_Year,
                                  numeric_Specied_Month,
                                  "赤子之心价值",
                                  ls_value, 0)


ts.result.yun <- GetIndividualFundReturn(numeric_Specied_Year,
                                       numeric_Specied_Month,
                                       "华润信托昀沣4号集合资金信托计划",
                                       ls_value, 0)

ts.result.zhongzheng500 <- zhishu[zhishu$zhishu == "zhongzheng500",  "zhangfu"]
ts.result.hushen300 <- zhishu[zhishu$zhishu == "hushen300",  "zhangfu"]
ts.result.shenzhenghongli <- zhishu[zhishu$zhishu == "shenzhenghongli",  "zhangfu"]

ylim.upper <- max(c(ts.result.chi, 
                    ts.result.yun)) + 2

ylim.lower <- min(c(ts.result.chi, 
                    ts.result.yun)) - 3

xlim.upper <- max(draw.label.month) + 0.55

xlim.lower <- 1



par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines

plot(ts.result.chi, type = "n", 
     ylim = c(ylim.lower, ylim.upper),
     xlim = c(xlim.lower, xlim.upper),
     axes = FALSE,
     xlab = "月份", ylab = "2017年收益率", main = "标杆指数的收益率月度曲线(2017年初以来)")

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



lines(ts.result.chi, type = "o", col = "blue")
# text(3.15, 9.12, "赤子之心价值（赵丹阳）", cex = 0.7, col = "blue")
text(max(draw.label.month) + 0.3, tail(ts.result.chi, 1) , 
     "赤子之心价值（赵丹阳）", cex = 0.7, col = "blue")
not.show.label <- -c(2,4)
text(draw.label.month[not.show.label], 
     ts.result.chi[draw.label.month[not.show.label]] - 0.5, 
     paste0(round(ts.result.chi[draw.label.month[not.show.label]], digits = 2),"%"),
     cex = 0.7,
     col = "blue") 
show.label <- c(2,4)
text(draw.label.month[show.label],
     ts.result.chi[draw.label.month[show.label]] + 0.5,
     paste0(round(ts.result.chi[draw.label.month[show.label]], digits = 2),"%"),
     cex = 0.7,
     col = "blue")
# show.label <- 1
# text(draw.label.month[show.label],
#      ts.result.chi[draw.label.month[show.label]] - 0.5,
#      paste0(round(ts.result.chi[draw.label.month[show.label]], digits = 2),"%"),
#      cex = 0.7,
#      col = "blue")



lines(ts.result.yun, type = "o", col = "brown")
# text(3.12, tail(ts.result.yun,1), "昀沣4号（王亚伟）", col = "brown", cex = 0.7)
text(max(draw.label.month) + 0.3, tail(ts.result.yun, 1) , 
     "昀沣4号（王亚伟）", col = "brown", cex = 0.7)
# text(draw.label.month[-c(2:4)],
#      ts.result.yun[draw.label.month[-c(2:4)]] - 1.5,
#      paste0(round(ts.result.yun[draw.label.month[-c(2:4)]], digits = 2),"%"),
#      col = "brown", cex = 0.7)
text(draw.label.month,
     ts.result.yun[draw.label.month] + 0.5,
     paste0(round(ts.result.yun[draw.label.month], digits = 2),"%"),
     col = "brown", cex = 0.7)

lines(ts.result.hushen300, type = "o", col = "darkgreen", lty = "dashed", lwd = 2)
text(max(draw.label.month) + 0.3, tail(ts.result.hushen300,1), "沪深300", col = "darkgreen", cex = 0.7)
text(draw.label.month[1],
     ts.result.hushen300[draw.label.month[1]] - 0.5,
     paste0(round(ts.result.hushen300[draw.label.month[1]], digits = 2),"%"),
     col = "darkgreen", cex = 0.7)
text(draw.label.month[-1],
     ts.result.hushen300[draw.label.month[-1]] + 0.5,
     paste0(round(ts.result.hushen300[draw.label.month[-1]], digits = 2),"%"),
     col = "darkgreen", cex = 0.7)


lines(ts.result.zhongzheng500, type = "o", col = "purple", lty = "dashed", lwd = 2)
text(max(draw.label.month) + 0.3, tail(ts.result.zhongzheng500,1), "中证500", col = "purple", cex = 0.7)
# text(draw.label.month[-c(2:4)],
#      ts.result.yun[draw.label.month[-c(2:4)]] - 1.5,
#      paste0(round(ts.result.yun[draw.label.month[-c(2:4)]], digits = 2),"%"),
#      col = "brown", cex = 0.7)
text(draw.label.month[-2],
     ts.result.zhongzheng500[draw.label.month[-2]] - 0.5,
     paste0(round(ts.result.zhongzheng500[draw.label.month[-2]], digits = 2),"%"),
     col = "purple", cex = 0.7)


lines(ts.result.shenzhenghongli, type = "o", col = "deeppink",lty = "dashed", lwd = 2)
text(max(draw.label.month) + 0.3, tail(ts.result.shenzhenghongli,1), "深证红利", col = "deeppink",cex = 0.7)
not.show.label <- -c(2,4)
text(draw.label.month[not.show.label],
     ts.result.shenzhenghongli[draw.label.month[not.show.label]] + 0.5,
     paste0(round(ts.result.shenzhenghongli[draw.label.month[not.show.label]], digits = 2),"%"),
     col = "deeppink", cex = 0.7)
show.label <- c(2,4)
text(draw.label.month[show.label],
     ts.result.shenzhenghongli[draw.label.month[show.label]] - 0.5,
     paste0(round(ts.result.shenzhenghongli[draw.label.month[show.label]], digits = 2),"%"),
     col = "deeppink",cex = 0.7)
