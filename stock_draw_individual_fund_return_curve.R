#### Author Comment Part
# modified on 2016-12-20

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
numeric_Specied_Month <- 2:4  ## change here every time!
draw.label.month <- 1:3  ## change here every time!

ls_value <- InputData("simujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

ts.result.chi <- GetIndividualFundReturn(numeric_Specied_Year,
                                  numeric_Specied_Month,
                                  "赤子之心价值",
                                  ls_value, 0)

ts.result.zhan <- GetIndividualFundReturn(numeric_Specied_Year,
                                      numeric_Specied_Month,
                                      "展博1期",
                                      ls_value, 0)

ts.result.yun <- GetIndividualFundReturn(numeric_Specied_Year,
                                       numeric_Specied_Month,
                                       "华润信托昀沣4号集合资金信托计划",
                                       ls_value, 0)

ts.result.qing <- GetIndividualFundReturn(numeric_Specied_Year,
                                         numeric_Specied_Month,
                                         "清水源1号",
                                         ls_value, 0)

ts.result.shang <- GetIndividualFundReturn(numeric_Specied_Year,
                                          numeric_Specied_Month,
                                          "金蕴38号(上善若水4期)",
                                          ls_value, 0)

ts.result.quantile <- GetIndividualFundReturn(numeric_Specied_Year,
                                         numeric_Specied_Month,
                                         "null",
                                         ls_value, 0.8)



ylim.upper <- max(c(ts.result.chi, 
                    ts.result.zhan,
                    ts.result.qing,
                    ts.result.yun)) + 4

ylim.lower <- min(c(ts.result.chi, 
                    ts.result.zhan,
                    ts.result.qing,
                    ts.result.yun)) - 2

xlim.upper <- max(draw.label.month) + 0.3

xlim.lower <- 1



par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines

plot(ts.result.chi, type = "n", 
     ylim = c(ylim.lower, ylim.upper),
     xlim = c(xlim.lower, xlim.upper),
     axes = FALSE,
     xlab = "月份", ylab = "2017年收益率", main = "标杆私募的收益率月度曲线(2017年初以来)")

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
text(3.15, 9.12, "赤子之心价值（赵丹阳）", cex = 0.7, col = "blue")
not.show.label <- -c(1:2)
text(draw.label.month[not.show.label], 
     ts.result.chi[draw.label.month[not.show.label]] - 0.5, 
     paste0(round(ts.result.chi[draw.label.month[not.show.label]], digits = 2),"%"),
     cex = 0.7,
     col = "blue") 
show.label <- 2
text(draw.label.month[show.label],
     ts.result.chi[draw.label.month[show.label]] + 0.5,
     paste0(round(ts.result.chi[draw.label.month[show.label]], digits = 2),"%"),
     cex = 0.7,
     col = "blue")
show.label <- 1
text(draw.label.month[show.label],
     ts.result.chi[draw.label.month[show.label]] - 0.5,
     paste0(round(ts.result.chi[draw.label.month[show.label]], digits = 2),"%"),
     cex = 0.7,
     col = "blue")


lines(ts.result.zhan, type = "o")
text(3.12, 1.91, "展博1期（陈锋）", cex = 0.7)
not.show.label <- -c(1:2)
text(draw.label.month[not.show.label], 
     ts.result.zhan[draw.label.month[not.show.label]] - 0.5, 
     paste0(round(ts.result.zhan[draw.label.month[not.show.label]], digits = 2),"%"),
     cex = 0.7)
# text(draw.label.month[c(1:2)], 
#      ts.result.zhan[draw.label.month[c(1:2)]] + 1.5, 
#      paste0(round(ts.result.zhan[draw.label.month[c(1:2)]], digits = 2),"%"),
#      cex = 0.7)
     
lines(ts.result.yun, type = "o", col = "brown")
text(3.12, 9.83, "昀沣4号（王亚伟）", col = "brown", cex = 0.7)
# text(draw.label.month[-c(2:4)],
#      ts.result.yun[draw.label.month[-c(2:4)]] - 1.5,
#      paste0(round(ts.result.yun[draw.label.month[-c(2:4)]], digits = 2),"%"),
#      col = "brown", cex = 0.7)
text(draw.label.month,
     ts.result.yun[draw.label.month] + 0.5,
     paste0(round(ts.result.yun[draw.label.month], digits = 2),"%"),
     col = "brown", cex = 0.7)

lines(ts.result.qing, type = "o", col = "purple")
text(3.13, 2.79, "清水源1号（张小川）", col = "purple", cex = 0.7)
not.show.label <- -c(1:2)
text(draw.label.month[not.show.label],
     ts.result.qing[draw.label.month[not.show.label]] + 0.5,
     paste0(round(ts.result.qing[draw.label.month[not.show.label]], digits = 2),"%"),
     col = "purple", cex = 0.7)
show.label <- 2
text(draw.label.month[show.label],
     ts.result.qing[draw.label.month[show.label]] - 0.5,
     paste0(round(ts.result.qing[draw.label.month[show.label]], digits = 2),"%"),
     col = "purple", cex = 0.7)

lines(ts.result.shang, type = "o", col = "darkgreen")
text(3.15, 1, "金蕴38号(上善若水4期)", col = "darkgreen", cex = 0.7)
text(draw.label.month,
     ts.result.shang[draw.label.month] - 0.5,
     paste0(round(ts.result.shang[draw.label.month], digits = 2),"%"),
     col = "darkgreen", cex = 0.7)

lines(ts.result.quantile, type = "o", col = "red", lty = "dashed", lwd = 2)
text(3.2, 6.2, "3500只股票型私募基金的80%业绩分位点", col = "red", cex = 0.7)
text(draw.label.month, 
     ts.result.quantile[draw.label.month] + 0.5, 
     paste0(round(ts.result.quantile[draw.label.month], digits = 2),"%"), 
     col = "red", cex = 0.7)