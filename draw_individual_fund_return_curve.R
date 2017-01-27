#### Author Comment Part
# modified on 2016-12-20

#### File Descriptiong Part
# 代码目的：用于细化分析基金收益率信息

###Clear environment
rm(list = ls())

#### Library Quoting Part
##library(forecast)

#### Function Definition Part
GetIndividualFundReturn <- function(arg.year = 2016, arg.month = 2,
                            arg.fund.name, arg.ls.value, arg.quantile) {
        
        # 2.read csv files to get data
        #
        # Args:
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回ls_value列表中的保存指定时间的csv数据的数据框元素的名称
        

        # browser()
        result <- NULL
        for (i in arg.month){
                
                csv.month.name <- GetCSVMonthName(arg.year, 
                                                  arg.month = i)
                
                csv.data <- arg.ls.value[[csv.month.name]]
                
                if(arg.quantile == 0){
                        #browser()
                        result[i - 1] <- csv.data[csv.data[,1] == arg.fund.name,2]        
                }else{
                        # browser()
                        result[i - 1] <- quantile(csv.data[,2], arg.quantile)
                }
                
                
        }
        
        return(result)
        
}


######Execution Part


setwd("d:/MyR/jijin")
source("input_and_preprocess_data.R")


##Specify the year and month range to draw plots
##Usually only numeric_Specied_Month need to be changed

numeric_Specied_Year <- 2016
numeric_Specied_Month <- 2:13  ## change here every time!
draw.label.month <- 1:12  ## change here every time!

ls_value <- InputData(numeric_Specied_Year,numeric_Specied_Month)

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

# ts.result.xing <- GetIndividualFundReturn(numeric_Specied_Year,
#                                           numeric_Specied_Month,
#                                           "星石1期",
#                                           ls_value, 0)

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

par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines

plot(ts.result.chi, type = "n", ylim = c(ylim.lower, ylim.upper),axes = FALSE,
     xlab = "月份", ylab = "2016年收益率", main = "知名私募的收益率月度曲线(2016年)")

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
text(7.25, 28, "赤子之心价值（赵丹阳）", cex = 0.7, col = "blue")
text(draw.label.month[-4], 
     ts.result.chi[draw.label.month[-4]] + 1.5, 
     paste0(round(ts.result.chi[draw.label.month[-4]], digits = 2),"%"),
     cex = 0.7,
     col = "blue") 
text(draw.label.month[4] + 0.25, 
     ts.result.chi[draw.label.month[4]], 
     paste0(round(ts.result.chi[draw.label.month[4]], digits = 2),"%"),
     cex = 0.7,
     col = "blue") 


lines(ts.result.zhan, type = "o")
text(7.25, -12, "展博1期（陈锋）", cex = 0.7)
text(draw.label.month, 
     ts.result.zhan[draw.label.month] - 1, 
     paste0(round(ts.result.zhan[draw.label.month], digits = 2),"%"),
     cex = 0.7)
# text(draw.label.month[c(1:2)], 
#      ts.result.zhan[draw.label.month[c(1:2)]] + 1.5, 
#      paste0(round(ts.result.zhan[draw.label.month[c(1:2)]], digits = 2),"%"),
#      cex = 0.7)
     
lines(ts.result.yun, type = "o", col = "brown")
text(7.25, 16, "昀沣4号（王亚伟）", col = "brown", cex = 0.7)
text(draw.label.month[-c(2:4)], 
     ts.result.yun[draw.label.month[-c(2:4)]] - 1.5, 
     paste0(round(ts.result.yun[draw.label.month[-c(2:4)]], digits = 2),"%"), 
     col = "brown", cex = 0.7)
text(draw.label.month[c(2:4)], 
     ts.result.yun[draw.label.month[c(2:4)]] + 1.5, 
     paste0(round(ts.result.yun[draw.label.month[c(2:4)]], digits = 2),"%"), 
     col = "brown", cex = 0.7)

lines(ts.result.qing, type = "o", col = "purple")
text(7.25, -4, "清水源1号（张小川）", col = "purple", cex = 0.7)
text(draw.label.month[-c(1:2)],
     ts.result.qing[draw.label.month[-c(1:2)]] - 1.5,
     paste0(round(ts.result.qing[draw.label.month[-c(1:2)]], digits = 2),"%"),
     col = "purple", cex = 0.7)
text(draw.label.month[c(1:2)],
     ts.result.qing[draw.label.month[c(1:2)]] + 1.5,
     paste0(round(ts.result.qing[draw.label.month[c(1:2)]], digits = 2),"%"),
     col = "purple", cex = 0.7)

# lines(ts.result.xing, type = "o", col = "green")
# text(7.25, -4, "（张小川）", col = "green", cex = 0.7)
# text(draw.label.month,
#      ts.result.qing[draw.label.month] - 1.5,
#      paste0(round(ts.result.qing[draw.label.month], digits = 2),"%"),
#      col = "purple", cex = 0.7)

lines(ts.result.quantile, type = "o", col = "red", lty = "dashed", lwd = 2)
text(7.25, 3, "1300只基金的80%业绩分位点", col = "red", cex = 0.7)
text(draw.label.month, 
     ts.result.quantile[draw.label.month] - 1, 
     paste0(round(ts.result.quantile[draw.label.month], digits = 2),"%"), 
     col = "red", cex = 0.7)