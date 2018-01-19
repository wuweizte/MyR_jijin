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
numeric_Specied_Month <- 3:12  ## change here every time!
draw.label.month <- numeric_Specied_Month - 1  ## change here every time!

ls_value <- InputData("bondsimujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

ts.result.huaanmeiyuan002393 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                        numeric_Specied_Month,
                                                        "huaanmeiyuan002393",
                                                        ls_value, 0)

ts.result.penghuaquanqiu000290 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                          numeric_Specied_Month,
                                                          "penghuaquanqiu000290",
                                                          ls_value, 0)

ts.result.penghuaxinyong206004 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                          numeric_Specied_Month,
                                                          "penghuaxinyong206004",
                                                          ls_value, 0)

ts.result.guotaishuangli020020 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                          numeric_Specied_Month,
                                                          "guotaishuangli020020",
                                                          ls_value, 0)

ts.result.zhaoshangzhizao001869 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                          numeric_Specied_Month,
                                                          "zhaoshangzhizao001869",
                                                          ls_value, 0)

ts.result.dongfanghong001564 <- GetIndividualFundReturn(numeric_Specied_Year,
                                                           numeric_Specied_Month,
                                                           "dongfanghong001564",
                                                           ls_value, 0)

ts.result.quantile <- GetIndividualFundReturn(numeric_Specied_Year,
                                         numeric_Specied_Month,
                                         "null",
                                         ls_value, 0.8)




ylim.upper <- max(c(ts.result.huaanmeiyuan002393,
                    ts.result.penghuaquanqiu000290,
                    ts.result.penghuaxinyong206004,
                    ts.result.guotaishuangli020020,
                    ts.result.zhaoshangzhizao001869,
                    ts.result.dongfanghong001564)) + 1

ylim.lower <- min(c(ts.result.huaanmeiyuan002393,
                    ts.result.penghuaquanqiu000290,
                    ts.result.penghuaxinyong206004,
                    ts.result.guotaishuangli020020,
                    ts.result.zhaoshangzhizao001869,
                    ts.result.dongfanghong001564)) - 1

xlim.upper <- max(draw.label.month) + 0.7

xlim.lower <- min(draw.label.month)



par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines
#browser()
plot(ts.result.huaanmeiyuan002393, type = "n", 
     ylim = c(ylim.lower, ylim.upper),
     xlim = c(xlim.lower, xlim.upper),
     axes = FALSE,
     xlab = "月份", ylab = "2017年收益率", main = "债券基金样本的收益率月度曲线(2017年初以来)")

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


###########
lines(draw.label.month,
      ts.result.huaanmeiyuan002393, 
      type = "o", col = "blue")

text(max(draw.label.month) + 0.45, tail(ts.result.huaanmeiyuan002393, 1) - 0.2 , 
     "华安美元002393", cex = 0.7, col = "blue")

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(2,4,6,10)), ] 
# df.show <- df[df$x %in% c(4), ] 
# browser()

text(df.notshow$x,
     ts.result.huaanmeiyuan002393[df.notshow$y] - 0.25,
     paste0(round(ts.result.huaanmeiyuan002393[df.notshow$y], digits = 2),"%"),
     cex = 0.7,
     col = "blue")

# text(df.show$x,
#      ts.result.huaanmeiyuan002393[df.show$y] + 0.5,
#      paste0(round(ts.result.huaanmeiyuan002393[df.show$y], digits = 2),"%"),
#      cex = 0.7,
#      col = "blue")

#############
lines(draw.label.month,
      ts.result.penghuaquanqiu000290, 
      type = "o",
      col = "darkorange2")


text(max(draw.label.month) + 0.45, tail(ts.result.penghuaquanqiu000290, 1) , 
     "鹏华全球000290", cex = 0.7, col = "darkorange2")

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(2:5,7,8,9:11)), ] 
df.show <- df[df$x %in% c(10:11), ]

text(df.notshow$x,
     ts.result.penghuaquanqiu000290[df.notshow$y] + 0.5,
     paste0(round(ts.result.penghuaquanqiu000290[df.notshow$y], digits = 2),"%"),
     cex = 0.7,
     col = "darkorange2")

text(df.show$x,
     ts.result.penghuaquanqiu000290[df.show$y] - 0.4,
     paste0(round(ts.result.penghuaquanqiu000290[df.show$y], digits = 2),"%"),
     cex = 0.7,
     col = "darkorange2")

#############
lines(draw.label.month,
      ts.result.penghuaxinyong206004, 
      type = "o")


text(max(draw.label.month) + 0.45, tail(ts.result.penghuaxinyong206004, 1), 
     "鹏华信用206004", cex = 0.7, col = "black")

# text(draw.label.month,
#      ts.result.penghuaxinyong206004 - 0.25,
#      paste0(round(ts.result.penghuaxinyong206004, digits = 2),"%"),
#      cex = 0.7)


df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.show <- df[!(df$x %in% c(11)), ] 

text(df.show$x,
     ts.result.penghuaxinyong206004[df.show$y] - 0.5,
     paste0(round(ts.result.penghuaxinyong206004[df.show$y], digits = 2),"%"),
     cex = 0.7)

#############
lines(draw.label.month,
      ts.result.guotaishuangli020020, 
      type = "o", col = "purple")


text(max(draw.label.month) + 0.45, tail(ts.result.guotaishuangli020020, 1) + 0.3 , 
     "国泰双利020020", cex = 0.7, col = "purple")

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(6)), ] 
# df.show <- df[df$x %in% c(6), ]
# browser()

text(df.notshow$x,
     ts.result.guotaishuangli020020[df.notshow$y] + 0.5,
     paste0(round(ts.result.guotaishuangli020020[df.notshow$y], digits = 2),"%"),
     cex = 0.7,
     col = "purple")

# text(df.show$x,
#      ts.result.guotaishuangli020020[df.show$y] - 0.5,
#      paste0(round(ts.result.guotaishuangli020020[df.show$y], digits = 2),"%"),
#      cex = 0.7,
#      col = "purple")




#############
lines(draw.label.month,
      ts.result.zhaoshangzhizao001869, 
      type = "o", col = "darkgreen")


text(max(draw.label.month) + 0.45, tail(ts.result.zhaoshangzhizao001869, 1) , 
     "招商制造混合\n001869", cex = 0.7, col = "darkgreen")

text(draw.label.month,
     ts.result.zhaoshangzhizao001869 + 0.5,
     paste0(round(ts.result.zhaoshangzhizao001869, digits = 2),"%"),
     cex = 0.7, col = "darkgreen")

#############
lines(draw.label.month,
      ts.result.dongfanghong001564, 
      type = "o", col = "brown")


text(max(draw.label.month) + 0.5, tail(ts.result.dongfanghong001564, 1) , 
     "东方红京东大数据\n001564", cex = 0.7, col = "brown")

text(draw.label.month,
     ts.result.dongfanghong001564 + 0.5,
     paste0(round(ts.result.dongfanghong001564, digits = 2),"%"),
     cex = 0.7, col = "brown")

################
lines(draw.label.month,
      ts.result.quantile,
      type = "o", col = "red", lty = "dashed", lwd = 3)


text(max(draw.label.month) + 0.6, tail(ts.result.quantile, 1) , 
     "200只债券型私募基金的\n80%业绩分位点", cex = 0.7, col = "red")


df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(4)), ] 
df.show <- df[df$x %in% c(4), ] 

text(df.notshow$x,
     ts.result.quantile[df.notshow$y] + 0.5,
     paste0(round(ts.result.quantile[df.notshow$y], digits = 2),"%"),
     cex = 0.7,
     col = "red")

text(df.show$x,
     ts.result.quantile[df.show$y] - 0.5,
     paste0(round(ts.result.quantile[df.show$y], digits = 2),"%"),
     cex = 0.7,
     col = "red")

