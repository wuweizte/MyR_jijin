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

numeric_Specied_Year <- 2018
numeric_Specied_Month <- 2:6  ## change here every time!
draw.label.month <- numeric_Specied_Month - 1  ## change here every time!

ls_value <- InputData("bondsimujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

zhishu <- read.csv(file = "bondzhongyaozhishu2018.csv", header = TRUE)

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
                    ts.result.zhaoshangzhizao001869,
                    ts.result.hushen300)) + 1

ylim.lower <- min(c(ts.result.huaanmeiyuan002393,
                    ts.result.guotaishuangli020020,
                    ts.result.zhaoshangzhizao001869,
                    ts.result.hushen300)) - 2
xlim.upper <- max(draw.label.month) + 0.6

xlim.lower <- min(draw.label.month)



par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines

plot(ts.result.huaanmeiyuan002393, type = "n", 
     ylim = c(ylim.lower, ylim.upper),
     xlim = c(xlim.lower, xlim.upper),
     axes = FALSE,
     xlab = "月份", ylab = "2018年收益率", main = "股债指数的收益率月度曲线(2018年初以来)")

box()

y.axis.grid.points <- seq(from = sign(ylim.lower) * (abs(ylim.lower) %/% 2 + 1) * 2 , 
                          to = sign(ylim.upper) * (abs(ylim.upper) %/% 2 + 1) * 2, 
                          by = 2)

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

text(max(draw.label.month) + 0.36, tail(ts.result.huaanmeiyuan002393, 1) , 
     "华安美元002393", cex = 0.9, col = "blue")

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)


df.notshow <- df#[!(df$x %in% c(2,6,8,12)), ] 
# df.show <- df[df$x %in% c(6,8,12), ]
# browser()

text(df.notshow$x,
     ts.result.huaanmeiyuan002393[df.notshow$y] - 0.25,
     paste0(round(ts.result.huaanmeiyuan002393[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "blue")

# text(df.show$x,
#      ts.result.huaanmeiyuan002393[df.show$y] + 0.5,
#      paste0(round(ts.result.huaanmeiyuan002393[df.show$y], digits = 2),"%"),
#      cex = 0.9,
#      col = "blue")


##########################

#############
lines(draw.label.month,
      ts.result.guotaishuangli020020, 
      type = "o", col = "purple")

text(max(draw.label.month) + 0.36, tail(ts.result.guotaishuangli020020, 1) + 0.1 , 
     "国泰双利020020", cex = 0.9, col = "purple")


# text(draw.label.month,
#      ts.result.guotaishuangli020020 + 0.25,
#      paste0(round(ts.result.guotaishuangli020020, digits = 2),"%"),
#      cex = 0.9, col = "purple")

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(1:2)), ] 
# df.show <- df[df$x %in% c(6), ]
# browser()

text(df.notshow$x,
     ts.result.guotaishuangli020020[df.notshow$y] + 0.25,
     paste0(round(ts.result.guotaishuangli020020[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "purple")

# text(df.show$x,
#      ts.result.guotaishuangli020020[df.show$y] - 0.5,
#      paste0(round(ts.result.guotaishuangli020020[df.show$y], digits = 2),"%"),
#      cex = 0.9,
#      col = "purple")

#############
lines(draw.label.month,
      ts.result.zhaoshangzhizao001869, 
      type = "o", col = "darkgreen")

text(max(draw.label.month) + 0.32, tail(ts.result.zhaoshangzhizao001869, 1) , 
     "招商制造混合\n001869", cex = 0.9, col = "darkgreen")

text(draw.label.month,
     ts.result.zhaoshangzhizao001869 - 0.25,
     paste0(round(ts.result.zhaoshangzhizao001869, digits = 2),"%"),
     cex = 0.9, col = "darkgreen")

########################

lines(draw.label.month,
      ts.result.hushen300, 
      type = "o", 
      col = "darkorange", 
      lty = "dashed", 
      lwd = 2)

text(max(draw.label.month) + 0.32, tail(ts.result.hushen300, 1) , 
     "沪深300指数", cex = 0.9, col = "darkorange")

# text(draw.label.month,
#      ts.result.hushen300 - 0.5,
#      paste0(round(ts.result.hushen300, digits = 2),"%"),
#      col = "darkorange", cex = 0.9)

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(5)), ] 

text(df.notshow$x,
     ts.result.hushen300[df.notshow$y] - 0.35,
     paste0(round(ts.result.hushen300[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "darkorange")

df.show <- df[(df$x %in% c(5)), ]

text(df.show$x,
     ts.result.hushen300[df.show$y] + 0.25,
     paste0(round(ts.result.hushen300[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "darkorange")

########################

lines(draw.label.month,
      ts.result.zhongzhengquanzhai, 
      type = "o", 
      col = "red", 
      lty = "dashed", 
      lwd = 2)
text(max(draw.label.month) + 0.3, 
     tail(ts.result.zhongzhengquanzhai,1), 
     "中证全债指数", col = "red", cex = 0.9)

text(draw.label.month,
     ts.result.zhongzhengquanzhai + 0.35,
     paste0(round(ts.result.zhongzhengquanzhai, digits = 2),"%"),
     col = "red", cex = 0.9)

dev.copy(png, file = "bond_draw_individual_index_return_curve.png", units= "px", width=1000, height=600)
dev.off()
