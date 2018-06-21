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

numeric_Specied_Year <- 2018
numeric_Specied_Month <- 2:6  ## change here every time!
draw.label.month <- numeric_Specied_Month - 1  

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

ts.result.jinglin <- GetIndividualFundReturn(numeric_Specied_Year,
                                       numeric_Specied_Month,
                                       "景林稳健",
                                       ls_value, 0)

ts.result.qing <- GetIndividualFundReturn(numeric_Specied_Year,
                                         numeric_Specied_Month,
                                         "清水源1号",
                                         ls_value, 0)

ts.result.shang <- GetIndividualFundReturn(numeric_Specied_Year,
                                          numeric_Specied_Month,
                                          "上善若水3期",
                                          ls_value, 0)



ts.result.quantile <- GetIndividualFundReturn(numeric_Specied_Year,
                                         numeric_Specied_Month,
                                         "null",
                                         ls_value, 0.8)



ylim.upper <- max(c(ts.result.chi, 
                    ts.result.zhan,
                    ts.result.qing,
                    ts.result.shang,
                    ts.result.jinglin)) + 2

ylim.lower <- min(c(ts.result.chi, 
                    ts.result.zhan,
                    ts.result.qing,
                    ts.result.shang,
                    ts.result.jinglin)) - 2

xlim.upper <- max(draw.label.month) + 0.8

xlim.lower <- min(draw.label.month)



par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines

plot(ts.result.chi, type = "n", 
     ylim = c(ylim.lower, ylim.upper),
     xlim = c(xlim.lower, xlim.upper),
     axes = FALSE,
     xlab = "月份", ylab = "2018年收益率", main = "样本股票私募的收益率月度曲线(2018年初以来)")

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



lines(  x = draw.label.month,
        y = ts.result.chi, 
        type = "o", 
        col = "blue")
text(max(draw.label.month) + 0.43, tail(ts.result.chi, 1)  , 
     "赤子之心价值（赵丹阳）", cex = 0.9, col = "blue")
# text(draw.label.month,
#      ts.result.chi + 0.8,
#      paste0(round(ts.result.chi, digits = 2),"%"),
#      cex = 0.9,
#      col = "blue")


df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(3,5)), ] 

text(df.notshow$x,
     ts.result.chi[df.notshow$y] - 0.8,
     paste0(round(ts.result.chi[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "blue")

df.show <- df[(df$x %in% c(3,5)), ]

text(df.show$x,
     ts.result.chi[df.show$y] + 0.8,
     paste0(round(ts.result.chi[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "blue")


#####################################
lines(x = draw.label.month,
      y = ts.result.zhan, 
      type = "o")
text(max(draw.label.month) + 0.32, tail(ts.result.zhan, 1) - 0.2 , 
     "展博1期（陈锋）", cex = 0.9)

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

# df.notshow <- df[!(df$x %in% c(1,3)), ] 
# 
# text(df.notshow$x,
#      ts.result.zhan[df.notshow$y] - 0.8,
#      paste0(round(ts.result.zhan[df.notshow$y], digits = 2),"%"),
#      cex = 0.9)

df.show <- df[(df$x %in% c(1,3,5)), ]

text(df.show$x,
     ts.result.zhan[df.show$y] + 0.8,
     paste0(round(ts.result.zhan[df.show$y], digits = 2),"%"),
     cex = 0.9)

##################################
lines(x = draw.label.month,
      y = ts.result.jinglin, 
      type = "o", 
      col = "brown")
text(max(draw.label.month) + 0.4, tail(ts.result.jinglin, 1) + 0.2 , 
     "景林稳健（高云程）", col = "brown", cex = 0.9)

# text(draw.label.month,
#      ts.result.jinglin + 0.8,
#      paste0(round(ts.result.jinglin, digits = 2),"%"),
#      col = "brown", cex = 0.9)


df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(2)), ] 

text(df.notshow$x,
     ts.result.jinglin[df.notshow$y] + 1,
     paste0(round(ts.result.jinglin[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "brown")

df.show <- df[(df$x %in% c(2)), ] 

text(df.show$x,
     ts.result.jinglin[df.show$y] - 0.8,
     paste0(round(ts.result.jinglin[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "brown")


##########################

lines(x = draw.label.month,
      y = ts.result.qing, 
      type = "o", 
      col = "purple")
text(max(draw.label.month) + 0.4, tail(ts.result.qing, 1)  , 
     "清水源1号（张小川）", col = "purple", cex = 0.9)

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(2,4)), ] 

text(df.notshow$x,
     ts.result.qing[df.notshow$y] + 0.8,
     paste0(round(ts.result.qing[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "purple")

df.show <- df[(df$x %in% c(2)), ]

text(df.show$x,
     ts.result.qing[df.show$y] - 1,
     paste0(round(ts.result.qing[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "purple")


################################

lines(x = draw.label.month,
      y = ts.result.shang, 
      type = "o", 
      col = "darkgreen")
text(max(draw.label.month) + 0.43, tail(ts.result.shang, 1)  , 
     "上善若水3期（侯安扬）", col = "darkgreen", cex = 0.9)

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

# df.notshow <- df[!(df$x %in% c(1,3)), ] 
# 
# text(df.notshow$x,
#      ts.result.shang[df.notshow$y] - 0.8,
#      paste0(round(ts.result.shang[df.notshow$y], digits = 2),"%"),
#      cex = 0.9,
#      col = "darkgreen")

df.show <- df[(df$x %in% c(2:5)), ]

text(df.show$x,
     ts.result.shang[df.show$y] + 0.8,
     paste0(round(ts.result.shang[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "darkgreen")


######################################

lines(x = draw.label.month,
      y = ts.result.quantile, 
      type = "o", 
      col = "red", 
      lty = "dashed", 
      lwd = 2)
text(max(draw.label.month) + 0.43, tail(ts.result.quantile, 1) , 
     "5000只股票型私募基金的\n80%业绩分位点", col = "red", cex = 0.9)

# text(draw.label.month,
#      ts.result.quantile - 1.2,
#      paste0(round(ts.result.quantile, digits = 2),"%"),
#      col = "red", cex = 0.9)

df.notshow <- df[!(df$x %in% c(2:4)), ] 

text(df.notshow$x,
     ts.result.quantile[df.notshow$y] - 1.2,
     paste0(round(ts.result.quantile[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "red")

df.show <- df[(df$x %in% c(3,4)), ]

text(df.show$x,
     ts.result.quantile[df.show$y] + 0.8,
     paste0(round(ts.result.quantile[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "red")


dev.copy(png, file = "stock_draw_individual_fund_return_curve.png", units= "px", width=1000, height=600)
dev.off()
