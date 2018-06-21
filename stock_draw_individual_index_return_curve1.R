#### Author Comment Part
# modified on 2017-4-23

#### File Descriptiong Part
# 4zBkD?5D#:SCSZO8;/7VNv;y=pJURfBJPEO"

###Clear environment
rm(list = ls())

#### Library Quoting Part
##library(forecast)

suppressMessages(library(lubridate))

#### Function Definition Part


######Execution Part


setwd("d:/MyR/jijin")
source("input_and_preprocess_data.R")


##Specify the year and month range to draw plots
##Usually only numeric_Specied_Month need to be changed

numeric_Specied_Year <- 2018
numeric_Specied_Month <- 2:6  ## change here every time!
draw.label.month <- numeric_Specied_Month - 1 

# ls_value <- InputData(numeric_Specied_Year,numeric_Specied_Month)
ls_value <- InputData("simujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

zhishu <- read.csv(file = "stockzhongyaozhishu2018.csv", header = TRUE)

ts.result.chi <- GetIndividualFundReturn(numeric_Specied_Year,
                                  numeric_Specied_Month,
                                  "3`WSV.PD<[V5",
                                  ls_value, 0)


ts.result.jinglin <- GetIndividualFundReturn(numeric_Specied_Year,
                                       numeric_Specied_Month,
                                       ">0AVNH=!",
                                       ls_value, 0)

ts.result.zhongzheng500 <- zhishu[(zhishu$zhishu == "zhongzheng500" & 
                                     month(as.Date(zhishu$date)) %in% draw.label.month),  "zhangfu"]

ts.result.hushen300 <- zhishu[(zhishu$zhishu == "hushen300" & 
                                 month(as.Date(zhishu$date)) %in% draw.label.month),  "zhangfu"]

ts.result.shenzhenghongli <- zhishu[(zhishu$zhishu == "shenzhenghongli" & 
                                       month(as.Date(zhishu$date)) %in% draw.label.month),  "zhangfu"]

ylim.upper <- max(c(ts.result.chi, 
                    ts.result.jinglin,
                    ts.result.zhongzheng500,
                    ts.result.shenzhenghongli)) + 2

ylim.lower <- min(c(ts.result.chi, 
                    ts.result.jinglin,
                    ts.result.zhongzheng500,
                    ts.result.shenzhenghongli)) - 2

xlim.upper <- max(draw.label.month) + 0.5

xlim.lower <- min(draw.label.month)



par(mfrow = c(1,1))
par(bg = "lightgrey")

#draw grid lines

plot(ts.result.chi, type = "n", 
     ylim = c(ylim.lower, ylim.upper),
     xlim = c(xlim.lower, xlim.upper),
     axes = FALSE,
     xlab = "TB7]", ylab = "2018DjJURfBJ", main = "1j8KV8J}5DJURfBJTB6HGzO_(2018Dj3uRT@4)")

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



lines(x = draw.label.month,
      y = ts.result.chi, 
      type = "o", 
      col = "blue")
text(max(draw.label.month) + 0.33, tail(ts.result.chi, 1) , 
     "3`WSV.PD<[V5#(UT5$Qt#)", cex = 0.9, col = "blue")

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(6,10)), ] 

text(df.notshow$x,
     ts.result.chi[df.notshow$y] + 0.6,
     paste0(round(ts.result.chi[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "blue")

# df.show <- df[(df$x %in% c(4,6,10)), ] 
# 
# text(df.show$x,
#      ts.result.chi[df.show$y] - 1.2,
#      paste0(round(ts.result.chi[df.show$y], digits = 2),"%"),
#      cex = 0.9,
#      col = "blue")



##############################
lines(x = draw.label.month,
      y = ts.result.jinglin, type = "o", col = "brown")
text(max(draw.label.month) + 0.28, 
     tail(ts.result.jinglin, 1) + 0.05 , 
     ">0AVNH=!#(8_TF3L#)", col = "brown", cex = 0.9)
# text(draw.label.month,
#      ts.result.jinglin + 0.8,
#      paste0(round(ts.result.jinglin, digits = 2),"%"),
#      col = "brown", cex = 0.9)

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(3)), ] 

text(df.notshow$x,
     ts.result.jinglin[df.notshow$y] + 0.8,
     paste0(round(ts.result.jinglin[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "brown")



###################################

lines(x = draw.label.month,
      y = ts.result.hushen300, 
      type = "o", 
      col = "darkgreen", 
      lty = "dashed", lwd = 2)
text(max(draw.label.month) + 0.18, 
     tail(ts.result.hushen300,1) - 0.1, 
     ";&In300", 
     col = "darkgreen", 
     cex = 0.9)

# text(draw.label.month,
#      ts.result.hushen300 + 1.2,
#      paste0(round(ts.result.hushen300, digits = 2),"%"),
#      col = "darkgreen", cex = 0.9)


df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(1:2,4)), ] 

text(df.notshow$x,
     ts.result.hushen300[df.notshow$y] + 0.5,
     paste0(round(ts.result.hushen300[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "darkgreen")

df.show <- df[(df$x %in% c(1:2)), ]

text(df.show$x,
     ts.result.hushen300[df.show$y] - 0.7,
     paste0(round(ts.result.hushen300[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "darkgreen")



##############################################

lines(x = draw.label.month,
      y = ts.result.zhongzheng500, 
      type = "o", 
      col = "purple", 
      lty = "dashed", 
      lwd = 2)
text(max(draw.label.month) + 0.18, 
     tail(ts.result.zhongzheng500,1), "VPV$500", col = "purple", cex = 0.9)
# text(draw.label.month,
#      ts.result.zhongzheng500 - 1.2,
#      paste0(round(ts.result.zhongzheng500, digits = 2),"%"),
#      col = "purple", cex = 0.9)


df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(1:2)), ] 

text(df.notshow$x,
     ts.result.zhongzheng500[df.notshow$y] + 0.8,
     paste0(round(ts.result.zhongzheng500[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "purple")

df.show <- df[(df$x %in% c(1:2)), ]

text(df.show$x,
     ts.result.zhongzheng500[df.show$y] - 0.7,
     paste0(round(ts.result.zhongzheng500[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "purple")



##################################
lines(x = draw.label.month,
      y = ts.result.shenzhenghongli, 
      type = "o", 
      col = "deeppink",
      lty = "dashed", 
      lwd = 2)
text(max(draw.label.month) + 0.19, 
     tail(ts.result.shenzhenghongli,1), "InV$:l@{", col = "deeppink",cex = 0.9)

df <- data.frame(y = seq_along(draw.label.month), 
                 x = draw.label.month)

df.notshow <- df[!(df$x %in% c(3)), ] 

text(df.notshow$x,
     ts.result.shenzhenghongli[df.notshow$y] + 0.7,
     paste0(round(ts.result.shenzhenghongli[df.notshow$y], digits = 2),"%"),
     cex = 0.9,
     col = "deeppink")

df.show <- df[(df$x %in% c(3)), ]

text(df.show$x,
     ts.result.shenzhenghongli[df.show$y] - 0.7,
     paste0(round(ts.result.shenzhenghongli[df.show$y], digits = 2),"%"),
     cex = 0.9,
     col = "deeppink")

dev.copy(png, file = "stock_draw_individual_index_return_curve.png", units= "px", width=1000, height=600)
dev.off()
