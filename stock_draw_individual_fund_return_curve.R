#### Author Comment Part
# modified on 2016-12-20

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
numeric_Specied_Month <- 2:4  ## change here every time!
draw.label.month <- 1:3  ## change here every time!

ls_value <- InputData("simujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

ts.result.chi <- GetIndividualFundReturn(numeric_Specied_Year,
                                  numeric_Specied_Month,
                                  "����֮�ļ�ֵ",
                                  ls_value, 0)

ts.result.zhan <- GetIndividualFundReturn(numeric_Specied_Year,
                                      numeric_Specied_Month,
                                      "չ��1��",
                                      ls_value, 0)

ts.result.yun <- GetIndividualFundReturn(numeric_Specied_Year,
                                       numeric_Specied_Month,
                                       "������������4�ż����ʽ����мƻ�",
                                       ls_value, 0)

ts.result.qing <- GetIndividualFundReturn(numeric_Specied_Year,
                                         numeric_Specied_Month,
                                         "��ˮԴ1��",
                                         ls_value, 0)

ts.result.shang <- GetIndividualFundReturn(numeric_Specied_Year,
                                          numeric_Specied_Month,
                                          "����38��(������ˮ4��)",
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
     xlab = "�·�", ylab = "2017��������", main = "���˽ļ���������¶�����(2017�������)")

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
text(3.15, 9.12, "����֮�ļ�ֵ���Ե�����", cex = 0.7, col = "blue")
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
text(3.12, 1.91, "չ��1�ڣ��·棩", cex = 0.7)
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
text(3.12, 9.83, "����4�ţ�����ΰ��", col = "brown", cex = 0.7)
# text(draw.label.month[-c(2:4)],
#      ts.result.yun[draw.label.month[-c(2:4)]] - 1.5,
#      paste0(round(ts.result.yun[draw.label.month[-c(2:4)]], digits = 2),"%"),
#      col = "brown", cex = 0.7)
text(draw.label.month,
     ts.result.yun[draw.label.month] + 0.5,
     paste0(round(ts.result.yun[draw.label.month], digits = 2),"%"),
     col = "brown", cex = 0.7)

lines(ts.result.qing, type = "o", col = "purple")
text(3.13, 2.79, "��ˮԴ1�ţ���С����", col = "purple", cex = 0.7)
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
text(3.15, 1, "����38��(������ˮ4��)", col = "darkgreen", cex = 0.7)
text(draw.label.month,
     ts.result.shang[draw.label.month] - 0.5,
     paste0(round(ts.result.shang[draw.label.month], digits = 2),"%"),
     col = "darkgreen", cex = 0.7)

lines(ts.result.quantile, type = "o", col = "red", lty = "dashed", lwd = 2)
text(3.2, 6.2, "3500ֻ��Ʊ��˽ļ�����80%ҵ����λ��", col = "red", cex = 0.7)
text(draw.label.month, 
     ts.result.quantile[draw.label.month] + 0.5, 
     paste0(round(ts.result.quantile[draw.label.month], digits = 2),"%"), 
     col = "red", cex = 0.7)