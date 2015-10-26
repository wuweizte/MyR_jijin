#代码目的：用于比较私募排排网提供的基金收益率信息
#modified on 2015-10-26

setwd("d:/MyR/jijin")

density_mean_sd <- function(x, lwd, lty, lcol, zhao){
  r <- density(x)
  lines(r, col = lcol, lwd = lwd, lty = 1)
  
  x_mean <- mean(x)
  x_sd <- x_mean - sd(x)

  x_median <- median(x)
  
  seq1 <- length(r$x[r$x < x_mean]) + 1
  seq2 <- length(r$x[r$x < x_sd]) + 1

  seq3 <- length(r$x[r$x < x_median]) + 1
  seq4 <- length(r$x[r$x < zhao]) + 1
  
  lines(c(x_mean, x_mean), c(0, r$y[seq1]), col = 'red', lwd = 1, lty = lty)
  text(x_mean,r$y[seq1],labels = paste('mean = ', format(x_mean, digits = 4), '%', sep = ''),
       pos = 4, font = 3, cex = 1,col = 'red')
  
  lines(c(x_sd, x_sd), c(0, r$y[seq2]), col = 'red', lwd = 1, lty = lty)
  text(x_sd,r$y[seq2],labels = paste('mean - sd = ',format(x_sd, digits = 4), '%', sep = ''),
       pos = 2, font = 3, cex = 1,col = 'red')
  
  lines(c(x_median, x_median), c(0, r$y[seq3]), col = 'red', lwd = 1, lty = lty)
  median_label <- paste('median = ',format(x_median, digits = 4), '%', sep = '')
  median_pos <- 4
  
  if (abs(x_mean - x_median) < 3.5) {
    median_pos <- 2
  }
  text(x_median,r$y[seq3],labels = median_label,pos = median_pos, font = 3, cex = 1,col = 'red')
  
  lines(c(zhao, zhao), c(0, r$y[seq4]), col = 'green', lwd = 1, lty = 6)
  zhao_label <- paste('赵基金收益率 = ',format(zhao, digits = 4), '%', sep = '') 
  if (abs(x_sd - zhao) < 1.5) {
    zhao_label <- paste("\n\n",zhao_label, sep = '')
  }  
  
  text(zhao,r$y[seq4],labels = zhao_label,pos = 2, font = 3, cex = 1,col = 'green')  
}

layout(matrix(c(1,1,1,4,4,2,2,2,3,3),nr = 2, byrow = TRUE))

# for data before 2015-9-5
month201509 <- read.csv(file = "simujijin2015-9-5.csv", header = TRUE)
month201509_return <- as.numeric(as.character(month201509[,2])) #有时会有一些基金的收益率尚未获取，显示为#NA，此时会化为factor类型
month201509_return <- month201509_return[!is.na(month201509_return)]

plot(month201509_return, type = "n", ylim = c(0, 0.02),xlim = c(-130, 375), axes = FALSE,
     main = '私募基金收益分布密度曲线(截止到2015-9-5)',xlab = '私募基金年收益率(%)',ylab = '分布密度')

x_break_number <- c(min(month201509_return), seq(from = -50, to = 100, by = 10),200,max(month201509_return))
axis(1, at = x_break_number, labels = x_break_number)


axis(2, las = 1)

zhao_value <- month201509[month201509[,1] == "赤子之心价值",2]
density_mean_sd(x = month201509_return, lwd = 1, lty = 3, lcol = 'blue', zhao_value)

# for data before 2015-10-5
month201510 <- read.csv(file = "simujijin2015-10-5.csv", header = TRUE)
month201510_return <- as.numeric(as.character(month201510[,2])) #有时会有一些基金的收益率尚未获取，显示为#NA，此时会化为factor类型
month201510_return <- month201510_return[!is.na(month201510_return)]

plot(month201510_return, type = "n", ylim = c(0, 0.02),xlim = c(-130, 375), axes = FALSE,
     main = '私募基金收益分布密度曲线(截止到2015-10-5)',xlab = '私募基金年收益率(%)',ylab = '分布密度')

x_break_number <- c(min(month201510_return), seq(from = -50, to = 100, by = 10),200,max(month201510_return))
axis(1, at = x_break_number, labels = x_break_number)


axis(2, las = 1)
zhao_value <- month201510[month201510[,1] == "赤子之心价值",2]
density_mean_sd(x = month201510_return, lwd = 1, lty = 3, lcol = 'blue', zhao_value)




#大盘指数
setwd("d:/MyR/jijin")
B <- read.csv(file = "zhishu2015.csv", header = TRUE)

Board_Index_array <- t(array(c(B[,5]), dim = c(4, 2)))

dimnames(Board_Index_array) <- list(c("2015-8-31","2015-9-30"),c("上证综指","深证成指","创业板","中小板"))
bar_x <- barplot(Board_Index_array, beside = TRUE,ylim = c(-15,45), 
                 main = "板块指数涨幅(从2015年初开始)", ylab = "涨幅(%)",
                 density = c(20,40), col = "green", las = 1,
                 legend.text = attr(Board_Index_array, "dimnames")[[1]],
                 args.legend = list(x = 4.5, y = 40, text.width = strwidth("1000")))
bar_y <- as.vector(Board_Index_array)
text(bar_x, bar_y + sign(bar_y)*2, paste(bar_y, "%"),col = "red", font = 3, cex = 1)

