#代码目的：用于比较私募排排网提供的基金收益率信息(Aug/Sep/Oct)
#modified on 2015-11-14

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
        
        lines(c(x_mean, x_mean), c(0, r$y[seq1]), col = 'red', lwd = 1, 
              lty = lty)
        
        mean_label <- paste('mean = ',format(x_mean, digits = 4), '%',
                              sep = '')

        median_label <- paste('median = ',format(x_median, digits = 4), '%', 
                              sep = '')
        median_pos <- 4
        
        if (abs(x_mean - x_median) < 3.5) {
                median_pos <- 2
        } else if (abs(x_mean - x_median) < 4.8) {
                mean_label <- paste("\n",mean_label, sep = '')
        } 
                
        text(x_mean,r$y[seq1],labels = mean_label,pos = 4, font = 3, 
             cex = 1,col = 'red')
        
        lines(c(x_sd, x_sd), c(0, r$y[seq2]), col = 'red', lwd = 1, lty = lty)
        
        text(x_sd,r$y[seq2],
             labels = paste('mean - sd = ',format(x_sd, digits = 4), '%',
                            sep = ''),
             pos = 2, font = 3, cex = 1,col = 'red')
        
        lines(c(x_median, x_median), c(0, r$y[seq3]), col = 'red', lwd = 1, 
              lty = lty)
        

        text(x_median,r$y[seq3],labels = median_label,pos = median_pos, 
             font = 3, cex = 1,col = 'red')
        
        lines(c(zhao, zhao), c(0, r$y[seq4]), col = 'green', lwd = 1, lty = 6)
        
        zhao_label <- paste('赵基金收益率 = ',format(zhao, digits = 4), '%', 
                            sep = '') 
        
        if (abs(x_sd - zhao) < 1.5) {
                zhao_label <- paste("\n\n",zhao_label, sep = '')
        }  else if (abs(x_sd - zhao) < 3) {
                zhao_label <- paste("\n",zhao_label, sep = '')
        } 
        
        text(zhao,r$y[seq4],labels = zhao_label,pos = 2, font = 3, cex = 1,
             col = 'green')  
}

layout_plot_matrix <- matrix(c(1,1,1,5,5,2,2,2,4,4,3,3,3,4,4),nr = 3, 
                              byrow = TRUE)
layout(layout_plot_matrix)



#read csv files to get data
month201509 <- read.csv(file = "simujijin2015-9-5.csv", header = TRUE)

#有时会有一些#基金的收益率尚未获取，显示为#NA，此时会化为factor类型

options(warn = -1)

month201509_return <- as.numeric(as.character(month201509[,2]))
month201509_return <- month201509_return[!is.na(month201509_return)] 
#complete.cases函数更好

month201510 <- read.csv(file = "simujijin2015-10-5.csv", header = TRUE)
month201510_return <- as.numeric(as.character(month201510[,2])) #有时会有一些
#基金的收益率尚未获取，显示为#NA，此时会化为factor类型
month201510_return <- month201510_return[!is.na(month201510_return)]

month201511 <- read.csv(file = "simujijin2015-11-5.csv", header = TRUE)
month201511_return <- as.numeric(as.character(month201511[,2])) #有时会有一些
#基金的收益率尚未获取，显示为#NA，此时会化为factor类型
month201511_return <- month201511_return[!is.na(month201511_return)]

options(warn = 0)

min_for_3months <- min(month201509_return, month201510_return, 
                       month201511_return)

max_for_3months <- max(month201509_return, month201510_return, 
                       month201511_return)

# for data before 2015-9-5
plot(month201509_return, type = "n", ylim = c(0, 0.02),
     xlim = c(min_for_3months, max_for_3months), 
     axes = FALSE,main = '私募基金收益分布密度曲线(截止到2015-9-5)',
     xlab = '私募基金年收益率(%)',ylab = '分布密度')

x_break_number <- c(min_for_3months, 
                    seq(from = -50, to = 100, by = 10),200,
                    max_for_3months)

axis(1, at = x_break_number, labels = x_break_number)
axis(2, las = 1)

zhao_value <- as.numeric(as.character(
                        month201509[month201509[,1] == "赤子之心价值",2]))
density_mean_sd(x = month201509_return, lwd = 1, lty = 3, lcol = 'blue', 
                zhao_value)


# for data before 2015-10-5

plot(month201510_return, type = "n", ylim = c(0, 0.02),
     xlim = c(min_for_3months, max_for_3months), 
     axes = FALSE,main = '私募基金收益分布密度曲线(截止到2015-10-5)',
     xlab = '私募基金年收益率(%)',ylab = '分布密度')

x_break_number <- c(min_for_3months, 
                    seq(from = -50, to = 100, by = 10),200,
                    max_for_3months)

axis(1, at = x_break_number, labels = x_break_number)
axis(2, las = 1)

zhao_value <- as.numeric(as.character(
                         month201510[month201510[,1] == "赤子之心价值",2]))
density_mean_sd(x = month201510_return, lwd = 1, lty = 3, lcol = 'blue', 
                zhao_value)


# for data before 2015-11-5

plot(month201511_return, type = "n", ylim = c(0, 0.02),
     xlim = c(min_for_3months, max_for_3months), 
     axes = FALSE,main = '私募基金收益分布密度曲线(截止到2015-11-5)',
     xlab = '私募基金年收益率(%)',ylab = '分布密度')

x_break_number <- c(min_for_3months, 
                    seq(from = -50, to = 100, by = 10),200,
                    max_for_3months)

axis(1, at = x_break_number, labels = x_break_number)
axis(2, las = 1)

zhao_value <- as.numeric(as.character(
                           month201511[month201511[,1] == "赤子之心价值",2]))
density_mean_sd(x = month201511_return, lwd = 1, lty = 3, lcol = 'blue', 
                zhao_value)




#大盘指数
setwd("d:/MyR/jijin")
B <- read.csv(file = "zhishu2015.csv", header = TRUE)

Board_Index_array <- t(array(c(B[,5]), dim = c(4, 3)))

dimnames(Board_Index_array) <- list(c("2015-8-31","2015-9-30","2015-10-30"),
                                    c("上证综指","深证成指","创业板","中小板"))

bar_x <- barplot(Board_Index_array, beside = TRUE,ylim = c(-15,80), 
                 main = "板块指数涨幅(从2015年初开始)", ylab = "涨幅(%)",
                 density = c(20,40,60), col = "green", las = 1,
                 legend.text = attr(Board_Index_array, "dimnames")[[1]],
                 args.legend = list(x = 4.5, y = 75, 
                                    text.width = strwidth("10000")))

bar_y <- as.vector(Board_Index_array)

text(bar_x, bar_y + sign(bar_y)*2, paste(bar_y, "%"),col = "red", font = 3, 
     cex = 1)


#月收益率曲线移动轨迹图
plot(month201511_return, type = "n", ylim = c(0, 0.02),
     xlim = c(-50, 200), 
     axes = FALSE,main = '私募基金收益分布密度曲线移动轨迹图',
     xlab = '私募基金年收益率(%)',ylab = '分布密度')

x_break_number <- c(seq(from = -50, to = 100, by = 10), 200)

axis(1, at = x_break_number, labels = x_break_number)
axis(2, las = 1)


lines(density(month201509_return), col = rainbow(3)[1], lwd = 1, lty = 1)
lines(density(month201510_return), col = rainbow(3)[2], lwd = 1, lty = 1)
lines(density(month201511_return), col = rainbow(3)[3], lwd = 2, lty = 1)

legend("topright", col = rainbow(3),lty = 1, lwd = c(1,1,2),
       legend = c("截止到2015-9-5","截止到2015-10-5","截止到2015-11-5"),
       text.width = strwidth("10000000"))