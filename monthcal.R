#代码目的：用于比较私募排排网提供的基金收益率信息(Aug/Sep/Oct)
#modified on 2015-11-19

#####     Function Definition Part

##  1.Draw the monthly return density curve

density_mean_sd <- function(x, lwd, lty, lcol, zhao){
        
        #draw density Curve
        r <- density(x)
        lines(r, col = lcol, lwd = lwd, lty = 1)
        
        x_mean <- mean(x)
        x_sd <- x_mean - sd(x)
        x_median <- median(x)
        
        #prepare labels and relative positions
        mean_label <- paste('mean = ',format(x_mean, digits = 4), '%',sep = '')
        if (abs(x_mean - x_median) < 4.8 && abs(x_mean - x_median) > 3.5) {
                
                mean_label <- paste("\n",mean_label, sep = '')
        }
        
        median_label <- paste('median = ',format(x_median, digits = 4), '%', 
                              sep = '')
        median_pos <- 4
        if (abs(x_mean - x_median) < 3.5) {
                median_pos <- 2
        } 
        
        zhao_label <- paste('赵基金收益率 = ',format(zhao, digits = 4), '%', 
                            sep = '') 
        
        if (abs(x_sd - zhao) < 1.5) {
                zhao_label <- paste("\n\n",zhao_label, sep = '')
        }  else if (abs(x_sd - zhao) < 3) {
                zhao_label <- paste("\n",zhao_label, sep = '')
        } 
        
        #get the x coordinate for label
        seq1 <- length(r$x[r$x < x_mean]) + 1
        seq2 <- length(r$x[r$x < x_sd]) + 1
        
        seq3 <- length(r$x[r$x < x_median]) + 1
        seq4 <- length(r$x[r$x < zhao]) + 1
        
        #draw lines and labels
        lines(c(x_mean, x_mean), c(0, r$y[seq1]), col = 'red', lwd = 1, 
              lty = lty)
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
        text(zhao,r$y[seq4],labels = zhao_label,pos = 2, font = 3, cex = 1,
             col = 'green')  
}



#################################################
##  2.read csv files to get data

InputData <- function(id = 9:11) {
        
        ls_value_data <- list()
        for(i in id){
                CSVname <- paste("simujijin2015-",i,"-5",".csv",sep = "")
                CSVData <- read.csv(CSVname)
                
                value_data <- as.character(CSVData[,2])
                value_data <- value_data[value_data != "#NA" & 
                                                 value_data != "#VALUE!"]
                
                month_name <- paste("value-2015-",i,"-5",sep = "")
                ls_value_data[[month_name]] <- as.numeric(value_data)
                
                zhao_name <- paste("zhao-2015-",i,"-5",sep = "")
                ls_value_data[[zhao_name]] <- as.numeric(as.character(
                        CSVData[CSVData[,1] == "赤子之心价值",2]))
        }
        
        all_data <- numeric()
        for(i in id){
                month_name <- paste("value-2015-",i,"-5",sep = "")
                all_data <- c(all_data, ls_value_data[[month_name]])
        }
        
        ls_value_data[["min_data"]] <- min(all_data)
        ls_value_data[["max_data"]] <- max(all_data)
        
        return(ls_value_data)
}



## 3. Draw monthly return curve for the specified month

DrawMonthValueCurve <- function(ls_value_input, id = 9){
        
        MonthValueName <- paste("value-2015-",id,"-5",sep = "")
        month_return <- ls_value_input[[MonthValueName]]
        
        PlotMainName <- paste('私募基金收益分布密度曲线(截止到2015-',id ,'-5)',
                              sep = "")
        min_for_months <- ls_value_input[["min_data"]]
        max_for_months <- ls_value_input[["max_data"]]
        
        plot(month_return, type = "n", ylim = c(0, 0.02),
             xlim = c(min_for_months, max_for_months), 
             axes = FALSE,main = PlotMainName,
             xlab = '私募基金年收益率(%)',ylab = '分布密度')
        
        x_break_number <- c(min_for_months, 
                            seq(from = -50, to = 100, by = 10),200,
                            max_for_months)
        
        axis(1, at = x_break_number, labels = x_break_number)
        axis(2, las = 1)
        
        zhao_month <- paste("zhao-2015-",id,"-5",sep = "")
        zhao_value <- ls_value_input[[zhao_month]]
        
        density_mean_sd(x = month_return, lwd = 1, lty = 3, lcol = 'blue', 
                        zhao_value)        
}


#####################################################
## 4.大盘指数
DrawBoardIndex <- function(){
        
        
        B <- read.csv(file = "zhishu2015.csv", header = TRUE)
        
        Board_Index_array <- t(array(c(B[,5]), dim = c(4, 3)))
        
        dimnames(Board_Index_array) <- list(c("2015-8-31","2015-9-30",
                                              "2015-10-30"),
                                            c("上证综指","深证成指","创业板",
                                              "中小板"))
        
        bar_x <- barplot(Board_Index_array, beside = TRUE,ylim = c(-15,80), 
                         main = "板块指数涨幅(从2015年初开始)", 
                         ylab = "涨幅(%)", density = c(20,40,60), col = "green", 
                         las = 1,
                         legend.text = attr(Board_Index_array, "dimnames")[[1]],
                         args.legend = list(x = 5.5, y = 75, 
                                            text.width = strwidth("100000")))
        
        bar_y <- as.vector(Board_Index_array)
        
        text(bar_x, bar_y + sign(bar_y)*2, paste(bar_y, "%"),col = "red", 
             font = 3,cex = 1)        
}



## 5.月收益率曲线移动轨迹图

DrawMonthValueMovingCurve <- function(ls_value_input, id = 9:11){
        
        lid <- length(id)
        
        MonthValueName <- paste("value-2015-",id[lid],"-5",sep = "")
        month_return <- ls_value_input[[MonthValueName]]
        
        
        plot(month_return, type = "n", ylim = c(0, 0.02),
             xlim = c(-50, 200), 
             axes = FALSE,main = '私募基金收益分布密度曲线移动轨迹图',
             xlab = '私募基金年收益率(%)',ylab = '分布密度')
        
        x_break_number <- c(seq(from = -50, to = 100, by = 10), 200)
        
        axis(1, at = x_break_number, labels = x_break_number)
        axis(2, las = 1)
        
        lines(density(month_return), col = rainbow(lid)[lid], lwd = 2, lty = 1)
        
        if(lid > 1){
                
                for(i in 1:(lid - 1)){
                        
                        MonthValueName <- paste("value-2015-",id[i],"-5",
                                                sep = "")
                        month_return <- ls_value_input[[MonthValueName]]
                        
                        lines(density(month_return), col = rainbow(lid)[i], 
                              lwd = 1,lty = 1)
                        
                }        
        }
        
        lwd_legend <- c(rep(1,(lid - 1)), 2)
        
        Text_legend <- paste("截止到2015-",id,"-5",sep = "")
        legend("topright", col = rainbow(lid),lty = 1, lwd = lwd_legend,
               legend = Text_legend,text.width = strwidth("100000000"))           
}

######Execution Part

setwd("d:/MyR/jijin")

## plan how to place plots

layout_plot_matrix <- matrix(c(1,1,1,5,5,2,2,2,4,4,3,3,3,4,4),nr = 3, 
                             byrow = TRUE)
layout(layout_plot_matrix)

##read csv files to get data. The input months length can be larger than 3
ls_value <- InputData(9:11)

##Draw monthly return curves for the recent 3 months
lapply(9:11, DrawMonthValueCurve, ls_value_input = ls_value)

#大盘指数
DrawBoardIndex()

#月收益率曲线移动轨迹图, The input months length can be larger than 3
DrawMonthValueMovingCurve(ls_value, 9:11)
