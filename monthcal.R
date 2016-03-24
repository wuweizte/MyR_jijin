#代码目的：用于比较私募排排网提供的基金收益率信息(Aug/Sep/Oct)
#modified on 2016-3-24

#####     Function Definition Part

#################################################
##  1.plan how to place plots according to the input month vector
func_layout_plot <- function(Varmonth){

        i <- length(Varmonth)

        if(i == 1){
                layout_plot_matrix <- matrix(c(1,1,1,2,2,),nr = 1, 
                                             byrow = TRUE)
        }else if(i == 2) {
                layout_plot_matrix <- matrix(c(1,1,1,4,4,2,2,2,3,3),nr = 2, 
                                             byrow = TRUE)
        }else  {
                layout_plot_matrix <- matrix(c(1,1,1,5,5,2,2,2,4,4,3,3,3,4,4),
                                             nr = 3, 
                                             byrow = TRUE)
        }
        
        layout(layout_plot_matrix)
}



#################################################
##  2.read csv files to get data

InputData <- function(Varyear = 2016, Varmonth = 2) {
         #Varmonth can be a vector of more than one element 
         #to input more than one file
           
        ls_value_data <- list()
        for(i in Varmonth){
                if(i == 13){
                        CSVname <- paste("simujijin",Varyear + 1,"-1-5.csv")
                }else{
                        CSVname <- paste("simujijin",Varyear,"-",i,"-5",".csv", sep = "")
                }
                
                CSVData <- read.csv(CSVname)
                
                value_data <- as.character(CSVData[,2])
                value_data <- value_data[value_data != "#NA" & 
                                                 value_data != "#VALUE!"]
                
                if(i == 13){
                        month_name <- paste("value-",Varyear + 1,"-1-5")
                }else{
                        month_name <- paste("value-",Varyear,"-",i,"-5",sep = "")
                        
                }
                
                ls_value_data[[month_name]] <- as.numeric(value_data)
                
                if(i == 13){
                        zhao_name<- paste("zhao-",Varyear + 1,"-1-5")
                }else{
                        zhao_name <- paste("zhao-",Varyear,"-",i,"-5",sep = "")
                }
                
                ls_value_data[[zhao_name]] <- as.numeric(as.character(
                        CSVData[CSVData[,1] == "赤子之心价值",2]))
        }
        
        all_data <- numeric()
        for(i in Varmonth){
                
                if(i == 13){
                        month_name <- paste("value-",Varyear + 1,"-1-5")
                }else{
                        month_name <- paste("value-",Varyear,"-",i,"-5",sep = "")
                }
                all_data <- c(all_data, ls_value_data[[month_name]])
        }
        
        ls_value_data[["min_data"]] <- min(all_data)
        ls_value_data[["max_data"]] <- max(all_data)
        
        ls_value_data[["month_range"]] <- Varmonth
        
        return(ls_value_data)
}


## 3. Draw monthly return curve for the specified month

DrawMonthValueCurve <- function(ls_value_input, Varyear = 2016, Varmonth = 2){
  
  if(Varmonth == 13){
    MonthValueName <- paste("value-",Varyear + 1,"-1-5",sep = "")
    PlotMainName <- paste('私募基金收益分布密度曲线(截止到',Varyear+1,'-1-5)',
                          sep = "")
    zhao_month <- paste("zhao-",Varyear+1,"-1-5",sep = "")
  }else{
    MonthValueName <- paste("value-",Varyear,"-",Varmonth,"-5",sep = "")
    PlotMainName <- paste('私募基金收益分布密度曲线(截止到',Varyear,'-',Varmonth ,'-5)',
                          sep = "")
    zhao_month <- paste("zhao-",Varyear,"-",Varmonth,"-5",sep = "")
    
  }
  
  month_return <- ls_value_input[[MonthValueName]]
  
  
  min_for_months <- ls_value_input[["min_data"]]
  max_for_months <- ls_value_input[["max_data"]]
  
  
  plot(month_return, type = "n", ylim = c(0, 0.04),
       xlim = c(min_for_months, max_for_months), 
       axes = FALSE,main = PlotMainName,
       xlab = '私募基金年收益率(%)',ylab = '分布密度')
  
  x_break_number <- seq(from = round(min_for_months,digits = -1) - 10,
                        to = round(max_for_months,digits = -1) + 10, 
                        by = 10)
  
  
  axis(1, at = x_break_number, labels = x_break_number)
  axis(2, las = 1)
  
  #draw grid lines
  
  index_y_grid <- seq(from = 0, to = 0.040, by = 0.01)
  abline(h = index_y_grid, v = x_break_number, col = "white", 
         lty = "solid",lwd = par("lwd"))
  
  #draw rug lines
  #(rug function can't be used for making lines transparent effect)
  
  index_x_rug <- rep(month_return, each = 2)
  index_y_rug <- rep(c(-0.2,0), times = length(month_return))
  #lines(index_x_rug, index_y_rug, col = rgb(1,0,0,0.2))
  
  
  zhao_value <- ls_value_input[[zhao_month]]
  
  #let colors of density curves in monthly plot to be same as those in
  # moving curve
  
  month_range <- ls_value_input[["month_range"]]
  col_for_lines <- brewer.pal(length(month_range), "Set1") 
  
  col_seq <- seq_along(month_range)
  names(col_seq) <- month_range
  
  SelectedColor <- col_for_lines[col_seq[as.character(Varmonth)]]
  
  density_mean_sd(x = month_return, lwd = 1, lty = 5, lcol = SelectedColor, 
                  zhao_value, "brown", "black")        
  
}

##  4.Draw the monthly return density curve

density_mean_sd <- function(x, lwd, lty, lcol, zhao, NotzhaoCol, zhaoCol){
        
        #draw density Curve
        r <- density(x)
        lines(r, col = lcol, lwd = lwd, lty = 1)
        
        x_mean <- mean(x)
        x_sd <- x_mean + sd(x)
        x_median <- median(x)

        #get the x sequence number used to find the relative y coordinate 
        # of labels
        
        seq_mean <- length(r$x[r$x < x_mean]) + 1
        seq_sd <- length(r$x[r$x < x_sd]) + 1
        
        seq_median <- length(r$x[r$x < x_median]) + 1
        seq_zhao <- length(r$x[r$x < zhao]) + 1
        

        ##change the y coordinate of median label if the distance of 
        ##median label and mean label is too small
        
        y_coordinate_median_label <- r$y[seq_median]
        if (abs(r$y[seq_mean] - r$y[seq_median]) < 0.0012) {
        
                y_coordinate_median_label <- y_coordinate_median_label + 0.001
        }
        
        #prepare labels and relative positions
        mean_label <- paste(' mean = ',format(x_mean, digits = 4), '%',sep = '')
        
        median_label <- paste('median = ',format(x_median, digits = 4), '%', 
                              sep = '')
        median_pos <- 4
        if (abs(x_mean - x_median) < 3.5) {
                median_pos <- 2
        } 
        
        zhao_label <- paste('赵基金收益率 = ',format(zhao, digits = 4), '%', 
                            sep = '') 
        
        #   if (abs(x_sd - zhao) < 1.5) {
        #           zhao_label <- paste("\n\n",zhao_label, sep = '')
        #   }  else if (abs(x_sd - zhao) < 3) {
        #           zhao_label <- paste("\n",zhao_label, sep = '')
        #   } 
        
        sd_coordinate <- x_sd
        if ((abs(x_sd - zhao) < 0.15) & (x_sd > zhao)) {
                sd_coordinate <- x_sd + 1
        }
        
        
        #draw lines and labels
        lines(c(x_mean, x_mean), c(0, r$y[seq_mean]), col = NotzhaoCol, lwd = 1, 
              lty = lty)
        text(x_mean,r$y[seq_mean],labels = mean_label,pos = 2, font = 3, 
             cex = 1,col = NotzhaoCol)
        
        lines(c(sd_coordinate, sd_coordinate), c(0, r$y[seq_sd]), col = NotzhaoCol, 
              lwd = 1, lty = lty)
        text(x_sd,r$y[seq_sd],
             labels = paste('mean + sd = ',format(x_sd, digits = 3), '%',
                            sep = ''),
             pos = 4, font = 3, cex = 1,col = NotzhaoCol)
        
        lines(c(x_median, x_median), c(0, r$y[seq_median]), col = NotzhaoCol, 
              lwd = 1, 
              lty = lty)
        text(x_median,y_coordinate_median_label,labels = median_label,
             pos = median_pos, 
             font = 3, cex = 1,col = NotzhaoCol)
        
        lines(c(zhao, zhao), c(0, r$y[seq_zhao]), col = zhaoCol, lwd = 1, lty = 6)
        text(zhao,r$y[seq_zhao],labels = zhao_label,pos = 4, font = 3, cex = 0.9,
             col = zhaoCol)  
}


#####################################################
## 5.大盘指数


DrawBoardIndex <- function(){
  
  
  B <- read.csv(file = "zhishu2016.csv", header = TRUE)
  
  month_number <- length(B[,5])/4
  Board_Index_array <- t(array(c(B[,5]), dim = c(4, month_number)))
  
  dimnames(Board_Index_array) <- list(levels(B$date),
                                      c("上证综指","深证成指","创业板",
                                        "港股通精选100指数"))
  
  bar_x <- barplot(Board_Index_array, beside = TRUE,ylim = c(-35,10), 
                   main = "板块指数涨幅(从2016年初开始)", 
                   ylab = "涨幅(%)", 
                   density = seq(from = 20, to = 20 * month_number, by = 20),  
                   las = 1,col = "green",
                   legend.text = attr(Board_Index_array, "dimnames")[[1]],
                   args.legend = list(x = 3.5, y = 8, bty = "n", 
                                      text.width = strwidth("100000")))
  
  
  bar_y <- as.numeric(Board_Index_array)
  
  location_bar_x = bar_x
  
  if(FALSE){
    for(i in 1:(length(bar_y) - 1)){
      if(abs(bar_y[i] - bar_y[i + 1]) < 0.15){
        location_bar_x[i] = location_bar_x[i] - 0.4
        location_bar_x[i + 1] = location_bar_x[i + 1] + 0.6
      }else if(abs(bar_y[i] - bar_y[i + 1]) < 2){
        location_bar_x[i] = location_bar_x[i] - 0.6
        
      }
    }
    
  }
  
  text(location_bar_x, bar_y + sign(bar_y) * 2, paste(bar_y,"%",sep = ""),
       col = "red", font = 3,cex = 0.9)
  
}



## 6.月收益率曲线移动轨迹图

DrawMonthValueMovingCurve <- function(ls_value_input, Varyear = 2016, Varmonth = 2){
  
  lid <- length(Varmonth)
  
  if(max(Varmonth) == 13){
    MonthValueName <- paste("value-",Varyear + 1,"-1-5",sep = "")
    
    Text_legend <- c(paste("截止到",Varyear,"-",Varmonth[1:(lid-1)],"-5",sep = ""),
                     paste("截止到",Varyear + 1,"-1-5", sep = ""))
    
  }else{
    MonthValueName <- paste("value-",Varyear,"-",Varmonth[lid],"-5",sep = "")
    Text_legend <- paste("截止到",Varyear,"-",Varmonth,"-5",sep = "")
  }
  
  month_return <- ls_value_input[[MonthValueName]]
  
  
  plot(month_return, type = "n", ylim = c(0, 0.04),
       xlim = c(-50, 20), 
       axes = FALSE,main = '私募基金收益分布密度曲线移动轨迹图',
       xlab = '私募基金年收益率(%)',ylab = '分布密度')
  
  x_break_number <- seq(from = -50, to = 20, by = 5)
  
  axis(1, at = x_break_number, labels = x_break_number)
  axis(2, las = 1)
  
  #draw grid lines
  index_x_grid <- x_break_number
  index_y_grid <- seq(from = 0, to = 0.040, by = 0.01)
  abline(h = index_y_grid, v = index_x_grid, col = "white", 
         lty = "solid",lwd = par("lwd"))
  
  col_for_lines <- brewer.pal(lid, "Set1") 
  
  lines(density(month_return), col = col_for_lines[lid], lwd = 2, lty = 1)
  
  if(lid > 1){
    
    for(i in 1:(lid - 1)){
      
      MonthValueName <- paste("value-",Varyear,"-",Varmonth[i],"-5",
                              sep = "")
      month_return <- ls_value_input[[MonthValueName]]
      
      lines(density(month_return), col = col_for_lines[i], 
            lwd = 1,lty = 1)
      
    }        
  }
  
  
  lwd_legend <- c(rep(1,(lid - 1)), 2)
  
  
  legend("topright", col = col_for_lines,lty = 1, lwd = lwd_legend,
         legend = Text_legend,text.width = strwidth("100000000"),
         bty = "n")           
}

######Execution Part
library(RColorBrewer)

setwd("d:/MyR/jijin")

##Specify the year and month to draw plots
##Usually only numeric_Specied_Month need to be changed

numeric_Specied_Year <- 2016
numeric_Specied_Month <- 2:3  ## change only here every time!

##plan how to place plots according to the input month vector
func_layout_plot(numeric_Specied_Month) 

##read csv files to get data. The input months length can be larger than 3
ls_value <- InputData(numeric_Specied_Year,numeric_Specied_Month)


##Draw monthly return curves for the recent 3 months
op <- par(bg = "lightgrey")
Curve_related_to_Month <- numeric_Specied_Month

length_Specied_Month <- length(numeric_Specied_Month)
if(length_Specied_Month > 3){
        Curve_related_to_Month <- numeric_Specied_Month[
                (length_Specied_Month - 2):length_Specied_Month]
                
}

lapply(Curve_related_to_Month, DrawMonthValueCurve, ls_value_input = ls_value,
       Varyear = numeric_Specied_Year)
par(op)

#大盘指数: Manual action should not be needed.
DrawBoardIndex()


#月收益率曲线移动轨迹图, The input months length can be larger than 3
DrawMonthValueMovingCurve(ls_value, numeric_Specied_Year, 
                           numeric_Specied_Month)


