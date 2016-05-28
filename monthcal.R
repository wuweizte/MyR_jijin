#代码目的：用于比较私募排排网提供的基金收益率信息
#modified on 2016-5-27

#####     Function Definition Part

#################################################
##  1.plan how to place plots according to the input month vector
func_layout_plot <- function(Varmonth){

        i <- length(Varmonth)

        if(i == 1){
                layout_plot_matrix <- matrix(c(1,1,1,2,2),nr = 1, 
                                             byrow = TRUE)
        }else if(i == 2) {
                layout_plot_matrix <- matrix(c(1,1,1,4,4,2,2,2,3,3),nr = 2, 
                                             byrow = TRUE)
        }else  {
                if(file.exists("zhaobankuaizhishu2016.csv")){
                        #如果记录赵基金持仓板块涨幅的文件存在，则要多描绘一个图
                        layout_plot_matrix <- matrix(c(1,1,1,5,5,2,2,2,4,4,
                                                       3,3,3,6,6),
                                                     nr = 3, 
                                                     byrow = TRUE)                                        
                }else{
                        layout_plot_matrix <- matrix(c(1,1,1,5,5,2,2,2,4,4,
                                                       3,3,3,4,4),
                                                     nr = 3, 
                                                     byrow = TRUE)                
                }
        
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
                        CSVname <- paste("simujijin",Varyear,"-",i,"-5",".csv", 
                                         sep = "")
                }
                
                CSVData <- read.csv(CSVname)
                
                value_data <- as.character(CSVData[,2])
                value_data <- value_data[value_data != "#NA" & 
                                                 value_data != "#VALUE!"]
                
                if(i == 13){
                        month_name <- paste("value-",Varyear + 1,"-1-5")
                }else{
                        month_name <- paste("value-",Varyear,"-",i,"-5",
                                            sep = "")
                        
                }
                
                ls_value_data[[month_name]] <- as.numeric(value_data)
                
                if(i == 13){
                        zhao_name<- paste("zhao-",Varyear + 1,"-1-5")
                        zhanbo_name<- paste("zhanbo-",Varyear + 1,"-1-5")
                        yunfeng_name<- paste("yunfeng-",Varyear + 1,"-1-5")
                }else{
                        zhao_name <- paste("zhao-",Varyear,"-",i,"-5",sep = "")
                        zhanbo_name <- paste("zhanbo-",Varyear,"-",i,"-5",sep = "")
                        yunfeng_name <- paste("yunfeng-",Varyear,"-",i,"-5",sep = "")
                }
                
                ls_value_data[[zhao_name]] <- as.numeric(as.character(
                        CSVData[CSVData[,1] == "赤子之心价值",2]))
                ls_value_data[[zhanbo_name]] <- as.numeric(as.character(
                        CSVData[CSVData[,1] == "展博1期",2]))
                ls_value_data[[yunfeng_name]] <- as.numeric(as.character(
                        CSVData[CSVData[,1] == "华润信托昀沣4号集合资金信托计划",2]))
        }
        
        all_data <- numeric()
        for(i in Varmonth){
                
                if(i == 13){
                        month_name <- paste("value-",Varyear + 1,"-1-5")
                }else{
                        month_name <- paste("value-",Varyear,"-",i,"-5",
                                            sep = "")
                }
                all_data <- c(all_data, ls_value_data[[month_name]])
        }
        
        ls_value_data[["min_data"]] <- min(all_data)
        ls_value_data[["max_data"]] <- max(all_data)
        
        ls_value_data[["month_range"]] <- Varmonth
        
        return(ls_value_data)
}


## 3. Draw monthly return curve for the specified month

## 3-1 Execution Control Funciton
func_draw_all_month_curve <- function(arg_ls_value,numeric_Specied_Month,
                                      Varyear,
                                      numeric_input_ylim_upper = 0.04){
        ##let user to determine whether rug lines should be drawn.
        cat("\n\nIf you want to draw rug lines in x coordinate axis,please input yes.")
        cat("\nNow wait for your input: ")
        
        Draw_Rug_ANSWER <- 
                readline("?")
        
        Input_Var_rug_flag = 0
        if (substr(Draw_Rug_ANSWER, 1, 3) == "yes"){
                Input_Var_rug_flag = 1
        }
        
        ##Draw monthly return curves for the recent 3 months
        op <- par(bg = "lightgrey")
        Curve_related_to_Month <- numeric_Specied_Month
        
        length_Specied_Month <- length(numeric_Specied_Month)
        if(length_Specied_Month > 3){
                Curve_related_to_Month <- numeric_Specied_Month[
                        (length_Specied_Month - 2):length_Specied_Month]
                
        }
        
        lapply(Curve_related_to_Month, DrawMonthValueCurve, 
               ls_value_input = arg_ls_value,
               Varyear = numeric_Specied_Year, 
               Var_rug_flag = Input_Var_rug_flag,
               numeric_input_ylim_upper)
        
}


## 3-2 Prepare to draw monthly return curve
#     Var_rug_flag : flag variabel used to determine whether rug lines should
#                    be drawn.

DrawMonthValueCurve <- function(ls_value_input, Varyear = 2016, Varmonth = 2,
                                Var_rug_flag = 0,
                                numeric_input_ylim_upper){
        
        
        if(Varmonth == 13){
                MonthValueName <- paste("value-",Varyear + 1,"-1-5",sep = "")
                PlotMainName <- paste('私募基金收益分布密度曲线(截止到',
                                      Varyear+1,'-1-5)',
                                      sep = "")
                zhao_month <- paste("zhao-",Varyear+1,"-1-5",sep = "")
                zhanbo_month <- paste("zhanbo-",Varyear+1,"-1-5",sep = "")
        }else{
                MonthValueName <- paste("value-",Varyear,"-",Varmonth,"-5",
                                        sep = "")
                PlotMainName <- paste('私募基金收益分布密度曲线(截止到',
                                      Varyear,'-',Varmonth ,'-5)',
                                      sep = "")
                zhao_month <- paste("zhao-",Varyear,"-",Varmonth,"-5",sep = "")
                zhanbo_month <- paste("zhanbo-",Varyear,"-",Varmonth,"-5",sep = "")
                yunfeng_month <- paste("yunfeng-",Varyear,"-",Varmonth,"-5",sep = "")
        }
        
        month_return <- ls_value_input[[MonthValueName]]
        
        
        min_for_months <- ls_value_input[["min_data"]]
        max_for_months <- ls_value_input[["max_data"]]
        
        
        plot(month_return, type = "n", ylim = c(0, numeric_input_ylim_upper),
             xlim = c(min_for_months, max_for_months), 
             axes = FALSE,main = PlotMainName,
             xlab = '私募基金年收益率(%)',ylab = '分布密度')
        
        x_break_number <- seq(from = round(min_for_months,digits = -1) - 10,
                              to = round(max_for_months,digits = -1) + 10, 
                              by = 10)
        
        
        axis(1, at = x_break_number, labels = x_break_number)
        axis(2, las = 1)
        
        #draw grid lines
        
        index_y_grid <- seq(from = 0, to = numeric_input_ylim_upper, by = 0.01)
        abline(h = index_y_grid, v = x_break_number, col = "white", 
               lty = "solid",lwd = par("lwd"))
        
        #draw rug lines
        #(rug function can't be used for making lines transparent effect)
        
        index_x_rug <- rep(month_return, each = 2)
        index_y_rug <- rep(c(-0.2,0), times = length(month_return))
        
        if (Var_rug_flag == 1){
                lines(index_x_rug, index_y_rug, col = "tan2")
        }
        
        
        ## Prepare parameters for curve drawing funtion
        
        #let colors of density curves in monthly plot to be same as those in
        # moving curve
        
        month_range <- ls_value_input[["month_range"]]
        col_for_lines <- brewer.pal(length(month_range), "Set1") 
        
        col_seq <- seq_along(month_range)
        names(col_seq) <- month_range
        
        SelectedColor <- col_for_lines[col_seq[as.character(Varmonth)]]
        
        # Profits for specified funds
        zhao_value <- ls_value_input[[zhao_month]]
        zhanbo_value <- ls_value_input[[zhanbo_month]]
        yunfeng_value <- ls_value_input[[yunfeng_month]]
        
        # X-Y axis scale  : the length of Y axis is 1/6 of that of X axis
        
        XYscale <- ((max_for_months - min_for_months) / 6 ) / numeric_input_ylim_upper
        
        ## Call curve drawing funtion
        density_mean_sd(x = month_return, lwd = 1,  lcol = SelectedColor, 
                        zhao_value,zhanbo_value,yunfeng_value, XYscale)        
        
}

##  3-3 Draw the monthly return density curve

density_mean_sd <- function(x, lwd, lcol, zhao, zhanbo,yunfeng, arg_XYscale){
        
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
        seq_zhanbo <- length(r$x[r$x < zhanbo]) + 1
        seq_yunfeng <- length(r$x[r$x < yunfeng]) + 1
        

        ## Prepare data frame for line drawing and text labelling        
        DF_lineText <- data.frame(linename = c("mean","sd","mean + sd", "median",
                                               "zhao","zhanbo","yunfeng"),
                                  linex = c(mean(x),sd(x),mean(x) + sd(x), median(x), 
                                            zhao, zhanbo,yunfeng),
                                  liney = c(r$y[seq_mean],0,r$y[seq_sd],r$y[seq_median],
                                            r$y[seq_zhao],r$y[seq_zhanbo],r$y[seq_yunfeng]),
                                  stringsAsFactors = FALSE
                                  )
        ## Process figure setting
        DF_processed <- func_process_line_text(DF_lineText)
        DF_processed <- func_modify_text_position(DF_processed, arg_XYscale)
        
        ## Line drawing and text labelling        
        func_draw_line_text(DF_processed)

        ## The following browser command is used to decide the threshold set in 
        ## func_modify_text_position function through going to debug mode
        ##  to check the dist column of DF_processed
        
        #browser()
}

## 3-4 Input data frame with figure information about line and text
func_process_line_text <- function(arg_DF_lineText){
        DF_result <- as.data.frame(arg_DF_lineText)
        
        DF_result$drawy <- DF_result$liney
        
        DF_result$col = "brown"
        DF_result[DF_result$linename == "zhao","col"] <- "black"
        DF_result[DF_result$linename == "zhanbo","col"] <- "darkgreen"
        DF_result[DF_result$linename == "yunfeng","col"] <- "blue"
        
        ##lty
        DF_result$lty <- 5
        DF_result[DF_result$linename == "zhao","lty"] <- 6
        
        ##textcex
        DF_result$textcex <- 1
        DF_result[DF_result$linename == "zhao","textcex"] <- 0.9
        DF_result[DF_result$linename == "zhanbo","textcex"] <- 0.9
        DF_result[DF_result$linename == "yunfeng","textcex"] <- 0.9
        
        ##textpos
        DF_result$textpos <- 4
        DF_result[DF_result$linename == "median","textpos"] <- 3
        
        linex_median <- DF_result[DF_result$linename == "median","linex"]
        DF_result$textpos <- ifelse(DF_result$linex < linex_median,  2, 
                                    DF_result$textpos )

        ##textlabel
        DF_result$textlabel <- ""

        DF_result[DF_result$linename == "median","textlabel"] <- 
                paste('median = ',round(linex_median, digits = 2), '%', sep = '')

        linex_zhao <- DF_result[DF_result$linename == "zhao","linex"]
        DF_result[DF_result$linename == "zhao","textlabel"] <-
                paste('赵基金收益率 = ',format(linex_zhao, digits = 4), '%', 
                            sep = '') 
        
        linex_zhanbo <- DF_result[DF_result$linename == "zhanbo","linex"]
        DF_result[DF_result$linename == "zhanbo","textlabel"] <-
                paste('展博1期基金收益率 = ',format(linex_zhanbo, digits = 4), '%', 
                      sep = '') 
        
        linex_yunfeng <- DF_result[DF_result$linename == "yunfeng","linex"]
        DF_result[DF_result$linename == "yunfeng","textlabel"] <-
                paste('昀沣4号基金收益率 = ',format(linex_yunfeng, digits = 4), '%', 
                      sep = '') 
        
        linex_mean <- DF_result[DF_result$linename == "mean","linex"]
        linex_sd <- DF_result[DF_result$linename == "sd","linex"]
        linex_meansd <- DF_result[DF_result$linename == "mean + sd","linex"]
        
        DF_result[DF_result$linename == "mean + sd","textlabel"] <-
                paste('mean + sd = ',round(linex_mean, digits = 2), '%',
                      ' + ', round(linex_sd, digits = 2), '%',
                      ' ~~ ', round(linex_meansd, digits = 2), '%',
                      sep = '')

                
        #delete mean and sd value from data frame and return it
        
        tbl_df_result <- tbl_df(DF_result)
        tbl_df_filtered <- filter(tbl_df_result, linename != "mean" , 
                                  linename != "sd")

        return(tbl_df_filtered)
}

## 3-5 Modify text position in the density curve
func_modify_text_position <- function(arg_DF_processed_first, arg_XYscale){
       tbl_df_text_position <- tbl_df(arg_DF_processed_first)
       tbl_df_text_position <- arrange(tbl_df_text_position, linex)
       
       linex_median <- unlist(tbl_df_text_position %>% filter(linename == "median")
                                                   %>% select(linex))
       
       tbl_df_filter_left <- filter(tbl_df_text_position, linex <= linex_median)
       tbl_df_filter_right <- filter(tbl_df_text_position, linex >= linex_median)
       
       df_filter_left <- as.data.frame(tbl_df_filter_left %>% select(linex, liney))
       if(nrow(tbl_df_filter_left) > 1){

               df_filter_left_combine <- cbind(df_filter_left[-1,], 
                                               df_filter_left[-nrow(df_filter_left),])
               colnames(df_filter_left_combine) <- c("linex1", "liney1", 
                                                     "linex2", "liney2")
               
               ## the distance between 2 adjacent points is calculated through the 
               ## sum of subtract results of X coordinates and Y coordinates,
               ## but that of Y coordinates is needed to multiply with one scale
               ## because the scales of X axis and Y axis are different
               
               x1 <- df_filter_left_combine$linex1
               x2 <- df_filter_left_combine$linex2
               y1 <- df_filter_left_combine$liney1
               y2 <- df_filter_left_combine$liney2
               
               df_filter_left_combine$distance <- 
                       abs(x1- x2) + abs(y1- y2) * arg_XYscale
        
               rm(x1,x2,y1,y2)                         

               df_filter_left$distance <- c(Inf, df_filter_left_combine$distance)
       }else{
               df_filter_left$distance <- Inf
       }

       df_filter_right <- as.data.frame(tbl_df_filter_right %>% select(linex, liney))       
       if(nrow(tbl_df_filter_right) > 1){
               
               df_filter_right_combine <- cbind(df_filter_right[-nrow(df_filter_right),], 
                                                df_filter_right[-1,])
               colnames(df_filter_right_combine) <- c("linex1", "liney1", 
                                                      "linex2", "liney2")
               
               x1 <- df_filter_right_combine$linex1
               x2 <- df_filter_right_combine$linex2
               y1 <- df_filter_right_combine$liney1
               y2 <- df_filter_right_combine$liney2
               
               
               df_filter_right_combine$distance <- 
                       abs(x1- x2) + abs(y1- y2) * arg_XYscale                      
               
               rm(x1,x2,y1,y2)                 
               
               df_filter_right$distance <- c(df_filter_right_combine$distance,Inf)
       }else{
               df_filter_right$distance <- Inf
       }
       
               
       median_dist <- min(df_filter_left$distance[nrow(df_filter_left)],
                                  df_filter_right$distance[1])
       
       tbl_df_text_position$dist <- c(df_filter_left$distance[-nrow(df_filter_left)],
                                      median_dist,
                                      df_filter_right$distance[-1]
                                      )
       
       rownum_median <- which(tbl_df_text_position$linename == "median")
       
       ## This threshold can be observed through 'browser()' 
       ## in  density_mean_sd function
       dist_lowthreshold <- 3.4
       LargeStep <- 0.002
       MiddleStep <- LargeStep * 3/4
       SmallStep <- LargeStep/4
       
       for (i in 2:(nrow(tbl_df_text_position) - 1)) {
               if(tbl_df_text_position$dist[i] < dist_lowthreshold){
                       
                       tbl_df_text_position$drawy[i] <- 
                               tbl_df_text_position$drawy[i] + LargeStep
                       
                       if(i == rownum_median){
                               next
                       }
                       
                       tbl_df_text_position$drawy[rownum_median] <- 
                               tbl_df_text_position$drawy[rownum_median] + SmallStep
                       
                       if(i < (rownum_median - 1)){
                               tbl_df_text_position$drawy[(i + 1):(rownum_median - 1)] <- 
                                       tbl_df_text_position$drawy[(i + 1):(rownum_median - 1)] + MiddleStep
                       
                                       
                       }else if(i > (rownum_median + 1)){
                               tbl_df_text_position$drawy[(i - 1):(rownum_median + 1)] <- 
                                       tbl_df_text_position$drawy[(i - 1):(rownum_median + 1)] + MiddleStep
                               
                       }
               } 
                      
       }

       return(tbl_df_text_position)
}

## 3-6 Draw line and text in the density curve
func_draw_line_text <- function(arg_DF_processed){

        DF_Draw <- as.data.frame(arg_DF_processed)
        
        for (i in 1:nrow(DF_Draw)) {
                
                linex <- DF_Draw[i,"linex"]
                liney <- DF_Draw[i,"liney"]
                texty <- DF_Draw[i,"drawy"]
                
                rowcol <- DF_Draw[i,"col"]
                rowcex <- DF_Draw[i,"textcex"]
                rowlty <- DF_Draw[i,"lty"]
                rowpos <- DF_Draw[i,"textpos"]
                
                rowlabel <- DF_Draw[i,"textlabel"]
                
                ## Use 'approximately equal signal'
                ## In fact, the result of bquote function has 3 elements !
                rowlinename <- DF_Draw[i,"linename"]
                if(rowlinename == "mean + sd"){
                        textlabel_split <- strsplit(rowlabel, '~~')
                        rowlabel <- bquote(.(textlabel_split[[1]][1]) 
                                           %~~% .(textlabel_split[[1]][2]))        
                }
                        
                lines(c(linex, linex), c(0, liney), col = rowcol, lwd = 1, 
                      lty = rowlty)
                
                #对于中值，线与文字不再公用x坐标，文字稍微左移               
                if(rowlinename == "median"){
                        text(linex - 5,texty,labels = rowlabel,pos = rowpos, 
                             font = 3, 
                             cex = rowcex,
                             col = rowcol)  
                }else{
                        text(linex,texty,labels = rowlabel,pos = rowpos, 
                             font = 3, 
                             cex = rowcex,
                             col = rowcol)  
                }
                
        }
}


#####################################################
## 4.板块指数描绘函数：用于描绘大盘指数以及赵基金持仓板块

DrawBoardIndex <- function(filename, boardindexname, titlecontent,legendx,
                           legendy,barcol){
  
  
  B <- read.csv(file = filename, header = TRUE)
  
  month_number <- length(B[,5])/3
  Board_Index_array <- t(array(c(B[,5]), dim = c(3, month_number)))
  
  levels(B$date)
  dimnames(Board_Index_array) <- list(unique(B$date), boardindexname)
                                      
  
  bar_x <- barplot(Board_Index_array, beside = TRUE,ylim = c(-35,0), 
                   main = titlecontent, 
                   ylab = "涨幅(%)", 
                   density = seq(from = 20, to = 20 * month_number, by = 20),  
                   las = 1,col = barcol,
                   legend.text = attr(Board_Index_array, "dimnames")[[1]],
                   args.legend = list(x = legendx, 
                                      y = legendy, 
                                      bty = "n",
                                      ncol = (month_number %/% 3) + 1,  ##defind column counts of legend
                                      text.width = strwidth("10000")))
  
  
  bar_y <- as.numeric(Board_Index_array)
  
  location_bar_x = bar_x
  
  ## the following code chunk is not executed, and may be referred to later 
  ##if the position of text need to be modified
  
  #if(FALSE){
  #  for(i in 1:(length(bar_y) - 1)){
  #    if(abs(bar_y[i] - bar_y[i + 1]) < 0.15){
  #      location_bar_x[i] = location_bar_x[i] - 0.4
  #      location_bar_x[i + 1] = location_bar_x[i + 1] + 0.6
  #    }else if(abs(bar_y[i] - bar_y[i + 1]) < 2){
  #      location_bar_x[i] = location_bar_x[i] - 0.6
        
  #    }
  #  }
    
  #}
  ##end if
  
  text(location_bar_x, bar_y + sign(bar_y) * 2, paste(bar_y,"%",sep = ""),
       col = "red", font = 3,cex = 0.9)
  
}



## 5.月收益率曲线移动轨迹图

DrawMonthValueMovingCurve <- function(ls_value_input, Varyear = 2016, 
                                      Varmonth = 2,
                                      numeric_input_ylim_upper = 0.04,
                                      numeric_Specied_xlim_down = -50,
                                      numeric_Specied_xlim_upper = 20){
  
  lid <- length(Varmonth)
  
  if(max(Varmonth) == 13){
    MonthValueName <- paste("value-",Varyear + 1,"-1-5",sep = "")
    
    Text_legend <- c(paste("截止到",Varyear,"-",Varmonth[1:(lid-1)],"-5",
                           sep = ""),
                     paste("截止到",Varyear + 1,"-1-5", sep = ""))
    
  }else{
    MonthValueName <- paste("value-",Varyear,"-",Varmonth[lid],"-5",sep = "")
    Text_legend <- paste("截止到",Varyear,"-",Varmonth,"-5",sep = "")
  }
  
  month_return <- ls_value_input[[MonthValueName]]
  
  
  plot(month_return, type = "n", ylim = c(0, numeric_input_ylim_upper),
       xlim = c(numeric_Specied_xlim_down, numeric_Specied_xlim_upper), 
       axes = FALSE,main = '私募基金收益分布密度曲线移动轨迹图',
       xlab = '私募基金年收益率(%)',ylab = '分布密度')
  
  x_break_number <- seq(from = numeric_Specied_xlim_down, 
                        to = numeric_Specied_xlim_upper, by = 5)
  
  axis(1, at = x_break_number, labels = x_break_number)
  axis(2, las = 1)
  
  #draw grid lines
  index_x_grid <- x_break_number
  index_y_grid <- seq(from = 0, to = numeric_input_ylim_upper, by = 0.01)
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
  
  
  legend("topleft", col = col_for_lines,lty = 1, lwd = lwd_legend,
         legend = Text_legend,text.width = strwidth("100000"),
         bty = "n")           
}

######Execution Part
library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)

setwd("d:/MyR/jijin")

##Specify the year and month range to draw plots
##Usually only numeric_Specied_Month need to be changed

numeric_Specied_Year <- 2016

##The following only affects all curve figures
numeric_Specied_Month <- 2:5  ## change here every time!
numeric_Specied_ylim_upper <- 0.055 ## It may be needed to change here !

##The following only affects the last curve figure
numeric_Specied_xlim_down <- -50 ## It may be needed to change here !
numeric_Specied_xlim_upper <- 30 ## It may be needed to change here !

##plan how to place plots according to the input month vector
func_layout_plot(numeric_Specied_Month) 

##read csv files to get data. The input months length can be larger than 3
ls_value <- InputData(numeric_Specied_Year,numeric_Specied_Month)

## Draw monthly return curve for the specified month
## the browser() in density_mean_sd function should be enabled if text is overlapped
## debug模式启动后，观察DF_processed数据框，再考虑如何设置距离门限以调整文字间距

func_draw_all_month_curve(arg_ls_value = ls_value,
                          numeric_Specied_Month,
                          Varyear = numeric_Specied_Year,
                          numeric_input_ylim_upper = 
                                  numeric_Specied_ylim_upper)
                                      
#大盘指数: Manual action should not be needed.
DrawBoardIndex("dapanzhishu2016.csv",
               c("上证综指","创业板","港股通精选100指数"), 
               "大盘涨幅(从2016年初开始)", 16, -25,"khaki4")#"turquoise3"


#月收益率曲线移动轨迹图, The input months length can be larger than 3
##If there is only one month as input, this part need not to be executed.
length_Specied_Month <- length(numeric_Specied_Month)
if(length_Specied_Month > 1){
        DrawMonthValueMovingCurve(ls_value, numeric_Specied_Year, 
                                  numeric_Specied_Month,
                                  numeric_input_ylim_upper = 
                                          numeric_Specied_ylim_upper,
                                  numeric_Specied_xlim_down,
                                  numeric_Specied_xlim_upper)        
}


#赵持仓板块指数: Manual action should not be needed.
if(file.exists("zhaobankuaizhishu2016.csv")){
        #如果记录赵基金持仓板块涨幅的文件存在，则要多描绘一个图，否则省略
        DrawBoardIndex("zhaobankuaizhishu2016.csv",
               c("高端装备指数000097","医药指数399913","消费指数399912"), 
               "赵基金持仓板块涨幅(从2016年初开始)", 16, -25,"black")
}