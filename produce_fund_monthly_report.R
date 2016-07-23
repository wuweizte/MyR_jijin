#### Author Comment Part
# modified on 2016-7-1

#### File Descriptiong Part
# 代码目的：用于比较私募排排网提供的基金收益率信息

#### Library Quoting Part
library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)


#### Function Definition Part
DesignPlotLayout <- function(arg.month){
        
        # 1.plan how to place plots according to the input month vector
        #
        # Args:
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   没有输出，直接就是执行layout函数影响后续绘图
        
        i <- length(arg.month)
        
        if (i == 1){
                plot.layout.matrix <- matrix(c(1, 1, 1, 2, 2), nr = 1, 
                                             byrow = TRUE) 
        } else if (i == 2) {
                plot.layout.matrix <- matrix(c(1, 1, 1, 4, 4, 2, 2, 2, 3, 3), 
                                             nr = 2, byrow = TRUE) 
        } else  {
                if (file.exists("zhaobankuaizhishu2016.csv")){
                        #如果记录赵基金持仓板块涨幅的文件存在，则要多描绘一个图
                        plot.layout.matrix <- matrix(c(1, 1, 5, 5, 2, 2, 4, 4,
                                                       3, 3, 6, 6), nr = 3,
                                                     byrow = TRUE)
                } else {
                        plot.layout.matrix <- matrix(c(1, 1, 5, 5, 2, 2, 4, 4,
                                                       3, 3, 4, 4), nr = 3, 
                                                     byrow = TRUE)                
                }
                
        }
        layout(plot.layout.matrix)
}

GetCSVMonthName <- function(arg.year = 2016, arg.month = 2) {
        
        # 2.read csv files to get data
        #
        # Args:
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回ls_value列表中的保存指定时间的csv数据的数据框元素的名称
        
        if (arg.month == 13){
                csv.month.name <- paste("CSVData-",arg.year + 1,"-1-5")
        }else{
                csv.month.name <- paste("CSVData-",arg.year,"-",arg.month,"-5",
                                       sep = "")
        }
        return(csv.month.name)
}



DeleteDuplicateFundName <- function(arg.fund.name.vector) {
        
        # 删除存在重名的基金
        #
        # Args:
        #   arg.fund.name.vector: 原始的基金名称的向量组
        #
        # Returns:
        #   删除重名后的基金名称的向量组
        

        fund.name.table <- table(arg.fund.name.vector)
        duplicate.name.vector <- names(fund.name.table[fund.name.table > 1])
        #browser()
        selected.flag <- !(arg.fund.name.vector %in% duplicate.name.vector)
        #browser()
        result.fund.name.vector <- arg.fund.name.vector[selected.flag]
        #browser()
        return(result.fund.name.vector)
}

GetFundNameVector <- function(arg.ls.value, arg.year = 2016, arg.month = 2) {
        
        # 对月份收益数据取交集，确保分析的总体数据在时间上的一致
        #
        # Args:
        #   arg.ls.value: 存放基金月度收益数据的列表
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回最终用于分析的基金名称列表


        csv.month.name <- GetCSVMonthName(arg.year, arg.month[1])
        fund.Name.vector <- arg.ls.value[[csv.month.name]][,1]
        #browser() 
        fund.Name.vector <- DeleteDuplicateFundName(fund.Name.vector)
        #browser()
        
        if(length(arg.month) > 1){

                for(i in arg.month[-1]){
                        csv.month.name <- GetCSVMonthName(arg.year, i)
                        
                        Second.fund.Name.vector <- arg.ls.value[[csv.month.name]][,1]
                        Second.fund.Name.vector <- DeleteDuplicateFundName(Second.fund.Name.vector)
                        
                        fund.Name.vector <- intersect(Second.fund.Name.vector, fund.Name.vector)
                }
        }
        return(fund.Name.vector)
        
}        

InputData <- function(arg.year = 2016, arg.month = 2) {
        # 读取数据文件，选取满足要求的记录存放到全局列表中
        #
        # Args:
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回已经填好数据的列表
        
        ls.value.data <- list()
        for(i in arg.month){
                if(i == 13){
                        csv.file.name <- paste("simujijin",arg.year + 1,"-1-5.csv")
                }else{
                        csv.file.name <- paste("simujijin",arg.year,"-",i,"-5",".csv", 
                                         sep = "")
                }
                
                csv.file.content <- read.csv(csv.file.name, stringsAsFactors = FALSE)
                csv.file.content <- csv.file.content[csv.file.content[,2] != "#NA" & 
                                           csv.file.content[,2] != "#VALUE!",1:2]
                
                month.index <- GetCSVMonthName(arg.year, i)
                ls.value.data[[month.index]] <- csv.file.content
        }


        accepted.fund.name <- GetFundNameVector(ls.value.data, arg.year, arg.month)
        
        min.value <- Inf
        max.value <- -Inf
        for(i in arg.month){

                month.index <- GetCSVMonthName(arg.year, i)
                
                fund.data <- ls.value.data[[month.index]]
                fund.data <- fund.data[fund.data[,1] %in% accepted.fund.name,]
                ls.value.data[[month.index]] <- fund.data

                min.value <- min(as.numeric(fund.data[,2]),min.value)
                max.value <- max(as.numeric(fund.data[,2]),max.value)
        }

        ls.value.data[["min_data"]] <- min.value
        ls.value.data[["max_data"]] <- max.value
        
        ls.value.data[["month_range"]] <- arg.month
        return(ls.value.data)
}



## 
DrawAllMonthCurve <- function(arg.ls.value, arg.year, arg.month,
                                      arg.ylim.upper = 0.04,
                              arg.x.slope, arg.y.slope, arg.dist.lowthreshold){

        # 3. Draw monthly return curve for the specified month
        # 3-1 Execution Control Funciton
        # 
        # Args:
        #   arg.ls.value: 已经填好数据的列表
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #   arg.ylim.upper: 图中Y轴刻度上限
        # 
        # Returns:
        #   没有返回，结尾是调用子函数
        
        ##let user to determine whether rug lines should be drawn.
        cat("\n\nIf you want to draw rug lines in x coordinate axis,please input yes.")
        cat("\nNow wait for your input: ")
        
        draw.rug.selection <- readline("?")

        rug.flag = 0
        if (substr(draw.rug.selection, 1, 3) == "yes"){
                rug.flag = 1
        }
        
        ##Draw monthly return curves for the recent 3 months
        op <- par(bg = "lightgrey")
        month.to.draw <- arg.month
        
        length.specied.month <- length(arg.month)
        if(length.specied.month > 3){
                month.to.draw <- arg.month[
                        (length.specied.month - 2):length.specied.month]
        }

        ls.value.input <- arg.ls.value
        #browser()
        for(i in month.to.draw) {
                ls.value.input <- DrawMonthValueCurve( 
                                         arg.ylim.upper = arg.ylim.upper,
                                         ls_value_input = ls.value.input,
                                         arg.year = numeric_Specied_Year,
                                         arg.month = i,
                                         Var_rug_flag = rug.flag,
                                         arg.x.slope, arg.y.slope,
                                         arg.dist.lowthreshold)
        }
        # ls.value.input <- lapply(month.to.draw, DrawMonthValueCurve, 
        #                          arg.ylim.upper = arg.ylim.upper,
        #                          ls_value_input = arg.ls.value,
        #                          arg.year = numeric_Specied_Year, 
        #                          Var_rug_flag = rug.flag)
        #browser()
        return(ls.value.input)
}


## 3-2 Prepare to draw monthly return curve
#     Var_rug_flag : flag variabel used to determine whether rug lines should
#                    be drawn.

DrawMonthValueCurve <- function(arg.ylim.upper, ls_value_input, 
                                arg.year = 2016, arg.month = 2,
                                Var_rug_flag = 0,
                                arg.x.slope, arg.y.slope, arg.dist.lowthreshold){
                                
        #browser()
        if(arg.month == 13){
                PlotMainName <- paste('股票策略型私募基金收益分布密度曲线(截止到',
                                      arg.year+1,'-1-5)',sep = "")
        }else{
                PlotMainName <- paste('股票策略型私募基金收益分布密度曲线(截止到',
                                      arg.year,'-',arg.month ,'-5)',sep = "")
        }

        CSVmonth_name <- GetCSVMonthName(arg.year, arg.month)
        CSVData <- ls_value_input[[CSVmonth_name]]
        month_return <- CSVData[,2]
        
        min_for_months <- ls_value_input[["min_data"]]
        max_for_months <- ls_value_input[["max_data"]]
        
        plot(month_return, type = "n", ylim = c(0, arg.ylim.upper),
             xlim = c(min_for_months + 10, max_for_months - 10), 
             axes = FALSE,main = PlotMainName,
             xlab = '年收益率(%)',ylab = '分布密度')
        
        x_break_number <- seq(from = round(min_for_months,digits = -1) - 10,
                              to = round(max_for_months,digits = -1) + 10, 
                              by = 10)
        
        axis(1, at = x_break_number, labels = x_break_number)
        axis(2, las = 1)
        
        #draw grid lines
        
        index_y_grid <- seq(from = 0, to = arg.ylim.upper, by = 0.01)
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
        
        SelectedColor <- col_for_lines[col_seq[as.character(arg.month)]]
        
        # Profits for specified funds
        zhao_value <- as.numeric(CSVData[CSVData[,1] == "赤子之心价值",2])
        zhanbo_value <- as.numeric(CSVData[CSVData[,1] == "展博1期",2])
        yunfeng_value <- 
                as.numeric(CSVData[CSVData[,1] == "华润信托昀沣4号集合资金信托计划",2])
                

        # X-Y axis scale  : the length of Y axis is 1/4.5  of that of X axis
        
        XYscale <- ((max_for_months - min_for_months) / 4.5 ) / arg.ylim.upper
        
        ## Call curve drawing funtion
        
        #browser()
        DF_processed <- density_mean_sd(x = month_return, lwd = 1,  
                                        lcol = SelectedColor, 
                        zhao_value,zhanbo_value,yunfeng_value, 
                        XYscale,
                        arg.x.slope, arg.y.slope, arg.dist.lowthreshold)
        
        
        ls_value_input[[paste0("DF_processed.", CSVmonth_name)]] <- DF_processed
        #browser()
        return(ls_value_input)
}

##  3-3 Draw the monthly return density curve

density_mean_sd <- function(x, lwd, lcol, zhao, zhanbo,yunfeng, arg_XYscale,
                            arg.x.slope, arg.y.slope, arg.dist.lowthreshold){
        
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
        
        #browser()
        DF_processed <- func_set_text_adjacent_distance(DF_processed, arg_XYscale,
                                                        arg.x.slope, arg.y.slope)
       
        DF_processed <- func_set_text_distance_to_median(DF_processed, arg_XYscale)
               
        DF_processed <- func_modify_text_position(DF_processed, arg.dist.lowthreshold)
        #browser()
        
        ## Line drawing and text labelling        
        func_draw_line_text(DF_processed)

        ## The following browser command is used to decide the threshold set in 
        ## func_modify_text_position function through going to debug mode
        ##  to check the dist column of DF_processed
        
        #browser()
        return(DF_processed)
}

## 3-4 Input data frame with figure information about line and text
func_process_line_text <- function(arg_DF_lineText){
        DF_result <- as.data.frame(arg_DF_lineText)
        
        DF_result$drawy <- DF_result$liney
        
        DF_result$col = "brown"
        DF_result[DF_result$linename == "zhao","col"] <- "black"
        DF_result[DF_result$linename == "zhanbo","col"] <- "darkcyan"
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
                paste('赤子之心价值基金收益率 = ',format(linex_zhao, digits = 4), '%', 
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
func_set_text_adjacent_distance <- function(arg_DF_processed_first, arg_XYscale,
                                            arg.x.slope, arg.y.slope){
       
        tbl_df_text_position <- tbl_df(arg_DF_processed_first)
        tbl_df_text_position <- arrange(tbl_df_text_position, linex)
       
        linex_median <- unlist(tbl_df_text_position %>% filter(linename == "median")
                               %>% select(linex))
        
        ##当出现基金收益与median相同时，需要保证median记录的位置，所以在此median记录是单独写入的
        tbl_df_filter_left <- filter(tbl_df_text_position, linex < linex_median)
        tbl_df_filter_left <- rbind(tbl_df_filter_left, 
                                    filter(tbl_df_text_position, 
                                           linename == "median"))
        
       
        tbl_df_filter_right <- filter(tbl_df_text_position, linex >= linex_median,
                                      linename != "median")
        tbl_df_filter_right <- rbind(filter(tbl_df_text_position,linename == "median"), 
                                     tbl_df_filter_right)
        
        df_filter_left <- as.data.frame(tbl_df_filter_left %>% select(linex, liney))
        #browser()
        if(nrow(tbl_df_filter_left) > 1){
                
                df_filter_left_combine <- cbind(df_filter_left[-1,], 
                                                df_filter_left[-nrow(df_filter_left),])
                colnames(df_filter_left_combine) <- c("linex1", "liney1", 
                                                      "linex2", "liney2")
                
                ## the distance between 2 adjacent points is calculated through the 
                ## sum of subtract results of X coordinates and Y coordinates,
                ## but that of Y coordinates is needed to multiply with one scale
                ## because the scales of X axis and Y axis are different

                df_filter_left_combine <- transform(df_filter_left_combine,
                                                    distance = arg.x.slope * abs(linex1- linex2) + 
                                                            arg.y.slope * abs(liney1- liney2) * arg_XYscale,
                                                    distance.x = abs(linex1- linex2),
                                                    distance.y = abs(liney1- liney2) * arg_XYscale
                )
                
                df_filter_left$distance <- c(Inf, df_filter_left_combine$distance)
                df_filter_left$distance.x <- c(Inf, df_filter_left_combine$distance.x)
                df_filter_left$distance.y <- c(Inf, df_filter_left_combine$distance.y)
        }else{
                df_filter_left$distance <- Inf
                df_filter_left$distance.x <- Inf
                df_filter_left$distance.y <- Inf
        }
        #browser()
        
        df_filter_right <- as.data.frame(tbl_df_filter_right %>% select(linex, liney))       
        if(nrow(tbl_df_filter_right) > 1){
                
                df_filter_right_combine <- cbind(df_filter_right[-nrow(df_filter_right),], 
                                                 df_filter_right[-1,])
                colnames(df_filter_right_combine) <- c("linex1", "liney1", 
                                                       "linex2", "liney2")

                df_filter_right_combine <- transform(df_filter_right_combine,
                                                    distance = arg.x.slope * abs(linex1- linex2) + 
                                                            arg.y.slope * abs(liney1- liney2) * arg_XYscale,
                                                    distance.x = abs(linex1- linex2),
                                                    distance.y = abs(liney1- liney2) * arg_XYscale
                )
                
                
                df_filter_right$distance <- c(df_filter_right_combine$distance, Inf)
                df_filter_right$distance.x <- c(df_filter_right_combine$distance.x, Inf)
                df_filter_right$distance.y <- c(df_filter_right_combine$distance.y, Inf)
        }else{
                df_filter_right$distance <- Inf
                df_filter_right$distance.x <- Inf
                df_filter_right$distance.y <- Inf
        }
        
        median_dist <- min(df_filter_left$distance[nrow(df_filter_left)],
                           df_filter_right$distance[1])
        
        if (df_filter_left$distance[nrow(df_filter_left)] <= df_filter_right$distance[1]){
                median_dist.x <- df_filter_left$distance.x[nrow(df_filter_left)]
                median_dist.y <- df_filter_left$distance.y[nrow(df_filter_left)]
        } else {
                median_dist.x <- df_filter_right$distance.x[1]
                median_dist.y <- df_filter_right$distance.y[1]
        }
        
        #browser()
        tbl_df_text_position$dist <- c(df_filter_left$distance[-nrow(df_filter_left)],
                                       median_dist,
                                       df_filter_right$distance[-1]
        )
        
        tbl_df_text_position$dist.x <- c(df_filter_left$distance.x[-nrow(df_filter_left)],
                                       median_dist.x,
                                       df_filter_right$distance.x[-1]
        )

        tbl_df_text_position$dist.y <- c(df_filter_left$distance.y[-nrow(df_filter_left)],
                                         median_dist.y,
                                         df_filter_right$distance.y[-1]
        )
        
        return(tbl_df_text_position)
}

func_set_text_distance_to_median <- function(arg_DF_processed, arg_XYscale){
        tbl_df_text_position <- arg_DF_processed
        median_x <- filter(tbl_df_text_position,linename == "median")   %>% select(linex)
        median_x <- unlist(median_x)
        
        median_y <- filter(tbl_df_text_position,linename == "median")   %>% select(liney)
        median_y <- unlist(median_y)

        tbl_df_text_position <- transform(tbl_df_text_position,
                                          distance_to_median = abs(median_x- linex) + 
                                                  abs(median_y- liney) * arg_XYscale
        )
        
        #tbl_df_text_position$distance_to_median[tbl_df_text_position$linename == "median"] <- Inf
        
        tbl_df_text_position <- within(tbl_df_text_position,
                             distance_to_median[linename == "median"] <- Inf     
        )
        
        #browser()
        return(tbl_df_text_position)
}

func_modify_text_position <- function(arg_DF_processed, arg.dist.lowthreshold){

        tbl_df_text_position <- arg_DF_processed
        tbl_df_text_position$modified.flag <- FALSE
         
        rownum_median <- which(tbl_df_text_position$linename == "median")

        ## This threshold can be observed through 'browser()' 
        ## in  density_mean_sd function
        #dist_lowthreshold <- 1
        
        dist_to_median_threshold <- 0.24
        
        LargeStep <- 0.005
        MiddleStep <- LargeStep * 3/4
        SmallStep <- LargeStep/4
        #browser()
        for (i in 2:(nrow(tbl_df_text_position) - 1)) {
                if(tbl_df_text_position$dist[i] < arg.dist.lowthreshold){
                        
                        tbl_df_text_position$modified.flag[i] <- TRUE 
                                
                        tbl_df_text_position$drawy[i] <- 
                                tbl_df_text_position$drawy[i] + LargeStep
                        
                        if(i == rownum_median){
                                next
                        }
                        
                        tbl_df_text_position <- within(tbl_df_text_position,{
                                drawy[rownum_median] <- 
                                        drawy[rownum_median] + SmallStep                              
                                
                                if(i < (rownum_median - 1)){
                                        drawy[(i + 1):(rownum_median - 1)] <-
                                                drawy[(i + 1):(rownum_median - 1)] + MiddleStep

                                }else if(i > (rownum_median + 1)){
                                        drawy[(i - 1):(rownum_median + 1)] <-
                                                drawy[(i - 1):(rownum_median + 1)] + MiddleStep
                                }        
                        }
                        )
                } 
        }
   
        ##对于离中值很近的点，可能标题会覆盖曲线，所以需要提高位置
        
        tbl_df_text_position <- within(tbl_df_text_position, 
                                       drawy[distance_to_median < dist_to_median_threshold] <- 
                                               drawy[distance_to_median < dist_to_median_threshold] + LargeStep
        )
        
        #browser()
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
                        text(linex - 7,texty,labels = rowlabel,pos = rowpos, 
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
  #browser()
  levels(B$date)
  dimnames(Board_Index_array) <- list(unique(B$date), boardindexname)
                                      
  
  bar_x <- barplot(Board_Index_array, beside = TRUE,ylim = c(-35,10), 
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
  text(location_bar_x, bar_y + sign(bar_y) * 2, paste(bar_y,"%",sep = ""),
       col = "red", font = 3,cex = 0.9)
}

## 5.月收益率曲线移动轨迹图

DrawMonthValueMovingCurve <- function(ls_value_input, arg.year = 2016, 
                                      arg.month = 2,
                                      numeric_input_ylim_upper = 0.04,
                                      numeric_Specied_xlim_down = -50,
                                      numeric_Specied_xlim_upper = 20){
  
  lid <- length(arg.month)
  
  if(max(arg.month) == 13){
           Text_legend <- c(paste("截止到",arg.year,"-",arg.month[1:(lid-1)],"-5",
                                 sep = ""),
                           paste("截止到",arg.year + 1,"-1-5", sep = ""))
    
  }else{
           Text_legend <- paste("截止到",arg.year,"-",arg.month,"-5",sep = "")
  }

  CSVmonth_name <- GetCSVMonthName(arg.year, arg.month[lid])
  
  CSVData <- ls_value_input[[CSVmonth_name]]
  month_return <- CSVData[,2]

  plot(month_return, type = "n", ylim = c(0, numeric_input_ylim_upper),
       xlim = c(numeric_Specied_xlim_down, numeric_Specied_xlim_upper), 
       axes = FALSE,main = '股票策略型私募基金收益分布密度曲线移动轨迹图',
       xlab = '年收益率(%)',ylab = '分布密度')
  
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
            CSVmonth_name <- GetCSVMonthName(arg.year, arg.month[i])
            CSVData <- ls_value_input[[CSVmonth_name]]
            month_return <- CSVData[,2]
            
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

setwd("d:/MyR/jijin")

##Specify the year and month range to draw plots
##Usually only numeric_Specied_Month need to be changed

numeric_Specied_Year <- 2016
numeric_Specied_Month <- 2:7  ## change here every time!


##The following only affects all curve figures
numeric_Specied_ylim_upper <- 0.065 ## It may be needed to change here !

##The following only affects the last curve figure
numeric_Specied_xlim_down <- -50 ## It may be needed to change here !
numeric_Specied_xlim_upper <- 20 ## It may be needed to change here !

##plan how to place plots according to the input month vector
DesignPlotLayout(numeric_Specied_Month) 

##read csv files to get data. The input months length can be larger than 3
ls_value <- InputData(numeric_Specied_Year,numeric_Specied_Month)


## Draw monthly return curve for the specified month

## 观察哪些点存在重叠情况，然后使用ls_value携带的针对每个月的DF_processed里由(dist.x, dist.y)
## 确定的点的信息出图，观察arg.x.slope * dist.x + arg.y.slope * dist.y = arg.dist.lowthreshold
## 这条直线所属的系数如何设置才能将需要提升位置与不需要提升位置的点区分开

## 这里还需要人工确定标签是否重叠，事实上应该可以判断标签对应的外围矩形框是否重叠而由电脑自动
## 完成判断及调整工作---留待后续修改算法

ls_value <- DrawAllMonthCurve(arg.ls.value = ls_value, arg.year = numeric_Specied_Year,
                          numeric_Specied_Month,
                          arg.ylim.upper = 
                                  numeric_Specied_ylim_upper,
                          arg.x.slope = 0, arg.y.slope = 1, 
                          arg.dist.lowthreshold = 1)

#browser()
#大盘指数: Manual action to the coordinates of legend may be needed.
DrawBoardIndex("dapanzhishu2016.csv",
               c("上证综指","创业板","港股通精选100指数"), 
               "大盘涨幅(从2016年初开始)", 21, -21,"steelblue")


#月收益率曲线移动轨迹图, The input months length can be larger than 3
##If there is only one month as input, this part need not to be executed.

numeric_Specied_Month_for_Moving_Curve <- numeric_Specied_Month
#numeric_Specied_Month_for_Moving_Curve <- c(2,4,6)

length_Specied_Month <- length(numeric_Specied_Month_for_Moving_Curve)
if(length_Specied_Month > 1){
        DrawMonthValueMovingCurve(ls_value, numeric_Specied_Year, 
                                  numeric_Specied_Month_for_Moving_Curve,
                                  numeric_input_ylim_upper = 
                                          numeric_Specied_ylim_upper,
                                  numeric_Specied_xlim_down,
                                  numeric_Specied_xlim_upper)        
}


#赵持仓板块指数: Manual action to the coordinates of legend may be needed.

if(length(numeric_Specied_Month) > 2){
        if(file.exists("zhaobankuaizhishu2016.csv")){
                #如果记录赵基金持仓板块涨幅的文件存在，则要多描绘一个图，否则省略
                DrawBoardIndex("zhaobankuaizhishu2016.csv",
                               c("高端装备指数000097","医药指数399913","消费指数399912"), 
                               "机构推荐板块涨幅(从2016年初开始)", 21, -20,"springgreen4")
        }        
}
