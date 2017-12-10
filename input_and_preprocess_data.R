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
                csv.month.name <- paste("CSVData-",arg.year + 1,"-1-5",
                                        sep = "")
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

InputData <- function(arg.fundtype, arg.year = 2016, arg.month = 2) {
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
                        csv.file.name <- paste(arg.fundtype,
                                               arg.year + 1,
                                               "-1-5.csv", 
                                               sep = "")
                }else{
                        csv.file.name <- paste(arg.fundtype,
                                               arg.year,"-",i,"-5",".csv", 
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
                # max.value <- max(as.numeric(fund.data[,2]),max.value)
                
                # min.value <- quantile(c(as.numeric(fund.data[,2]),min.value), probs = 0.005)
                max.value <- quantile(c(as.numeric(fund.data[,2]),max.value), probs = 0.995)
                
        }
        
        ls.value.data[["min_data"]] <- min.value
        ls.value.data[["max_data"]] <- max.value
        
        ls.value.data[["month_range"]] <- arg.month
        return(ls.value.data)
}

GetIndividualFundReturn <- function(arg.year = 2016, arg.month = 2,
                                    arg.fund.name, arg.ls.value, arg.quantile) {
        
        # 2.read csv files to get data
        #
        # Args:
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回ls_value列表中的保存指定时间的csv数据的数据框元素的名称
        
        
        # browser()
        result <- NULL
        for (i in arg.month){
                
                csv.month.name <- GetCSVMonthName(arg.year, 
                                                  arg.month = i)
                
                csv.data <- arg.ls.value[[csv.month.name]]
                
                if(arg.quantile == 0){
                        #browser()
                        # result[i - 1] <- csv.data[csv.data[,1] == arg.fund.name,2]
                        result <- c(result, csv.data[csv.data[,1] == arg.fund.name,2])
                }else{
                        # browser()
                        # result[i - 1] <- quantile(csv.data[,2], arg.quantile)
                        result <- c(result, quantile(csv.data[,2], arg.quantile))
                }
                
                
        }
        
        return(result)
        
}
