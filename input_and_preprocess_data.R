GetCSVMonthName <- function(arg.year = 2016, arg.month = 2) {
        
        # 2.read csv files to get data
        #
        # Args:
        #   arg.year: �滭��Ե���ݣ��Ե�������ʽ����
        #   arg.month: �滭��Ե��·ݷ�Χ����������ʽ����
        #
        # Returns:
        #   ����ls_value�б��еı���ָ��ʱ���csv���ݵ����ݿ�Ԫ�ص�����
        
        if (arg.month == 13){
                csv.month.name <- paste("CSVData-",arg.year + 1,"-1-5")
        }else{
                csv.month.name <- paste("CSVData-",arg.year,"-",arg.month,"-5",
                                        sep = "")
        }
        return(csv.month.name)
}



DeleteDuplicateFundName <- function(arg.fund.name.vector) {
        
        # ɾ�����������Ļ���
        #
        # Args:
        #   arg.fund.name.vector: ԭʼ�Ļ������Ƶ�������
        #
        # Returns:
        #   ɾ��������Ļ������Ƶ�������
        
        
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
        
        # ���·���������ȡ������ȷ������������������ʱ���ϵ�һ��
        #
        # Args:
        #   arg.ls.value: ��Ż����¶��������ݵ��б�
        #   arg.year: �滭��Ե���ݣ��Ե�������ʽ����
        #   arg.month: �滭��Ե��·ݷ�Χ����������ʽ����
        #
        # Returns:
        #   �����������ڷ����Ļ��������б�
        
        
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
        # ��ȡ�����ļ���ѡȡ����Ҫ��ļ�¼��ŵ�ȫ���б���
        #
        # Args:
        #   arg.year: �滭��Ե���ݣ��Ե�������ʽ����
        #   arg.month: �滭��Ե��·ݷ�Χ����������ʽ����
        #
        # Returns:
        #   �����Ѿ�������ݵ��б�
        
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