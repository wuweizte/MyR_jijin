

library(jsonlite)

url.upper.part <- "http://dc.simuwang.com/Ranking/get.html?page="
url.down.part <- "&condition=fund_type%3A1%2C6%2C4%2C3%2C8%2C2%3Bret%3A10%3Brating_year%3A1%3Bstrategy%3A1%3Bistiered%3A0%3Bcompany_type%3A1%3Bsort_name%3Aprofit_col2%3Bsort_asc%3Adesc%3Bkeyword%3A"


for(i in 1:94){
        url <- paste0(url.upper.part, i ,url.down.part)
        #browser()
        json.data <- fromJSON(url)
        
        df <- json.data$data
        # browser()
        if(i == 1){
                result <- df[,c("fund_short_name", "ret_ytd", "company_short_name")]
        } else if(i > 1){
                result <- rbind(result, df[,c("fund_short_name", "ret_ytd", "company_short_name")])

        }

}

#####网页显示的基金名称和JSON数据不一致时，清理掉名称头部
processed.name <- strsplit(result$fund_short_name, "-")
regularized.name <- lapply(processed.name, function(x) {x[length(x)]})

result$fund_short_name <- unlist(regularized.name)

View(result)

setwd("D:\\MyR\\jijin")

write.csv(result, "simu201705.csv")






















