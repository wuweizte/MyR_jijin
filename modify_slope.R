library(ggplot2)

df1 <- ls_value$`DF_processed.CSVData-2017-2-5`[2:4, c("dist.x", "dist.y", "modified.flag")]
df1$modified.flag <- FALSE
df1$modified.flag[1:2] <- TRUE

df2 <- ls_value$`DF_processed.CSVData-2017-3-5`[2:4, c("dist.x", "dist.y", "modified.flag")]
df2$modified.flag <- FALSE
df2$modified.flag[1:2] <- TRUE


df3 <- ls_value$`DF_processed.CSVData-2017-4-5`[2:4, c("dist.x", "dist.y", "modified.flag")]

df <- rbind(df1, df2, df3)

ggplot(data = df, aes(x = dist.x, y = dist.y)) + geom_point(aes(color = modified.flag))

### 如果直线可以区别该修改y轴坐标的点与不用修改的点�? 那在主程序中
### 调用DrawAllMonthCurve函数时调整arg.dist.lowthreshold 即可
