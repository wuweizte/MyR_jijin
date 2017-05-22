library(ggplot2)


## first set arg.dist.lowthreshold = 0 in DrawAllMonthCurve() to see the original situation

df1 <- ls_value$`DF_processed.CSVData-2017-3-5`[2:5, c("dist.x", "dist.y", "modified.flag")]
df1$modified.flag[1:2] <- TRUE

df2 <- ls_value$`DF_processed.CSVData-2017-4-5`[2:5, c("dist.x", "dist.y", "modified.flag")]
df2$modified.flag[3:4] <- TRUE


df3 <- ls_value$`DF_processed.CSVData-2017-5-5`[2:5, c("dist.x", "dist.y", "modified.flag")]
df3$modified.flag[c(1,4)] <- TRUE

df <- rbind(df1, df2, df3)

ggplot(data = df, aes(x = dist.x, y = dist.y)) + geom_point(aes(color = modified.flag))

