# rm(list = ls())
# 
# layout(matrix(c(1)))

DrawUCCountryBond <- function(filename, arg.month.number){
        
        
        bond.data <- read.csv(file = filename, header = TRUE)
        bond.data <- tail(bond.data, arg.month.number)
        
        # row.number <- nrow(B)
        
        ###取最近几个月数据，最多 monthnumber 月

        # browser()

        us.bond <- bond.data[,2]

        ch.bond <- bond.data[,3]
        
        bond.diff <- bond.data[,4]
        
        ylim.min <- min(bond.diff) - 1
        ylim.max <- max(us.bond,ch.bond) + 1
        
        plot(ch.bond,
             col="blue",
             type = "o",
             ylim = c(ylim.min,ylim.max),
             axes = FALSE,
             xlab = "",
             ylab = "",
             main = "中美十年期国债利率(%)",
             cex.axis = 1.4)
        
        text(1:arg.month.number,
             ch.bond + 0.35,
             round(ch.bond,digits = 2),
             col = "blue",cex = 1.2)
        
        lines(us.bond,col="red",type = "o")
        text(1:arg.month.number,
             us.bond + 0.35,
             us.bond,
             col = "red",cex = 1.2)
        
        
        lines(bond.diff,col="purple",type = "o", lty = 2)
        text(1:arg.month.number,
             bond.diff + 0.35,
             bond.diff,
             col = "purple",cex = 1.2)
        
        legend("bottom",
               legend = c("CHINA","USA",  "Difference"),
               col = c("blue",'red', "purple"),
               lty = 1,
               ncol = 3,
               bty = "n"
               # lwd = c(1,2,3),
               # inset = .02,
               # text.width = strwidth("1"),
               # adj = c(1,0.5)
               )
        
        axis(1, at = 1:arg.month.number, labels = bond.data[,1])
        
        axis(2,las = 1)
        box()
}


# DrawUCCountryBond("D:\\MyR\\jijin\\bondUSACHINA10year.csv",
#                   arg.month.number = 4)