stock <- "PETR4.SA"
start.date <- "2010-01-01"
end.date <- Sys.Date()
quote <- paste("http://ichart.finance.yahoo.com/table.csv?s=",
               stock,
               "&a=", substr(start.date,6,7),
               "&b=", substr(start.date, 9, 10),
               "&c=", substr(start.date, 1,4), 
               "&d=", substr(end.date,6,7),
               "&e=", substr(end.date, 9, 10),
               "&f=", substr(end.date, 1,4),
               "&g=d&ignore=.csv", sep="")  
quote
stock.data <- read.csv(quote, as.is=TRUE)
stock.data$Date <- as.Date(stock.data$Date)
## Uncomment the next 3 lines to install the developer version of googleVis
# install.packages(c("devtools","RJSONIO", "knitr", "shiny", "httpuv"))
# library(devtools)
# install_github("mages/googleVis")
library(googleVis)
plot( 
        gvisCalendar(data=stock.data, datevar="Date", numvar="Adj.Close",
                     options=list(
                             title="Calendar heat map of MSFT adjsuted close",
                             calendar="{cellSize:10,
                                 yearLabel:{fontSize:20, color:'#444444'},
                                 focusedCellColor:{stroke:'red'}}",
                             width=590, height=320),
                     chartid="Calendar")
)


###############

library(lattice)
library(chron)
source("calendarHeat.R")
# Plot as calendar heatmap
calendarHeat(stock.data$Date, stock.data$Adj.Close, varname="PETR4.SA")

library(ggplot2)

qplot(stock.data$Adj.Close, data=stock.data, geom="histogram")



qplot(stock.data$Date, stock.data$Adj.Close, data=stock.data ) +
        geom_smooth( data=stock.data, stat="identity")


library(manipulate)


manipulate({
     
        dataFilter = stock.data[1:minData,]
        ggplot(dataFilter, aes(x=Date, y=Adj.Close ) )+ 
                #geom_bar(stat="identity")
                geom_line()
                         
        } , minData = slider(2,dim(stock.data)[1],2) 
        
)

 
