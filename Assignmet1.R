library("ggplot2")
data <- read.csv("C:/Users/Nan/Desktop/Programming Assignment 1 Data New/ExcelFormattedGISTEMPDataCSV.csv", 
                 head = TRUE,na.strings = "****")

Data <- data[which(data$Year>=2010 & data$Year<2015),1:13]
row.names(Data) <- c()


year <- rep(Data$Year, rep(12,5))
month <- rep(names(Data)[-1],5)


Data[,1] <- c()
Data_ <- data.frame(Year = year, Month = month, data = as.vector(t(Data)) )

p1 <- ggplot(data=Data_[seq(1,180,2),], aes(x=Year, y=data, fill=Month)) +
  geom_bar(stat="identity", position=position_dodge()) + ggtitle("Visualization of recent 5 years data of Odd-numbered Months")
p2 <- ggplot(data=Data_[seq(2,180,2),], aes(x=Year, y=data, fill=Month)) +
  geom_bar(stat="identity", position=position_dodge()) + ggtitle("Visualization of recent 5 years data of Even-numbered Months")

p3 <- ggplot(data=Data_[seq(1,60,2),], aes(x=Year, y=data, fill=Month)) +
  geom_histogram(colour="black", stat = "identity",binwidth=10) +
  facet_grid(Month ~ .) +
  ggtitle("Visualization of recent 5 years histograph of Odd-numbered Months respectively") +
  theme(legend.position="none")  

p4 <- ggplot(data=Data_[seq(2,60,2),], aes(x=Year, y=data, fill=Month)) +
  geom_histogram(colour="black", stat = "identity",binwidth=10) +
  facet_grid(Month ~ .) +
  ggtitle("Visualization of recent 5 years histograph of Even-numbered Months respectively") +
  theme(legend.position="none")  

multiplot(p1, p2, p3, p4, cols=2)



