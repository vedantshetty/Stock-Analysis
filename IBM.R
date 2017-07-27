#inputing the datasets
BM <- read.csv("Downloads/Data/Stocks/IBMStock.csv")
GE <- read.csv("Downloads/Data/Stocks/GEStock.csv")
ProcterGamble <- read.csv("Downloads/Data/Stocks/ProcterGambleStock.csv")
CocaCola <- read.csv("Downloads/Data/Stocks/CocaColaStock.csv")
Boeing <- read.csv("Downloads/Data/Stocks/BoeingStock.csv")

#Converting the dates to POSIXCT
IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date <- as.Date(GE$Date,"%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date,"%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date,"%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date,"%m/%d/%y")

#Analysis starts from 1970
min(IBM$Date,GE$Date,CocaCola$Date,ProcterGamble$Date,Boeing$Date)
#to 2009
max(IBM$Date,GE$Date,CocaCola$Date,ProcterGamble$Date,Boeing$Date)

#mean stock prices of IBM
mean(IBM$StockPrice)

#Minimum stock price of GE and Maximum stock prices of Coca Cola
min(GE$StockPrice)
max(CocaCola$StockPrice)

median(Boeing$StockPrice)

#Standard Deviation in stock price for P&G
sd(ProcterGamble$StockPrice)


#Plotting the Date(x) to StockPrice(y) for Coke
plot(CocaCola$Date,CocaCola$StockPrice, type = "l", col="red")

#Coke prices have huge variation reaching an all time high in 1973 of 146 but dropping immedietly to 30 in 1980

#Comparing this to the P&G stocks
lines(ProcterGamble$Date,ProcterGamble$StockPrice,col="blue")


#Analyzing the effects for the tech bubble burst of march 2000 on both Coca Cola and Proctor and Gamble
abline(v=as.Date(c("2000-03-01")),lwd=2)
#Although the prices of coca-cola were cheaper P&G had a greater fall in stock prices


#Visualizing stock dynamics between 1995(before tech revolution) to 2005(after tech bubble bursting)
plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],type = "l",col="red",ylim = c(0,210))
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col="blue")
lines(IBM$Date[301:432],IBM$StockPrice[301:432],col="green")
lines(GE$Date[301:432],GE$StockPrice[301:432],col="purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col="black")

#GE was the worst affected by the tech crash of march 2000 while IBM stocks reach their peak during the tech boom.

#Suprisingly P&G and Boeing were the most affected by the 97 Asia stock market collapse
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)


#Boeing was the quickest to recover from the stock market collapse


tapply(IBM$StockPrice,months(IBM$Date),mean)
#Analyzing the IBM stocks over the last 40 years we see that IBM stocksprices have historically been higher than average between January-May
#The prices are also at their lowest between September- November




#After seeing the following trends we know when to purchase stocks and when to sell them but I still have not accounted for a good or bad year which could skew the average to show a trend that is not really there in general.