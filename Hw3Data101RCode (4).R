Citi <- citibike.May21.50K

Citi$started_at = as.POSIXct(Citi$started_at)
Citi$ended_at = as.POSIXct(Citi$ended_at)

Citi$ride_duration_in_minutes = as.double((Citi$ended_at-Citi$started_at)/60)

Linear_Regression= lm(Citi$ride_duration_in_minutes ~ Citi$member_casual, data= Citi)

summary(Linear_Regression)

Citi$ride_time_categories <- cut(Citi$ride_duration_in_minutes, breaks=c(0,6.367,17.983,20.883,6747.217),labels=c("Short", "Medium", "Long", "Very 
Long"))

barplot(Citi$ride_duration_in_minutes, width = Citi$member_casual)
boxplot(Citi$member_casual, x = Citi$ride_duration_in_minutes)

library(ggplot2)
Stacked <- ggplot(data = Citi, aes(x= member_casual, y= Citi$ride_duration_in_minutes , fill = Citi$ride_time_categories)
Stacked <- geom_bar(stat = "Count")
Stacked





Citi$New_Member_Type_Int<- cut(Citi$member_casual, breaks=c('Casual','Member'),labels=c('1','2')
> ride2






Citi.casual <- subset(Citi,member_casual=="casual")
Citi.member <-subset(Citi,member_casual=='member')


boxplot(Citi.casual$ride_duration~ day(Citi.casual$started_at),las=2, main="Ride Length by Day for Casual 
Riders",ylime=c(0,6000))


install.packages('plyr')
library(plyr)
Citi$member_casual=revalue(Citi$member_casual, c("casual"="1", "member"="2"))

library(ggplot2)


Plot1<- plot(Citi$member_casual, Citi$ride_duration_in_minutes)
Barplot_of_MemberRideDuration <- barplot(Citi.member$ride_duration_in_minutes, space = )

boxplot(Citi$ride_duration_in_minutes~(Citi$member_casual), las=2, ylime=c(0,1500))
na.omit(Citi)
Plot1 <- plot(Citi$member_casual, Citi$ride_duration_in_minutes, ylim=c(0,400), main = "Plot of Ride Duration in Minutes", sub = "1= Casual Ride Type and 2= Member Ride Type")

Citi2020 <- (citibike.May20.50K)

Citi2020$Age = as.double(2022-Citi2020$birth.year)
Citi2020$Triptime_in_Minutes = as.double(Citi2020$tripduration/60)

install.packages("ggthemes") 
library(ggthemes)


ggplot(Citi2020, aes(Citi2020$Age, Citi2020$Triptime_in_Minutes)) + geom_point(size=0.8) + xlim(10,100) + ylim(0,2000) + geom_smooth(linetype="dashed") + ggtitle("Citi Age vs Trip Time") + labs(y="Trip Time in Minutes", x="Age") + theme_economist()


Citi2020.Male = subset(Citi2020,gender=="1")
summary(Citi2020.Male$tripduration)

Citi2020.Female = subset(Citi2020,gender=="2")

library(ggplot2)

ggplot(Citi2020, aes(x=Citi2020$Triptime_in_Minutes, fill=Citi2020$gender)) + geom_histogram() + facet_wrap(-Citi2020$gender)
library(plyr)
Citi2020$gender=mapvalues(Citi2020$gender, "2","Female")


ggplot(Citi2020, aes(x=Citi2020$Triptime_in_Minutes, col=Citi2020$gender)) + geom_density()

Citi2020$CategoryofRideTime <- cut(Citi2020$tripduration, breaks=c(0,300,600,1800,6000),labels=c("Short", "Medium", "Long", "Very 
Long"))

ggplot(Citi2020, aes(x=Citi2020$gender, fill=Citi2020$CategoryofRideTime)) + geom_bar(position="dodge") + ggtitle("Count of Categorical Ride Times by Gender") + labs(y="Count of Categorical Ride Times", x="Grouped by Gender", fill="Category of Ride Time") + theme_dark()+ theme(
  plot.title = element_text(color="Blue", size=15, face="bold.italic"),
  axis.title.y = element_text(color="Black", size=12, face="italic"),
  axis.title.x = element_text(color="Red", size=12, face="italic"))


