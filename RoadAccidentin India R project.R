c<-read.csv("C:\\Users\\hp\\OneDrive\\Desktop\\traffic-accidents-cause-state-city-2021.csv")
library(tidyverse)
library(ggplot2)
library(GGally)
glimpse(c)
injured<-c(c$Total.Road.Accidents.Injured)
died<-c(c$Total.Road.Accidents.Died)
ggplot(c,aes(x=onlystatesinjured,
             y=onlystatesdied,
             col=onlystates))+
  geom_point()
boxplot.stats(only)$out
out_ind<-which(injured %in% c(out))
out_ind
c[out_ind,]
boxplot(onlystatesdied_out, ylab="no. of accidents",main="boxplot for died in total accidents")
mtext(paste("Outliers:",paste(out,collapse=",")))
lower<-quantile(injured,0.025, na.rm = TRUE)
lower
upper<-quantile(injured,.95,na.rm=TRUE)
upper
outliers<-which(injured<lower | injured>upper)
print(outliers)
#jharkhand,kerala,sikkim,west bengal,ladakh,meerut,nagpur,vishakhapatnam are treated as outlier.
iqr<-IQR(injured,na.rm=TRUE)
up<-quantile(injured,0.75,na.rm = TRUE)+1.5*iqr
low<-quantile(injured,0.25,na.rm = TRUE)-1.5*iqr
injured_no_out<-subset(injured, injured>low & injured<up)
injured_no_out
#injured stats without outliers
lower1<-quantile(died,0.025, na.rm = TRUE)
lower1
upper1<-quantile(died,.95,na.rm=TRUE)
upper1
outliers1<-which(died<lower | died>upper)
outliers1
#mizoram,west bengal,ladakh are treated as outliers
iqr1<-IQR(died,na.rm=TRUE)
up1<-quantile(died,0.75,na.rm = TRUE)+1.5*iqr
low1<-quantile(died,0.25,na.rm = TRUE)-1.5*iqr
died_no_out<-subset(died, died>low & died<up)
died_no_out
df<-tibble(onlystatesdied,onlystatesinjured,onlystates)
ggpairs(df,cardinality_threshold = 28)
length(injured_no_out)
length(died_no_out)
h=c(onlystatesinjured,onlystatesdied)
ggplot(c,aes(x=onlystatesinjured,
             y=onlystatesdied,
             col=h))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)`
model1<-lm(onlystatesdied_out ~ onlystatesdied_out,data=df)
summary(model1)
plot(model1)
states<-data.matrix(onlystates)
states
predict(model1,df)
fitted<-predict(model1,df)
df$pred<-fitted
print(df,n=28)
onlystates<-c('ANDHRA PRADESH','ARUNACHAL PRADESH','ASSAM','BIHAR','CHHATTISGARH','GOA','GUJARAT','HARYANA','HIMACHAL PRADESH','JHARKHAND','KARNATAKA','KERALA','MADHYA PRADESH','MAHARASHTRA','MANIPUR','MEGHALAYA','MIZORAM','NAGALAND','ODISHA','PUNJAB','RAJASTHAN','SIKKIM','TAMIL NADU','TELANGANA','TRIPURA','UTTAR PRADESH','UTTARAKHAND','WEST BENGAL')
pie(onlystatesdied_out,labels = onlystates,radius = +1.1,main="pie chart for Total died cases")
onlystatesdied<-c(8186,173,3014,7660,5413,226,7457,4983,1032,3513,10038,3429,12480,13911,110,187,64,23,5081,4516,10043,64,15384,7557,194,21792,824,5831)
iqr2<-IQR(onlystatesdied,na.rm=TRUE)
up2<-quantile(onlystatesdied,0.75,na.rm = FALSE)+1.5*iqr
low2<-quantile(onlystatesdied,0.25,na.rm = FALSE)-1.5*iqr
onlystatesdied_out<-subset(onlystatesdied, onlystatesdied>low & onlystatesdied<up)
onlystatesdied_out
lower2<-quantile(onlystatesdied,0.025, na.rm = TRUE)
lower2
upper2<-quantile(onlystatesdied,.95,na.rm=TRUE)
upper2
outliers2<-which(onlystatesdied<lower | onlystatesdied>upper)
outliers2
onlystatesinjured<-c(21040,266,5420,7946,10682,856,13722,7972,3445,3227,40754,36514,47117,19676,504,263,28,35,9782,3034,19357,178,55996,20107,546,19813,1127,9796)
pie(onlystatesinjured_out,labels = onlystates,radius = +1.1,main="pie chart for Total injured cases")
iqr3<-IQR(onlystatesinjured,na.rm=TRUE)
up3<-quantile(onlystatesinjured,0.75,na.rm = FALSE)+1.5*iqr
low3<-quantile(onlystatesinjured,0.25,na.rm = FALSE)-1.5*iqr
onlystatesinjured_out<-subset(onlystatesdied, onlystatesinjured>low & onlystatesinjured<up)
onlystatesinjured_out
lower3<-quantile(onlystatesinjured,0.025, na.rm = TRUE)
lower3
upper3<-quantile(onlystatesinjured,.95,na.rm=TRUE)
upper3
outliers3<-which(onlystatesinjured<lower | onlystatesinjured>upper)
outliers3
barplot(onlystatesinjured,onlystates)
