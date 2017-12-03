getwd()

setwd("D:\\CS_2\\Post_eRA/4G GTM/")

dir()

library(openxlsx)
library(dplyr)



### Channel

channel<-read.csv2("Channel.txt",sep="\t",header = T,stringsAsFactors = T)

channel<-channel%>%select(-c(X,X.1))

channel$technology<-as.factor(channel$technology)
channel$C2C.Amount<-as.numeric(channel$C2C.Amount)
channel$Data.Sales.Amount<-as.numeric(gsub(",","",channel$Data.Sales.Amount))
channel$RET.number<-as.factor(channel$RET.number)

dim(channel)

colSums(is.na(channel))

### Device

device<-read.csv2("Device.txt",sep="\t",header = T,stringsAsFactors = T)

device$RGB.Count<-as.numeric(device$RGB.Count)
device$Data.User.count.Monthly.usage.100.MB<-as.numeric(device$Data.User.count.Monthly.usage.100.MB)
device$Total.Data.Usage..GB.<-as.numeric(device$Total.Data.Usage..GB.)

colSums(is.na(device))
dim(device)

### Prefer

prefer<-read.csv2("Preferred.txt",sep="\t",header = T,stringsAsFactors = T)

prefer<-prefer%>%select(-c(X,X.1,X.2,Deno))

prefer$SENDER_MSISDN<-as.factor(prefer$SENDER_MSISDN)


str(prefer)
colSums(is.na(prefer))

### Channel + Prefer

channel_prefer<-channel%>%
  left_join(prefer,by=c("RET.number"="SENDER_MSISDN"))

dim(channel_prefer)

colSums(is.na(channel_prefer))


#####
data_sales_UL<-5000
preferred_outlet_UL<-20

channel_prefer_1<-channel_prefer%>%
  filter(!is.na(BRAND))%>%
  mutate(
    High_Data_Outlet=ifelse(Data.Sales.Amount>data_sales_UL,'Y','N'),
    Prefeered_Data_Outlet=ifelse(HV.subs.served...100MB.>preferred_outlet_UL,'Y','N')
  )


colSums(is.na(channel_prefer_1))

dim(channel_prefer)

# save.image(file = "Script.RData")

library(ggplot2)



pref_outlet_thres<-30

channel_prefer_2<-channel_prefer_1%>%
  arrange(desc(Data.Sales.Amount))%>%
  filter(areas=='selected')%>%
  mutate(
    cum_data_sales_amount=cumsum(Data.Sales.Amount),
    cum_preferred_outlet=cumsum(ifelse(HV.subs.served...100MB.>pref_outlet_thres,1,0)),
    cum_HVC_Data_User=cumsum(HV.subs.served...100MB.),
    cum_outlets=row_number(),
    total_data_sales_amount=sum(Data.Sales.Amount),
    total_preferred_outlets=sum(ifelse(HV.subs.served...100MB.>pref_outlet_thres,1,0),na.rm=T),
    total_HVC_Data_User=sum(HV.subs.served...100MB.),
    total_outlets=n(),
    cum_percent_data_sales_amount= round(cum_data_sales_amount*100/total_data_sales_amount,1),
    cum_percent_preferred_outlet= round(cum_preferred_outlet*100/total_preferred_outlets,1),
    cum_percent_HVC_Data_User=round(cum_HVC_Data_User*100/total_HVC_Data_User,1),
    cum_pecent_outlet=round(cum_outlets*100/total_outlets,3)
    )

channel_prefer_2%>%head()

colSums(is.na(channel_prefer_2))



ggplot(channel_prefer_2,aes(cum_outlets,cum_percent_preferred_outlet))+
  geom_line(col='blue')+
  geom_text(aes(
    label=ifelse(cum_outlets %in% c(5000,10000,20000,30000),paste0(as.character(cum_outlets),",",as.character(cum_percent_preferred_outlet)),"")),
    size=3,color='darkblue',hjust=0,vjust=0
    )+
  geom_line(aes(y=cum_percent_data_sales_amount),col='red')+
  geom_line(aes(y=cum_percent_HVC_Data_User),col='green')+
  theme_gray()




jpeg(filename = "Outlets Paretto.jpeg")

label_cut_x<-c(10,20,30.40,50)

ggplot(channel_prefer_2,aes(cum_pecent_outlet,cum_percent_preferred_outlet))+
  geom_line(aes(col='blue'))+
  geom_line(aes(y=cum_percent_data_sales_amount,col='red'))+
  geom_line(aes(y=cum_percent_HVC_Data_User,col='green'))+
  geom_text(aes(y=cum_percent_preferred_outlet,
    label=ifelse(cum_pecent_outlet %in% label_cut_x,paste0(as.character(round(cum_pecent_outlet,0)),"%,",as.character(round(cum_percent_preferred_outlet,0)),"%"),"")),
    size=3,color='blue',hjust=0,vjust=0)+
  geom_text(aes(y=cum_percent_data_sales_amount,
                label=ifelse(cum_pecent_outlet %in% label_cut_x,paste0(as.character(round(cum_percent_data_sales_amount,0)),"%"),"")),
            size=3,color='red',hjust=0,vjust=0)+
  geom_text(aes(y=cum_percent_HVC_Data_User,
                label=ifelse(cum_pecent_outlet %in% label_cut_x,paste0(as.character(round(cum_percent_HVC_Data_User,0)),"%"),"")),
            size=3,color='green',hjust=0,vjust=0)+
  xlab("Percent of Total Outlets in Selected Areas")+
  ylab("Cumulative %")+
  ggtitle("Outlets Paretto",subtitle = "based on Ocotber'17 data " )+
  scale_color_manual("Legends",
                     values=c("blue"="blue","red"="red","green"='green'),
                     labels=c("% Preferred Retailer","% Data Sales Amount","% Data User(>100 MB)")
                     )+
  theme_gray()


dev.off()
