getwd()
setwd(dir = "D:/Shiny/Office/LTE_GTM/")

dir()

### Libraries

library(openxlsx)
library(dplyr)
library(ggplot2)


### Unzip

unzip("Both brand.zip")

dir("Both brand/")


### Read

df_Robi<-read.xlsx(xlsxFile = "Both brand/Robi single sheet.xlsx",sheet = 1,startRow = 3)

str(df_Robi)


df_Airtel<-read.xlsx(xlsxFile = "Both brand/Airtel single sheet.xlsx",sheet = 1,startRow = 2)

str(df_Airtel)

### Sanitizing

# Robi

df_Robi$Brand<-"Robi"

df_Robi[,c(1:6,13)]<-lapply(df_Robi[,c(1:6,13)],factor)

df_Robi[,7:12][is.na(df_Robi[,7:12])]<-0

summary(df_Robi)

# Airtel


df_Airtel$Brand<-"Airtel"

df_Airtel[,c(1:6,13)]<-lapply(df_Airtel[,c(1:6,13)],factor)

df_Airtel[,7:12][is.na(df_Airtel[,7:12])]<-0

summary(df_Airtel)
str(df_Airtel)




### Merging both together

names(df_Airtel)<-names(df_Robi)

str(df_Robi)
str(df_Airtel)

df_merged<-rbind(df_Robi,df_Airtel)

saveRDS(df_merged,file = "df_merged.rds")
write.xlsx(df_merged,"df_merged.xlsx")


str(df_merged)

summary(df_merged)

### Exploration

df_regional_clean<-df_merged%>%
    filter(!REGION %in% c("Unclassified","Unknown"))%>%
    filter(!is.na(REGION))%>%
    group_by(REGION,FOURG_AREA,USIM_STATUS,Phone.type,Phone.make)%>%
    summarise(
        c1_Count=sum(C_1_USER_COUNT),
        c1_Usage=sum(C_1_TOTAL_DATA_USAGE_MB),
        c2_Count=sum(C_2_USER_COUNT),
        c2_Usage=sum(C_2_TOTAL_DATA_USAGE_MB),
        c3_Count=sum(C_3_USER_COUNT),
        c3_Usage=sum(C_3_TOTAL_DATA_USAGE_MB)
    )



saveRDS(df_regional_clean,file = "df_regional_clean.rds")

write.xlsx(df_regional_clean,"df_regional_clean.xlsx")
