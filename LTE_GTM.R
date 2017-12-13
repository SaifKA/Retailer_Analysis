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

df_merged<-readRDS("df_merged.rds")

str(df_merged)

class(df_merged)

summary(df_merged)



### Exploration



df_regional_clean<-df_merged%>%
    filter(!REGION %in% c("Unclassified","Unknown"))%>%
    filter(!is.na(REGION))%>%
    group_by(Brand,REGION,FOURG_AREA,USIM_STATUS,Phone.type,Phone.make)%>%
    summarise(
        c1_Count=sum(C_1_USER_COUNT),
        c1_Usage=sum(C_1_TOTAL_DATA_USAGE_MB),
        c2_Count=sum(C_2_USER_COUNT),
        c2_Usage=sum(C_2_TOTAL_DATA_USAGE_MB),
        c3_Count=sum(C_3_USER_COUNT),
        c3_Usage=sum(C_3_TOTAL_DATA_USAGE_MB)
    )

df_regional_clean<-as.data.frame(df_regional_clean)

str(df_regional_clean)

class(df_regional_clean)



saveRDS(df_regional_clean,file = "df_regional_clean.rds")

write.xlsx(df_regional_clean,file = "df_regional_clean.xlsx")


#### New Data

# Robi Part

df_Robi_New<-read.csv2("data/Robi.csv",header = T,sep=",")

df_Robi_New%>%
    head()

str(df_Robi_New)

df_Robi_New[,c(1:4)]<-lapply(df_Robi_New[,c(1:4)],factor)

df_Robi_New[,5:27][is.na(df_Robi_New[,5:27])]<-0

summary(df_Robi_New)

df_Robi_New$Brand<-'Robi'

# Airtel Part

df_AT_New<-read.csv2("data/Airtel.csv",header = T,sep=",")

df_AT_New%>%
    head()

str(df_AT_New)


df_AT_New[,c(1:4)]<-lapply(df_AT_New[,c(1:4)],factor)

df_AT_New[,5:27][is.na(df_AT_New[,5:27])]<-0

summary(df_AT_New)

df_AT_New$Brand<-'AT'

# Merging



df_merged_New<-rbind(df_Robi_New,df_AT_New)

saveRDS(df_merged_New,file = "df_merged_New.rds")
write.xlsx(df_merged_New,"df_merged_New.xlsx")

df_merged_New<-readRDS("df_merged_New.rds")

str(df_merged_New)

class(df_merged_New)

summary(df_merged_New)


# Exploration

df_merged_New[,c(1:4)]<-lapply(df_merged_New[,c(1:4)],as.character)




str(df_merged_New)

df_regional_clean_New


df_merged_New%>%
    filter(nchar(REGION_NAME)>0)%>%
    mutate(COUNT=1)%>%
    group_by(Brand,REGION_NAME,SITE_TYPE,FOURG_AREA)%>%
    summarise_at(
        .vars= names(.)[c(5:27,29)],
        .funs=c(sum="sum"
                )
    )%>%
    View()


df_regional_clean_New<-as.data.frame(df_regional_clean_New)

str(df_regional_clean_New)

class(df_regional_clean_New)



saveRDS(df_regional_clean_New,file = "df_regional_clean_20171214.rds")

write.xlsx(df_regional_clean_New,file = "df_regional_clean_20171214.xlsx")


