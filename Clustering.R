library(tidyverse)
library()

#IMPORTING THE DATASET
DF.MAIN=read.csv("D:/Mini Hackathon/Finals/Datasets/Outlet Info.csv")
DF.backup=read.csv("D:/Mini Hackathon/Finals/Datasets/Outlet Info.csv")

df=DF.MAIN
#Structure of the dta
str(df)

#Summary of the data
summary(df)

#Importing the sales data
cols.Sales=colnames(Sales)[1]
DF.MAIN.SALES=Sales[,c(1,3,4,6)]
df.sales=DF.MAIN.SALES
str(df.sales)

#CLEANING THE DATA

#arrange df by outletcode


library(stringr)
er=df.sales$expected_rainfall
er=as.numeric(substr(df.sales$expected_rainfall,1,nchar(df.sales$expected_rainfall)-2))
df.sales$expected_rainfall=er
str(df.sales)


Outlet_Info.sale=df.sales %>% group_by(outlet_id) %>%
  summarise(Total_Sales=sum(sales_quantity),Total_expected_rainfall=sum(expected_rainfall)) 


#Remove first names in the outlet id
s=df$outlet_id
s=(substring(s, first = 13))
df$outlet_id=s
length(unique(df$outlet_id))

#Removing the first strings in the outlet id in Outlet_Info.sale
s=Outlet_Info.sale$outlet_id
s=(substring(s, first = 13))
Outlet_Info.sale$outlet_id=s
length(unique(Outlet_Info.sale$outlet_id))

#Turning Outlet_Info.sale$outlet_id into numbers
Outlet_Info.sale$outlet_id=as.numeric(Outlet_Info.sale$outlet_id)

Outlet_Info.sale = Outlet_Info.sale %>%
  arrange(outlet_id)

#Adding Sales and Expected Rainfall to the df dataset
df$sales_quantity=Outlet_Info.sale$Total_Sales
df$expected_rainfall=Outlet_Info.sale$Total_expected_rainfall

#checking the structure of the df and summary of the data
str(df)

#Factorizing the region
df$region=factor(df$region)
str(df)


#Adding a Recency Vector into the dataset
df.recency=Sales[,c("outlet_id","transaction_time")]
str(df.recency)

#Checking the recency
head(df.recency$transaction_time)
tail(df.recency$transaction_time)
da=as.Date(df.recency$transaction_time)
str(da)
head(da)

max_date=max(da)
df.recency=df.recency %>% mutate(Recency=max_date - da)
#mode(df)

#Recency
rfm_r=df.recency %>%
  group_by(outlet_id) %>%
  summarise(R=min(Recency))

#Removing the first strings in the outlet id in rfm
s=rfm_r$outlet_id
s=(substring(s, first = 13))
rfm_r$outlet_id=s
length(unique(rfm_r$outlet_id))

#Turning Outlet_Info.sale$outlet_id into numbers
rfm_r$outlet_id=as.numeric(rfm_r$outlet_id)

#Turning recency into numbers
rfm_r$R=as.numeric(rfm_r$R)

#Turning rfm
rfm_r=rfm_r %>%
  arrange(outlet_id)

df$Recency=rfm_r$R
str(df)

write.csv(df,"D:Mini Hackathon/Finals/Datasets/Clustering datasets/MAIN_CLUSTER_DATASET.csv",row.names = FALSE)
