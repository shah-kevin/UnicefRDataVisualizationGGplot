install.packages("openxlsx")
library(openxlsx)
library(reshape)
library(ggplot2)
library(reshape2); library(dplyr); library(tidyr)
#countries <- c("United States","Germany","Greece","United States of America")
#setwd("C:/Analytics/Intermediate Analytics/Final Project")
#getwd()
unicef_data_Country_estimates<- read.xlsx("C:/Users/kevin/Downloads/U5MR_mortality_rate_2018.xlsx", "Country estimates", rows = 12:600,colNames = TRUE)
unicef_data_Country_estimates
unicef_data_Country_estimates<-melt(unicef_data_Country_estimates,variable.name = "Year",value.name = "Mortality_Rate") 
unicef_data_Country_estimates
unicef_data_Country_estimates<-subset(unicef_data_Country_estimates,unicef_data_Country_estimates$`Uncertainty.bounds*`=="Median")
unicef_data_Country_estimates

unicef_data_UNICEF_region <- read.xlsx("C:/Users/kevin/Downloads/U5MR_mortality_rate_2018.xlsx", "Regional and global estimates", rows = 2:39,colNames = TRUE)
unicef_data_UNICEF_region
unicef_data_UNICEF_region <- melt(unicef_data_UNICEF_region,variable.name = "Year",value.name = "Mortality_Rate")
unicef_data_UNICEF_region
unicef_data_UNICEF_region <- subset(unicef_data_UNICEF_region,unicef_data_UNICEF_region$`Uncertainty.bounds*`=="Median")
unicef_data_UNICEF_region

unicef_data_SDG_region <- read.xlsx("C:/Users/kevin/Downloads/U5MR_mortality_rate_2018.xlsx", "Regional and global estimates", rows = 40:104,colNames = TRUE)
unicef_data_SDG_region
unicef_data_SDG_region <- melt(unicef_data_SDG_region,variable.name = "Year",value.name = "Mortality_Rate")
unicef_data_SDG_region
unicef_data_SDG_region <- subset(unicef_data_SDG_region,unicef_data_SDG_region$`Uncertainty.bounds*` == 'Median')
unicef_data_SDG_region


ggplot()+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="India",], aes(x=Year, y=Mortality_Rate, group=1,colour="INDIA"))+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="United States",], aes(x=Year, y=Mortality_Rate, group=1,colour="United States"))+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="China",], aes(x=Year, y=Mortality_Rate, group=1,colour="China"))+
  scale_color_discrete(name = "Country", labels = c("INDIA", "UNITED STATES","CHINA"))

ggplot()+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="India",], aes(x=Year, y=Mortality_Rate, group=1,colour="INDIA"))+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="United States",], aes(x=Year, y=Mortality_Rate, group=1,colour="United States"))+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="China",], aes(x=Year, y=Mortality_Rate, group=1,colour="China"))+
  geom_line(data=unicef_data_UNICEF_region[unicef_data_UNICEF_region$Region.Name=="World",], aes(x=Year, y=Mortality_Rate, group=1,colour="World"))+
  scale_color_discrete(name = "Country", labels = c("INDIA", "UNITED STATES","CHINA","WORLD"))

ggplot()+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="China",], aes(x=Year, y=Mortality_Rate, group=1,colour="China"))+
  geom_line(data=unicef_data_UNICEF_region[unicef_data_UNICEF_region$Region.Name=="South Asia",], aes(x=Year, y=Mortality_Rate, group=1,colour="World"))+
  scale_color_discrete(name = "Country", labels = c("CHINA","SOUTH ASIA"))
ggplot()+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="India",], aes(x=Year, y=Mortality_Rate, group=1,colour="China"))+
  geom_line(data=unicef_data_UNICEF_region[unicef_data_UNICEF_region$Region.Name=="South Asia",], aes(x=Year, y=Mortality_Rate, group=1,colour="World"))+
  scale_color_discrete(name = "Country", labels = c("India","SOUTH ASIA"))
ggplot()+
  geom_line(data=unicef_data_Country_estimates[unicef_data_Country_estimates$Country.Name=="United States",], aes(x=Year, y=Mortality_Rate, group=1,colour="China"))+
  geom_line(data=unicef_data_UNICEF_region[unicef_data_UNICEF_region$Region.Name=="North America",], aes(x=Year, y=Mortality_Rate, group=1,colour="World"))+
  scale_color_discrete(name = "Country", labels = c("UNITED STATES","NORTH AMERICA"))

Under_five <- read.xlsx("C:/Users/sejal/Downloads/Cause-of-Death-2017.xlsx", "Country-specific Estimates", rows = 22:410,cols = 1:16,colNames = TRUE)
Under_five
Under_five <- melt(Under_five,id=c("ISO.Code","Country.Name","Year"),variable.name = "Disease",value.name = "Mortality_Rate") 
Under_five
Under_five <- subset(Under_five,Under_five$Year == '2016')
Under_five

causeofdeath_UNICEF_region <- read.xlsx("C:/Users/kevin/Downloads/Cause-of-Death-2017.xlsx", "Global and Regional Estimates ", rows = 23:47,cols = 1:15 ,colNames = TRUE)
causeofdeath_UNICEF_region <- melt(causeofdeath_UNICEF_region, id=c("Region.Name","Year"),variable.name = "Disease",value.name = "Mortality_Rate")
causeofdeath_UNICEF_region <- subset(causeofdeath_UNICEF_region,causeofdeath_UNICEF_region$Year == '2016')
causeofdeath_UNICEF_region

fill.color <- c( "blue", NA, NA, NA,"blue", NA, NA, NA, NA, NA, NA, NA,"blue")

ggplot(data=Under_five[Under_five$Country.Name == "United States of America",], aes(x=reorder(Disease,-Mortality_Rate), y=Mortality_Rate,fill = fill.color)) +
  geom_bar(stat="identity",show.legend = F)+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text = element_text(face="bold",size = 12))

fill.color <- c( "blue","blue", NA, NA,NA,"blue",NA,NA,NA,NA,NA,NA,NA)
ggplot(data=Under_five[Under_five$Country.Name == "India",], aes(x=reorder(Disease,-Mortality_Rate), y=Mortality_Rate,fill = fill.color)) +
  geom_bar(stat="identity",show.legend = F)+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text = element_text(face="bold",size = 12))


fill.color <- c( "blue", NA, NA, NA,"blue", NA, NA, NA, NA, NA, NA, NA,"blue")
ggplot(data=Under_five[Under_five$Country.Name == "China",], aes(x=reorder(Disease,-Mortality_Rate), y=Mortality_Rate,fill = fill.color)) +
  geom_bar(stat="identity",show.legend = F)+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text = element_text(face="bold",size = 12))

ggplot(data=causeofdeath_UNICEF_region[causeofdeath_UNICEF_region$Region.Name == "North America",], aes(x=reorder(Disease,-Mortality_Rate), y=Mortality_Rate,fill=fill.color <- c( "blue", NA, NA, NA,"blue", NA, NA, NA, NA, NA, NA, NA,"blue"))) +
  geom_bar(stat="identity",show.legend = F)+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text = element_text(face="bold",size = 12))

ggplot(data=causeofdeath_UNICEF_region[causeofdeath_UNICEF_region$Region.Name == "South Asia",], aes(x=reorder(Disease,-Mortality_Rate), y=Mortality_Rate,fill=fill.color <- c( "blue","blue", NA, NA,NA,"blue", NA, NA, NA, NA, NA, NA,NA))) +
  geom_bar(stat="identity",show.legend = F)+
  theme_bw()+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text = element_text(face="bold",size = 12))

