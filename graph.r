library(tidyverse)
library(dplyr)
library(scales)
install.packages("fmsb")
library(fmsb)
library(ggrepel)

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")

Towns = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv")
broadband = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanBroardbandSpeeds.csv")

broadTown=Towns %>%
  left_join(broadband, by="shortPostcode")

ggplot(broadTown,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title="GREATER MANCHESTER Broadband Speeds")+
  geom_bar(data=filter(broadTown,County=="GREATER MANCHESTER"),aes(x=MaxDownload,fill="Maximum"),stat="Identity")+
  geom_bar(data=filter(broadTown,County=="GREATER MANCHESTER"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))


ggplot(broadTown,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title="MERSEYSIDE Broadband Speeds")+
  geom_bar(data=filter(broadTown,County=="MERSEYSIDE"),aes(x=MaxDownload,fill="Maximum"),stat="Identity")+
  geom_bar(data=filter(broadTown,County=="MERSEYSIDE"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))




ggplot(broadTown,aes(x=AverageDownload,y=District))+
  geom_boxplot(outlier.colour="red")+
  scale_x_continuous(limits=c(25,70), breaks=seq(25,70,10)) +
  labs(y="District",x="Speeds (Mbits/s)",title="Average Download Speeds")




Towns = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv")


housePrices = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanedHousePrice.csv', show_col_types = FALSE) %>%
  left_join(Towns,by="shortPostcode") %>%
  na.omit()


# BOXPLOT Average house prices by district (2020)
housePrices %>%
  filter(Year == 2020) %>%
  group_by(District) %>%
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000),
                     label = euro) +
  geom_boxplot() +
  labs(title="2020 house prices w.r to district")


# BARGRAPH houseprices by district (2019-2021)
housePrices %>%
  group_by(District) %>%
  summarise(AveragePrice = mean(Price)) %>%
  ggplot(aes(x = District, y = AveragePrice)) +
  geom_bar(position = "stack", stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(breaks = seq(0, 5000000, 30000),
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)),
            vjust = -0.25) +
  labs(title = "2019-2021 Average house prices w.r to district")+
  coord_flip()


# BARGRAPH houseprices by district (2020)
housePrices %>%
  filter(Year == 2021) %>%
  group_by(District,County) %>%
  summarise(AveragePrice = mean(Price)) %>%
  ggplot(aes(x = District, y = AveragePrice, fill = County)) +
  geom_bar(position = "stack",stat = "identity") +
  scale_y_continuous(breaks = seq(0, 5000000, 30000),
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)),
            vjust = -0.25) +
  labs(title = "2021 Average house prices w.r to district") +
  coord_flip()


#LINEGRAPH Average house prices by year (2019-2021)
housePrices %>%
  group_by(Year,County) %>%
  summarise(AveragePrice = mean(Price)) %>%
  ggplot(aes(x = Year, y = AveragePrice, color = County)) +
  geom_line(size = 1.5) +
  geom_text(aes(label = euro(AveragePrice)),
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 300000, 5000),
                     label = euro) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2,
             color = "steelblue")+
  labs(title = "2019-2021 Average house prices by year")



# BOXPLOT Average house prices by district (2019-2021)
housePrices %>%
  group_by(District) %>%
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000),
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 house prices by district")





#-------------end------------------------------------------

#-----------crime graph-------------------

cleanCrimes=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanCrimes.csv")

crimeData = cleanCrimes %>%
  left_join(Towns, by = "shortPostcode") %>%
  na.omit()


# Boxplot for 2019-2021 Drugs count by District
crimeData %>%
  filter(CrimeType == "Drugs") %>%
  ggplot(aes(x=District, y=n, fill=CrimeType)) +
  geom_boxplot() +
  labs(title=" 2019-2021 Drugs count by District")+
  coord_flip()



# Piechart for 2021 Robbery by District
cleanCrimes=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanCrimes.csv")

crimeData = cleanCrimes %>%
  left_join(Towns, by = "shortPostcode") %>%
  na.omit()

RobberyData <- crimeData %>%
  filter(CrimeType=="Robbery", Year == 2021) %>%
  group_by(Town) %>%
  mutate(sumCount = sum(n)) %>%
  ungroup() %>%
  mutate(perc =sumCount / sum(n)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>%
  distinct(Town, sumCount, perc, labels) %>%
  select(Town, sumCount, perc, labels)

RobberyData %>%
  ggplot(aes(x = "", y = perc, fill = Town)) +
  geom_col(color = "white") +
  coord_polar("y")+
  geom_label_repel(aes(label = labels),color="black",size=5,
              position = position_stack(vjust = 0.5),show.legend = FALSE )  +
  guides(fill = guide_legend(title = "Town"))+
  labs(title="Year 2021 Robbery based on Town")

cleanCrimes=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanCrimes.csv")

crimeData = cleanCrimes %>%
  left_join(Towns, by = "shortPostcode") %>%
  na.omit()


Drugoffenserate <- crimeData %>%
  filter(CrimeType=="Drugs") %>%
  group_by(Town) %>%
  mutate(sumCount = sum(n)) %>%
  ungroup() %>%
  mutate(perc =sumCount / sum(n)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>%
  distinct(Town, sumCount, perc, labels,Year,County) %>%
  select(Town, sumCount, perc, labels,Year,County)



Drugoffenserate %>%
  group_by(Year,County) %>%
  summarise(Averagerate = mean(perc*100)) %>%
  ggplot(aes(x = Year, y = Averagerate, color = County))+
  geom_line(size = 1.5) +
  geom_text(aes(label = (Averagerate)),
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 12, by = 0.5)) +
  scale_x_continuous(breaks = 2019:2021)+
  geom_point(size = 2,
             color = "red")+
  labs(title = "Drug offence rate of both county")




#-----------------school graph---------------

schoolData = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanSchools.csv', show_col_types = FALSE)

liverpoolSchool = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/livschool.csv")
manchesterSchoolData = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/manschool.csv')

#line graph of school year 2016-2019
schoolData %>%
  group_by(Year) %>%
  summarise(AverageAttainment = mean(Attainment8Score)) %>%
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5,
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment),
            vjust = -0.85) +
  scale_x_continuous(breaks = 2016:2019) +
  geom_point(size = 2,
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")

# Boxplot of year 2016-2019 where Attainment8Score is greater than 30
schoolData %>%
  filter(Attainment8Score>30) %>%
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Attainment8Score of Schools")


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (LIVERPOOL SCHOOL ONLY)
liverpoolSchool %>%
  filter(Attainment8Score>30) %>%
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Average Attainment8Score of Liverpool Schools")



# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (MANCHESTER SCHOOL ONLY)
manchesterSchoolData %>%
  filter(Attainment8Score>30) %>%
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Average Attainment8Score of Manchester Schools")



