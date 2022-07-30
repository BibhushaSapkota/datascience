library(tidyverse)
library(stringi)


#---------------house rank------
Towns = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv")%>%
  dplyr::select(PostCode, shortPostcode, Town, District, County)



HousePrices = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/uncleanedHouseprice.csv", show_col_types = FALSE)

summary(HousePrices$Price)
HousePricesRanking = Towns %>%
  left_join(House_price, by = "shortPostcode") %>%
  dplyr::select(District, shortPostcode, Price) %>%
  na.omit() %>%
  group_by(District) %>%
  summarise(AveragePrice = mean(Price)) %>%
  arrange(AveragePrice) %>%
  mutate(PriceScore=10 - AveragePrice/247995) %>%
  dplyr::select(District, PriceScore)



#--------------------------------------------------------------------------------------------------------------------------------------


# BROADBAND SPEED RANKING

broadband = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Broadband.csv', show_col_types = FALSE)

Towns  = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv") %>%
  dplyr::select(PostCode, shortPostcode, Town, District, County)

summary(broadband)


BroadbandSpeedRanking = Towns %>%
  mutate(shortPostcode = str_trim(str_sub(PostCode, -4,-1))) %>%
  left_join(broadband, by = "shortPostcode") %>%
  dplyr::select(District, shortPostcode, `Average download speed (Mbit/s)`) %>%
  na.omit() %>%
  group_by(District) %>%
  summarise(AverageDownloadSpeed = mean(`Average download speed (Mbit/s)`)) %>%
  mutate(DownloadScore=AverageDownloadSpeed/6) %>%
  dplyr::select(District, DownloadScore) %>%
  arrange(-DownloadScore)



#--------------------------------------------------------------------------------------------------------------------------------------


# CRIME RANKING

Town = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv")%>%
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>%
  dplyr::select(PostCode, shortPostcode, Town, District, County)

crimeData = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Crime.csv") %>%
  dplyr::select(ID,Year,shortPostcode,CrimeType, CrimeCount)


summary(crimeData$CrimeCount)

crimeDataRanking = crimeData %>%
  left_join(Town, by = "shortPostcode") %>%
  na.omit() %>%
  dplyr::select(District,CrimeCount) %>%
  group_by(District) %>%
  summarise(AverageCrimeCount=mean(CrimeCount)) %>%
  arrange(AverageCrimeCount) %>%
  mutate(CrimeCountScore=10 - AverageCrimeCount/391) %>%
  dplyr::select(District,CrimeCountScore)



#--------------------------------------------------------------------------------------------------------------------------------------


# SCHOOL RANKING

Towns = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/uncleanedHouseprice.csv")%>%
  filter(County == 'GREATER MANCHESTER' | County == 'MERSEYSIDE') %>%
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  dplyr::select(shortPostcode, Town, District, County) %>%
  na.omit()

SchoolData=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/School.csv")

summary(SchoolData$Attainment8Score)

SchoolScoreData = SchoolData %>%
  rename(shortPostcode=shortPostCode) %>%
  left_join(Towns,by="shortPostcode") %>%
  na.omit() %>%
  group_by(District,SchoolName) %>%
  summarise(score=mean(Attainment8Score)) %>%
  mutate(score=score/8) %>%
  dplyr::select(District,SchoolName,score) %>%
  arrange(-score)

SchoolRanking = SchoolScoreData %>%
  group_by(District) %>%
  summarise(score=max(score)) %>%
  left_join(SchoolScoreData, by="score") %>%
  arrange(-score) %>%
  filter(SchoolName!="Levenshulme High School" ,SchoolName!="Saint Paul's Catholic High School") %>%
  dplyr::select(District = District.x,SchoolName,SchoolScore=score) %>%
  distinct()




#--------------------------------------------------------------------------------------------------------------------------------------


# OVERALL RANKING


RankingMerge = HousePricesRanking %>%
  left_join(BroadbandSpeedRanking, by = "District") %>%
  left_join(crimeDataRanking, by = "District") %>%
  left_join(SchoolRanking, by = "District")


RankingMerge$SchoolName[is.na(RankingMerge$SchoolName)] <- "Not available"
RankingMerge$SchoolScore[is.na(RankingMerge$SchoolScore)] <- 0

overallRank = RankingMerge %>%
  group_by(PriceScore, DownloadScore,CrimeCountScore,SchoolScore) %>%
  mutate(overallScore = (PriceScore + DownloadScore + CrimeCountScore + SchoolScore)/4) %>%
  arrange(-overallScore)

