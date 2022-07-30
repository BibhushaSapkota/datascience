library(tidyverse)
library(dplyr)


#cleaning houseprices
houseprice2019=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/pp-2019.csv")
houseprice2020=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/pp-2020.csv")
houseprice2021=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/pp-2021.csv")
colnames(houseprice2019) = c("ID","Price","Date","PostCode","PAON","SAON","FL","House Num","Flat","Street Name","Locality","Town","District","County","Type1","Type2")
colnames(houseprice2020) = c("ID","Price","Date","PostCode","PAON","SAON","FL","House Num","Flat","Street Name","Locality","Town","District","County","Type1","Type2")
colnames(houseprice2021) = c("ID","Price","Date","PostCode","PAON","SAON","FL","House Num","Flat","Street Name","Locality","Town","District","County","Type1","Type2")

Houseprices= houseprice2021 %>%
  add_row(houseprice2019) %>%
  add_row(houseprice2020)
view(Houseprices)

write.csv(Houseprices,"C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/uncleanedHouseprice.csv")

cleanHousePrices = Houseprices %>%
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>%
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year=substring(Date,1,4)) %>%
  arrange(County) %>%
  select(PostCode,shortPostcode,Price,Year,PAON)
View(cleanHousePrices)
write.csv(cleanHousePrices,"C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanedHouseprice.csv")

# cleaning population data
population=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/Population2011_1656567141570.csv")
view (population)

PopulationData = population %>%  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>%
  group_by(shortPostcode) %>%
  summarise_at(vars(Population),list(Population2011 = sum)) %>%
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>%
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>%

  select(shortPostcode,Population2016,Population2017,Population2018,Population2019,Population2020)

FilteredTown = Houseprices %>%
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE")

Towns = FilteredTown %>%
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year=str_trim(substring(Year,1,4))) %>%
  left_join(PopulationData,by = "shortPostcode") %>%
  select(shortPostcode,Town,District,County,Population2016,Population2017,Population2018,Population2019,Population2020) %>%
  group_by(shortPostcode) %>%
  filter(row_number()==1) %>%
  arrange(County) %>%
  na.omit()

write.csv(Towns, "C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv")

#cleaning data of broadband Speed
BroadbandSpeed=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/BroadbandSpeeds.csv")
BroadbandSpeed = replace(BroadbandSpeed,is.na(BroadbandSpeed), 0)
cleanBroadbandSpeed = BroadbandSpeed %>%
  mutate(shortPostcode = str_trim(substring(postcode_space, 1,4))) %>%
  group_by(shortPostcode) %>%
  summarise_at(vars("Average download speed (Mbit/s)","Maximum download speed (Mbit/s)","Average upload speed (Mbit/s)","Maximum upload speed (Mbit/s)"),
               list(name = mean)) %>%
  left_join(Towns,by="shortPostcode") %>%
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>%
  arrange(County) %>%
  select(-County,-Town,-District,-Population2016,-Population2017,-Population2018,-Population2019,-Population2020) %>%
  rename("AverageDownload"="Average download speed (Mbit/s)_name","MaxDownload"="Maximum download speed (Mbit/s)_name","AverageUpload"="Average upload speed (Mbit/s)_name","MaxUpload"="Maximum upload speed (Mbit/s)_name")
write.csv(cleanBroadbandSpeed,"C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanBroardbandSpeeds.csv")



#lsoa to postcode
Towns = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv")
lsoa = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/Postcode to LSOA.csv")
lsoa  = lsoa %>%
  mutate(shortPostcode = str_trim(substring(pcds, 1,4))) %>%
  left_join(Towns,by="shortPostcode") %>%
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>%
  group_by(lsoa11cd) %>%
  filter(row_number()==1) %>%
  select(lsoa11cd,shortPostcode,Town,District,County)
lsoa

#Cleaning data of crime

crime2019=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/crime2019.csv")
crime2020=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/crime2020.csv")
crime2021=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/crime2021.csv")

crimedata =crime2019%>%
  add_row(crime2020) %>%
  add_row(crime2021) %>%
  mutate(Year=substring(Month, 1,4)) %>%
  rename(lsoa11cd="LSOA code",CrimeType="Crime type") %>%
  select(lsoa11cd,Year,CrimeType)

cleanCrimes = crimedata %>%
  left_join(lsoa,by="lsoa11cd")%>%
  group_by(shortPostcode,Year,CrimeType)  %>%
  select(shortPostcode,Year,CrimeType) %>%
  tally()
write.csv(cleanCrimes, "C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanCrimes.csv")



#school

library(scales)
library(stringi)

# setting working directory

man2020 = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/manchesterSchool.csv', show_col_types = FALSE) %>% mutate(Year = 2020) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
man2019= read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/manchesterSchool19.csv', show_col_types = FALSE)%>% mutate(Year = 2019) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
man2018= read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/manchesterSchool18.csv', show_col_types = FALSE)%>% mutate(Year = 2018) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
man2017 = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/manchesterSchool17.csv', show_col_types = FALSE) %>% mutate(Year = 2017) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
man2016 = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/manchesterSchool16.csv', show_col_types = FALSE) %>% mutate(Year = 2016) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
liv2020 = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/liverpoolSchool.csv', show_col_types = FALSE)%>% mutate(Year = 2020) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
liv2019 = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/liverpoolSchool19.csv', show_col_types = FALSE)%>% mutate(Year = 2019) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
liv2018= read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/liverpoolSchool18.csv', show_col_types = FALSE)%>% mutate(Year = 2018) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
liv2017= read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/liverpoolSchool17.csv', show_col_types = FALSE)%>% mutate(Year = 2017) %>% select(Year, PCODE, SCHNAME, ATT8SCR)
liv2016 = read_csv('C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/assignment datasets/liverpoolSchool16.csv', show_col_types = FALSE)%>% mutate(Year = 2016) %>% select(Year, PCODE, SCHNAME, ATT8SCR)


manschool=man2020 %>%
  add_row(man2019) %>%
  add_row(man2018) %>%
  add_row(man2017) %>%
  add_row(man2016) %>%
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>%
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%
  mutate(ID = row_number()) %>%
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(manschool) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")
write.csv(manschool, "C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/manschool.csv")


livschool = liv2020%>%
  add_row(liv2019) %>%
  add_row(liv2018) %>%
  add_row(liv2017) %>%
  add_row(liv2016) %>%
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>%
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%
  mutate(ID = row_number()) %>%
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(livschool) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")
write.csv(livschool, "C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/livschool.csv")

Schools= manschool %>%
  add_row(livschool) %>%
  mutate(shortPostCode = str_trim(substring(PostCode,1,4))) %>%
  filter(Attainment8Score != "SUPP" & Attainment8Score != "NE") %>%
  mutate(ID = row_number()) %>%
  select(ID, Year, PostCode, shortPostCode, SchoolName, Attainment8Score) %>%
  na.omit()
colnames(Schools) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")


write.csv(Schools, "C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanSchools.csv")




