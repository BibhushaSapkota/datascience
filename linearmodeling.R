library(tidyverse)


#liner regression
Towns = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/Towns.csv")%>%
  select(shortPostcode, Town, District, County)
prices = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanedHousePrice.csv")

speeds = read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanBroardbandSpeeds.csv")

crime=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanCrimes.csv")

schools=read_csv("C:/Users/Acer/OneDrive - Softwarica College/Documents/assignment/cleaneddata/cleanSchools.csv")

priceTown= prices %>%
  left_join(Towns,by="shortPostcode") %>%
  na.omit()

SpeedTown = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  na.omit()

SchoolTown = schools %>%
  rename("shortPostcode"="shortPostCode")

schoolTowns=schools %>%
  left_join(Towns,by="shortPostcode") %>%
  na.omit()

crimeTown=crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town) %>%
  filter(CrimeType=="Drugs") %>%
  na.omit()




#-------------------------------

priceSpeed=priceTown %>%
  left_join(SpeedTown,by="shortPostcode") %>%
  na.omit()


priceDrug=crimeTown %>%
  left_join(priceTown,by="shortPostcode") %>%
  na.omit()





DownloadSchool=schools %>%
  left_join(crimeTown,by="shortPostcode") %>%
  na.omit()

schoolDrugs=SpeedTown %>%
  left_join(schoolTowns,by="shortPostcode") %>%
  na.omit()


speeddrug=SpeedTown %>%
  left_join(crimeTown,by="shortPostcode") %>%
  na.omit()

speedschool=SpeedTown %>%
  left_join(schoolTowns,by="shortPostcode") %>%
  na.omit()

#-----------------------------
options(scipen = 100)

lm_piceSpeed=lm(Price~AverageDownload,data=priceSpeed)
summary(lm_piceSpeed)


plot(priceSpeed$Price,priceSpeed$AverageDownload,"Regression for houseprie on downloa speed",xlab="price",
     y="speed")
abline(lm(Price~AverageDownload,data=priceSpeed),col="red")


#new
a = hp_bb%>%
  group_by(shortPostcode)%>%
  summarise(meanHP = mean(Price), meanDS = mean(`Average download speed (Mbit/s)`))


ggplot(priceSpeed, aes(meanHP, meanDS)) +
  geom_point(aes(color = meanHP)) +
  geom_smooth(method="lm", se=FALSE)

cor(priceSpeed$meanDS, priceSpeed$meanHP)

model_ds_hp = lm(meanHP~meanDS, data=b)
summary(model_ds_hp)
model_ds_hp$coefficients

mean_price=(meanp=mean(priceSpeed$Price))
Downloadspeed=priceSpeed$AverageDownload



#linear regression of price and average download
ggplot(priceSpeed, aes(x=Price,y=AverageDownload)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
model = lm(Price~AverageDownload,data=priceSpeed)
summary(model)
model$coefficients

#linear regression of price and drug offence
ggplot(priceDrug, aes(x=Price,y=n)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
model = lm(Price~n,data=priceDrug)
summary(model)
model$coefficients


#mistake
#linear regression of drug and school
ggplot(schoolDrugs, aes(x=Attainment8Score,y=n)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
model = lm(Attainment8Score~n,data=schoolDrugs)
summary(model)
model$coefficients


#linear regression of drug and downloadspeed
ggplot(speeddrug, aes(x=n,y=AverageDownload)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
model = lm(AverageDownload~n,data=speeddrug)
summary(model)
model$coefficients

#linear regression speed and school
ggplot(speedschool, aes(x=AverageDownload,y=Attainment8Score)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
model = lm(AverageDownload~Attainment8Score,data=speedschool)
summary(model)
model$coefficients

