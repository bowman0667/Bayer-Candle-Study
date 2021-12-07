library(tidyverse)
library(kableExtra)


longRawDataSet <- RawDataBayerLantern11_12_2021 %>%
  gather(key = "time", value = "knockdowncount", -c(location, date))


oneMeter <- filter(longRawDataSet, grepl("[1]", location)) 
OneMeter = oneMeter %>% 
  mutate("distance" = "1m")
head(OneMeter)

twoMeter <- filter(longRawDataSet, grepl("[2]", location)) 
TwoMeter = twoMeter %>% 
  mutate("distance" = "2m")
head(OneMeter)

threeMeter <- filter(longRawDataSet, grepl("[3]", location)) 
ThreeMeter = threeMeter %>% 
  mutate("distance" = "3m")
head(OneMeter)

fourMeter <- filter(longRawDataSet, grepl("[4]", location)) 
FourMeter = fourMeter %>% 
  mutate("distance" = "4m")
head(OneMeter)

x <- full_join(OneMeter, TwoMeter) %>% 
  full_join(ThreeMeter) %>% 
  full_join(FourMeter)
tibble(x)

x <- filter(x, time != 'date')

x %>% 
  kbl() %>% 
  kable_classic_2()

x$time <- factor(x$time, levels = c('15m', '30m', '45m', '1hr', '2hr', '4hr', '24hr'))



xFINAL %>% 
  ggplot(aes(distance, knockdowncount, fill = height)) +
  geom_boxplot() + 
  facet_wrap(~time)
ggsave("goodLanternBoxPlotWithLEVELS5thExp.png", dpi = 300, width = 15, height = 8, scale = 1)

xFINAL %>% 
  ggplot(aes(distance, knockdowncount)) +
  geom_boxplot() + 
  facet_wrap(~time)
ggsave("goodLanternBoxGeneral5thExp.png", dpi = 300, width = 15, height = 8, scale = 1)




xFINAL$height <- factor(xFINAL$height, levels = c("0.5m", "1m", "1.5m"))
xFINAL$time <- factor(xFINAL$time, levels = c("15m", "30m", "45m", "1hr", "2hr", "4hr", "24hr"))

species = c("Aedes aegyptii", "Aedes taeniorhynchus", "Culex quinquefasciatus", "Anopheles quadrimaculatus" )

released = c(300,300,300,300, 300, 300, 300, 300)

treatment = c("control", "control", "control", "control", "Bayer candle", "Bayer candle", "Bayer candle", "Bayer candle")

treatment <- rep(treatment, 8)
freeFlyer<- tibble(species, released, recaptured,treatment)

recaptured <- c(199,52,213,221,6,1,7,3)

species <- rep(species, 2)


kbl(freeFlyer) %>% 
  kable_classic_2(full_width = F) 

#trying out some functions in lubridate
library(lubridate)

x$date[2] %>% 
  month() 

x %>% 
  filter(grepl("T$|^N", location)) %>% 
  group_by(location) %>% 
  summarize(mean = mean(knockdowncount))

xT<- x %>% 
  filter(grepl("T$", location)) %>%
  mutate(height = "1.5m")

xM<- x %>% 
  filter(grepl("M$", location)) %>%
  mutate(height = "1m")

xB <- x %>% 
  filter(grepl("B$", location)) %>%
  mutate(height = "0.5m")

x01 <- full_join(xT, xM)

xFINAL <- full_join(x01, xB)
transform(x, location = "top")

for (i in 1:length(x)) {
  if (grepl("T", x$location[i])) {
    print(transform(x[i], height = "top"))
  }
  
  
}

x
x %>% 
  group_by(grepl("B", location)) %>% 
  summarise(mean = mean(knockdowncount))

x %>% 
  filter(grepl("^N.*M$", location)) %>% 
  group_by(location) %>% 
  summarize(mean = mean(knockdowncount))
x %>% 
  group_by(location) %>% 
  summarise(mean = mean(knockdowncount)) %>% 
  filter(grepl("^N.*M$", location))



