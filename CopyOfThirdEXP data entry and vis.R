library(tidyverse)
library(kableExtra)


longRawDataSet <- RawDataBayerLantern %>%
  gather(key = "time", value = "knockdowncount", c(-location))


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



x %>% 
  ggplot(aes(distance, knockdowncount)) +
  geom_boxplot() + 
  facet_wrap(~time)
ggsave("goodLanternBoxPlot3rdExp.png", dpi = 300, width = 15, height = 8, scale = 1)




species = c("Aedes aegyptii", "Aedes taeniorhynchus", "Culex quinquefasciatus", "Anopheles quadrimaculatus" )

released = c(300,300,300,300, 300, 300, 300, 300)

treatment = c("control", "control", "control", "control", "Bayer candle", "Bayer candle", "Bayer candle", "Bayer candle")

treatment <- rep(treatment, 8)
freeFlyer<- tibble(species, released, recaptured,treatment)

recaptured <- c(199,52,213,221,6,1,7,3)

species <- rep(species, 2)


kbl(freeFlyer) %>% 
  kable_classic_2(full_width = F) 



