install.packages("dplyr")
library(dplyr)
msleep = read.csv("msleep_ggplot2.csv",header = T)

##1 check number of rows and columns using dim()
dim(msleep) ##83 rows, 11 columns

##2 show a summary of all variables using summary()
summary(msleep)

##3 show first 10 rows using function head()
head(msleep,10)

##4 using dplyr select()
sleepdata = select(msleep,name,sleep_total)
sleepdata1 = select(msleep,-name)
sleepdata2 = select(msleep, name:sleep_total)
sleepdata3 = select(msleep,starts_with("sl"))

##5 using dplyr filter()
sleepdata16h = filter(msleep,sleep_total >= 16)
sleepdata16weight = filter(msleep, sleep_total >=16 ,bodywt >1)
sleepdataorder = filter(msleep,order %in% c("Perissodactyla","Primates"))

##6 using pipe operator %>%
msleep %>%
  select(name,sleep_total) %>%
  head(6)

##7 using arrange
msleep %>% arrange(order) %>% head
msleep %>%
  select(name,order,sleep_total) %>%
  arrange(order,sleep_total) %>%
  filter(sleep_total >= 16)
msleep %>%
  select(name,order,sleep_total) %>%
  arrange(order,desc(sleep_total)) %>%
  filter(sleep_total >= 16)
  
