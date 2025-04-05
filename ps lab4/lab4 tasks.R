download.file(url <-"https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv", destfile = "msleep_ggplot2.csv")
data = read.csv("msleep_ggplot2.csv")

#1 sleep_stats
sleep_stats = msleep %>% group_by(vore) %>%
  summarise(
    count = n(),
    mean = mean(sleep_total, na.rm=TRUE),
    sd = sd(sleep_total, na.rm=TRUE),
    range = max(sleep_total,na.rm = TRUE) - min(sleep_total,na.rm = TRUE),
  )

#2 bar plot
ggplot(sleep_stats,aes(x=vore,y=mean)) +geom_bar(stat="identity")
ggplot(sleep_stats,aes(x=vore,y=sd)) +geom_bar(stat="identity")
ggplot(sleep_stats,aes(x=vore,y=range)) +geom_bar(stat="identity")
#insecti group has largest sd. This group only includes 5 observations,
#which do not concentrate around the mean value, resulting in large variance
#the range in insecti group is not as big as that in carni group

#3 histogram plot
ggplot(msleep,aes(sleep_total)) +geom_histogram(bins= 20, na.rm=TRUE)
ggplot(msleep,aes(sleep_total)) +geom_density()

ggplot(msleep,aes(sleep_rem)) +geom_histogram(bins= 20, na.rm=TRUE)
ggplot(msleep,aes(sleep_rem)) +geom_density()

ggplot(msleep,aes(sleep_total)) +geom_histogram(bins=20,na.rm=TRUE,aes(y=..density..),
colour="black",fill="steelblue") +geom_density(alpha=.2,fill="pink")
#note: when imposing density onto histogram, vertical axis should be density
#instead of count (default setting in hist) to make them comparable

#4 
sleep1 = msleep %>% 
  filter(!is.na(brainwt),brainwt < 0.1,!is.na(sleep_rem), !is.na(sleep_total)) %>% 
  mutate(rem_ratio = sleep_rem/sleep_total)
ggplot(sleep1,aes(x=brainwt, y=rem_ratio)) + geom_point() +
  labs(title = "rem_ratio against brainwt" , x= "brainwt" , y="rem_ratio") +
  theme(plot.title = element_text(hjust = 0.5))

#5
ggplot(sleep1,aes(x=brainwt, y=rem_ratio,col=vore)) + geom_point() +
  labs(title = "rem_ratio against brainwt" , x= "brainwt" , y="rem_ratio") +
  theme(plot.title = element_text(hjust = 0.5))

#verify using corr coeff
sleep2 = msleep %>%filter(brainwt < .1)%>%
  mutate(rem_ratio = sleep_rem/sleep_total ) %>%
  select(brainwt,rem_ratio)
cor(na.omit(sleep2))
#corr coeff between 2 var is 0.37 indicating weak linear relationship

#focus on carni group only
sleep3 = msleep %>% filter(brainwt < .1, vore == "carni") %>%
  mutate(rem_ratio = sleep_rem/sleep_total ) %>%
  select(brainwt,rem_ratio)
cor(na.omit(sleep3))
#corr coeff between 2 var is 0.87 indicating strong linear relation for carni mammals