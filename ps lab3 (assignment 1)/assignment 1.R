download.file(url <-"https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv", destfile = "msleep_ggplot2.csv")
data = read.csv("msleep_ggplot2.csv")

#1
data1 = data %>% 
  filter(!is.na(brainwt)) %>%
  select(genus:brainwt,-conservation)

#2 adding variables rem_percentage and cycl_percentage to data1
data1 = data1 %>% mutate(data1,rem_percentage=(sleep_rem/sleep_total)*100,cycl_percentage=(sleep_cycle/sleep_total)*100)

#3 summary statistics
byvore = group_by(data1,vore)
summarise(byvore, num_mammals = n(), avg_rem=mean(rem_percentage,na.rm=TRUE), avg_cycl=mean(cycl_percentage,na.rm=TRUE))
#comparison between vore groups:
#carnivores tend to have highest rem sleep percentage
#herbivores show lowest rem sleep percentage
#insectivores and omnivores have intermediate rem sleep percentage
#sleep cycle percentages also vary, with herbivores having longer cycles compared
#to carnivores and insectivores.

#4 Distributions of cycl_percentage across different vore groups
data1_filtered =filter(data1, !is.na(vore), !is.na(cycl_percentage), is.finite(cycl_percentage))
ggplot(data1_filtered, aes(x=vore, y=cycl_percentage)) +geom_boxplot() + 
  labs(title ="Distributions of cycl_percentage across different
       vore groups", x= "vore groups", y= "cycl_percentage")
#comment on the distribution
#herbivores have the widest spread in cycl_percentage with a few high outliers
#insectivores and carnivores have relatively low cycl_percentage values with with few variations
#omnivores have intermediate spread with a higher cycl_percentage median than herbivores.

#5 relationship between rem_percentage and cycl_percentage.
ggplot(data1_filtered,aes(x=rem_percentage,y=cycl_percentage))+geom_point(col="red", size=0.7)+ 
  labs(title = "relationship between rem_percentage and cycl_percentage", x = "rem_percentage", y="cycl_percentage")
#comment on the relationship
#there is no strong correlation between rem_percentages and cycl_percentages as
#the points are spread out.