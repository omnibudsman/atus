library(tidyverse)
library(foreign)

######### Sources

# data source https://www.bls.gov/tus/data/datafiles-0321.htm
# Activity lexicon https://www.bls.gov/tus/lexicons/lexiconnoex0321.pdf

setwd(Sys.Getenv(ATUS_DIR))

# respondent file 2003-21
atus.resp.raw <- read.delim("atusresp-0321/atusresp_0321.dat", sep=",")
atus.resp <- atus.resp.raw %>% select(TUCASEID, TUYEAR, TRTFRIEND)

# summary file 2003-21
atus.sum.raw <- read.delim("atussum-0321/atussum_0321.dat", sep=",")
atus.sum.raw$computer.leisure <- atus.sum.raw$t120308
atus.sum.raw$socializing <- atus.sum.raw$t120101 + atus.sum.raw$t120199
atus.sum.raw$educ.travel <- atus.sum.raw$t180601 + atus.sum.raw$t180682 + atus.sum.raw$t180699
atus.sum.raw$sleeping <- atus.sum.raw$t010101
atus.sum.raw$homework <- atus.sum.raw$t060301
atus.sum.raw$working <- atus.sum.raw$t050101
atus.sum.raw$work.travel <- atus.sum.raw$t180501
atus.sum.raw$tv.movies <- atus.sum.raw$t120303
atus.sum.raw$reading.personal <- atus.sum.raw$t120312
atus.sum.raw$games <- atus.sum.raw$t120307

atus.raw <- merge(atus.sum.raw, atus.resp, by=c("TUCASEID","TUYEAR"))
atus <- atus.raw %>%
  mutate(age.group = cut(TEAGE, breaks=c(0,18,29,44,65,99), labels=c("14 to 18","19 to 29","30 to 44","45 to 65","66 and up"))) %>%
  mutate(travel.time = rowSums(across(starts_with("t18")))) %>%
  mutate(educ.time = rowSums(across(starts_with("t06")))) %>%
  mutate(caring.time.hh = rowSums(across(starts_with("t03")))) %>%
  mutate(caring.time.non.hh = rowSums(across(starts_with("t04")))) %>%
  mutate(caring.time = caring.time.hh + caring.time.non.hh) %>%
  mutate(day = plyr::mapvalues(TUDIARYDAY, c(1,2,3,4,5,6,7),c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")))

# sums
atus$homework.sleeping.working <- atus$homework + atus$sleeping + atus$working
atus$activities.sum <- atus$educ.time + atus$travel.time + atus$sleeping + atus$working + atus$computer.leisure + atus$games + atus$tv.movies

###### Time spent with friends per day by year

time.by.year <- atus %>% group_by(TUYEAR) %>% summarise(avg.with.friends = mean(TRTFRIEND))
ggplot(time.by.year) + geom_line(aes(x = TUYEAR, y = avg.with.friends))

###### Time spent with friends per day by year, by age group

time.by.age <- atus %>% group_by(TUYEAR, age.group) %>% summarise(avg.with.friends = mean(TRTFRIEND))
ggplot(time.by.age) + geom_line(aes(x = TUYEAR, y = avg.with.friends, color = age.group))

###### Time spent with friends per day by year, by school enrollments
enrollment <- atus %>% subset(TEAGE <= 18) %>% group_by(TUYEAR, TESCHENR) %>% summarise(avg.with.friends = mean(TRTFRIEND))
ggplot(enrollment) + geom_line(aes(x = TUYEAR, y = avg.with.friends, color = as.factor(TESCHENR)))


###### Time spent traveling for education, by age group

travel.by.age <- atus %>% group_by(TUYEAR, age.group) %>% summarise(educ.travel = mean(educ.travel))
ggplot(travel.by.age) + geom_line(aes(x = TUYEAR, y = educ.travel, color = age.group))

###### Time spent traveling for education, by metropolitan status

travel.by.metsta <- atus %>% subset(TEAGE <= 18) %>% group_by(TUYEAR, GTMETSTA) %>% summarise(educ.travel = mean(educ.travel))
ggplot(travel.by.metsta) + geom_line(aes(x = TUYEAR, y = educ.travel, color = as.factor(GTMETSTA)))

###### Time spent doing online leisure, by age group

leisure.by.age <- atus %>% group_by(TUYEAR, age.group) %>% summarise(leisure = mean(computer.leisure))
ggplot(leisure.by.age) + geom_line(aes(x = TUYEAR, y = leisure, color = age.group))

###### Socializing/communicating, by age group

socializing.by.age <- atus %>% group_by(TUYEAR, age.group) %>% summarise(socializing = mean(socializing))
ggplot(socializing.by.age) + geom_line(aes(x = TUYEAR, y = socializing, color = age.group))

###### working, by age group

working.kids <- atus %>% subset(TEAGE <= 18) %>% group_by(TUYEAR, age.group) %>% summarise(working = mean(working))
ggplot(working.kids) + geom_line(aes(x = TUYEAR, y = working))

###### homework, by age group

homework.by.age <- atus %>% group_by(TUYEAR, age.group) %>% summarise(homework = mean(homework))
ggplot(homework.by.age) + geom_line(aes(x = TUYEAR, y = homework, color = age.group))

###### sleeping, by age group

sleeping.by.age <- atus %>% group_by(TUYEAR, age.group) %>% summarise(sleeping = mean(sleeping))
ggplot(sleeping.by.age) + geom_line(aes(x = TUYEAR, y = sleeping, color = age.group))

###### proportion of kids
percent.kids <- atus %>% group_by(TUYEAR) %>% summarise(percent.kids = round(100*mean(age.group == "14 to 18"),2))
ggplot(percent.kids) + geom_line(aes(x = TUYEAR, y = percent.kids))

###### proportion of kids living in a metropolitan area
percent.metro <- atus %>% subset(age.group == "14 to 18" & TUYEAR >= 2005) %>% group_by(TUYEAR) %>% summarise(percent.metro = mean(GTMETSTA == 1))
ggplot(percent.metro) + geom_line(aes(x = TUYEAR, y = percent.metro))

###### labor force status
percent.working <- atus %>% subset(age.group == "14 to 18") %>% group_by(TUYEAR) %>% summarise(percent.working = mean(TELFS == 1))
ggplot(percent.working) + geom_line(aes(x = TUYEAR, y = percent.working)) # identical to working hours!

###### playing games
games <- atus %>% subset(age.group == "14 to 18") %>% group_by(TUYEAR) %>% summarise(games = mean(t120307))
ggplot(games) + geom_line(aes(x = TUYEAR, y = games)) # identical to working hours!

###### travel time
travel <- atus %>% subset(age.group == "14 to 18") %>% group_by(TUYEAR) %>% summarise(travel.time = mean(travel.time))
ggplot(travel) + geom_line(aes(x = TUYEAR, y = travel.time)) # identical to working hours!

###### education time
educ <- atus %>% subset(age.group == "14 to 18") %>% group_by(TUYEAR) %>% summarise(educ.time = mean(educ.time))
ggplot(educ) + geom_line(aes(x = TUYEAR, y = educ.time)) # identical to working hours!

###### percentage in school
in.school <- atus %>% subset(age.group == "14 to 18") %>% group_by(TUYEAR) %>% summarise(in.school = mean(TESCHENR == 1))
ggplot(in.school) + geom_line(aes(x = TUYEAR, y = in.school)) # identical to working hours!

###### caring time
caring <- atus %>% subset(age.group == "14 to 18") %>% group_by(TUYEAR) %>% summarise(caring = mean(caring.time))
ggplot(caring) + geom_line(aes(x = TUYEAR, y = caring)) # identical to working hours!


#################### Analysis - what is most correlated with time spent with friends, for the youth?

# cross years
youth <- atus %>% subset(TEAGE <= 18)
c <- cor(youth)
sort(c['TRTFRIEND',], decreasing = T) # all correlations

# 2021 only
youth.2021 <- youth %>% subset(TUYEAR == 2021)
c.2021 <- cor(youth.2021)
sort(c.2021['TRTFRIEND',], decreasing = T) # all correlations # TELFS - labor force status!

# 2003 only
youth.2003 <- youth %>% subset(TUYEAR == 2003)
c.2003 <- cor(youth.2003)
sort(c.2003['TRTFRIEND',], decreasing = T) # all correlations

#################### Analysis - what are the time-series correlations?

###### correlations (raw time)

find.numeric <- atus[,sapply(atus, is.numeric)]
time.series <- find.numeric %>%
  subset(TEAGE <= 18) %>%
  group_by(TUYEAR) %>%
  summarise_all("mean")

c.time <- cor(time.series)
sort(c.time['TRTFRIEND',], decreasing = T) # all correlations

###### corelations (differences)

lagger <- function(x) { return( x - lag(x, 1))}
time.series.lag <- time.series %>% mutate_all(lag)
time.series.diff <- (time.series - time.series.lag) %>% select(-TUYEAR)

c.diff <- cor(time.series.diff, use="pairwise.complete.obs")
sort(c.diff['TRTFRIEND',], decreasing = T) # all correlations

# 030502 Organization & planning for hh adults

#################### Analysis - What are time differences between 2021 and 2003

time.diff <- time.series %>%
  subset(TUYEAR == 2003 | TUYEAR == 2021) %>%
  pivot_longer(2:ncol(time.series)) %>%
  pivot_wider(id_cols = name, names_from=TUYEAR, names_prefix="year.") %>%
  mutate(year.diff = year.2021 - year.2003) %>%
  arrange(year.diff)

# TRTFRIEND is down by 132 ... what's up by more than, say, 90?

time.diff.extract <- time.diff %>% subset(year.diff >= 20) %>% arrange(desc(year.diff))

#################### Analysis - what are kids just doing MORE of?
sort(c['TUYEAR',], decreasing = F) # all correlations

# GTMETSTA - moving out of cities?


#################### Visualization of tradeoffs (time series)

###### sum of activities
activities.sum <- atus %>% subset(age.group == "14 to 18") %>% group_by(TUYEAR) %>% summarise(activities.sum = mean(activities.sum), time.friends = mean(TRTFRIEND))
ggplot(activities.sum) +
  geom_line(aes(x = TUYEAR, y = activities.sum, color = "Time spent sleeping, working, or doing homework, or online")) +
  geom_line(aes(x = TUYEAR, y = time.friends, color = "Time spent with friends"))

###### sum of activities versus 2003
activities.sum.diff <- activities.sum %>% mutate(friends.versus.2003 = time.friends - mean(youth.2003$TRTFRIEND), activities.versus.2003 = activities.sum - mean(youth.2003$activities.sum))
ggplot(activities.sum.diff) +
  geom_line(aes(x = TUYEAR, y = activities.versus.2003, color = "Time spent sleeping, working, or doing homework, or online")) +
  geom_line(aes(x = TUYEAR, y = friends.versus.2003, color = "Time spent with friends")) +
  geom_hline(yintercept=0, color="black")

#################### 

###### Games diff
games.diff <- atus %>%
  subset(age.group == "14 to 18") %>%
  group_by(TUYEAR) %>%
  summarise(games = mean(t120307), time.friends = mean(TRTFRIEND)) %>%
  mutate(games.diff = games - mean(youth.2003$t120307), friends.diff = time.friends - mean(youth.2003$TRTFRIEND))
  
ggplot(games.diff) +
  geom_line(aes(x = TUYEAR, y = games.diff, color = "Time spent playing games")) +
  geom_line(aes(x = TUYEAR, y = time.friends, color = "Time spent with friends"))

###### Computer leisure diff
computer.diff <- atus %>%
  subset(age.group == "14 to 18") %>%
  group_by(TUYEAR) %>%
  summarise(computer.leisure = mean(computer.leisure), time.friends = mean(TRTFRIEND)) %>%
  mutate(computer.leisure.diff = computer.leisure - mean(youth.2003$computer.leisure), friends.diff = time.friends - mean(youth.2003$TRTFRIEND))

ggplot(computer.diff) +
  geom_line(aes(x = TUYEAR, y = computer.leisure.diff, color = "Time spent on computer leisure")) +
  geom_line(aes(x = TUYEAR, y = friends.diff, color = "Time spent with friends"))

#################### Visualization of tradeoffs (point-in-time)

# 2021
year.2021 <- atus %>% subset(TUYEAR == 2021)
year.2021.young <- year.2021 %>% subset(TEAGE <= 18)

# homework
ggplot(year.2021.young, aes(x = homework, y = TRTFRIEND)) + geom_point() + geom_smooth(method="lm")

# sleeping
ggplot(year.2021.young, aes(x = sleeping, y = TRTFRIEND)) + geom_point() + geom_smooth(method="lm")

# 2003
year.2003 <- atus %>% subset(TUYEAR == 2003)
year.2003.young <- year.2003 %>% subset(TEAGE <= 18)

ggplot(year.2003.young) + geom_point(aes(x = homework, y = TRTFRIEND))

#################### Analysis - Is the drop driven by some kids in particular?

#### Time with friends

# 2003
mean(year.2003.young$TRTFRIEND)
median(year.2003.young$TRTFRIEND)

# 2021
mean(year.2021.young$TRTFRIEND)
median(year.2021.young$TRTFRIEND)

#### Online leisure

# 2003
mean(year.2003.young$computer.leisure)
median(year.2003.young$computer.leisure)

# 2021
mean(year.2021.young$computer.leisure)
median(year.2021.young$computer.leisure)


#################### Analysis: how long were kids spending with their friends each day of the week

youth.diary.days <- atus %>%
  subset(TEAGE <= 18) %>%
  group_by(TUYEAR, TUDIARYDAY, day) %>%
  summarise(friend.hours = mean(TRTFRIEND))

# ... in 2003 vs in 2021
diary.days.years <- youth.diary.days %>% subset(TUYEAR == 2003 | TUYEAR == 2021)

ggplot(diary.days.years) + geom_bar(aes(x = reorder(day, TUDIARYDAY), y = friend.hours, fill=as.factor(TUYEAR)), stat="identity", position="dodge")


#################### Final vizzes

# 1. Friendship decline

ggplot(time.by.year) + 
  geom_line(aes(x = TUYEAR, y = avg.with.friends)) +
  theme_minimal() +
  ylim(0,75) +
  xlab("Year") +
  ylab("Minutes per day spent with friends") +
  ggtitle("Time spent with friends per day: All Americans")

# 2. Friendship decline by age

ggplot(time.by.age) +
  geom_line(aes(x = TUYEAR, y = avg.with.friends, color = age.group)) +
  theme_minimal() +
  ylim(0,200) +
  xlab("Year") +
  guides(color=guide_legend(title="Age")) +
  ylab("Minutes per day spent with friends") +
  ggtitle("Time spent with friends per day: By age")

# 3. Computer leisure

ggplot(computer.diff) +
  geom_line(aes(x = TUYEAR, y = computer.leisure.diff, color = "Time spent on computer leisure")) +
  geom_line(aes(x = TUYEAR, y = friends.diff, color = "Time spent with friends")) +
  theme_minimal() +
  xlab("Year") +
  guides(color=guide_legend(title="")) +
  ylab("Change in average minutes\nper day since 2003") +
  ggtitle("Kids probably don't substitute Facebook for friendship")

# 4. Activity sum
ggplot(activities.sum.diff) +
  geom_line(aes(x = TUYEAR, y = activities.versus.2003, color = "Time spent sleeping, working, traveling,\nin school, doing homework, or online")) +
  geom_line(aes(x = TUYEAR, y = friends.versus.2003, color = "Time spent with friends")) +
  theme_minimal() +
  xlab("Year") +
  guides(color=guide_legend(title="")) +
  ylab("Change in average minutes\nper day since 2003") +
  ggtitle("Kids don't obviously substitute other activities for friendship")

# 5.Diary days
ggplot(diary.days.years) +
  geom_bar(aes(x = reorder(day, TUDIARYDAY), y = friend.hours, fill=as.factor(TUYEAR)), stat="identity", position="dodge") +
  theme_minimal() +
  xlab("Day of the week") +
  guides(fill=guide_legend(title="Year")) +
  ylab("Average time spent with friends") +
  ggtitle("Reported time spent with friends by day of the week\n(14- to 18-year-olds only)")

