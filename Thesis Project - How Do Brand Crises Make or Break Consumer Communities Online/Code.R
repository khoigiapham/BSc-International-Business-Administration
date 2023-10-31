library(tidytext)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyr)
library(data.table)
library(lubridate)
library(tidyverse)
library(stargazer)
setwd("D:/OneDrive - Erasmus University Rotterdam/Bachelor 3/Project Analytic/data")

#Data preparation----
#load dataset
crises <- read_excel("Crises-Filtered_yearCompanyProductcrises.xlsx")
team7 <- read_csv("Team7.csv")
text_team7 <- read_csv("Team7_text.csv")
date_subset <- fread("nam_subset.csv", header = F)
#filter data
date_subset <- date_subset %>%
  mutate(V1 = lubridate::mdy(V1))
team7_subset <- team7 %>%
  filter(reference_scandaldate %in% date_subset$V1)
#select variables 
crises_team7 <- crises[,c(-1,-2,-4:-9)]
#remove NAs
text_team7 <- text_team7[complete.cases(text_team7),]
team7_subset <- team7_subset[complete.cases(team7_subset),]
crises_team7 <- crises_team7[complete.cases(crises_team7),]
#changing $reference_scandaldate from team 7 data to match with $news_date from crises
colnames(team7_subset)[colnames(team7_subset)=="reference_scandaldate"]<-"news_date"
crises$news_date <- lubridate::as_date(crises$news_date)
#check data structure
class(crises$news_date)
class(team7_subset$news_date)
str(team7_subset)
str(crises)
#join dataset
#data1 <- left_join(team7_subset %>% group_by(news_date) %>% mutate(id = row_number()),
#          crises_team7 %>% group_by(news_date) %>% mutate(id = row_number()), 
#         by = c("news_date", "id", "company", "subreddit"))

data1 <- left_join(team7_subset, crises_team7, by = c("news_date", "company", "subreddit"))

#SENTIMENT ANALYSIS----
sentiment <- text_team7 %>%
  select(id, selftext) %>% 
  unnest_tokens(., word, selftext) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(id, word, sentiment, sort = TRUE) %>%
  group_by(id, sentiment) %>% 
  summarise(n = sum(n)) %>% 
  pivot_wider(names_from =sentiment, values_from = n, values_fill = 0) %>%
  mutate(netsentiment = positive - negative)
#saveRDS(sentiment, file = "sentiment.rds")
sentiment <- readRDS(file = "sentiment.rds")
#join back to data 1
data2 <- left_join(data1, sentiment, by ="id")
data3 <- data2[complete.cases(data2),]
data_final <- data3[,-c(10,11)]

#Linear Probability Model - Hypotheses testing----
#H1: The occurrence of product-harm crises decrease the probability of of positive user-generated content----
#Graph of main effect
freq <- table(data_final$CR, data_final$UGC)
freq_pct <- prop.table(freq, 1)
freq_graph <- as.data.table(freq_pct)

plotH1 <- ggplot(freq_graph, aes(x=V1, y=N, fill=V2)) +
  geom_bar(stat="identity", position="stack") +
  labs(y="Percentage", x = " ") +
  theme_bw() +
  theme(legend.title = element_blank())

#LPM
lpmH1 <- lm(UGC.n ~ CR, data = data_with_days)
summary(lpmH1)
#COMMENT:
#intercept: 0.6928 => 69.28% of user generated content is positive when there is no brand crises
#B1: -0.0051598 => the occurence of brand crisis decreases the proportion of positive content by 0.5160%

#H2: The greater number of authors' active weeks pre-crisis amplifies the negative influence of product-harm crises on the percentage of positive user-generated content (H1) -----
#Active weeks prior crisis
data_with_days$AD <- ifelse(data_with_days$AD > 0, data_with_days$AD, 0)
data_with_days <- data_with_days %>%
  mutate(AW = AD%/%7)
data_with_days$Aw <- as.factor(data_with_days$Aw)
#Graph of moderation effect 1
freq_mod1 <- table(data_with_days$CR, data_with_days$AW, data_with_days$UGC)
freq_mod1_pct <- prop.table(freq_mod1, c(1, 2))
freq_mod1_graph <- as.data.table(freq_mod1_pct)

plotH2 <- ggplot(freq_mod1_graph[V3=="positive content"],
                 aes(x=as.factor(V2), y=N, group=V1, fill=V1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  labs(y="Percentage of positive content", x = "Number of active weeks prior crisis") +
  theme_bw() +
  theme(legend.title =element_blank()) +
  coord_cartesian(ylim = c(0.65,0.725))

#LPM
lpmH2 <- lm(UGC.n ~ CR*AW, data = data_with_days)
summary(lpmH2)
##COMMENT:
#Intercept = 7.007e-01 captures the impact of no crisis with 0 active weeks, and is significantly equal to 0
#CRcrisis: extra 0% probability of positive sentiment in the occurrence of a crisis, compared to no_crisis condition
#AW: in the 'no crisis' group, each additional active week gives an extra 0% probability to be an positive UGC
#CRcrisis:AW: in the 'crisis' group each additional active week gives an extra 0% probability to be an positive UGC; p-value < 0.05
##AW+CRcrisis:AW = 0+0 = 0 no effect of author's account active week for the 'crisis' group
##p-value > 0.05 H0: there is no moderation effect of 'number of authors' active days pre-crisis' on H1

#

#H3: 
freq_mod2 <- table(data_with_days$CR, data_with_days$reach, data_with_days$UGC)
freq_mod2_pct <- prop.table(freq_mod2, c(1, 2))
freq_mod2_graph <- as.data.table(freq_mod2_pct)
freq_mod2_graph$V2 <- as.factor(freq_mod2_graph$V2)

#Graph of moderation effect 2
plotH2 <- qplot(x=V2,y=N, group=V1, color=V1, 
                data = freq_mod2_graph[V3=="positive content"], geom = "line") + 
  labs(y="Percentage of positive content", x = "Level of media coverage") +  
  theme(legend.title =element_blank())

plotH2 <- ggplot(freq_mod2_graph[V3=="positive content"],
                 aes(x=as.factor(V2), y=N, group=V1, fill=V1)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) +
  labs(y="Percentage of positive content", x = "Level of media coverage") +
  theme_bw() +
  theme(legend.title =element_blank()) +
  coord_cartesian(ylim = c(0.67,0.7))

#LPM
lpmH3 <- lm(UGC.n ~ CR*reach, data = data_with_days)
summary(lpmH3)
#Intercept = 0.6955472 captures the impact of no crisis with 0 active weeks, and is significantly differ from 0
#CRcrisis: minus 0.9% probability of positive sentiment in the occurrence of a crisis, compared to no_crisis condition
#reach: minus 0.1% probability of positive sentiment given by each additional media coverage level, in the no_crisis condition.
#CRcrisis:reach: extra 0.2% probability of positive sentiment given by each additional media coverage level in the occurrence of crisis, compared to no_crisis condition 
##reach+CRcrisis:reach = -0.1%+0.2% = 0.1 => very limited effect of media coverage for the crisis condition

#stargazer----
library(stargazer)
stargazer(lpmH1, lpmH2, lpmH3, 
          type="text",
          column.labels=c("Model for H1", "Model for H2", "Model for H3"),
          digits = 6,
          out = "result.txt")
summary(lpmH1)

#Descriptive stats
DV <- summary(as.factor(data_with_days$UGC.n)) # report percentages
IV_main <- summary(as.factor(data_with_days$CR))
IV_mod1 <- summary(data_with_days$AW)
IV_mod2 <- summary(data_with_days$reach) # categorical also as percentage