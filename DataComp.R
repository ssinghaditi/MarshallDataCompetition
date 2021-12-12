# What are potential factors that lead to longer journey times for trade between China and US?

library(ggplot2)
library(moderndive)
library(dplyr)
library(tidyr)
library(infer)
library(sqldf)

data <- read.csv("Downloads/FinalDatasetMaersk.csv")

ggplot(data, aes(x = `Final.Port.Of.Discharge.Country`)) +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar()

ggplot(data, aes(x = `Original.Port.Of.Loading.Country`, y = `Estimated.Time.of.Journey`)) +
  #scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point()

ggplot(data, aes(x =`ETD`, y = `ETA`, fill = `Original.Port.Of.Loading.Country`)) +
  #scale_y_log10() +
  #theme(axis.text.x = element_text(angle = 90)) +
  geom_point()

loadingC <- unique(data$Original.Port.Of.Loading.Country)
dischargeC <- unique(data$Final.Port.Of.Discharge.Country)

germany = filter(data, `Original.Port.Of.Loading.Country` == "GERMANY")

### CITY IN GERMANY
ggplot(germany, aes(x = `Original.Port.Of.Loading.City`)) + geom_bar() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90)) 

#GERMANY -> DESTINATION CITY
ggplot(germany, aes(x = `Final.Port.Of.Discharge.Country`)) + geom_bar() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90)) 

germany <- germany[-c(547, 548),]

ggplot(germany, aes(x =`ETA`, y = `Estimated.Time.of.Journey`, fill = `Original.Port.Of.Loading.City`)) +
  #scale_y_log10() +
  #theme(axis.text.x = element_text(angle = 90)) +
  geom_point()


#------------------------------------------------------------------------------
CHN_US <- filter(data, `Original.Port.Of.Loading.Country` == "CHINA" | `Original.Port.Of.Loading.Country` == "UNITED STATES")
CHN_US <- filter(CHN_US, `Final.Port.Of.Discharge.Country` == "CHINA" | `Final.Port.Of.Discharge.Country` == "UNITED STATES")

CHN<- filter(CHN_US, `Original.Port.Of.Loading.Country` == "CHINA" )
USA<- filter(CHN_US, `Original.Port.Of.Loading.Country` == "UNITED STATES" )

CHN_US <- CHN_US %>%
  mutate(Origin_Type_cat = ifelse(`Origin.Type`=="CY", 1, 0))

cor(CHN_US[,c(8,16,21,22)])
cor(CHN[,c(8,16,21,22)])

reg1 <- lm(Estimated.Time.of.Journey~Origin.Type, CHN_US)
get_regression_table(reg1)

reg3 <- lm(Estimated.Time.of.Journey~Equipment.Size, CHN)
get_regression_table(reg3)

reg2 <- lm(Estimated.Time.of.Journey~Origin.Type, CHN)
get_regression_table(reg2)

ultitable <- table(CHN_US$Original.Port.Of.Loading.City, CHN_US$Final.Port.Of.Discharge.City)

View(ultitable)

#------------------------------------------------------------------------------
ggplot(CHN, aes(x = Consignee)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar()

CHN_home <- filter(CHN, Consignee == "HOME DEPOT U.S.A.\\,INC." | Consignee == "HOMEGOODS INC")

CHN <- CHN %>%
  mutate(Consignee_cat = ifelse(Consignee == "HOME DEPOT U.S.A.\\,INC." | Consignee == "HOMEGOODS INC", "1", "0"))
CHN_other <- filter(CHN, Consignee_cat == "0")
ggplot(CHN, aes(x = Consignee_cat)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point()

# journey time
table(CHN$Consignee_cat)
table(CHN$Consignee_cat)
chin_mean_time <- mean(CHN$Estimated.Time.of.Journey)
chin_mean_time_home <- mean(CHN$Consignee_cat == "1")
chin_mean_time_other <- mean(CHN$Consignee_cat == "0")
chin_mean_time_home
chin_mean_time_other
CHN <- CHN %>% 
  mutate(EDT_cat = ifelse(Estimated.Time.of.Journey > chin_mean_time, -1, -2)) #-1 = above mean of journey time


table(CHN$Consignee_cat, CHN$EDT_cat)

# H0: whether consignee is home depot/homegoods or not, it doesn't affect the estimated journey time
# H1: if consignee is home depot/homegoods, it will have a larger estimated journey time

obs_stat <- CHN%>% 
  specify(Estimated.Time.of.Journey~Consignee_cat) %>% 
  calculate(stat = "diff in means", order = c("1", "0"))
null_dist <- CHN %>% 
  specify(Estimated.Time.of.Journey~Consignee_cat) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps=1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

visualize(null_dist) +
  shade_p_value(obs_stat, direction = "greater")

get_p_value(null_dist, obs_stat, direction = "greater")

#############33

CHN_OUT <- CHN %>% 
  filter( `Final.Port.Of.Discharge.Country` == "UNITED STATES")

obs_stat_OUT <- CHN_OUT %>% 
  specify(Estimated.Time.of.Journey~Consignee_cat) %>% 
  calculate(stat = "diff in means", order = c("1", "0"))
null_dist_OUT <- CHN_OUT %>% 
  specify(Estimated.Time.of.Journey~Consignee_cat) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps=1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

get_p_value(null_dist_OUT, obs_stat_OUT, direction = "less") 

##############################

tab <- table(CHN_OUT$Consignee_cat, CHN_OUT$Equipment.Number, CHN_OUT$Equipment.Size, CHN_OUT$Origin_Type_cat)
View(tab)

table(CHN_home$Equipment.Size)
table(CHN_other$Equipment.Size)


obs_stat_m <- CHN %>% 
  specify(EDT_cat~Consignee_cat) %>% 
  calculate(stat = "diff in means", order = c("1", "0"))

null_dist_m <- CHN %>% 
  specify(EDT_cat~Consignee_cat) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps=1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("1", "0"))

visualize(null_dist_m) +
  shade_p_value(obs_stat_m, direction = "greater")

get_p_value(null_dist_m, obs_stat_m, direction = "less")



# sqldf("select count(distinct(x)) from data")


# H0: whether origin type has no effect on edt
# H1: if origin type CAYhas higher EDT than CFS
cor(CHN_home[,c(8,16,21,22)])
cor(CHN_other[,c(8,16,21,22)])

View(CHN_home)

obs_stat_OT <- CHN %>%
  specify(Estimated.Time.of.Journey~Origin.Type) %>% 
  calculate(stat = "diff in means", order = c("CY", "CFS"))

null_dist_OT <- CHN_home %>% 
  specify(Measurement~Origin.Type) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps=1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("CY", "CFS"))

visualize(null_dist_OT) +
  shade_p_value(obs_stat_OT, direction = "greater")

get_p_value(null_dist_OT, obs_stat_OT, direction = "greater")

#############

ggplot(CHN_US, aes(x = `Original.Port.Of.Loading.Country`, y = `Estimated.Time.of.Journey`)) +
  #scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point()
