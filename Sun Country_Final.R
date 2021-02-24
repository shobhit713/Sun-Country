library(tidyverse)
library(lubridate)
setwd('C:/Users/Shobhit/Desktop/SunCountry/SunCountry_data/')
df <- read.csv('SunCountry.csv', sep = ',')

summary(SunCountry)


sun_raw <- df
sun_raw$Customers = paste(sun_raw$EncryptedName, sun_raw$birthdateid, sep="_")
sun <- sample(sun_raw$Customers, size = 100000)
Customers <- sun_raw$Customers
###########HIDE THIS : NO CODE NO OUTPUT###########################
# write.csv(customers, 'customers.csv')
read.csv('customers.csv')
#################################################################



############################### Data Transformation #########################
sun <- sun_raw %>%  filter(Customers %in% sun)
sun$Customers = paste(sun$EncryptedName, sun$birthdateid, sep="_")

booked_class <- sun %>%
  group_by(Customers, BkdClassOfService) %>%
  summarize(count = n()) %>%
  spread(BkdClassOfService, count, fill =0)
colnames(booked_class) <- c("Customers","b_Coach","b_Discount First Class","b_First Class")


travelled_class <- sun %>%
  group_by(Customers, TrvldClassOfService) %>%
  summarize(count = n()) %>%
  spread(TrvldClassOfService, count, fill =0)
colnames(travelled_class) <- c("Customers","t_Coach","t_Discount First Class","t_First Class")

booking_channel <- sun %>%
  mutate(booking_grouped = ifelse(sun$BookingChannel == 'SCA Website Booking','SCA Website Booking','Rest' ))%>%
  group_by(Customers, booking_grouped) %>%
  summarize(count = n()) %>%
  spread(booking_grouped, count, fill =0)

Ufly <- sun %>%
  mutate(Ufly = ifelse(sun$UflyMemberStatus == 'Elite' | sun$UflyMemberStatus == 'Standard', 1, 0))%>%
  group_by(Customers, Ufly) %>%
  summarize(count = n()) %>%
  spread(Ufly, count, fill =0)

Ufly <- Ufly %>% mutate(ufly_flag = ifelse((Elite + Standard)>0, 1, 0))

layover <- sun %>%
  mutate(layover = ifelse(sun$StopoverCode == 'O', 'less_24_hrs',
                          ifelse(sun$StopoverCode == 'X', 'more_24_hrs','no_stop')))%>%
  group_by(Customers, layover) %>%
  summarize(count = n()) %>%
  spread(layover, count, fill =0)

CardHolder <- sun %>%
  mutate(CardHolder = ifelse(sun$CardHolder == 'true', 'true','false'))%>%
  group_by(Customers, CardHolder) %>%
  summarize(count = n()) %>%
  spread(CardHolder, count, fill =0) %>%
  mutate(CardHolder = ifelse(true > 0, 1,0)) %>%
  select(Customers,CardHolder)

age <- sun %>%
  filter(sun$Age > 0 & sun$Age <= 100)%>%
  group_by(Customers) %>%
  summarize(age = max(Age)) 

gender <- sun %>%
  mutate(gender = ifelse(sun$GenderCode == '', 'U',
                         ifelse(sun$GenderCode == 'F', 'F',
                                ifelse(sun$GenderCode == 'M', 'M' ,
                                       ifelse(sun$GenderCode == 'U', 'U', 'U'))))) %>%
  select(Customers, gender) %>%
  group_by(Customers, gender) %>%
  summarize(count = n()) %>%
  spread(gender, count, fill =0)

avg_total_doc_amt <- sun %>%
  group_by(Customers) %>%
  summarize(avg_amt = mean(TotalDocAmt), total_amt = sum(TotalDocAmt), total_tickets = n()) 

upgrades <- sun %>%
  mutate(upgraded = ifelse(sun$BkdClassOfService == sun$TrvldClassOfService, 0,1))%>%
  group_by(Customers) %>%
  summarize(num_upgrades = sum(upgraded))


early_booked <- sun %>%
  mutate(early_booked = difftime(sun$ServiceStartDate, sun$PNRCreateDate, units = "days"))%>%
  group_by(Customers) %>%
  summarize(avg_difftime = mean(early_booked))

Price_Check <- sun_raw %>% 
  filter(TotalDocAmt != 0) %>%
  group_by(PNRLocatorID, TotalDocAmt) %>%
  summarise(num = n()) %>%
  group_by(PNRLocatorID) %>%
  summarise(avg_doc_price = mean(TotalDocAmt))

Price_customers <- sun_raw %>% 
  filter(TotalDocAmt != 0) %>%
  group_by(PNRLocatorID, Customers) %>%
  summarise(num_customers = n()) %>%
  group_by(PNRLocatorID) %>%
  summarise(num_customers = n())

Customers_PNR <- Price_Check %>%
  left_join(Price_customers, by = 'PNRLocatorID') %>%
  mutate(avg_ticket_price = avg_doc_price/num_customers)%>%
  left_join(sun_raw, by = 'PNRLocatorID') %>%
  select(PNRLocatorID, avg_doc_price, num_customers, avg_ticket_price, Customers) %>%
  group_by(Customers) %>%
  summarise(ticket_price = sum(avg_ticket_price))

customers <- sun %>% distinct(Customers)
SC_final <- merge(customers, age, by = "Customers", )
SC_final <- left_join(customers, age, by = c("Customers") )
SC_final <- left_join(SC_final, avg_total_doc_amt, by = c("Customers") ) 
SC_final <- left_join(SC_final, booked_class, by = c("Customers") ) 
SC_final <- left_join(SC_final, booking_channel, by = c("Customers") ) 
SC_final <- left_join(SC_final, CardHolder, by = c("Customers") ) 
SC_final <- left_join(SC_final, gender, by = c("Customers") ) 
SC_final <- left_join(SC_final, layover, by = c("Customers") ) 
SC_final <- left_join(SC_final, travelled_class, by = c("Customers") ) 
SC_final <- left_join(SC_final, Ufly, by = c("Customers") ) 
SC_final <- left_join(SC_final, upgrades, by = c("Customers") ) 
SC_final <- left_join(SC_final, early_booked, by = c("Customers") )
SC_final <- left_join(SC_final, Customers_PNR, by = c("Customers"))

############# Clustering ########################################

colnames(SC_final)
cols <- c('Customers','age', 'avg_amt', 'total_amt', 'total_tickets', 'SCA Website Booking', 'CardHolder', 'num_upgrades', 'avg_difftime', 'more_24_hrs', "ufly_flag", 'avg_ticket_price')
sun_data <- SC_final[cols]

#### Age Bucketing - Graph - Bucketing them into 4.. Why 0, 1 and 2?
age_income <- SC_final %>% group_by(age) %>% summarise(avg_amt = sum(total_amt))
ggplot(age_income, aes(x = age, y = avg_amt )) + geom_bar(stat = 'identity')  + theme_bw()

normalize <- function(x){
  return ((x - mean(x, na.rm = T))/sd(x, na.rm = T))}

sun_scale <- sun_data %>%
  mutate(age_bucket = ifelse(age<25, 0, ifelse(age<40, 1, ifelse(age<60, 2, 0))), 
         avg_amt = normalize(avg_amt),
         norm_total_amt = normalize(avg_amt),
         total_tickets = normalize(total_tickets),
         bookings = normalize(`SCA Website Booking`),
         upgrades = normalize(num_upgrades),
         difftime = normalize(as.numeric(avg_difftime)),
         more_24 = normalize(more_24_hrs),
         Card = CardHolder) %>%
  select(Customers, age_bucket,norm_total_amt, total_tickets,bookings, Card, upgrades, ufly_flag) %>% 
  na.omit()

set.seed(123)
SSE_curve <- c()
for (k in 1:10) {
  kcluster <- kmeans(sun_scale[-c(1)], k)
  sse <- sum(kcluster$withinss)
  SSE_curve[k] <- sse
}
plot(1:10, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")

## Num Clusters = 4


###### Cluster Profiling
clust_cust <- c(sun_scale$Customers)
SC <- SC_final %>% filter(Customers %in% clust_cust)
SC <- merge(SC, sun_scale, by = "Customers", all.x=TRUE)

Clust <- SC %>%
  rename(Cluster = "kcluster$cluster", SCA_Booking = "SCA Website Booking", total_price = "ticket_price" ) %>%
  mutate(elite_flag = ifelse(Elite>0,1,0), 
         standard_flag = ifelse(Standard>0,1,0),
         male_flag = ifelse(M>0, 1, 0),
         female_flag = ifelse(`F`>0, 1, 0),
         price_per_ticket = total_price/total_tickets.x,
         SCA_flag = ifelse(SCA_Booking>0, 1, 0)
  ) %>%
  group_by(Cluster) %>%
  summarise(Cust_num = n(),
            med_age = median(age),
            how_early = mean(avg_difftime),
            avg_upgrades = mean(num_upgrades),
            perc_CardHolder = mean(CardHolder),
            avg_ticket_price = mean(price_per_ticket, na.rm = T),
            Total_amt = sum(total_price, na.rm = T),
            SCA_Booking = sum(SCA_Booking),
            SCA_Booking_cnt = sum(SCA_flag), 
            Other_Booking = sum(Rest),
            Female = sum(`F`),
            Male = sum(M),
            Female = sum(male_flag),
            Male = sum(female_flag),
            elite = sum(Elite),
            standard = sum(Standard),
            elite_cnt = sum(elite_flag),
            standard_cnt = sum(standard_flag),
            Ufly = sum(ufly_flag.x)
  ) %>%
  mutate(other_perc = Other_Booking / (SCA_Booking + Other_Booking))


