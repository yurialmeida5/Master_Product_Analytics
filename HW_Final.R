# Load necessary packages

library(data.table)
library(skimr)
library(ggplot2)
library(gganimate)
library(ggiraph)
library(scales)
library(animation)
library(tidyverse)
library(ggrepel)
#install.packages("tidytuesdayR")
library(tidytuesdayR)
library(lubridate)

# Clear Environment 
rm(list = ls())


# Define the graph theme custom function in order to standardize the graph analysis

theme_wealth <- function(){
  
  theme_gray() + 
    theme(plot.caption = element_text(hjust = 0, face = "italic"), 
          plot.title = element_text(hjust = 0.5, color = "blue3", size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          panel.grid.minor = element_blank()) 
  
}


# Load the necessary data for analysis

registration <- data.table(read.csv('D:/CEU/Master_Product_Analytics/Master_Product_Analytics/Data/registrations.csv',
                         header = TRUE, sep = ";"))

activity <- data.table(read.csv('D:/CEU/Master_Product_Analytics/Master_Product_Analytics/Data/activity.csv',
                                    header = TRUE, sep = ";"))


############## Task 1: Acquisition #################

registration <- registration[ , `:=` (month_name = as.character(month(ymd(010101) + months(registration_month - 1),label= TRUE , abbr = TRUE)),
                                      year = ifelse(as.numeric(registration_month) > 12, 'Year2', 'Year1'))]



registration_t1 <- union(registration[, .(number_of_registrations = .N , region = 'World' ), by = .(year , month_name , registration_month)],
                   registration[, .(number_of_registrations = .N ), by = .(year , month_name , registration_month, region)])


registration_t1A <- data.table( growth_rate = ((registration_t1[year == 'Year2' & region == 'World', number_of_registrations]) / 
                                              (registration_t1[year == 'Year1' & region == 'World' & registration_month < 10, number_of_registrations]) - 1),
                                month_number = registration_t1[registration_month < 10 & year == 'Year1' & region == 'World', registration_month],
                                month_abbr = registration_t1[registration_month < 10 & year == 'Year1' & region == 'World', month_name])


registration_t1A$month_abbr <- factor(registration_t1A$month_abbr , levels = unique(registration_t1A$month_abbr))

ggplot(registration_t1A, aes(x = month_abbr, y = growth_rate)) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Monthly Registration Growth Rate', 
       subtitle = 'Registration Growth Rate by Month between Year 1 and Year 2', 
       caption = 'Data source: SaaS Product - Mastering Product Analytics',
       y = 'Growth Rate Year2 / Year1(%)',
       x = 'Month') +
  theme_wealth() +
  theme(legend.position = "none" , axis.text.x = element_text(angle = 90, hjust = 1)) 


ggplot(registration_t1, aes(x = registration_month , y = number_of_registrations , color = region)) + 
  geom_line() + geom_point() + 
  transition_reveal(registration_month) + 
  labs(title = 'Number of Registrations per Month and Region', 
       subtitle = 'Saas application number of registrations split by month and region', 
       caption = 'Data source: SaaS Product - Mastering Product Analytics',
       y = 'Number of Registrations',
       x = 'Year') +
  scale_x_continuous(limits = c(1,21), breaks = seq(1,21, by = 1)) +
  scale_y_continuous(breaks = seq(0,3000, by = 200)) +
  theme_wealth() +
  theme(legend.position = "top" , legend.title = element_blank())


############## Task 2: Activity #################

activity_t2 <- activity[ , `:=` (month_name = as.character(month(ymd(010101) + months(registration_month - 1),label= TRUE , abbr = TRUE)),
                                 year = ifelse(as.numeric(registration_month) > 12, 'Year2', 'Year1'),
                                 row_id = seq.int(nrow(activity)))]

activity_t2 <- setnames(activity_t2,'id','user_id')

# Create user category: New, Retained, Resurrected

activity_t2$user_cat <- NA                

for (i in activity$row_id) {
  
    current_month <- activity_t2$activity_month[i]
    registration_month <- activity_t2$registration_month[i]
    
    if(current_month > registration_month){
      
      ifelse(is.element(activity_t2$user_id[i], activity_t2[activity_month == current_month - 1, user_id]) == TRUE, 
             activity_t2$user_cat[i] <- "Retained",
             activity_t2$user_cat[i] <- "Resurrected")
      
    } else {
      
      activity_t2$user_cat[i] <- "New"
      
    }
    
}
 
activity_t2A <- union(activity_t2[, .(n_of_active_users = .N , region = 'World' ), by = .(year , month_name , registration_month)],
                         activity_t2[, .(n_of_active_users = .N ), by = .(year , month_name , registration_month, region)])

  
ggplot(activity_t2A, aes(x = registration_month , y = n_of_active_users , color = region)) + 
  geom_line() + geom_point() + 
  transition_reveal(registration_month) + 
  labs(title = 'Number of active users per Month and Region', 
       subtitle = 'Saas application percentage of active users by month and region', 
       caption = 'Data source: SaaS Product - Mastering Product Analytics',
       y = 'Number of Registrations',
       x = 'Year') +
  scale_x_continuous(limits = c(1,21), breaks = seq(1,21, by = 1)) +
  scale_y_continuous(breaks = seq(0,6000, by = 500)) +
  theme_wealth() +
  theme(legend.position = "top" , legend.title = element_blank())
  

ggplot(activity_t2A[region != 'World'], aes(x = registration_month , y = n_of_active_users , fill = region)) +
  geom_bar(stat = 'identity' , position = 'fill') +
  labs(title = 'Number of active users per Month and Region', 
       subtitle = 'Saas application number of active users split by month and region', 
       caption = 'Data source: SaaS Product - Mastering Product Analytics',
       y = 'Number of active users (%)',
       x = 'Month') + 
  scale_x_continuous(limits = c(1,21), breaks = seq(1,21, by = 1)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.1) , labels = percent) + 
  theme_wealth() +
  theme(legend.position = "top" , legend.title = element_blank())


activity_t2B <- activity_t2[ , .(n_of_active_users = .N ), by = .(year , activity_month, user_cat )][order(year)]

gh_t2b <- ggplot(activity_t2B, aes(x = activity_month, y = n_of_active_users )) +
  geom_bar(stat = 'identity') +
  theme_wealth() +
  transition_states(user_cat) +
  labs(title = 'Number of users by category and month', 
       subtitle = 'User Category: {closest_state}',
       y = 'Number of users') +
  theme(legend.position = "top" , legend.title = element_blank())

animate(gh_t2b, end_pause = 20, fps = 2)

############## Task 3: Retention #################

activity_t3 <- activity_t2[ , .(n_of_active_users = .N ), by = .(year , activity_month, user_cat )][order(year)]

ggplot(activity_t3[activity_month == 2 | activity_month == 14], aes(x = activity_month , y = n_of_active_users , fill = user_cat)) +
  geom_bar(stat = 'identity' , position = 'fill') +
  labs(title = 'Number of active users per month and category', 
       subtitle = 'Saas application number of active users split by month and category', 
       caption = 'Data source: SaaS Product - Mastering Product Analytics',
       y = 'Number of users (%)',
       x = 'Feb/Year1                                     Feb/Year2') + 
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = seq(0,1, by = 0.1) , labels = percent) + 
  theme_wealth() +
  theme(legend.position = "top" , legend.title = element_blank())


# t3A

ggplot(activity_t3, aes(x = activity_month , y = n_of_active_users , fill = user_cat)) +
  geom_bar(stat = 'identity' , position = 'fill') +
  labs(title = 'Number of active users per month and category', 
       subtitle = 'Saas application number of active users split by month and category', 
       caption = 'Data source: SaaS Product - Mastering Product Analytics',
       y = 'Number of users (%)',
       x = 'Month') + 
  scale_x_continuous(limits = c(1,21), breaks = seq(1,21, by = 1)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.1) , labels = percent) + 
  theme_wealth() +
  theme(legend.position = "top" , legend.title = element_blank())

activity_t2$first_month_user <- ifelse(activity_t2$registration_month == 1, 'Yes', 'No')
activity_t3A <- activity_t2[ , .(n_of_active_users = .N ), by = .(year , activity_month, first_month_user )][order(year)]

ggplot(activity_t3A, aes(x = activity_month , y = n_of_active_users , fill = first_month_user)) +
  geom_bar(stat = 'identity' , position = 'fill') +
  labs(title = 'Number of active users per month and category', 
       subtitle = 'Saas application number of active users split by month and category', 
       caption = 'Data source: SaaS Product - Mastering Product Analytics',
       y = 'Number of users (%)',
       x = 'Month') + 
  scale_x_continuous(limits = c(1,21), breaks = seq(1,21, by = 1)) +
  scale_y_continuous(breaks = seq(0,1, by = 0.1) , labels = percent) + 
  theme_wealth() +
  theme(legend.position = "top" , legend.title = element_blank())




# t3B

activity_t3B <- activity_t2[ activity_month == 2 & user_cat == 'Retained' , .(n_of_active_users = .N ), by = .(year , activity_month, user_cat, operating_system)][order(year)]

ggplot(activity_t3B, aes(x = operating_system , y = n_of_active_users , fill =  operating_system )) +
  geom_bar(stat = 'identity') +
  theme_wealth() +
  labs(title = 'Number of users by category and month', 
       subtitle = 'User Category: {closest_state}',
       y = 'Number of users') +
  theme(legend.position = "top" , legend.title = element_blank())

animate(gh_t2b, end_pause = 20, fps = 2)


