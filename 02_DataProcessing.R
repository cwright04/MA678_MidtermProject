#Title: 02_DataProcessing
#Author: Carolyn Wright
#Date: November 2021
#Abstract: this program creates and checks all variables that will be used 
#         in the modeling done in the following program.


###Read in packages
library("tidyverse")
library("magrittr")
library("ggplot2")
library("tidytext")
library("dplyr")
library("stringr")
library("broom")
library("egg")
library("ggplot2")
library("wordcloud2") 

### Read in raw datasets

  # Massachusetts Yelp Restaurant Review data - 
  #   NOTE: this data has been pulled from the yelp academic datasets and has been 
  #         subset using SQL queries and exported to CSV files to easily pull into R
  
  Yelp_data <- read_csv("MA_RESTAURANT_REVIEW.csv")
  
  # Population data: 
  #  https://www.massachusetts-demographics.com/zip_codes_by_population
  pop_counts <- read_csv("Population_per_zip.csv")
  
  
  # Median income data: 
  #   http://zipatlas.com/us/ma/zip-code-comparison/median-household-income.6.htm
  median_income <- read_csv("Median_Income_per_zip.csv")
  median_income %<>% mutate(postal_code = paste0("0",postal_code))
  
  
### Clean up Yelp Data:
  
  # Subset down to just Boston Zipcodes
  #   https://www.usmapguide.com/massachusetts/boston-zip-code-map/
  
  Boston_zips <- c("02108", "02109", "02110", "02111", "02113", "02114", 
                   "02115", "02116", "02118", "02119", "02120", "02121", 
                   "02122", "02124", "02125", "02126", "02127", "02128", 
                   "02129", "02130", "02131", "02132", "02133", "02134", 
                   "02135", "02136", "02163", "02199", "02203", "02210", 
                   "02215","02222")
  Yelp_data %<>% filter(postal_code %in% Boston_zips )
  

  # Remove major grocery stores 0 they will not be considered as restaurants
  Yelp_data %<>% filter(name != "Star Market") %>% filter(name != "Whole Foods Market")
 
  
### Merge on population and income data
  
  # Population
   Yelp_data <- left_join(Yelp_data,pop_counts[,c("Population","postal_code")], by = "postal_code")
   
  # Median
   Yelp_data <- left_join(Yelp_data,median_income, by = "postal_code")

#Dedupilcate yelp_data
   Yelp_data1 <- arrange(Yelp_data, business_id) 
   Yelp_data1$last <- ifelse(Yelp_data1$business_id != lead(Yelp_data1$business_id),1, 0)
   Yelp_data1 <- subset(Yelp_data1[,c("business_id", "name", "postal_code", "Population", "median_income","last", 
                                      "bus_stars","attributes", "categories")],last ==1 |is.na(last) == TRUE)
   
  
### Create Sentiment Score:
   
   tidy_reviews <- Yelp_data %>%
     group_by(business_id, review_id, year) %>%
     ungroup() %>%
     unnest_tokens(word, text)
   
   bing_pos<- get_sentiments("bing") %>% 
     filter(sentiment == "positive")
   
   review_sentiment <- tidy_reviews %>%
     inner_join(get_sentiments("bing")) %>%
     group_by(business_id, review_id, year) %>%
     count(sentiment) %>%
     pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
     mutate(sentiment = positive - negative, pos_sent_pct = 100*(positive/sum(positive,negative)))
   
   
   review_sentiment %<>% group_by(business_id) %>% summarise(avg_sentiment = mean(sentiment), avg_pos_sent_pct = mean(pos_sent_pct))
  
  #merge sentiment scores onto deduplicated data
   Yelp_data1 %<>% inner_join(review_sentiment, by = "business_id")
   
  
### Create Chain indicator
   #Note: all names have been manually cleaned to remove all special characters
   Clean_names <- read.csv("Name_ID_CrossWalk_Clean.csv")
   
   chains <- data.frame(table(Clean_names$name_clean))
   
   chains %<>% rename(name_clean = Var1, Relations = Freq)
   
   
   Yelp_data1 <- left_join(Yelp_data1,Clean_names[,c("business_id","name_clean")], by = "business_id") 
   Yelp_data1$name_clean <- ifelse(is.na(Yelp_data1$name_clean)==TRUE,Yelp_data1$name,Yelp_data1$name_clean)
   
   Yelp_data1 <- left_join(Yelp_data1,chains,by = "name_clean")
   Yelp_data1$Relations <- ifelse(is.na(Yelp_data1$Relations)==TRUE,1,Yelp_data1$Relations)
   

### Create Tourist indicator
   #Zipcodes with major tourist attractions: https://www.brewsandclues.com/bostons-top-10-must-visit-tourist-destinations/
   # - Boston Harbor/Aquarium 02110
   # - The North End 02109, 02110, 02113
   # - Boston Common 02116
   # - Harvard Yard 02138
   # - Fenway 02215
   # - Museum of Fine Arts 02115
   # - Museum of Science 02114
   
   Tourist_Zip <- c("02110","02109", "02113","02116","02138", "02215", "02114")
   
   Yelp_data1$Tourist <- ifelse(Yelp_data1$postal_code %in% Tourist_Zip,1,0)
   
   Yelp_data1$Tourist <- as.factor(Yelp_data1$Tourist)
   

### Create Attributes variables
   
   tidy_attributes <- Yelp_data1 %>%
   group_by(business_id) %>%
      ungroup() %>%
      unnest_tokens(word, attributes)
   
   table(tidy_attributes$word)
   
   #Keep only price range and alcohol attributes:
   tidy_attributes<- subset(tidy_attributes[,c("business_id", "word")], word %in% c("restaurantspricerange2", "byob", "alcohol") | lag(word) %in% c("restaurantspricerange2", "byob", "alcohol"))
   tidy_attributes %<>% mutate(value = lead(word))
   tidy_attributes<- subset(tidy_attributes, word %in% c("restaurantspricerange2", "byob", "alcohol") )
   tidy_attributes <- pivot_wider(tidy_attributes, business_id, names_from = word, values_from = value)

   table(tidy_attributes$restaurantspricerange2)
   table(tidy_attributes$alcohol)
   table(tidy_attributes$byob)
   
   
   #Join with overall yelp_data
   Yelp_data1 <- left_join(Yelp_data1,tidy_attributes[,c("business_id", "alcohol","restaurantspricerange2")], by = "business_id")
   
   #Recodes
   Yelp_data1$pricerange <- ifelse(is.na(Yelp_data1$restaurantspricerange2) == TRUE,99, Yelp_data1$restaurantspricerange2)
   Yelp_data1$alcohol <- ifelse(is.na(Yelp_data1$alcohol) == TRUE,99, Yelp_data1$alcohol)

   table(Yelp_data1$pricerange)
   table(Yelp_data1$alcohol)

   Yelp_data1$pricerange <- as.factor(Yelp_data1$pricerange)
   Yelp_data1$alcohol_r <- ifelse(Yelp_data1$alcohol == "beer_and_wine" | Yelp_data1$alcohol == "u'beer_and_wine",1,
                                       ifelse(Yelp_data1$alcohol == "full_bar" | Yelp_data1$alcohol == "u'full_bar",2,
                                              ifelse(Yelp_data1$alcohol == "none" |Yelp_data1$alcohol == "u'none",0,99)))
   Yelp_data1$alcohol_r <- as.factor(Yelp_data1$alcohol_r)
   
   table(Yelp_data1$pricerange)
   table(Yelp_data1$alcohol,Yelp_data1$alcohol_r)

### Create Restaurant Type variables
   
   tidy_cats <- Yelp_data1 %>%
      group_by(business_id) %>%
      ungroup() %>%
      unnest_tokens(word, categories)
   
   table(tidy_cats$word)
   
   
   tidy_cats<- subset(tidy_cats[,c("business_id", "word")], word %in% c("afghan", "african", "american", "arabian", "armenian", "asian", "australian", "austrian","bangladeshi","basque", "belgian",
   "brazilian", "british", "burmese","cajun", "calabrian", "cambodian", "cantonese", "caribbean","catalan", "chinese", "creole", "cuban", "dominican",
   "eastern", "egyptian", "ethiopian", "european", "greek", "haitian", "hawaiian",
   "himalayan", "honduran","hungarian", "indian", "indonesian", "iranian", "irish", "italian","japanese", "korean", "latin", "kosher", "lebanese",
   "malaysian", "mediterranean", "mex", "mexican", "mongolian", "moroccan", "nepalese", "pakistani", "persian", "peruvian", "portuguese","puerto", "russian",
   "salvadoran", "scottish", "senegalese", "shanghainese","somali", "southern", "spanish",
   "szechuan", "taiwanese", "thai", "trinidadian", "turkish", "tuscan", "ukrainian","venezuelan", "vietnamese"))
    
   tidy_cats$flag <- 1
   
   tidy_cats <- tidy_cats %>% arrange(business_id,word) 
   tidy_cats$dup <-  ifelse(tidy_cats$word == lag(tidy_cats$word) & tidy_cats$business_id == lag(tidy_cats$business_id),1,0)
   tidy_cats <- subset(tidy_cats, dup != 1)
   
   tidy_cats1 <- pivot_wider(tidy_cats, business_id, names_from = word, values_from = flag) 
   
   
   
   #Join with overall yelp_data
   Yelp_data1 <- left_join(Yelp_data1,tidy_cats1, by = "business_id") %>% mutate_if(is.numeric, ~replace(., is.na(.), 2))
   
#Make 1 variable with mulitple levels for type of restaurant
   Yelp_data1$Restaurant_type <-  ifelse(Yelp_data1$italian == 1 & Yelp_data1$chinese != 1 & Yelp_data1$mexican != 1 & 
                                                          Yelp_data1$japanese != 1 & Yelp_data1$greek != 1 & Yelp_data1$thai != 1 & 
                                                          Yelp_data1$spanish != 1 & Yelp_data1$indian != 1 &  Yelp_data1$mediterranean != 1, 1,
                                        ifelse(Yelp_data1$chinese == 1 & Yelp_data1$italian != 1 & Yelp_data1$mexican != 1 & 
                                                  Yelp_data1$japanese != 1 & Yelp_data1$greek != 1 & Yelp_data1$thai != 1 & 
                                                  Yelp_data1$spanish != 1 & Yelp_data1$indian != 1 & Yelp_data1$mediterranean != 1,2,
                                               ifelse(Yelp_data1$mexican == 1 & Yelp_data1$italian != 1 & Yelp_data1$chinese != 1 & 
                                                         Yelp_data1$japanese != 1 & Yelp_data1$greek != 1 & Yelp_data1$thai != 1 & 
                                                         Yelp_data1$spanish != 1 & Yelp_data1$indian != 1 &  Yelp_data1$mediterranean != 1,3,
                                                      ifelse(Yelp_data1$japanese == 1 & Yelp_data1$italian != 1 & Yelp_data1$chinese != 1 & 
                                                                Yelp_data1$mexican != 1 & Yelp_data1$greek != 1 & Yelp_data1$thai != 1 & 
                                                                Yelp_data1$spanish != 1 & Yelp_data1$indian != 1 & Yelp_data1$mediterranean != 1,4,
                                                             ifelse(Yelp_data1$greek == 1 & Yelp_data1$italian != 1 & Yelp_data1$chinese != 1 & 
                                                                       Yelp_data1$mexican != 1 & Yelp_data1$japanese != 1 & Yelp_data1$thai != 1 & 
                                                                       Yelp_data1$spanish != 1 & Yelp_data1$indian != 1 & Yelp_data1$mediterranean != 1,5,
                                                                    ifelse(Yelp_data1$thai == 1 & Yelp_data1$italian != 1 & Yelp_data1$chinese != 1 & 
                                                                              Yelp_data1$mexican != 1 & Yelp_data1$japanese != 1 & Yelp_data1$greek != 1 & 
                                                                              Yelp_data1$spanish != 1 & Yelp_data1$indian != 1 & Yelp_data1$mediterranean != 1,6,
                                                                           ifelse(Yelp_data1$spanish == 1 & Yelp_data1$italian != 1 & Yelp_data1$chinese != 1 & 
                                                                                     Yelp_data1$mexican != 1 & Yelp_data1$japanese != 1 & Yelp_data1$greek != 1 & 
                                                                                     Yelp_data1$thai != 1 & Yelp_data1$indian != 1 & Yelp_data1$mediterranean != 1,7,
                                                                                  ifelse(Yelp_data1$indian == 1 & Yelp_data1$italian != 1 & Yelp_data1$chinese != 1 & 
                                                                                            Yelp_data1$mexican != 1 & Yelp_data1$japanese != 1 & Yelp_data1$greek != 1 & 
                                                                                            Yelp_data1$thai != 1 & Yelp_data1$spanish != 1 & Yelp_data1$mediterranean != 1,8,
                                                                                         ifelse(Yelp_data1$mediterranean == 1 & Yelp_data1$italian != 1 & Yelp_data1$chinese != 1 & 
                                                                                                   Yelp_data1$mexican != 1 & Yelp_data1$japanese != 1 & Yelp_data1$greek != 1 & 
                                                                                                   Yelp_data1$thai != 1 & Yelp_data1$spanish != 1 & Yelp_data1$indian != 1,9,10)))))))))
                                                      
   
### Create average rating by zipcode
   average_star_by_zip <- Yelp_data1 %>% group_by(postal_code) %>% summarise(avg_rating_by_zip = mean(bus_stars))
   
   Yelp_data1 <- inner_join(Yelp_data1,average_star_by_zip, by = "postal_code")
   

   
### Create average number of reviews by zipcode
   review_num_zip <- Yelp_data %>% arrange(.,postal_code) %>% group_by(postal_code) %>% count(postal_code) %>% rename(review_count_zip = n)
   rest_num_zip <- Yelp_data1 %>% group_by(postal_code) %>% count(postal_code) %>% rename(rest_num = n)
   
   Yelp_data1 <- inner_join(Yelp_data1,review_num_zip, by = "postal_code")
   Yelp_data1 <- inner_join(Yelp_data1,rest_num_zip, by = "postal_code") 
   
   Yelp_data1 %<>% mutate(average_num_reviews = review_count_zip/rest_num )
   
   
   

### Keep only necessary variables
   Yelp_data_final <- Yelp_data1[,c("business_id","bus_stars","postal_code","median_income", "Population", "Tourist", 
                                    "avg_pos_sent_pct", "Relations", "pricerange","alcohol_r",  "average_num_reviews",
                                    "italian", "chinese", "mexican", "japanese", "greek", "thai", "spanish", "indian", 
                                    "mediterranean", "Restaurant_type")]
   
   
   
#Cleaned up EDA Plots  

#Income Plot
   # ggplot(Yelp_data1) + geom_point(mapping=aes(x=bus_stars, y = median_income), alpha = .5,  size = 1, position = "jitter") + 
   #    xlab("Stars") + ylab("Median Income") +
   #    ggtitle("Relationship Between Rating and Median Income")
   # 
#Population Plot
   # ggplot(Yelp_data1) + geom_point(mapping=aes(x=bus_stars, y = Population), alpha = .5,  size = 1, position = "jitter") + 
   #    xlab("Stars") + ylab("Population") +
   #    ggtitle("Relationship Between Rating and Population")
   # 
#Tourist Plot
  #  ggplot(Yelp_data1) + geom_point(mapping=aes(x=Tourist, y = bus_stars), alpha = .5,  size = 1, position = "jitter") + 
  #     xlab("Tourist") + ylab("Stars") +
  #     ggtitle("Relationship Between Rating and Area's with Tourist Attractions")  +
  #     theme_bw()
  #  
  # ggplot() +
  #     geom_jitter(aes(x = factor(Tourist), y = bus_stars),
  #                 data = Yelp_data_final,
  #                 width = 0.1) +
  #     xlab("Tourist") + ylab("Stars") +
  #     ggtitle("Relationship Between Rating and Area's with Tourist Attractions")  +
  #     theme_bw()
  #  
   
 
   
#Sentiment Plots
   # 
   # ggplot(Yelp_data1, aes(postal_code, avg_sentiment, fill = bus_stars)) +
   #    geom_col(show.legend = FALSE) +
   #    facet_wrap(~bus_stars, ncol = 2, scales = "free_x") + 
   #    theme(axis.text.x = element_text(size = 8,angle = 45)) + xlab("Postal Code")+ ylab("Sentiment") +
   #    ggtitle("Sentiment Scores by Star Rating")
   # 
   # 
   # ggplot(Yelp_data1) + geom_point(mapping=aes(x=avg_pos_sent_pct, y = bus_stars), alpha = .5,  size = 1, position = "jitter") + 
   #     ylab("Stars") + xlab("Average Sentiment - Percent Positive") +
   #    ggtitle("Relationship Between Rating and Average Postive Sentiment Percent Scores")
   # 

   
   
# Relation Plots
   # Starbucks <- subset(Yelp_data1, name_clean == "Starbucks")
   # 
   # McDonalds <- subset(Yelp_data1, name_clean == "McDonald's")
   # 
   # ggplot(Yelp_data1) + geom_point(mapping=aes(x=Relations, y = bus_stars), alpha = .5,  size = 1, position = "jitter")  + facet_wrap(~postal_code)
   #    xlab("Number of Relations") + ylab("Stars") +
   #    ggtitle("Relationship Between Rating and Number of Relations") 
   # 
   # ggplot(Starbucks) + geom_point(mapping= aes(x = median_income, y = bus_stars), position = "jitter") + ggtitle("Stars by median income - Starbucks")
   # 
   # ggplot(McDonalds) + geom_point(mapping= aes(x = median_income, y = bus_stars), position = "jitter") + ggtitle("Stars by median income - McDonald's")
   # 
   # ggplot(Starbucks) + geom_point(mapping= aes(x = postal_code, y = bus_stars), position = "jitter") + ggtitle("Stars by postal code - Starbucks")
   # 
   # ggplot(McDonalds) + geom_point(mapping= aes(x = postal_code, y = bus_stars), position = "jitter") + ggtitle("Stars by postal code - McDonald's")
   # 
   # ggplot(Starbucks) + geom_point(mapping= aes(x = Tourist, y = bus_stars), position = "jitter") + ggtitle("Stars by Tourist Attraction - Starbucks")
   # 
   # ggplot(McDonalds) + geom_point(mapping= aes(x = Tourist, y = bus_stars), position = "jitter") + ggtitle("Stars by Tourist Attraction - McDonald's")
   # 
   
#Attribute Plots
   # ggplot(Yelp_data1) + geom_point(mapping= aes(x = pricerange, y = bus_stars), position = "jitter") + ggtitle("Stars by Price Range")
   # ggplot(Yelp_data1) + geom_point(mapping= aes(x = alcohol_r, y = bus_stars), position = "jitter") + ggtitle("Stars by Alcohol")
   
   
 #Restaurant Type plots
   # 
   # Italian <- subset(Yelp_data_final, italian ==1)
   # 
   # Italian_avg <- Italian %>% group_by(postal_code) %>% summarise(avg_star = mean(bus_stars)) 
   # names(Italian_avg)[1] = "postal_code"
   # 
   # #http://www.iacc-sgb.com/our-heritage.html
   # 
   # Chinese <- subset(Yelp_data_final, chinese ==1)
   # 
   # Chinese_avg <- Chinese %>% group_by(postal_code) %>% summarise(avg_star = mean(bus_stars)) 
   # names(Chinese_avg)[1] = "postal_code"
   # 
   #    
   # ggplot(subset(Yelp_data_final, italian ==1)) + geom_point(mapping=aes(x=as.factor(postal_code), y = bus_stars)) + 
   #    xlab("Postal Code") + ylab(" Stars") +
   #    ggtitle("Relationship Between Postal Code and Stars for Italian Restaurants - (North End zip = 02113)")+ theme(axis.text.x = element_text(size = 8,angle = 45))
   # 
   # ggplot(Italian_avg) + geom_point(mapping=aes(x=as.factor(postal_code), y = avg_star)) + 
   #    xlab("Postal Code") + ylab(" Stars") +
   #    ggtitle("Relationship Between Postal Code and Average Stars for Italian Restaurants - (North End zip = 02113)") + theme(axis.text.x = element_text(size = 8,angle = 45))
   # 
   # 
   # 
   # ggplot(subset(Yelp_data_final, chinese ==1)) + geom_point(mapping=aes(x=as.factor(postal_code), y = bus_stars)) + 
   #    xlab("Postal Code") + ylab(" Stars") +
   #    ggtitle("Relationship Between Postal Code and Stars for Chinese Restaurants- (Chinatown zip = 02111)") + theme(axis.text.x = element_text(size = 8,angle = 45))
   # 
   # ggplot(Chinese_avg) + geom_point(mapping=aes(x=as.factor(postal_code), y = avg_star)) + 
   #    xlab("Postal Code") + ylab(" Stars") +
   #    ggtitle("Relationship Between Postal Code and Average Stars for Chinese Restaurants - (Chinatown zip = 02111)") + theme(axis.text.x = element_text(size = 8,angle = 45))
   # 
   # 
#Plots of count of reviews;
   # ggplot(Yelp_data_final) + geom_point(mapping=aes(x=review_count_bus, y = bus_stars)) + 
   #    xlab("Number of Reviews a Business Has") + ylab(" Stars") +
   #    ggtitle("Relationship Between the Number of Reviews a Business Has and Stars ")
   # 
   # ggplot(Yelp_data_final) + geom_point(mapping=aes(x=review_count_zip, y = bus_stars)) + 
   #    xlab("Number of Reviews a Zipcode Has") + ylab(" Stars") +
   #    ggtitle("Relationship Between the Number of Reviews a Zip code Has and Stars ")
   
      