library("tidyverse")
library("magrittr")
library("ggplot2")
library("tidytext")
library("dplyr")
library("stringr")
library("wordcloud2") 

df1 <- read.csv("yelp_dataset/data/MA_RESTAURANT_REVIEW.csv")


######################################## Minor Cleaning ##################################################

#Subset down to just Boston Zipcodes
  #https://www.usmapguide.com/massachusetts/boston-zip-code-map/
  
  Boston_zips <- c(02108, 02109, 02110, 02111, 02113, 02114, 
                   02115, 02116, 02118, 02119, 02120, 02121, 
                   02122, 02124, 02125, 02126, 02127, 02128, 
                   02129, 02130, 02131, 02132, 02133, 02134, 
                   02135, 02136, 02163, 02199, 02203, 02210, 
                   02215,02222)
  df2 <- df1 %>% filter(postal_code %in% Boston_zips )
  
  df2$postal_code_f <- as.factor(df2$postal_code)
  
#Remove major grocery stores 0 they will not be considered as restaurants
  df2 <- df2 %>% filter(name != "Star Market") %>% filter(name != "Whole Foods Market")

#Bring in population data: https://www.massachusetts-demographics.com/zip_codes_by_population
pop_counts <- read.csv("yelp_dataset/data/Population_per_zip.csv")
  
df2 <- left_join(df2,pop_counts[,c("Population","postal_code")], by = "postal_code")

#CHeck for missing values
# table(df2$Population)

#Bring in median income data: http://zipatlas.com/us/ma/zip-code-comparison/median-household-income.6.htm
median_income <- read.csv("yelp_dataset/data/Median_Income_per_zip.csv")

df2 <- left_join(df2,median_income, by = "postal_code")

#CHeck for missing values
# table(df2$median_income)

######################################## Initial Exploration ##################################################
    
#Plot distribution of number of reviews by postal code
ggplot(data = df2) +geom_histogram(mapping = aes(postal_code_f), fill = "tomato",alpha = .5, stat = "count") +
  facet_wrap(~ df2$year, scale = "free_y", ncol = 2) + theme(axis.text.x = element_text(size = 5,angle = 45)) +
  xlab("Postal Code") + ylim(0,8000)

#Plot distribution of average star rating  by postal code
average_star_by_zip <- df2 %>% group_by(year,postal_code_f) %>% summarise(avg_rating = mean(stars))
ggplot(data = average_star_by_zip) + geom_point(mapping = aes(x = postal_code_f, y= avg_rating), color = average_star_by_zip$postal_code_f, pch = 8, size = 5) +
  facet_wrap(~ average_star_by_zip$year, scale = "free_y", ncol = 2) + theme(axis.text.x = element_text(size = 8,angle = 45)) +
  xlab("Postal Code") +ylab("Average Rating") + ylim(3,4.5)

#How important is the number of reviews? 2020 and 2021 have much fewer reviews however their ratings did not differ hugely.

######################################## Investigate Reviews ##################################################

  ##Function pulled from the following link: https://www.kaggle.com/ambarish/a-very-extensive-data-analysis-of-yelp
   star1 <- df2 %>% filter(stars == 1) 
   star2 <- df2 %>% filter(stars == 2) 
   star3 <- df2 %>% filter(stars == 3) 
   star4 <- df2 %>% filter(stars == 4) 
   star5 <- df2 %>% filter(stars == 5) 
   
   
   createWordCloud = function(train, name)
   {
     stars <- train %>%
       unnest_tokens(word, text) %>%
       filter(!word %in% stop_words$word) %>%
       count(word,sort = TRUE) %>%
       ungroup() %>%
       head(100)
      
        wordcloud2(stars,shape = 'star' ,color='random-light', backgroundColor="black",size = .6)
   }
   
   createWordCloud(star1)
   createWordCloud(star2)
   createWordCloud(star3)
   createWordCloud(star4)
   createWordCloud(star5)
   

  
  ##################################### 
   
  tidy_reviews <- df2 %>%
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
    mutate(sentiment = positive - negative)


  stars <- df2[ ,c("business_id","review_id","year", "stars","postal_code_f","bus_stars")]
  
  review_sentiment2 <- inner_join(review_sentiment, stars, by=c("business_id","review_id","year")) %>% 
    mutate(pos_sent_pct = 100*(positive/sum(positive,negative)))

  ggplot(review_sentiment2, aes(postal_code_f, sentiment, fill = stars)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~bus_stars, ncol = 2, scales = "free_x") + 
    theme(axis.text.x = element_text(size = 8,angle = 45)) + xlab("Postal Code")+ ylab("Sentiment") +
    ggtitle("Sentiment Scores by Star Rating")
  

  ggplot(review_sentiment2) + geom_point(mapping=aes(x=bus_stars, y = pos_sent_pct), alpha = .5,  size = 1, position = "jitter", color = review_sentiment2$year) + 
    facet_wrap(~review_sentiment2$year, ncol = 2, scales = "free_x") + xlab("Stars") + ylab("Sentiment - Percent Positive") +
    ggtitle("Relationship Between Rating and Postive Sentiment Pct Scores")
  


######################################## Identify Chains/Relations ##################################################

df2 <- df2[order(df2$business_id),]
df_unique <- subset(df2, business_id != lag(business_id ))

write.csv(df_unique[,c("business_id","name","address","postal_code")],"yelp_dataset/data/Name_ID_CrossWalk.csv", row.names = FALSE)
df_unique_clean <- read.csv("yelp_dataset/data/Name_ID_CrossWalk_Clean.csv")

chains <- data.frame(table(df_unique_clean$name_clean))

chains %<>% filter(Freq>1) %>% rename(name_clean = Var1, Relations = Freq)


df3 <- left_join(df2,df_unique_clean[,c("business_id","name_clean")], by = "business_id") 
df3$name_clean <- ifelse(is.na(df3$name_clean)==TRUE,df3$name,df3$name_clean)

#Check that the above join worked correctly.
test <- df3 %>% filter(is.na(name_clean) == TRUE )

df3 <- inner_join(df3,chains,by = "name_clean")

df3<-df3[,c("business_id", "review_id", "name_clean", "Relations", "stars","year","postal_code","cool","useful","funny","bus_stars", "Population","median_income")]

ggplot(df3) + geom_point(mapping=aes(x=bus_stars, y = log(Relations)), alpha = .5,  size = 1, position = "jitter", color = df3$year) + 
                           facet_wrap(~df3$year, ncol = 2, scales = "free_x") + xlab("Stars") + ylab("Log Relations") +
  ggtitle("Relationship Between Rating and Number of Relations")


#Look at population in relation to how the chain does. Or area where there is a lot of chains. possibly economic?


summary(df3$Relations)

Starbucks <- subset(df3, name_clean == "Starbucks")

McDonalds <- subset(df3, name_clean == "McDonald's")


ggplot(df3) + geom_point(mapping= aes(x = Population, y = stars), size = Starbucks$Tourist) + facet_wrap(~df3$Tourist, ncol = 2, scales = "free_x")

ggplot(Starbucks) + geom_point(mapping= aes(x = Population, y = stars), shape = Starbucks$Tourist, size = 2) 

ggplot(Starbucks) + geom_point(mapping= aes(x = median_income, y = bus_stars)) 

ggplot(McDonalds) + geom_point(mapping= aes(x = median_income, y = bus_stars)) 

ggplot(Starbucks) + geom_point(mapping= aes(x = postal_code, y = bus_stars)) 

ggplot(McDonalds) + geom_point(mapping= aes(x = postal_code, y = bus_stars)) 



######################################## Pull sentiment score onto the overall data frame ##################################################

df3 <- inner_join(df3, review_sentiment2[,c("business_id", "review_id","year", "negative","positive","sentiment","pos_sent_pct")], 
                  by = c("business_id","review_id","year"))

######################################## Investigate Tourist Areas ##################################################

#Zipcodes with major tourist attractions: https://www.brewsandclues.com/bostons-top-10-must-visit-tourist-destinations/
# - Boston Harbor/Aquarium 02110
# - The North End 02109, 02110, 02113
# - Boston Common 02116
# - Harvard Yard 02138
# - Fenway 02215
# - Museum of Fine Arts 02115
# - Museum of Science 02114

Tourist_Zip <- c(02110,02109, 02113,02116,02138, 02215, 02114)

df3$Tourist <- ifelse(df3$postal_code %in% Tourist_Zip,1,0)

df3$Tourist <- as.factor(df3$Tourist)

average_star_by_tourist <- df3 %>% group_by(Tourist,year) %>% summarise(avg_rating = mean(stars))


ggplot(df3) + geom_point(mapping=aes(x=Tourist, y = stars), alpha = .5,  size = 1, position = "jitter", color = df3$Tourist) + 
  ggtitle("Relationship Between Rating and Tourist Zip")

ggplot(average_star_by_tourist) + geom_bar(mapping=aes(x=Tourist,y = avg_rating), stat="identity", alpha = .5, fill = "blue") + facet_wrap(~year)+
  ggtitle("Relationship Between Rating and Tourist Zip") + ylab("Average Rating") + xlab("Tourist Dest. (0- No,1 - Yes)")


######################################## Average Rating of a Zipcode ##################################################
average_star_by_zip <- df2 %>% group_by(postal_code) %>% summarise(avg_rating_by_zip = mean(stars))

df3 <- inner_join(df3,average_star_by_zip, by = "postal_code")




                  