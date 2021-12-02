#Title: 03_ModelFitting
#Author: Carolyn Wright
#Date: November 2021
#Abstract: this program fits models to predict the star rating for restaurants in Boston


#Read in packages
library("lme4")
library(lattice)
library(gridExtra)

#List of all possible predictor vars:
#BUSINESS LEVEL
  # avg_pos_sent_pct
  # pricerange
  # Relations
  # alcohol_r
  #italian
  # chinese
  # mexican
  # japanese
  # greek
  # thai
  # spanish
  # indian
  # mediterranean
  # review_count_bus
#GROUP LEVEL
  # average_num_reviews
  # Population
  # median_income
  # postal code
# Tourist

### Fit a simple lm model
  # Fit1 <- lm(bus_stars~avg_pos_sent_pct + pricerange + Population + Tourist + Relations + alcohol_r + median_income , data = Yelp_data_final)
  # summary(Fit1)

  # plot(Fit1)
  
  # car::marginalModelPlots(Fit1)
  
  #add location information
  # Fit2 <- lm(bus_stars~avg_pos_sent_pct + pricerange + Population + Tourist + Relations + alcohol_r +  median_income + postal_code, data = Yelp_data_final)
  # summary(Fit2)
    #Result: postal codes 2199,2210 and 2215 are showing -- indicates that they are linearly related to another variable. Try dropping some terms
  
 

  #Remove postal code and keep median_income - good fit!
  # Fit2a <- lm(bus_stars~avg_pos_sent_pct + pricerange + Relations + alcohol_r + median_income , data = Yelp_data_final)
  # summary(Fit2a)
  
  # car::marginalModelPlots(Fit2a)
  
  #Add population and tourist
  # Fit2b <- lm(bus_stars~avg_pos_sent_pct + pricerange + Relations + alcohol_r + median_income + Population +Tourist, data = Yelp_data_final)
  # summary(Fit2b)
  
  # car::marginalModelPlots(Fit2b)
  
  

  
  #Add restaurant type indicators - note: these are the most popular 'ethnic cuisines'
  Yelp_data_final$italian <- as.factor(Yelp_data_final$italian)
  Yelp_data_final$chinese <- as.factor(Yelp_data_final$chinese)
  Yelp_data_final$mexican <- as.factor(Yelp_data_final$mexican)
  Yelp_data_final$japanese <- as.factor(Yelp_data_final$japanese)
  Yelp_data_final$greek <- as.factor(Yelp_data_final$greek)
  Yelp_data_final$spanish <- as.factor(Yelp_data_final$spanish)
  Yelp_data_final$thai <- as.factor(Yelp_data_final$thai)
  Yelp_data_final$indian <- as.factor(Yelp_data_final$indian)
  Yelp_data_final$mediterranean <- as.factor(Yelp_data_final$mediterranean)
  Yelp_data_final$pricerange <- as.factor(Yelp_data_final$pricerange)
  Yelp_data_final$Tourist <- as.factor(Yelp_data_final$Tourist)
  
  
  # Fit2e <- lm(bus_stars~avg_pos_sent_pct + pricerange + Relations + alcohol_r + italian + chinese + mexican + japanese + greek  + thai +
  #               spanish + indian + mediterranean + median_income + Population + Tourist + average_num_reviews, data = Yelp_data_final)
  # summary(Fit2e)
  # 
  # car::marginalModelPlots(Fit2e)
  # 
  
  
  
  #Add interactions and removed alcohol -- no longer see its usefulness in answering the questions
  
  Fit2f <- lm(bus_stars ~ avg_pos_sent_pct + pricerange + Relations  + italian + chinese + mexican + japanese + greek  + thai +
                spanish + indian + mediterranean + median_income + Population + Tourist + average_num_reviews + 
                Tourist*italian +Tourist*chinese + Tourist*mexican + Tourist*japanese + Tourist*greek  + Tourist*thai +
                Tourist*spanish + Tourist*indian + Tourist*mediterranean + pricerange*median_income + Relations*median_income, 
              data = Yelp_data_final)
  summary(Fit2f)
  
  car::marginalModelPlots(Fit2f)
  


##Multilevel Model
  # MultiFit1 <- lmer(bus_stars~avg_pos_sent_pct + pricerange + Relations + alcohol_r  + median_income + 
  #                 Relations*median_income + (1|postal_code), data = Yelp_data_final)

  #rescale vars
  Yelp_data_final$median_income_scaled <- Yelp_data_final$median_income/sd(Yelp_data_final$median_income)
  Yelp_data_final$avg_pos_sent_pct_scaled <- Yelp_data_final$avg_pos_sent_pct/sd(Yelp_data_final$avg_pos_sent_pct)
  Yelp_data_final$Relations_scaled <- Yelp_data_final$Relations/sd(Yelp_data_final$Relations)
  Yelp_data_final$average_num_reviews_scaled <- Yelp_data_final$average_num_reviews/sd(Yelp_data_final$average_num_reviews)
  Yelp_data_final$Population_scaled <- Yelp_data_final$Population/sd(Yelp_data_final$Population)
  
  
  #Basic model
    # MultiFit2 <- lmer(bus_stars~avg_pos_sent_pct_scaled + pricerange + Relations + alcohol_r  + median_income_scaled + 
    #                     Relations*median_income_scaled + (1|postal_code), data = Yelp_data_final)
    # MultiFit2
    # 
    # plot(MultiFit2)
    # coef(MultiFit2)$postal_code
    # ranef(MultiFit2)$postal_code
    # 
    # qqmath(MultiFit2)
    # 
    # 
    # ggplot(data.frame(lev=hatvalues(MultiFit3),pearson=residuals(MultiFit3,type="pearson")),
    #        aes(x=lev,y=pearson)) +
    #   geom_point() +
    #   theme_bw()
  
  #Expand model to match Fit2f and add postal_code as the random intercept
  # MultiFit3 <- lmer(bus_stars ~ avg_pos_sent_pct_scaled + pricerange + Relations_scaled  + italian + chinese + mexican + japanese + greek  + thai +
  #                     spanish + indian + mediterranean + median_income_scaled + Population_scaled + Tourist + average_num_reviews_scaled + 
  #                     Tourist*italian +Tourist*chinese + Tourist*mexican + Tourist*japanese + Tourist*greek  + Tourist*thai +
  #                     Tourist*spanish + Tourist*indian + Tourist*mediterranean + pricerange*median_income_scaled + 
  #                     Relations_scaled*median_income_scaled + (1|postal_code), data = Yelp_data_final)
  # MultiFit3
  # 
  # plot(MultiFit3)
  # coef(MultiFit3)$postal_code
  # ranef(MultiFit3)$postal_code
  # 
  # qqmath(MultiFit3)
  # 
  # 
  # ggplot(data.frame(lev=hatvalues(MultiFit3),pearson=residuals(MultiFit3,type="pearson")),
  #        aes(x=lev,y=pearson)) +
  #   geom_point() +
  #   theme_bw()
  

  #Add tourist as the random slope var instead of including interaction terms - convergence issues
  library(rstanarm)
  library(bayesplot)
   # MultiFit4 <- stan_glmer(bus_stars ~ avg_pos_sent_pct_scaled + pricerange + Relations_scaled  + italian + chinese + mexican + japanese + greek  + thai +
   #                     spanish + indian + mediterranean + median_income_scaled + Population_scaled + Tourist + average_num_reviews_scaled + 
   #                      pricerange*median_income_scaled + Relations_scaled*median_income_scaled + (1+Tourist|postal_code), data = Yelp_data_final)
   # print(MultiFit4)
   # 
   # 
   # fit41 <- pp_check(MultiFit4)
   # 
   #    
   # yrep <- posterior_predict(MultiFit4, nsamples = 1000)
   # y <- Yelp_data_final$bus_stars
   # fit4 <- ppc_violin_grouped(y, yrep,  group= Yelp_data_final$bus_stars,  alpha = 0.1, y_draw = "points", y_size = 1.5)
   # 


   #TO DO:
   # look different ppchecks for discrete outcomes
   
   # density plot, different color by postal code, all in one plot. scale it. potentially do prpoportion to be able to 
   # compare if tourist areas have higher ratings
   
   
   # MultiFit5 <- stan_glmer(bus_stars ~ avg_pos_sent_pct_scaled  + Relations_scaled  + italian + chinese + mexican + japanese + greek  + thai +
   #                           spanish + indian + mediterranean + median_income_scaled + Population_scaled + Tourist + average_num_reviews_scaled + 
   #                            Relations_scaled*median_income_scaled + (1+Tourist|postal_code), data = Yelp_data_final)
   # print(MultiFit5)
   # 
   # fit51 <- pp_check(MultiFit5)
   # 
   # yrep <- posterior_predict(MultiFit5, nsamples = 1000)
   # y <- Yelp_data_final$bus_stars
   # fit5 <- ppc_violin_grouped(y, yrep,  group= Yelp_data_final$bus_stars,  alpha = 0.1, y_draw = "points", y_size = 1.5)
   # 
# require(gridExtra)
   # 
   # grid.arrange(fit4, fit5)
   # grid.arrange(fit41, fit51)
   # 
   # 
   # MultiFit6 <- stan_glmer(bus_stars ~ avg_pos_sent_pct_scaled  + Relations_scaled  + italian + chinese + mexican + japanese + greek  + thai +
   #                           spanish + indian + mediterranean  + Population_scaled + Tourist + average_num_reviews_scaled  + 
   #                           (1+Tourist|postal_code), data = Yelp_data_final)
   # print(MultiFit6)
   # 
   # fit61 <- pp_check(MultiFit6)
   # 
   # yrep <- posterior_predict(MultiFit6, nsamples = 1000)
   # y <- Yelp_data_final$bus_stars
   # fit6 <- ppc_violin_grouped(y, yrep,  group= Yelp_data_final$bus_stars,  alpha = 0.1, y_draw = "points", y_size = 1.5)
   # 
   # grid.arrange(fit4, fit5,fit6)
   # grid.arrange(fit41, fit51, fit61)
   # 
   # 
   # MultiFit7 <- stan_glmer(bus_stars ~ avg_pos_sent_pct_scaled  + Relations_scaled  + italian + chinese + mexican + japanese + greek  + thai +
   #                       spanish + indian + mediterranean  + Population_scaled + Tourist + average_num_reviews_scaled  + 
   #                       (1+Tourist|postal_code), data = Yelp_data_final)
   # print(MultiFit7)
   # 
   #  
   # fit71 <- pp_check(MultiFit7)
   # 
   # yrep <- posterior_predict(MultiFit7, nsamples = 1000)
   # y <- Yelp_data_final$bus_stars
   # fit7 <- ppc_violin_grouped(y, yrep,  group= Yelp_data_final$bus_stars,  alpha = 0.1, y_draw = "points", y_size = 1.5)
   # 
   # 
   # resid<- MultiFit7$residuals
   # 
   # fitted<- MultiFit7$fitted.values
   # 
   # plot(Yelp_data_final$bus_stars, resid, ylab="Residuals", xlab="Star") 
   # abline(0,0)
   # 
   # 
   # plot(Yelp_data_final$bus_stars, fitted, ylab="Predicted Stars", xlab="Actual Stars") 
   
 #Don't use stan_glmer  
   
   MultiFit7.1 <- lmer(bus_stars ~ avg_pos_sent_pct_scaled  + Relations_scaled  + italian + chinese + mexican + japanese + greek  + thai +
                             spanish + indian + mediterranean  + Population_scaled + Tourist + average_num_reviews_scaled  + 
                             (1+Tourist|postal_code), data = Yelp_data_final)
   print(MultiFit7.1)
   
   plot(MultiFit7.1)
   
   coef(MultiFit7.1)$postal_code
   ranef(MultiFit7.1)$postal_code
   
   qqmath(MultiFit7.1)

   ggplot(data.frame(lev=hatvalues(MultiFit7.1),pearson=residuals(MultiFit7.1,type="pearson")),
          aes(x=lev,y=pearson)) +
     geom_point() +
     theme_bw()
   
   

   

   
   
   #Model checking resources
   # https://www.ssc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html