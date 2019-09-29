# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 

#change criterion and drift
rwm.test<- random.walk.model(1000, drift = 0.01, sdrw = 0.3, criterion = 4.8)
#get one stable, adjust other
rwm.accuracy<- sum(rwm.test$correct) / length(rwm.test$correct) # should be close to 0.80
rwm.correct <- rwm.test %>% #setting up correct, 250
  filter(correct == TRUE)
rwm.correct.mean<-  mean(rwm.correct$rt)

rwm.incorrect <- rwm.test %>% #setting up incorrect, 246
  filter(correct == FALSE)
rwm.incorrect.mean <-  mean(rwm.incorrect$rt)

#only change rates
  acc.test<- accumulator.model(1000, rate.1 = 83, rate.2 = 89, criterion = 3)
  #lower rate, faster time
  acc.accuracy<- sum(acc.test$correct) / length(acc.test$correct) # should be close to 0.8
  
  acc.1 <- acc.test %>% #250
    filter(correct == TRUE)
  acc.1.mean<-  mean(acc.1$rt)
  
  acc.2 <- acc.test %>%#246
    filter(correct == FALSE)
  acc.2.mean<-  mean(acc.2$rt)


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:
# Both models can get in the ballpark of the ranges we wanted, but aren't exact.



# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.
  
  #The accumulator model creates a much tighter histogram that looks like a nomal distribution.
  # The random walk movel looks much less like a normal distribution and has more frequency of lower rts.
  #I would argue that the accumulator model is a better descriptor of data because of this.
  
  layout(matrix(1:4,nrow=2, byrow = T))
  hist(acc.1$rt, xlim = c(0,1500))
  hist(acc.2$rt, xlim = c(0,1500))
  hist(rwm.correct$rt, xlim = c(0,1500))
  hist(rwm.incorrect$rt, xlim = c(0,1500))
