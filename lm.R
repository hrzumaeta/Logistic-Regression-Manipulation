library (h2o)
library (tidyverse)

h2o.init(ip='localhost', nthreads=-1,
         min_mem_size='10G', max_mem_size='100G')
h2o.removeAll()

df <- mtcars %>% 
  rownames_to_column("type") %>% #Making the rownames the first column, to become our unique identifier for the cars
  mutate(efficient=ifelse(mpg>=25,1,0)) %>% 
  mutate(efficient=as.factor(efficient)) #Important to do this for the H2O function to recognize it as factor and not run into cardinality errors later

##This is the extent of the feature engineering for this model
##We can do more interesting things, but for the purposes of this tutorial this is the bare minimum

df %>% summarize(n())
##We only have 32 observations and we have to split these into training, validation and test datasets... not ideal but we'll work with what we have

data_h2o <- as.h2o(df, row.names = FALSE)
#Split the data randomly, not using the split given by H2O because it's not random#
rnd <- h2o.runif(data_h2o, seed=1000)
training <- data_h2o[rnd<0.70,]
validation <- data_h2o[rnd>=0.70 & rnd<0.85,]
test <- data_h2o[rnd>=0.85 & rnd<1,]

#Here we choose the dependent variable (y) and the independent variables which we separate by specifying the variables which
#will NOT be included#
y <- "efficient"
x <- setdiff(names(data_h2o), c(y,"mpg", "qsec","disp","vs","drat","carb","type"))
print(x)  

glm_model <- h2o.glm(
  training_frame = data_h2o,
  validation_frame=validation,
  x=x, 
  y=y, #make this binomial, so something like efficient
  model_id = "glm_model",  #The model id is set here so you can reference it later
  family='binomial',#This specifies a logistic regression
  nfolds=5
) #Include cross validation, this is good practice, but with so few samples right now, not the most useful#

summary(glm_model)
glm_test_perf <- h2o.performance(model = glm_model, newdata = test)
glm_test_perf
plot(glm_test_perf, type = "roc") #if binomial
h2o.auc(glm_test_perf) #if binomial


h2o.varimp_plot(glm_model)
coefficients <- as.data.frame(glm_model@model$coefficients_table)
finalRf_predictions <- h2o.predict(object=glm_model, newdata=test)

##Can't use tidyverse on H20 frames, so will have to use base R functions##
test$predict <- finalRf_predictions$predict
test$p0 <- finalRf_predictions$p0
test$p1 <- finalRf_predictions$p1
test$goodpredict <- 0
test$goodpredict[(test$predict)==(test$efficient)] <- 1
tibbler <- as_tibble(test)

##########################################################
##This is the weight optimization function##
##IOW what is the weight which will give this car a 50% (or whatever other % you choose) chance of being "efficient"##
weightoptimizer <- function(poe=.5, testobs="Mazda RX4"){
  
  testob <- data_h2o[data_h2o$type==testobs,]
  finalRf_predictions <- h2o.predict(object=glm_model, newdata=testob) %>% as.tibble()
  print (paste("Original Prediction:",finalRf_predictions))
  
  probabilityofefficiency <- poe
  
  magicvalue <- as.numeric((as.numeric(filter(coefficients, names=='Intercept')[2])+
                              as.numeric(filter(coefficients, names=='cyl')[2])*as.numeric(testob$cyl)+
                              as.numeric(filter(coefficients, names=='gear')[2])*as.numeric(testob$gear)+
                              as.numeric(filter(coefficients, names=='hp')[2])*as.numeric(testob$hp)+
                              as.numeric(filter(coefficients, names=='am')[2])*as.numeric(testob$am))-
                          (log(probabilityofefficiency/(1-probabilityofefficiency))))/
                          as.numeric(-filter(coefficients, names=='wt')[2]) %>% unlist()
  
  magicvalue <- as.numeric(as.tibble(magicvalue))
  
  testob$wt <- magicvalue
  finalRf_predictions <- h2o.predict(object=glm_model, newdata=testob)
  print (paste("Prediction after weight changes for specified efficiency probability:",finalRf_predictions))
  
  return (magicvalue)
}

weightoptimizer()
