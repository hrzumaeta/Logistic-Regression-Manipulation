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

dataH2o <- as.h2o(df, row.names = FALSE)
#Split the data randomly, not using the split given by H2O because it's not random#
rnd <- h2o.runif(dataH2o, seed=1000)
training <- dataH2o[rnd<0.70,]
validation <- dataH2o[rnd>=0.70 & rnd<0.85,]
test <- dataH2o[rnd>=0.85 & rnd<1,]

#Here we choose the dependent variable (y) and the independent variables which we separate by specifying the variables which
#will NOT be included#
y <- "efficient"
x <- setdiff(names(dataH2o), c(y,"mpg", "qsec","disp","vs","drat","carb","type"))
print(x)  

glmModel <- h2o.glm(
  training_frame = dataH2o,
  validation_frame=validation,
  x=x, 
  y=y, #make this binomial, so something like efficient
  model_id = "glmModel",  #The model id is set here so you can reference it later
  family='binomial',#This specifies a logistic regression
  nfolds=5
) #Include cross validation, this is good practice, but with so few samples right now, not the most useful#

summary(glmModel)
glmTestPerf <- h2o.performance(model = glmModel, newdata = test)
plot(glmTestPerf, type = "roc") #if binomial
h2o.auc(glmTestPerf) #if binomial


h2o.varimp_plot(glmModel)
coefficients <- as.data.frame(glmModel@model$coefficients_table)
finalRfPredictions <- h2o.predict(object=glmModel, newdata=test)

##Can't use tidyverse on H20 frames, so will have to use base R functions##
test$predict <- finalRfPredictions$predict
test$p0 <- finalRfPredictions$p0
test$p1 <- finalRfPredictions$p1
test$correctPrediction <- 0
test$correctPrediction[(test$predict)==(test$efficient)] <- 1
tibbler <- as_tibble(test)

##########################################################
##This is the weight optimization function##
##IOW what is the weight which will give this car a 50% (or whatever other % you choose) chance of being "efficient"##
weightOptimizer <- function(poe=.5, testObs="Mazda RX4"){
  
  testOb <- dataH2o[dataH2o$type==testObs,]
  finalRfPredictions <- h2o.predict(object=glmModel, newdata=testOb) %>% as.tibble()
  print (paste("Original Prediction:",finalRfPredictions$p1))
  
  probabilityOfEfficiency <- poe
  
  magicValue <- as.numeric((as.numeric(filter(coefficients, names=='Intercept')[2])+
                              as.numeric(filter(coefficients, names=='cyl')[2])*as.numeric(testOb$cyl)+
                              as.numeric(filter(coefficients, names=='gear')[2])*as.numeric(testOb$gear)+
                              as.numeric(filter(coefficients, names=='hp')[2])*as.numeric(testOb$hp)+
                              as.numeric(filter(coefficients, names=='am')[2])*as.numeric(testOb$am))-
                          (log(probabilityOfEfficiency/(1-probabilityOfEfficiency))))/
                          as.numeric(-filter(coefficients, names=='wt')[2]) %>% as.tibble() %>% unlist()
  
  magicValue <- as.numeric(as.tibble(magicValue))

  testOb$wt <- magicValue
  
  finalRfPredictions <- h2o.predict(object=glmModel, newdata=testOb) %>% as.tibble()
  
  print (paste("Prediction after weight changes for specified efficiency probability:",finalRfPredictions$p1))
  
  return (magicValue)
}

weightvalue <- weightOptimizer()
