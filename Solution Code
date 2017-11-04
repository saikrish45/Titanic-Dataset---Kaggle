#To get the location of working directory
getwd() 

#To set the location of working directory
setwd("C:/Users/Sai Krishna/Documents") 

#Loading Training data 
train<-read.csv("C:\\Users\\Sai Krishna\\Desktop\\R\\TitanicTrain.csv",stringsAsFactors = FALSE)

#Loading Test data
test<-read.csv("C:\\Users\\Sai Krishna\\Desktop\\R\\TitanicTest.csv",stringsAsFactors = FALSE)

#Binding Train and Test data
full <- bind_rows(train,test)

#attaching the attributes without explicitly using the dataset name
attach(full)

# Installing required packages 
install.packages('ggplot2')
install.packages('ggthemes')
install.packages('scales')
install.packages('dplyr')
install.packages('mice')
install.packages('randomForest')

# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

################################## Statistical Hypothesis Testing ############################################################

##################################  PassengerId ###############################################################################
#correlation test for contionous(PassengerId) and continous(Survived) 
cor.test(PassengerId,Survived)
#PassengerId will not be taken as a Parameter as Ho is Accepted as p value 0.8814 is > 0.05 (alpha value)

##################################  Sex ####################################################################################
#t-test is used for categorical and Continous attributes where n.o of groups is 2 (for sex - male and female )
t.test(Survived~Sex,data=Titanic)
#Sex will be taken as a Parameter as Ha is Accepted as P value is less than 0.05

##################################  Pclass ####################################################################################
#correlation test for contionous(Pclass) and continous(Survived) 
cor.test(Pclass,Survived)
#Pclass will be taken as a Parameter as Ha is Accepted as p value 2.2e-16 is < 0.05 (alpha value)

##################################  Age ####################################################################################
#correlation test is for Continous(Age) and Continous(Survived) 
cor.test(Age,Survived)
#Age will be taken as a Parameter as Ha is Accepted as p value 0.03912 < 0.05 (alpha value)

##################################  SibSp ####################################################################################
#correlation test for Continous(SibSp) and Continous(Survived) 
cor.test(SibSp,Survived)
#SibSp will be taken as a Parameter as Ha is Accepted as p value 2.2e-16 < 0.05

##################################  Parch ####################################################################################
#correlation test for Continous(Parch) and Continous(Survived) 
cor.test(Parch,Survived)
#Parch  will be taken as a Parameter as Ha is Accepted as p value 0.0148 < 0.05

##################################  Fare ####################################################################################
#correlation test for Continous(Fare) and Continous(Survived) 
cor.test(Fare,Survived)
#Fare will be taken as a Parameter as Ha is Accepted as p value 6.12e-15 < 0.05

##################################  Embarked ####################################################################################
#Anova test for Categorical(Embarked) more than 2 groups  and Continos(Survived) 
Embarked<-as.factor(Embarked)
kruskal.test(Survived~Embarked,data=full) 
#Embarked will be taken as a parameter as Ha is accepted as p value 8.426e-07 is < 0.05

######## Some more attributes are created from the given attributes as per the insight needed ##################################
 
# Grab Title from the name of the passenger
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

table(full$Sex,full$Title)

rare_Title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# This variable appears to have a lot of missing values
full$Cabin[1:30]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

## Sensible value imputation

# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']

# Get rid of our missing passenger IDs
embark_fare <- filter(full,PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044 ; Here Fare is not present for this passenger
full[1044, ]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(45)

# Perform mice imputation, excluding certain less-than-useful attributes: Take those are Ha accepted
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# check - Show new number of missing Age values
sum(is.na(full$Age))

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

## Building the model - Random Forest Implementation ##

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)

# Predict using the test set
prediction <- predict(rf_model, test,type='response')

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'Titanic_Solution_Kaggle.csv', row.names = F)






