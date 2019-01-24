
#Load Data

traindata <- read.csv("train.csv", header = TRUE)

testdata <- read.csv("test.csv", header = TRUE)

#Add Survived variable to the test set to allow for combining of data sets

testdata.survived <- data.frame(Survived =rep("None", nrow(testdata)), testdata[,])

#Combining Data Sets
data.combined <- rbind(traindata, testdata.survived)

#R Datatypes

str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Gross Survival Rates
table(data.combined$Survived)

#Distribution Across Classes of Boarding
table(data.combined$Pclass)

#Loading Visualization Library
library(ggplot2)

#Finding Hypothesis of Rich people survived from the 1st Pclass
traindata$Pclass <- as.factor(traindata$Pclass)
ggplot(traindata, aes(x = Pclass,  fill = factor(Survived)))+
  geom_histogram(stat="count") +
  ggtitle("Pclass Vs Survived")+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")
#First few names in the data set
head(as.character(traindata$Name))
#Last Names in the Data set Name variable name
tail(as.character(traindata$Name))
#Unique names found between the training and Testing data set
length(unique(as.character(data.combined$Name)))
#Checking on two duplicate Names
#Then Storing them as vectors
dupl.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Checking on the records in combined data set
data.combined[which(data.combined$Name %in% dupl.names),]

#What to explicitly know what does the salutations in the records actually mean
#So we start by loading the library into working environment
library(stringr)

#Checking on their correlation with other variables i.e name or Sibsp variables

missTitle <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
missTitle[1:5,]

mrsTitle <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrsTitle[1:5,]

mrTitle <- data.combined[which(str_detect(data.combined$Name, "Mr.")),]
mrTitle[1:5,]

#Cheching on Men with gender
males <- data.combined[which(traindata$Sex == "male"),]
males[1:5,]

#Delving deeper into Variable Correlation by trying to ascertain 3D rshp
#That is relationship between "Survived", "pclass" and now "title"

#Starting with creating a utility function to help with title/ salutation extraction
extractTitles <- function(name) {
  name <- as.character(name)
  if(length(grep("Miss.", name))>0){
    return("Miss.")
  }
  else if(length(grep("Master.", name))>0){
    return("Master")
  }
  else if(length(grep("Mrs.", name))>0){
    return("Mrs.")
  }
  if(length(grep("Mr.", name))>0){
    return("Mr.")
  } else{
    return("Others")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitles(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)
#Grabbing the first 891 records and all columns

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived))+
  geom_bar(stat="count")+
  facet_wrap(~Pclass)+
  ggtitle("Passenger class vs Title and Survived")+
  xlab("Tittle")+
  ylab("Total Count")+
  labs(fill = "Survived")
# Number of people with their titles in combined data set
table(data.combined$Title)
# sex (male and felame) Across training and Test data
table(data.combined$Sex)
#Visual Gender Representation
ggplot(data.combined[1:891,], aes(x= Sex, fill = Survived))+
  geom_bar(stat = "count")+
  ggtitle("Gender Distribution")+
  xlab("Gender/Sex")+
  ylab("Total Count")+
  labs(fill = "Survived")

# sex (male and felame) togethe with Pclass Across training and Test data checking Survived
table(data.combined$Sex, data.combined$Pclass)
#Visual Representation 

ggplot(data.combined[1:891,], aes(x= Sex, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass)+
ggtitle("Gender Distribution in Passenger Classes")+
  xlab("Gender/Sex")+
  ylab("Total Count")+
  labs(fill = "Survived")

#Checking clearly on Age and Sex in depth if the coorelate
#Clearly analysing age distribution among the whole data set

#Converting Age variable to a factor
data.combined$Age <- as.numeric(data.combined$Age)
#data.combined$Age <- as.double(data.combined$Age)
summary(data.combined$Age)

#Deeper Analysis of the 3-way relationship beetwen Age, Sex and Pclass Visually
ggplot(data.combined[1:891,], aes(x= Age, fill = Survived))+
  geom_histogram( binwidth = 0.5)+
  facet_wrap(~Sex + Pclass)+
  ggtitle("3-way Rshp Age, Sex and Passenger class")+
  xlab("Age")+
  ylab("Total Count")


#Time to ascertain "Master" Title equates a young man
youngmen <- data.combined[which(data.combined$Title == "Master"),]
summary(youngmen$Age)

#Ascertain Miss. Title age
missladies <- data.combined[which(data.combined$Title == "Miss."),]
summary(missladies$Age)

ggplot(missladies[missladies$Survived != "None",], aes(x= Age, fill = "Survived"))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass)+
  ggtitle("Miss. Title Ladies Ages in PClasses")+
  xlab("Age")+
  ylab("Total Count")+
  labs("Survived")
  

#Checking the young ladies in miss
missladies.alone <- missladies[which(missladies$SibSp ==0 & missladies$Parch ==0),]
summary(missladies.alone$Age)
length(which(missladies.alone$Age <=14.5))


#On siblings now Summary of Sibsp variable
summary(data.combined$SibSp)

#Getting unique values in Sibsp Variable
length(unique(data.combined$SibSp))

#Converting Sibsp Variable from Int to Factor for better Visualization

data.combined$SibSp <- as.factor(data.combined$SibSp)


ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass & Title")+
  xlab("No. of Siblings")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill = "Survived")

#Having explored the sibling variable its time to Brainstorm on Parent
#Parch

#First Converting it to a Factor
data.combined$Parch <- as.factor(data.combined$Parch)
#Secondly visualizing it
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived))+
  geom_histogram(stat = "count")+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass & Title")+
  xlab("Parents/ Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill = "Survived")

#Brainstorming to create family size if posible By:
#First Concat sibling variables of 2 sets
now.Sibsp <- c(traindata$SibSp, testdata$SibSp)
#Secondly Concat Parch variable of 2 sets
now.Parch <- c(traindata$Parch, testdata$Parch)
#Joining the 2 into one set
data.combined$FamilySize <- as.factor(now.Parch + now.Sibsp + 1)
summary(data.combined$FamilySize)

#Finding Predictives Patterns and visualize

#ggplot(data.combined[1,891,], aes(x= FamilySize, fill = Survived))+
 # geom_bar(width = 0.25)+
 # facet_wrap(~Title)+
  #ggtitle("Pclass & Title")+
  #xlab("Family Size")+
  #ylab("Total Count")+
  #ylim(0,300)+
  #labs(fill = "Survived")







