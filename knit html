# Hi, everyone, it would be a preasure for me to present you what we found through the case study project. The purpose of this project is to analyze the data set given to generate the desired answer related that has real world applications.
# I will take a few mintues to answer all the questions that concerns our company, as well as some new discoveries that will giude us throught the next period of our marketing.


library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

totalBeer = read.csv(file.choose(), header = TRUE)
totalBrew = read.csv(file.choose(), header = TRUE)

#1.   How many breweries are present in each state?
totalBrew %>% group_by(State) %>% count(State)

# by running this code we will be having a new data set that tells us the number of breweries in each state
# we can see that AK has 7, AL has 3, AR has 2 and so on.




#2.   Merge beer data with the breweries data. Print the first 6 observations
#and the last six observations to check the merged file.  

dim(totalBeer)
dim(totalBrew)

#merging totalBeer and totalBrew
mergedB = merge(totalBeer, totalBrew, by.x = "Brewery_id", by.y = "Brew_ID")
dim(mergedB)

#1st 6 observations
head(mergedB, n=6)

#Last 6th  observations
tail(mergedB, n = 6)

# by running the code above, we can have the data set merged and names mergedB
# the first 6 observations are:   
#    Brewery_id        Name.x Beer_ID   ABV IBU                               Style Ounces             Name.y        City State
#          1  Get Together    2692 0.045  50                        American IPA     16 NorthGate Brewing  Minneapolis    MN
#          1 Maggie's Leap    2691 0.049  26                  Milk / Sweet Stout     16 NorthGate Brewing  Minneapolis    MN
#          1    Wall's End    2690 0.048  19                   English Brown Ale     16 NorthGate Brewing  Minneapolis    MN
#          1       Pumpion    2689 0.060  38                         Pumpkin Ale     16 NorthGate Brewing  Minneapolis    MN
#          1    Stronghold    2688 0.060  25                     American Porter     16 NorthGate Brewing  Minneapolis    MN
#          1   Parapet ESB    2687 0.056  47 Extra Special / Strong Bitter (ESB)     16 NorthGate Brewing  Minneapolis    MN

# the last 6 observations are:
#.     Brewery_id                    Name.x Beer_ID   ABV IBU                   Style Ounces                        Name.y          City State
#        556             Pilsner Ukiah      98 0.055  NA         German Pilsener     12         Ukiah Brewing Company         Ukiah    CA
#        557  Heinnieweisse Weissebier      52 0.049  NA              Hefeweizen     12       Butternuts Beer and Ale Garrattsville    NY
#        557           Snapperhead IPA      51 0.068  NA            American IPA     12       Butternuts Beer and Ale Garrattsville    NY
#        557         Moo Thunder Stout      50 0.049  NA      Milk / Sweet Stout     12       Butternuts Beer and Ale Garrattsville    NY
#        557         Porkslap Pale Ale      49 0.043  NA American Pale Ale (APA)     12       Butternuts Beer and Ale Garrattsville    NY
#       558 Urban Wilderness Pale Ale      30 0.049  NA        English Pale Ale     12 Sleeping Lady Brewing Company     Anchorage    AK


#3.   Address the missing values in each column.
mergedB = na.omit(mergedB)
dim(mergedB)

# by doing so we removed the rows that contains the na values. it will reduce the number of rows down to 1405 rows



#4.   Compute the median alcohol content (ABV) and international bitterness unit for each state. Plot a bar chart to compare.
#getting the median for Alcohol content
medianABV = median(mergedB$ABV)
#median of bitterness
medianIBU = median(mergedB$IBU)

medFrame = mergedB %>% group_by(State) %>% summarise(medianABV = median(ABV), medianIBU = median(IBU))

#plotting, bar charts
ggplot(medFrame, aes(x = State, y = medianABV)) + geom_bar(stat = 'identity')

ggplot(medFrame, aes(x = State, y = medianIBU)) + geom_bar(stat = 'identity')

#by doing so we will have a clear bar chart for median ABD and median IBU in each state




#5Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?

arrange(medFrame, desc(medianIBU))

arrange(medFrame, desc(medianABV))

# by doing so, we will rearrange the data frame based on IBU and ABV respectively. 
# After the running, we will see that Maine is the state that has both the highest ABV and highet IBU beer




# 6.Comment on the summary statistics and distribution of the ABV variable
ggplot(mergedB, aes(x = ABV)) + geom_histogram()

#Most beers average around 0.052 and average around.Looking at the overall distribution, the data
# is right skewed. Our max for ABV is at 0.12500 and min is 0.02700.





#7.   Is there an apparent relationship between the bitterness of the beer and its alcoholic content?
#Draw a scatter plot.  Make your best judgment of a relationship and EXPLAIN your answer.

ggplot(mergedB, aes(x = ABV, y = IBU)) + geom_point() + geom_jitter() + geom_smooth(se = FALSE)

# after drawing the scatter plot with the code above, we can see a positive correlation relationship between IBU and ABV, 
# there are outliers, but they do not affect the result too much.

#8. 

iterations = 500
numks = 30
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(mergedBall)[1], round(0.75* dim(mergedBall)[1]))
  train = mergedBall[trainIndices,]
  test = mergedBall[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(4,5)], test[,c(4,5)], train$group, prob = TRUE, k =3)
    table(classifications, test$group)
    cm = confusionMatrix(table(classifications, test$group))
    masterAcc[j,i] = cm$overall[1]
  }
}
MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

# with the code above, we can create a KNN model that help us define the ale type being IPA or not. 
# After running the code, we will find out that the best k value is 23, 
# which means if we have an ale, we will compare its ABV and IBU to 23 ales that have the mose similar ABV and IBU, and the majority of their type will be our best guess.
# further more, we found that IPA mostly have higher ABV and IBU, which means we can simply imagine that if an ale has high ABV and IBU, it is very likely to be an IPA



#9
#Based on the findings previously, we can figured out that if, say, we would like to presiute high ABV beer and/ or high IBU beer, we should go for these state:

#These are the top 5 states for ABV
#1 Maine 0.067
#2  Colorado 0.065
#3 Oklahoma.  0.063
#4 Florida 0.062
#5 Georgia 0.062

#These are the top 5 states for IBU
# 1 Maine 61
#2 West Virginia 57.5
#3 Florida  55
#4 Georgia 55 
#5 Delaware 52
# those states have beers with a high median of IBU or ABV. It shows us that people there are more welcoming beers with high ABV or IBU. 
#Therefore, if we would like to have a new type of beer that is high on IBU and/or ABV, we can consider selling it to those states. 

# conclusion
# the data provided is very useful in predicting market and identifiying ale type. All the questions we cared about can be answered clearly and supported with evidence from the data.
# It has the potential of revealing us more about beer, brewies and beer markets. We might have the chance of seeing them in the future
