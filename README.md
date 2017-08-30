---
title: "Analysis of White Wine Dataset: Investigating Factors Contributing to the Taste of White Wines"
author: "Woody Yao"
date: "2017/3/7"
output: html_document
---
##Understanding Dataset and Objective

The wine industry is a lucrative industry which is growing as social drinking is on rise. There are many factors that make the taste and quality of wine unique. These factors are but now limited to the followings:

acidity
pH level
sugar remaining in wine
chlorides
density

In this project we use a dataset of wines. In this dataset there are 4898 observations of White Wines that are produced in Portugal. Different properties of each wine is tested and collected for this dataset. Also, Each variety of wine is tasted by three independent tasters and the final rank assigned is the median rank given by the tasters.

In this project, I try to understand this dataset better and also try to find out if there is a relationship between quality of wine and its different properties.

```{r}
# Read the csv file, as well as summary of the data.
getwd() #check the route
wwd <- read.csv('wineQualityWhites.csv') #read the csv file
str(wwd) #check the structure
summary(wwd) #show the summary

#global options
#For future reference, note that you can use global options (instead of specifying in each chunk) to 
#suppress code, warnings, and messages output by R so that it doesn't appear in your knit HTML 
#file with the following:

# {r global_options, include=FALSE}
# knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
```
There are 4898 observations and 12 features. Input variables which includes 11 chemical features of white wine and output variable is wine quality.

Below is brief description of each feature input variables (based on physicochemical tests):

####Chemical properties:

fixed acidity: most acids involved with wine are fixed or nonvolatile (do not evaporate readily) (tartaric acid - g / dm^3)

volatile acidity: the amount of acetic acid in wine, which at too high of levels can lead to an unpleasant, vinegar taste (acetic acid - g / dm^3)

citric acid: found in small quantities, citric acid can add 'freshness' and flavor to wines (g / dm^3)

residual sugar: the amount of sugar remaining after fermentation stops (g / dm^3)

chlorides: the amount of salt in the wine (sodium chloride - g / dm^3

free sulfur dioxide: he free form of SO2 that exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion (mg / dm^3)

total sulfur dioxide: amount of free and bound forms of SO2 (mg / dm^3)

density: the density of water is close to that of water depending on the percentage of alcohol and sugar content (g / cm^3)

pH: describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic)

sulphates: a wine additive which can contribute to sulfur dioxide gas (SO2) levels (potassium sulphate - g / dm3)

alcohol: the percentage of alcohol content in the wine (% by volume)

Output variable (based on sensory data):

quality (score between 0 and 10)

The summary figure above shows the distribution of data over different variables. As we can see, the normal range for fixed acidity is 6.3 to 7.3 g / dm^3. As for sugar, 75% of wines in our dataset have below 9.9 mg / dm^3 sugar remaining after fermentation stops. Average alcohol percentage in our dataset is about 10.51, and average density is about 0.9940.

```{r echo=FALSE, message=FALSE}
#Import required libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(GGally)
library(gridExtra)
library(memisc)
library(scales)
library(nnet)
library(RColorBrewer)
```


```{r message=FALSE}
#First check the quality distribution of the white wines
#Draw boxplot and the scatter plot to see the distribution
b_g_q <- ggplot(aes(x = 'Quality', y = quality), data = wwd) +
  geom_boxplot() 
#  geom_jitter(alpha = 0.5, size = 0.5, color = 'pink')
b_b_q <- ggplot(aes(x = quality), data = wwd) +
  geom_histogram() + stat_bin(binwidth = 1)
Q_box <- grid.arrange(b_g_q, b_b_q, nrow = 1)
ggsave('Q_box.jpg', Q_box)
```

As we can see from the boxplot and scatter plot, most white wines are rated from 5 to 7, some are rated at 4, 3 is extremely low, and some excellent wines are rated at 8 or above.

Next I would like to check the acidity

```{r echo=FALSE, message=FALSE, warning=FALSE}
p1 = ggplot(data = wwd, aes(x = wwd$fixed.acidity)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Fixed Acidity')  +
  geom_vline(xintercept = mean(wwd$fixed.acidity), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$fixed.acidity, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$fixed.acidity, 0.25), color = 'yellow', linetype = 3)
#Distribution of fixed acidity
p2 = ggplot(data = wwd, aes(x = wwd$volatile.acidity)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Volatile Acidity')  +
  geom_vline(xintercept = mean(wwd$volatile.acidity), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$volatile.acidity, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$volatile.acidity, 0.25), color = 'yellow', linetype = 3)
#Distribution of volotile acidity
p3 = ggplot(data = wwd, aes(x = wwd$citric.acid)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Citric Acid')  +
  geom_vline(xintercept = mean(wwd$citric.acid), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$citric.acid, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$citric.acid, 0.25), color = 'yellow', linetype = 3)
#Distribution of citric acid
p4 = ggplot(data = wwd, aes(x = wwd$pH)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('pH')  +
  geom_vline(xintercept = mean(wwd$pH), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$pH, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$pH, 0.25), color = 'yellow', linetype = 3)
#Distribution of pH

p1_4 <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2) #combine the plots above
ggsave('Acid_bar.jpg', p1_4) #Save the grid.arrange image
```

Based on the bottom-right figure, wines are acidic and their pH range from 2.72 to 3.82 according to the summary of our database; however, most wine have a pH between 3 and 3.5.

The acidic nature of wines can come from three different types of acids:

Fixed acidity which is for most cases between 6 and 8.
Volatile Acidity which is mostly in range of 0.1 and 0.5
Citric Acidity which is ranging from 0 to 1 but for most of wines in our dataset is between 0.2 and 0.5

These features all seem to follow a normal distribution except Volatile Acidity which is slightly right skewed.

I will do some transformations and compare which one of the results would be more bell-shaped:

```{r echo=FALSE, message=FALSE, warning=FALSE}
#Try to modify the skewed plot
#Add functions of squareroot and cuberoot
squareroot_trans = function() trans_new('squareroot', transform = function(x) x^(1/2),
                                      inverse = function(x) x^2)
cuberoot_trans = function() trans_new('cuberoot', transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)
applied_values <- list()
seqfun = function(x) {applied_values <- c()
for (i in 1:10){applied_values[i] = max(x)*0.1*i}
return(applied_values)
}
quantilefun = function(x) {applied_values <- c()
for (i in 1:5){applied_values[i] = quantile(x, 0.2*i)}
return(applied_values)
}
#creat a function to add x intercepts easily
p21 <- ggplot(aes(x = volatile.acidity), data = wwd) +
  geom_histogram() + xlab('Volatile Acidity Log10') +
  scale_x_continuous(trans = log10_trans(), breaks = seqfun(wwd$volatile.acidity)) #adjust with log10
p22 <- ggplot(aes(x = volatile.acidity), data = wwd) +
  geom_histogram() + xlab('Volatile Acidity Squareroot') +
  scale_x_continuous(trans = squareroot_trans(), breaks = seqfun(wwd$volatile.acidity)) 
#adjust with square root
p23 <-ggplot(aes(x = volatile.acidity), data = wwd) +
  geom_histogram() + xlab('Volatile Acidity Cuberoot') +
  scale_x_continuous(trans = cuberoot_trans(), breaks = seqfun(wwd$volatile.acidity)) 
#adjust with cuber root
p21_23 <- grid.arrange(p21, p22, p23, ncol = 1) #combine the plots above

ggsave('V_A.jpg', p21_23) #save
```

It seems that square root (volatile acidity) follows normal distribution (at least it is more bell-shaped than before and the other two adjustments); therefore we will use the square root transformation for our further analysis.


Second: Take a look at another 4 variables

```{r echo=FALSE, message=FALSE, warning=FALSE}
?stat_bin
p5 = ggplot(data = wwd, aes(x = residual.sugar)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Residual Sugar') + 
  geom_vline(xintercept = mean(wwd$residual.sugar), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$residual.sugar, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$residual.sugar, 0.25), color = 'yellow', linetype = 3) 
#Distribution of sugar
p6 = ggplot(data = wwd, aes(x = chlorides)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Chlorides') + 
  geom_vline(xintercept = mean(wwd$chlorides), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$chlorides, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$chlorides, 0.25), color = 'yellow', linetype = 3) 
#Distribution of chlorides
p7 = ggplot(data = wwd, aes(x = density)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Density') + 
  geom_vline(xintercept = mean(wwd$density), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$density, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$density, 0.25), color = 'yellow', linetype = 3) 
#Distribution of density
p8 =  ggplot(data = wwd, aes(x = alcohol)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Alcohol') + 
  geom_vline(xintercept = mean(wwd$alcohol), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$alcohol, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$alcohol, 0.25), color = 'yellow', linetype = 3) 
#Distribution of alcohol

p5_8 <- grid.arrange(p5, p6, p7, p8, nrow = 2, ncol = 2) #combine the plots above
ggsave('R~A.jpg', p5_8) #save
```

Based on the above figures, the amount of sugar remaining after fermentation is rarely more than 20 g/dm^3. 

The chlorides range in wines in our dataset is usually between 0 and 0.1 with some exceptions of more than 0.1 g/dm^3.

Density for wine is typically less than water but very slightly. The typical range for density would be (0.99, 1)

Alcohol percentage in wine varies between 8 and 14; however, for most of the wines it is between 9 and 13.

Besides, the distributions of residual sugar and alcohol are right-skewed.


```{r echo=FALSE, message=FALSE, warning=FALSE}
#Remove outliers from the distribution
p51 = ggplot(data = wwd, aes(x = residual.sugar)) +
  geom_histogram() + xlab('Residual Sugar') + stat_bin(binwidth = 0.5) +
  xlim(c(0, quantile(wwd$residual.sugar, 0.99))) #remove the outliers of sugar
p61 = ggplot(data = wwd, aes(x = chlorides)) +
  geom_histogram() + xlab('Chlorides') + stat_bin(binwidth = 0.005) +
  xlim(c(min(wwd$chlorides), quantile(wwd$chlorides, 0.99))) #remove the outliers of chlorides

p51_61 <- grid.arrange(p51, p61, ncol = 1) #combined plot
ggsave('R~C_adj.jpg', p51_61) #save
```

Now that we can see the distribution more clearly, next I would like to try transforming the data to make it more bell-shaped.

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=7}
p52 <- ggplot(aes(x = residual.sugar), data = wwd) +
  geom_histogram() + xlab('Residual Sugar Cuberoot') +
  scale_x_continuous(trans = log10_trans(), breaks = quantilefun(wwd$residual.sugar)) +
  theme(axis.text=element_text(size=8))
#adjust with log10
p62 <- ggplot(data = wwd, aes(x = chlorides)) +
  geom_histogram() + xlab('Chlorides Cuberoot') + stat_bin(binwidth = 0.005) +
  scale_x_continuous(trans = cuberoot_trans(), 
                     limits = c(min(wwd$chlorides), quantile(wwd$chlorides, 0.99)),
                     breaks = quantilefun(wwd$chlorides))  +
  theme(axis.text=element_text(size=4))
#adjust with cuber root
p72 <- ggplot(data = wwd, aes(x = density)) +
  geom_histogram() + xlab('Density Log10') + stat_bin(binwidth = 0.0005) +
  scale_x_continuous(trans = log10_trans(), 
                     limits = c(min(wwd$density), quantile(wwd$density, 0.999)),
                     breaks = quantilefun(wwd$density))  +
  theme(axis.text=element_text(size=5)) #adjust with log10
p82 <- ggplot(data = wwd, aes(x = alcohol)) +
  geom_histogram() + xlab('Alcohol Log10') + stat_bin(binwidth = 0.02) +
  scale_x_continuous(trans = log10_trans(), breaks = quantilefun(wwd$alcohol))  +
  theme(axis.text=element_text(size=8))#adjust with log10

p52_82 <- grid.arrange(p52, p62, p72, p82, nrow = 2, ncol = 2) #combine plots
ggsave('R~A_transformation.jpg', p52_82) #save
```

Now chlorides and density are more like bell-shaped. However, alcohol is still a little right-skewed even though it is adjusted by log function. Residual sugar is far from normal distribution as shown above; it looks more like two different bells in the distribution.


Finally, the other 3 variables: sulphate, free sulfur dioxide, and total sulfur dioxide

```{r echo=FALSE, message=FALSE, warning=FALSE}
p9 = ggplot(data = wwd, aes(x = sulphates)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Sulphate') + 
  geom_vline(xintercept = mean(wwd$sulphates), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$sulphates, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$sulphates, 0.25), color = 'yellow', linetype = 3) 
#Distribution of sulphate
p10 = ggplot(data = wwd, aes(x = free.sulfur.dioxide)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Free Sulfur Dioxide') + 
  geom_vline(xintercept = mean(wwd$free.sulfur.dioxide), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$free.sulfur.dioxide, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$free.sulfur.dioxide, 0.25), color = 'yellow', linetype = 3) 
#Distribution of free SO2
p11 =  ggplot(data = wwd, aes(x = total.sulfur.dioxide)) +
  geom_histogram(fill = 'brown', alpha = 0.5) + xlab('Total Sulfur Dioxide') + 
  geom_vline(xintercept = mean(wwd$total.sulfur.dioxide), color = 'red', linetype = 2) +
  geom_vline(xintercept = quantile(wwd$total.sulfur.dioxide, 0.75), color = 'yellow', linetype = 3) +
  geom_vline(xintercept = quantile(wwd$total.sulfur.dioxide, 0.25), color = 'yellow', linetype = 3) 
#Distribution of total SO2

p9_11 <- grid.arrange(p9, p10, p11, nrow = 1) #combine plots
ggsave('S~SO2.jpg', p9_11) #save
```

From the graph we can see that except for some outliers, free sulfur dioxide and total sulfur dioxide look like bell-shaped, and sulphate is a little right-skewed.

```{r echo=FALSE, message=FALSE, warning=FALSE}
p91 = ggplot(data = wwd, aes(x = sulphates)) +
  geom_histogram() + xlab('Sulphate Cuberoot') + stat_bin(binwidth = 0.02) +
  scale_x_continuous(trans = cuberoot_trans()) #adjust with cuber root
p91
ggsave('Sulphate_adj.jpg', p91)
```

Now it’s more bell-shaped.


After having a basic sense about the variables, I proceed to check the correlation between the input variables in our dataset:

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=15, fig.height = 10}
wwd_tmp <- wwd[,2:12] #set a new dataset with less variables
wwd_tmp$quality <- factor(wwd$quality) #turn the data structure into factor
lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(alpha=0.05, size=0.2) +
    geom_smooth(method = 'lm', size = 0.2, alpha = 0.6)
  p
} #add function to make the plot prettier
ggpairs(wwd_tmp, lower = list(continuous = wrap(lowerFn)), 
        upper = list(continuous = wrap("cor", size = 4))) +
  theme(axis.text=element_text(size=5))
#show the correlations of paired variables

ggsave('simCor.jpg') #save

# with names (another way!)
#exclude <- c("X", "quality")
#keep <- !names(wine) %in% exclude
#summary(wine[,keep])
```

Some observations are listed below:

Strong positive relationship between density and sugar remaining (0.839)

Positive relationship between total SO2 and free SO2 (0.616)

Negative relationship between alcohol and density (-0.78)

Features in our data seems to follow a normal distribution

To avoid multicollinearity in model building using regressions, we have to be aware of strong correlations among input variables.

After knowing that there are some variables related to others, I want to pick them up and add the quality factors to see if the correlations are different between various quality factors:

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=8.5, fig.height = 7}
#Simpler way to do the same thing
names(wwd[,2:12]) #see the column names
lowerFn <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(alpha=0.1, size=0.1) +
    geom_smooth(method = 'lm', se = F, size = 0.4, alpha = 0.3)
  p
} #add function to make the plot prettier
p_with_qt <- ggpairs(wwd_tmp, mapping = aes(color = quality), columns = c("residual.sugar", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "alcohol"), 
                     lower = list(continuous = wrap(lowerFn)), 
                     upper = list(continuous = wrap("cor", size = 2.5))) +
  theme(axis.text=element_text(size=6))
#draw relationships between sugar, dioxide, density and alcohol with different qualities.

ggsave('ggpair_with_qt.jpg', p_with_qt) #save
```

When taking a deeper look at the dataset, I find that some correlation varies a lot between quality factors. For example, the correlation between alcohol and free sulfur dioxide is 0.295 for quality 9, but it is negative for other quality factors. We should bear in mind that the relationships between variables might vary from different quality scores.


We use Spearman's rho statistic to estimate a rank-based measure of association. Correlation falls between -1 and 1. 0 suggests there is no association between the two variables while numbers close to -1 or 1 suggests strong negative and positive associations accordingly.

```{r echo=FALSE, message=FALSE, warning=FALSE}
cor(wwd[, 2:12], wwd$quality, method = 'spearman') #check the correlation between quality and other input variables
```

As we can see, only alcohol, density, and chlorides have weak correlation with quality, the others do not seem to correlate to the quality of wine. However, to discover more correlation between input variables and quality to predict the wine quality, I would like to dig deeper into the dataset.


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(x = density, y = pH, color = factor(quality)), data = wwd) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = 'lm', se = F, size = 0.7) +
  scale_x_continuous(trans = cuberoot_trans(), 
                     limits = c(min(wwd$density), quantile(wwd$density, 0.99))) +
  scale_color_brewer(type='seq',
                   guide=guide_legend(title='Quality'))
#show the pH and density distribution with different qualities
ggsave('q~den~pH.jpg') #save
```

It is difficult to find a specific pattern in this figure since quality has a wide range. To simplify the situation, I categorize the quality of wine into four groups of Poor, Normal, Good, and Great to be able to differentiate patterns in each category.

```{r echo=FALSE, message=FALSE, warning=FALSE}
wwd$rating[5 > wwd$quality] = 'Poor' #add quality label
wwd$rating[5 == wwd$quality] = 'Normal' #add quality label
wwd$rating[5< wwd$quality & wwd$quality < 7] = "Good" #add quality label
wwd$rating[7<= wwd$quality ] = "Great" #add quality label
wwd$rating <- factor(wwd$rating, levels = c('Poor', 'Normal', 'Good', 'Great')) #set the label as factor
# add rating labels into the dataset
```

Below is how the quality of wines is distributed based on the rating that I just introduced:

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wwd, aes(x = rating)) +
  geom_bar() #show the quality label distribution
ggsave('rating.jpg') #save
```
```{r}
table(wwd$rating)
```

Now again we plot the two features of pH and density but this time use the new rating to see a pattern between quality and these two features:

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(x = density, y = pH, color = rating), data = wwd) +
  geom_point(size = 0.3, alpha = 0.3) +
  scale_x_continuous(trans = cuberoot_trans(), 
                     limits = c(min(wwd$density), quantile(wwd$density, 0.99))) +
  geom_smooth(method = 'lm', se = F, size = 0.7, alpha = 0.5)
#plot again with rating
ggsave('q_with_rating.jpg') #save
```

From the scatter plot we can see that most good and great wines have lower density, whereas pH has no valid correlation with wine quality. Next I would like to dig deeper into the relationships between density and wine rating.

```{r echo=FALSE, message=FALSE, warning=FALSE}
wwd$densitylLabel[0.9917 >= wwd$density] = 'Low' #add density label
wwd$densitylLabel[0.9917 < wwd$density & wwd$density <= 0.9961 ] = 'Normal' #add density label
wwd$densitylLabel[wwd$density > 0.9961 ] = 'High' #add density label
wwd$densityLabel <- factor(wwd$densitylLabel, 
                           levels = c('Low', 'Normal', 'High')) #set the variable as factor
# categorize density into 3 different levels
#ggplot(aes(x = densitylLabel, y = density, color = rating), data = wwd) +
#  geom_point(size = 0.5, alpha = 0.2, position = 'jitter') +
#  xlim(c('Low', 'Normal', 'High')) +
#  scale_y_continuous(limits = c(min(wwd$density), quantile(wwd$density, 0.99)))
#plot again with density label
ggplot(aes(x = densitylLabel, fill = rating), data = wwd) +
  geom_bar(size = 0.5, aes(y = (..count..)/sum(..count..)), position = 'dodge') +
  xlim(c('Low', 'Normal', 'High')) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = ((..count..)/sum(..count..)), color = rating, 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, check_overlap = T, size = 3,
            position = position_dodge(width = 0.9)) +
  ylab('Percentage')

ggsave('den~q.jpg') #save
```

After categorizing the density into 3 groups with 1st and 3rd quartiles, we can easily see that most wines with low density are rated as “Good” or “Great”, but poor and normal wines are still difficult to find connection with density.


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(x = alcohol, y = citric.acid, color = rating), data = wwd) +
  geom_point(alpha = 0.3, size = 0.7, position = 'jitter') +
  geom_smooth(method = 'lm', se = F, size = 0.4, alpha = 0.2) + 
  scale_y_continuous(limits = c(min(wwd$citric.acid), 
                                quantile(wwd$citric.acid, 0.99))) +
  ylab('Citric Acid') 
#show the distribution of alcohol and citric acid between different ratings
ggsave('a~ca~q.jpg') #save
```

From the distribution we can see that the alcohol by volume has no valid correlation with wine in poor quality, as well as the volume of citric acid has no valid connection with wine quality. They are widely spread all over the plot. To make the relationships more clear, let's take a deeper look at it.

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=7 }
wwd$alcohol.rounding <- floor(wwd$alcohol)
give.n <- function(x){
   return(c(y = mean(x), label = length(x)))
} #going to display the sample size at the sample mean, making the label multifunctional
ggplot() +
  geom_boxplot(aes(x = rating, y = citric.acid, fill = rating, color = rating), 
       data = wwd, alpha = 0.5, varwidth = T) +
  labs(x = 'Alcohol with Rating', y = ('Citric Acid')) +
  scale_y_continuous(limits = c(min(wwd$citric.acid), quantile(wwd$citric.acid, 0.99))) +
  stat_summary(aes(x = rating, y = citric.acid), data = wwd, fun.data = give.n, geom = "text", 
               vjust = -1.5, hjust = 1, size = 2.5) +
  facet_wrap(~alcohol.rounding, nrow = 1) +
  theme(axis.text=element_text(size=6), legend.position = "none")
#set the alcohol filter
ggsave('a~ca~q_dp.jpg') #save
```

Percentage of getting good/great wines with alcohol percentage >= 14%:  
(6+1)/(6+1+0+0) = 100%  
Percentage of getting good/great wines with alcohol percentage between 13% and 14%:  
(84+41)/(84+41+4+1) = 96.15%  
Percentage of getting good/great wines with alcohol percentage between 12% and 13%:  
(328+302)/(328+302+31+10) = 93.89%  
Percentage of getting good/great wines with alcohol percentage between 11% and 12%:  
(298+453)/(298+453+120+32) = 83.17%  
Percentage of getting good/great wines with alcohol percentage >= 13%:  
(6+1+84+41)/(6+1+0+0+84+41+4+1) = 96.35%  
Percentage of getting good/great wines with alcohol percentage >= 12%:  
(6+1+84+41+328+302)/(6+1+0+0+84+41+4+1+328+302+31+10) = 94.31%  
Percentage of getting good/great wines with alcohol percentage >=11%:  
(6+1+84+41+328+302+298+453)/(6+1+0+0+84+41+4+1+328+302+31+10+298+453+120+32) = 88.43%  

The boxplot shows that most wines with alcohol volume equal and greater than 11% are rated as “Good” or “Great”. Now I want to combine the two findings we discovered above and see whether it is possible to find the distribution pattern. Also, the easiest way to pick good/great wine is choosing the wine with 14 alcohol percentage and above, though we have very few choices.

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=5 }
#combine desity and alcohol together
#ggplot(aes(x = densityLabel, y = alcohol, color = rating), data = wwd) +
#  geom_point(alpha = 0.5, size = 1, position = 'jitter') #plot with density label and alcohol
finalfig2_1 <- ggplot(aes(x = rating, y = density), 
       data = subset(wwd, density <= quantile(density, 0.99))) +
  geom_boxplot(aes(color = rating, fill = rating), alpha = 0.6, varwidth = T) +
  geom_point(aes(color = rating), alpha = 0.15, size = 0.3, position = 'jitter') +
  geom_text(aes(y = ((..count..)/(..count..)) + 0.001, color = rating, 
                label = (..count..)), 
            stat = "count", vjust = 0, check_overlap = T, size = 2.5,
            position = position_dodge(width = 0.9)) +
  facet_wrap(~alcohol.rounding, nrow = 1) +
  theme(axis.text=element_text(size=6), legend.position = "none") +
  labs(title = 'Relationships Between Rating, Density, and Alcohol %')

finalfig2_1

ggsave('dL~a~q.jpg') #save
```

The plot looks pretty good now, but I want to make a difference between high and low alcohol volume with normal density, to make it more clear.

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=5 }
ggplot(aes(x = rating, y = density), 
       data = subset(wwd, densityLabel == 'Low')) +
  geom_boxplot(aes(color = rating, fill = rating), alpha = 0.6, varwidth = T) +
  geom_point(aes(color = rating), alpha = 0.15, size = 0.3, position = 'jitter') +
  geom_text(aes(y = ((..count..)/(..count..)) - 0.008, color = rating, 
                label = (..count..)), 
            stat = "count", vjust = -1.5, check_overlap = T, size = 2.5,
            position = position_dodge(width = 0.9)) +
  facet_wrap(~alcohol.rounding, nrow = 1) +
  theme(axis.text=element_text(size=6), legend.position = "none") 
#plot again with new labels
ggsave('dL~a~q_adj.jpg') #save
```

Percentage of getting good/great wines with alcohol percentage >= 14%:  
(6+1)/(6+1+0+0) = 100%  
Percentage of getting good/great wines with alcohol percentage between 13% and 14%:  
(78+39)/(78+39+4+0) = 96.69%  
Percentage of getting good/great wines with alcohol percentage between 12% and 13%:  
(260+243)/(260+243+20+10) = 94.37%  
Percentage of getting good/great wines with alcohol percentage between 11% and 12%:  
(161+225)/(161+225+57+18) = 83.73%  
Percentage of getting good/great wines with alcohol percentage >= 13%:  
(6+1+78+39)/(6+1+0+0+78+39+4+0) = 96.88%  
Percentage of getting good/great wines with alcohol percentage >= 12%:  
(6+1+78+39+260+243)/(6+1+0+0+78+39+4+0+260+243+20+10) = 94.86%  
Percentage of getting good/great wines with alcohol percentage >=11%:  
(6+1+78+39+260+243+161+225)/(6+1+0+0+78+39+4+0+260+243+20+10+161+225+57+18) = 90.29%  

Now according to the plot, we know that we have a great chance to get good or great wine if we choose one with higher alcohol percentage and lower density. It’s a great progress!

```{r echo=FALSE, message=FALSE, warning=FALSE }
#What about the impact of sugar and chlorides on quality?
ggplot(aes(x = residual.sugar, y = chlorides, color = rating), data = wwd) +
  geom_point(alpha = 0.2, size = 0.5, position = 'jitter') +
  geom_smooth(method = 'lm', se = F, size = 0.5) +
  xlim(c(0, quantile(wwd$residual.sugar, 0.99))) +
  ylim(c(0, quantile(wwd$chlorides, 0.99))) 
#distribution of sugar and chloride with different rating
ggsave('s~ch~q.jpg') #save
```

Again, poor quality wines are spread widely all over the plot, and great quality wines seem to contain less sugar. Let's dig deeper into it.

```{r echo=FALSE, message=FALSE, warning=FALSE }
ggplot(aes(x = rating, y =residual.sugar, fill = rating), 
       data = wwd) +
  geom_boxplot(varwidth = T) + 
  ylim(c(0, quantile(wwd$residual.sugar, 0.99))) +
  facet_wrap(~alcohol.rounding, nrow = 1) +
  geom_text(aes(y = ((..count..)/(..count..)) + 17.5, color = rating, 
                label = (..count..)), 
            stat = "count", vjust = -3, check_overlap = T, size = 2,
            position = position_dodge(width = 0.9)) +
  theme(axis.text=element_text(size=4), legend.position = "none")
  #remove outliers and check the distribution of sugar

ggsave('s~q.jpg') #save
```

Although many good and great wines contain less sugar, there are many normal and poor wines with low sugar content as well. There is no valid correlation between rating of wine and residual sugar volume. But surprisingly, we can increase the likelihood of choosing good or great wine with 13% alcohol and above to 100% by removing the normal and poor wines with low sugar volume.

```{r echo=FALSE, message=FALSE, warning=FALSE }
summary(wwd$residual.sugar[13 <= wwd$alcohol & wwd$rating == 'Normal'])
# get the max value of sugar
finalfig2_2 <- ggplot(aes(x = rating, fill = rating), 
       data = subset(wwd, alcohol >= 13 & residual.sugar > 1.5)) +
  geom_bar()+
  geom_text(aes(x = rating, y = (..count..),
            label = (..count..)), stat = 'count', check_overlap = T,
            size = 4,
            position = position_dodge(width = 1)) +
  labs(title = 'Wine with 13% alcohol and above and sugar volume greater than 1.5 g / dm^3') +
  theme(legend.position = "none")
finalfig2_2
ggsave('finalfig1.jpg')
```

Now we can easily pick a one out of the 114 bottles and it would always be good or great wine, wow! 

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5 }
wwd$chlorides.buckets = cut(wwd$chlorides, c(0, 0.01, 0.02, 0.03, 0.035, 0.04, 0.05, 0.1, 0.15, 
                                             max(wwd$chlorides)+0.001))
table(wwd$chlorides.buckets)
#divide into some groups
ggplot(aes(x = chlorides.buckets, color = rating, fill = rating), 
       data = wwd) +
  geom_bar(position = 'dodge') +
  geom_text(aes(y = (..count..), color = rating, 
                label = (..count..)), 
            stat = "count", vjust = -2, check_overlap = T, size = 2.3,
            position = position_dodge(width = 0.9)) +
  theme(axis.text=element_text(size=6))
  #take chloride into consideration
ggsave('ch~q.jpg') #save
```
```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=15 }
ggplot(aes(x = chlorides.buckets, y = density, color = rating, fill = rating), 
       data = wwd) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(0.980, 0.985, 0.990, 0.995, 1.000), 
                     limits = c(min(wwd$density), 1.005)) +
  geom_text(aes(y = (..count..)/(..count..)+0.003, color = rating, 
                label = (..count..)), 
            stat = "count", vjust = 0, check_overlap = T, size = 3,
            position = position_dodge(width = 0.9)) +
  facet_wrap(~alcohol.rounding, ncol = 1) +
  theme(axis.text=element_text(size=6))

ggsave('ch~q_box.jpg')
```

Chance to get good/great wines when chloride volume <= 0.035:  
(229+287+213+221+27+20)/(229+287+94+23+213+221+65+9+27+20+6+2+1) = 83.29%  
Chance to get good/great wines when chloride volume < 0.030:  
(213+221+27+20)/(213+221+65+9+27+20+6+2+1) = 85.28%  

From the plot we can see that most wines with chlorides below or equal to 0.035 g / dm^3 are rated as “Good” or “Great”, but again, there is no valid correlation between poor wines and chlorides volume.

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=8, fig.height=5 }
finalfig2_3 <- ggplot(aes(x = rating, y = density), 
       data = subset(wwd, densityLabel == 'Low' & chlorides <= 0.035)) +
  geom_boxplot(aes(color = rating, fill = rating), alpha = 0.6, varwidth = T) +
  geom_point(aes(color = rating), alpha = 0.15, size = 0.3, position = 'jitter') +
  geom_text(aes(y = ((..count..)/(..count..)) - 0.008, color = rating, 
                label = (..count..)), 
            stat = "count", vjust = -1, check_overlap = T, size = 3,
            position = position_dodge(width = 0.9)) +
  facet_wrap(~alcohol.rounding, nrow = 1) +
  theme(axis.text=element_text(size=4), legend.position = "none") +
  labs(title = 'Relationships Between Rating and Alcohol % After Filtering')
#ggplot(aes(x = densityLabel, y = alcohol, color = rating), 
#       data = subset(wwd, chlorides <= 0.035)) +
#  geom_point(alpha = 0.5, size = 1, position = 'jitter') 
#take density, alcohol and chloride into consideration together
finalfig2_3
ggsave('ch~a~dL.jpg') #save
```

Percentage of getting good/great wines with alcohol percentage >= 14%:  
  (2+1)/(2+1+0+0) = 100%  
Percentage of getting good/great wines with alcohol percentage between 13% and 14%:  
  (50+24)/(50+24+4+0) = 94.87%  
Percentage of getting good/great wines with alcohol percentage between 12% and 13%:  
  (171+140)/(171+140+9+8) = 94.82%  
Percentage of getting good/great wines with alcohol percentage between 11% and 12%:  
  (91+87)/(91+87+17+8) = 87.68%  
Percentage of getting good/great wines with alcohol percentage >= 13%:  
  (2+1+50+24)/(2+1+0+0+50+24+4+0) = 95.06%  
Percentage of getting good/great wines with alcohol percentage >= 12%:  
  (2+1+50+24+171+140)/(2+1+0+0+50+24+4+0+171+140+9+8) = 94.87%  
Percentage of getting good/great wines with alcohol percentage >=11%:  
  (2+1+50+24+171+140+91+87)/(2+1+0+0+50+24+4+0+171+140+9+8+91+87+17+8) = 92.48%  

Now we know how to pick good and great wines, but we still don't know what makes the wines poor.

```{r}
wwd_poor <- subset(wwd, rating == 'Poor' | rating == 'Normal') #set new dataset only contain rating poor and normal
wwd_poor$rating_score[wwd_poor$rating == 'Poor']  = 0 #set new variable
wwd_poor$rating_score[wwd_poor$rating == 'Normal'] = 1 #set new variable
cor(wwd_poor[, 2:12], wwd_poor$rating_score, method = 'spearman')
# Try to see what make poor wines different from the normal ones
```

It's pretty hard to find out what leads to a “Poor” rating in wines, let’s try making boxplots of each input variable in different ratings and see if we can find some clues:

```{r echo=FALSE, message=FALSE, warning=FALSE }
#reorder_wwd <- wwd[, c(1,2,3,4,5,6,14,15,16,7,8,9,10,11,12,13)]
# reorder and move the label characters to the middle
#melted_wwd1 <- melt(reorder_wwd[, 2:10])
# make the dataset to be long rather wide
#ggplot(data = melted_wwd1, aes(color = rating, variable, value)) +
#  geom_boxplot() +
#  facet_grid(. ~ rating)
# draw a comparsion plot of rating vs each variable

#melted_wwd2 <- melt(reorder_wwd[, 7:16]) # make the dataset to be long rather wide
#ggplot(data = melted_wwd2, aes(color = rating, variable, value)) +
#  geom_boxplot() +
#  facet_grid(. ~ rating)
# try facet_wrap
#ggplot(data = melted_wwd2, aes(color = rating, variable, value)) +
#  geom_boxplot() +
#  facet_wrap(~ rating)
# The variable range differs a lot and hard to compare
```

```{r echo=FALSE, message=FALSE, warning=FALSE, fig.height=8 }
# set boxplots with rating and each input variable first
bp1 <- ggplot(aes(x = rating, y = wwd[,2]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Fixed Acidity') +
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,2], 0.99))) +
  theme(axis.text=element_text(size=4), legend.position = "none") 
bp2 <- ggplot(aes(x = rating, y = wwd[,3]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Volatile Acidity')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,3], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp3 <- ggplot(aes(x = rating, y = wwd[,4]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Citirc Acid')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,4], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp4 <- ggplot(aes(x = rating, y = wwd[,5]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Residual Sugar')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,5], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp5 <- ggplot(aes(x = rating, y = wwd[,6]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Chlorides')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,6], 0.99)), breaks = c(0,0.035,0.05,0.1,0.15))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp6 <- ggplot(aes(x = rating, y = wwd[,7]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Free SO2')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,7], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp7 <- ggplot(aes(x = rating, y = wwd[,8]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Total SO2')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,8], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp8 <- ggplot(aes(x = rating, y = wwd[,9]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Density')+
  xlab('Rating') +
  scale_y_continuous(limits = c(min(wwd[,9]), quantile(wwd[,9], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp9 <- ggplot(aes(x = rating, y = wwd[,10]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('pH')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,10], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp10 <- ggplot(aes(x = rating, y = wwd[,11]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Sulphates')+
  xlab('Rating') +
  scale_y_continuous(limits = c(0, quantile(wwd[,11], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp11 <- ggplot(aes(x = rating, y = wwd[,12]), data = wwd) +
  geom_boxplot(aes(color = rating)) + ylab('Alcohol Percentage')+
  xlab('Rating') +
  scale_y_continuous(limits = c(min(wwd[,12]), quantile(wwd[,12], 0.99)))+
  theme(axis.text=element_text(size=4), legend.position = "none")
bp_combine <- grid.arrange(bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8, bp9, 
                           bp10, bp11,
                         ncol = 3, nrow = 4) #combine all plots
bp_final <- grid.arrange(bp5, bp6, bp8, bp11, 
                         ncol = 2, nrow = 2, 
                         top = "Relationships between Rating and Input Variables")
#delete some plots we are not interested
#combine the boxplots I found interesting in earlier analysis
ggsave('finalp1.jpg', bp_final) #save the final plot 1
ggsave('bpsave.jpg', bp_combine) #save

```

From the boxplots we see that the 7th input variable, which is free sulfur dioxide, is more related to poor wines than the others. Next I would like to create a scatter plot to see if free sulfur dioxide and residual sugar have an impact on the quality of wine.

```{r echo=FALSE, message=FALSE, warning=FALSE }
ggplot(aes(x = residual.sugar, y = free.sulfur.dioxide, color = rating), 
       data = wwd) +
  geom_point(alpha = 0.3, size = 0.4, position = 'jitter') + 
  geom_smooth(method = 'lm', se = F, size = 0.4) +
  scale_x_continuous(limits = c(0, quantile(wwd$residual.sugar, 0.99))) +
  scale_y_continuous(limits = c(0, quantile(wwd$free.sulfur.dioxide, 0.99))) #check the distribution of sugar and free SO2 with different ratings without outliers
ggsave('so~s~pq.jpg') #save
```


```{r warning=FALSE}
wwd_poor <- subset(wwd, rating == 'Poor') #creat a new dataset with only poor wines
table(wwd_poor$free.sulfur.dioxide)
# total poor wines below 10 is 57
table(wwd$free.sulfur.dioxide)
# total wines below 10 is 223
# the proportion is 25.56%, which is much higher than choosing blindly (3.74%) but not enough
```

It seems like the wines with lower free sulfur dioxide volume have a higher chance to be rated as “Poor”. In wines with free sulfur dioxide volume below 10  mg / dm^3, the poor ones take 57 out of the total 223 (25.56%), which is much higher than picking blindly (183 out of 4,898, 3.74%). It’s a pretty good finding but it’s still not valid enough to distinguish poor wines from the others. It looks like that there are no clues to tell poor quality wines with the data we have so far.

```{r echo=FALSE, message=FALSE, warning=FALSE }
ggplot(aes(x = rating, y = free.sulfur.dioxide, color = rating), data = wwd) +
  geom_boxplot(varwidth = T) +
  scale_y_continuous(limits = c(min(wwd$free.sulfur.dioxide), 
                                quantile(wwd$free.sulfur.dioxide, 0.99))) +
  geom_text(aes(y = ((..count..)/(..count..)) + 80, color = rating, 
                label = (..count..)), 
            stat = "count", vjust = -1.2, check_overlap = T, size = 2.5,
            position = position_dodge(width = 0.9)) +
  facet_grid(~alcohol.rounding) +
  theme(axis.text=element_text(size=4), legend.position = "none")
#check the distribution of free SO2 with all rating
ggsave('so2~q.jpg') #save
#summary(wwd_poor$free.sulfur.dioxide[wwd_poor$rating == 'Poor']) 
#check the chloride data summary of poor wines
```

It shows that there is connection between poor wine qualty and volume of free SO2 solely for when filtering the alcohol percentage equal or greater than 11. However, normal wine qualty seems to be unrelated with the volume of free SO2. It seems like there are no clues to tell poor and normal quality wines with the data we have.

##How possible can we get a good or great bottle of wine?

```{r}
wwd_gg <- wwd %>%
  filter(alcohol >= 11) %>%
  filter(densityLabel == 'Low') %>%
  filter(chlorides <= 0.035)
# apply the filters we discover from the analysis above
table(wwd_gg$rating) #show the counts
```

Chance to get a good or great bottle of wine = (252+314)/(16+30+252+314) = 92.48%  
Chance to get a great one: 314/(16+30+252+314) =51.31%  

Compare to the original dataset (choose blindly):
```{r}
table(wwd$rating) #show the counts
```

Chance to get a good or great bottle of wine = (2198+1060)/(183+1457+2198+1060) = 66.52%  
Chance to get a great one: 1060/4898 =21.64%  

The possibility that we can get a bottle of wine rated as “Good” or “Great” is: 92.48%, which is pretty high! Using the filter we would have more than 50% (51.31% actually) to get a great one. Both are much better than choosing blindly (66.52% and 21.64% separately).


##Making prediction  
Take alcohol into consideration  
```{r echo=FALSE, message=FALSE, warning=FALSE }
model_mglm = multinom(rating ~ alcohol, data = wwd) #set a multinomial function between rating and alcohol
pred_mglm = predict(model_mglm) 
# make prediction with alcohol variables
table(wwd$rating, pred_mglm) #show the prediction output
```
Accuracy = (0+738+1444+313) / 4898 = 50.9%  
Percentage of good / great prediction accuracy = (1444+231+629+313)/(2198+1060) = 2617/3258 = 80.33%  

Now add density label into consideration  
```{r echo=FALSE, message=FALSE, warning=FALSE }
model_mglm = multinom(rating ~ alcohol + densityLabel, data = wwd) #add density label into consideration
pred_mglm = predict(model_mglm) #make prediction

table(wwd$rating, pred_mglm) #show the prediction output
```

Accuracy = (0+697+1416+325) / 4898 = 49.78%  
Percentage of good / great prediction accuracy = (1416+264+620+325)/(2198+1060) = 2625/3258 = 80.57%  

Maybe due to the high correlations between alcohol and density

Add chlorides into consideration

```{r echo=FALSE, message=FALSE, warning=FALSE }
wwd$chloridesLabel[0.035 <= wwd$chlorides] = 'High' #add chloride label
wwd$chloridesLabel[0.035 > wwd$chlorides] = 'Low' #add chloride label
model_mglm = multinom(rating ~ alcohol + chloridesLabel, data = wwd) #add density label into consideration
pred_mglm = predict(model_mglm) #make prediction

table(wwd$rating, pred_mglm) #show the prediction output
```
Accuracy = (0+738+1444+339) / 4898 = 51.47%  
Percentage of good / great prediction accuracy = (1444+231+603+339)/(2198+1060) = 2617/3258 = 80.33%  

From the prediction output we can see that the accuracy is not really good, and all poor wines are overrated, and some of them are even predicted as “Great”, which means there is no effective method to distinguish between them.

```{r echo=FALSE, message=FALSE, warning=FALSE }
model_mglm = multinom(quality ~ alcohol + densityLabel + chlorides, data = wwd)
# take alcohol, density, and chlorides into consider together
wwd$prediction = as.numeric(as.character(predict(model_mglm)))
# make a prediction function
# prediction function output factors, need to be transformed into numeric
wwd$prediction.diff = wwd$quality - as.numeric(wwd$prediction)
# compute the differences
table(wwd$prediction.diff)

wwd$great.prediction.diff[wwd$prediction.diff >=2 | wwd$prediction.diff <= -2] = FALSE #set filter with difference equal or greater than 2
wwd$great.prediction.diff[wwd$prediction.diff <2 & wwd$prediction.diff > -2] = TRUE #set filter with difference less than 2

finalfig3 <- ggplot(aes(x = quality, fill = great.prediction.diff),
       data = wwd) +
  geom_bar( position = 'dodge') +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9)) +
  ggtitle('Error < 1 Point by Quality Prediction') +
  geom_text(aes(y = ..count.., 
                label = ..count..), 
            stat = "count", vjust = -1.2, check_overlap = T, size = 2.5,
            position = position_dodge(width = 0.9))
  #draw the prediction error distribution

finalfig3
#final plot 3
```
```{r echo=FALSE, message=FALSE, warning=FALSE }
#wwd$X <- NULL #remove the column not used in the analysis
```
##FINAL PLOT1
```{r echo=FALSE, message=FALSE, warning=FALSE, fig.height=8}
# final plot 1
bp_final <- grid.arrange(bp5, bp6, bp8, bp11, 
                         ncol = 2, nrow = 2, 
                         top = "Final Plot 1: Relationships between \nRating and Input Variables")

ggsave('finalp1.jpg', bp_final)
```

##Description One

I present 4 boxplots to show the relationships between rating and input variables I found to be correlated in prior analysis:  
For chlorides on the top-left, we can see that more great wines have lower chlorides volume than the other 3 rating categories;  
For free SO2 on the top-right, more than half of the poor wines contain free SO2 that is less than 20 mg / dm^3, which is the most valid difference between poor wines and the others;  
When comparing with density, we can see that good and great wines are with lower density, especially the wines rated as “Great”; and  
For alcohol on the bottom-right, it’s easy to tell that most great wines and many good wines contain a higher alcohol percentage than normal and poor ones.  
From the boxplots we can also find that the differences between each rating compared with the input variables are not clear enough that we can easily distinguish between them.  


##FINAL PLOT2
```{r echo=FALSE, message=FALSE, warning=FALSE, fig.width=12, fig.height=15}
finalfig2 <- grid.arrange(finalfig2_1, finalfig2_2, finalfig2_3, ncol = 1, 
                          top = 'Final Plot 2: How to Get Good/Great Wines?') 
# Show how many good and great wines we can pick after we set the filters
ggsave('finalp2.jpg', finalfig2) #save the final plot 2
```

##Description Two

Although I can not find a exact pattern to predict wine quality properly, we can still have a pretty high chance, which is 92.48%, to get a bottle of wine rated at 6 or above out of 612 options through the listed filters:  
Density below or equal to 0.9917;  
Alcohol percentage equal to or above 11%; and  
Chlorides volume below or equal to 0.035  g / dm^3  
Besides, I also find two methods to get high quality wines easily and they would never fail. One is to pick wine with high alcohol percentage, which is equal or above 14, though we have very few choices that is only 7. The other is to pick wine with 13% alcohol and above and sugar volume greater than 1.5 g / dm^3. The choices are expanded to 114, which is much more.


##FINAL PLOT3
```{r echo=FALSE, message=FALSE, warning=FALSE }
ggplot(aes(x = quality, fill = great.prediction.diff),
       data = wwd) +
  geom_bar( position = 'dodge') +
  scale_x_continuous(breaks = c(3,4,5,6,7,8,9)) +
  ggtitle('Final Plot 3: Error < 1 Point by Quality Prediction') +
  geom_text(aes(y = ..count.., 
                label = ..count..), 
            stat = "count", vjust = -1.2, check_overlap = T, size = 2.5,
            position = position_dodge(width = 0.9))
ggsave('finalp3.jpg') #save the final plot 3
```

##Description 3

After deriving a prediction function by taking alcohol, density, and chlorides into consideration, we can make a good prediction on wine quality between 5 and 7 with prediction difference below or equal to 1. But for the great wines rated at 8 or above and the poor wines rated at 4 or below, most prediction items differ in 2 or even more from the actual quality. As the plot shows, no any extreme wines rated at the extremes of 3 and 9 are predicted properly with quality difference less than 2. 

##Reflections

In this project, I explored a moderately sized dataset on white wines. The dataset was provided in a clean format, without any missing data, and I didn’t need to augment it with any external sources. That said, the data only consisted of various chemical properties, and a quality score. Some of the key attributes in defining a wine’s quality, like region, vintage, grape variety, etc. were absent.

I used correlation values to find relationships between attributes. Where significant skew was present, I took logs, square roots, or cube roots. I found bimodal distribution in a feature, but it appeared to not be related to quality; it is likely that some confounders are causing the bimodal distribution, and this is worth investigating further.

With all the quality levels, the plots started looking messy. To make things more clear, I re-categorized wine quality into four groups. This helped quite a lot. I found interesting trends in the relationships between wine quality, alcohol percentage, density, and chloride volume, as well as a special relationship between sugar volume and high alcohol percentage. Although some trends were discovered, I found it to be pretty hard to plot the relationships properly. I tried scatter plot at first, but it was messy and hard to quantify. After googling for a long long time, I finally figured out some ways to plot them: boxplot and barplot, accompanying some quantification arguments such as varwidth in geom_boxplot, geom_text, and stat_summary, which were not practiced frequently or even not mentioned in video lectures. 

I created a predictive model which was not very good at predicting exact scores. Although most differences between prediction output and actual quality are below or equal to 1, wines rated as “Poor” were all overrated. The model only took some input variables into consideration. Perhaps some higher order terms, and interaction terms could help in reducing the margin of error further.

Of course, to finish this project, I googled a lot of analysis reports on this dataset and copy some of their ideas. But it still took me a long time to finish because I had to figure out how to make my codelines work properly on my own. When receiving feedbacks from the 1st review, I really learned a lot about how to express relationships properly and tried to make the analysis report more readable, though it was very frustrating and tiring.

Finally, I found that the the relationships between quality of wine and other input variables varied a lot, but maybe we can find some patterns on two continued levels. For example, what factors make wine rated at 7 and 6 different? Or what factors make wine with 11% alcohol and 12% alcohol different? It might better interpret and analyze the relationships between variables.

Reference:  
Loop in R reference:
https://www.r-bloggers.com/how-to-write-the-first-for-loop-in-r/

ggpairs reference:
http://stackoverflow.com/questions/39709745/decreasing-the-line-thickness-and-corr-font-size-in-ggpairs-plot
https://www.rdocumentation.org/packages/GGally/versions/1.2.0/topics/ggpairs

Adding labels and text into the plot:
http://stackoverflow.com/questions/3695497/show-instead-of-counts-in-charts-of-categorical-variables
https://github.com/tidyverse/ggplot2/issues/1254
http://stackoverflow.com/questions/3483203/create-a-boxplot-in-r-that-labels-a-box-with-the-sample-size-n

Removing legend labels:
http://stackoverflow.com/questions/14604435/turning-off-some-legends-in-a-ggplot

Reshape reference:
http://stackoverflow.com/questions/20892266/multiple-plots-using-loops-in-r

Reorder reference:
http://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame

GGally reference: 
http://ggobi.github.io/ggally/#canonical_correlation_analysis

Other reference:
http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
