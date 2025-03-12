## Homework 4 Submission ## 

## Question 1:

# Hypothesis: mistletoe changes light availability and soil fertility beneath canopies of parasitized trees

# Explanatory Variable = Treatment 
# Categorical variable, whether tree is parasitized by mistletoe or not

# Response Variable = Seedlings
# Count of number of seedlings in 5x5m plots beneath parasitized and not parasitized trees

# Extra Explanatory Variable = Year
# Whether survey taken in 2011 or 2012

## 1a)  Fit a glm assessing evidence for the following hypothesis: Seedling density is increased beneath trees experiencing mistletoe infection. Describe the rationale you used in selecting a glm structure and probability distribution. Calculate model fit using MAE.

# GLM: poisson or negative binomial

# mean: 160.6591
# variance: 113237.6
# data is overdispersed (variance > mean)

# GLM = negative binomial to account for overdispersion

# exp() to transform 

# read in mistletoe csv

mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)

# fit negative binomial model
#
library(MASS)

mod.nbin <- glm.nb(Seedlings~Treatment, data = mistletoe)

summary(mod.nbin)

### Describe rationale for selecting glm and probability distribution:

# Because my explanatory variable was categorical (treatment) and response variable was counts (seedlings), I knew the appropriate glm would either be poisson or negative binomial. Since the mean seedling value was less than the variance seedling value, I chose negative binomial to account for overdispersion in my data

# Calculate model fit with MAE
library(performance)

performance::mae(mod.nbin)

# MAE = 145.841

## 1b) Use visual (e.g. a marginal effects plot) and written (e.g. effect sizes on scale of response) approaches to interpret the results of your model.

# Marginal Effects Plots
library(marginaleffects)

plot_predictions(mod.nbin, condition="Treatment")

# View model summary
summary(mod.nbin)

# Estimates of seedlings recruited for parasitized and unparaistized treatments
exp(5.7308) # Parasitized (Intercept) = 308.2
exp(5.7308+-3.1575)  # Unparasitized (Intercept + Slope) = 13.1

# Make Predictions - automatically transforms
predictions(mod.nbin, newdata=data.frame(Treatment=c("parasitized", "unparasitized")))

### Based on your fitted model results and model fit, write 3-4 sentences to discuss the following biolgical conclusions. Does mistletoe infection alter seedling density? How much does seedling recruitment differ beneath parasitized and unparasitized trees? Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters.

# Mistletoe infection has a significant effect on altering seedling density of forest systems (p-value < 0.05). Seedling recruitment for parasitized versus unparasitized trees differs by approximatley 295.1 seedlings, as average seedling recruitment for parasitized trees is 308.2 and unparasitized trees is 13.1. The standard error for both treatments was realtivley high, 776.3 and 156.4 respectively. MAE was also high for the relative values of seedling recruitment (145.841) indicating there is likely other variables not currently included in the model that are impacting seedling recruitment within forest systems and if included in the model could account for more variation. Still, these results supports our hypothesis that mistletoe as plays a significant role parasitizing trees, changes light availability, and altering seedling recruitment and community composition beneath trees.

### Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters.

# To inform my conclusions, I mostly used the predicted values for the mean count of seedlings based on treatment. I also looked at the estimates from my summary table and saw that the unparasitized treatment was negative relative to the intercept, so I knew that the unparasitized seedling counts would be much lower. I also looked at the standard deviation and MAE to see how far off these estimates were. 

## 1c)  Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study.

mistletoe$Year<-as.factor(mistletoe$Year)

# Negative binomial with year as an interaction term
mod.nbin2 <- glm.nb(Seedlings~Treatment*Year, data = mistletoe)

# View summary
summary(mod.nbin2)

plot_predictions(mod.nbin2, condition = c("Treatment", "Year")) 

# Seedling recruitment estimate for unparasitized groups when including year

predictions(mod.nbin2, newdata=data.frame(Treatment=c("parasitized", "unparasitized","unparasitized","parasitized"), Year=c("2011","2012","2011","2012")))

# Parasitized, 2011 = 218.4
# Parasitized, 2012 = 4.8
# Unparasitized, 2011 = 21.4
# Unparasitized, 2012 = 398.0

# Calculate MAE:
performance::mae(mod.nbin2)

# 140.0407

### Write ~2 new sentences that summarize the results of the new model and their biological implications.

# The effect of treatment is not dependent on year because it was only marginally significant (p-value = 0.0599). If year was significant, we would see that it had a negative impact on seedling recruitment for the parasitized treatments and a positive impact on seedling recruitment for unparasitized treatments from 2011 to 2012 (found from the predictions table). MAE only decreased slightly (~140) and was still very high relative to seedling estimates. These results, however, are not significant and have substantial standard error for seedling estimates, thus these conclusions cannot be made.

## Question 2:

# Research Question:
# Does thinning decrease the probability of tree mortality # in wildfire?

treemortality <- read.csv("treemortality.csv")
head(treemortality)

## 2a) Fit a glm (using a probability distribution of your choice) that reflects the following research question (including thinning as your only predictor and mortality as your response): Do forest thinning treatments reduce the probability of tree mortality?

# Fit glm:
# Explanatory Variable: Thinning (Yes = 1, No = 0)
# Response Variable: Tree Mortality (Survived = 1, Died = 0)

# Binary Probability Distribution
# Use plogis to transform

mod.bi <- glm(mortality~thinning, 
           data=treemortality, family="binomial"(link="logit"))

summary(mod.bi)

# Transform intercept back to probability scale

# Probability of tree mortality regardless of thinning treatment
plogis(coef(mod.bi)["(Intercept)"]) # same as plogis(0.9933)
# = 0.7297297
 
# Probability of mortality of trees that received thinning - equation of the line
plogis(0.9933+-1.8559*1)
# = 0.2967964

# Plot to visualize slope 

plot_predictions(mod.bi, condition="thinning") + # specify which slope to plot (matters when we add more X's!)
  ylab("Probability of Tree Mortality") + # Add label axes
  xlab("Thinning Experiment") + # Add label axes
  theme_bw()

# Explore model fit
## ROC for model fit from the pROC package
library(pROC)
test_prob <- predict(mod.bi, type = "response")
test_roc <- roc(treemortality$mortality 
                ~ test_prob, plot = TRUE,  
                print.auc = TRUE)

### Interpret the results of the glm by writing 2-4 sentences about the biological significance of the effect of thinning, including descriptions of the sizes of the effect on the scale of the response variable, evidence for/against the hypothesis, and a metric of model fit.
 
# Thinning treatments had a significant effect on reducing the probability of tree mortality (p-value < 0.05). Without a thinning treatment, the baseline probability of tree mortality was 0.729 or roughly 73%. Including thinning treatment reduce the probability of tree mortality down to 0.297 or approximately 30%. Metrics of model fit were relatively high (71.0%) indicating that the results of our model were better than what we would predict by chance alone. 

## 2b) The researchers explicitly considered the potential for confounding relationships related to tree size in their design and randomized their post-fire sampling by tree size. Given this information, do the researchers need to incorporate tree size into their glm to accurately estimate the effect of thinning? Why or why not?

# Based on the information that the researchers accounted for potential confounding variables in their design by incorporating randomized sampling by tree size, it should not be necessary for them to include tree size in their glm. However, if they wanted to improve model fit they could try including tree size. 

## 2c) Refit the model from 2a to include the necessary variables to minimize bias in our estimation of the “thinning” variable, based on the reviewer’s proposed DAG (above). 

# Plan: ------
# Fit full dag model to include three explanatory variables:
# 1. thinning
# 2. road distance
# 3. slope

# To interpret the impact of of the unthinned treatments on tree mortality while including road distance and slope as explanatory variables, I will hold thinning at zero and the slope of the road distance and slope of the slope at their mean. 

# To interpert the impact of the thinned treatment on tree mortality while including road distance and slope as extra explanatory variables, I will hold the thinning treatment at 1, and the slope of the raod distance and slope of the slops at their mean. 

# -------------
# DAG Model: (still binary)

bigmod.bi <- glm(mortality~thinning + roaddist + slope, 
               data=treemortality, family="binomial"(link="logit"))

# View summary table
summary(bigmod.bi)

# Save coefficient estimates for intercept, the slope of thinning (slopethin), the slope of road distance (sloperoad), and the slope of slope (slopeslope)

int<-coef(bigmod.bi)["(Intercept)"]
slopethin<-coef(bigmod.bi)["thinning"]
sloperoad<-coef(bigmod.bi)["roaddist"]
slopeslope<-coef(bigmod.bi)["slope"]

# Calculate tree mortality for unthinned forests
# Use plogis to transform back to odds scale
# Hold slope of thinning treatment at zero (unthinned)
plogis(int + (slopethin*0)+(sloperoad*mean(treemortality$roaddist))+slopeslope*mean(treemortality$slope))
# = 0.528606, 53%

# Calculate tree mortality for thinned forests
# Use plogis to transform back to odds scale
# Hold slope of thinning treatment at 1 (thinned)
plogis(int + (slopethin*1)+(sloperoad*mean(treemortality$roaddist))+slopeslope*mean(treemortality$slope))
# = 0.3096572, 31%

# Plot by condition of thinning (but model also accounts for raod distance and slope)
plot_predictions(bigmod.bi, condition="thinning") + # specify which slope to plot (matters when we add more X's!)
  ylab("Probability of Tree Mortality") + # Add label axes
  xlab("Thinning Experiment") + # Add label axes
  theme_bw()

# Make predictions 
predictions(bigmod.bi, newdata=data.frame(thinning=c("thinned", "unthinned"), roaddist=mean(treemortality$roaddist), slope=mean(treemortality$slope)))

## ROC for model fit from the pROC package
test_prob <- predict(bigmod.bi, type = "response")
test_roc <- roc(treemortality$mortality 
                ~ test_prob, plot = TRUE,  
                print.auc = TRUE)
#AUC = 0.96

### Does the effect of “thinning” change? If so, describe the degree of change and why the two models may differ in their conclusions. If needed, modify your model interpretation from 2a.

# Road distance and slope both have a significant effect on the effect of thinning on tree mortality (p-value <0.05). By including both of these variables, tree mortality in unthinned forests is reduced from ~ 73% to 53% and tree mortality in thinned forests increases very slightly from 29.6% to 30.9%. The model fit also increases significantly from an AUC of 71.0% 95.6, indicating the model is well fit the results are very likely not due to chance. 

