#1) 
#DATA IMPORT 
newborns=read.csv("neonati.csv", sep = ",", stringsAsFactors = T) 
nrow(newborns)
attach(newborns)
summary(newborns)
library(moments)
library(dplyr)
library(ggplot2)
library(utils)
library(tidyverse)
library(car)
library(lmtest)
library(plotly)
library(rgl)
#------------------------------------------------------------------------------------------------------------------

#after having done the analysis of the residuals, we find that there are relevant outliers in line 1551
#let's return here to delete this line

lines_to_delete=c(1551)
newborns <- newborns %>% filter(!(row_number() %in% lines_to_delete))




#I detected two anomalies in the mother's age, where 0 and 1 year are indicated as mother's age, so I removed that information

anomalous_lines <- which(newborns$Anni.madre <2)
newborns_clean <- subset(newborns, !rownames(newborns) %in% anomalous_lines)
newborns <- newborns_clean

attach(newborns_clean)
summary(newborns)
nrow(newborns)


#----------------------------------------------------------------------------------------------------------

#2)
# DATASET DESCRIPTION
#The dataset has 10 variables: 
#The age of the mother of the newborns in the sample, a discrete quantitative variable
#The number of pregnancies, also a discrete quantitative variable
#Mother smoker or not. This is a dummy variable to which the SI is assigned the value of 1 and NO the value of 0

smokers=sum(newborns$Fumatrici)
n=length(newborns$Fumatrici)
non_snokers=n-smokers
#There are 104 smoking mothers and 2394 non-smoking mothers
#The gestation period in weeks, a discrete quantitative variable
#The weight of the newborn, a continuous quantitative variable
#Length, continuous quantitative variable
#The diameter of the skull, continuous quantitative variable
#the type of birth is a qualitative variable on a nominal scale, with Natural or Cesarean mode
table(Tipo.parto)
percentage_ces=728/n
percentage_ces
percentage_nat=1772/n
#with the Tipo.parto table we see that in the sample, 728 women performed cesarean section (29.14%)
#while 1770 had a natural birth (70.93%)
#The hospital variable concerns the hospitals in which the births took place
hospital=table(Ospedale)
mean(hospital)
#hospital Table tells us that the births took place in 3 hospitals with an average of 833 births per hospital
#"sesso" indicates the gender of newborns
gender=table(Sesso)
gender
#In the sample there are 1255 females and 1243 males
#The study aims to verify whether the recorded variables can influence each other,
#for example whether the fact that the mother is a smoker can influence other variables, such as the period of
#gestation, the weight, size or even sex of the newborn. The same consideration can be made
#for the mother's years or for the number of pregnancies, assuming that these factors can influence
#the characteristics of the newborn. Furthermore, the study wants to create a regression model that contains the
#Significant and influential #variables capable of making predictions on the characteristics of the unborn child.
#----------------------------------------------------------------------------------------------------------------------------

#3) DESCRIPTIVE ANALYSIS
#WEIGHT
View(table(Peso))
skewness(Peso)
kurtosis(Peso)
plot(density(Peso))

range_weight=c(800, 1600, 2400, 3200, 4000, max(Peso) )
weight_labels=c( "0.8 kg - 1.6 kg", 
                  "1.6 kg - 2.4 kg", "2.4 kg - 3.2 kg",
                  "3.2 kg - 4 kg", " + 4 kg")

weight_cl=cut(Peso, breaks = range_weight, labels = labels_peso)
table(weight_cl)

library(ggplot2)
ggplot(data=newborns_clean)+
  geom_bar(aes(x=peso_cl),
           col="black",
           fill="lightblue")+
  labs(title = "Weight distribution of newborns",
       x="Weight classes",
       y="Observations number")
 
#after dividing the weight variable into classes, we see that the modal class is that
#which between 3.2 and 4 kg with 1277 observations, while the class with fewer observations
#is that of newborns between 0.8 and 1.6 kg.
#The average weight is 3284, the median is 3300, as is the mode.
#the weight distribution is slightly negative asymmetric (mean<median=mode) )
#and leptokurtic.

    

#LENGTH
View(table(Lunghezza))
skewness(Lunghezza)
kurtosis(Lunghezza)
plot(density(Lunghezza))

range_length=c(300, 350, 400, 450, 500, max(Lunghezza) )
labels_length=c( "300 mm - 350 mm", 
                  "350 mm - 400 mm", "400 mm - 450 mm",
                  "450 mm - 500 mm", "+ 500 mm")

length_cl=cut(Lunghezza, breaks = range_length, labels = labels_length)
table(length_cl)

library(ggplot2)
ggplot(data=neonati)+
  geom_bar(aes(x=length_cl),
           col="black",
           fill="lightblue")+
  labs(title = "Length distribution of newborns",
       x="Length classes",
       y="Observations number")  
  
#the length is also negative asymmetric and leptokurtic. The mean is 494.7 mm, the median
#is 500mm and fashion is also 500mm.
#the class with the highest number of observations detected is in fact that of
#newborns with length between 450 and 500 mm, with 1465 observations,
#even if numerous observations are found in the class with the longest length
#of 500 mm

#SKULL DIAMETER
View(table(Cranio))
skewness(Cranio)
kurtosis(Cranio)
plot(density(Cranio))

range_skull=c(200, 250, 300, 350, 400)
labels_skull=c( "200 mm - 250 mm", 
                       "250 mm - 300 mm", "300 mm - 350 mm",
                       " 350 mm - 400 mm")

skull_cl=cut(Cranio, breaks = range_skull, labels = labels_skull)
table(skull_cl)


ggplot(data=neonati)+
  geom_bar(aes(x=cranio_cl),
           col="black",
           fill="lightblue")+
  labs(title = "Skull diameter distribution of newborns",
       x="Skull diameter classes",
       y="Observations number")  

#for the diameter of the skull the distribution is rather symmetrical, average, median
#and fashion are the same, i.e. 340 mm.
#The modal class is the one that includes children with a skull between 300 and 350 mm

#Graphical analysis of weight in relation to the mother's age and the type of birth

ggplot(data = newborns) +
  geom_point(aes(x = Peso, y = Anni.madre, color = Tipo.parto)) +
  labs(title = "Mother's age - weight - type of birth",
       x = "Weight", y = "Mother's age") +
  scale_color_manual(values = c("Nat" = "blue", "Ces" = "red"),
                     labels = c("Natural", "Cesarean"))



#At first glance from the scatterplot that relates the mother's age to the weight of the newborn
#there does not appear to be a consistent relationship between the mother's age and the weight of
#baby, regardless of the mother's age, the weight does not seem to be affected
#particularly.
model_weight_mother <- lm(Peso~Anni.madre, newborns)
Anova_result=Anova(model_weight_mother)
print(Anova_result)
#also the Anova test confirms that there is no significant relationship between the weight of the newborn and the mother's age
#since the Anova test relating the mother's weight and age returns a p-value of 0.234

#Graphic analysis of weight in relation to weeks of gestation and type of birth

ggplot(neonati, aes(x = factor(Gestazione), y = Peso, fill=Tipo.parto)) +
  geom_boxplot()+
  xlab("Gestation weeks") +
  ylab("Newborns weight in g") +
  ggtitle("Newborns weight - gestation weeks")

model_weight_gestation <- lm(Peso~Gestazione, newborns)
result_Anova_gest=Anova(model_weight_gestation)
print(result_Anova_gest)

#by graphically correlating the weeks of gestation with the weight, let's see
#as the weeks of gestation increase, the weight of the newborn grows, the weight
#median is always greater as the weeks increase. There do not seem to be substantial ones
#weight differences between natural and cesarean births. Premature births all happen
#naturally up to 30 weeks
#Confirming the graphic analysis that establishes the relationship between weeks of gestation and years
#of the mother, there is also the Anova test which returns a very small p-value (2.2e-16), which gives us
#leads us to accept the hypothesis that the weight of the newborn varies depending on the weeks of gestation
#-----------------------------------------------------------------------------------------------------------------
#4)
# HYPOTHESIS OF EQUALITY BETWEEN SAMPLE AND POPULATION AVERAGE
shapiro.test(Peso)
t.test(Peso, mu=3284)     #p-value=0.986 we accept the null hypothesis of equality between means

t.test(Lunghezza, mu=494) #p-value=0.186 we accept the null hypothesis of equality between means

#Not having the average weight and length of the population, we use the average sample

#--------------------------------------------------------------------------------------------------------------------------
#5)
#DIFFERENCES BETWEEN THE TWO GENDERS
ggplot(newborns, aes(x = factor(Sesso), y = Peso)) +
  geom_boxplot()+
  xlab("Gender") +
  ylab("Newborn's weight in grams") +
  ggtitle("Relation weight-gender")

male_weight <- subset(newborns, Sesso == "M")$Peso
average_male_weight <- mean(male_weight)  #3408.49
sd_male_weight = sd(male_weight)        #493.9
var(male_weight)

female_weight=subset(newborns, Sesso=="F")$Peso
average_female_weight=mean(female_weight)          #3161.06
sd_femalei_weight=sd(female_weight)               #526.51
var((female_weight))

t.test(Peso ~ Sesso, data = newborns, var.equal = F)   #p-value=2.2e-16

#doing the t-test for the length between males and females, the p-value turns out to be equal to 2.2e-16,
#a very low value that makes us reject the null hypothesis of equality between means. Therefore we can
#say that the average weights of males and females are significantly different


t.test(Lunghezza~Sesso, data = newborns, var.equal=F)     #p-value=2.2e-16
ggplot(newborns, aes(x = factor(Sesso), y = Lunghezza)) +
  geom_boxplot()+
  xlab("Gender") +
  ylab("Newborn's Length in mm") +
  ggtitle("Relation Length - Gender")
#also for the length of newborns the p-value of the t-test turns out to be very small, hence the averages
#for length for males and females are significantly different

t.test(Cranio~Sesso, data = newborns, var.equal=F)       #p-value=1.414e-13
 ggplot(newborns, aes(x = factor(Sesso), y = Cranio)) +
  geom_boxplot()+
  xlab("Gender") +
  ylab("Skull diameter in mm") +
  ggtitle("Relation Skull diameter - Gender")

#The average diameters of the skulls of males and females are also significantly different

#--------------------------------------------------------------------------------------------------------------------

#6) 
#TABLE AND GRAPH OF FREQUENCY BORN WITH CESAREAN AND NATURAL DELIVERY BY HOSPITAL

frequency_birth_table <- table(newborns$Tipo.parto, newborns$Ospedale)

View(frequency_birth_table)

distr_freq_birth_type=as.data.frame(frequency_birth_table)
colnames(distr_freq_birth_type) <- c("Birth.type", "Hospital", "Frequences")



#pivottable with tidyverse to better aggregate data
birth_type_table <- distr_freq_birth_type %>%
  pivot_wider(names_from = Hospital, values_from = Frequences)

rownames(birth_type_table) <- c("Natural", "Caesarean")

#side-by-side bar graph of the absolute frequencies of cesarean and natural deliveries by hospital

x11()
ggplot(data=distr_freq_tipo.parto) +
  geom_bar(aes(x = Hospital, y=Frequences, fill=Birth.type),
           stat = "identity", position = "dodge") +
           labs(x = "Hospital", y = "Frequences", fill = "Birth type") +
  scale_y_continuous(breaks = seq(0, 2500, 100))+
  theme(panel.spacing = unit(0.3, "cm"),
        legend.position = c(0.1, 0.85),
        legend.direction = "vertical") 

#verification of the hypothesis of a greater number of caesarean sections in some hospitals
table_caesarean <- table(newborns$Tipo.parto=="Ces", newborns$Ospedale)

 chisq.test(table_caesarean)
#Pearson's Chi-squared test
#data:  table_caesarean
#X-squared = 1.083, df = 2, p-value = 0.5819
 
# In this case, having a p-value of 0.5819, which is higher than the usual level
#of significance of 0.05, we cannot reject the null hypothesis.
#This suggests that there is no significant difference in the proportion of parts
#caesareans among the hospitals analyzed, so it cannot be said that more deliveries are being done
 #cesarean sections in some hospitals

#---------------------------------------------------------------------------------------------------------

#MULTIDIMENSIONAL ANALYSIS
 
#1)
# Investigation of the variables in relation to the weight response variable
x11()
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor,...)
{
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)

  txt <- paste0(prefix, format(c(r, 0.123456789), digits = digits)[1])
  
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.8, 0.8, txt, cex = 1.5, cex.labels=1.5)
 }
 
 pairs(newborns, lower.panel = panel.cor, upper.panel = panel.smooth, cex.labels=1.5)
 

#from the grid showing the correlation coefficients between all the variables, we see that:
 #- Mother's years have virtually no correlation with any of the variables
 # quantities present.
 
 #- Even the number of pregnancies has practically no correlation with the other variables
 
 #- Weeks of gestation have a positive correlation with length, weight and diameter
 # of the newborn's skull
 
 #Weight, as well as with the weeks of gestation, has a very positive correlation with
 #lengths and diameter of the skull

 #At the end of the analysis we can therefore say that the variables all present a strong coefficient
 #of correlation are weeks of gestation, weight, length and diameter of the skull, all the others
 #quantitative variables do not appear to be correlated with each other


#Let's now analyze the qualitative variables:

#1 - GENDER
par(mfrow=c(1,2)) 
boxplot(Peso)
boxplot(Peso~Sesso)
 
t.test(Peso~Sesso)   #p-value=2.2e-16
t.test(Lunghezza~Sesso) #p-value=2.2e-16
t.test(Cranio~Sesso)    #p-value=1.414e-13
t.test(Anni.madre~Sesso) #p-value=0.636
t.test(Gestazione~Sesso) #p-value=2.096e-11
t.test(Fumatrici~Sesso)  #p-value=0.51
t.test(N.gravidanze~Sesso)  #p-value=0.21
#the gender variable shows a very low p-value in relation to all other variables, except with
#the mother's years, with the mother smoking or not and with the number of pregnancies, with which it doesn't seem like it
#there is a connection.
#so when compared to quantitative variables, the differences between males and
#females seem to be relevant, so it is good to include this variable in the model.


#-TYPE OF BIRTH
t.test(Peso~Tipo.parto)
t.test(Lunghezza~Tipo.parto)
t.test(Cranio~Tipo.parto) 

#the type of birth always returns a high p-value with quantitative variables, except with Length
#but it's very borderline, but in any case it doesn't seem to be such a relevant variable in relation
#to the others.
 
ggplot(newborns, aes(x = factor(Tipo.parto), y = Peso)) +
  geom_boxplot()+
  xlab("Tipo di parto") +
  ylab("peso in g") +
  ggtitle("Relation birth type - weight")

ggplot(newborns, aes(x = factor(Tipo.parto), y = Lunghezza)) +
  geom_boxplot()+
  xlab("Birth type") +
  ylab("Length in mm") +
  ggtitle("Relatione birth type - length")


#2)
#Creating the model with all the variables

mod1=lm(Peso~. , data=newborns)
summary(mod1)

#from the summary of the model which sets the weight as the response variable and all the explanatory variables
#the other variables available, we see that the variables that turn out to be very significant
#are the weeks of gestation, the length of the newborn, the diameter of the skull and the sex.
#The number of pregnancies and the type of birth also seem to have a significance, albeit minor.
#The betas of the most significant variables tell us that:

#For every additional mm of length, the weight increases by 10.29 g
#For every additional mm in diameter of the skull, the weight increases by 10.47 g
#For each additional week of gestation, the weight increases by 32.57 g
#SessoM with beta of 77.57 indicates that males weigh 77.57 g greater than females
#females.
#for each additional pregnancy the weight increases by 11.38 g
#The weight of newborns born naturally appears to be 29.6 g greater than those born from
#cesarean birth, even if from the previous analysis there does not appear to be such a strong correlation between
#type of birth and weight.

#The adjusted R-squared of the model is 0.7278, an explained variability of 73%, which is not bad
#but the model can be improved.


#3)
#Search for the best model

mod2=update(mod1, ~.-Ospedale)
summary(mod2)
#in model 2 the variable "smokers" continues to be insignificant, but otherwise not
#a lot has changed

anova(mod1, mod2) #the anova test has a p-value of 0.1036, which tells us that the information
                  #we removed, i.e. hospital, could provide important information
BIC(mod1, mod2)   #however the BIC is lower in model 2, which would lead us to prefer it
AIC(mod1, mod2)   #the AIC, however, is lower in model 1
car::vif(mod2)    #all VIFs are below 5, so there is no multicollinearity problem


mod3=update(mod2, ~.-Fumatrici)
summary(mod3)
anova(mod2,mod3)         
BIC(mod1, mod2, mod3)   #the lowest BIC is model 3
AIC(mod1, mod2, mod3)   #the AIC continues to be lower in model 1 and the same in the other two
#the R square remains unchanged
car::vif(mod3)          #here too the vifs are all below 5


mod4=update(mod3, ~.-Anni.madre)
summary(mod4)
anova(mod3, mod4)   #anova test has a p-value of 0.44, the variable mother's years could therefore
                    #do not add relevant information
BIC(mod1,mod2,mod3,mod4)  #the BIC of model 4 is the lowest
AIC(mod1,mod2,mod3,mod4)  #the AIC of model 4 is also lower than that of 2 and 3
vif(mod4)                 #vif all below 5


mod5=update(mod4,~.-Tipo.parto)
summary(mod5)
anova(mod4, mod5)
BIC(mod1,mod2,mod3,mod4,mod5)
AIC(mod1,mod2,mod3,mod4,mod5)
vif(mod5)

#the chosen model is number 5
#mod5=Weight ~ No. of pregnancies + Gestation + Length + Skull + Sex)
#The output represents the results of the linear regression fitted to Model 5.
#The "Estimate" column provides estimates of the regression coefficients for each predictor variable
#in the model. For example, the intercept is estimated at -6681.7251, the coefficient of "N.pregnancies" is estimated
#at 12.4554, the coefficient of "Gestation" is estimated at 32.3827, the coefficient of "Length" is estimated
#at 10.2455, the coefficient of "Skull" is estimated at 10.5410 and the coefficient of "SexM" is estimated at 77.9807.
#The "Std. Error" column represents the standard error of the coefficient estimates, which measures their
#precision.
#The "t value" column indicates the t value calculated to test the hypothesis that the coefficient is equal
#to zero.
#The "Pr(>|t|)" column provides the p-value associated with the t-test, which measures statistical significance
#of the coefficient.
#The coefficients of the variables "No. of pregnancies", "Gestation", "Length", "Skull" and "SexM" are all
#statistically significant, since their p-values ​​are less than 0.05.
#The "Residual standard error" represents the residual standard error, which provides an estimate of the
#standard deviation of residuals.
#The "Multiple R-squared" represents the proportion of total variance of the response variable
#explained by the model. In this case it is 0.727, which indicates that the model explains approximately 72.7% of the
#change in response variable.
#The "Adjusted R-squared" takes into account the number of predictors in the model and provides a measure of
#adaptation of the model also considering the complexity of the model.
#Finally, the "F-statistic" is the F value calculated to test the null hypothesis that all coefficients
#of regression are zero. The p-value associated with the F-statistic is very small (p-value < 2.2e-16),
#which indicates that the model as a whole is statistically significant.
#In summary, the results indicate that Model 5 is statistically significant and explains well
The change in the "Weight" response variable based on the predictor variables is significant
#included in the model.


#best model with package MASS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# stepwise method
stepwise.mod=MASS::stepAIC(mod1,
                           direction = "both",
                           k=2)

summary(stepwise.mod)
anova( mod5, stepwise.mod)
BIC(mod1,mod2,mod3,mod4,mod5,stepwise.mod)  #model 5 has lowest BIC 
AIC(mod1,mod2,mod3,mod4,mod5, stepwise.mod)  #model stepwise.mod has lowest AIC

par(mfrow=c(2,2))

plot(stepwise.mod)

lev_stepwise=hatvalues(stepwise.mod)
plot(lev_stepwise)
p_stepwise=sum(lev_stepwise)
n=nrow(neonati)
soglia_stepwise=2*p_stepwise/n
abline(h=soglia_stepwise, col="red")

lev[lev_stepwise>soglia_stepwise]

plot(rstudent(stepwise.mod))
abline(h=c(-2,2), col="red")
outlierTest(stepwise.mod)
vif(stepwise.mod)
#MASS's stepwise METHOD gives us this model:
#WEIGHT=N. OF PREGNANCIES+GESTATION+LENGTH+SKULL+TYPE OF DELIVERY+HOSPITAL+SEX
#the lowest BIC continues to be that of model 5, while the stepwise.mod model has the AIC
#improve.
#vifs are also all below the threshold of 5.
#in the anova test the p-value the p-value is very low (0.001405), which suggests that
#the addition of the predictor variables in Model 2 is statistically significant and improves
#significantly the ability of the model to explain the variation in the response variable.

#THE BEST MODEL FOR THE ANOVA TEST SEEMS TO BE THE ONE IDENTIFIED BY THE MASS PACKAGE, BUT
#IT ADDS TWO VARIABLES SUCH AS THE TYPE OF DELIVERY AND THE HOSPITAL, WHICH WE HAD SEEN NOT TO HAVE A LINK
#SO STRONG WITH THE WEIGHT, ALSO WE DO NOT HAVE THIS INFORMATION FOR THE PURPOSE OF OUR PREDICTION AND
#SINCE THE VARIABLES ARE NON-QUANTITATIVE, I WOULD NOT ADD THEM INTO THE MODEL, GOING BACK TO PREFERRING MODEL 5


#--------------------------------------------------------------------------------------------------------


#4)
#Presence of interactions and non-linear effects

interactions=lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + Sesso + Gestazione*Lunghezza, data = newborns)
summary(interactions)
#By adding the interaction between Gestation and Length, the latter loses
#significance, so this interaction should not be added to the model.

interaction2=lm(Peso~N.gravidanze+Gestazione+Lunghezza+Cranio+Sesso+Cranio*Lunghezza, data = neonati)
summary(interaction2)
#Even adding the interaction between Length and Skull the Length loses
#significance

#Graphical analyzes can also be done between the Weight response variable and the variables
#independent of the model
par(mfrow=c(2,3))
plot(newborns$Gestazione, newborns$Peso, xlab = "Gestation", ylab = "Weight", main = "Scatterplot Gestation-Weight")
plot(newborns$Lunghezza, newborns$Peso, xlab = "Length", ylab = "Weight", main = "Scatterplot Length-Weight")
plot(newborns$N.gravidanze, newborns$Peso, xlab = "N.pregnancies", ylab = "Weight", main = "Scatterplot N.pregnancies-Weight")
plot(newborns$Sesso, newborns$Peso, xlab = "Gender", ylab = "Weight", main = "Scatterplot Gender-Weight")
plot(newborns$Cranio, newborns$Peso, xlab = "Skull", ylab = "Weight", main = "Scatterplot Skull-Weight")

#From the graphs that relate the independent variables to the response variable, they appear
There are regular patterns, so there don't seem to be any non-linear effects.

interactions_model<- lm(Peso ~ N.gravidanze + poly(Gestazione, 2) + poly(Lunghezza, 3) + Cranio + Sesso + Gestazione:Lunghezza, data = newborns)
summary(interactions_model)


#-------------------------------------------------------------------------------------------------------------------
#5)
#Residuals analysis

par(mfrow=c(2,2))

plot(mod5)
#From the model graph we see that in the first graph the points are randomly dispersed around
#to the average of zero. In the second graph the residuals follow the bisector and are therefore distributed
#normally. The third graph does not follow a precise pattern, hence all the requirements for the residuals
#they seem to be respected.
#In the last graph where the potential influential values ​​are shown, just one observation yes
#finds in the area between 0.5 and 1 of the Cook distance

#leverage values
lev=hatvalues(mod5)
plot(lev)
p=sum(lev)
n=nrow(neonati)
theshold=2*p/n
abline(h=theshold, col="red")

lev[lev>theshold]

#outliers
plot(rstudent(mod5))
abline(h=c(-2,2), col="red")
outlierTest(mod5)
#there are three outlier values, and they are recordings number 1551, 155 and 1306

#Cook distance
cook=cooks.distance(mod4)
plot(cook)
max(cook)
#the only anomalous value is reported by observation number 1551, which moves away from the distance Cook's 
#so let's go back to the beginning and remove this row from the data

bptest(mod5)      #p-value of 2.2e-16, so there is homoscedasticity (0.045 after removing outliers)
dwtest(mod5)      #the p-value is 0.1209, there is no autocorrelation problem
shapiro.test(residuals(mod5))  #the p-value is 2.2e-16, so there is no normality of the residuals
plot(density(residuals(mod5)))
qqnorm(residuals(mod5))
qqline(residuals(mod5))
plot(density(residuals(mod5)))
mean(residuals(mod5))
#from the plots of the residuals, they appear to be normally distributed, both from the density of the residuals, which they have
#an average of practically zero (1.56415e-14). Even from the Q-Q plot they appear to follow the bisector
#and therefore a normal distribution.
#After removing row 1551 that contained outliers, the homoscedasticity test always rejects the hypothesis
#nothing, but it does so with a p-value just below the 5% threshold. Autocorrelation is absent
#and despite the Shapiro test I reject the hypothesis of normality, the graphical analysis seems to suggest normality
#of the distribution of residuals, with a mean very close to zero

#--------------------------------------------------------------------------------------------------------------------------

#6)
#Goodness of the model
summary(mod5)

#The adjusted R square of the chosen model is equal to 0.736, therefore an explained variability of 73.6%.
#After removing the row presenting outlier values, the residuals analysis has a bptest with p-value
#equal to 0.045, therefore very close to the 5% threshold, and more acceptable than the dataframe
#containing the line with outliers that exceeds the Cook distance. There is no autocorrelation and despite it
#Shapiro test rejects the hypothesis of normality of the residuals, the graphic representations seem all
#demonstrate normality of the residuals.
#adjusted R-square represents the proportion of variance in infant weight explained by the model.
#In this case, the R-squared is 0.7372, which indicates that the model explains approximately 73.72% of the variation
#in the weight of newborns.
#The significance test shows that all the variables in the model are significant as they have
#p values ​​below the significance level (0.05).
#This suggests that all variables considered are statistically significant to explain
#the weight of newborns.
#The p-value of the model is very small, so overall the model is significant

#--------------------------------------------------------------------------------------------------------------------------

#7)
#Weight prediction

#Let'spredict the weight of a newborn baby with her mother in her third pregnancy at week 39
#Not having available ultrasound data regarding the length and diameter of the skull
#which are variables present in the regression model, being missing values ​​due to missingness
#recording of these measurements by the ultrasound, we calculate a sample average of them for the
#females at week 39 of gestation with the mother in her third pregnancy, so as to enter these values
#in the model


length_female_week_39 <- subset(newborns, Gestazione == 39 & Sesso == "F" & N.gravidanze==3)$Lunghezza
average_length_female_week_39=mean(length_female_week_39)

skull_female_week_39<- subset(newborns, Gestazione ==39 & Sesso =="F" & N.gravidanze==3)$Cranio
average_skull_female_week_39=mean(skull_female_week_39)


preidction=predict(mod5, newdata = data.frame(N.gravidanze=3, 
                   Lunghezza=average_length_female_week_39, 
                   Cranio=media_cranio_femmine_sett_39, 
                   Gestazione=39, Sesso="F"))

prediction    #3225.44

#the regression model we have chosen, after removing the outlier values, shows us that the newborn
#having these characteristics it will weigh 3225.44 grams
#----------------------------------------------------------------------------------------------------------------

#8)
#Graphs


library(scatterplot3d)
scatterplot3d(x=Cranio, y=Gestazione, z=Peso, 
              color =  ifelse(Sesso == "M", "blue", "red"), size = 2,
              type = "h", main = " 3D graph newborn weight", 
              xlab = "Skull", ylab = "Gestation", 
              zlab = "Weight")
legend("bottomright", legend = c("Male", "Female"), 
fill = c("blue", "red"), bty = "n")


