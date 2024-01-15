income <- read.csv("tulot2017.csv",  dec = ".", sep=",", 
                   header=TRUE,
                   blank.lines.skip=TRUE, encoding="latin1")

election <- read.csv("ek2019.csv",  dec = ".", sep=",", 
                     header=TRUE,
                     blank.lines.skip=TRUE, encoding="latin1")


# Extract the unique values from the "Alue" column in each data frame
unique_income <- unique(income$Alue)
unique_election <- unique(election$Alue)

#find the municipalities that has no match on the other dataset
unmatched <- c(unique_income, unique_election)

#combine the datasets into a merged one, leaving out the unmatched municipalities using dplyr library
library(dplyr)
merged <- inner_join(income, election, by="Alue")
View(merged)

#for starters, we will make the correlation matrix, to quickly
#identify the variables that are related to the other.

sub <- subset(merged, select = -c(Alue, Vaalipiiri))
mcor <- cor(sub)
round(mcor, digits=3)
library(corrplot)
library(ggplot2)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         addCoef.col = "black",  number.cex =0.26, tl.cex=0.3)

#identify which has low correlation and which ones have high correlations
categorize <- function(matrix, marks) {
  num <- ncol(matrix)
  var <- colnames(matrix)
  
  very_high <- list()
  high <- list()
  moderate <- list()
  low<- list()
  negligible<- list()
  
  for (i in 1:(num - 1)) {
    for (j in (i + 1):num) {
      correlation <- matrix[i, j]
      relation <- paste(var[i], "-", var[j])
      
      if (!is.na(correlation)) {
        if (abs(correlation) >= marks[1]) {
          very_high <- append(very_high, relation)
        } else if (abs(correlation) >= marks[2]) {
          high <- append(high, relation)
        } else if (abs(correlation) >= marks[3]) {
          moderate <- append(moderate, relation)
        } else if (abs(correlation) >= marks[4]) {
          low <- append(low, relation)
        } else {
          negligible <- append(negligible, relation)
        }
      }
    }
  }
  
  relations <- list(
    "Very High Correlations" = very_high,
    "High Correlations" = high,
    "Moderate Correlations" = moderate,
    "Low Correlations" = low,
    "Negligible Correlations" = negligible
  )
  
  return(relations)
}
marks <- c(0.9, 0.7, 0.5, 0.3, 0)
res <- categorize(mcor, marks)
View(res)

#moderate relationships:


#plot the more than average relationships
strong <- merged[, c("VIHR", "PIR", "FP", "KOK", "KESK", "LIB", "Tulonsaajia", "Tulot", "Mediaanitulot", "Ansiotulot", "Verot", "Kunnallisvero", "Tulot_miinus_verot")]
scor <- cor(strong)
corrplot(scor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         addCoef.col = "black",  number.cex =0.4, tl.cex=0.5, cl.pos = "n")



#assess each important correlations

#####
#VIHR
#####
ggplot(merged, aes(x=Tulonsaajia, y=VIHR), label=Alue) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="VIHR support", x="Tulonsaajia")


cor(merged$Tulonsaajia, merged$VIHR)
model <- lm(VIHR ~ Tulonsaajia, data = merged)
summary(model)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9.5446 -1.9174 -0.5878  1.3177 11.2774 

#Coefficients:
#  Estimate  Std. Error t value            Pr(>|t|)
#(Intercept) 4.877771784 0.182757740   26.69 <0.0000000000000002
#Tulonsaajia 0.000051227 0.000004106   12.48 <0.0000000000000002

#(Intercept) ***
#  Tulonsaajia ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.939 on 293 degrees of freedom
#Multiple R-squared:  0.347,	Adjusted R-squared:  0.3447 
#F-statistic: 155.7 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Tulot, y=VIHR), label=Alue) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="VIHR support", x="Tulot")
cor(merged$Tulot, merged$VIHR)
model <- lm(VIHR ~ Tulot, data = merged)
summary(model)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-12.3065  -1.8131  -0.4762   0.9784  13.3770 

#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)
#(Intercept) -6.8626438  1.2292688  -5.583          0.000000054
#Tulot        0.0004714  0.0000457  10.315 < 0.0000000000000002

#(Intercept) ***
# Tulot       ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.115 on 293 degrees of freedom
#Multiple R-squared:  0.2664,	Adjusted R-squared:  0.2639 
#F-statistic: 106.4 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Mediaanitulot, y=VIHR), label=Alue) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="VIHR support", x="Mediaanitulot")
cor(merged$Mediaanitulot, merged$VIHR)
model <- lm(VIHR ~ Mediaanitulot, data = merged)
summary(model)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-6.8683 -1.8132 -0.4444  1.1110 14.5301 

#Coefficients:
# Estimate  Std. Error t value
#(Intercept)   -8.26595262  1.33085433  -6.211
#Mediaanitulot  0.00062444  0.00005905  10.575
#Pr(>|t|)    
#(Intercept)           0.0000000018 ***
#  Mediaanitulot < 0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.094 on 293 degrees of freedom
#Multiple R-squared:  0.2762,	Adjusted R-squared:  0.2738 
#F-statistic: 111.8 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Ansiotulot, y=VIHR), label=Alue) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="VIHR support", x="Ansiotulot")
cor(merged$Ansiotulot, merged$VIHR)
model <- lm(VIHR ~ Ansiotulot, data = merged)
summary(model)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-10.142  -1.754  -0.409   1.070  13.368 

#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)
#(Intercept) -8.1340948  1.2262294  -6.633       0.000000000158
#Ansiotulot   0.0005623  0.0000494  11.382 < 0.0000000000000002

#(Intercept) ***
#  Ansiotulot  ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.029 on 293 degrees of freedom
#Multiple R-squared:  0.3066,	Adjusted R-squared:  0.3042 
#F-statistic: 129.6 on 1 and 293 DF,  p-value: < 0.00000000000000022


ggplot(merged, aes(x=Verot, y=VIHR), label=Alue) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="VIHR support", x="Verot")
cor(merged$Verot, merged$VIHR)
model <- lm(VIHR ~ Verot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-13.5785  -1.8125  -0.5865   0.9862  13.5254 

#Coefficients:
#  Estimate Std. Error t value            Pr(>|t|)    
#(Intercept) -1.3526051  0.7219483  -1.874               0.062 .  
#Verot        0.0013300  0.0001321  10.065 <0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.135 on 293 degrees of freedom
#Multiple R-squared:  0.2569,	Adjusted R-squared:  0.2544 
#F-statistic: 101.3 on 1 and 293 DF,  p-value: < 0.00000000000000022


ggplot(merged, aes(x=Kunnallisvero, y=VIHR), label=Alue) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="VIHR support", x="Kunnallisvero")
cor(merged$Kunnallisvero, merged$VIHR)
model <- lm(VIHR ~ Kunnallisvero, data = merged)
summary(model)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-6.7030 -1.8189 -0.4105  1.2301 14.2420 

#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)
#(Intercept)   -4.1463254  0.9075589  -4.569           0.00000724
#Kunnallisvero  0.0028752  0.0002605  11.039 < 0.0000000000000002

#(Intercept)   ***
#  Kunnallisvero ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.057 on 293 degrees of freedom
#Multiple R-squared:  0.2937,	Adjusted R-squared:  0.2913 
#F-statistic: 121.9 on 1 and 293 DF,  p-value: < 0.00000000000000022


ggplot(merged, aes(x=Tulot_miinus_verot, y=VIHR), label=Alue) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="VIHR support", x="Tulot miinus verot")
cor(merged$Tulot_miinus_verot, merged$VIHR)
model <- lm(VIHR ~ Tulot_miinus_verot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-11.3785  -1.8067  -0.4504   0.9975  13.3586 

#Coefficients:
#  Estimate  Std. Error t value
#(Intercept)        -9.67114905  1.49377584  -6.474
#Tulot_miinus_verot  0.00072006  0.00006956  10.352
#Pr(>|t|)    
#(Intercept)                0.0000000004 ***
#  Tulot_miinus_verot < 0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.112 on 293 degrees of freedom
#Multiple R-squared:  0.2678,	Adjusted R-squared:  0.2653 
#F-statistic: 107.2 on 1 and 293 DF,  p-value: < 0.00000000000000022



#####
#PIR#
#####

ggplot(merged, aes(x=Tulonsaajia, y=PIR)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="PIR support", x="Tulonsaajia")
cor(merged$Tulonsaajia, merged$PIR)
model <- lm(PIR ~ Tulonsaajia, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.51700 -0.10505 -0.01388  0.09229  0.70256 

#Coefficients:
#  Estimate  Std. Error t value            Pr(>|t|)
#(Intercept) 0.190134085 0.010014506   18.99 <0.0000000000000002
#Tulonsaajia 0.000003322 0.000000225   14.77 <0.0000000000000002

#(Intercept) ***
#  Tulonsaajia ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1611 on 293 degrees of freedom
#Multiple R-squared:  0.4267,	Adjusted R-squared:  0.4248 
#F-statistic: 218.1 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Ansiotulot, y=PIR)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="PIR support", x="Ansiotulot")
cor(merged$Ansiotulot, merged$PIR)
model <- lm(PIR ~ Ansiotulot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.46875 -0.11002 -0.02268  0.06534  1.01352 

#Coefficients:
#  Estimate   Std. Error t value
#(Intercept) -0.515971817  0.073584820  -7.012
#Ansiotulot   0.000030857  0.000002964  10.409
#Pr(>|t|)    
#(Intercept)      0.0000000000163 ***
#  Ansiotulot  < 0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.1818 on 293 degrees of freedom
#Multiple R-squared:   0.27,	Adjusted R-squared:  0.2675 
#F-statistic: 108.4 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Kunnallisvero, y=PIR, label=Alue)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="PIR support", x="Kunnallisvero")
cor(merged$Kunnallisvero, merged$PIR)
model <- lm(PIR ~ Kunnallisvero, data = merged)
summary(model)

#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.35883 -0.11547 -0.02639  0.06870  1.06095 

#Coefficients:
#  Estimate  Std. Error t value
#(Intercept)   -0.29860438  0.05432837  -5.496
#Kunnallisvero  0.00015823  0.00001559  10.148
#Pr(>|t|)    
#(Intercept)           0.0000000844 ***
#  Kunnallisvero < 0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.183 on 293 degrees of freedom
#Multiple R-squared:  0.2601,	Adjusted R-squared:  0.2576 
#F-statistic:   103 on 1 and 293 DF,  p-value: < 0.00000000000000022


#Based on these confirmations, it seems that
#the number of taxable income recipients are 
#related to an increase in PIR support, as well
#as average earned income and average municipal 
#tax paid. It seems that these variables contribute
#a little to voting behavior. However, since the PIR 
#party gets contrastingly little votes as compared to 
#other parties, it cannot quite be concluded that the 
#number of taxable income recipients, average earned 
#income, and average municipal tax paid are big reasons 
#for individuals in municipalities to vote for PIR. 
#Thus, this will not be shown in the report


#####
#FP
######

ggplot(merged, aes(x=Tulonsaajia, y=FP)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="FP support", x="Tulonsaajia")
cor(merged$Tulonsaajia, merged$FP)
model <- lm(FP ~ Tulonsaajia, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.16001 -0.06213  0.01657  0.03888  0.24063 

#Coefficients:
#  Estimate   Std. Error t value            Pr(>|t|)
#(Intercept) 0.0573322179 0.0043709667   13.12 <0.0000000000000002
#Tulonsaajia 0.0000012341 0.0000000982   12.57 <0.0000000000000002

#(Intercept) ***
#  Tulonsaajia ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.0703 on 293 degrees of freedom
#Multiple R-squared:  0.3503,	Adjusted R-squared:  0.348 
#F-statistic:   158 on 1 and 293 DF,  p-value: < 0.00000000000000022

#Only one variable of the income level is very unlikely
#to be able to affect the outcome of FP voting. Therefore,
#this party will be excluded from the report.













######
#KOK
######
ggplot(merged, aes(x=Tulot, y=KOK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KOK support", x="Tulot")
cor(merged$Tulot, merged$KOK)
model <- lm(KOK ~ Tulot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-14.1081  -3.5886  -0.6876   3.3087  19.2953 

#Coefficients:
#  Estimate   Std. Error t value
#(Intercept) -13.57801384   2.09821074  -6.471
#Tulot         0.00094515   0.00007801  12.116
#Pr(>|t|)    
#(Intercept)       0.000000000408 ***
#  Tulot       < 0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.318 on 293 degrees of freedom
#Multiple R-squared:  0.3338,	Adjusted R-squared:  0.3315 
#F-statistic: 146.8 on 1 and 293 DF,  p-value: < 0.00000000000000022


ggplot(merged, aes(x=Mediaanitulot, y=KOK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KOK support", x="Mediaanitulot")
cor(merged$Mediaanitulot, merged$KOK)
model <- lm(KOK ~ Mediaanitulot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-13.4413  -3.7785  -0.7672   3.3601  21.2255 

#Coefficients:
#  Estimate  Std. Error t value
#(Intercept)   -13.6501285   2.3749356  -5.748
#Mediaanitulot   0.0011292   0.0001054  10.716
#Pr(>|t|)    
#(Intercept)           0.0000000227 ***
#  Mediaanitulot < 0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.522 on 293 degrees of freedom
#Multiple R-squared:  0.2816,	Adjusted R-squared:  0.2791 
#F-statistic: 114.8 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Ansiotulot, y=KOK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KOK support", x="Ansiotulot")
cor(merged$Ansiotulot, merged$KOK)
model <- lm(KOK ~ Ansiotulot, data = merged)
summary(model)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-12.487  -3.568  -0.725   3.402  20.058 

#Coefficients:
#  Estimate   Std. Error t value
#(Intercept) -13.69799820   2.17544983  -6.297
#Ansiotulot    0.00102843   0.00008764  11.735
#Pr(>|t|)    
#(Intercept)        0.00000000111 ***
#  Ansiotulot  < 0.0000000000000002 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.373 on 293 degrees of freedom
#Multiple R-squared:  0.3197,	Adjusted R-squared:  0.3174 
#F-statistic: 137.7 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Verot, y=KOK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KOK support", x="Verot")
cor(merged$Verot, merged$KOK)
model <- lm(KOK ~ Verot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-16.2004  -3.6930  -0.6179   3.5065  19.8353 

#Coefficients:
#  Estimate Std. Error t value            Pr(>|t|)    
#(Intercept) -2.3565761  1.2424236  -1.897              0.0588 .  
#Verot        0.0026336  0.0002274  11.582 <0.0000000000000002 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.396 on 293 degrees of freedom
#Multiple R-squared:  0.314,	Adjusted R-squared:  0.3117 
#F-statistic: 134.1 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Kunnallisvero, y=KOK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KOK support", x="Kunnallisvero")
cor(merged$Kunnallisvero, merged$KOK)
model <- lm(KOK ~ Kunnallisvero, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-12.1134  -3.7476  -0.6362   3.5746  21.5206 

#Coefficients:
#  Estimate Std. Error t value             Pr(>|t|)
#(Intercept)   -5.7560482  1.6360171  -3.518             0.000503
#Kunnallisvero  0.0050694  0.0004695  10.797 < 0.0000000000000002

#(Intercept)   ***
#  Kunnallisvero ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.51 on 293 degrees of freedom
#Multiple R-squared:  0.2846,	Adjusted R-squared:  0.2822 
#F-statistic: 116.6 on 1 and 293 DF,  p-value: < 0.00000000000000022


ggplot(merged, aes(x=Tulot_miinus_verot, y=KOK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KOK support", x="Tulot miinus verot")
cor(merged$Tulot_miinus_verot, merged$KOK)
model <- lm(KOK ~ Tulot_miinus_verot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-12.4563  -3.5105  -0.6163   3.2160  19.0067 

#Coefficients:
#  Estimate  Std. Error t value
#(Intercept)        -19.4057358   2.5404886  -7.639
#Tulot_miinus_verot   0.0014529   0.0001183  12.282
#Pr(>|t|)    
#(Intercept)           0.000000000000314 ***
#  Tulot_miinus_verot < 0.0000000000000002 ***
#  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 5.293 on 293 degrees of freedom
#Multiple R-squared:  0.3399,	Adjusted R-squared:  0.3376 
#F-statistic: 150.8 on 1 and 293 DF,  p-value: < 0.00000000000000022

######
#KESK#
######
ggplot(merged, aes(x=Tulot, y=KESK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KESK support", x="Tulot")
cor(merged$Tulot, merged$KESK)
model <- lm(KESK ~ Tulot, data = merged)
summary(model)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-30.060  -6.866   0.190   6.713  55.372 

#Coefficients:
#  Estimate Std. Error t value            Pr(>|t|)    
#(Intercept) 84.4890515  4.5773484   18.46 <0.0000000000000002 ***
#  Tulot       -0.0021767  0.0001702  -12.79 <0.0000000000000002 ***
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 11.6 on 293 degrees of freedom
#Multiple R-squared:  0.3583,	Adjusted R-squared:  0.3561 
#F-statistic: 163.6 on 1 and 293 DF,  p-value: < 0.00000000000000022

ggplot(merged, aes(x=Mediaanitulot, y=KESK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KESK support", x="Mediaanitulot")
cor(merged$Mediaanitulot, merged$KESK)
model <- lm(KESK ~ Mediaanitulot, data = merged)
summary(model)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-31.397  -7.370   0.138   7.104  34.760 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   95.4365547  4.7232225   20.21   <2e-16 ***
#  Mediaanitulot -0.0030835  0.0002096  -14.71   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 10.98 on 293 degrees of freedom
#Multiple R-squared:  0.4249,	Adjusted R-squared:  0.4229 
#F-statistic: 216.5 on 1 and 293 DF,  p-value: < 2.2e-16


ggplot(merged, aes(x=Ansiotulot, y=KESK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KESK support", x="Ansiotulot")
cor(merged$Ansiotulot, merged$KESK)
model <- lm(KESK ~ Ansiotulot, data = merged)
summary(model)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-32.840  -7.097   0.112   6.894  45.335 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 90.3216532  4.4961617   20.09   <2e-16 ***
#  Ansiotulot  -0.0025947  0.0001811  -14.32   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 11.11 on 293 degrees of freedom
#Multiple R-squared:  0.4119,	Adjusted R-squared:  0.4099 
#F-statistic: 205.2 on 1 and 293 DF,  p-value: < 2.2e-16

ggplot(merged, aes(x=Verot, y=KESK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KESK support", x="Verot")
cor(merged$Verot, merged$KESK)
model <- lm(KESK ~ Verot, data = merged)
summary(model)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-30.972  -7.051   0.118   6.555  60.449 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 58.7434751  2.7106328   21.67   <2e-16 ***
#  Verot       -0.0060839  0.0004961  -12.26   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 11.77 on 293 degrees of freedom
#Multiple R-squared:  0.3392,	Adjusted R-squared:  0.3369 
#F-statistic: 150.4 on 1 and 293 DF,  p-value: < 2.2e-16

ggplot(merged, aes(x=Kunnallisvero, y=KESK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KESK support", x="Kunnallisvero")
cor(merged$Kunnallisvero, merged$KESK)
model <- lm(KESK ~ Kunnallisvero, data = merged)
summary(model)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-35.798  -6.745  -0.261   6.384  34.082 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   74.345723   3.223252   23.07   <2e-16 ***
#  Kunnallisvero -0.013979   0.000925  -15.11   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 10.86 on 293 degrees of freedom
#Multiple R-squared:  0.438,	Adjusted R-squared:  0.4361 
#F-statistic: 228.4 on 1 and 293 DF,  p-value: < 2.2e-16


ggplot(merged, aes(x=Tulot_miinus_verot, y=KESK)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="KESK support", x="Tulot miinus verot")
cor(merged$Tulot_miinus_verot, merged$KESK)
model <- lm(KESK ~ Tulot_miinus_verot, data = merged)
summary(model)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-29.504  -6.770   0.521   6.810  51.449 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        97.7984155  5.5442806   17.64   <2e-16 ***
#  Tulot_miinus_verot -0.0033409  0.0002582  -12.94   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 11.55 on 293 degrees of freedom
#Multiple R-squared:  0.3637,	Adjusted R-squared:  0.3615 
#F-statistic: 167.5 on 1 and 293 DF,  p-value: < 2.2e-16





######
#LIB##
######
ggplot(merged, aes(x=Tulot, y=LIB)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="LIB support", x="Tulot")
cor(merged$Tulot, merged$LIB)
model <- lm(LIB ~ Tulot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.33730 -0.05029 -0.01664  0.02988  0.43316 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.909e-01  3.143e-02  -9.256   <2e-16 ***
#  Tulot        1.312e-05  1.169e-06  11.222   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.07966 on 293 degrees of freedom
#Multiple R-squared:  0.3006,	Adjusted R-squared:  0.2982 
#F-statistic: 125.9 on 1 and 293 DF,  p-value: < 2.2e-16

ggplot(merged, aes(x=Mediaanitulot, y=LIB)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="LIB support", x="Mediaanitulot")
cor(merged$Mediaanitulot, merged$LIB)
model <- lm(LIB ~ Mediaanitulot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.14938 -0.05262 -0.01074  0.03041  0.46175 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -3.271e-01  3.410e-02  -9.593   <2e-16 ***
#  Mediaanitulot  1.724e-05  1.513e-06  11.398   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.07929 on 293 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3048 
#F-statistic: 129.9 on 1 and 293 DF,  p-value: < 2.2e-16


ggplot(merged, aes(x=Ansiotulot, y=LIB)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="LIB support", x="Ansiotulot")
cor(merged$Ansiotulot, merged$LIB)
model <- lm(LIB ~ Ansiotulot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.26841 -0.04736 -0.01320  0.02691  0.44387 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -3.184e-01  3.152e-02  -10.10   <2e-16 ***
#  Ansiotulot   1.532e-05  1.270e-06   12.06   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.07787 on 293 degrees of freedom
#Multiple R-squared:  0.3318,	Adjusted R-squared:  0.3296 
#F-statistic: 145.5 on 1 and 293 DF,  p-value: < 2.2e-16

ggplot(merged, aes(x=Verot, y=LIB)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="LIB support", x="Verot")
cor(merged$Verot, merged$LIB)
model <- lm(LIB ~ Verot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.36292 -0.04844 -0.01937  0.03491  0.44067 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.339e-01  1.862e-02  -7.191 5.38e-12 ***
#  Verot        3.630e-05  3.409e-06  10.650  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.08088 on 293 degrees of freedom
#Multiple R-squared:  0.2791,	Adjusted R-squared:  0.2766 
#F-statistic: 113.4 on 1 and 293 DF,  p-value: < 2.2e-16

ggplot(merged, aes(x=Kunnallisvero, y=LIB)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="LIB support", x="Kunnallisvero")
cor(merged$Kunnallisvero, merged$LIB)
model <- lm(LIB ~ Kunnallisvero, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.16978 -0.05104 -0.01111  0.02791  0.46616 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -2.055e-01  2.353e-02  -8.737   <2e-16 ***
#  Kunnallisvero  7.712e-05  6.751e-06  11.423   <2e-16 ***
 # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.07924 on 293 degrees of freedom
#Multiple R-squared:  0.3081,	Adjusted R-squared:  0.3058 
#F-statistic: 130.5 on 1 and 293 DF,  p-value: < 2.2e-16

ggplot(merged, aes(x=Tulot_miinus_verot, y=LIB)) + geom_point(color="navy", size=1.3) +
  geom_smooth(formula = y ~ x, method = "lm", color="lightblue", alpha=0.5) + labs(y="LIB support", x="Tulot miinus verot")
cor(merged$Tulot_miinus_verot, merged$LIB)
model <- lm(LIB ~ Tulot_miinus_verot, data = merged)
summary(model)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.31593 -0.05042 -0.01560  0.02916  0.42911 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -3.733e-01  3.803e-02  -9.816   <2e-16 ***
#  Tulot_miinus_verot  2.023e-05  1.771e-06  11.425   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.07923 on 293 degrees of freedom
#Multiple R-squared:  0.3082,	Adjusted R-squared:  0.3058 
#F-statistic: 130.5 on 1 and 293 DF,  p-value: < 2.2e-16