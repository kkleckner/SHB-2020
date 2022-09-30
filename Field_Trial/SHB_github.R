##Field trial analysis for "A novel acute toxicity bioassay and field trial to 
  #evaluate compounds for small hive beetle control."
##Completed by Kaylin Kleckner
##Publically available on https://github.com/kkleckner/SHB-2020

#####LOAD IN PACKAGES AND DATA#####

######Packages######
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(car)

######Data######
combined <- read.csv("Combined_Beetle_Data.csv")
head(combined)

#filter for just live beetle counts
live <- combined %>% filter(Live_Dead == "Live")
head(live)

str(live)
live$Treatment <- as.factor(live$Treatment) #change treatment to factor
str(live) #confirm change to factor

#filter for just dead beetle counts
dead<- combined %>% filter(Live_Dead == "Dead")
head(dead)

str(dead)
dead$Treatment <- as.factor(dead$Treatment) #change treatment to factor
str(dead) #confirm change to factor


####LIVE BEETLE ANALYSIS#####

######Visualize####
hist(live$Count) #poisson or negative binomial distribution for count data


######Build model####

live_mod <- glmmTMB(Count~Treatment, data=live, family="nbinom2")
  #constructed generalized linear model with negative binomial distribution
  #Number of live beetles aspirated as response variable
  #Treatment (acetone, acetamiprid, fipronil) in pollen traps as fixed predictor variable


#######Check assumptions#####

#Use simulated residuals for negative binomial distribution
plot(simulateResiduals(live_mod)) #qqplot residuals are mostly linear
                               #residuals vs predicted are mostly flat lines

hist(simulateResiduals(live_mod)) #histogram of residuals is okay, mostly even distribution of residuals


#######Check dispersion parameter####

summary(live_mod) ##3.5. Negative binomial needed to account for overdispersion


######Check significance####

Anova(live_mod) ##Treatment is significant (2.718e -10)

######Pairwise comparisons####

emmeans(live_mod, pairwise~Treatment, type="response")
  #using estimate marginal means
  #using negative binomial confidence intervals for comparison

####DEAD BEETLE ANALYSIS#####

######Visualize####

hist(dead$Count) ##poisson or negative binomial for counts


######Build model####

dead_mod <- glmmTMB(Count~Treatment, data=dead, family="nbinom2")
  #Generalized linear model with negative binomial distribution
  #Count of dead beetles in trap as response variable
  #Treatment (acetone, acetamiprid, fipronil) in pollen traps as fixed predictor variable
  #Note: Keeping acetone treatment (all zeros) in model for residuals
      #will use comparison of confidence intervals to determine significance


######Check assumptions####

#Use simulated residuals for negative binomial distribution
plot(simulateResiduals(dead_mod)) 

hist(simulateResiduals(dead_mod))
  #these look OK


######Check dispersion parameter####

summary(dead_mod) #extremely high with lots of zero values - 72.8


######Check significance####

Anova(dead_mod) ##very small p value

#Note:A variety of model distributions and factors were considered with so many 
      #zeroes and a small sample size.
      #No matter the choice, treatment was very significant.
      #While the right model can be debated, we default to the clear lack 
      #of overlap of confidence intervals. This model was needed to get
      #CI with a negative distribution


######Pairwise comparisons#####

emmeans(dead_mod, pairwise~Treatment, type="response")
  #Contrasts with acetone are incorrect (1)
  #Use negative binomial confidence intervals, not contrasts 


#####FIGURES#####

######Live beetles####

LiveCI <- read.csv("LiveCI.csv") #load in negative binomial CI
head(LiveCI)

##black and white
ggplot(LiveCI, aes(Treatment, LiveMean)) + 
  geom_jitter(data=combined%>% filter(Live_Dead == "Live"), aes(x=Treatment, y=Count), color="gray", position = position_jitter(0.3))+
  geom_point(size=3) + geom_errorbar(aes(ymin = Lower, ymax= Upper))+ 
  labs(x ="Treatment", y ="# Live Beetles") + 
  theme(panel.border=element_rect(color = "black", fill = NA, size = 1), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size=15)) + 
  scale_x_discrete(limits=c("Acetone", "Acetamiprid", "Fipronil"))

##color for graphical abstract
ggplot(LiveCI, aes(Treatment, LiveMean)) + 
  geom_jitter(data=combined%>% filter(Live_Dead == "Live"), aes(x=Treatment, y=Count), color="lightgoldenrod3", position = position_jitter(0.3))+
  geom_point(size=3) + geom_errorbar(aes(ymin = Lower, ymax= Upper))+ 
  labs(x ="Treatment", y ="# Live Beetles") + 
  theme(panel.border=element_rect(color = "black", fill = NA, size = 1), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size=15)) + 
  scale_x_discrete(limits=c("Acetone", "Acetamiprid", "Fipronil"))

######Dead beetles####

DeadCI <- read.csv("DeadCI.csv") #load in negative binomial CI
head(DeadCI)

##black and white
ggplot(DeadCI, aes(Treatment, DeadMean)) + 
  geom_jitter(data=combined%>% filter(Live_Dead == "Dead"), aes(x=Treatment, y=Count), color="gray", position = position_jitter(0.3))+
  geom_errorbar(aes(ymin = Lower, ymax= Upper))+ geom_point(size=3)+
  labs(x ="Treatment", y ="# Dead Beetles") +
  theme(panel.border=element_rect(color = "black", fill = NA, size = 1), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size=15)) + 
  scale_x_discrete(limits=c("Acetone", "Acetamiprid", "Fipronil"))

##color for graphical abstract
ggplot(DeadCI, aes(Treatment, DeadMean)) + 
  geom_jitter(data=combined%>% filter(Live_Dead == "Dead"), aes(x=Treatment, y=Count), color="lightsteelblue2", position = position_jitter(0.3))+
  geom_errorbar(aes(ymin = Lower, ymax= Upper))+ geom_point(size=3)+
  labs(x ="Treatment", y ="# Dead Beetles") +
  theme(panel.border=element_rect(color = "black", fill = NA, size = 1), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text = element_text(size=15)) + 
  scale_x_discrete(limits=c("Acetone", "Acetamiprid", "Fipronil"))

