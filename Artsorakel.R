#####-----Read SPSS####
library(foreign)
Artsorakel<-read.spss("ArtsApp_Artsorakel_questionnaire.sav", use.value.labels = F, to.data.frame = TRUE)

#####-----Check data frame####
View(Artsorakel)
str(Artsorakel)
head(Artsorakel)
names(Artsorakel)

####-----Data preparation####
library(memisc)
library(car)

options(scipen = 999)

####-----Citation packages####
citation("base")
citation("foreign")
citation("psych")
citation("multicon")
citation("memisc")
citation("car")
citation("ggplot2")
citation("apaTables")
citation("patchwork")
citation("performance")
citation("see")
citation("effectsize")
citation("pwr")

#--------------------------------------Creating scales and Cronbachs alpha####
library(psych)
library(multicon)

#---Achievement
Achievement_scale <- data.frame(Artsorakel$Algae1, Artsorakel$Algae2, Artsorakel$Algae3, Artsorakel$Algae4, 
                                Artsorakel$Algae5, Artsorakel$Algae6, Artsorakel$Algae7, Artsorakel$Algae8)

Artsorakel$Achievement <- Achievement_sum <- composite(Achievement_scale, R=NULL, rel = FALSE, Zitems = F)

Artsorakel$Achievement2 <- Achievement_scale2 <- (Artsorakel$Algae1 + Artsorakel$Algae2 + Artsorakel$Algae3 + Artsorakel$Algae4 + 
                                                              Artsorakel$Algae5 + Artsorakel$Algae6 + Artsorakel$Algae7 + Artsorakel$Algae8)

#---Competence satisfaction
Competence_scale <- data.frame(Artsorakel$Comp1, Artsorakel$Comp2, Artsorakel$Comp3)

Artsorakel$Competence <- Competence_sum <- composite(Competence_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#---Autonomy satisfaction
Autonomy_scale <- data.frame(Artsorakel$Auto1, Artsorakel$Auto2, Artsorakel$Auto3)

Artsorakel$Autonomy <- Autonomy_sum <- composite(Autonomy_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#---Intrinsic motivation
#-Recode reversed worded items and then create scale
Artsorakel$Intrinsic3_REV <- Recode(Artsorakel$Intrinsic3_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

Artsorakel$Intrinsic4_REV <- Recode(Artsorakel$Intrinsic4_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

Intrinsic_mot_scale <- data.frame(Artsorakel$Intrinsic1, Artsorakel$Intrinsic2, Artsorakel$Intrinsic3_REV, 
                                  Artsorakel$Intrinsic4_REV, Artsorakel$Intrinsic5, Artsorakel$Intrinsic6, 
                                  Artsorakel$Intrinsic7)

Artsorakel$Intrinsic_motivation <- Intrinsic_mot_sum <- composite(Intrinsic_mot_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#---Effort
#-Recode reversed worded items and the create scale
Artsorakel$Effort2_REV <- Recode(Artsorakel$Effort2_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

Artsorakel$Effort5_REV <- Recode(Artsorakel$Effort5_R, "1=7; 2= 6; 3=5; 4=4; 5=3; 6=2; 7=1" 
                                       , as.numeric=T) 

Effort_scale <- data.frame(Artsorakel$Effort1, Artsorakel$Effort2_REV, Artsorakel$Effort3, 
                           Artsorakel$Effort4, Artsorakel$Effort5_REV)

Artsorakel$Effort <- Effortt_sum <- composite(Effort_scale, R=NULL, rel = TRUE, Zitems = FALSE, maxScore = 7) 

#---Value/usefulness
Internalization_scale <- data.frame(Artsorakel$Value_use1, Artsorakel$Value_use2, Artsorakel$Value_use3,
                                    Artsorakel$Value_use4, Artsorakel$Value_use5, Artsorakel$Value_use6, 
                                    Artsorakel$Value_use7)

Artsorakel$Internalization <- Internalization_sum <- composite(Internalization_scale, R=NULL, rel = TRUE, Zitems = FALSE) 

#--------------------------------------Descriptive analyses####
library(psych)

#--Recode variables

#-Gender
Artsorakel$Gender2 = factor(Artsorakel$Gender, labels = c("Males", "Females", "Other", "Norespond")) #Males=0, Females=1, Other=2, Norespond=3
table(Artsorakel$Gender)
table(Artsorakel$Gender2)

#-Condition
Artsorakel$Condition2 = factor(Artsorakel$Condition, labels = c("Artsorakel", "ArtsApp")) #Artsorakel=0, ArtsaApp=1
table(Artsorakel$Condition)
table(Artsorakel$Condition2)

#-Class
Artsorakel$Class2 = factor(Artsorakel$Class, labels = c("First", "Second", "Third")) #First=1, Second=2, Third=3
table(Artsorakel$Class)
table(Artsorakel$Class2)

#Descriptive
describe(Artsorakel$Age)
summary(Artsorakel)
desc<- data.frame(Artsorakel$Competence, Artsorakel$Autonomy, Artsorakel$Intrinsic_motivation,
                  Artsorakel$Effort, Artsorakel$Internalization, Artsorakel$Achievement2)
describe(desc)

#--------------------------------------Preliminary analyses####

#--------Correlation for total sample 
corr_var <- data.frame(Artsorakel$Competence, Artsorakel$Autonomy, Artsorakel$Intrinsic_motivation,
                      Artsorakel$Internalization, Artsorakel$Effort,Artsorakel$Achievement2)
corr.test(corr_var)

#-Create APA table
library(apaTables)
apa.cor.table(corr_var, filename = "Table2.doc", table.number = 2,
              show.conf.interval = TRUE, landscape = TRUE)

#--------------------------------------Primary analyses####

#--Main effects

#Checking assumptions
leveneTest(Artsorakel$Competence ~ Artsorakel$Condition2, data = Artsorakel)
leveneTest(Artsorakel$Autonomy ~ Artsorakel$Condition2, data = Artsorakel)
leveneTest(Artsorakel$Intrinsic_motivation ~ Artsorakel$Condition2, data = Artsorakel)
leveneTest(Artsorakel$Internalization ~ Artsorakel$Condition2, data = Artsorakel)
leveneTest(Artsorakel$Effort ~ Artsorakel$Condition2, data = Artsorakel)
leveneTest(Artsorakel$Achievement2 ~ Artsorakel$Condition2, data = Artsorakel)

#by condition
t.test(Artsorakel$Competence ~ Artsorakel$Condition2)
t.test(Artsorakel$Autonomy ~ Artsorakel$Condition2)
t.test(Artsorakel$Intrinsic_motivation ~ Artsorakel$Condition2)
t.test(Artsorakel$Internalization ~ Artsorakel$Condition2)
t.test(Artsorakel$Effort ~ Artsorakel$Condition2)
t.test(Artsorakel$Achievement2 ~ Artsorakel$Condition2)

#Effect sizes
install.packages("effectsize")
library(effectsize)

cohens_d(Artsorakel$Competence~Artsorakel$Condition2, data = Artsorakel, paired = F) #Competence
cohens_d(Artsorakel$Autonomy~Artsorakel$Condition2, data = Artsorakel, paired = F) #Autonomy
cohens_d(Artsorakel$Intrinsic_motivation~Artsorakel$Condition2, data = Artsorakel, paired = F) #Intrinsic motivation
cohens_d(Artsorakel$Internalization~Artsorakel$Condition2, data = Artsorakel, paired = F) #Internalization
cohens_d(Artsorakel$Effort~Artsorakel$Condition2, data = Artsorakel, paired = F) #Effort
cohens_d(Artsorakel$Achievement2~Artsorakel$Condition2, data = Artsorakel, paired = F) #Achievement

#checking conventional effect sizes
library(pwr) 
cohen.ES(test = c("t"), size = c("small"))
cohen.ES(test = c("t"), size = c("medium"))
cohen.ES(test = c("t"), size = c("large"))

#--Multiple regression

#--Achievement
#- Check assumptions and model performance
library(performance)
library(see)
modelassump <- lm(Achievement2 ~ Condition + Gender + Class + Competence + Autonomy + Intrinsic_motivation + 
                    Internalization, data = Artsorakel)

model_performance(modelassump)
check_model(modelassump)

#- Fit model
summary(modelassump)
confint(modelassump, level = 0.95)

#--Effort
#- Check assumptions and model performance
modelassump2 <- lm(Effort ~ Condition + Gender + Class + Competence + Autonomy + Intrinsic_motivation + Internalization,
                    data = Artsorakel)

model_performance(modelassump2)
check_model(modelassump2)

#- Fit model
summary(modelassump2)
confint(modelassump2, level = 0.95)
