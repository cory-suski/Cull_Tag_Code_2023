# Cull Tag 2023 -----------------------------------------------------------
## Packages ----------------------------------------------------------------
getwd()
library(tidyverse)
library(car)
library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(MuMIn)
library(lme4)
library(emmeans)
library(multcomp)
library(DescTools)
library(cowplot)

select <- dplyr::select
filter <- dplyr::filter
col_factor <- readr::col_factor

## Cull Tag Data File ------------------------------------------------------
# All Cull Data Import
Cull_Tag_Combined_Data_2023 <- read_csv("Cull_Tag_Combined_Data_2023.csv",
  col_types = cols(
    PIT = col_number(),
    Sample_group = col_factor(levels = c(
      "1", "2", "3", "4" )),
    Netpen = col_character(),
    Density = col_double(), Time_Entered = col_time(format = "%H:%M"),
    Time_Released_to_Pens = col_time(format = "%H:%M"),
    Time_Released_from_Pens = col_time(format = "%H:%M"),
    Air_Exposures = col_factor(levels = c(
      "1",
      "2", "3", "4"
    )), Initial_Tag_Site_Score = col_factor(levels = c(
      "0",
      "1", "2", "3", "4"
    )), Initial_Fin_Score = col_factor(levels = c(
      "0",
      "1", "2", "3"
    )), LW_Tag_Site_Score = col_factor(levels = c(
      "0",
      "1", "2", "3", "4"
    )), LW_Fin_Score = col_factor(levels = c(
      "0",
      "1", "2", "3"
    )), LW_Wound_Length = col_double(),
    LW_Wound_Width = col_double(), Infection...19 = col_factor(levels = c(
      "A",
      "P"
    )), Abrasions...20 = col_factor(levels = c(
      "A",
      "P"
    )), Final_Tag_Site_Score = col_factor(levels = c(
      "0",
      "1", "2", "3", "4"
    )), Final_Fin_Score = col_factor(levels = c(
      "0",
      "1", "2", "3"
    )), Infection...26 = col_factor(levels = c(
      "A",
      "P"
    )), Abrasions...27 = col_factor(levels = c(
      "A",
      "P"
    )), Time_of_Mortality = col_time(format = "%H:%M"),
    Initial_RAMP = col_double(), NP_Startle = col_double(),
    NP_Flex = col_double(),
    NP_Mouth = col_double(), NP_VOR = col_double(),
    NP_Righting = col_double(), Final_RAMP = col_double()
  )
)

Cull <- Cull_Tag_Combined_Data_2023
glimpse(Cull)
summary(Cull)


# Cull Injury Data Import for Initial and LW Only
Cull_Tag_Combined_Data_2023_Injury_Plot_LW <- read_csv("Cull_Tag_Combined_Data_2023_Injury_Plot_LW.csv",
  col_types = cols(
    PIT = col_number(), Density = col_number(), Sample_group = col_factor(levels = c(
      "1", "2", "3", "4"
    )),
    Treatment = col_factor(levels = c(
      "Plastic Clip",
      "Metal Clip", "Stringer-Style",
      "Tail Lasso", "Livewell Control"
    )), Initial_LW = col_factor(levels = c(
      "Initial",
      "Livewell"
    )), Injury_Score = col_integer()
  )
)

Cull_Injury_LW <- Cull_Tag_Combined_Data_2023_Injury_Plot_LW
glimpse(Cull_Injury_LW)



# Cull Fin Injury Data Import for Initial and LW Only
Cull_Tag_Combined_Data_2023_Fin_Injury_Plot_LW <- read_csv("Cull_Tag_Combined_Data_2023_Fin_Injury_Plot_LW.csv",
  col_types = cols(
    PIT = col_number(), Sample_group = col_factor(levels = c(
      "1", "2", "3", "4"
    )),
    Treatment = col_factor(levels = c(
      "Livewell Control", "Metal Clip",
      "Plastic Clip", "Stringer-Style",
      "Tail Lasso"
    )), Initial_LW = col_factor(levels = c(
      "Initial",
      "Livewell"
    )), Fin_Injury_Score = col_integer(),
    Density = col_double()
  )
)

Cull_Fin_Injury_LW <- Cull_Tag_Combined_Data_2023_Fin_Injury_Plot_LW
glimpse(Cull_Fin_Injury_LW)


# Cull RAMP Import for Initial and Final Only
Cull_Tag_Combined_Data_2023_PLOTS <- read_csv("Cull_Tag_Combined_Data_2023_PLOTS.csv",
  col_types = cols(
    PIT = col_number(), Density = col_number(),
    Sample_group = col_factor(levels = c(
      "1", "2", "3", "4"
    )),
    Treatment = col_factor(levels = c(
      "Plastic Clip",
      "Metal Clip", "Stringer-Style",
      "Tail Lasso", "Livewell Control",
      "Net Pen Control"
    )), Initial_or_Final = col_factor(levels = c(
      "Initial",
      "Final"
    )), RAMP_Score = col_double()
  )
)
Cull_RAMP <- Cull_Tag_Combined_Data_2023_PLOTS
glimpse(Cull_RAMP)

# Livewell Density Data Import
Cull_Tag_Density_Parameter <- read_csv("Cull_Tag_Density_Parameter.csv")
glimpse(Cull_Tag_Density_Parameter)


# Proportion of Injuries Present or Absent for Initial and LW Only 
Cull_Tag_Combined_Data_2023_Injury_Plot_PorA_LW <- read_csv("Cull_Tag_Combined_Data_2023_Injury_Plot_PorA_LW.csv",
  col_types = cols(
    PIT = col_number(), Sample_group = col_factor(levels = c(
      "1", "2", "3", "4"
    )),
    Treatment = col_factor(levels = c(
      "Livewell Control",
      "Metal Clip",
      "Plastic Clip", "Stringer-Style",
      "Tail Lasso"
    )), Initial_LW = col_factor(levels = c(
      "Initial",
      "Livewell"
    )), Injury_Score = col_integer(),
    Injury_Score_PorA = col_integer(),
    Density = col_double()
  )
)
Injury_Presence_Absence_LW <- Cull_Tag_Combined_Data_2023_Injury_Plot_PorA_LW
glimpse(Injury_Presence_Absence_LW)

# Graph Data for Proportion of Injured Fish Model
Injury_Initial_Livewell_Graph_Data <- read_csv("Injury_Initial_LW_Graph_Data.csv",
  col_types = cols(Treatment = col_factor(levels = c(
    "Livewell Control",
    "Metal Clip", "Plastic Clip", "Stringer-Style",
    "Tail Lasso"
  )), Assessment_Time = col_factor(levels = c("Initial", "Livewell")))
)
glimpse(Injury_Initial_Livewell_Graph_Data)

# Injury Severity Data Import for Initial and LW Graph 
Graph_Data_LW <- read_csv("Graph_Data_LW.csv",
  col_types = cols(
    Initial_or_LW = col_factor(levels = c(
      "Initial",
      "Livewell"
    )), Treatment = col_factor(levels = c(
      "Livewell Control",
      "Stringer-Style",
      "Plastic Clip", "Metal Clip", "Tail Lasso"
    )),
    Injury_Category = col_factor(levels = c(
      "No Injury",
      "Minor Injury", "Moderate Injury",
      "Major Injury", "Severe Injury"
    ))
  )
)
glimpse(Graph_Data_LW)



# Water Quality for Net Pens Data Import
WQ_Netpens <- read_csv("WQ_Netpens.csv",
  col_types = cols(
    Date = col_date(format = "%m/%d/%y"),
    Time = col_time(format = "%H:%M")
  )
)
glimpse(WQ_Netpens)
summary(WQ_Netpens)

# Water Quality for LW Import
WQ_Livewells <- read_csv("WQ_Livewells.csv")
glimpse(WQ_Livewells)
summary(WQ_Livewells)



# WQ Summary Statistics  ------------------------------------------------------
WQ_DO_Netpens.sum <- describeBy(WQ_Netpens$DO)
WQ_DO_Netpens.sum

hist(WQ_Netpens$DO)
ggplot(WQ_Netpens, aes(Date, DO))+
geom_point()

WQ_Temp_Netpens.sum <- describeBy(WQ_Netpens$Temperature)
WQ_Temp_Netpens.sum

WQ_DO_Livewells.sum <- describeBy(WQ_Livewells$DO)
WQ_DO_Livewells.sum

WQ_Temp_Livewells.sum <- describeBy(WQ_Livewells$Temperature)
WQ_Temp_Livewells.sum

## Summary Statistics for Total Length and Weight ------------------------------
TL.sum <- describeBy(Cull$TL, Cull$Treatment)
TL.sum

Weight.sum <- describeBy(Cull$Weight, Cull$Treatment)
Weight.sum

Cull_Tag_Density_Parameter.sum <- describeBy(Cull_Tag_Density_Parameter$Density)
Cull_Tag_Density_Parameter.sum


## ANOVAs for Total Length and Weight ------------------------------------------
TL.out <- aov(TL ~ Treatment, data = Cull)
summary(TL.out)

Weight.out <- aov(Weight ~ Treatment, data = Cull)
summary(Weight.out)

## RAMP glmer ---------------------------------------------------------------
# Data used in the Models#######################################################
glimpse(Cull_RAMP)
summary(Cull_RAMP)

# Combined RAMP Models##########################################################
#Individual models
RAMP_Model_1 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment +(1 | PIT),
                    family = poisson, data = Cull_RAMP)

RAMP_Model_1_summary <- Anova(RAMP_Model_1)
RAMP_Model_1_summary

#TL
RAMP_Model_2 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment +TL+(1 | PIT),
                    family = poisson, data = Cull_RAMP)
RAMP_Model_2_summary <- Anova(RAMP_Model_2)
RAMP_Model_2_summary

#Sample_group
RAMP_Model_3 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment +Sample_group+
                      (1 | PIT),family = poisson, data = Cull_RAMP)
RAMP_Model_3_summary <- Anova(RAMP_Model_3)
RAMP_Model_3_summary

#Air_Exposures
RAMP_Model_4 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment +Air_Exposures+
                      (1 | PIT),family = poisson, data = Cull_RAMP)
RAMP_Model_4_summary <- Anova(RAMP_Model_4)
RAMP_Model_4_summary

#Combined Elimination Models
#All Variables
RAMP_Model_5 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment + TL+Sample_group+ 
                    Air_Exposures+(1 | PIT),family = poisson, data = Cull_RAMP)
RAMP_Model_5_summary <- Anova(RAMP_Model_5)
RAMP_Model_5_summary

#Without Air_Exposures
RAMP_Model_6 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment + TL+Sample_group+
                      (1 | PIT),family = poisson, data = Cull_RAMP)
RAMP_Model_6_summary <- Anova(RAMP_Model_6)
RAMP_Model_6_summary

#Without Sample_group
RAMP_Model_7 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment + TL+ 
                  Air_Exposures+(1 | PIT),family = poisson, data = Cull_RAMP)
RAMP_Model_7_summary <- Anova(RAMP_Model_7)
RAMP_Model_7_summary

#Without TL
RAMP_Model_8 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment + Sample_group+ 
                  Air_Exposures+(1 | PIT),family = poisson, data = Cull_RAMP)
RAMP_Model_8_summary <- Anova(RAMP_Model_8)
RAMP_Model_8_summary

#Selected RAMP Model
RAMP_Model_9 <- glmer(RAMP_Score ~ Initial_or_Final * Treatment +(1 | PIT),
                    family = poisson, data = Cull_RAMP)

# Summary Statistics
summary(RAMP_Model_9)
anova(RAMP_Model_9)
RAMP_Model_9_summary <- Anova(RAMP_Model_9)
RAMP_Model_9_summary

# R-squared values
r.squaredGLMM(RAMP_Model_9)

# validation
par(mfrow = c(2, 2))
plot(lm(RAMP_Model_9))

# Export Summary Data
write.csv(RAMP_Model_9_summary, "RAMP_Model_9_summary.csv")


# Injury Presence Absence Models FOR INITIAL AND LIVEWELL ----------------------
#Data used in presence absence models for initial and livewell tag site injury
glimpse(Injury_Presence_Absence_LW)
summary(Injury_Presence_Absence_LW)
######### glmer for Presence Absence Models Injury Score and Treatment##########
#Individual Models
Injury_Presence_Absence_Model_LW_1 <- glmer(
  Injury_Score_PorA ~ Initial_LW *Treatment +(1 | PIT),
  family = binomial,data = Injury_Presence_Absence_LW)
Injury_Presence_Absence_Model_LW_1_summary <- Anova(Injury_Presence_Absence_Model_LW_1)
Injury_Presence_Absence_Model_LW_1_summary

#TL
Injury_Presence_Absence_Model_LW_2 <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment + TL+(1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
Injury_Presence_Absence_Model_LW_2_summary <- Anova(Injury_Presence_Absence_Model_LW_2)
Injury_Presence_Absence_Model_LW_2_summary

#Sample_group
Injury_Presence_Absence_Model_LW <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment + Sample_group+(1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
Injury_Presence_Absence_Model_LW_summary <- Anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary

#Air_Exposures
Injury_Presence_Absence_Model_LW <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment +Air_Exposures+(1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
Injury_Presence_Absence_Model_LW_summary <- Anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary

#Combined Elimination Models
#All variables
Injury_Presence_Absence_Model_LW <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment + TL+ Sample_group + Air_Exposures+(1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
Injury_Presence_Absence_Model_LW_summary <- Anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary

#Without Air_Exposure
Injury_Presence_Absence_Model_LW <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment + TL+ Sample_group +(1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
Injury_Presence_Absence_Model_LW_summary <- Anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary

#Without Sample_group
Injury_Presence_Absence_Model_LW <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment + TL + Air_Exposures+(1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
Injury_Presence_Absence_Model_LW_summary <- Anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary

#Without TL
Injury_Presence_Absence_Model_LW <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment + Sample_group + Air_Exposures+(1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
Injury_Presence_Absence_Model_LW_summary <- Anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary

#Selected Model for Presence Absence Initial and Livewell tag site injury
Injury_Presence_Absence_Model_LW <- glmer(
  Injury_Score_PorA ~ Initial_LW *
    Treatment + (1 | PIT),
  family = binomial,
  data = Injury_Presence_Absence_LW
)
# Summary Statistics
summary(Injury_Presence_Absence_Model_LW)
anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary <- Anova(Injury_Presence_Absence_Model_LW)
Injury_Presence_Absence_Model_LW_summary

# R-squared values
r.squaredGLMM(Injury_Presence_Absence_Model_LW)

# validation
par(mfrow = c(2, 2))
plot(lm(Injury_Presence_Absence_Model_LW))

# Export Summary Data
write.csv(Injury_Presence_Absence_Model_LW, "Injury_Presence_Absence_Model_LW_summary.csv")


# Injury Scores Models FOR INITIAL AND LIVEWELL SCORES -------------------------
# Data used in the Model
glimpse(Cull_Injury_LW)
summary(Cull_Injury_LW)
# glmer for Injury Score and Treatment##########################################
#Individual Models
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + (1 | PIT),
  family = poisson, data = Cull_Injury_LW
)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

#TL
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + TL+
                        (1 | PIT),family = poisson, data = Cull_Injury_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

#Sample_group
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + 
                 Sample_group+(1 | PIT),family = poisson, data = Cull_Injury_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

#Air_Exposures
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + 
               Air_Exposures+(1 | PIT),family = poisson, data = Cull_Injury_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

#Combined Elimination Models
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + TL + 
                                Sample_group + Air_Exposures+ (1 | PIT),
                              family = poisson, data = Cull_Injury_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

#Without Air_Exposures
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + TL + 
                              Sample_group + (1 | PIT),
                              family = poisson, data = Cull_Injury_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

#Without Sample_Group
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + TL + 
                              Air_Exposures + (1 | PIT),
                              family = poisson, data = Cull_Injury_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

#Without TL
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + 
                              Sample_group + Air_Exposures + (1 | PIT),
                              family = poisson, data = Cull_Injury_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary


#Selected Model for Initial Livewell Injury Scores
Cull_Injury_Model_LW <- glmer(Injury_Score ~ Initial_LW * Treatment + (1 | PIT),
                              family = poisson, data = Cull_Injury_LW
)
# Summary Statistics
summary(Cull_Injury_Model_LW)
anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary <- Anova(Cull_Injury_Model_LW)
Injury_Model_LW_summary

# R-squared values
r.squaredGLMM(Cull_Injury_Model_LW)

# validation
par(mfrow = c(2, 2))
plot(lm(Cull_Injury_Model_LW))

# Export Summary Data
write.csv(Injury_Model_LW_summary, "Injury_Model_LW_summary.csv")

# Contrasts for glmer Model output
#Initial_LW_Contrast
Initial_LW_emmeans <- emmeans(Cull_Injury_Model_LW, "Initial_LW")
pairs(Initial_LW_emmeans)
pwpp(Initial_LW_emmeans)

#interaction contrast
emmean2<-emmeans(Cull_Injury_Model_LW, ~ Initial_LW:Treatment)
contrast2<-pairs(emmean2)
contrast2

# Fin Injury Scores Models FOR INITIAL AND LW DATA -----------------------------
# Data used in the Model
glimpse(Cull_Fin_Injury_LW)
summary(Cull_Fin_Injury_LW)

# glmer for Fin Injury Score and Treatment INITIAL AND LW DATA##################
#Individual Models
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +
                        (1 | PIT), family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary

#TL
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +TL+
                        (1 | PIT), family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary

#Sample_group
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +
            Sample_group+(1 | PIT), family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary

#Air_Exposures
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +
          Air_Exposures+(1 | PIT), family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary

#Combined Elimination Models
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +TL+
                            Sample_group+Air_Exposures+(1 | PIT), 
                            family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary

#Without Air_Exposures
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +TL+
                                    Sample_group+(1 | PIT), 
                                  family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary 

#Without Sample_group
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +TL+
                                    Air_Exposures+(1 | PIT), 
                                  family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary 

#Without TL
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +
                                    Sample_group+ Air_Exposures+(1 | PIT), 
                                  family = poisson, data = Cull_Fin_Injury_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary 


#Selected Model For Initial Livewell Fin Injury Scores
Cull_Fin_Injury_Model_LW <- glmer(Fin_Injury_Score ~ Initial_LW * Treatment +TL+
            Sample_group+(1 | PIT), family = poisson, data = Cull_Fin_Injury_LW)

# Summary Statistics
summary(Cull_Fin_Injury_Model_LW)
anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary <- Anova(Cull_Fin_Injury_Model_LW)
Fin_Injury_Model_LW_summary

# R-squared values
r.squaredGLMM(Cull_Fin_Injury_Model_LW)

# validation
par(mfrow = c(2, 2))
plot(lm(Cull_Fin_Injury_Model_LW))


# Contrasts for glmer Model output
Fin_LW_emmeans <- emmeans(Cull_Fin_Injury_Model_LW, "Sample_group")
pairs(Fin_LW_emmeans)
pwpp(Fin_LW_emmeans)
plot(Fin_LW_emmeans, comparisons = TRUE)

Fin_LW_emmeans <- emmeans(Cull_Fin_Injury_Model_LW, "Initial_LW")
pairs(Fin_LW_emmeans)
pwpp(Fin_LW_emmeans)
plot(Fin_LW_emmeans, comparisons = TRUE)


# Fin Injury Score Regression with Total Length
finregression_LW <- lm(Cull_Fin_Injury_LW$Fin_Injury_Score ~ Cull_Fin_Injury_LW$TL)
plot(finregression_LW)
acf(finregression_LW$res)
summary(finregression_LW)
finregression_LW

plot(Cull_Fin_Injury_LW$Fin_Injury_Score, Cull_Fin_Injury_LW$TL)
abline(lm(Fin_Injury_Score ~ TL, data = Cull_Fin_Injury_LW))

ggp <- ggplot(Cull_Fin_Injury_LW, aes(as.numeric(TL), as.numeric(Fin_Injury_Score), fill = Initial_LW, linetype = Initial_LW, colour = Initial_LW)) +
  geom_point(aes(shape = Treatment,color = Initial_LW),
              size = 5, position=position_jitter(h=0.17,w=0.9))+
  geom_smooth(method = lm)
ggp

ggp.1 <- ggp + 
  theme_grey(base_size = 14) +
  scale_fill_manual(values = c("#666666", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "right") +
  scale_size_manual(values = c(5, 5))+
  labs(y = "Fin Injury Score", x = "Total Length (mm)") +
  scale_colour_manual(
    labels = c("Initial", "Livewell"),
    values = c("#666666", "#0066CC")
  ) +
  scale_shape_manual(
    name = "Treatment",
    labels = c("Livewell Control", "Metal Clip", "Plastic Clip", "Stringer-Style", "Tail Lasso"),
    values = c(15, 16, 17, 13, 12))
  
ggp.1

ggp.20 <- ggplot(Cull_Fin_Injury_LW, aes(as.numeric(TL), as.numeric(Fin_Injury_Score), fill = Initial_LW, linetype = Initial_LW, colour = Initial_LW)) +
  geom_point(aes(color = Initial_LW),
             size = 3, position=position_jitter(h=0.17,w=0.9))+
  geom_smooth(method = lm)
ggp.20

ggp.21 <- ggp.20 + 
  theme_grey(base_size = 14) +
  scale_fill_manual(values = c("#666666", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "right") +
  scale_size_manual(values = c(5, 5))+
  scale_linetype_manual(values = c(1, 2)) +
  labs(y = "Fin Injury Score", x = "Total Length (mm)") +
  scale_colour_manual(
    labels = c("Initial", "Livewell"),
    values = c("#666666", "#0066CC")
  ) +
  theme(legend.position = "right")
  

ggp.21


# Fin Injury Score Comparison with Sample Group
ggp <- ggplot(Cull_Fin_Injury_LW, aes(x = Sample_group, y = Fin_Injury_Score)) +
  geom_boxplot()
ggp


#TL Comparison with Sample Group 
TL.sample.group.out <- aov(TL ~ Sample_group, data = Cull_Fin_Injury_LW2)
anova(TL.sample.group.out)
summary(TL.sample.group.out)
TukeyHSD(TL.sample.group.out)

ggp <- ggplot(Cull_Fin_Injury_LW2, aes(x = Sample_group, y = TL)) +
  geom_boxplot()+
  geom_point(position = "jitter")+
  stat_summary(fun.y="mean", color="black", shape=4, stroke = 2, size =2)

ggp+ theme_classic(base_size = 18) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  labs(y = "Total Length (mm)", x = "Collection Group")

#Comparison Graphs
# Removal of livewell fin injury scores for comparison with sample group
Cull_Fin_Injury_LW2 <- Cull_Fin_Injury_LW[!(Cull_Fin_Injury_LW$Initial_LW %in% c("Livewell")), ]

fin.sample.group.out2 <- aov(Fin_Injury_Score ~ Sample_group, data = Cull_Fin_Injury_LW2)
summary(fin.sample.group.out2)
TukeyHSD(fin.sample.group.out2)

ggp <- ggplot(Cull_Fin_Injury_LW2, aes(x = Sample_group, y = Fin_Injury_Score))+
  geom_bar(stat = "identity", position = "dodge")
ggp

ggp <- ggplot(Cull_Fin_Injury_LW2, aes(x = Sample_group, y = Fin_Injury_Score))+
  geom_boxplot()
ggp

# Removal of initial fin injury scores for comparison with sample group
Cull_Fin_Injury_LW1 <- Cull_Fin_Injury_LW[!(Cull_Fin_Injury_LW$Initial_LW %in% c("Initial")), ]

fin.sample.group.out1 <- aov(Fin_Injury_Score ~ Sample_group, data = Cull_Fin_Injury_LW1)
summary(fin.sample.group.out1)

ggp <- ggplot(Cull_Fin_Injury_LW1, aes(x = Sample_group, y = Fin_Injury_Score)) +
  geom_bar(stat = "identity", position = "dodge")
ggp

## Plots -------------------------------------------------------------------
########### RAMP Plots##########################################################
Initial_Final_LW <- relevel(Cull_RAMP$Initial_or_Final, "Initial")
Cull_RAMP$Treatment <- relevel(Cull_RAMP$Treatment, "Tail Lasso")
Cull_RAMP$Treatment <- relevel(Cull_RAMP$Treatment, "Stringer-Style")
Cull_RAMP$Treatment <- relevel(Cull_RAMP$Treatment, "Plastic Clip")
Cull_RAMP$Treatment <- relevel(Cull_RAMP$Treatment, "Metal Clip")
Cull_RAMP$Treatment <- relevel(Cull_RAMP$Treatment, "Net Pen Control")
Cull_RAMP$Treatment <- relevel(Cull_RAMP$Treatment, "Livewell Control")
levels(Cull_RAMP$Treatment)
glimpse(Cull_RAMP)
p <- ggplot(Cull_RAMP, aes(x = Treatment, y = RAMP_Score, fill = Initial_or_Final)) +
  geom_boxplot() +
  geom_jitter(
    alpha = 0.00,
    position = position_jitter(width = 0)
  )
p + theme_classic(base_size = 18) + scale_fill_manual(values = c("#CCCCCC", "#000066")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  labs(y = "RAMP", x = "Treatment")


p.ramp <- ggplot(Cull_RAMP, aes(x = Treatment, y = RAMP_Score, fill = Initial_or_Final)) +
  geom_violin() +geom_point(position = position_jitterdodge(), size =2)

p.ramp + theme_classic(base_size = 14) + scale_fill_manual(values = c("#CCCCCC", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  labs(y = "RAMP", x = "Treatment")
########### Injury Plots########################################################
# Cull Injury Plot
Initial_LW1 <- relevel(Cull_Injury_LW$Initial_LW, "Initial")
Cull_Injury_LW$Treatment <- relevel(Cull_Injury_LW$Treatment, "Tail Lasso")
Cull_Injury_LW$Treatment <- relevel(Cull_Injury_LW$Treatment, "Stringer-Style")
Cull_Injury_LW$Treatment <- relevel(Cull_Injury_LW$Treatment, "Plastic Clip")
Cull_Injury_LW$Treatment <- relevel(Cull_Injury_LW$Treatment, "Metal Clip")
Cull_Injury_LW$Treatment <- relevel(Cull_Injury_LW$Treatment, "Livewell Control")
levels(Cull_Injury_LW$Treatment)
glimpse(Cull_Injury_LW)


p1 <- ggplot(Cull_Injury_LW, aes(x = Treatment, y = Injury_Score, fill = Initial_LW1)) +
  geom_boxplot() +
  geom_jitter(
    alpha = 0.00,
    position = position_jitter(width = 0)
  )
p7 <- p1 + theme_classic(base_size = 14) + scale_fill_manual(values = c("#CCCCCC", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  labs(y = "Injury Score", x = "Treatment") +
  annotate("text",
    x = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2, 3.8, 4.2, 4.8, 5.2), y = c(4.5, 4.5, 4.5, 4.5, 2.5, 4.5, 3.8, 4.5, 2, 4.5),
    label = c("", "", "", "", "", "", "", "*", "", ""), color = "black",
    size = 10, angle = 0, fontface = "bold"
  )
p7
p7.3 <- p1 + theme_classic(base_size = 14) + scale_fill_manual(values = c("#CCCCCC", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.position = "none") +
  labs(y = "Injury Score", x = "Treatment") +
  annotate("text",
    x = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2, 3.8, 4.2, 4.8, 5.2), y = c(4.5, 4.5, 4.5, 4.5, 2.5, 4.5, 3.8, 4.5, 2, 4.5),
    label = c("", "", "", "", "", "", "", "*", "", ""), color = "black",
    size = 10, angle = 0, fontface = "bold"
  )
p7

#Cull Injury Violin Plot
p <- ggplot(Cull_Injury_LW, aes(x=Treatment, y=Injury_Score, colour = Initial_LW1, fill= Initial_LW1)) + 
  geom_violin()+geom_point(position = position_jitterdodge(), size =2)
p
p.1<-p+ theme_classic(base_size = 14) + scale_colour_manual(values = c("black", "black")) +
  scale_fill_manual(values = c("#CCCCCC", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.position = "none") +
  labs(y = "Injury Score", x = "Treatment") +
  annotate("text",
           x = c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2, 3.8, 4.2, 4.8, 5.2), y = c(4.5, 4.5, 4.5, 4.5, 2.5, 4.5, 3.8, 4.3, 2, 4.5),
           label = c("", "", "", "", "", "", "", "*", "", ""), color = "black",
           size = 10, angle = 0, fontface = "bold"
  )


p.1

# Graph for Proportion of Injured fish Plot
p1.1 <- ggplot(Injury_Initial_Livewell_Graph_Data, aes(x = Treatment, 
                                y = Percent_Injured, fill = Assessment_Time)) +
                                geom_bar(stat = "identity", position = "dodge")

p7.1 <- p1.1 + theme_classic(base_size = 14) + 
  scale_fill_manual(values = c("#CCCCCC", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  labs(y = "Proportion of Fish Injured", x = "Treatment")

p7.2 <- p1.1 + theme_classic(base_size = 14) + 
  scale_fill_manual(values = c("#CCCCCC", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  labs(y = "Proportion of Fish Injured", x = " ")

plot_grid(p7.2, p.1, labels = c("A", "B"), label_size = 12, ncol = 1)

# Injury severity among treatments before and after livewells Plot
Initial_or_LW <- relevel(Graph_Data_LW$Initial_or_LW, "Initial")
Graph_Data_LW$Treatment <- relevel(Graph_Data_LW$Treatment, "Tail Lasso")
Graph_Data_LW$Treatment <- relevel(Graph_Data_LW$Treatment, "Stringer-Style")
Graph_Data_LW$Treatment <- relevel(Graph_Data_LW$Treatment, "Plastic Clip")
Graph_Data_LW$Treatment <- relevel(Graph_Data_LW$Treatment, "Metal Clip")
Graph_Data_LW$Treatment <- relevel(Graph_Data_LW$Treatment, "Livewell Control")
Graph_Data_LW$Injury_Category <- relevel(Graph_Data_LW$Injury_Category, "No Injury")
Graph_Data_LW$Injury_Category <- relevel(Graph_Data_LW$Injury_Category, "Minor Injury")
Graph_Data_LW$Injury_Category <- relevel(Graph_Data_LW$Injury_Category, "Moderate Injury")
Graph_Data_LW$Injury_Category <- relevel(Graph_Data_LW$Injury_Category, "Major Injury")
Graph_Data_LW$Injury_Category <- relevel(Graph_Data_LW$Injury_Category, "Severe Injury")

p1.2 <- ggplot(Graph_Data_LW, aes(x = Initial_or_LW, y = Percent, fill = Injury_Category)) +
  facet_wrap(vars(Treatment)) +
  geom_col(position = "fill") +
  scale_y_continuous(limits = NULL, breaks = c(0, .5, 1)) +
  geom_jitter(
    alpha = 0.00,
    position = position_jitter(width = 0)
  ) +
  scale_fill_manual(values = c("#000066", "#1565C0", "#4FC3F7", "#90CAF9", "#BBDEFB"), 
  name = "Injury Score", labels = c("Severe Injury (4)", "Major Injury (3)", "Moderate Injury (2)", "Minor Injury (1)", "No Injury (0)"))

p1.2 + theme_gray(base_size = 18) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.85, .25)) +
  labs(y = "Proportion of Fish", x = "Time Assessed")

# Fin Injury Plot
Initial_LW1 <- relevel(Cull_Fin_Injury_LW$Initial_LW, "Initial")
Cull_Fin_Injury_LW$Treatment <- relevel(Cull_Fin_Injury_LW$Treatment, "Tail Lasso")
Cull_Fin_Injury_LW$Treatment <- relevel(Cull_Fin_Injury_LW$Treatment, "Stringer-Style")
Cull_Fin_Injury_LW$Treatment <- relevel(Cull_Fin_Injury_LW$Treatment, "Plastic Clip")
Cull_Fin_Injury_LW$Treatment <- relevel(Cull_Fin_Injury_LW$Treatment, "Metal Clip")
Cull_Fin_Injury_LW$Treatment <- relevel(Cull_Fin_Injury_LW$Treatment, "Livewell Control")
levels(Cull_Fin_Injury_LW$Treatment)
glimpse(Cull_Fin_Injury_LW)


p1.17 <- ggplot(Cull_Fin_Injury_LW, aes(
  x = Treatment, y = Fin_Injury_Score,
  fill = Initial_LW1
)) +
  geom_boxplot() +
  geom_jitter(
    alpha = 0.00,
    position = position_jitter(width = 0)
  )
p.70 <- p1.17 + theme_classic(base_size = 18) + 
  scale_fill_manual(values = c("#666666", "#0066CC")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  labs(y = "Fin Injury Score", x = "Treatment")

plot_grid(p.3, ggp.21, labels = c("A", "B"), label_size = 12, ncol = 1)


#Cull Fin Injury Violin Plot
p.2 <- ggplot(Cull_Fin_Injury_LW, aes(x=Treatment, y=Fin_Injury_Score, fill= Initial_LW1)) + 
  geom_violin()+geom_point(position = position_jitterdodge(), size =2)
p.2
p.3<-p.2+ theme_classic(base_size = 14) + scale_colour_manual(values = c("black", "black")) +
  theme(axis.title = element_text(face = "bold")) +
  theme(axis.line = element_line(size = 1)) +
  theme(axis.text.x = element_text(face = "bold")) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.position = "top") +
  labs(y = "Fin Injury Score", x = "Treatment")+
  guides(set_fill = "none")+
  scale_fill_manual(values = c("#666666", "#0066CC"), 
                     name = " ",labels = c("Initial", "Livewell"))
p.3
