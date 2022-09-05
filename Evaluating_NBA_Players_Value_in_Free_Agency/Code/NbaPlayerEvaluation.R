rm(list = ls())
library(rms) #for VIF
library(MASS) #for Box Cox transformation
library(lme4) # for hierarchical model
library(Matrix) # lme4
library(sjlabelled) # to remove labels


getwd()
# setwd("../FinalProject/Evalauting-NBA-players-value-in-the-free-agency/")
# setwd("Desktop/MIDS/courses/702_modeling and representation of data//FinalProject/Evalauting-NBA-players-value-in-the-free-agency/")
training <- read.csv("training.csv", header=T, 
                     colClasses=c("character","numeric","factor","factor",
                                  "numeric","numeric","numeric","numeric", 
                                  "numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric"))
testing <- read.csv("testing.csv",header=T, 
                     colClasses=c("character","numeric","factor","factor",
                                  "numeric","numeric","numeric","numeric", 
                                  "numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric"))

training$SALARY_100 <- (training$SALARY/training$SALARY_CAP)*100
testing$SALARY_100 <- (testing$SALARY/testing$SALARY_CAP)*100

head(training)
str(training)
rownames(training)
summary(training)
# sample_index <- sample(rownames(training), 100)
# training_sample <- training[sample_index,]
# traing_after2020 <- training[training$SEASON >= 2020,]
  
str(testing)

######### EDA ######### 
# check if the response variable is normal
ggplot(training,aes(x=SALARY_100)) +
  geom_histogram(aes(y=..density..),
                 color="black",linetype="dashed", 
                 binwidth = 2, fill=rainbow(27)) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Salary Percentage",y="Salary percentage") + 
  theme_classic() + theme(legend.position="none")

# transform the response to log(response) => a little help, can see the 
# distribution might be some accumulation of normal distribution
ggplot(training,aes(x=log(SALARY_100))) +
  geom_histogram(aes(y=..density..),
                 color="black",linetype="dashed", 
                 binwidth = 0.5, fill=rainbow(16)) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Salary Percentage",y="Density") + 
  theme_classic() + theme(legend.position="none")

str(training)

# log(SALARY_100) vs PLAYER 
# ggplot(training,aes(x=PLAYER, y=log(SALARY_100))) +
#   geom_point(alpha = .7) +
#   geom_smooth(method="lm",col="red3") + theme_classic() +
#   labs(title="log(SALARY_100) vs PLAYER",x="PLAYER",y="log(SALARY_100)") +
#   theme()

# log(SALARY_100) vs SEASON
ggplot(training,aes(x=SEASON, y=log(SALARY_100), fill=SEASON)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="SEASON vs log(SALARY_100)",x="SEASON",y="log(SALARY_100)") + 
  theme_classic() + theme(legend.position="none")

# log(SALARY_100) vs AGE => interesting (counter-intuitive)
ggplot(training,aes(x=AGE, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Player's Salary vs Age",x="Age",y="log(SALARY_100)") +
  theme()

ggplot(training,aes(x=AGE, y=SALARY_100)) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100 vs AGE",x="AGE",y="SALARY_100") +
  theme()

# log(SALARY_100) vs GP
ggplot(training,aes(x=GP, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs GP",x="GP",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs W
ggplot(training,aes(x=W, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Wins",x="Wins",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs L => interesting (counter-intuitive)
ggplot(training,aes(x=L, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Player's Salary vs Losses",x="Losses",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs PTS
ggplot(training,aes(x=PTS, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Points",x="Points",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs X3PM
ggplot(training,aes(x=X3PM, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs 3 pointers made",x="X3PM",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs REB
ggplot(training,aes(x=REB, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Rebound",x="REB",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs AST
ggplot(training,aes(x=AST, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Assist",x="Assist",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs TOV => interesting (counter-intuitive)
ggplot(training,aes(x=TOV, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Player's Salary vs Turnovers",x="Turnovers",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs STL
ggplot(training,aes(x=STL, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Steals",x="Steals",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs BLK
ggplot(training,aes(x=BLK, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Blocks",x="Blocks",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs X...
ggplot(training,aes(x=X..., y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs +/-",x="+/-",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs OFFRTG
ggplot(training,aes(x=OFFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Offensive Rating",
       x="Offensive Rating",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs DEFRTG => Interesting 
ggplot(training,aes(x=DEFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Defensive Rating",
       x="Defensive Rating",y="log(SALARY_100)") +
  theme()

# log(SALARY_100) vs NETRTG
ggplot(training,aes(x=NETRTG, y=log(SALARY_100))) +
  geom_point(alpha = .7) +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Net Rating",
       x="Net Rating",y="log(SALARY_100)") +
  theme()

# take aways:
# - Nearly all of predictors, except DEFRTG, have positiveproportions 
#   with response, even TOV, L, AGE
# - DEFRTG have unobvious postive relation, but still sightly
# - can include all predictors in MLR
    
    
## interactions ##
# investigate from the interesting terms, AGE, L, TOV, DEFRTG

str(training)
head(training)

training$PTS_fac <- "1. PTS < 10"
training$PTS_fac[(training$PTS >=10) & (training$PTS < 19)] <- "2. 10 <= PTS < 20"
training$PTS_fac[(training$PTS >=20) ] <- "3. PTS > 20"
training$PTS_fac <- as.factor(training$PTS_fac)
training[,c("PTS", "PTS_fac")]

training$REB_fac <- "1. REB < 5"
training$REB_fac[(training$REB >=5) & (training$REB < 10)] <- "2. 5 <= REB < 10"
training$REB_fac[(training$REB >=10) ] <- "3. REB > 10"
training$REB_fac <- as.factor(training$REB_fac)
training[,c("REB", "REB_fac")]

training$AST_fac <- "1. AST < 2.5"
training$AST_fac[(training$AST >=2.5) & (training$AST < 5)] <- "2. 2.5 <= AST < 5"
training$AST_fac[(training$AST >=5) ] <- "3. AST > 5"
training$AST_fac <- as.factor(training$AST_fac)
training[,c("AST", "AST_fac")]
summary(training)

# log(SALARY_100) and AGE by PTS => No interaction
ggplot(training,aes(x=AGE, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs AGE by Points",x="AGE",y="log(SALARY_100)") +
  facet_wrap( ~ PTS_fac, ncol=3)

# log(SALARY_100) and L by PTS => shall include this interaction
ggplot(training,aes(x=L, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Losses by Points",x="Losses",y="log(SALARY_100)") +
  facet_wrap( ~ PTS_fac, ncol=3)

# log(SALARY_100) and TOV by PTS => shall include this interaction
ggplot(training,aes(x=TOV, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Player's Salary vs Turnovers by Points",x="Turnovers",y="log(SALARY_100)") +
  facet_wrap( ~ PTS_fac, ncol=3)

# log(SALARY_100) and OFFRTG by PTS => No interaction
ggplot(training,aes(x=OFFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Offensive Rating by Points",x="Offensive Rating",y="log(SALARY_100)") +
  facet_wrap( ~ PTS_fac, ncol=3)

# log(SALARY_100) and DEFRTG by PTS => shall include this interaction
ggplot(training,aes(x=DEFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Defensive Rating by Points",x="Defensive Rating",y="log(SALARY_100)") +
  facet_wrap( ~ PTS_fac, ncol=3)

# log(SALARY_100) and NETRTG by PTS => No interaction
ggplot(training,aes(x=NETRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Net Rating by Points",x="Net Rating",y="log(SALARY_100)") +
  facet_wrap( ~ PTS_fac, ncol=3)

# log(SALARY_100) and AGE by REB => No interaction
ggplot(training,aes(x=AGE, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs AGE by REB",x="AGE",y="log(SALARY_100)") +
  facet_wrap( ~ REB_fac, ncol=3)

# log(SALARY_100) and L by REB => shall include this interaction
ggplot(training,aes(x=L, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Losses by REB",x="Losses",y="log(SALARY_100)") +
  facet_wrap( ~ REB_fac, ncol=3)

# log(SALARY_100) and TOV by REB => No obvious interaction
ggplot(training,aes(x=TOV, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Turnovers by REB",x="Turnovers",y="log(SALARY_100)") +
  facet_wrap( ~ REB_fac, ncol=3)

# log(SALARY_100) and OFFRTG by REB => No interaction
ggplot(training,aes(x=OFFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Offensive Rating by REB",x="Offensive Rating",y="log(SALARY_100)") +
  facet_wrap( ~ REB_fac, ncol=3)

# log(SALARY_100) and DEFRTG by REB => shall include this interaction
ggplot(training,aes(x=DEFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Defensive Rating by REB",x="Defensive Rating",y="log(SALARY_100)") +
  facet_wrap( ~ REB_fac, ncol=3)

# log(SALARY_100) and NETRTG by REB => No interaction
ggplot(training,aes(x=NETRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Net Rating by REB",x="Net Rating",y="log(SALARY_100)") +
  facet_wrap( ~ REB_fac, ncol=3)

# log(SALARY_100) and AGE by AST => No interaction
ggplot(training,aes(x=AGE, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs AGE by AST",x="AGE",y="log(SALARY_100)") +
  facet_wrap( ~ AST_fac, ncol=3)

# log(SALARY_100) and L by AST => shall include this interaction
ggplot(training,aes(x=L, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Losses by AST",x="Losses",y="log(SALARY_100)") +
  facet_wrap( ~ AST_fac, ncol=3)

# log(SALARY_100) and TOV by AST => No interaction
ggplot(training,aes(x=TOV, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Turnovers by AST",x="Turnovers",y="log(SALARY_100)") +
  facet_wrap( ~ AST_fac, ncol=3)

# log(SALARY_100) and OFFRTG by AST => No interaction
ggplot(training,aes(x=OFFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Offensive Rating by AST",x="Offensive Rating",y="log(SALARY_100)") +
  facet_wrap( ~ AST_fac, ncol=3)

# log(SALARY_100) and DEFRTG by AST => shall include this interaction
ggplot(training,aes(x=DEFRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Defensive Rating by AST",x="Defensive Rating",y="log(SALARY_100)") +
  facet_wrap( ~ AST_fac, ncol=3)

# log(SALARY_100) and NETRTG by AST => No interaction
ggplot(training,aes(x=NETRTG, y=log(SALARY_100))) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log(SALARY_100) vs Net Rating by AST",x="Net Rating",y="log(SALARY_100)") +
  facet_wrap( ~ AST_fac, ncol=3)

# take aways
# - can consider include the above interactions, 
#   L:PTS, TOV:PTS, DEFRTG:PTS
#   L:REB, DEFRTG:REB
#   L:AST, DEFRTG:AST

str(training)
head(training)
######### Modeling #########
cov_names <- names(training)
# cov_names <- cov_names[!cov_names %in% c("SEASON")] 
# ?prof

# Centering all numeric variables for interpretations
training$AGE_c <- training$AGE - mean(training$AGE)
training$GP_c <- training$GP - mean(training$GP)
training$W_c <- training$W - mean(training$W)
training$L_c <- training$L - mean(training$L)
training$PTS_c <- training$PTS - mean(training$PTS)
training$X3PM_c <- training$X3PM - mean(training$X3PM)
training$REB_c <- training$REB - mean(training$REB)
training$AST_c <- training$AST - mean(training$AST)
training$TOV_c <- training$TOV - mean(training$TOV)
training$STL_c <- training$STL - mean(training$STL)
training$BLK_c <- training$BLK - mean(training$BLK)
training$X..._c <- training$X... - mean(training$X...)
training$OFFRTG_c <- training$OFFRTG - mean(training$OFFRTG)
training$DEFRTG_c <- training$DEFRTG - mean(training$DEFRTG)
training$NETRTG_c <- training$NETRTG - mean(training$NETRTG)

# training$PLAYER_fac <- as.factor(training$PLAYER)
# ?prof:
str(training)

cov_names_c <- names(training)
cov_names_c <- cov_names_c[!cov_names_c %in% c("L_c")] 
# ?prof: why L_c will be NA?

# p_formula_1 <- as.formula(paste("log(SALARY_100) ~",
#                               paste(cov_names_c[!cov_names_c %in% cov_names],
#                                     collapse = " + ")))
# # base model
# reg_evalateNBA_1 <- lm(p_formula_1, data= training)
# summary(reg_evalateNBA_1)

# ## Model Assessment ##
# # - Linearity
# # - Normal Residual
# # - constant variance & independence
# 
# # - Linearity: pass
# ggplot(training,aes(x=PTS, y=reg_evalateNBA_1$residual)) + 
#   geom_point(alpha = .7) +  
#   geom_hline(yintercept=0,col="red3") + 
#   theme_classic() +
#   labs(title="Residuals vs Points",x="Points",y="Residuals")
# 
# # - constant variance & independence: pass
# plot(reg_evalateNBA_1,which=1,col=c("blue4"))
# # - Normal Residual: pass
# plot(reg_evalateNBA_1,which=2,col=c("blue4"))
# 
# # check outlier, high leverage, and influential points
# plot(reg_evalateNBA_1,which=4,col=c("blue4")) 
# # no influential points as all cook's distances < 0.5
# 
# nrow(training)
# plot(reg_evalateNBA_1,which=5,col=c("blue4"))
# # high leverage cut-off = 2 * (p+1) / n = 2 * (15+1) / 3835 = 0.008
# # many high leverage points
# # outliers: observation 2281, 2282, 2968
# training[c(2281, 2282, 2968),]
# 
# # multicollinearity check
# vif(reg_evalateNBA_1)
# # Advanced statistics: OFFRTG, DEFRTG, and NETRTG have 
# # multicollinearity issues

# cov_names_c_2 <- names(training)
# 
# # remove the OFFRTG_c from predictors to avoid multicollinearity issue
# # choose OFFRTG to remove since we want to investigate the interaction
# # between DEFRTG and PTS
# cov_names_c_2 <- cov_names_c_2[!cov_names_c_2 %in% c("L_c", "OFFRTG_c")] 
# p_formula_2 <- as.formula(paste("log(SALARY_100) ~",
#                               paste(cov_names_c_2[!cov_names_c_2 %in% cov_names],
#                                     collapse = " + ")))
# # second model
# reg_evalateNBA_2 <- lm(p_formula_2, data= training)
# summary(reg_evalateNBA_2)
# vif(reg_evalateNBA_2)

## third model: include interactions 
#   L:PTS, TOV:PTS, DEFRTG:PTS
#   L:REB, DEFRTG:REB
#   L:AST, DEFRTG:AST
# reg_evalateNBA_3 <- lm(log(SALARY_100) ~ 
#                          AGE_c + GP_c + W_c + PTS_c + X3PM_c + REB_c + 
#     AST_c + TOV_c + STL_c + BLK_c + X..._c + DEFRTG_c + NETRTG_c + 
#     TOV_c:PTS_c + DEFRTG_c:PTS_c
#     , data= training)
# summary(reg_evalateNBA_3)
# vif(reg_evalateNBA_3)

reg_evalateNBA_3_star <- lm(log(SALARY_100) ~ 
                         AGE_c + GP_c + W_c + PTS_c + X3PM_c + REB_c + 
    AST_c + TOV_c + STL_c + BLK_c + X..._c + DEFRTG_c + NETRTG_c + 
    TOV_c:PTS_c + DEFRTG_c:PTS_c
    , data= training)
summary(reg_evalateNBA_3_star)
vif(reg_evalateNBA_3_star) 
# original remove DEFRTG:REB_c, DEFRTG:AST_c since VIF >> 10


### Model selection ###
# Using stepwise with BIC (for prediction, so need smaller accuracte model)
str(training)
n <- nrow(training)
# reg_evalateNBA_0 <- lm(log(SALARY_100) ~ 
#                          AGE_c + PTS_c + REB_c + AST_c, data= training)
# reg_evalateNBA_stepwise <- step(reg_evalateNBA_0, 
#                                 scope = formula(reg_evalateNBA_3),
#                                 direction="both",trace=0,k = log(n))
# # Let's see the variables the model selected
# reg_evalateNBA_stepwise$call
# summary(reg_evalateNBA_stepwise)

# reg_evalateNBA_stepwise_2 <- step(reg_evalateNBA_2, 
#                                 scope = formula(reg_evalateNBA_3),
#                                 direction="both",trace=0,k = log(n))
# # Let's see the variables the model selected
# reg_evalateNBA_stepwise_2$call
# summary(reg_evalateNBA_stepwise_2)

reg_evalateNBA_0_star <- lm(log(SALARY_100) ~ 
                         PTS_c + REB_c + AST_c, data= training)
reg_evalateNBA_stepwise_3 <- step(reg_evalateNBA_0_star, 
                                scope = formula(reg_evalateNBA_3_star),
                                direction="both",trace=0,k = log(n))
# Let's see the variables the model selected
reg_evalateNBA_stepwise_3$call
summary(reg_evalateNBA_stepwise_3)


## fourth model: stepwise + boxcox 
reg_evalateNBA_stepwise_star  <- lm(formula = SALARY_100 ~ 
                                    AGE_c + PTS_c + REB_c + AST_c + 
                                      W_c + GP_c,
                                    data = training)

boxcox_trans <- boxcox(reg_evalateNBA_stepwise_star,
                       lambda = seq(-5, 5, length = 50))
lambda_trans <- boxcox_trans$x[boxcox_trans$y == max(boxcox_trans$y)]
lambda_trans

reg_evalateNBA_4 <- lm(SALARY_100^lambda_trans ~ 
                         AGE_c + PTS_c + REB_c + AST_c + W_c + GP_c,
                       data = training) 
summary(reg_evalateNBA_4)

# reg_evalateNBA_stepwise_2_star  <- lm(formula = SALARY_100 ~ 
#                                         AGE_c + W_c + PTS_c + REB_c + 
#                                         TOV_c + PTS_c:TOV_c, 
#                                     data = training)
# 
# boxcox_trans_2 <- boxcox(reg_evalateNBA_stepwise_2_star,
#                        lambda = seq(-5, 5, length = 50))
# lambda_trans_2 <- boxcox_trans_2$x[boxcox_trans_2$y == max(boxcox_trans_2$y)]
# lambda_trans_2
# 
# reg_evalateNBA_4_2 <- lm(SALARY_100^lambda_trans_2 ~ 
#                          AGE_c + W_c + PTS_c + REB_c + 
#                         TOV_c + PTS_c:TOV_c,
#                        data = training) 
# summary(reg_evalateNBA_4_2)


# verification of response normal
ggplot(training,aes(x=SALARY_100^lambda_trans)) +
  geom_histogram(aes(y=..density..),
                 color="black",linetype="dashed", 
                 binwidth = 0.2, fill=rainbow(13)) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Salary Percentage",y="Salary percentage") + 
  theme_classic() + theme(legend.position="none")

# - Linearity: pass
ggplot(training,aes(x=PTS, y=reg_evalateNBA_4$residual)) + 
  geom_point(alpha = .7) +  
  geom_hline(yintercept=0,col="red3") + 
  theme_classic() +
  labs(title="Residuals vs Points",x="Points",y="Residuals")

# - constant variance & independence: pass
plot(reg_evalateNBA_4,which=1,col=c("blue4"))
# - Normal Residual: pass
plot(reg_evalateNBA_4,which=2,col=c("blue4"))

# check outlier, high leverage, and influential points
plot(reg_evalateNBA_4,which=4,col=c("blue4")) 
# no influential points as all cook's distances < 0.5

nrow(training)
plot(reg_evalateNBA_4,which=5,col=c("blue4"))
# high leverage cut-off = 2 * (p+1) / n = 2 * (6+1) / 4616 = 0.003
# many high leverage points
# outliers: observation 2282, 2859, 3682
training[c(2282, 2859, 3682),]


## Model Validation
testing$AGE_c <- testing$AGE - mean(testing$AGE)
testing$GP_c <- testing$GP - mean(testing$GP)
testing$W_c <- testing$W - mean(testing$W)
testing$L_c <- testing$L - mean(testing$L)
testing$PTS_c <- testing$PTS - mean(testing$PTS)
testing$X3PM_c <- testing$X3PM - mean(testing$X3PM)
testing$REB_c <- testing$REB - mean(testing$REB)
testing$AST_c <- testing$AST - mean(testing$AST)
testing$TOV_c <- testing$TOV - mean(testing$TOV)
testing$STL_c <- testing$STL - mean(testing$STL)
testing$BLK_c <- testing$BLK - mean(testing$BLK)
testing$X..._c <- testing$X... - mean(testing$X...)
testing$OFFRTG_c <- testing$OFFRTG - mean(testing$OFFRTG)
testing$DEFRTG_c <- testing$DEFRTG - mean(testing$DEFRTG)
testing$NETRTG_c <- testing$NETRTG - mean(testing$NETRTG)

y_test_pred <- predict(reg_evalateNBA_4,testing, 
                       interval = "confidence", level = 0.99)
testMSE <- mean((testing$SALARY_100 - y_test_pred)^2); testMSE
sqrt(testMSE)

forecast_1 <- cbind(testing$PLAYER, testing$SALARY_100, 
                    y_test_pred^(1/lambda_trans));
colnames(forecast_1) <- c("PLAYER", "Truth","Predicted",
                          "Predicted_2.5", "Predicted_97.5")
forecast_1[1:5,]
forecast_1 <- as.data.frame(forecast_1, stringsAsFactors = FALSE)
forecast_1 <- remove_all_labels(forecast_1)

# calculate the percentage of observation lies within the confidence 
# interval of predictions
head(forecast_1)
str(forecast_1)
forecast_1$Truth <- as.numeric(forecast_1$Truth)
forecast_1$Predicted <- as.numeric(forecast_1$Predicted)
forecast_1$Predicted_2.5 <- as.numeric(forecast_1$Predicted_2.5)
forecast_1$Predicted_97.5 <- as.numeric(forecast_1$Predicted_97.5)
df_accuracte <- forecast_1[(forecast_1$Truth >= forecast_1$Predicted_2.5) & (forecast_1$Truth <= forecast_1$Predicted_97.5),]
hit_rate_1 <- nrow(df_accuracte)/nrow(testing)*100

# # no.2  
# 
# y_test_pred_2 <- predict(reg_evalateNBA_4_2,testing, 
#                        interval = "confidence", level = 0.99)
# # y_test_pred_2 <- predict(reg_evalateNBA_4_2,testing, 
# #                        interval = "prediction")
# testMSE_2 <- mean((testing$SALARY_100 - y_test_pred_2)^2); testMSE
# sqrt(testMSE_2)
# 
# forecast_2 <- cbind(testing$PLAYER, testing$SALARY_100, 
#                     y_test_pred_2^(1/lambda_trans_2));
# colnames(forecast_2) <- c("PLAYER", "Truth","Predicted",
#                           "Predicted_2.5", "Predicted_97.5")
# forecast_2[1:5,]
# forecast_2 <- as.data.frame(forecast_2, stringsAsFactors = FALSE)
# forecast_2 <- remove_all_labels(forecast_2)
# 
# # calculate the percentage of observation lies within the confidence 
# # interval of predictions
# head(forecast_2)
# str(forecast_2)
# forecast_2$Truth <- as.numeric(forecast_2$Truth)
# forecast_2$Predicted <- as.numeric(forecast_2$Predicted)
# forecast_2$Predicted_2.5 <- as.numeric(forecast_2$Predicted_2.5)
# forecast_2$Predicted_97.5 <- as.numeric(forecast_2$Predicted_97.5)
# df_accuracte_2 <- forecast_2[(forecast_2$Truth >= forecast_2$Predicted_2.5) & (forecast_2$Truth <= forecast_2$Predicted_97.5),]
# hit_rate_2 <- nrow(df_accuracte_2)/nrow(testing)*100


# K-fold
set.seed(123) # use whatever number you want
# Now randomly re-shuffle the data
training
K_training <- training[sample(nrow(training)),]; K_training
# Define the number of folds you want
K <- 10
# Define a matrix to save your results into
RSME <- matrix(0,nrow=10,ncol=1)
HIT <- matrix(0,nrow=10,ncol=1)
# Split the row indexes into k equal parts
kth_fold <- cut(seq(1,nrow(K_training)),breaks=10,labels=FALSE); kth_fold
# Now write the for loop for the k-fold cross validation
for(k in 1:K){
  # Split your data into the training and test datasets
  test_index <- which(kth_fold==k)
  train <- K_training[-test_index,]
  test <- K_training[test_index,]
  
  # Now that you've split the data, 
  # write your code for computing RMSE for each k here
  # You should consider using your code for question 7 above
  reg_evalateNBA_4_train_k <- lm(SALARY_100^lambda_trans ~ 
                         AGE_c + PTS_c + REB_c + AST_c + W_c + GP_c,
                         data=train)
  y_test_pred_k <- predict(reg_evalateNBA_4_train_k,test, 
                           interval = "confidence")
  forecast_2 <- cbind(test$PLAYER, test$SEASON, test$SALARY_100, y_test_pred_k^(1/lambda_trans))
  colnames(forecast_2) <- c("PLAYER", "SEASON", "Truth",
                            "Predicted","Predicted_2.5",
                            "Predicted_97.5"); forecast_2[1:5,]
  # RSME[k,] <- sqrt(mean((test$SALARY_100 - y_test_pred_k)^2))
  forecast_2 <- as.data.frame(forecast_2, stringsAsFactors = FALSE)
  forecast_2 <- remove_all_labels(forecast_2)
  forecast_2$Truth <- as.numeric(forecast_2$Truth)
  forecast_2$Predicted <- as.numeric(forecast_2$Predicted)
  forecast_2$Predicted_2.5 <- as.numeric(forecast_2$Predicted_2.5)
  forecast_2$Predicted_97.5 <- as.numeric(forecast_2$Predicted_97.5)
  df_accuracte_2 <- forecast_2[(forecast_2$Truth >= forecast_2$Predicted_2.5) & (forecast_2$Truth <= forecast_2$Predicted_97.5),]
  HIT[k,] <- nrow(df_accuracte_2)/nrow(testing)*100
}
# RSME
# mean(RSME)
forecast_2 <- data.frame(forecast_2)
forecast_2 <- forecast_2[order(forecast_2$SEASON),]
head(forecast_2)
str(forecast_2)
HIT; mean(HIT)
forecast_2[(forecast_2$PLAYER == "Luka Doncic") | (forecast_2$PLAYER == "Devin Booker"),]
 

############# HMLR Section ################
reg_evalateNBA_4$call
str(training)
unique(training$TEAM) # NOH == NOP, NJN == BKN
training$TEAM[training$TEAM == "NOH"] = "NOP"
training$TEAM[training$TEAM == "NJN"] = "BKN"
training$TEAM <- factor(training$TEAM)

# first consider team as group variable

## EDA ###
# group variable effect on response
ggplot(training,aes(x=TEAM, y=SALARY_100^lambda_trans, fill=TEAM)) +
  geom_boxplot() + 
  labs(title="TEAM vs SALARY_100^lambda_trans",x="TEAM",y="SALARY_100^lambda_trans") + 
  theme_classic() + theme(legend.position="none", axis.text.x = element_text(angle = 90))

# response vs age by team
ggplot(training,aes(x=AGE_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs AGE_c",x="AGE_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ TEAM, ncol=6)

# response vs pts by team
ggplot(training,aes(x=PTS_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs PTS_c",x="PTS_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ TEAM, ncol=6)

# response vs reb by team
ggplot(training,aes(x=REB_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs REB_c",x="REB_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ TEAM, ncol=6)

# response vs ast by team
ggplot(training,aes(x=AST_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs AST_c",x="AST_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ TEAM, ncol=6)

# response vs wins by team
ggplot(training,aes(x=W_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs W_c",x="W_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ TEAM, ncol=6)

# response vs gp by team
ggplot(training,aes(x=GP_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs GP_c",x="GP_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ TEAM, ncol=6)

# not consider adding random slope for team in the hierarchical model

# response vs age by season
ggplot(training,aes(x=AGE_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs AGE_c",x="AGE_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ SEASON, ncol=5)

# response vs pts by season
ggplot(training,aes(x=PTS_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs PTS_c",x="PTS_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ SEASON, ncol=5)

# response vs reb by season
ggplot(training,aes(x=REB_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs REB_c",x="REB_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ SEASON, ncol=5)

# response vs ast by season
ggplot(training,aes(x=AST_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs AST_c",x="AST_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ SEASON, ncol=5)

# response vs wins by season
ggplot(training,aes(x=W_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs W_c",x="W_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ SEASON, ncol=5)

# response vs gp by season
ggplot(training,aes(x=GP_c, y=SALARY_100^lambda_trans)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="SALARY_100^lambda_trans vs GP_c",x="GP_c",y="SALARY_100^lambda_trans") +
  facet_wrap( ~ SEASON, ncol=5)

## Model ##
reg_evalateNBA_5 <- lmer(SALARY_100^lambda_trans ~ 
                           AGE_c + PTS_c + REB_c + 
                           AST_c + W_c + GP_c + (1|TEAM), 
                         data = training)
summary(reg_evalateNBA_5)


(ranef(reg_evalateNBA_5)$TEAM)["BOS",]
dotplot(ranef(reg_evalateNBA_5,condVar=TRUE))$TEAM

# TEAM effect is not obvious, how about Season as a group variable?

reg_evalateNBA_6 <- lmer(SALARY_100^lambda_trans ~ 
                           AGE_c + PTS_c + REB_c + 
                           AST_c + W_c + GP_c + (1|SEASON) + (1|TEAM), 
                         data = training)
summary(reg_evalateNBA_6)

(ranef(reg_evalateNBA_6)$TEAM)["GSW",]
(ranef(reg_evalateNBA_6)$SEASON)["2019",]
# avg intercept: 4.67
mean(training$AGE)
mean(training$PTS)
mean(training$REB)
mean(training$AST)
mean(training$W)
mean(training$GP)

dotplot(ranef(reg_evalateNBA_6,condVar=TRUE))$SEASON

anova(reg_evalateNBA_5, reg_evalateNBA_6, test = "Chisq")

######### Conclusion/Limitation/Future Work ######### 
# Our model confirm age and tov, and the interpretations or explanation. 

# Model performance: if abs() > 5, then 299/387  hit rate >> 77% accuracy
# However, among the 88 Lowball and overpaid cases., we found some of the cases
# could lead to future studies: 

# Limitation of this research and also future work
# Every, except Luka:
# - forecasting will lose part of accuracy due to the prediction is on a
#   per year base, but in real free-agency market, major of the times is
#   multi-year contract

# For Luka, 
# together with the dedicate examples in the tables
# - shall separate the topic based on the current contract stage of a 
#   player, e.g, rookie, bird, RFA, to build corresponding model so that 
#   will get best prediction

# most of the cases is overpaid, however, why?
# is it really a bias or GM see the potential of the player? 
# if yes on the latter case, how to make it a variable?

#===
# - Series injuries, e.g. ACL

# - Playoff statistics

# - Per player analysis, player as a predictors, or even as a group variable

### Michael Hint
# - VIF not issue for preditcing purpose

