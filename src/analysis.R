library(readr)
library(dplyr)
library(here)

source(here("src/functions.R"))

data <- readr::read_rds(here("data/database.rds"))
data$id <- factor(data$id)

# EXPLORATORY ANALYSIS -------------------

# Please run the functions of "functions.R" file
data %>%
  group_by(method, sex, operator, device) %>%
  var_summary(hematocrit)

data %>%
  group_by(method, sex, operator, device) %>%
  var_mean_sd(hematocrit)

## HERE COMPLETE THE GRAPHS DANIEEEEEL


# ANALYSIS (Linear Mixed Effect Model) -------------

library(lme4)
library(lmerTest)
library(car)

contrasts(data$method) <- "contr.sum" ## contrast before (no post-hoc analysis)
contrasts(data$sex) <- "contr.sum"
contrasts(data$id) <- "contr.sum"
contrasts(data$operator) <- "contr.sum"
contrasts(data$device) <- "contr.sum"

# crude results

m <- lmer(hematocrit ~ method + (1|id),
          data)
Anova(m, type = 3, test.statistic = "F") # p < 0.05 (significant difference between groups)

# adjusted results

m <- lmer(hematocrit ~ method + sex + age_group + operator + device + (1|id), # checl ps
          data)
Anova(m, type = 3, test.statistic = "F") # sex, age_group, operator and device not p < 0.05


m <- lmer(hematocrit ~ method + sex + age_group + # fixed effects
            (1|id) + (1|operator) + (1|device),   # random effects
          data)
Anova(m, type = 3, test.statistic = "F") # adjusted ps

# adjusted results

var_among <- as.data.frame(print(VarCorr(m),comp="Variance"))[1,4]
var_within <- as.data.frame(print(VarCorr(m),comp="Variance"))[4,4]

var_among/(var_among+var_within) # replicability = 30%



## ## ## ## ## ## same results:

n <- lme(fixed = hematocrit ~ method,
         random = ~ 1|id,
         data = data, na.action = na.omit)

VarCorr(n)

var_among <- as.numeric(VarCorr(m)[1,1])
var_within <- as.numeric(VarCorr(m)[3,1])

var_among/(var_among+var_within)


# DISTRIBUCIONES NORMALES (p > 0.05)

par(mfrow = c(2,2))

hist( data[data$method == "centrifuge" & data$sex == "M", ]$hematocrit,
      main = "Centrifuge M")
mtext( paste("Shapiro (p) =",
             round(shapiro.test( data[data$method == "centrifuge" & data$sex == "M", ]$hematocrit )$p.value, 3))
)

hist( data[data$method == "centrifuge" & data$sex == "F", ]$hematocrit,
      main = "Centrifuge F" )
mtext( paste("Shapiro (p) =",
             round(shapiro.test( data[data$method == "centrifuge" & data$sex == "F", ]$hematocrit )$p.value, 3))
)
hist( data[data$method == "runrun" & data$sex == "M", ]$hematocrit,
      main = "Runrun M" )
mtext( paste("Shapiro (p) =",
             round(shapiro.test( data[data$method == "runrun" & data$sex == "M", ]$hematocrit )$p.value, 3))
)

hist( data[data$method == "runrun" & data$sex == "F", ]$hematocrit,
      main = "Runrun F" )
mtext( paste("Shapiro (p) =",
             round(shapiro.test( data[data$method == "runrun" & data$sex == "F", ]$hematocrit )$p.value, 3))
)

par(mfrow = c(1,1))
boxplot(hematocrit ~ method * sex, data)

with( data, interaction.plot( method, sex, hematocrit,
                              ylim = c(0, max(data$hematocrit, na.rm = T)) ) )

plot(hematocrit ~ age, data, bg = data$sex, pch = 21)
legend("bottomleft",levels(data$sex),col= 1:2 ,pch=16 ,box.lwd= 0)
