# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# CODE AUTHOR:    Jakob Manthey

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

# load libraries
library( data.table )
library( openxlsx )
library( ggplot2 )
library( ggthemes )
library( lme4 )
library( Hmisc )

theme_set(theme_gdocs())
options(scipen=999)



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

# prepared in "prepare data.R"

filename <- "data/data.csv"
data <- data.table(read.csv(filename))
nrow(data) # 663

# state... federal state name -> 16 states and national ("Bund")
# sex... sex -> men and women
# year... 2009 to 2021
# gkv_pop... population size (number of persons covered by statutory health insurance)
# diag_prop... diagnostic proportion (dependent variable)
# thc_dev... deviation of THC from 12% (independent variable)

data$state <- factor(data$state,
                     levels = c("Bund", "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", 
                                "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", 
                                "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", 
                                "Thüringen"))



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) ANALYSE DATA - MAIN MODELS
# ______________________________________________________________________________________________________________________

# 2.1) Men
# ----------------------------------------------

mendat <- copy(data)[state != "Bund" & sex == "men"]

##  model 1: random intercept
menmod1 <- lmer(diag_prop ~ thc_dev + (1|state), data = mendat, REML = F)
summary(menmod1)
confint(menmod1) # THC: 0.3 to 0.4

##  model 2: random intercept + random slope
menmod2 <- lmer(diag_prop ~ thc_dev + (thc_dev|state), data = mendat, REML = F)
summary(menmod2)
confint(menmod2) # THC: 0.3 to 0.6

##  compare model fits
anova(menmod1, menmod2) # mod2: lower AIC but the same BIC
lmtest::lrtest(menmod1, menmod2) # same results -> lower LogLik in mod2
sjPlot::tab_model(menmod1, menmod2) # mod2: increased ICC und higher conditional R2 (.66 vs .56)

##  look at random slopes
state.effects.men <- data.table(state = row.names(ranef(menmod2)$state),
                            ranef(menmod2)$state)
names(state.effects.men) <- c("state","interc.dev","slope.dev")
state.effects.men$interc.all <- fixef(menmod2)[1]
state.effects.men$slope.all <- fixef(menmod2)[2]

state.effects.men[, slope.state := slope.dev + slope.all]
state.effects.men[, summary(slope.state)]


# 2.2) Women
# ----------------------------------------------

womendat <- copy(data)[state != "Bund" & sex == "women"]

##  model 1: random intercept
womenmod1 <- lmer(diag_prop ~ thc_dev + (1|state), data = womendat, REML = F)
summary(womenmod1)
confint(womenmod1) # THC: 0.1 to 0.2

##  model 2: random intercept + random slope
womenmod2 <- lmer(diag_prop ~ thc_dev + (thc_dev|state), data = womendat, REML = F)
summary(womenmod2)
confint(womenmod2) # THC: 0.1 to 0.3

##  compare model fits
anova(womenmod1, womenmod2) # mod2: lower AIC and BIC
lmtest::lrtest(womenmod1, womenmod2) # same results -> lower LogLik in mod2
sjPlot::tab_model(womenmod1, womenmod2) # mod2: increased ICC und higher conditional R2 (.69 vs .80)

##  look at random slopes
state.effects.women <- data.table(state = row.names(ranef(womenmod2)$state),
                                ranef(womenmod2)$state)
names(state.effects.women) <- c("state","interc.dev","slope.dev")
state.effects.women$interc.all <- fixef(womenmod2)[1]
state.effects.women$slope.all <- fixef(womenmod2)[2]

state.effects.women[, slope.state := slope.dev + slope.all]
state.effects.women[, summary(slope.state)]


##  PREDICT FOR NATIONAL
#   .........................

mod.nat <- data[state == "Bund" & sex == "total"]

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) TABLES
# ______________________________________________________________________________________________________________________

# 4.1) TABLE 1
# ----------------------------------------------

sjPlot::tab_model(womenmod2, menmod2,
                  file = "tables/table 1.html")


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) REPORT IN PAPER
# ______________________________________________________________________________________________________________________

# 4.1) Descriptives
# ----------------------------------------------

##  diagnostic proportion
data[state == "Bund" & sex == "total",.(year,diag_prop)]
data[state == "Bund" & sex == "men",.(year,diag_prop)]
data[state == "Bund" & sex == "men" & year == 2021,diag_prop] - data[state == "Bund" & sex == "men" & year == 2009,diag_prop] # abs
data[state == "Bund" & sex == "men" & year == 2021,diag_prop] / data[state == "Bund" & sex == "men" & year == 2009,diag_prop] # rel

data[state == "Bund" & sex == "women",.(year,diag_prop)]
data[state == "Bund" & sex == "women" & year == 2021,diag_prop] - data[state == "Bund" & sex == "women" & year == 2009,diag_prop] # abs
data[state == "Bund" & sex == "women" & year == 2021,diag_prop] / data[state == "Bund" & sex == "women" & year == 2009,diag_prop] # rel

data[state == "Bund" & sex == "total",.(year,thc_dev+12)]
data[state == "Bund" & sex == "total" & year == 2021,.(thc_dev+12)] / data[state == "Bund" & sex == "total" & year == 2009,.(thc_dev+12)] # rel
data[state != "Bund" & sex == "total" & year %in% c(2009,2021),.(state,year,thc = thc_dev+12)][order(state,year),diff(thc), by = state] # abs

##  THC
data[state != "Bund" & sex == "total" & year %in% c(2009,2021),.(state,year,thc = thc_dev+12)][order(state,year),diff(thc), by = state]
data[state != "Bund" & sex == "total",.(state,year,thc = thc_dev+12)][, .(b = summary(lm(thc ~ year))$coef[2,1],
                                                                          t = summary(lm(thc ~ year))$coef[2,3]), by = state]
qt(1 - 0.05 / 2, 11) # t (df=11) = 2.2
data[state != "Bund" & sex == "total",.(state,year,thc = thc_dev+12)][, .(b = summary(lm(thc ~ year))$coef[2,1],
                                                                          t = summary(lm(thc ~ year))$coef[2,3]), by = state][abs(t)<2.2]

data[state != "Bund" & sex == "total" & year == 2021,.(state,diag_prop)][order(diag_prop)]



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) FIGURES
# ______________________________________________________________________________________________________________________

# 5.1) FIGURE 1 - Boxplot of diagnostic proportion over the years
# ----------------------------------------------

pdat <- data[state != "Bund" & sex != "total",
             .(year,sex,gkv_pop,diag_prop = diag_prop/100)]
pdat[,Geschlecht := ifelse(sex == "men","männlich",
                           ifelse(sex == "women", "weiblich", "gesamt"))]


ggplot(pdat, aes(x = year, y = diag_prop, group = year)) + 
  facet_wrap(Geschlecht ~ ., nrow = 1) +
  geom_boxplot(fill = "dark blue", alpha = 0.7) +
  scale_x_continuous("Jahr", breaks = scales::pretty_breaks()) +
  scale_y_continuous("", labels = scales::percent, limits = c(0,0.06))

ggsave(paste0("figures/fig1_diagnostic proportion_boxplot.png"), 
       width = 10, height = 5)

pdat[year == 2009]
pdat[year == 2021]

