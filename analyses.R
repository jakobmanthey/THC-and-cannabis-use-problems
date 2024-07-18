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

# state... federal state name -> 16 states and national ("Bund") in German
# sex... sex -> men and women
# year... 2009 to 2021
# gkv_pop... population size (number of persons covered by statutory health insurance)
# diag_prop... diagnostic proportion (dependent variable)
# thc_dev... deviation of THC from 12% (independent variable)
# thc_dev_lag_X ... lag of thc_dev by X years

data$state <- factor(data$state,
                     levels = c("Bund", "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", 
                                "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", 
                                "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", 
                                "Thüringen"))

data <- data[order(state)]
data$state_en <- as.numeric(data$state)
data$state_en <- factor(data$state_en, labels = c("Germany", "Baden-Württemberg", "Bavaria", "Berlin", "Brandenburg", "Bremen", 
                                "Hamburg", "Hesse", "Mecklenburg-Western Pomerania", "Lower Saxony", "North Rhine-Westphalia", 
                                "Rhineland-Palatinate", "Saarland", "Saxony", "Saxony-Anhalt", "Schleswig-Holstein", 
                                "Thuringia"))

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) ANALYSE DATA - MAIN MODELS
# ______________________________________________________________________________________________________________________

# 2.1) Men
# ----------------------------------------------

mendat <- copy(data)[state_en != "Germany" & sex == "men"]

##  model 1: random intercept
menmod1 <- lmer(diag_prop ~ thc_dev + (1|state_en), data = mendat, REML = F)
summary(menmod1)
confint(menmod1) # THC: 0.3 to 0.4

##  model 2: random intercept + random slope
menmod2 <- lmer(diag_prop ~ thc_dev + (thc_dev|state_en), data = mendat, REML = F)
summary(menmod2)
confint(menmod2) # THC: 0.3 to 0.6

##  compare model fits
anova(menmod1, menmod2) # mod2: lower AIC but the same BIC
lmtest::lrtest(menmod1, menmod2) # same results -> lower LogLik in mod2
sjPlot::tab_model(menmod1, menmod2) # mod2: increased ICC und higher conditional R2 (.75 vs .67)

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

womendat <- copy(data)[state_en != "Germany" & sex == "women"]

##  model 1: random intercept
womenmod1 <- lmer(diag_prop ~ thc_dev + (1|state_en), data = womendat, REML = F)
summary(womenmod1)
confint(womenmod1) # THC: 0.1 to 0.2

##  model 2: random intercept + random slope
womenmod2 <- lmer(diag_prop ~ thc_dev + (thc_dev|state_en), data = womendat, REML = F)
summary(womenmod2)
confint(womenmod2) # THC: 0.1 to 0.3

##  compare model fits
anova(womenmod1, womenmod2) # mod2: lower AIC and BIC
lmtest::lrtest(womenmod1, womenmod2) # same results -> lower LogLik in mod2
sjPlot::tab_model(womenmod1, womenmod2) # mod2: increased ICC und higher conditional R2 (.80 vs .69)

##  look at random slopes
state.effects.women <- data.table(state = row.names(ranef(womenmod2)$state),
                                ranef(womenmod2)$state)
names(state.effects.women) <- c("state","interc.dev","slope.dev")
state.effects.women$interc.all <- fixef(womenmod2)[1]
state.effects.women$slope.all <- fixef(womenmod2)[2]

state.effects.women[, slope.state := slope.dev + slope.all]
state.effects.women[, summary(slope.state)]


# 2.3) Sex interaction
# ----------------------------------------------

alldat <- copy(data)[state_en != "Germany" & sex != "total"]
alldat$sex <- factor(alldat$sex, levels = c("women","men"))

##  model 1: random intercept
allmod1 <- lmer(diag_prop ~ thc_dev*sex + (1|state_en), data = alldat, REML = F)
summary(allmod1)
confint(allmod1) # THC: 0.1 to 0.2; THC*sex(male): 0.1 to 0.3

##  model 2: random intercept + random slope
allmod2 <- lmer(diag_prop ~ thc_dev*sex + (thc_dev|state_en), data = alldat, REML = F)
summary(allmod2)
confint(allmod2) # THC: 0.1 to 0.2; THC*sex(male): 0.1 to 0.3

##  compare model fits
anova(allmod1, allmod2) # mod2: lower AIC and BIC
lmtest::lrtest(allmod1, allmod2) # same results -> lower LogLik in mod2
sjPlot::tab_model(allmod1, allmod2) # mod2: increased ICC und higher conditional R2 (.76 vs .82)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) ANALYSE DATA - LAGGED MODELS
# ______________________________________________________________________________________________________________________

# 3.1) Men
# ----------------------------------------------

##  lags: 0-5
menmodlag0 <- lmer(diag_prop ~ thc_dev_lag_0 + (thc_dev_lag_0|state_en), data = mendat, REML = F)
menmodlag1 <- lmer(diag_prop ~ thc_dev_lag_1 + (thc_dev_lag_1|state_en), data = mendat, REML = F)
menmodlag2 <- lmer(diag_prop ~ thc_dev_lag_2 + (thc_dev_lag_2|state_en), data = mendat, REML = F)
menmodlag3 <- lmer(diag_prop ~ thc_dev_lag_3 + (thc_dev_lag_3|state_en), data = mendat, REML = F)
menmodlag4 <- lmer(diag_prop ~ thc_dev_lag_4 + (thc_dev_lag_4|state_en), data = mendat, REML = F)
menmodlag5 <- lmer(diag_prop ~ thc_dev_lag_5 + (thc_dev_lag_5|state_en), data = mendat, REML = F)
menmodlag6 <- lmer(diag_prop ~ thc_dev_lag_6 + (thc_dev_lag_6|state_en), data = mendat, REML = F)
menmodlag7 <- lmer(diag_prop ~ thc_dev_lag_7 + (thc_dev_lag_7|state_en), data = mendat, REML = F) # singular fit
menmodlag8 <- lmer(diag_prop ~ thc_dev_lag_8 + (thc_dev_lag_8|state_en), data = mendat, REML = F) # singular fit
menmodlag9 <- lmer(diag_prop ~ thc_dev_lag_9 + (thc_dev_lag_9|state_en), data = mendat, REML = F) # singular fit
menmodlag10 <- lmer(diag_prop ~ thc_dev_lag_10 + (thc_dev_lag_10|state_en), data = mendat, REML = F) # singular fit

##  lags: 0-10 --> without random slope
menmodlag0 <- lmer(diag_prop ~ thc_dev_lag_0 + (1|state_en), data = mendat, REML = F)
menmodlag1 <- lmer(diag_prop ~ thc_dev_lag_1 + (1|state_en), data = mendat, REML = F)
menmodlag2 <- lmer(diag_prop ~ thc_dev_lag_2 + (1|state_en), data = mendat, REML = F)
menmodlag3 <- lmer(diag_prop ~ thc_dev_lag_3 + (1|state_en), data = mendat, REML = F)
menmodlag4 <- lmer(diag_prop ~ thc_dev_lag_4 + (1|state_en), data = mendat, REML = F)
menmodlag5 <- lmer(diag_prop ~ thc_dev_lag_5 + (1|state_en), data = mendat, REML = F)
menmodlag6 <- lmer(diag_prop ~ thc_dev_lag_6 + (1|state_en), data = mendat, REML = F)
menmodlag7 <- lmer(diag_prop ~ thc_dev_lag_7 + (1|state_en), data = mendat, REML = F)
menmodlag8 <- lmer(diag_prop ~ thc_dev_lag_8 + (1|state_en), data = mendat, REML = F)
menmodlag9 <- lmer(diag_prop ~ thc_dev_lag_9 + (1|state_en), data = mendat, REML = F)
menmodlag10 <- lmer(diag_prop ~ thc_dev_lag_10 + (1|state_en), data = mendat, REML = F)

sjPlot::tab_model(menmodlag0, 
                  menmodlag1,
                  menmodlag2,
                  menmodlag3,
                  menmodlag4,
                  menmodlag5,
                  menmodlag6,
                  menmodlag7,
                  menmodlag8,
                  menmodlag9,
                  menmodlag10) #

# 3.2) Women
# ----------------------------------------------

##  lags: 0-10
womenmodlag0 <- lmer(diag_prop ~ thc_dev_lag_0 + (thc_dev_lag_0|state_en), data = womendat, REML = F)
womenmodlag1 <- lmer(diag_prop ~ thc_dev_lag_1 + (thc_dev_lag_1|state_en), data = womendat, REML = F)
womenmodlag2 <- lmer(diag_prop ~ thc_dev_lag_2 + (thc_dev_lag_2|state_en), data = womendat, REML = F)
womenmodlag3 <- lmer(diag_prop ~ thc_dev_lag_3 + (thc_dev_lag_3|state_en), data = womendat, REML = F)
womenmodlag4 <- lmer(diag_prop ~ thc_dev_lag_4 + (thc_dev_lag_4|state_en), data = womendat, REML = F)
womenmodlag5 <- lmer(diag_prop ~ thc_dev_lag_5 + (thc_dev_lag_5|state_en), data = womendat, REML = F)
womenmodlag6 <- lmer(diag_prop ~ thc_dev_lag_6 + (thc_dev_lag_6|state_en), data = womendat, REML = F)
womenmodlag7 <- lmer(diag_prop ~ thc_dev_lag_7 + (thc_dev_lag_7|state_en), data = womendat, REML = F) # singular fit
womenmodlag8 <- lmer(diag_prop ~ thc_dev_lag_8 + (thc_dev_lag_8|state_en), data = womendat, REML = F) # singular fit
womenmodlag9 <- lmer(diag_prop ~ thc_dev_lag_9 + (thc_dev_lag_9|state_en), data = womendat, REML = F) # singular fit
womenmodlag10 <- lmer(diag_prop ~ thc_dev_lag_10 + (thc_dev_lag_10|state_en), data = womendat, REML = F) # singular fit

##  lags: 0-10 --> without random slope
womenmodlag0 <- lmer(diag_prop ~ thc_dev_lag_0 + (1|state_en), data = womendat, REML = F)
womenmodlag1 <- lmer(diag_prop ~ thc_dev_lag_1 + (1|state_en), data = womendat, REML = F)
womenmodlag2 <- lmer(diag_prop ~ thc_dev_lag_2 + (1|state_en), data = womendat, REML = F)
womenmodlag3 <- lmer(diag_prop ~ thc_dev_lag_3 + (1|state_en), data = womendat, REML = F)
womenmodlag4 <- lmer(diag_prop ~ thc_dev_lag_4 + (1|state_en), data = womendat, REML = F)
womenmodlag5 <- lmer(diag_prop ~ thc_dev_lag_5 + (1|state_en), data = womendat, REML = F)
womenmodlag6 <- lmer(diag_prop ~ thc_dev_lag_6 + (1|state_en), data = womendat, REML = F)
womenmodlag7 <- lmer(diag_prop ~ thc_dev_lag_7 + (1|state_en), data = womendat, REML = F)
womenmodlag8 <- lmer(diag_prop ~ thc_dev_lag_8 + (1|state_en), data = womendat, REML = F)
womenmodlag9 <- lmer(diag_prop ~ thc_dev_lag_9 + (1|state_en), data = womendat, REML = F)
womenmodlag10 <- lmer(diag_prop ~ thc_dev_lag_10 + (1|state_en), data = womendat, REML = F)

sjPlot::tab_model(womenmodlag0, 
                  womenmodlag1,
                  womenmodlag2,
                  womenmodlag3,
                  womenmodlag4,
                  womenmodlag5,
                  womenmodlag6,
                  womenmodlag7,
                  womenmodlag8,
                  womenmodlag9,
                  womenmodlag10) #

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) TABLES
# ______________________________________________________________________________________________________________________

# 4.1) TABLE 1
# ----------------------------------------------

out <- copy(data)[year == 2021,.(state_en,sex,thc = (thc_dev+12)/100,diag_prop=diag_prop/100)][order(state_en,sex)]
out <- dcast(out, state_en ~ sex, value.var = c("thc","diag_prop"))

out <- out[,.(state_en,
              thc_women,diag_prop_women,
              thc_men,diag_prop_men,
              thc_total,diag_prop_total)]

write.csv(out, "tables/table 1.csv", row.names = F)


# 4.2) TABLE 1
# ----------------------------------------------

sjPlot::tab_model(womenmod2, menmod2,
                  file = "tables/table 2.html")


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 4) REPORT IN PAPER
# ______________________________________________________________________________________________________________________

# 4.1) Descriptives
# ----------------------------------------------

##  diagnostic proportion
data[state_en == "Germany" & sex == "total",.(year,diag_prop)]
data[state_en == "Germany" & sex == "men",.(year,diag_prop)]
data[state_en == "Germany" & sex == "men" & year == 2021,diag_prop] - data[state_en == "Germany" & sex == "men" & year == 2009,diag_prop] # abs
data[state_en == "Germany" & sex == "men" & year == 2021,diag_prop] / data[state_en == "Germany" & sex == "men" & year == 2009,diag_prop] # rel

data[state_en == "Germany" & sex == "women",.(year,diag_prop)]
data[state_en == "Germany" & sex == "women" & year == 2021,diag_prop] - data[state_en == "Germany" & sex == "women" & year == 2009,diag_prop] # abs
data[state_en == "Germany" & sex == "women" & year == 2021,diag_prop] / data[state_en == "Germany" & sex == "women" & year == 2009,diag_prop] # rel

data[state_en == "Germany" & sex == "total",.(year,thc_dev+12)]
data[state_en == "Germany" & sex == "total" & year == 2021,.(thc_dev+12)] / data[state_en == "Germany" & sex == "total" & year == 2009,.(thc_dev+12)] # rel
data[state_en != "Germany" & sex == "total" & year %in% c(2009,2021),.(state_en,year,thc = thc_dev+12)][order(state_en,year),diff(thc), by = state_en] # abs

##  THC
data[state_en != "Germany" & sex == "total" & year %in% c(2009,2021),.(state_en,year,thc = thc_dev+12)][order(state_en,year),diff(thc), by = state_en]
data[state_en != "Germany" & sex == "total",.(state_en,year,thc = thc_dev+12)][, .(b = summary(lm(thc ~ year))$coef[2,1],
                                                                          t = summary(lm(thc ~ year))$coef[2,3]), by = state_en]
qt(1 - 0.05 / 2, 11) # t (df=11) = 2.2
data[state_en != "Germany" & sex == "total",.(state_en,year,thc = thc_dev+12)][, .(b = summary(lm(thc ~ year))$coef[2,1],
                                                                          t = summary(lm(thc ~ year))$coef[2,3]), by = state_en][abs(t)<2.2]

data[state_en != "Germany" & sex == "total" & year == 2021,.(state_en,diag_prop)][order(diag_prop)]



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 5) FIGURES
# ______________________________________________________________________________________________________________________

# 5.1) FIGURE 1 - Boxplot of diagnostic proportion over the years
# ----------------------------------------------

pdat <- data[state_en != "Germany" & sex != "total",
             .(year,sex,gkv_pop,diag_prop = diag_prop/100)]

ggplot(pdat, aes(x = year, y = diag_prop, group = year)) + 
  facet_wrap(sex ~ ., nrow = 1) +
  geom_boxplot(fill = "dark blue", alpha = 0.7) +
  scale_x_continuous("Year", breaks = scales::pretty_breaks()) +
  scale_y_continuous("", labels = scales::percent, limits = c(0,0.06))

ggsave(paste0("figures/fig1_diagnostic proportion_boxplot.png"), 
       width = 10, height = 5)
ggsave(paste0("figures/fig1_diagnostic proportion_boxplot.tiff"), 
       width = 10, height = 5, dpi = 300)

pdat[year == 2009]
pdat[year == 2021]


# 5.2) FIGURE 2 - Lag plot
# ----------------------------------------------

lag.dat <- data.table()

for (lag in 0:10){
  
  # women
  temp <- get(paste0("womenmodlag",lag))
  coef <- summary(temp)$coefficients[2,1]
  ci <- confint(temp)[4,]
  
  lag.dat <- rbind(lag.dat,
                   data.table(sex = "women",
                              lag,
                              coef,
                              low = ci[1],
                              high = ci[2]))
  rm(temp,coef,ci)
  
  # men
  temp <- get(paste0("menmodlag",lag))
  coef <- summary(temp)$coefficients[2,1]
  ci <- confint(temp)[4,]
  
  lag.dat <- rbind(lag.dat,
                   data.table(sex = "men",
                              lag,
                              coef,
                              low = ci[1],
                              high = ci[2]))
  rm(temp,coef,ci)
  
}

ggplot(lag.dat, aes(x = lag, y = coef)) + 
  facet_wrap(sex ~ ., nrow = 1) +
  geom_hline(yintercept = 0, linetype = 10) +
  geom_errorbar(aes(ymin = low, ymax = high), alpha = 0.5) +
  geom_point(color = "dark blue",size = 2) + 
  scale_x_continuous("Lag in years", breaks = scales::pretty_breaks()) +
  scale_y_continuous("coefficient")

ggsave(paste0("figures/fig2_lag plot.png"), 
       width = 10, height = 5)
ggsave(paste0("figures/fig2_lag plot.tiff"), 
       width = 10, height = 5, dpi = 300)

rm(lag.dat)
