library(dplyr)
library(ggplot2)
library(stargazer)
library(data.table)

#removed top 1 and bottom 1 percentile
#compared top 25 to bottom 25

setwd("~/")
setwd("Google Drive/Non-Academic Work/Freshman Year/Clubs/Fed Challenge/FDIC-Challenge/Cleaning")
#reading in the data, see data codebook for fdic_data variable explanation 
#(not included in folder)
fdic <- read.csv("FDIC_data.csv")
#percapita calculation
fdic_percap <- c("farm_jobs","nonfarm_jobs","owner_jobs")
fdic_percap_ins <- paste(fdic_percap, "_percap", sep = "")
fdic[fdic_percap_ins] = fdic[fdic_percap]/ fdic$pop
#inflation
fdic["income_adjinf_percap"] = fdic$percap_pers_inc * 100/fdic$cpi_2018
#renaming
fdic <- fdic %>% rename(FIPS = FIPS_county)
fdic <- fdic %>% filter(year > 1997)

setwd("../")
setwd("Industry_Analysis")

sb1 <- read.csv("industry_data/23----.csv")
#percapita calculation
#join data where FIPS and year data is present in both, eliminate missing FIPS rows
SB_FDIC1 <- inner_join(fdic, sb1, by = c("FIPS", "year"))
SB_FDIC1$X <- NULL

SB_FDIC1 <- SB_FDIC1[!is.na(SB_FDIC1$cpi_2018),]
a <- table(SB_FDIC1$FIPS)
a <- as.numeric(row.names(a[a != 40]))
SB_FDIC1 <- SB_FDIC1 %>% filter(!FIPS %in% a)
sb_percap <- c("est","n1_4","n5_9", "n1_19","n20_49", "n50_99", "n1_19_change", "est_change")
sb_percap_ins <- paste(sb_percap, "_percap", sep = "")
SB_FDIC1[sb_percap_ins] = SB_FDIC1[sb_percap]/ SB_FDIC1$pop

#Adjusting everything for inflation and making it per capita
quantitative_inf <- c("deposits", "asset", "all_loans", "equity", "ci_loans", "na_asset",                   
                      "na_ci_loans", "dollarloans_nonfarm_bus_LT1M", "dollarloans_ci_LT1M",
                      "farm_inc", "nonfarm_inc", "owner_inc")     

quantitative_inf2 <- paste(quantitative_inf, "_adjinf_percap", sep = "")
SB_FDIC1[quantitative_inf2] <- (SB_FDIC1[quantitative_inf] * 100/SB_FDIC1$cpi_2018)/SB_FDIC1$pop

quantitative<- c("deposits_adjinf_percap", "asset_adjinf_percap", "all_loans_adjinf_percap", "equity_adjinf_percap", 
                 "ci_loans_adjinf_percap", "na_asset_adjinf_percap", "na_ci_loans_adjinf_percap", 
                 "dollarloans_nonfarm_bus_LT1M_adjinf_percap", "dollarloans_ci_LT1M_adjinf_percap") 

quantitative2 <- c("deposits_ratio", "asset_ratio", "all_loans_ratio", "equity_ratio", "ci_loans_ratio", 
                   "na_asset_ratio", "na_ci_loans_ratio", "dollarloans_nonfarm_bus_LT1M_ratio", "dollarloans_ci_LT1M_ratio") 

#descriptive variables
vars <- c("FIPS", "county", "state", "year", "pop", "jobs", "income_adjinf_percap", "est_percap", "n1_4_percap", "n5_9_percap", 
          "n1_19_percap", "n20_49_percap", "n50_99_percap", "n1_19_change_percap", "est_change_percap", 
          "farm_jobs_percap", "nonfarm_jobs_percap", "owner_jobs_percap", "farm_inc_adjinf_percap", 
          "nonfarm_inc_adjinf_percap", "owner_inc_adjinf_percap", "cpi_2018")

cb1 <- SB_FDIC1[c(TRUE, FALSE),][quantitative]
noncb1 <- SB_FDIC1[c(FALSE, TRUE),][quantitative]
ratio1 <- cb1/noncb1
SB_FDIC1_a <- SB_FDIC1[c(TRUE, FALSE),][vars]
SB_FDIC1_a[quantitative2] <- ratio1

a <- SB_FDIC1_a$na_asset_ratio != Inf
SB_FDIC1_a <- SB_FDIC1_a[a,]
a <- SB_FDIC1_a$na_ci_loans_ratio != Inf
SB_FDIC1_a <- SB_FDIC1_a[a,]
a <- SB_FDIC1_a$dollarloans_nonfarm_bus_LT1M_ratio != Inf
SB_FDIC1_a <- SB_FDIC1_a[a,]
a <- !is.na(SB_FDIC1_a$na_ci_loans_ratio)
SB_FDIC1_a <- SB_FDIC1_a[a,]

#differences on differences regression - all business growth
bChange_1 <- SB_FDIC1_a[!is.na(SB_FDIC1_a$est_change_percap),]

bChange_1 <- bChange_1 %>% 
  filter(between(bChange_1$ci_loans_ratio, 
                 quantile(bChange_1$ci_loans_ratio, 0.01), 
                 quantile(bChange_1$ci_loans_ratio, 0.99)))

firstq_ci <- summary(bChange_1$ci_loans_ratio)[2]
thirdq_ci <- summary(bChange_1$ci_loans_ratio)[4]

#small business change ratio treatment
ci_treated <- ifelse(bChange_1$ci_loans_ratio > firstq_ci, 
                     ifelse(bChange_1$ci_loans_ratio < thirdq_ci, -1, 1), 0)
bChange_1$treatment <- ci_treated
ci_treated <- ci_treated[ci_treated != -1]

bChange_1 <- bChange_1 %>% filter(treatment != -1)

year <- ifelse(bChange_1$year >= 2009, 1, 0)
growth <- bChange_1$est_change_percap * 10000

didreg_ci <- lm(growth ~ ci_treated * year)
didreg_ci_cfe <- lm(growth ~ ci_treated * year + factor(bChange_1$county))
didreg_ci_cfe_yfe <- lm(growth ~ ci_treated * year + factor(bChange_1$county) + factor(bChange_1$year))

#1-19 business growth
onenineteenC <- SB_FDIC1_a[!is.na(SB_FDIC1_a$n1_19_change_percap),]

onenineteenC <- onenineteenC %>% 
  filter(between(onenineteenC$ci_loans_ratio, 
                 quantile(onenineteenC$ci_loans_ratio, 0.01), 
                 quantile(onenineteenC$ci_loans_ratio, 0.99)))

firstq19_ci <- summary(onenineteenC$ci_loans_ratio)[2]
thirdq19_ci <- summary(onenineteenC$ci_loans_ratio)[4]

ci_treated <- ifelse(onenineteenC$ci_loans_ratio > firstq19_ci, 
                     ifelse(onenineteenC$ci_loans_ratio < thirdq19_ci, -1, 1), 0)
onenineteenC$treatment <- ci_treated
ci_treated <- ci_treated[ci_treated != -1]

onenineteenC <- onenineteenC %>% filter(treatment != -1)

year <- ifelse(onenineteenC$year >= 2009, 1, 0)
growth <- onenineteenC$n1_19_change_percap * 10000

didreg1_19_ci <- lm(growth ~ ci_treated * year)
didreg1_19_ci_cfe <- lm(growth ~ ci_treated * year + factor(onenineteenC$county))
didreg1_19_ci_cfe_yfe <- lm(growth ~ ci_treated * year + factor(onenineteenC$county) + factor(onenineteenC$year))

stargazer(didreg_ci, didreg_ci_cfe, didreg_ci_cfe_yfe, didreg1_19_ci, didreg1_19_ci_cfe, didreg1_19_ci_cfe_yfe,
          type = "html", 
          omit = c("county", "20"),
          covariate.labels = c("CB Presence", "Post-2008","CB Presence · Post-2008", "Intercept"),
          column.separate = c(3, 3), 
          dep.var.labels = c("Construction"),
          column.labels= c("All Businesses", "Businesses with under 20 employees"),
          dep.var.caption = "Establishment Growth per 10,000 people",
          add.lines = list(c("County Fixed effects?", rep(c("No", "Yes", "Yes"), 2)),
                           c("Year Fixed effects?", rep(c("No", "No", "Yes"), 2))))


