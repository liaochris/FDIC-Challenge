library(dplyr)
library(ggplot2)

setwd("~/")
setwd("Google Drive/UChicago/Non-Academic Work/Freshman Year/Fed Challenge/FDIC/Export")

#reading in the data, see data codebook for fdic_data variable explanation 
#(not included in folder)
fdic <- read.csv("Changes1.csv")
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
setwd("Industries")
sb1 <- read.csv("44----.csv")
#percapita calculation
#join data where FIPS and year data is present in both, eliminate missing FIPS rows
SB_FDIC1 <- inner_join(fdic, sb1, by = c("FIPS", "year"))
SB_FDIC1$X <- NULL

SB_FDIC1 <- SB_FDIC1[!is.na(SB_FDIC1$cpi_2018),]
a <- table(SB_FDIC1$FIPS)
a <- as.numeric(row.names(a[a != 40]))
SB_FDIC1 <- SB_FDIC1 %>% filter(!FIPS %in% a)
print(unique(SB_FDIC1$naics_desc))
sb_percap <- c("est","n1_4","n5_9", "n1_19","n20_49", "n50_99", "businessChange2", "onenineteenChange2")
sb_percap_ins <- paste(sb_percap, "_percap", sep = "")
SB_FDIC1[sb_percap_ins] = SB_FDIC1[sb_percap]/ SB_FDIC1$pop

#Adjusting everything for inflation and making it per capita
quantitative_inf <- c("deposits", "asset", "all_loans", "equity", "ci_loans", "na_asset",                   
                      "na_ci_loans", "dollarloans_nonfarm_bus_LT1M", "dollarloans_ci_LT1M",
                      "farm_inc", "nonfarm_inc", "owner_inc")     

quantitative_inf2 <- paste(quantitative_inf, "_adjinf_percap", sep = "")
SB_FDIC1[quantitative_inf2] <- (SB_FDIC1[quantitative_inf] * 100/SB_FDIC1$cpi_2018)/SB_FDIC1$pop

quantitative<- c("deposits_adjinf_percap", "asset_adjinf_percap", "all_loans_adjinf_percap", "equity_adjinf_percap", 
                 "ci_loans_adjinf_percap", "na_asset_adjinf_percap", "na_ci_loans_adjinf_percap", "dollarloans_nonfarm_bus_LT1M_adjinf_percap", 
                 "dollarloans_ci_LT1M_adjinf_percap") 

quantitative2 <- c("deposits_ratio", "asset_ratio", "all_loans_ratio", "equity_ratio", "ci_loans_ratio", 
                   "na_asset_ratio", "na_ci_loans_ratio", "dollarloans_nonfarm_bus_LT1M_ratio", "dollarloans_ci_LT1M_ratio") 

#descriptive variables
vars <- c("FIPS", "county", "state", "year", "pop", "jobs", "income_adjinf_percap", "est_percap", "n1_4_percap", "n5_9_percap", "n1_19_percap",
          "n20_49_percap", "n50_99_percap", "businessChange2_percap", "onenineteenChange2_percap", "farm_jobs_percap", "nonfarm_jobs_percap", "owner_jobs_percap", 
          "farm_inc_adjinf_percap", "nonfarm_inc_adjinf_percap", "owner_inc_adjinf_percap", "businessChange",  "onefourChange", "fivenineChange","onenineteenChange", "cpi_2018")

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


#small business regression
smallbiz_1 <- summary(lm(est_percap ~ deposits_ratio + asset_ratio + all_loans_ratio + equity_ratio + 
                           ci_loans_ratio + na_asset_ratio + na_ci_loans_ratio + dollarloans_nonfarm_bus_LT1M_ratio +
                           dollarloans_ci_LT1M_ratio, SB_FDIC1_a))

#regressions on change in businesses
bChange_1 <- SB_FDIC1_a[!is.na(SB_FDIC1_a$businessChange),]
bChangeReg_1 <- summary(lm(businessChange ~ deposits_ratio + asset_ratio + all_loans_ratio + equity_ratio + 
                             ci_loans_ratio + na_asset_ratio +
                             na_ci_loans_ratio + dollarloans_nonfarm_bus_LT1M_ratio + dollarloans_ci_LT1M_ratio, bChange_1))

#differences on differences regression - all business growth
bChange_1 <- SB_FDIC1_a[!is.na(SB_FDIC1_a$businessChange2_percap),]
year <- ifelse(bChange_1$year >= 2009, 1, 0)
growth <- bChange_1$businessChange2_percap * 10000

median_ci_ratio <- summary(bChange_1$ci_loans_ratio)[3]
median_ci1m_ratio <- summary(bChange_1$dollarloans_ci_LT1M_ratio)[3]
median_na_ci <- summary(bChange_1$na_ci_loans_ratio)[3]

#small business change ratio treatment
ci_treated <- ifelse(bChange_1$ci_loans_ratio > median_ci, 1, 0)
ci1m_treated <- ifelse(bChange_1$dollarloans_ci_LT1M_ratio > median_ci1m, 1, 0)
na_ci_treated  <- ifelse(bChange_1$na_ci_loans_ratio > median_na_ci, 1, 0)

didreg_ci <- lm(growth ~ ci_treated * year)
didreg_ci1m <- lm(growth ~ ci1m_treated * year)
didreg_ci_loans <- lm(growth ~ na_ci_treated * year)

#1-19 business growth
onenineteenC <- SB_FDIC1_a[!is.na(SB_FDIC1_a$onenineteenChange2),]
year <- ifelse(onenineteenC$year >= 2009, 1, 0)
growth <- onenineteenC$onenineteenChange2_percap * 10000

#differences on differences regression
median3_ci <- summary(onenineteenC$ci_loans_ratio)[3]
median3_ci1m <- summary(onenineteenC$dollarloans_ci_LT1M_ratio)[3]
median3_na_ci <- summary(onenineteenC$na_ci_loans_ratio)[3]

#small business change ratio treatment
ci_treated <- ifelse(onenineteenC$ci_loans_ratio > median3_ci, 1, 0)
ci1m_treated <- ifelse(onenineteenC$dollarloans_ci_LT1M_ratio > median3_ci1m, 1, 0)
na_ci_treated <- ifelse(onenineteenC$na_ci_loans_ratio > median3_na_ci, 1, 0)

didreg1_19_ci <- lm(growth ~ ci_treated * year)
didreg1_19_ci1m <- lm(growth ~ ci1m_treated * year)
didreg1_19_ci_loans <- lm(growth ~ na_ci_treated * year)

stargazer(didreg_ci,didreg1_19_ci, type = "html", 
          covariate.labels = c("CI Loan Ratio Treatment", "Year",
                               "CI Loan Treatment · Year",
                               "Intercept"),
          column.separate = c(1, 1), column.labels= c("All Businesses", 
                                                      "Businesses with under 20 employees"),
          dep.var.caption = "Dependent Variable (Retail)")

