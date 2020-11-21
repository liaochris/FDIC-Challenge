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
sb_percap <- c("est","n1_4","n5_9", "n1_19", "n10_19","n20_49", "n50_99")
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
          "n10_19_percap", "n20_49_percap", "n50_99_percap", "farm_jobs_percap", "nonfarm_jobs_percap", "owner_jobs_percap", 
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
c_year <- ifelse(bChange_1$year >= 2009, 1, 0)
c_businessChange <- bChange_1$businessChange
median_ar <- summary(bChange_1$asset_ratio)[3]
median_lr <- summary(bChange_1$all_loans_ratio)[3]
median_ci <- summary(bChange_1$ci_loans_ratio)[3]
median_ci1m <- summary(bChange_1$dollarloans_ci_LT1M_ratio)[3]
median_e <- summary(bChange_1$equity_ratio)[3]
median_nf1m <- summary(bChange_1$dollarloans_nonfarm_bus_LT1M_ratio)[3]
median_na_ar <- summary(bChange_1$na_asset_ratio)[3]
median_na_ci <- summary(bChange_1$na_ci_loans_ratio)[3]

#small business change ratio treatment
c_asset_ratio_treatment<- ifelse(bChange_1$asset_ratio > median_ar, 1, 0)
c_loan_ratio_treatment <- ifelse(bChange_1$all_loans_ratio > median_lr, 1, 0)
c_ci_ratio_treatment <- ifelse(bChange_1$ci_loans_ratio > median_ci, 1, 0)
c_ci1m_ratio_treatment <- ifelse(bChange_1$dollarloans_ci_LT1M_ratio > median_ci1m, 1, 0)
c_equity_ratio_treatment <- ifelse(bChange_1$equity_ratio > median_e, 1, 0)
c_nonfarm1m_ratio_treatment <- ifelse(bChange_1$dollarloans_nonfarm_bus_LT1M_ratio > median_nf1m, 1, 0)
c_na_asset_ratio_treatment <- ifelse(bChange_1$na_asset_ratio > median_na_ar, 1, 0)
c_na_ci_loan_treatment <- ifelse(bChange_1$na_ci_loans_ratio > median_na_ci, 1, 0)

didreg_ar <- summary(lm(c_businessChange ~ c_asset_ratio_treatment * c_year))
didreg_lr <- summary(lm(c_businessChange ~ c_loan_ratio_treatment * c_year))
didreg_ci <- summary(lm(c_businessChange ~ c_ci_ratio_treatment * c_year))
didreg_ci1m <- summary(lm(c_businessChange ~ c_ci1m_ratio_treatment * c_year))
didreg_e <- summary(lm(c_businessChange ~ c_equity_ratio_treatment * c_year))
didreg_nf1m <- summary(lm(c_businessChange ~ c_nonfarm1m_ratio_treatment * c_year))
didreg_na_ar <- summary(lm(c_businessChange ~ c_na_asset_ratio_treatment * c_year))
didreg_ci_loans <- summary(lm(c_businessChange ~ c_na_ci_loan_treatment * c_year))

#1-4 business growth
onefourC <- SB_FDIC1_a[!is.na(SB_FDIC1_a$onefourChange),]
onefour_year <- ifelse(onefourC$year >= 2009, 1, 0)
est_1_4 <- onefourC$onefourChange

#differences on differences regression
median_ar <- summary(onefourC$asset_ratio)[3]
median_lr <- summary(onefourC$all_loans_ratio)[3]
median_ci <- summary(onefourC$ci_loans_ratio)[3]
median_ci1m <- summary(onefourC$dollarloans_ci_LT1M_ratio)[3]
median_e <- summary(onefourC$equity_ratio)[3]
median_nf1m <- summary(onefourC$dollarloans_nonfarm_bus_LT1M_ratio)[3]
median_na_ar <- summary(onefourC$na_asset_ratio)[3]
median_na_ci <- summary(onefourC$na_ci_loans_ratio)[3]

#small business change ratio treatment
c_asset_ratio_treatment<- ifelse(onefourC$asset_ratio > median_ar, 1, 0)
c_loan_ratio_treatment <- ifelse(onefourC$all_loans_ratio > median_lr, 1, 0)
c_ci_ratio_treatment <- ifelse(onefourC$ci_loans_ratio > median_ci, 1, 0)
c_ci1m_ratio_treatment <- ifelse(onefourC$dollarloans_ci_LT1M_ratio > median_ci1m, 1, 0)
c_equity_ratio_treatment <- ifelse(onefourC$equity_ratio > median_e, 1, 0)
c_nonfarm1m_ratio_treatment <- ifelse(onefourC$dollarloans_nonfarm_bus_LT1M_ratio > median_nf1m, 1, 0)
c_na_asset_ratio_treatment <- ifelse(onefourC$na_asset_ratio > median_na_ar, 1, 0)
c_na_ci_loan_treatment <- ifelse(onefourC$na_ci_loans_ratio > median_na_ci, 1, 0)

didreg14_ar <- summary(lm(est_1_4 ~ c_asset_ratio_treatment * onefour_year))
didreg14_lr <- summary(lm(est_1_4 ~ c_loan_ratio_treatment * onefour_year))
didreg14_ci <- summary(lm(est_1_4 ~ c_ci_ratio_treatment * onefour_year))
didreg14_ci1m <- summary(lm(est_1_4 ~ c_ci1m_ratio_treatment * onefour_year))
didreg14_e <- summary(lm(est_1_4 ~ c_equity_ratio_treatment * onefour_year))
didreg14_nf1m <- summary(lm(est_1_4 ~ c_nonfarm1m_ratio_treatment * onefour_year))
didreg14_na_ar <- summary(lm(est_1_4 ~ c_na_asset_ratio_treatment * onefour_year))
didreg14_ci_loans <- summary(lm(est_1_4 ~ c_na_ci_loan_treatment * onefour_year))

#5-9 business growth
fivenineC <- SB_FDIC1_a[!is.na(SB_FDIC1_a$fivenineChange),]
fivenine_year <- ifelse(fivenineC$year >= 2010, 1, 0)
est_5_9 <- fivenineC$fivenineChange

#differences on differences regression
median1_ar <- summary(fivenineC$asset_ratio)[3]
median1_lr <- summary(fivenineC$all_loans_ratio)[3]
median1_ci <- summary(fivenineC$ci_loans_ratio)[3]
median1_ci1m <- summary(fivenineC$dollarloans_ci_LT1M_ratio)[3]
median1_e <- summary(fivenineC$equity_ratio)[3]
median1_nf1m <- summary(fivenineC$dollarloans_nonfarm_bus_LT1M_ratio)[3]
median1_na_ar <- summary(fivenineC$na_asset_ratio)[3]
median1_na_ci <- summary(fivenineC$na_ci_loans_ratio)[3]

#small business change ratio treatment
c_asset_ratio_treatment<- ifelse(fivenineC$asset_ratio > median1_ar, 1, 0)
c_loan_ratio_treatment <- ifelse(fivenineC$all_loans_ratio > median1_lr, 1, 0)
c_ci_ratio_treatment <- ifelse(fivenineC$ci_loans_ratio > median1_ci, 1, 0)
c_ci1m_ratio_treatment <- ifelse(fivenineC$dollarloans_ci_LT1M_ratio > median1_ci1m, 1, 0)
c_equity_ratio_treatment <- ifelse(fivenineC$equity_ratio > median1_e, 1, 0)
c_nonfarm1m_ratio_treatment <- ifelse(fivenineC$dollarloans_nonfarm_bus_LT1M_ratio > median1_nf1m, 1, 0)
c_na_asset_ratio_treatment <- ifelse(fivenineC$na_asset_ratio > median1_na_ar, 1, 0)
c_na_ci_loan_treatment <- ifelse(fivenineC$na_ci_loans_ratio > median1_na_ci, 1, 0)

didreg59_ar <- summary(lm(est_5_9 ~ c_asset_ratio_treatment * fivenine_year))
didreg59_lr <- summary(lm(est_5_9 ~ c_loan_ratio_treatment * fivenine_year))
didreg59_ci <- summary(lm(est_5_9 ~ c_ci_ratio_treatment * fivenine_year))
didreg59_ci1m <- summary(lm(est_5_9 ~ c_ci1m_ratio_treatment * fivenine_year))
didreg59_e <- summary(lm(est_5_9 ~ c_equity_ratio_treatment * fivenine_year))
didreg59_nf1m <- summary(lm(est_5_9 ~ c_nonfarm1m_ratio_treatment * fivenine_year))
didreg59_na_ar <- summary(lm(est_5_9 ~ c_na_asset_ratio_treatment * fivenine_year))
didreg59_ci_loans <- summary(lm(est_5_9 ~ c_na_ci_loan_treatment * fivenine_year))

#1-9 business growth
onenineteenC <- SB_FDIC1_a[!is.na(SB_FDIC1_a$onenineteenChange),]
onenineteen_year <- ifelse(onenineteenC$year >= 2009, 1, 0)
est_1_19 <- onenineteenC$onenineteenChange

#differences on differences regression
median3_ar <- summary(onenineteenC$asset_ratio)[3]
median3_lr <- summary(onenineteenC$all_loans_ratio)[3]
median3_ci <- summary(onenineteenC$ci_loans_ratio)[3]
median3_ci1m <- summary(onenineteenC$dollarloans_ci_LT1M_ratio)[3]
median3_e <- summary(onenineteenC$equity_ratio)[3]
median3_nf1m <- summary(onenineteenC$dollarloans_nonfarm_bus_LT1M_ratio)[3]
median3_na_ar <- summary(onenineteenC$na_asset_ratio)[3]
median3_na_ci <- summary(onenineteenC$na_ci_loans_ratio)[3]

#small business change ratio treatment
c19_asset_ratio_treatment<- ifelse(onenineteenC$asset_ratio > median3_ar, 1, 0)
c19_loan_ratio_treatment <- ifelse(onenineteenC$all_loans_ratio > median3_lr, 1, 0)
c19_ci_ratio_treatment <- ifelse(onenineteenC$ci_loans_ratio > median3_ci, 1, 0)
c19_ci1m_ratio_treatment <- ifelse(onenineteenC$dollarloans_ci_LT1M_ratio > median3_ci1m, 1, 0)
c19_equity_ratio_treatment <- ifelse(onenineteenC$equity_ratio > median3_e, 1, 0)
c19_nonfarm1m_ratio_treatment <- ifelse(onenineteenC$dollarloans_nonfarm_bus_LT1M_ratio > median3_nf1m, 1, 0)
c19_na_asset_ratio_treatment <- ifelse(onenineteenC$na_asset_ratio > median3_na_ar, 1, 0)
c19_na_ci_loan_treatment <- ifelse(onenineteenC$na_ci_loans_ratio > median3_na_ci, 1, 0)

didreg1_19_ar <- summary(lm(est_1_19 ~ c19_asset_ratio_treatment * onenineteen_year))
didreg1_19_lr <- summary(lm(est_1_19 ~ c19_loan_ratio_treatment * onenineteen_year))
didreg1_19_ci <- summary(lm(est_1_19 ~ c19_ci_ratio_treatment * onenineteen_year))
didreg1_19_ci1m <- summary(lm(est_1_19 ~ c19_ci1m_ratio_treatment * onenineteen_year))
didreg1_19_e <- summary(lm(est_1_19 ~ c19_equity_ratio_treatment * onenineteen_year))
didreg1_19_nf1m <- summary(lm(est_1_19 ~ c19_nonfarm1m_ratio_treatment * onenineteen_year))
didreg1_19_na_ar <- summary(lm(est_1_19 ~ c19_na_asset_ratio_treatment * onenineteen_year))
didreg1_19_ci_loans <- summary(lm(est_1_19 ~ c19_na_ci_loan_treatment * onenineteen_year))
