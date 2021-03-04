library(dplyr)
library(ggplot2)
library(stargazer)
library(data.table)

#removed top 1 and bottom 1 percentile
#compared top 25 to bottom 25

setwd("~/Google Drive/Non-Academic Work/Freshman Year/Clubs/Fed Challenge/FDIC/Cleaning/")
#reading in the data, see data codebook for fdic_data variable explanation 
#(not included in folder)
fdic <- read.csv("FDIC_data.csv")

CI_total <- fdic %>% group_by(year) %>% summarise(ci_loan_agg = sum(ci_loans))
CI_total_cb <- fdic %>% group_by(year, cb) %>% summarise(ci_loan_agg = sum(ci_loans))
CI_total$cb <- (CI_total_cb %>% filter(cb == 1))$ci_loan_agg
CI_total$ncb <- (CI_total_cb %>% filter(cb == 0))$ci_loan_agg


ggplot(CI_total, aes(x=year)) + 
  geom_line(aes(y = ci_loan_agg, color = "Aggregate")) + 
  geom_line(aes(y = cb, color="Community Bank")) + 
  geom_line(aes(y = ncb, color="Non-Community Bank")) + 
  ggtitle("Commercial Industrial Loans Over Time") +
  labs(x = "Year",
       y = "$ (USD)",
       color = "Legend")

CI_total$cpi_adj_cb <- CI_total$cb[1]/unique(fdic$cpi_2018)[1] * unique(fdic$cpi_2018)
population <- fdic %>% group_by(year) %>% summarise(pop = sum(pop))
population$pop <- population$pop/2
CI_total$adj_pop_cb <-  CI_total$cb[1]/population$pop[1] * population$pop

ggplot(CI_total, aes(x=year)) + 
  geom_line(aes(y = cb, color="Community Bank")) + 
  geom_line(aes(y = cpi_adj_cb, color="CPI Index")) + 
  geom_line(aes(y = adj_pop, color="Pop")) + 
  ggtitle("Community Bank Commercial Industrial Loans") + 
  labs(x = "Year",
       y = "$ (USD)",
       color = "Legend")

CI_total$cpi_adj_ncb <- CI_total$ncb[1]/unique(fdic$cpi_2018)[1] * unique(fdic$cpi_2018)
CI_total$adj_pop_ncb <-  CI_total$ncb[1]/population$pop[1] * population$pop

ggplot(CI_total, aes(x=year)) + 
  geom_line(aes(y = ncb, color="Non-Community Bank")) + 
  geom_line(aes(y = cpi_adj_ncb, color="CPI Index")) + 
  geom_line(aes(y = adj_pop_ncb, color="Pop")) + 
  ggtitle("Non-Community Bank Commercial Industrial Loans") + 
  labs(x = "Year",
       y = "$ (USD)",
       color = "Legend")

CI_total$ratio <- CI_total$cb/CI_total$ncb
ggplot(CI_total, aes(x=year)) + 
  geom_line(aes(y = ratio*100), color = "darkred") +
  labs(x = "Year", y = "%") +
  ggtitle("% of Commercial Industrial Loans Offered by Community banks")



#percapita calculation
fdic_percap <- c("farm_jobs","nonfarm_jobs","owner_jobs")
fdic_percap_ins <- paste(fdic_percap, "_percap", sep = "")
fdic[fdic_percap_ins] = fdic[fdic_percap]/ fdic$pop
#inflation
fdic["income_adjinf_percap"] = fdic$percap_pers_inc * 100/fdic$cpi_2018
#renaming
fdic <- fdic %>% rename(FIPS = FIPS_county)
fdic <- fdic %>% filter(year > 1997)

fdic <- fdic[!is.na(SB_FDIC1$cpi_2018),]
a <- table(fdic$FIPS)
a <- as.numeric(row.names(a[a != 42]))

fdic <- fdic %>% filter(!FIPS %in% a)
fdic <- fdic[-1]

#Adjusting everything for inflation and making it per capita
quantitative_inf <- c("deposits", "asset", "all_loans", "equity", "ci_loans",                   
                      "dollarloans_ci_LT1M", "nonfarm_inc", "owner_inc")     

quantitative_inf2 <- paste(quantitative_inf, "_adjinf_percap", sep = "")
fdic[quantitative_inf2] <- (fdic[quantitative_inf] * 100/fdic$cpi_2018)/fdic$pop

quantitative<- c("deposits_adjinf_percap", "asset_adjinf_percap", "all_loans_adjinf_percap", "equity_adjinf_percap", 
                 "ci_loans_adjinf_percap", "na_asset_adjinf_percap", "na_ci_loans_adjinf_percap", 
                 "dollarloans_nonfarm_bus_LT1M_adjinf_percap", "dollarloans_ci_LT1M_adjinf_percap") 

quantitative2 <- c("deposits_ratio", "asset_ratio", "all_loans_ratio", "equity_ratio", 
                   "ci_loans_ratio", "dollarloans_ci_LT1M_ratio") 

cb1 <- fdic[c(TRUE, FALSE),][quantitative]
noncb1 <- fdic[c(FALSE, TRUE),][quantitative]
ratio1 <- cb1/noncb1
fdic[quantitative2] <- ratio1

a <- fdic$na_asset_ratio != Inf
fdic <- fdic[a,]
a <- fdic$na_ci_loans_ratio != Inf
fdic <- fdic[a,]
a <- fdic$dollarloans_nonfarm_bus_LT1M_ratio != Inf
fdic <- fdic[a,]
a <- !is.na(fdic$na_ci_loans_ratio)
fdic <- fdic[a,]

fdic$income_adjinf_percap
