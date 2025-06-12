# Code for penny?.tex titled: "Penny Elimination and Inflation"
# This code uses the 2024 translation-level public data

# Packages used
library(ggplot2); theme_set(theme_bw())# for graphics
library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displayes multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 
#library(nnet)# multinomial regressions
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
library("xtable") #exporting to LaTeX
library(dplyr)# for sample_n
library(gtools)# for stars.pval function

#setwd("C:/Oz_local_workspace_1/penny_R")
setwd("~/Papers/penny/penny_R")
dir()
#
trans2024_1.df = readRDS("dcpc-2024-tranlevel-public.rds")
dim(trans2024_1.df)
names(trans2024_1.df)

# analyzing the in-person variable
table(trans2024_1.df$in_person)
table(trans2024_1.df$in_person==1)
table(is.na(trans2024_1.df$in_person))

# analyzing the pi variable 
table(trans2024_1.df$pi)
table(is.na(trans2024_1.df$pi))
nrow(subset(trans2024_1.df, pi==1))

# restrict data to: in-person & cash only
trans2024_2.df = subset(trans2024_1.df, in_person==1 & pi==1)
dim(trans2024_2.df)

# check for missing amnt
table(is.na(trans2024_2.df$amnt))
sum(is.na(trans2024_2.df$amnt))

# focus on amount and merchant type and ID only
trans2024_3.df = subset(trans2024_2.df, select = c(id,merch,amnt))
dim(trans2024_3.df)# number of in-person cash payments

# deleting payments with 0 amount
nrow(subset(trans2024_3.df, amnt==0))
nrow(subset(trans2024_3.df, is.na(amnt)))
trans2024_4.df = as.data.frame(subset(trans2024_3.df, amnt != 0))
dim(trans2024_4.df)
nrow(trans2024_4.df) - nrow(trans2024_3.df)# number of deleted payments with 0 amount

# Information for Section 2: Data
nrow(trans2024_4.df)# number of cash payments
# number of unique respondents
length(unique(trans2024_4.df$id)) # out of
length(trans2024_4.df$id)
# number of cash payments per cash-using respondent
length(trans2024_4.df$id)/length(unique(trans2024_4.df$id))

# add column with the penny cent components (0 to 99), call it "decimal"
trans2024_4.df$decimal = 100*(trans2024_4.df$amnt %% 1)
head(trans2024_4.df,50)
tail(trans2024_4.df,50)

## add column with the penny component (0 to 9), call it "ending"
trans2024_4.df$ending = round(trans2024_4.df$decimal %% 10)
head(trans2024_4.df,50)
tail(trans2024_4.df,50)

#problem => some endings are 10 instead of 0
nrow(subset(trans2024_4.df, !(ending %in% c(0,1,2,3,4,5,6,7,8,9))))
subset(trans2024_4.df, !(ending %in% c(0,1,2,3,4,5,6,7,8,9)))
# replace 10 endings with 0 endings
trans2024_5.df = trans2024_4.df
trans2024_5.df$ending = ifelse(trans2024_4.df$ending==10, 0, trans2024_4.df$ending)
# verify
nrow(subset(trans2024_5.df, !(ending %in% c(0,1,2,3,4,5,6,7,8,9))))
nrow(subset(trans2024_4.df, ending==0))# before
nrow(subset(trans2024_5.df, ending==0))# after 0 replaced 10 ending

## verify fractions sum up to 1
(frac_ending_2024.vec  = c(sum(trans2024_5.df$ending==0)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==1)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==2)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==3)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==4)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==5)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==6)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==7)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==8)/nrow(trans2024_5.df), sum(trans2024_5.df$ending==9)/nrow(trans2024_5.df)))
sum(frac_ending_2024.vec)# verify sums up to 1

#shorted the name of the trans data frame
d5.df = trans2024_5.df

#Start Figure 1: stats on the dollar amount & histogram ####
mean(d5.df$amnt)
median(d5.df$amnt)
quantile(d5.df$amnt)
# the bins below are not used not needed for the histogram
(bin0_5 = nrow(subset(d5.df, amnt >= 0 & amnt <= 5)))
(bin5_10 = nrow(subset(d5.df, amnt > 5 & amnt <= 10)))
(bin10_15 = nrow(subset(d5.df, amnt > 10 & amnt <= 15)))
(bin15_20 = nrow(subset(d5.df, amnt > 15 & amnt <= 20)))
(bin20_30 = nrow(subset(d5.df, amnt > 20 & amnt <= 30)))
(bin30_50 = nrow(subset(d5.df, amnt > 30 & amnt <= 50)))
(bin50_70 = nrow(subset(d5.df, amnt > 50 & amnt <= 70)))
(bin70_100 = nrow(subset(d5.df, amnt > 70 & amnt <= 100)))
(bin100_200 = nrow(subset(d5.df, amnt > 100 & amnt <= 200)))
#
# frac of payments > $100
(bin100_max = nrow(subset(d5.df, amnt > 100 & amnt <= max(d5.df$amnt))))
bin100_max/nrow(d5.df)
# frac between 0 and $100
1-bin100_max/nrow(d5.df)

# Restrict data to amount <= $100 for the histogram
dim(d5.df)
d5_100.df = subset(d5.df, amnt <= 100)
dim(d5_100.df)

ggplot(d5_100.df, aes(amnt)) +geom_histogram(aes(y = after_stat(count / sum(count))), breaks=seq(0, 100, 5), color="black", fill="cyan", closed="right") +stat_bin(breaks=seq(0, 100, 5), geom = "text", color = "black",  aes(y = after_stat(count / sum(count)), label = scales::percent(after_stat(count / sum(count)), accuracy = 1)), position = position_nudge(y=0.02), size=5) +scale_y_continuous(breaks = seq(0,0.3, 0.05), labels = scales::percent_format(accuracy = 1)) +scale_x_continuous(breaks = seq(0,100,5), minor_breaks = seq(0,100,5)) +labs(y="Fraction of payments (%)", x= "Cash payment amount (US Dollars)")  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +stat_bin(breaks=seq(0, 100, 5), geom = "text", color = "black", aes(y = after_stat(count/sum(count)), label = after_stat(count)), position = position_nudge(y=0.01), size =5) 

# checking the cutoffs (verify right-closed intervals)
nrow(subset(d5_100.df, amnt >= 0 & amnt <=5)) 
nrow(subset(d5_100.df, amnt > 5 & amnt <=10)) 
(amnt_freq.vec = cut(d5_100.df$amnt, breaks = seq(0, 100, 5)))
str(amnt_freq.vec)
table(amnt_freq.vec)
#End Figure 1: stats and histogram on payment amount ####

#Begin Figure 2: bar plot of fraction ending with 0 as function of dollar amount ####
# Use right-closed intervals (x,y]
(frac_0_5 = nrow(subset(d5.df, amnt > 0 & amnt <= 5 & ending ==0))/nrow(subset(d5.df, amnt > 0 & amnt <= 5)))
(frac_0_5_n = nrow(subset(d5.df, amnt > 0 & amnt <= 5 & ending ==0)))
(n_0_5 = nrow(subset(d5.df, amnt > 0 & amnt <= 5)))
#
(frac_5_10 = nrow(subset(d5.df, amnt > 5 & amnt <= 10 & ending ==0))/nrow(subset(d5.df, amnt > 5 & amnt <= 10)))
(frac_5_10_n = nrow(subset(d5.df, amnt > 5 & amnt <= 10 & ending ==0)))
(n_5_10 = nrow(subset(d5.df, amnt > 5 & amnt <= 10)))
#
(frac_10_20 = nrow(subset(d5.df, amnt > 10 & amnt <= 20 & ending ==0))/nrow(subset(d5.df, amnt > 10 & amnt <= 20)))
(frac_10_20_n = nrow(subset(d5.df, amnt > 10 & amnt <= 20 & ending ==0)))
(n_10_20 = nrow(subset(d5.df, amnt > 10 & amnt <= 20)))
#
(frac_20_30 = nrow(subset(d5.df, amnt > 20 & amnt <= 30 & ending ==0))/nrow(subset(d5.df, amnt > 20 & amnt <= 30)))
(frac_20_30_n = nrow(subset(d5.df, amnt > 20 & amnt <= 30 & ending ==0)))
(n_20_30 = nrow(subset(d5.df, amnt > 20 & amnt <= 30)))
#
(frac_30_50 = nrow(subset(d5.df, amnt > 30 & amnt <= 50 & ending ==0))/nrow(subset(d5.df, amnt > 30 & amnt <= 50)))
(frac_30_50_n = nrow(subset(d5.df, amnt > 30 & amnt <= 50 & ending ==0)))
(n_30_50 = nrow(subset(d5.df, amnt > 30 & amnt <= 50)))
#
(frac_50_100 = nrow(subset(d5.df, amnt > 50 & amnt <= 100 & ending ==0))/nrow(subset(d5.df, amnt > 50 & amnt <= 100)))
(frac_50_100_n = nrow(subset(d5.df, amnt > 50 & amnt <= 100 & ending ==0)))
(n_50_100 = nrow(subset(d5.df, amnt > 50 & amnt <= 100)))
#
(frac_100_200 = nrow(subset(d5.df, amnt > 100 & amnt <= 200 & ending ==0))/nrow(subset(d5.df, amnt > 100 & amnt <= 200)))
(frac_100_200_n = nrow(subset(d5.df, amnt > 100 & amnt <= 200 & ending ==0)))
(n_100_200 = nrow(subset(d5.df, amnt > 100 & amnt <= 200)))
#
(frac_200_max = nrow(subset(d5.df, amnt > 200 & amnt <= max(d5.df$amnt) & ending ==0))/nrow(subset(d5.df, amnt > 200 & amnt <= max(d5.df$amnt))))
(frac_200_max_n = nrow(subset(d5.df, amnt > 200 & amnt <= max(d5.df$amnt) & ending ==0)))
(n_200_max = nrow(subset(d5.df, amnt > 200 & amnt <= max(d5.df$amnt))))

# build fraction of payments ending with 5 (not zero), call it frac5
(frac5_0_5 = nrow(subset(d5.df, amnt > 0 & amnt <= 5 & ending ==5))/nrow(subset(d5.df, amnt > 0 & amnt <= 5)))
#
(frac5_5_10 = nrow(subset(d5.df, amnt > 5 & amnt <= 10 & ending ==5))/nrow(subset(d5.df, amnt > 5 & amnt <= 10)))
#
(frac5_10_20 = nrow(subset(d5.df, amnt > 10 & amnt <= 20 & ending ==5))/nrow(subset(d5.df, amnt > 10 & amnt <= 20)))
#
(frac5_20_30 = nrow(subset(d5.df, amnt > 20 & amnt <= 30 & ending ==5))/nrow(subset(d5.df, amnt > 20 & amnt <= 30)))
#
(frac5_30_50 = nrow(subset(d5.df, amnt > 30 & amnt <= 50 & ending ==5))/nrow(subset(d5.df, amnt > 30 & amnt <= 50)))
#
(frac5_50_100 = nrow(subset(d5.df, amnt > 50 & amnt <= 100 & ending ==5))/nrow(subset(d5.df, amnt > 50 & amnt <= 100)))
#
(frac5_100_200 = nrow(subset(d5.df, amnt > 100 & amnt <= 200 & ending ==5))/nrow(subset(d5.df, amnt > 100 & amnt <= 200)))
#
(frac5_200_max = nrow(subset(d5.df, amnt > 200 & amnt <= max(d5.df$amnt) & ending ==5))/nrow(subset(d5.df, amnt > 200 & amnt <= max(d5.df$amnt))))

# build a data.frame
(amount_bins.vec = factor(c("0-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-Max"), levels =c("0-5", "5-10", "10-20", "20-30", "30-50", "50-100", "100-200", "200-Max"), ordered = TRUE ))
length(amount_bins.vec)
levels(amount_bins.vec)

#
(frac_bins.vec = c(frac_0_5, frac_5_10, frac_10_20, frac_20_30, frac_30_50, frac_50_100, frac_100_200, frac_200_max))# zero cents
#
(frac5_bins.vec = c(frac5_0_5, frac5_5_10, frac5_10_20, frac5_20_30, frac5_30_50, frac5_50_100, frac5_100_200, frac5_200_max))# 5 cents ending

(frac0_amount_n.vec = c(frac_0_5_n, frac_5_10_n, frac_10_20_n, frac_20_30_n, frac_30_50_n, frac_50_100_n, frac_100_200_n, frac_200_max_n))
(n_amount.vec = c(n_0_5, n_5_10, n_10_20, n_20_30, n_30_50, n_50_100, n_100_200, n_200_max))# number of payments in each amount bin

(frac_round.df = data.frame(amount_bins.vec, frac_bins.vec, frac5_bins.vec, frac0_amount_n.vec, n_amount.vec))
str(frac_round.df)

ggplot(frac_round.df, aes(x=amount_bins.vec, y=frac_bins.vec)) +geom_bar(stat = "identity",  color="black", fill="cyan") +scale_y_continuous(breaks = seq(0, 1.0, 0.1), labels = scales::percent_format(accuracy = 1)) +labs(y="Fraction of cash payments ending with 0 and 5 pennies (%)", x= "Cash payment amount (US Dollars)")  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_text(aes(label = scales::percent(frac_bins.vec, accuracy=1), vjust =-0.5), size = 5) +geom_bar(aes(x=amount_bins.vec, y=frac5_bins.vec), stat = "identity") +geom_text(y=frac5_bins.vec, aes(label = scales::percent(frac5_bins.vec, accuracy=1), vjust =-0.5), size = 5) +geom_text(aes(label =paste0(after_stat(frac0_amount_n.vec), " of ", after_stat(n_amount.vec))), vjust = 1.4, size = 5)

# discussion of Finding 1 in paper (meaning of zero-edings, items I, II, and III)
(frac0_all = nrow(subset(d5.df, ending ==0))/nrow(subset(d5.df)))
(frac0_all_n = nrow(subset(d5.df, ending ==0)))
(n_all = nrow(subset(d5.df)))
#
(frac5_all = nrow(subset(d5.df, ending ==5))/nrow(subset(d5.df)))
(frac5_all_n = nrow(subset(d5.df, ending ==5)))
(n_all = nrow(subset(d5.df)))
#End Figure 2: bar plot of fraction ending with 0 and 5 cents as function of dollar amount ####

#Begin: List of merchant types ####
### Full merchant names
## Vector of 21 merchant names (Table 1)
(merch1_name = "Grocery stores, convenience stores without gas stations, pharmacies")
(merch2_name = "Gas stations")
(merch3_name = "Sit-down restaurants and bars")
(merch4_name = "Fast food restaurants, coffee shops, cafeterias, food trucks")
(merch5_name = "General merchandise stores, department stores, other stores, online shopping")
(merch6_name = "General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.")
(merch7_name = "Arts, entertainment, recreation")
(merch8_name = "Utilities not paid to the government: electricity, natural gas, water, sewer, trash, heating oil")
(merch9_name = "Taxis, airplanes, delivery")
(merch10_name = "Telephone, internet, cable or satellite TV, video or music streaming services, movie theaters")
(merch11_name = "Building contractors, plumbers, electricians, HVAC, etc.")
(merch12_name = "Professional services: legal, accounting, architectural services; veterinarians, photographers or photo
processers")
(merch13_name = "Hotels, motels, RV parks, campsites")
(merch14_name = "Rent for apartments, homes, or other buildings, real estate companies, property managers, etc.")
(merch15_name = "Mortgage companies, credit card companies, banks, insurance companies, stock brokers, IRA funds, mutual funds, credit unions, sending remittances")
(merch16_name = "Can be a gift or repayment to a family member, friend, or co-worker. Can be a payment to somebody who did a small job for you.")
(merch17_name = "Charitable or religious donations")
(merch18_name = "Hospital, doctor, dentist, nursing homes, etc.")
(merch19_name = "Government taxes or fees")
(merch20_name = "Schools, colleges, childcare centers")
(merch21_name = "Public transportation and tolls")
# Make names a vector
(merch_name.vec = c(merch1_name, merch2_name, merch3_name, merch4_name, merch5_name, merch6_name, merch7_name, merch8_name, merch9_name, merch10_name, merch11_name, merch12_name, merch13_name, merch14_name, merch15_name, merch16_name, merch17_name, merch18_name, merch19_name, merch20_name, merch21_name))
# Finalizing merchant name table
merch_num.vec = 1:21
(merch_name.df = data.frame(merch_num.vec, merch_name.vec))
dim(merch_name.df)

### Abbreviating Table 1's 21 merchant description (from Brian's slides)
# to be used in tables (see below further merchX_abv_fig for figures)
(merch1_abv = "1. Grocery store")
(merch2_abv = "2. Gas station")
(merch3_abv = "3. Restaurant/bar")
(merch4_abv = "4. Fast food/coffee shop")
(merch5_abv = "5. General merchandise store")
(merch6_abv = "6. General service")
(merch7_abv = "7. Art/entertainment")
(merch8_abv = "8. Non-government utility")
(merch9_abv = "9. Taxi/airplane/delivery")
(merch10_abv = "10. Phone/internet/cable")
(merch11_abv = "11. Contractor/plumber/ electrician")
(merch12_abv = "12. Professional service")
(merch13_abv = "13. Hotel/motel/campsite")
(merch14_abv = "14. Rent")
(merch15_abv = "15. Mortgage/insurance/credit card")
(merch16_abv = "16. Person-to-person")
(merch17_abv = "17. Charitable/religious donation")
(merch18_abv = "18. Hospital/doctor/dentist")
(merch19_abv = "19. Government taxes")
(merch20_abv = "20. School/college/childcare centers")
(merch21_abv = "21. Public transport/tolls")
# Make names a vector
(merch_abv.vec = c(merch1_abv, merch2_abv, merch3_abv, merch4_abv, merch5_abv, merch6_abv, merch7_abv, merch8_abv, merch9_abv, merch10_abv, merch11_abv, merch12_abv, merch13_abv, merch14_abv, merch15_abv, merch16_abv, merch17_abv, merch18_abv, merch19_abv, merch20_abv, merch21_abv))

# adding merch abv (shorter for figures) description
(merch1_abv_fig = "1. Grocery store")
(merch2_abv_fig = "2. Gas station")
(merch3_abv_fig = "3. Restaurant/bar")
(merch4_abv_fig = "4. Fast food/coffee shop")
(merch5_abv_fig = "5. General merchandise")
(merch6_abv_fig = "6. General service")
(merch7_abv_fig = "7. Art/entertainment")
(merch8_abv_fig = "8. Non-gov't utility")
(merch9_abv_fig = "9. Taxi/airplane/delivery")
(merch10_abv_fig = "10. Phone/internet/cable")
(merch11_abv_fig = "11. Contractor")
(merch12_abv_fig = "12. Professional service")
(merch13_abv_fig = "13. Hotel/motel/campsite")
(merch14_abv_fig = "14. Rent")
(merch15_abv_fig = "15. Mortg/insur/credit c")
(merch16_abv_fig = "16. Person-to-person")
(merch17_abv_fig = "17. Charitable/religious")
(merch18_abv_fig = "18. Hospital/doctor/dentist")
(merch19_abv_fig = "19. Government taxes")
(merch20_abv_fig = "20. Education/childcare")
(merch21_abv_fig = "21. Public transport/tolls")
# Make names a vector
(merch_abv_fig.vec = c(merch1_abv_fig, merch2_abv_fig, merch3_abv_fig, merch4_abv_fig, merch5_abv_fig, merch6_abv_fig, merch7_abv_fig, merch8_abv_fig, merch9_abv_fig, merch10_abv_fig, merch11_abv_fig, merch12_abv_fig, merch13_abv_fig, merch14_abv_fig, merch15_abv_fig, merch16_abv_fig, merch17_abv_fig, merch18_abv_fig, merch19_abv_fig, merch20_abv_fig, merch21_abv_fig))
#End: List of merchant types ####

#Begin Table 1: Logit regression 0 cents ####
names(d5.df)
# new columns binary 1=ending==0, 0=otherwise
d6.df = d5.df
d6.df$round0 =ifelse(d6.df$ending==0, TRUE, FALSE)
head(d6.df, 50)
table(d6.df$ending)
table(d6.df$round0)

# new columns binary True=ending==5, False=otherwise => Not used until Section 4 (Nickel elimination)
d6.df$round5 =ifelse(d6.df$ending==5, TRUE, FALSE)
sample_n(d6.df, 30)
 table(d6.df$ending)
table(d6.df$round5)

#round_model0 = round ~ amnt# amount is the only variable
#(round_logit0 = logitmfx(round_model1, data = d6.df, atmean = F))

# Model 1 merchant type and amount
round_model1 = round0 ~ amnt + relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
(round_logit1 = logitmfx(round_model1, data = d6.df, atmean = F))

# Model 2 truncate to 0-$100 payments
dim(d6.df)
d6_100.df = subset(d6.df, amnt <= 100)
dim(d6_100.df)
#
round_model2 = round0 ~ amnt + relevel(as.factor(merch), ref = "1")# same as model1 with grocery stores  is the reference merchant type
(round_logit2 = logitmfx(round_model2, data = d6_100.df, atmean = F))# truncated data

# # Model 3 merchant type only (not amount)
# round_model3 = round0 ~ relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
# (round_logit3 = logitmfx(round_model3, data = d6.df, atmean = F))

# Converting to a LaTeX table by replacing the coefficient names column with merchant description. 
# preparing coefficient names
merch_abv.vec# recall
var_list.vec = merch_abv.vec
var_list.vec[1] = "Dollar amount"# replace merchant 1 (reference variable) with Amount variable
var_list.vec
# convert to LaTeX
(texreg.latex = texreg(list(round_logit1, round_logit2), digits = 3, custom.model.names = c("All payments", "Payments $100 and lower"), custom.coef.names = var_list.vec, single.row = T))
#End Table 1: Logit regression 0 cents ####

#Begin Figure 3: bar plot of fraction of cash payments ending with 0 and 5 by merchant type
(frac0_merch1 = nrow(subset(d6.df, merch==1 & ending ==0))/nrow(subset(d6.df, merch==1)))
(frac0_merch1_n = nrow(subset(d6.df, merch==1 & ending ==0)))
(merch1_n = nrow(subset(d6.df, merch==1)))
#
(frac0_merch2 = nrow(subset(d6.df, merch==2 & ending ==0))/nrow(subset(d6.df, merch==2)))
(frac0_merch2_n = nrow(subset(d6.df, merch==2 & ending ==0)))
(merch2_n = nrow(subset(d6.df, merch==2)))
#
(frac0_merch3 = nrow(subset(d6.df, merch==3 & ending ==0))/nrow(subset(d6.df, merch==3)))
(frac0_merch3_n = nrow(subset(d6.df, merch==3 & ending ==0)))
(merch3_n = nrow(subset(d6.df, merch==3)))
#
(frac0_merch4 = nrow(subset(d6.df, merch==4 & ending ==0))/nrow(subset(d6.df, merch==4)))
(frac0_merch4_n = nrow(subset(d6.df, merch==4 & ending ==0)))
(merch4_n = nrow(subset(d6.df, merch==4)))
#
(frac0_merch5 = nrow(subset(d6.df, merch==5 & ending ==0))/nrow(subset(d6.df, merch==5)))
(frac0_merch5_n = nrow(subset(d6.df, merch==5 & ending ==0)))
(merch5_n = nrow(subset(d6.df, merch==5)))
#
(frac0_merch6 = nrow(subset(d6.df, merch==6 & ending ==0))/nrow(subset(d6.df, merch==6)))
(frac0_merch6_n = nrow(subset(d6.df, merch==6 & ending ==0)))
(merch6_n = nrow(subset(d6.df, merch==6)))
#
(frac0_merch7 = nrow(subset(d6.df, merch==7 & ending ==0))/nrow(subset(d6.df, merch==7)))
(frac0_merch7_n = nrow(subset(d6.df, merch==7 & ending ==0)))
(merch7_n = nrow(subset(d6.df, merch==7)))
#
(frac0_merch8 = nrow(subset(d6.df, merch==8 & ending ==0))/nrow(subset(d6.df, merch==8)))
(frac0_merch8_n = nrow(subset(d6.df, merch==8 & ending ==0)))
(merch8_n = nrow(subset(d6.df, merch==8)))
#
(frac0_merch9 = nrow(subset(d6.df, merch==9 & ending ==0))/nrow(subset(d6.df, merch==9)))
(frac0_merch9_n = nrow(subset(d6.df, merch==9 & ending ==0)))
(merch9_n = nrow(subset(d6.df, merch==9)))
#
(frac0_merch10 = nrow(subset(d6.df, merch==10 & ending ==0))/nrow(subset(d6.df, merch==10)))
(frac0_merch10_n = nrow(subset(d6.df, merch==10 & ending ==0)))
(merch10_n = nrow(subset(d6.df, merch==10)))
#
(frac0_merch11 = nrow(subset(d6.df, merch==11 & ending ==0))/nrow(subset(d6.df, merch==11)))
(frac0_merch11_n = nrow(subset(d6.df, merch==11 & ending ==0)))
(merch11_n = nrow(subset(d6.df, merch==11)))
#
(frac0_merch12 = nrow(subset(d6.df, merch==12 & ending ==0))/nrow(subset(d6.df, merch==12)))
(frac0_merch12_n = nrow(subset(d6.df, merch==12 & ending ==0)))
(merch12_n = nrow(subset(d6.df, merch==12)))
#
(frac0_merch13 = nrow(subset(d6.df, merch==13 & ending ==0))/nrow(subset(d6.df, merch==13)))
(frac0_merch13_n = nrow(subset(d6.df, merch==13 & ending ==0)))
(merch13_n = nrow(subset(d6.df, merch==13)))
#
(frac0_merch14 = nrow(subset(d6.df, merch==14 & ending ==0))/nrow(subset(d6.df, merch==14)))
(frac0_merch14_n = nrow(subset(d6.df, merch==14 & ending ==0)))
(merch14_n = nrow(subset(d6.df, merch==14)))
#
(frac0_merch15 = nrow(subset(d6.df, merch==15 & ending ==0))/nrow(subset(d6.df, merch==15)))
(frac0_merch15_n = nrow(subset(d6.df, merch==15 & ending ==0)))
(merch15_n = nrow(subset(d6.df, merch==15)))
#
(frac0_merch16 = nrow(subset(d6.df, merch==16 & ending ==0))/nrow(subset(d6.df, merch==16)))
(frac0_merch16_n = nrow(subset(d6.df, merch==16 & ending ==0)))
(merch16_n = nrow(subset(d6.df, merch==16)))
#
(frac0_merch17 = nrow(subset(d6.df, merch==17 & ending ==0))/nrow(subset(d6.df, merch==17)))
(frac0_merch17_n = nrow(subset(d6.df, merch==17 & ending ==0)))
(merch17_n = nrow(subset(d6.df, merch==17)))
#
(frac0_merch18 = nrow(subset(d6.df, merch==18 & ending ==0))/nrow(subset(d6.df, merch==18)))
(frac0_merch18_n = nrow(subset(d6.df, merch==18 & ending ==0)))
(merch18_n = nrow(subset(d6.df, merch==18)))
#
(frac0_merch19 = nrow(subset(d6.df, merch==19 & ending ==0))/nrow(subset(d6.df, merch==19)))
(frac0_merch19_n = nrow(subset(d6.df, merch==19 & ending ==0)))
(merch19_n = nrow(subset(d6.df, merch==19)))
#
(frac0_merch20 = nrow(subset(d6.df, merch==20 & ending ==0))/nrow(subset(d6.df, merch==20)))
(frac0_merch20_n = nrow(subset(d6.df, merch==20 & ending ==0)))
(merch20_n = nrow(subset(d6.df, merch==20)))
#
(frac0_merch21 = nrow(subset(d6.df, merch==21 & ending ==0))/nrow(subset(d6.df, merch==21)))
(frac0_merch21_n = nrow(subset(d6.df, merch==21 & ending ==0)))
(merch21_n = nrow(subset(d6.df, merch==21)))

# putting all the 21 values in to a vector
(frac0_merch.vec = c(frac0_merch1, frac0_merch2, frac0_merch3, frac0_merch4, frac0_merch5, frac0_merch6, frac0_merch7, frac0_merch8, frac0_merch9, frac0_merch10, frac0_merch11, frac0_merch12, frac0_merch13, frac0_merch14, frac0_merch15, frac0_merch16, frac0_merch17, frac0_merch18, frac0_merch19, frac0_merch20, frac0_merch21))
length(frac0_merch.vec)

# putting all 21 ending =0 by merchant type into a vector
(frac0_merch_n.vec = c(frac0_merch1_n, frac0_merch2_n, frac0_merch3_n, frac0_merch4_n, frac0_merch5_n, frac0_merch6_n, frac0_merch7_n, frac0_merch8_n, frac0_merch9_n, frac0_merch10_n, frac0_merch11_n, frac0_merch12_n, frac0_merch13_n, frac0_merch14_n, frac0_merch15_n, frac0_merch16_n, frac0_merch17_n, frac0_merch18_n, frac0_merch19_n, frac0_merch20_n, frac0_merch21_n))

(merch_n.vec = c(merch1_n, merch2_n, merch3_n, merch4_n, merch5_n, merch6_n, merch7_n, merch8_n, merch9_n, merch10_n, merch11_n, merch12_n, merch13_n, merch14_n, merch15_n, merch16_n, merch17_n, merch18_n, merch19_n, merch20_n, merch21_n))

## frac payments ending with 5 (not 0)
(frac5_merch1 = nrow(subset(d6.df, merch==1 & ending ==5))/nrow(subset(d6.df, merch==1)))
#
(frac5_merch2 = nrow(subset(d6.df, merch==2 & ending ==5))/nrow(subset(d6.df, merch==2)))
#
(frac5_merch3 = nrow(subset(d6.df, merch==3 & ending ==5))/nrow(subset(d6.df, merch==3)))
#
(frac5_merch4 = nrow(subset(d6.df, merch==4 & ending ==5))/nrow(subset(d6.df, merch==4)))
#
(frac5_merch5 = nrow(subset(d6.df, merch==5 & ending==5))/nrow(subset(d6.df, merch==5)))
#
(frac5_merch6 = nrow(subset(d6.df, merch==6 & ending==5))/nrow(subset(d6.df, merch==6)))
#
(frac5_merch7 = nrow(subset(d6.df, merch==7 & ending==5))/nrow(subset(d6.df, merch==7)))
#
(frac5_merch8 = nrow(subset(d6.df, merch==8 & ending==5))/nrow(subset(d6.df, merch==8)))
#
(frac5_merch9 = nrow(subset(d6.df, merch==9 & ending==5))/nrow(subset(d6.df, merch==9)))
#
(frac5_merch10 = nrow(subset(d6.df, merch==10 & ending==5))/nrow(subset(d6.df, merch==10)))
#
(frac5_merch11 = nrow(subset(d6.df, merch==11 & ending==5))/nrow(subset(d6.df, merch==11)))
#
(frac5_merch12 = nrow(subset(d6.df, merch==12 & ending==5))/nrow(subset(d6.df, merch==12)))
#
(frac5_merch13 = nrow(subset(d6.df, merch==13 & ending==5))/nrow(subset(d6.df, merch==13)))
#
(frac5_merch14 = nrow(subset(d6.df, merch==14 & ending==5))/nrow(subset(d6.df, merch==14)))
#
(frac5_merch15 = nrow(subset(d6.df, merch==15 & ending==5))/nrow(subset(d6.df, merch==15)))
#
(frac5_merch16 = nrow(subset(d6.df, merch==16 & ending==5))/nrow(subset(d6.df, merch==16)))
#
(frac5_merch17 = nrow(subset(d6.df, merch==17 & ending==5))/nrow(subset(d6.df, merch==17)))
#
(frac5_merch18 = nrow(subset(d6.df, merch==18 & ending==5))/nrow(subset(d6.df, merch==18)))
#
(frac5_merch19 = nrow(subset(d6.df, merch==19 & ending==5))/nrow(subset(d6.df, merch==19)))
#
(frac5_merch20 = nrow(subset(d6.df, merch==20 & ending==5))/nrow(subset(d6.df, merch==20)))
#
(frac5_merch21 = nrow(subset(d6.df, merch==21 & ending==5))/nrow(subset(d6.df, merch==21)))
#
(frac5_merch.vec = c(frac5_merch1, frac5_merch2, frac5_merch3, frac5_merch4, frac5_merch5, frac5_merch6, frac5_merch7, frac5_merch8, frac5_merch9, frac5_merch10, frac5_merch11, frac5_merch12, frac5_merch13, frac5_merch14, frac5_merch15, frac5_merch16, frac5_merch17, frac5_merch18, frac5_merch19, frac5_merch20, frac5_merch21))
length(frac5_merch.vec)

# making a data frame for bar plot
(frac_merch.df = data.frame(merch_abv_fig.vec, frac0_merch.vec, frac5_merch.vec, frac0_merch.vec, merch_n.vec))

ggplot(frac_merch.df, aes(x=reorder(merch_abv_fig.vec, frac0_merch.vec), y=frac0_merch.vec)) +geom_bar(stat = "identity",  color="black", fill="cyan") +scale_y_continuous(breaks = seq(0, 1.0, 0.1), labels = scales::percent_format(accuracy = 1)) +labs(y="Fraction of cash payments ending with 0 and 5 pennies (%)", x= "Merchant type (21 types)")  +theme(axis.text.x = element_text(size = 14, color = "black"), axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_text(aes(label = scales::percent(frac0_merch.vec, accuracy=1), hjust =-0.3), size = 5) +geom_bar(aes(x=merch_abv_fig.vec, y=frac5_merch.vec), stat = "identity") +geom_text(y=frac5_merch.vec, aes(label = scales::percent(frac5_merch.vec, accuracy=1), hjust =-0.2), size = 5) +geom_text(aes(label =paste0(after_stat(frac0_merch_n.vec), " out of ", after_stat(merch_n.vec), " payments")), hjust = 1.2, size = 5) +coord_flip()
#End Figure 3: bar plot of fraction of cash payments ending with 0 and 5 by merchant type ####

#Begin Table 2: Penny rounding table ####
d7.df = d6.df
names(d7.df)
sample_n(d7.df, 40)

# adding column symmetric rounding (sr) amount (number of pennies), rounding to nearest 0 or 5 cent
d7.df$sr0 = ifelse(d7.df$ending==0, 0, 
  ifelse(d7.df$ending==1, -1, 
  ifelse(d7.df$ending==2, -2, ifelse(d7.df$ending==3, 2, ifelse(d7.df$ending==4, 1, ifelse(d7.df$ending==5, 0, ifelse(d7.df$ending==6, -1, ifelse(d7.df$ending==7, -2, ifelse(d7.df$ending==8, 2, ifelse(d7.df$ending==9, 1, NA))))))))))
#
sample_n(d7.df,30)

# adding column upward rounding (ur) amount
d7.df$ur0 = ifelse(d7.df$ending==0, 0, 
ifelse(d7.df$ending==1, 4, 
ifelse(d7.df$ending==2, 3, 
ifelse(d7.df$ending==3, 2, 
ifelse(d7.df$ending==4, 1, 
ifelse(d7.df$ending==5, 0, 
ifelse(d7.df$ending==6, 4, 
ifelse(d7.df$ending==7, 3, 
ifelse(d7.df$ending==8, 2, 
ifelse(d7.df$ending==9, 1, NA))))))))))
#
sample_n(d7.df,30)

# adding inflation under sr0 rounding (as fractions of pennies, not %). Used to determine the p.values for this ratio
d7.df$sr0_inf = d7.df$sr0 / (100*d7.df$amnt)

# adding inflation under ur rounding (as fractions of pennies, not %). Used to determine the p.values for this ratio
d7.df$ur0_inf = d7.df$ur0 / (100*d7.df$amnt)
#
sample_n(d7.df, 30)

# construct subset of the data without zero-ending and without 5-ending
dim(d7.df)
d7_non_0_non_5.df = subset(d7.df, round0 == F & round5 == F)
dim(d7_non_0_non_5.df)

# stat about number of pennies rounding (symmetric)
(avg_sr0_all = mean(d7.df$sr0))
(med_sr0_all = median(d7.df$sr0))
(sd_sr0_all = sd(d7.df$sr0))
t.test(d7.df$sr0)
(p_sr0_all = t.test(d7.df$sr0)$p.value)
(ci_sr0_all = t.test(d7.df$sr0)$conf.int)

# now for the subset of non 0 non 5 (symmetric)
(avg_sr0_non_0_non_5 = mean(d7_non_0_non_5.df$sr0))
(med_sr0_non_0_non_5 = median(d7_non_0_non_5.df$sr0))
(sd_sr0_non_0_non_5 = sd(d7_non_0_non_5.df$sr0))
t.test(d7_non_0_non_5.df$sr0)
(p_sr0_non_0_non_5 = t.test(d7_non_0_non_5.df$sr0)$p.value)
(ci_sr0_non_0_non_5 = t.test(d7_non_0_non_5.df$sr0)$conf.int)

# stat about number of penny rounding (upward rounding)
(avg_ur0_all = mean(d7.df$ur0))
(med_ur0_all = median(d7.df$ur0))
(sd_ur0_all = sd(d7.df$ur0))
t.test(d7.df$ur0, alternative = "greater")
(p_ur0_all = t.test(d7.df$ur0, alternative = "greater")$p.value)
#
#now for the subset of non 0 non 5 (upward)
(avg_ur0_non_0_non_5 = mean(d7_non_0_non_5.df$ur0))
(med_ur0_non_0_non_5 = median(d7_non_0_non_5.df$ur0))
(sd_ur0_non_0_non_5 = sd(d7_non_0_non_5.df$ur0))
t.test(d7_non_0_non_5.df$ur0, alternative = "greater")
(p_ur0_non_0_non_5 = t.test(d7_non_0_non_5.df$ur0, alternative = "greater")$p.value)

# inflation results (stats): symmetric rounding (in fractions pf pennies)
(avg_sr0_inf_all = mean(d7.df$sr0_inf))
(med_sr0_inf_all = median(d7.df$sr0_inf))
(sd_sr0_inf_all = sd(d7.df$sr0_inf))
t.test(d7.df$sr0_inf)
(p_sr0_inf_all = t.test(d7.df$sr0_inf)$p.value)
#
# now for the subset of non 0 non 5 
(avg_sr0_inf_non_0_non_5 = mean(d7_non_0_non_5.df$sr0_inf))
(med_sr0_inf_non_0_non_5 = median(d7_non_0_non_5.df$sr0_inf))
(sd_sr0_inf_non_0_non_5 = sd(d7_non_0_non_5.df$sr0_inf))
t.test(d7_non_0_non_5.df$sr0_inf)
(p_sr0_inf_non_0_non_5 = t.test(d7_non_0_non_5.df$sr0_inf)$p.value)

# inflation results (stats): upward rounding (fractions)
(avg_ur0_inf_all = mean(d7.df$ur0_inf))
(med_ur0_inf_all = median(d7.df$ur0_inf))
(sd_ur0_inf_all = sd(d7.df$ur0_inf))
t.test(d7.df$ur0_inf, alternative = "greater")
(p_ur0_inf_all = t.test(d7.df$ur0_inf, alternative = "greater")$p.value)
#
# now for the subset of non 0 non 5 (fractions)
(avg_ur0_inf_non_0_non_5 = mean(d7_non_0_non_5.df$ur0_inf))
(med_ur0_inf_non_0_non_5 = median(d7_non_0_non_5.df$ur0_inf))
(sd_ur0_inf_non_0_non_5 = sd(d7_non_0_non_5.df$ur0_inf))
t.test(d7_non_0_non_5.df$ur0_inf, alternative = "greater")
(p_ur0_inf_non_0_non_5 = t.test(d7_non_0_non_5.df$ur0_inf, alternative = "greater")$p.value)

# Constructing (redefining) inflation measures by ratios of TOTAL sums (instead of mean of ratios which is used for the t-test) (in %)
(inf_sr0_all = 100*sum(d7.df$sr0)/(100*sum(d7.df$amnt)))
#
(inf_ur0_all = 100*sum(d7.df$ur0)/(100*sum(d7.df$amnt)))
#
(inf_sr0_non_0_non_5 = 100*sum(d7_non_0_non_5.df$sr0)/(100*sum(d7_non_0_non_5.df$amnt)))
#
(inf_ur0_non_0_non_5 = 100*sum(d7_non_0_non_5.df$ur0)/(100*sum(d7_non_0_non_5.df$amnt)))

# Adding median and average payment amount ($)
(avg_amount_all = mean(d7.df$amnt)) 
(med_amount_all = median(d7.df$amnt)) 
(avg_amount_non_0_non_5 = mean(d7_non_0_non_5.df$amnt)) 
(med_amount_non_0_non_5 = median(d7_non_0_non_5.df$amnt)) 

# Putting the above into a data frame

# row labels
(var0.vec = c("Average rounding (number of pennies)", "Median rounding (number of pennies)", "p-value (rounding)", "Average payment amount ($)", "Median payment amount ($)", "Inflationary impact (%)", "p-value (inflation)"))

# All data column sr0
(sr0_all_data.vec = c(avg_sr0_all, med_sr0_all, p_sr0_all, avg_amount_all, med_amount_all, inf_sr0_all, p_sr0_inf_all))
#
# non_0_non_5 column sr0
(sr0_non_0_non_5_data.vec = c(avg_sr0_non_0_non_5, med_sr0_non_0_non_5, p_sr0_non_0_non_5, avg_amount_non_0_non_5, med_amount_non_0_non_5, inf_sr0_non_0_non_5, p_sr0_inf_non_0_non_5))
#
# All data column ur0
(ur0_all_data.vec = c(avg_ur0_all, med_ur0_all, p_ur0_all, avg_amount_all, med_amount_all,inf_ur0_all, p_ur0_inf_all))
#
# non_0_non_5 column ur0
(ur0_non_0_non_5_data.vec = c(avg_ur0_non_0_non_5, med_ur0_non_0_non_5, p_ur0_non_0_non_5, avg_amount_non_0_non_5, med_amount_non_0_non_5, inf_ur0_non_0_non_5, p_ur0_inf_non_0_non_5))

(penny.df = data.frame("Statistical_variable"=var0.vec, "SR"=sr0_all_data.vec, "UR" = ur0_all_data.vec, "SR"= sr0_non_0_non_5_data.vec, "UR"= ur0_non_0_non_5_data.vec))

# below, create matrix w\ 1 extra column to indicate number of digits for each row
dim(penny.df)
(digitm = matrix(c(rep(3,6), rep(3,6), rep(4,6), rep(2,6), rep(2,6), rep(3,6),  rep(3,6), rep(4,6)), nrow = 7, ncol = 6, byrow = T))
#
print(xtable(penny.df, digits = digitm), include.rownames = F, hline.after = c(0,3,5))

# Discussion of Findings 3b => Canceled because multiplying the inflationary impact by the median payment value may not have meaning 
# median/quantiles payment value (all data)
(all_amnt_med = median(d7.df$amnt))# 
(all_amnt_quantiles = quantile(d7.df$amnt))
(all_amnt_1st_quantile = all_amnt_quantiles[2])
(all_amnt_3rd_quantile = all_amnt_quantiles[4])
# impacts
inf_ur0_all
(penny_inf_impact_med_ur_all = all_amnt_med*inf_ur0_all/100)# 
#
# median/quantiles payment value (all data)
(no_0_no_5_amnt_med = median(d7_non_0_non_5.df$amnt))# 
(no_0_no_5_amnt_quantiles = quantile(d7_non_0_non_5.df$amnt))
(no_0_no_5_amnt_1st_quantile = quantile(d7_non_0_non_5.df$amnt)[2])
(no_0_no_5_amnt_3rd_quantile = quantile(d7_non_0_non_5.df$amnt)[4])
# impacts
inf_ur0_non_0_non_5
(penny_inf_impact_med_ur_non_0_non_5 = inf_ur0_non_0_non_5*no_0_no_5_amnt_med /100)
#End Table 2: Penny rounding table ####

#Begin Table 3: nickel rounding table ####
d8.df = d7.df
names(d8.df)

# adding column symmetric rounding (sr) amount (number of pennies) to the nearest 0
d8.df$sr5 = ifelse(d8.df$ending==0, 0, 
                   ifelse(d8.df$ending==1, -1, 
                          ifelse(d8.df$ending==2, -2, ifelse(d8.df$ending==3, -3, ifelse(d8.df$ending==4, -4, ifelse(d8.df$ending==5, 5, ifelse(d8.df$ending==6, 4, ifelse(d8.df$ending==7, 3, ifelse(d8.df$ending==8, 2, ifelse(d8.df$ending==9, 1, NA))))))))))
#
sample_n(d8.df,30)

# adding column upward rounding (ur) amount
d8.df$ur5 = ifelse(d8.df$ending==0, 0, 
                   ifelse(d8.df$ending==1, 9, 
                          ifelse(d8.df$ending==2, 8, 
                                 ifelse(d8.df$ending==3, 7, 
                                        ifelse(d8.df$ending==4, 6, 
                                               ifelse(d8.df$ending==5, 5, 
                                                      ifelse(d8.df$ending==6, 4, 
                                                             ifelse(d8.df$ending==7, 3, 
                                                                    ifelse(d8.df$ending==8, 2, 
                                                                           ifelse(d8.df$ending==9, 1, NA))))))))))
#
sample_n(d8.df,30)

# adding inflation under sr5 rounding (as fraction of pennies)
d8.df$sr5_inf = d8.df$sr5 / (100*d8.df$amnt)

# adding inflation under ur rounding (as fraction of pennies)
d8.df$ur5_inf = d8.df$ur5 / (100*d8.df$amnt)
#
sample_n(d8.df, 30)

# construct subset without zero-ending only!
dim(d8.df)
d8_non_0.df = subset(d8.df, round0 == F)
dim(d8_non_0.df)

# statistics about number of penny rounding (symmetric)
(avg_sr5_all = mean(d8.df$sr5))
(med_sr5_all = median(d8.df$sr5))
t.test(d8.df$sr5)
(p_sr5_all = t.test(d8.df$sr5)$p.value)
#
# for restricted data with no 0 endings
(avg_sr5_non_0 = mean(d8_non_0.df$sr5))
(med_sr5_non_0 = median(d8_non_0.df$sr5))
t.test(d8_non_0.df$sr5)
(p_sr5_non_0 = t.test(d8_non_0.df$sr5)$p.value)

# stat about number of penny rounding (upward)
(avg_ur5_all = mean(d8.df$ur5))
(med_ur5_all = median(d8.df$ur5))
t.test(d8.df$ur5, alternative = "greater")
(p_ur5_all = t.test(d8.df$ur5, alternative = "greater")$p.value)

# for restricted data with no 0 endings
(avg_ur5_non_0 = mean(d8_non_0.df$ur5))
(med_ur5_non_0 = median(d8_non_0.df$ur5))
t.test(d8_non_0.df$ur5, alternative = "greater")
(p_ur5_non_0 = t.test(d8_non_0.df$ur5, alternative = "greater")$p.value)

# inflation results (stats): symmetric rounding (fractions of pennies)
(avg_sr5_inf_all = mean(d8.df$sr5_inf))
(med_sr5_inf_all = median(d8.df$sr5_inf))
t.test(d8.df$sr5_inf)
(p_sr5_inf_all = t.test(d8.df$sr5_inf)$p.value)
#
#for restricted data with no 0 endings (symmetric)
(avg_sr5_inf_non_0 = mean(d8_non_0.df$sr5_inf))
(med_sr5_inf_non_0 = median(d8_non_0.df$sr5_inf))
t.test(d8_non_0.df$sr5_inf)
(p_sr5_inf_non_0 = t.test(d8_non_0.df$sr5_inf)$p.value)

# inflation results (stats): upward rounding (fractions)
(avg_ur5_inf_all = mean(d8.df$ur5_inf))
(med_ur5_inf_all = median(d8.df$ur5_inf))
t.test(d8.df$ur5_inf, alternative = "greater")
(p_ur5_inf_all = t.test(d8.df$ur5_inf, alternative = "greater")$p.value)
#
# for restricted data with no 0 endings
(avg_ur5_inf_non_0 = mean(d8_non_0.df$ur5_inf))
(med_ur5_inf_non_0 = median(d8_non_0.df$ur5_inf))
t.test(d8_non_0.df$ur5_inf, alternative = "greater")
(p_ur5_inf_non_0 = t.test(d8_non_0.df$ur5_inf, alternative = "greater")$p.value)

# Constructing (redefining) inflation measures by ratios of total sums (instead of mean of ratios which is used for the t-test) (in %)
(inf_sr5_all = 100*sum(d8.df$sr5)/(100*sum(d8.df$amnt)))
#
(inf_ur5_all = 100*sum(d8.df$ur5)/(100*sum(d8.df$amnt)))
#
(inf_sr5_non_0 = 100*sum(d8_non_0.df$sr5)/(100*sum(d8_non_0.df$amnt)))
#
(inf_ur5_non_0 = 100*sum(d8_non_0.df$ur5)/(100*sum(d8_non_0.df$amnt)))

# Adding median and average payment amount ($)
(avg_amount_all = mean(d8.df$amnt)) 
(med_amount_all = median(d8.df$amnt)) 
(avg_amount_non_0 = mean(d8_non_0.df$amnt)) 
(med_amount_non_0 = median(d8_non_0.df$amnt)) 

# Putting the above into a data frame

# row labels
(var5.vec = c("Average rounding (number of pennies)", "Median rounding (number of pennies)", "p-value (rounding)", "Average payment amount ($)", "Median payment amount ($)", "Inflationary impact (%)", "p-value (inflation)"))

# All data column sr5
(sr5_all_data.vec = c(avg_sr5_all, med_sr5_all, p_sr5_all, avg_amount_all, med_amount_all, inf_sr5_all, p_sr5_inf_all))
#
# non_0 column sr5
(sr5_non_0_data.vec = c(avg_sr5_non_0, med_sr5_non_0, p_sr5_non_0, avg_amount_non_0, med_amount_non_0, inf_sr5_non_0, p_sr5_inf_non_0))
#
# All data column ur5
(ur5_all_data.vec = c(avg_ur5_all, med_ur5_all, p_ur5_all, avg_amount_all, med_amount_all, inf_ur5_all, p_ur5_inf_all))
#
# non_0_non_5 column ur5
(ur5_non_0_data.vec = c(avg_ur5_non_0, med_ur5_non_0, p_ur5_non_0, avg_amount_non_0, med_amount_non_0, inf_ur5_non_0, p_ur5_inf_non_0))

(nickel.df = data.frame("Statistical_variable"=var5.vec, "SR"=sr5_all_data.vec, "UR"=ur5_all_data.vec, "SR"=sr5_non_0_data.vec,  "UR"=ur5_non_0_data.vec))

# below, create matrix w\ 1 extra column to indicate number of digits for each row
#
print(xtable(nickel.df, digits = digitm), include.rownames = F, hline.after = c(0, 3, 5))

# Discussion in the Conclusion on the effect of nickel removal on the use of quarters.
# number of payments ending with 25 cents.
nrow(d8.df) # number of all cash payments
nrow(subset(d8.df, decimal==25))# number ending with 25
nrow(subset(d8.df, decimal==75))# number ending with 75
# both ending with 25 or 75
nrow(subset(d8.df, decimal==25 | decimal==75))
# frac of total
nrow(subset(d8.df, decimal==25 | decimal==75))/nrow(d8.df)
#End Table 3: nickel rounding table ####

#Begin Table 4: Nickel elimination by merchant type (all data) ####
names(d8.df)
dim(d8.df)
dim(d8_non_0.df)# no zero endings
str(d8_non_0.df)
#
# Start Nickel inflation all data by merchant type 
#
# inflationary impact under SR (%)
(nickel_inf_all_avg_sr.vec = as.vector(d8.df %>% group_by(merch) %>% summarise(100*sum(sr5)/(100*sum(amnt)))))
(temp1.vec = unlist(nickel_inf_all_avg_sr.vec, use.names = F))
# it doubled the size so cut the vector
(temp2.vec = temp1.vec[22:42])
(nickel_inf_all_avg_sr.vec = temp2.vec)

# p-values for the above
(nickel_inf_all_pv_sr.vec = as.vector(d8.df %>% group_by(merch) %>% summarise(t.test(sr5_inf, mu=0)$p.value)))
# this creates a list, so make it a vector
(x.vec = (unlist(nickel_inf_all_pv_sr.vec , use.names = F)))
# it doubled the size so cut the vector
str(x.vec)
(y.vec = x.vec[22:42])
(nickel_inf_all_pv_sr.vec = y.vec)
#
# Inflationary impact under UR (%)
#
(nickel_inf_all_avg_ur.vec = as.vector(d8.df %>% group_by(merch) %>% summarise(100*sum(ur5)/(100*sum(amnt)))))
# this creates a list, so make it a vector
(temp3.vec = (unlist(nickel_inf_all_avg_ur.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp3.vec)
(temp4.vec = temp3.vec[22:42])
(nickel_inf_all_avg_ur.vec = temp4.vec)
#
# p values for the above (note: 1-side t test)
(nickel_inf_all_pv_ur.vec = as.vector(d8.df %>% group_by(merch) %>% summarise(t.test(ur5_inf, alternative = "greater", mu=0)$p.value)) )
# this creates a list, so make it a vector
(z.vec = (unlist(nickel_inf_all_pv_ur.vec , use.names = F)))
# it doubled the size so cut the vector
str(z.vec)
(w.vec = z.vec[22:42])
(nickel_inf_all_pv_ur.vec = w.vec)

# constructing vectors with *** reflecting p-values using gtools
(nickel_inf_all_stars_sr.vec = stars.pval(nickel_inf_all_pv_sr.vec))
(nickel_inf_all_stars_ur.vec = stars.pval(nickel_inf_all_pv_ur.vec))
#
# Constructing a vector with median payment amount 
(med_amnt_all.vec = as.vector(d8.df %>% group_by(merch) %>% summarise(median(amnt))))
# this creates a list, so make it a vector
(temp5.vec = (unlist(med_amnt_all.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp5.vec)
(temp6.vec = temp5.vec[22:42])
(med_amnt_all.vec = temp6.vec)
str(med_amnt_all.vec)

# Constructing a vector with average payment amount 
(avg_amnt_all.vec = as.vector(d8.df %>% group_by(merch) %>% summarise(mean(amnt))))
# this creates a list, so make it a vector
(temp15.vec = (unlist(avg_amnt_all.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp15.vec)
(temp16.vec = temp15.vec[22:42])
(avg_amnt_all.vec = temp16.vec)
str(avg_amnt_all.vec)

# building a data frame (All data only)
(nickel_inf_all.df = data.frame("Merchant type" = merch_abv.vec, "Obs." = merch_n.vec, "Med $"= med_amnt_all.vec, "Avg $" =avg_amnt_all.vec, "Impact" = nickel_inf_all_avg_sr.vec, "Sig" = nickel_inf_all_stars_sr.vec,   "Impact" = nickel_inf_all_avg_ur.vec, "Sig" = nickel_inf_all_stars_ur.vec))
#
dim(nickel_inf_all.df)

(digits_nickel_inf = matrix(c(0,0,0,2,2,4,0,4,0), nrow = 21, ncol = 9, byrow = T))
#
print(xtable(nickel_inf_all.df, digits = digits_nickel_inf), include.rownames = F, hline.after = c(0))
#End Table 4: Nickel elimination by merchant (all data) ####

#Begin Table 5: Nickel elimination by merchant (non_0 data) ####

dim(d8_non_0.df)# no zero endings
str(d8_non_0.df)

# some merchants need to be removed below of small number of observations
d8_non_0.df %>% group_by(merch) %>% summarise(n())
# Keep only merchant types with 8 or more observations
d9_non_0.df = d8_non_0.df %>% group_by(merch) %>% filter(n() >= 8) 
d9_non_0.df %>% group_by(merch) %>% summarise(n())
dim(d9_non_0.df)
table(d9_non_0.df$merch)# number of merchant types
length(table(d9_non_0.df$merch))# number of merchant types

# Inflationary impact SR (%)
(nickel_inf_non_0_avg_sr.vec = as.vector(d9_non_0.df %>% group_by(merch) %>% summarise(100*sum(sr5)/(100*sum(amnt)))))
str(nickel_inf_non_0_avg_sr.vec)
# this creates a list, so make it a vector
(temp1.vec = unlist(nickel_inf_non_0_avg_sr.vec, use.names = F))
# it doubled the size so cut the vector
(temp2.vec = temp1.vec[8:14])
(nickel_inf_non_0_avg_sr.vec = temp2.vec)
#
# p-values for the above
(nickel_inf_non_0_pv_sr.vec = as.vector(d9_non_0.df %>% group_by(merch) %>% summarise(t.test(sr5_inf, mu=0)$p.value)))
# this creates a list, so make it a vector
(x.vec = (unlist(nickel_inf_non_0_pv_sr.vec , use.names = F)))
# it doubled the size so cut the vector
str(x.vec)
(y.vec = x.vec[8:14])
(nickel_inf_non_0_pv_sr.vec = y.vec)
#
# Inflationary impact UR (%)
(nickel_inf_non_0_avg_ur.vec = as.vector(d9_non_0.df %>% group_by(merch) %>% summarise(100*sum(ur5)/(100*sum(amnt)))))
# this creates a list, so make it a vector
(temp3.vec = (unlist(nickel_inf_non_0_avg_ur.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp3.vec)
(temp4.vec = temp3.vec[8:14])
(nickel_inf_non_0_avg_ur.vec = temp4.vec)
#
# p values for the above (note: 1-side t test)
(nickel_inf_non_0_pv_ur.vec = as.vector(d9_non_0.df %>% group_by(merch) %>% summarise(t.test(ur5_inf, alternative = "greater", mu=0)$p.value)) )
# this creates a list, so make it a vector
(z.vec = (unlist(nickel_inf_non_0_pv_ur.vec , use.names = F)))
# it doubled the size so cut the vector
str(z.vec)
(w.vec = z.vec[8:14])
(nickel_inf_non_0_pv_ur.vec = w.vec)

# constructing vectors with *** reflecting p-values using gtools
(nickel_inf_non_0_stars_sr.vec = stars.pval(nickel_inf_non_0_pv_sr.vec))
(nickel_inf_non_0_stars_ur.vec = stars.pval(nickel_inf_non_0_pv_ur.vec))
#
# Constructing a vector with median payment amount 
(med_amnt_non_0.vec = as.vector(d9_non_0.df %>% group_by(merch) %>% summarise(median(amnt))))
# this creates a list, so make it a vector
(temp5.vec = (unlist(med_amnt_non_0.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp5.vec)
(temp6.vec = temp5.vec[8:14])
(med_amnt_non_0.vec = temp6.vec)
str(med_amnt_non_0.vec)
#
# Constructing a vector with average payment amount 
(avg_amnt_non_0.vec = as.vector(d9_non_0.df %>% group_by(merch) %>% summarise(mean(amnt))))
# this creates a list, so make it a vector
(temp5.vec = (unlist(avg_amnt_non_0.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp5.vec)
(temp6.vec = temp5.vec[8:14])
(avg_amnt_non_0.vec = temp6.vec)
str(avg_amnt_non_0.vec)

# Need to shorten the list of merchant names
merch_abv.vec# A complete list of merchant types
table(d9_non_0.df$merch)# number of merchant types
#
(temp7.vec = unique(d9_non_0.df$merch))# merchant codes in the restricted data
(temp8.vec =sort(temp7.vec))
(merch_abv_non_0.vec = merch_abv.vec[temp8.vec])

# need to revise the number of observations (revise merch_n.vec)
merch_n.vec# this was for the All data (21 merchants)
(merch_n_non_0.vec = as.vector(d9_non_0.df %>% group_by(merch) %>% summarise(n())))
# this is a list. Converting to a vector
(temp9.vec = unlist(merch_n_non_0.vec, use.names = F))
(merch_n_non_0.vec = temp9.vec[8:14])

# building a data frame (Non_0 data only)
(nickel_inf_non_0.df = data.frame("Merchant type" = merch_abv_non_0.vec, "Obs." = merch_n_non_0.vec, "Med $" =med_amnt_non_0.vec, "Avg $" =avg_amnt_non_0.vec, "Impact" = nickel_inf_non_0_avg_sr.vec, "Sig" = nickel_inf_non_0_stars_sr.vec,   "Impact" = nickel_inf_non_0_avg_ur.vec, "Sig" = nickel_inf_non_0_stars_ur.vec))
#
dim(nickel_inf_non_0.df)
names(nickel_inf_non_0.df)
sum(nickel_inf_non_0.df$Obs.)# total number of payment observations

(digits_nickel_inf_non_0 = matrix(c(0,0,0,2,2,4,0,4,0), nrow = 7, ncol = 9, byrow = T))
#
print(xtable(nickel_inf_non_0.df, digits = digits_nickel_inf_non_0), include.rownames = F, hline.after = c(0))
#End Table 5: Nickel elimination by merchant (non_0 data) ####

#Begin: Table 6: Rounding down cost to merchants ####
# restricting to merchants 1 to 6 (delete low number of payments)
d10.df = subset(d8.df, merch < 7 | merch==21)
table(d10.df$merch)
sum(table(d10.df$merch))
nrow(d10.df)

# adding column upward rounding (dr) amount (down rounding, penny and nickel, all payments)
d10.df$dr5 = ifelse(d10.df$ending==0, 0, 
                   ifelse(d10.df$ending==1, -1, 
                          ifelse(d10.df$ending==2, -2, 
                                 ifelse(d10.df$ending==3, -3, 
                                        ifelse(d10.df$ending==4, -4, 
                                               ifelse(d10.df$ending==5, -5, 
                                                      ifelse(d10.df$ending==6, -6, 
                                                             ifelse(d10.df$ending==7, -7, 
                                                                    ifelse(d10.df$ending==8, -8, 
                                                                           ifelse(d10.df$ending==9, -9, NA))))))))))
#
sample_n(d10.df,30)

# adding deflation under dr5 rounding (in % b/c 100 cancels out)
d10.df$dr5_inf = d10.df$dr5 / (d10.df$amnt)
#
sample_n(d10.df, 30)

# subset to d10_non_0 data frame
dim(d10.df)
d10_non_0.df = subset(d10.df, ending != 0)
dim(d10_non_0.df)
names(d10_non_0.df)

# Inflationary impact of dr by merchant all (%)
(nickel_inf_avg_dr.vec = as.vector(d10.df %>% group_by(merch) %>% summarise(100*sum(dr5)/(100*sum(amnt)))))
str(nickel_inf_avg_dr.vec)
# this creates a list, so make it a vector
(temp25.vec = unlist(nickel_inf_avg_dr.vec, use.names = F))
# it doubled the size so cut the vector
(temp26.vec = temp25.vec[8:14])
(nickel_inf_avg_dr.vec = temp26.vec)
#
# p-values for the above
(nickel_inf_pv_dr.vec = as.vector(d10.df %>% group_by(merch) %>% summarise(t.test(dr5_inf, mu=0)$p.value)))
# this creates a list, so make it a vector
(temp27.vec = (unlist(nickel_inf_pv_dr.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp27.vec)
(temp28.vec = temp27.vec[8:14])
(nickel_inf_pv_dr.vec = temp28.vec)


# Inflationary impact of dr by merchant non_0 (%)
(nickel_inf_non_0_avg_dr.vec = as.vector(d10_non_0.df %>% group_by(merch) %>% summarise(100*sum(dr5)/(100*sum(amnt)))))
str(nickel_inf_non_0_avg_dr.vec)
# this creates a list, so make it a vector
(temp21.vec = unlist(nickel_inf_non_0_avg_dr.vec, use.names = F))
# it doubled the size so cut the vector
(temp22.vec = temp21.vec[8:14])
(nickel_inf_non_0_avg_dr.vec = temp22.vec)
#
# p-values for the above
(nickel_inf_non_0_pv_dr.vec = as.vector(d10_non_0.df %>% group_by(merch) %>% summarise(t.test(dr5_inf, mu=0)$p.value)))
# this creates a list, so make it a vector
(temp23.vec = (unlist(nickel_inf_non_0_pv_dr.vec , use.names = F)))
# it doubled the size so cut the vector
str(temp23.vec)
(temp24.vec = temp23.vec[8:14])
(nickel_inf_non_0_pv_dr.vec = temp24.vec)
#

# constructing vectors with *** reflecting p-values using gtools
(nickel_inf_stars_dr.vec = stars.pval(nickel_inf_pv_dr.vec))
(nickel_inf_non_0_stars_dr.vec = stars.pval(nickel_inf_non_0_pv_dr.vec))
#

# Need to shorten the list of merchant names
merch_abv.vec# A complete list of merchant types
table(d10.df$merch)# number of merchant types
table(d10_non_0.df$merch)# number of merchant types

# Adding Visa and MasterCard interchange fees from the KC Fed (2024 update)
mastercard.vec = c(1.30, 1.90, 1.54, 1.54, 1.54, 1.54, 1.54)
#
visa.vec = c(1.34, 1.15, 1.47, 1.47, 1.47, 1.47, 1.47)


# building a data frame (both all data and Non_0 data)
(nickel_dr.df = data.frame("Merchant type" = merch_abv_non_0.vec, "Impact_all" = nickel_inf_avg_dr.vec, "Sig_all" = nickel_inf_stars_dr.vec,  "Impact_non_0" = nickel_inf_non_0_avg_dr.vec, "Sig_non_0" = nickel_inf_non_0_stars_dr.vec, "Visa" = visa.vec, "MasterCard" = mastercard.vec))
#
dim(nickel_dr.df)
names(nickel_dr.df)


(digits_nickel_dr = matrix(c(0,0,4,0,4,0,2,2), nrow = 7, ncol = 8, byrow = T))
#
print(xtable(nickel_dr.df, digits = digits_nickel_dr), include.rownames = F, hline.after = c(0))
#End Table 5: Nickel elimination by merchant (non_0 data) ####



#end: Table 6: Rounding down cost to merchants ####


#Begin: unused code ####
# # Inflation regressions (not in paper due to no results)
# on merchant type and amount: penny elimination
# inf_model_penny1 = ur0_inf ~ amnt + relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
# #
# (inf_reg_penny1 = lm(inf_model_penny1, data = d8.df))
# summary(inf_reg_penny1)
# 
# # Inflation on merchant type and amount: penny and nickel elimination
# inf_model_penny_nickel1 = ur5_inf ~ amnt + relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
# #
# (inf_reg_penny_nickel1 = lm(inf_model_penny_nickel1, data = d8.df))
# summary(inf_reg_penny_nickel1)
# 
# # try without amnt
# 
# # Inflation on merchant type only: penny elimination
# inf_model_penny2 = ur0_inf ~ relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
# #
# (inf_reg_penny2 = lm(inf_model_penny2, data = d8.df))
# summary(inf_reg_penny2)
# 
# # Inflation on merchant type only: penny and nickel elimination
# inf_model_penny_nickel2 = ur5_inf ~ relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
# #
# (inf_reg_penny_nickel2 = lm(inf_model_penny_nickel2, data = d8.df))
# summary(inf_reg_penny_nickel2)
######### End inflation regressions (not in paper due to no results)

######### Begin: Rounding regressions merchant types (not in the paper, perhaps not interesting to regress number of rounded pennies on merchant type)
# names(d8.df)
# # Note: upward rounding only!
# sample_n(d8.df, 30)
# 
# ## penny rounding as function of amnt and merch (penny elimination)
# round_model_penny1 = ur0 ~ amnt + relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
# #
# (round_reg_penny1 = lm(round_model_penny1, data = d8.df))
# summary(round_reg_penny1)
# 
# # penny rounding on merchant type and amount: penny and nickel elimination
# round_model_penny_nickel1 = ur5 ~ amnt + relevel(as.factor(merch), ref = "1")# grocery stores  is the reference merchant type
# #
# (round_reg_penny_nickel1 = lm(round_model_penny_nickel1, data = d8.df))
# summary(round_reg_penny_nickel1)
# #
# merch_abv.vec# recall
# (var_list.vec = c("Intercept", merch_abv.vec))# OLS has another variable "Intercept"
# var_list.vec[2] = "Dollar amount"# replace merchant 1 (reference variable) with Amount variable
# var_list.vec
# str(var_list.vec)
# # convert to LaTeX
# (texreg.latex = texreg(list(round_reg_penny1, round_reg_penny_nickel1), digits = 3, custom.model.names = c("Penny elimination", "Penny and nickel elimination"), custom.coef.names = var_list.vec, single.row = T))
# ###### End: Regressions Rounding on merchant type (not in the paper, perhaps not interesting to regress number of rounded pennies on merchant type)
