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
#library("xtable") #exporting to LaTeX

setwd("~/Papers/penny")
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

#Start: stats on the dollar amount & histogram ####
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
#End: stats and histogram on payment amount ####

#Begin: bar plot of fraction ending with 0 as function of dollar amount (right-closed intervals) ####
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

# discussion of Finding 1 in paper
(frac0_all = nrow(subset(d5.df, ending ==0))/nrow(subset(d5.df)))
(frac0_all_n = nrow(subset(d5.df, ending ==0)))
(n_all = nrow(subset(d5.df)))
#
(frac5_all = nrow(subset(d5.df, ending ==5))/nrow(subset(d5.df)))
(frac5_all_n = nrow(subset(d5.df, ending ==5)))
(n_all = nrow(subset(d5.df)))
#End: bar plot of fraction ending with 0 and 5 cents as function of dollar amount ####

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

#Begin: Logit regression 0 cents ####
names(d5.df)
# new columns binary 1=ending==0, 0=otherwise
d6.df = d5.df
d6.df$round0 =ifelse(d6.df$ending==0, TRUE, FALSE)
head(d6.df, 50)
table(d6.df$ending)
table(d6.df$round0)

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
#End: Logit regression 0 cents ####

#Begin: bar plot of fraction of cash payments ending with 0 and 5 by merchant type
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
#End: bar plot of fraction of cash payments ending with 0 and 5 by merchant type ####