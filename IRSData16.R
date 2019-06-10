library(readr)
library(dplyr)
library(ggplot2)


# Data from IRS SOI page; downloaded in CSV

irs2016 <- read_csv("C:/Users/evd19/Downloads/16zpallagi.csv")


# Eliminate summarized data (will return to it later)

sample_for_test <- filter(irs2016, !zipcode=='00000') %>% select(zipcode, N1, agi_stub, A00100, A00200, A00300, A00600, N02300,N19300,N01000,A01000,N18500,A18500) %>%
  mutate(avg_agi= A00100 / N1, avg_int=A00300/ N1,
         avg_wages=A00200 / N1, avg_divs=A00600/N1,
         pct_w_capgain=N01000/N1,avg_capgain=A01000/N1,
         pct_w_unemp=N02300/N1, pct_w_mipaid=N19300/N1,
         pct_w_retaxes=N18500/N1,avg_retaxes=A18500/N1,
         zipcode=as.character(zipcode))




irs_example0 <- sample_for_test %>% group_by(zipcode) %>% mutate(n=sum(N1),
                                                                 mean_agi=weighted.mean(avg_agi,N1),
                                                                 mean_int=weighted.mean(avg_int,N1),
                                                                 mean_wages=weighted.mean(avg_wages,N1),
                                                                 mean_divs=weighted.mean(avg_divs,N1),
                                                                 pct_unemp=weighted.mean(pct_w_unemp,N1),
                                                                 pct_mipaid=weighted.mean(pct_w_mipaid,N1),
                                                                 pct_capgain=weighted.mean(pct_w_capgain,N1),
                                                                 mean_capgain=weighted.mean(avg_capgain,N1),
                                                                 pct_retaxes=weighted.mean(pct_w_retaxes,N1),
                                                                 avg_retaxes=weighted.mean(avg_retaxes,N1)
                                                                 )

irs_example <- irs_example0 %>% group_by(zipcode) %>% select(zipcode, n, mean_agi, mean_int, mean_wages, mean_divs, pct_unemp, 
                                                             pct_mipaid, pct_capgain, mean_capgain, pct_retaxes, avg_retaxes) %>% slice(1)

# Deal with zips with leading zeroes in a really hacky way...it works.

irs_example$zip5 <- ifelse(substr(irs_example$zipcode,5,5)=='',paste("0",irs_example$zipcode, sep = ""),irs_example$zipcode)

irs_example$zipcode <- irs_example$zip5

irs_example <- select(irs_example, -zip5)



write.csv(irs_example, 'C:/users/eric vondohlen/dropbox/NCA/agidata.csv', row.names = F)