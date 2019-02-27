# File: EB84.3
#---- imports, load and prepare data (background: http://www.princeton.edu/~otorres/DID101R.pdf) ----
library(foreign)
library(haven)
library(stargazer)
library(ggplot2)
library(ks)
library(magrittr)
library(dplyr)
library(tidyverse)
library(tibble)
setwd("xxx")

# set "Century Schoolbook" as default font
windowsFonts("CentSch" = windowsFont("Century Schoolbook")) # assign font to Family
op <- par(family = "CentSch") # saving to default

EB843 <- read.dta("84.3_ZA6643_v3-1-0.dta", convert.factors = FALSE) #  eurobarometer wave 84.3, gesis reference data set: ZA6643
EB843$date <- as.Date(EB843$p1, origin = "2015-11-06")
EB843 <- subset(EB843, (!is.na(EB843$qa3a_6 & EB843$qa4a_6))) #subset on DV != na
EB843 <- subset(EB843, (!is.na(EB843$date))) #subset on date != na
EB843$group = ifelse((EB843$isocntry == "FR" & EB843$p7fr_r == 1), 1, ifelse((EB843$isocntry == "FR" & EB843$p7fr_r != 1), 2, 3)) # group: 1 = Ile-de-F, 2 = France, 3 = Rest
EB843 <- EB843 %>% mutate(regions = group_indices(., isocntry, p7be, p7at, p7bg, p7cy, p7cz, p7dk, p7ee, p7de, p7gr, p7es, p7fi, p7fr, p7gb, p7hu, p7ie, p7it, p7lt, p7lu, p7lv, p7nl, p7pl, p7pt, p7ro, p7se, p7si_r, p7sk, p7hr_r, p7tr, p7cytcc, p7mk, p7me, p7rs)-1)
# add one column with all regions

#View(EB843$date)
#---- information ----
# Date        Date# City      Region#                        +   *
# 2015-11-13  8    Paris      p7fr_r:1 | isocntry: FR      137 413   (attack happened in the evening)

#---- DiD ----
# dummy time
EB843$time = ifelse(EB843$p1 >=8, 1, 0) # treatment on evening of 13.11. ==> 14.
# dummies treatment
EB843$treated1 = ifelse((EB843$isocntry == "FR" & EB843$p7fr_r == 1), 1, 0) #Ile de France
EB843$treated2 = ifelse(EB843$isocntry == "FR", 1, 0) #France
# did interactions
EB843$did1 = EB843$time * EB843$treated1 # interaction term *did1*
EB843$did2 = EB843$time * EB843$treated2 # interaction term *did2*

### DiD MODELS ###
#DVs: qa3a_6 qa4a_6
didreg1a    = lm(qa3a_6 ~ treated1 + time + did1, data = EB843[(EB843$date > "2015-11-12" & EB843$date < "2015-11-15"),]) #subset 10-17
didreg1a_pt = lm(qa3a_6 ~ date * regions + treated1 + time + did1, data = EB843)
#didreg1b = lm(qa4a_6 ~ treated1 + time + did1, data = EB843[(EB843$date > "2015-11-12" & EB843$date < "2015-11-15"),]) #subset 10-17
didreg2a    = lm(qa3a_6 ~ treated2 + time + did2, data = EB843[(EB843$date > "2015-11-12" & EB843$date < "2015-11-15"),]) #subset 10-17
didreg2a_pt = lm(qa3a_6 ~ date * regions + treated2 + time + did2, data = EB843)
#didreg2b = lm(qa4a_6 ~ treated2 + time + did2, data = EB843[(EB843$date > "2015-11-12" & EB843$date < "2015-11-15"),]) #subset 10-17
summary(didreg1a_pt) # ***  important issue COUNTRY   /Ile d F     significant
#summary(didreg1b) # --- important issue PERSONALLY/Ile d F not significant
summary(didreg2a_pt) # ***  important issue COUNTRY   / FRANCE     significant
#summary(didreg2b) # --- important issue PERSONALLY/ FRANCE not significant
# ==> personally not significant but COUNTRY!

### OUTPUT ###
stargazer(didreg1a_pt,didreg2a_pt, type="html",
          dep.var.labels = "Afraid of terrorism",
          dep.var.caption = "Important issues: terrorism mentioned",
          title = "Difference-in-differences (Paris attacks, 2015)",
          dep.var.labels.include = FALSE, intercept.bottom = TRUE, model.names = TRUE, model.numbers = TRUE,
          no.space = TRUE, star.char = "*",
          order = c("date", "date * regions", "time", "regions","treated1", "treated2", "did1", "did2"),
          covariate.labels = c("Date", "Parallel trends control", "After attack", "Regions", "Île-de-France", "France",
                               "Île-de-France after attack", "France after attack"),
          notes.align = "l", notes = c('The attack occurred in Paris on the evening of 13.11. Two groups are analysed as “treated”: (i) the region around Paris (Île-de-France), and (ii) France, each compared to the remaining sample (excluding France). "Date" describes a linear control for time; "Parallel trends control" is an interaction of regions and time to account for temporal and spatial variations other than through the attack; "After attack" is a dummy for those interviewed after the attack (participants interviewed on 14.11. and after); "Regions" is a control for the entire sample on the regional level; "Île-de-France" and "France" are dummies for the affected region/ country;  "Île-de-France after attack" and "France after attack" respectively describe the increased proportion of those in Île-de-France or France interviewed after the attack that mentioned "terrorism" as one of the two most important issues for the country in the future.'),
          out = "#EB843_new.html")
#--> all significant

#---- BACKGROUND PLOTS ----
#### DENSITY PLOTS ####
EB843$date_f <- as.factor(EB843$date[!is.na(EB843$date)]) # date as factor
EB843$date_i <- as.integer(EB843$date_f)
EB843$date_i <- EB843$date_i[!is.na(EB843$date_i)]
# number ==> date....

# date by region
date_i  <- EB843$date_i[EB843$isocntry != "FR"] # date group 1: rest
date_i2 <- EB843$date_i[EB843$p7fr_r == 1]  # date group 2: Ile de France
date_i3 <- EB843$date_i[EB843$isocntry == "FR"] # date group 3: FRA

date_i  <- date_i[!is.na(date_i)]
date_i2 <- date_i2[!is.na(date_i2)]
date_i3 <- date_i3[!is.na(date_i3)]
#View(date_i)

#plot
plot(density(date_i3), family = "CentSch", xaxt='n',xlab = 'Date', lwd = 2.5, ylab = 'Density', main = 'Density and attack dates', las=1, col = "black", lty = 1) # 
lines(density(date_i2), col = 'blue', lwd = 2.5, lty = 1) # 
lines(density(date_i), col = 'dark grey', lwd = 2.5) #
abline(v= 8, col='black', lwd = 1.5, lty = 1) #
tx=seq(min(date_i), max(date_i), by = 2)
lb=levels(EB843$date_f)[tx]
axis(side = 1,at=tx,labels=lb, las=0.2)
# legend
legend(-1, 0.2,
       legend = c("Density Ile d Fr", "Density FRA", "Density remaining sample"),
       title="",
       col = c("blue", 'black', "grey"),
       lty = c(1, 1, 1),
       lwd = 2,
       seg.len = 0.4,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.3)
legend(10, 0.2,
       legend = "Attacks Paris",
       title="",
       col = "black",
       lty = 1,
       lwd = 2,
       seg.len = 0.4,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.3)

#### PARALLEL TRENDS ASSUMPTION PLOTS ####
#--- OLD ----
# qa3a_6 | treated1 [Ile d F]
EB843 %>% mutate(treated1 = factor(treated1)) %>%
  mutate(date = as.POSIXct(date)) %>% #convert date to date
  group_by(treated1, date) %>% #group
  summarise(prop = sum(qa3a_6=="1")/n()) %>% #calculate proportion 
  ggplot()+ theme_classic() + 
  geom_line(aes(x = date, y = prop, color = treated1)) +
  geom_point(aes(x = date, y = prop, color = treated1)) +
  geom_vline(xintercept = as.POSIXct("2015-11-13 12:00:00 UTC"), color = 'black', lwd = 1)
# qa3a_6 | treated2 [France]
EB843 %>% mutate(treated2 = factor(treated2)) %>%
  mutate(date = as.POSIXct(date)) %>% #convert date to date
  group_by(treated2, date) %>% #group
  summarise(prop = sum(qa3a_6=="1")/n()) %>% #calculate proportion 
  ggplot()+   theme_classic() + 
  geom_line(aes(x = date, y = prop, color = treated2)) +
  geom_point(aes(x = date, y = prop, color = treated2)) +
  geom_vline(xintercept = as.POSIXct("2015-11-13"), color = 'black', lwd = 1)
#--- NEW ----

# region and country smooth
tiff('843.par.sm.tiff', units="px", width=3000, height=2500, res=500)
EB843 %>% mutate(group = factor(group), treated1 = factor(treated1), date = as.POSIXct(date)) %>% #convert date to date
  group_by(group, date) %>% #group
  summarise(prop = sum(qa3a_6=="1")/n()) %>% #calculate proportion 
  ggplot()+  xlab("Date") + ylab('Proportion terrorism mentioned') + theme_classic() + theme(text = element_text(family = "CentSch")) + 
  scale_color_manual(values = c('1' = 'black', '2' = 'darkgrey', "3" = "lightgrey"),
                     labels = c('1' = 'Île-de-France', '2' = 'France', '3' = "Remaining sample"), name = "Legend"
                     ) +
  geom_smooth(aes(x = date, y = prop, color = group), se = F, method = 'loess') +
  geom_point(aes(x = date, y = prop, color = group)) +
  geom_vline(xintercept = as.POSIXct("2015-11-13 12:00:00 UTC"), color = 'black', lwd = 0.8, lty = 4) +
  geom_vline(xintercept = as.POSIXct("2015-11-09 15:00:00 UTC"), color = 'grey', lwd = 0.2, lty = 4) # foiled attack on the 10th
dev.off()

# difference in proportions
EB843_mut <- EB843 %>%
  mutate(group = factor(group), date = as.POSIXct(date)) %>% 
  group_by(group, date) %>%
  summarise(
    prop = sum(qa3a_6 == "1") / n()) %>%
  spread(group, prop) %>%
  mutate(propdiff1 = `1` - `3`, propdiff2 = `2` - `3`)

tiff('843.par.diff.tiff', units="px", width=3000, height=2500, res=500)
EB843_mut %>% 
  ggplot(family = "CentSch", aes(date, propdiff1)) + theme_classic() + xlab("Date") + ylab('Difference in proportions of "Terrorism mentioned"') + 
  theme(text = element_text(family = "CentSch")) +
  scale_color_manual(
    values = c("a" = "black", "b" = "grey"), labels = c("a" = "Difference 1", "b" = "Difference 2"), name = "Legend") +
  geom_line(data = EB873_mut[!is.na(EB873_mut$propdiff1), ], aes(y = propdiff1, x = date, color = "a")) + 
  geom_point(aes(x = date, y = propdiff1)) + 
  
  geom_line(data = EB873_mut[!is.na(EB873_mut$propdiff2), ], aes(y = propdiff2, x = date, color = "b")) + 
  geom_point(aes(x = date, y = propdiff2, color = "b")) + 
  
  geom_vline(xintercept = as.POSIXct("2017-05-22 12:00:00 UTC"), color = 'black', lwd = 0.5, lty = 4)
dev.off()