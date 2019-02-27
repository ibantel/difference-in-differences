# File: EB562
#---- imports, load and prepare data ----
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

EB562 <- read.dta("56.2_ZA3627_v1-0-1.dta", convert.factors = FALSE) #  eurobarometer wave 56.2, gesis reference data set: ZA3627
EB562$date <- as.Date(EB562$v418, origin = "2001-10-12")
EB562 <- subset(EB562, (!is.na(EB562$v200))) #subset on DV != na
EB562 <- subset(EB562, (!is.na(EB562$date))) #subset on date != na

#---- information ----
# Date        Date# Time  City      Region# +  *    Source
# 2001-11-06  25    09:08 Madrid    12      0 95    https://www.theguardian.com/world/2001/nov/06/spain

#---- DiD ----
# dummies time
EB562$time1 = ifelse(EB562$v418 >= 25, 1, 0) # ==> this gives a significant result. BUT: see end (cross tabulation STATA)
  #(EB562$date >= "2001-11-06" | (EB562$date == "2001-11-06" & EB562$v419 == 1)), 1, 0) # treatment on 6.11., 09:08

# dummies treatment
EB562$treated1a = ifelse((EB562$isocntry == "ES" & EB562$v450 == 12), 1, 0) #Mad
# since there is no data on Madrid and Cataluna before and after the attack: go with Spain
EB562$treated1b = ifelse(EB562$isocntry == "ES", 1, 0)
# did interaction
EB562$did1a = EB562$time1 * EB562$treated1a # interaction term *did1*
EB562$did1b = EB562$time1 * EB562$treated1b # interaction term

### DiD MODELS ###
didreg1a = lm(v200 ~ treated1a + time1 + did1a, data = EB562[(EB562$date > "2001-11-03" & EB562$date < "2001-11-13"),]) # subset 1-15
didreg1b = lm(v200 ~ treated1b + time1 + did1b, data = EB562[(EB562$date > "2001-11-04" & EB562$date < "2001-11-14"),]) # subset 2-16
summary(didreg1a) # **       | MADRID ATTACK, MADRID 
#summary(didreg1b) # -        | MADRID ATTACK, SPAIN

### OUTPUT ###
stargazer(didreg1a #, didreg1b, didreg2
          , type="html",
          title = "Difference in differences Analysis 1",
          dep.var.caption = "Afraid of terrorism (0/1)",
          dep.var.labels.include = FALSE,
          intercept.bottom = TRUE,
          model.names = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          star.char = "*",
          covariate.labels = c('Madrid (Treated)',   '06.11. (Time)', "Attack interaction"),
                               out = "#EB562_4day.html")
#--> significant for MAD and Getxo, not for ESP
#---- BACKGROUND PLOTS ----
#### DENSITY PLOTS ####
EB562$date_f <- as.factor(EB562$date[!is.na(EB562$date)]) # date as factor
EB562$date_i <- as.integer(EB562$date_f)
EB562$date_i <- EB562$date_i[!is.na(EB562$date_i)]
# 25 = 2001-11-06 | 26 = 2001-11-07

# date by region
date_i  <- EB562$date_i[(EB562$isocntry != "ES") | EB562$isocntry == "ES" &  EB562$v450 != 12 & EB562$v450 != 17] # date group 1: rest
date_i2 <- EB562$date_i[EB562$v450 == 12]  # date group 2: == Madrid
date_i3 <- EB562$date_i[EB562$v450 == 17] # date group 3: == Pais Vasco
date_i4 <- EB562$date_i[EB562$isocntry == "ES"] # date group 4: == SPAIN

date_i <- date_i[!is.na(date_i)]
date_i2 <- date_i2[!is.na(date_i2)]
date_i3 <- date_i3[!is.na(date_i3)]
date_i4 <- date_i4[!is.na(date_i4)]
#View(date_i)

# plot
plot(density(date_i2), family = "CentSch", xaxt='n',xlab = 'Date', lwd = 2.5, ylab = 'Density', main = 'Density and attack dates', las=1, col = "blue", lty = 1) # MAD
lines(density(date_i3), col = 'green', lwd = 2.5, lty = 1) # Pais Vas
lines(density(date_i4), col = 'black', lwd = 2.5, lty = 1) # SPAIN
lines(density(date_i), col = 'dark grey', lwd = 2.5) # rest
abline(v= 25, col='blue', lwd = 2, lty = 1) # MAD 06.11.
abline(v= 26, col='green', lwd = 2, lty = 1) # PaVa 07.11.
tx=seq(min(date_i), max(date_i), by = 2)
lb=levels(EB562$date_f)[tx]
axis(side = 1,at=tx,labels=lb, las=0.2)
# legend
legend(6.5, 0.305,
       legend = c("Density Madrid (ES)", "Density Pais Vasco (ES)", "Density Spain", "Density remaining sample"),
       title="",
       col = c("blue", 'green', 'black', "grey"),
       lty = 1,
       lwd = 2,
       seg.len = 0.4,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.3)
legend(1.5, 0.2,
       legend = c("Attack in Madrid (0|95):\n06.11.***", "Attack in Pais Vasco (1|0):\n 07.11.***"),
       title="",
       col = c("blue", "green"),
       lty = 1,
       lwd = 2,
       seg.len = 0.3,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.1)
#### PARALLEL TRENDS ASSUMPTION PLOTS ####
# v200 | treated1a
EB562 %>% mutate(treated1a = factor(treated1a)) %>%
  mutate(date = as.POSIXct(date)) %>% #convert date to date
  group_by(treated1a, date) %>% #group
  summarise(prop = sum(v200=="1")/n()) %>% #calculate proportion 
  ggplot()+ theme_classic() + 
  scale_color_manual(values = c("0" = "grey", "1" = "black")) +
  geom_line(aes(x = date, y = prop, color = treated1a)) +
  geom_point(aes(x = date, y = prop, color = treated1a)) +
  geom_vline(
    xintercept = as.POSIXct("2001-11-06"), color = 'black', lwd = 1)
# v200 | treated2 and v200 treated 1b not even significant
# ==> parallel trends assumption doesn't hold!

#--- NEW smooth -----
tiff('562.par.sm.tiff', units="px", width=3000, height=2500, res=500)
EB562 %>% mutate(treated1a = factor(treated1a, levels = c('1', '0')), date = as.POSIXct(date)) %>% #convert date to date
  group_by(treated1a, date) %>% #group
  summarise(prop = sum(v200=="1")/n()) %>% #calculate proportion 
  ggplot()+ theme_classic() + xlab("Date") + ylab('Proportion terrorism mentioned') + theme(text = element_text(family = "CentSch")) + 
  scale_color_manual(values = c('1' = 'black', '0' = 'darkgrey'), 
                     labels = c('1' = 'Madrid', '0' = 'Remaining sample'), name = "Legend") +
  geom_smooth(aes(x = date, y = prop, color = treated1a), se = F, method = 'loess') +
  geom_point(aes(x = date, y = prop, color = treated1a)) +
  geom_vline(xintercept = as.POSIXct("2001-11-06"), color = 'black', lwd = 0.5, lty = 4)
dev.off()


# plot DIFFERENCE between proportions
EB562_mut <- EB562 %>%
  mutate(treated1a = factor(treated1a, levels = c("0", "1")), date = as.POSIXct(date)) %>% 
  group_by(treated1a, date) %>%
  summarise(
    prop = sum(v200 == "1") / n()) %>%
  spread(treated1a, prop) %>%
  mutate(propdiff = `1` - `0`)

tiff('562.par.diff.tiff', units="px", width=3000, height=2500, res=500)
EB562_mut %>% 
  ggplot(family = "CentSch", aes(date, propdiff)) + theme_classic() + xlab("Date") + ylab('Difference') + 
  theme(text = element_text(family = "CentSch")) +
  scale_color_manual(values = c("a" = "black", propdiff = 'black'), labels = 'Difference in pro- \nportions of "Terror- \nism mentioned"', name = "Legend") +
  geom_line(data = EB562_mut[!is.na(EB562_mut$propdiff), ], aes(y = propdiff, x = date, color = "a")) + 
  #geom_smooth(data = EB610_mut[!is.na(EB610_mut$propdiff), ], aes(x = date, y = propdiff, color = "propdiff"), se = F, method = 'loess') +
  geom_point(aes(x = date, y = propdiff)) + 
  geom_vline(xintercept = as.POSIXct("2001-11-06 12:00:00 UTC"), color = 'black', lwd = 0.5, lty = 4)
dev.off()