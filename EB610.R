# File: EB610
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

EB610 <- read.dta("1_61.0__ZA4056_v1-0-1.dta", convert.factors = FALSE) #  eurobarometer wave 61.0, gesis reference data set: ZA4056
EB610$date <- as.Date(EB610$v359, origin = "2004-02-19")
EB610 <- subset(EB610, (!is.na(EB610$v169))) #subset on DV != na
EB610 <- subset(EB610, (!is.na(EB610$date))) #subset on date != na

#---- information ----
# Date        Date# City      Region#  +   *
# 2004-03-11  21    Madrid    XX      191 450
#---- DiD preparation ----
# dummies time
EB610$time = ifelse(EB610$v359 >=21, 1, 0) # treatment on 11.03.
# dummies treatment
EB610$treated1 = ifelse((EB610$isocntry == "ES" & EB610$v392 == 12), 1, 0) # Madrid
EB610$treated2 = ifelse((EB610$isocntry == "ES"), 1, 0) # ES

#did interactions
EB610$did1 = EB610$time * EB610$treated1 # interaction term *did1*
EB610$did2 = EB610$time * EB610$treated2 # interaction term *did2*
#---- DiD ----
### DiD MODELS ###
didreg1 = lm(v169 ~ treated1 + time + did1, data = EB610[(EB610$date > "2004-03-08" & EB610$date < "2004-03-13"),]) # subset 06-15
didreg2 = lm(v169 ~ treated2 + time + did2, data = EB610[(EB610$date > "2004-03-08" & EB610$date < "2004-03-13"),]) # subset 06-15
summary(didreg1)  #      | MADR
summary(didreg2)  # 5%   | ES

### OUTPUT ###
stargazer(didreg1, type="html",
          dep.var.caption = "Important issues: terrorism mentioned (0/1)",
          title = "Difference in differences (Analysis 2)",
          dep.var.labels.include = FALSE,
          intercept.bottom = TRUE,
          model.names = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          star.char = "*",
          covariate.labels = c('Madrid (Treated)',   '11.03. (Time)', "Attack interaction"),
          out = "#EB610_4days.html")

#---- BACKGROUND PLOTS ----
#### DENSITY PLOTS ####
EB610$date_f <- as.factor(EB610$date[!is.na(EB610$date)]) # date as factor
EB610$date_i <- as.integer(EB610$date_f)
EB610$date_i <- EB610$date_i[!is.na(EB610$date_i)]
# number ==> date....

# date by region
date_i  <- EB610$date_i[((EB610$isocntry != "DE-E") # not GER-EAS
                          & (EB610$isocntry != "ES")) # not ESP
                        | (EB610$isocntry == "DE-E" & EB610$v400 != 13) # GER but not Mech Pomm
                        | (EB610$isocntry == "ES" & EB610$v392 != 12)] # ES but not MAD
#date_i2 <- EB610$date_i[EB610$v400 == 13]  # date group 2: MECK-VORP
date_i3 <- EB610$date_i[EB610$v392 == 12] # date group 3: MADR
date_i4 <- EB610$date_i[EB610$isocntry == "ES"] # date group 4: ESP

date_i  <- date_i[!is.na(date_i)]
#date_i2 <- date_i2[!is.na(date_i2)]
date_i3 <- date_i3[!is.na(date_i3)]
date_i4 <- date_i4[!is.na(date_i4)]

#--- plots ----
plot(density(date_i4), family = "CentSch", xaxt='n',xlab = 'Date', lwd = 2.5, ylab = 'Density', main = 'Density and attack dates', las=1, col = "blue", lty = 2) # MAD
#lines(density(date_i3), col = 'green', lwd = 2.5, lty = 1) # ES
#lines(density(date_i2), col = 'black', lwd = 2.5, lty = 1) # MECKPOM
#lines(density(date_i), col = 'dark grey', lwd = 2.5) # rest
#abline(v= 6, col='black', lwd = 1.5, lty = 1) # 25.02.
#abline(v= 21, col='blue', lwd = 1.5, lty = 2) # 11.03.
tx=seq(min(date_i4), max(date_i4), by = 2)
lb=levels(EB610$date_f)[tx]
axis(side = 1,at=tx,labels=lb, las=0.2)
# legend

legend(2, 0.15,
       legend = c("Density MAD", "Density ESP", "Density MECKPOM", "Density remaining sample"),
       title="",
       col = c("blue", "green", 'black', "grey"),
       lty = c(2, 1, 1, 1),
       lwd = 2,
       seg.len = 0.4,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.3)
legend(22, 0.15,
       legend = #c("Attack ROST (1|0) (-)", 
         "Attack MAD (191|450)\n[time ***, explore further:\nMAD/ES, ES/ sample]", #),
       title="",
       col = c("black", "blue"),
       lty = c(1, 2),
       lwd = 2,
       seg.len = 0.4,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.3)
#### PARALLEL TRENDS ASSUMPTION PLOTS ####
# v169 | treated 1 ==> not significant
# v169 | treated 2
#----
EB610 %>% mutate(treated2 = factor(treated2)) %>%
  mutate(date = as.POSIXct(date)) %>% #convert date to date
  group_by(treated2, date) %>% #group
  summarise(prop = sum(v169=="1")/n()) %>% #calculate proportion 
  ggplot()+ theme_classic() + 
  geom_line(aes(x = date, y = prop, color = treated2)) +
  geom_point(aes(x = date, y = prop, color = treated2)) +
  geom_vline(xintercept = as.POSIXct("2004-03-10 16:00:00 UTC"), color = 'black', lwd = 1)
#----
# new: smoothed
# subset Feb 23
EB610_1 <- EB610[(EB610$date > "2004-02-22"),]
# v169 | treated 2
tiff('610.par.sm.tiff', units="px", width=3000, height=2500, res=500)
EB610_1 %>% mutate(treated2 = factor(treated2, levels = c("1", "0")), date = as.POSIXct(date)) %>% #convert date to date
  group_by(treated2, date) %>% #group
  summarise(prop = sum(v169=="1")/n()) %>% #calculate proportion 
  ggplot(family = "CentSch")+ theme_classic() + xlab("Date") + ylab('Proportion terrorism mentioned') + theme(text = element_text(family = "CentSch")) + 
  scale_color_manual(values = c('1' = 'black', '0' = 'darkgrey'),
                     labels = c('0' = 'Remaining sample', '1' = 'Madrid'), name = "Legend") +
  geom_smooth(aes(x = date, y = prop, color = treated2), se = F, method = 'loess') +
  geom_point(aes(x = date, y = prop, color = treated2)) +
  geom_vline(xintercept = as.POSIXct("2004-03-10 16:00:00 UTC"), color = 'black', lwd = 0.5, lty = 4)
dev.off()

# plot DIFFERENCE between proportions
EB610_mut <- EB610_1 %>%
  mutate(treated2 = factor(treated2, levels = c("0", "1")), date = as.POSIXct(date)) %>% 
  group_by(treated2, date) %>%
  summarise(
    prop = sum(v169 == "1") / n()) %>%
  spread(treated2, prop) %>%
  mutate(propdiff = `1` - `0`)

tiff('610.par.diff.tiff', units="px", width=3000, height=2500, res=500)
EB610_mut %>% 
  ggplot(family = "CentSch", aes(date, propdiff)) + theme_classic() + xlab("Date") + ylab('Difference in proportions of "Terrorism mentioned"') + 
  theme(text = element_text(family = "CentSch")) +
  scale_color_manual(values = c("a" = "black", propdiff = 'black'), labels = 'Madrid minus re- \nmaining sample', name = "Legend") +
  geom_line(data = EB610_mut[!is.na(EB610_mut$propdiff), ], aes(y = propdiff, x = date, color = "a")) + 
  #geom_smooth(data = EB610_mut[!is.na(EB610_mut$propdiff), ], aes(x = date, y = propdiff, color = "propdiff"), se = F, method = 'loess') +
  geom_point(aes(x = date, y = propdiff)) + 
  geom_vline(xintercept = as.POSIXct("2004-03-09 12:00:00 UTC"), color = 'black', lwd = 0.5, lty = 4)
dev.off()