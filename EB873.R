# File: EB87.3
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

EB873 <- read.dta("87.3_ZA6863_v1-0-0.dta", convert.factors = FALSE) #  eurobarometer wave 87.3, gesis reference data set: ZA6863
EB873$date <- as.Date(EB873$p1, origin = "2017-05-19")
EB873 <- subset(EB873, (!is.na(EB873$qa3a_6) & !is.na(EB873$qa4a_6) & !is.na(EB873$qa5_6))) #subset on DV != na
EB873 <- subset(EB873, (!is.na(EB873$date))) #subset on date != na
EB873$group = ifelse((EB873$isocntry == "GB-GBN" & EB873$p7gb == 2), 1,
                     ifelse(((EB873$isocntry == "GB-GBN" & EB873$p7gb != 2) | EB873$isocntry == "GB-NIR"), 2, 3)) # group: 1 = North West Engl, 2 = GB-GBN, 3 = Rest
EB873 <- EB873 %>% mutate(regions = group_indices(., isocntry, p7be, p7at, p7bg, p7gr, p7es, p7fi, p7fr, p7gb, p7hu, p7ie, p7it, p7lt, p7lu, p7lv, 
                                 p7nl, p7pl, p7pt, p7ro, p7se, p7si, p7sk, p7hr, p7al, p7tr, p7cytcc, p7mk, p7me, p7rs)-1)
      # add one column with all regions

#View(EB873$date)
#---- information ----
# Date        Date# City        Region# +   *
# 2017-05-22  3(+1) Manchester  2       22  512 # w (attack was on the 22nd [3] in the evening ==> 4)

#---- DiD ----
# dummy time
EB873$time = ifelse(EB873$p1 >=4, 1, 0) # treatment on 22.05. night ==> 23.05.
# dummy treatment
EB873$treated1 = ifelse((EB873$isocntry == "GB-GBN" & EB873$p7gb == 2), 1, 0) # North West
EB873$treatedUK = ifelse((EB873$isocntry == "GB-GBN"), 1, 0) # UK
# did interaction
EB873$did1 = EB873$time * EB873$treated1 # interaction term *did1*
EB873$didUK = EB873$time * EB873$treatedUK # interaction term *did1*

### DiD MODELS ####
#DVs qa3a_6 qa4a_6 qa5_6
didreg1    = lm(qa3a_6 ~ treated1 + time + did1, data = EB873[(EB873$date > "2017-05-21" & EB873$date < "2017-05-24"),]) #subset 19-26
didreg1_pt = lm(qa3a_6 ~ #factor(date) + 
                  date * regions + treated1 + time + did1, data = EB873)
didreg1UK    = lm(qa3a_6 ~ treatedUK + time + didUK, data = EB873[(EB873$date > "2017-05-21" & EB873$date < "2017-05-24"),]) #subset 19-26
didreg1UK_pt = lm(qa3a_6 ~ #factor(date) + 
                  date * regions + treatedUK + time + didUK, data = EB873)
#didreg2 = lm(qa4a_6 ~ treated + time + did, data = EB873[(EB873$date > "2017-05-21" & EB873$date < "2017-05-24"),]) #subset 19-26
#didreg3 = lm(qa5_6  ~ treated + time + did, data = EB873[(EB873$date > "2017-05-21" & EB873$date < "2017-05-24"),]) #subset 19-26
summary(didreg1_pt)  # **     | important issues country UK/NW
summary(didreg1UK_pt) # ***   | important issues country UK
#summary(didreg2)  # -     | important issues personally
#summary(didreg3)  # .     | important issues EU
# ==> country significant but not personally!

### OUTPUT ###
stargazer(didreg1_pt, didreg1UK_pt, type="html",
          dep.var.labels = "Afraid of terrorism",
          dep.var.caption = "Important issues: terrorism mentioned (0/1)",
          title = "Difference-in-differences (Manchester attack, 2017)",
          dep.var.labels.include = FALSE,
          intercept.bottom = TRUE,
          model.names = TRUE,
          model.numbers = TRUE,
          no.space = TRUE,
          star.char = "*",
          order = c("date",  "date*regions", "time", "regions", "treated1", "treatedUK", "did1", "didUK"),
          covariate.labels = c("Date",  "Parallel trends control", "After attack", "Regions","NW England", "UK",
                              "NW England after attack", "UK after attack"),
          notes.align = "l", notes = c('The attack occurred in Manchester on the evening of 22.05. Two groups are analysed as "treated": (i) the region around Manchester (Northwest England), and (ii) the United Kingdom (excluding NI), each compared to the remaining sample (excluding the United Kingdom). "Date" describes a linear control for time; "Parallel trends control" is an interaction of regions and time to account for temporal and spatial variations other than through the attack; "After attack" is a dummy for those interviewed after the attack (participants interviewed on 23.05. and after); "Regions" is a control for the entire sample on the regional level; "NW England" and "UK" are dummies for the affected region/ country; "NW England after attack" and "UK after attack" respectively describe the increased proportion of those in Northwest England or the United Kingdom interviewed after the attack that mentioned "terrorism" as one of the two most important issues for the country in the future.'),
          out = "#EB873_new.html")
# ==> significant for full sample

#---- BACKGROUND PLOTS ----
#### DENSITY PLOTS ####
EB873$date_f <- as.factor(EB873$date[!is.na(EB873$date)]) # date as factor
EB873$date_i <- as.integer(EB873$date_f)
EB873$date_i <- EB873$date_i[!is.na(EB873$date_i)]
# number ==> date....

# date by region
date_i  <- EB873$date_i[(EB873$isocntry != "GB-GBN") | (EB873$isocntry == "GB-GBN" & EB873$p7gb != 2)] # date group 1: rest
date_i2 <- EB873$date_i[EB873$p7gb == 2]  # date group 2: UK N/W
date_i3 <- EB873$date_i[EB873$isocntry == "GB-GBN"]  # date group 2: UK

date_i  <- date_i[!is.na(date_i)]
date_i2 <- date_i2[!is.na(date_i2)]
date_i3 <- date_i3[!is.na(date_i3)]
#View(date_i)

#plot
plot(density(date_i3), family = "CentSch", xaxt='n',xlab = 'Date', lwd = 2.5, ylab = 'Density', main = 'Density and attack dates', las=1, col = "blue", lty = 1) # GB
lines(density(date_i), col = 'grey', lwd = 2.5, lty = 1) # rest
lines(density(date_i2), col = 'black', lwd = 2.5, lty = 1) # GB N/W
abline(v= 4, col='black', lwd = 1.5, lty = 1) #
tx=seq(min(date_i), max(date_i), by = 2)
lb=levels(EB873$date_f)[tx]
axis(side = 1,at=tx,labels=lb, las=0.2)
# legend
legend(-0.95, 0.21,
       legend = c("Density NW/GB", "Density GB", "Density remaining sample"),
       title="",
       col = c("black", 'blue', "grey"),
       lty = c(1, 1, 1),
       lwd = 2,
       seg.len = 0.4,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.3)
legend(9, 0.21,
       legend = "Attack MANCH",
       title="",
       col = "black",
       lty = c(1, 2),
       lwd = 2,
       seg.len = 0.4,
       bty = 'n', # no box around legend
       pt.cex = 0.9,
       cex = 0.9, merge = TRUE,
       text.width = 10,
       x.intersp = 0.3)

#### PARALLEL TRENDS ASSUMPTION PLOTS ####
#--- OLD ----
EB873 %>% mutate(treated = factor(treated1)) %>%
  mutate(date = as.POSIXct(date)) %>% #convert date to date
  group_by(treated, date) %>% #group
  summarise(prop = sum(qa5_6=="1")/n()) %>% #calculate proportion 
  ggplot()+ theme_classic() + 
  geom_line(aes(x = date, y = prop, color = treated)) +
  geom_point(aes(x = date, y = prop, color = treated)) +
  geom_vline(xintercept = as.POSIXct("2017-05-22 12:00 GMT"), color = 'black', lwd = 1)

EB873 %>% mutate(treatedUK = factor(treatedUK)) %>%
  mutate(date = as.POSIXct(date)) %>% #convert date to date
  group_by(treatedUK, date) %>% #group
  summarise(prop = sum(qa5_6=="1")/n()) %>% #calculate proportion 
  ggplot()+ theme_classic() + 
  geom_line(aes(x = date, y = prop, color = treatedUK)) +
  geom_point(aes(x = date, y = prop, color = treatedUK)) +
  geom_vline(xintercept = as.POSIXct("2017-05-22 12:00 GMT"), color = 'black', lwd = 1)
#--- NEW ----
EB873_1 <- EB873[EB873$date < "2017-05-28",] # exclude time too long after attack
# region & country    smoothing of line
tiff('873.par.sm.tiff', units="px", width=3000, height=2500, res=500)
EB873_1 %>%
  mutate(group = factor(group)) %>%
  mutate(date = as.POSIXct(date)) %>% #convert date to date
  group_by(group, date) %>% #group
  summarise(qa5_6 = sum(qa5_6=="1")/n()) %>% #calculate proportion 
  ggplot() + xlab("Date") + ylab('Proportion terrorism mentioned') +
  theme_classic() + theme(text = element_text(family = "CentSch")) + 
  scale_color_manual(values = c('1' = 'black', '2' = 'darkgrey', "3" = "lightgrey"),
                     labels = c('1' = 'NW England', '2' = 'United Kingdom', "3" = "Remaining sample" ),
                     name = "Legend") +
  geom_smooth(aes(x = date, y = qa5_6, color = group), se = F, method = 'loess') +
  geom_point(aes(x = date, y = qa5_6, color = group)) +
  geom_vline(xintercept = as.POSIXct("2017-05-22 12:00 GMT"), color = 'black', lwd = 0.5, lty = 4)
dev.off()


# plot DIFFERENCE between proportions
EB873_mut <- EB873_1 %>%
  mutate(group = factor(group), date = as.POSIXct(date)) %>% 
  group_by(group, date) %>%
  summarise(
    prop = sum(qa5_6 == "1") / n()) %>%
  spread(group, prop) %>%
  mutate(propdiff1 = `1` - `3`, propdiff2 = `2` - `3`)

colnames(EB873_1)
table(EB873_1$group)

tiff('873.par.diff.tiff', units="px", width=3000, height=2500, res=500)
EB873_mut %>% 
  ggplot(family = "CentSch", aes(date, propdiff1)) + theme_classic() + xlab("Date") + ylab('Difference in proportions of "Terrorism mentioned"') + 
  theme(text = element_text(family = "CentSch")) +
  scale_color_manual(
    values = c("a" = "black", "b" = "grey"), labels = c("a" = "NW England minus remaining \nsample (excl. UK)", "b" = "UK minus remaining sample"), name = "Legend") +
  geom_line(data = EB873_mut[!is.na(EB873_mut$propdiff1), ], aes(y = propdiff1, x = date, color = "a")) + 
  geom_point(aes(x = date, y = propdiff1)) + 
  
  geom_line(data = EB873_mut[!is.na(EB873_mut$propdiff2), ], aes(y = propdiff2, x = date, color = "b")) + 
  geom_point(aes(x = date, y = propdiff2, color = "b")) + 
  
  geom_vline(xintercept = as.POSIXct("2017-05-22 12:00:00 UTC"), color = 'black', lwd = 0.5, lty = 4)
dev.off()