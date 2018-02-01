library(tidyverse)
library(stringr)
library(dbplyr)

#import and view stucture of raw data
raw <- read_csv("nyc_data.csv")
View(raw)

#clean data, dimensional reduction
sel <- raw %>% select(crimsusp, arstmade, frisked, searched, contrabn, pistol, riflshot, asltweap,
                       knifcuti, machgun, othrweap, sex, race, age, ht_feet, ht_inch, weight, stname,
                       city)
clean <- filter(sel, !is.na(sel$arstmade))
#confirm results
View(clean)

#How many stoped citizens are actually arrested?
ggplot(sel, mapping = aes(x = arstmade)) +
  geom_bar()
  
#Find exact numbers
arst_per <- function(x){
  total <- 0
  num_n <- 0
  for(i in seq_along(x)){
    total <- total + 1
    if(x[i] == "N"){
      num_n <- num_n +1
    }
  }
  return(num_n / total)
}

(per <- arst_per(clean$arstmade));

# A staggering 78.69% are innocent!!!
# lets look at distribution of race amoung those who are stopped

ggplot(clean, mapping = aes(x = race))+
  geom_bar(mapping = aes(fill = arstmade))

# A stuning view Black people are stopped much more than anyother race and innocent the vast 
# majority of time

b_males <- filter(clean, race == "B")
(b_per <- arst_per(b_males$arstmade))

# Higher than total 79.93% of black people are innocent!
# lets look at the amount of stop that involve weapon and there distribution

c_weap <- function(x){
  count <- 0
  for(i in seq_along(x)){
    if(x[[i]] == "Y"){
      count <- count + 1
    }
  }
  return(count)
}

pistol <- c_weap(clean$pistol)
rifle_shot <- c_weap(clean$riflshot)
assault <- c_weap(clean$asltweap)
knife <- c_weap(clean$knifcuti)
mach_gun <- c_weap(clean$machgun)
other <- c_weap(clean$othrweap)

weap <- tribble(
  ~weapon,              ~num_offenses,
  #-------------------/---------------
  "pistol",             pistol,
  "rifle or shotgun",   rifle_shot,
  "assault rife",       assault,
  "knife",              knife,
  "machine gun",        mach_gun,
  "other",              other
  )
 
ggplot(weap, mapping = aes(x = weapon, y = num_offenses))+
  geom_point(aes(color = weapon))+
  coord_flip()

# knife is the most common weapon, then an improvised weapon, then a pistol