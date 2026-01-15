# Green, Donald P., Alan S. Gerber, and David W. Nickerson. " Getting out the 
# vote in local elections: Results from six door-to-door canvassing experiments." 
# The Journal of Politics 65.4 (2003): 1083-1096.
# doi: https://doi.org/10.1111/1468-2508.t01-1-00126
# replication data here: https://isps.yale.edu/research/data/d017
library(dplyr)

dat <- read.csv("../data/GreenGerberNickerson_JP_2003-1_EDITED.csv")
dat <- dat |> 
  filter(city == "Raleigh")

# Treatment and control are unequal
table(dat$treatmen) # yes it's missing a 't'

# Differences are due to different treatment assignments across "turfs"
prop.table(table(dat$turf, dat$treatmen), margin = 1)

# recode to Ys and Ds
dat <- dat |> 
  mutate(
    y = voted00,
    d = ifelse(treatmen == "Treatment", 1, 0)
  )

# generate p_score
dat <- dat |> 
  group_by(turf) |> 
  mutate(p_score = mean(d))