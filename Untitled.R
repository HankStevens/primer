devtools::document()

coneflower <- read.csv('coneflower.csv')
coneflowerseeds <- read.csv('seeds.csv')
coneflowerrecruits <- read.csv('recruits.csv')
usethis::use_data(coneflowerrecruits)
