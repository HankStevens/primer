devtools::document()

coneflower <- read.csv('coneflower.csv')
coneflowerseeds <- read.csv('seeds.csv')
usethis::use_data(coneflower, coneflowerseeds, overwrite=TRUE)
