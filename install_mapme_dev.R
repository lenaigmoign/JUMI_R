library(mapme.biodiversity)
detach("package:mapme.biodiversity", unload = TRUE)
remove.packages("mapme.biodiversity")
install.packages("remotes")
new <- "https://github.com/fBedecarrats/mapme.biodiversity/tree/gfw_2017_to_2020"
remotes::install_github(new, upgrade = "never")
library(mapme.biodiversity)