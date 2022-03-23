install.packages('swirl')
library(swirl)
library(curl)

# 01: Completed
swirl()
install_course('Advanced R Programming')

Sys.Date()
evaluate <- function(func, dat){
  e= func(dat)
  return(e)
}
evaluate(sd, c(1.4, 3.6, 7.9, 8.8))
evaluate(func = function(x) {x+1}, 6) #anonymous function
telegram <- function(...){
  paste('START', ..., 'STOP')
}
mad_libs <- function(...){
  args= list(...)
  place= args[['place']]
  adjective= args[['adjective']]
  noun= args[['noun']]
  paste("News from", place, "today where", adjective, "students took to the streets in protest of the new", noun, "being installed on campus.")
}
mad_libs(place= 'Huatuo Road', adjective= 'happy together', noun= 2)
"%p%" <- function(left, right){
  paste(left, right)
}
'I' %p% 'love' %p% 'R!'

citation()

# 02: not work
# repos <- "http://wush978.github.io/R"
# pkgs.info <- available.packages(contriburl = contrib.url(repos, "source"))
# pkgs.info
# pkgs <- tools:::.extract_dependency_package_names(pkgs.info["swirl","Imports"])
# pkgs
# invisible(lapply(pkgs, function(pkg) {
#   utils::install.packages(pkg, repos = repos)
#   # utils::install.packages("swirl", repos = repos, type = "source")
# }))
# update.packages(ask = FALSE)
try(uninstall_course("DataScienceAndR"), silent=TRUE)
# Sys.setlocale(locale = "cht")

# 03: waiting
swirl::install_course("Exploratory Data Analysis")

# 04: waiting
# Statistical Inference

# 05: in progress
swirl::install_course("Getting and Cleaning Data")
remove.packages('tidyverse')
remove.packages('dplyr')
# install.packages('githubinstall')
library(githubinstall)
?curl_download
hadleyverse= gh_list_packages(username= "hadley")[1:3,]
repos= with(hadleyverse, paste(username, package_name, sep="/"))  
githubinstall(repos)
devtools::install_github("tidyverse/tidyverse")
library(tidyverse)
tidyverse_update()
packageVersion('dplyr')
devtools::install_version('dplyr', version = '1.0.0')
packageVersion('swirl')
packageVersion('yaml')
devtools::install_github('viking/r-yaml')
try(uninstall_course("Getting and Cleaning Data"), silent=TRUE)
file.choose()
file.show('/Library/Frameworks/R.framework/Versions/4.1/Resources/library/swirl/Courses/Getting_and_Cleaning_Data/Manipulating_Data_with_dplyr/lesson.yaml')
?Comparison




