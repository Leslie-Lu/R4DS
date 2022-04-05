# install.packages('swirl')
library(swirl)
library(curl)
library(magrittr)

# 01: Completed
swirl()
install_course('Advanced R Programming')

Sys.Date()
Sys.getlocale('LC_TIME')
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

# 05: Completed
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
suppressMessages(library(tidyverse))
tidyverse_update()
packageVersion('dplyr')
devtools::install_version('dplyr', version = '1.0.8')
packageVersion('swirl')
packageVersion('yaml')
devtools::install_github('viking/r-yaml')
try(uninstall_course("Getting and Cleaning Data"), silent=TRUE)
file.choose()
file.show('/Library/Frameworks/R.framework/Versions/4.1/Resources/library/swirl/Courses/Getting_and_Cleaning_Data/Manipulating_Data_with_dplyr/lesson.yaml')
?Comparison
lifecycle::last_lifecycle_warnings()
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
?`magrittr-package`
result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
    unique = n_distinct(ip_id),
    countries = n_distinct(country),
    avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)
# Note the minus sign before grade, which says we want to gather all columns EXCEPT grade
students |> gather(sex, count, -grade)
students3 %>%
  gather(class, grade, class1:class5, na.rm= TRUE) %>%
  print
res |> separate(col = sex_class, into = c('sex', 'class'))
students2 %>%
  gather(sex_class, count, -grade) %>%
  separate(sex_class, c("sex", "class")) %>%
  print
# Use the contains() function, which you'll find detailed in 'Special functions' section of ?select
sat %>%
  select(-contains('total')) %>%
  gather(key = part_sex, value = count, -score_range) %>%
  separate(col = part_sex, into = c('part', 'sex')) |> 
  print()
sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) |> 
  mutate(
    total= sum(count),
    prop= count/ total
  ) %>% print
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  print
parse_number('class5')
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class= parse_number(class)) |> 
  print()
student_info <- students4 %>%
  select(id, name, sex) %>%
  unique() |> 
  print()
bind_rows(passed, failed)
help(package = lubridate)
this_day <- today()
this_day
year(this_day)
month(this_day)
day(this_day)
wday(this_day) #the day of week
wday(this_day, label = TRUE)
this_moment <- now()
this_moment
update(this_moment, hours= 8, minutes= 34, seconds= 55) #The update() function allows us to update one or more components of a date-time
hour(this_moment)
minute(this_moment)
second(this_moment)
my_date <- ymd('1989-05-17')
my_date
class(my_date) #ymd() took a character string as input and returned an object of class POSIXct
ymd('1989 May 17')
mdy('March 12, 1975')
dmy(25081985) #Parse 25081985, which is supposed to represent the 25th day of August 1985
dmy('25081985')
ymd("192012") #All formats failed to parse. No formats found
ymd("1920/1/2") #two dashes
ymd("1920-1-2") #two forward slashes
ymd_hms('2014-08-23 17:23:02')
hms("03:22:14") #hms() stands for hours (h), minutes (m), and seconds (s)
nyc <- now(tzone = 'America/New_York')
nyc
depart <- nyc+ days(2)
depart
depart <- update(depart, hours= 17, minutes= 34)
depart
arrive <- depart+ hours(15)+ minutes(50)
arrive <- with_tz(arrive, tzone = 'Asia/Hong_Kong')
arrive
last_time <- mdy('June 17, 2008', tz = 'Singapore')
last_time
how_long <- interval(start = last_time, end = arrive)
as.period(how_long)

# 06: in progress
swirl::install_course("MARSYS_Data_analysis_with_R")
# ?lm
# sum_lm= summary(lm.D9)
# sum_lm$coefficients[,'Pr(>|t|)'] |> round(3) |> format(scientific = F)
# library(gtsummary)
# tbl_regression(
#   lm.D9, 
#   pvalue_fun = function(x) {
#     if_else(
#       is.na(x), 
#       NA_character_,
#       if_else(x < 0.001, format(x, digits = 3, scientific = TRUE), format(round(x, 3), scientific = F))
#     )
#   } 
# )
library(swirl)
swirl()
c(1, FALSE) #double vector
# The infite set of numbers cannot be reduced to simply 2 states whereas TRUE or FALSE can easily be coerced into the two
# numbers 0 and 1. As the value 1 in this vector is not specified explicitly as integer the vector coerces both to type double
c(TRUE, 1L) #integer vector
v1 <- c(10, 5, 100); v2 <- 1:5
(v1*v2)*3
x2[5]
x2[1:4] |> sum()
x2[-c(3, 15)] |> sum()
# What happens when you subset with a name that doesn’t exist?
# NA returned
sum((x3- mean(x3))^2)
str(lm_list, max.level = 1)

# install.packages("alr4")
library(alr4)
alr4Web("primer")
# car::carWeb(setup = TRUE)






