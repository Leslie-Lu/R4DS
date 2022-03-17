install.packages('swirl')
library(swirl)

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



