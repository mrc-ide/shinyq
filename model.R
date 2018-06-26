## User supplied R code that will just wait for some time and return
## something!
user_model <- function(p) {
  Sys.sleep(sqrt(p))
  xx <- 0:20
  list(x = xx, y = dpois(xx, p))
}
