rm(list = ls()) # clear out the variables from memory to make a clean execution of the code.

# If you want to remove all previous plots and clear the console, run the following two lines.
graphics.off() # clear out all plots from previous work.

cat("\014") # clear the console

library(tidyverse)
crime <- read_csv("Desktop/CS301/classDocs301/lessons/09/sandbox/crime.csv")
tibble::as_tibble(crime)

#plot with general trend line
crime %>% ggplot(aes(x = low, y = tc2009)) + geom_point(alpha = I(1/4)) + geom_smooth()
#plot with linear model line
crime %>% ggplot(aes(x = low, y = tc2009)) + geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

crime %>% ggplot(aes(x = low, y = tc2009)) + geom_point(alpha = I(1/4)) + geom_smooth()

crime %>% ggplot(aes(x = low, y = tc2009)) + geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

mod <- lm(tc2009 ~ low, data = crime)
mod
# Function to compute estimated y value for an entered x value
tellMeY <- function(x_int){
  cat("  intercept :",mod$coefficients[1] )
  cat("\n  slope     :",mod$coefficients[2] )
  y = mod$coefficients[1] + x_int * mod$coefficients[2]
  cat("\n Model predicts y = ",y, "from x = ",x_int)
}
# what if x = -10? 
tellMeY(20) # note: x = (x)

plot(crime$low, crime$low*mod$coefficients[2] + mod$coefficients[1] )

?predict
crime %>% ggplot(aes(x = low, y = predict(mod))) + geom_point(alpha = I(1/4))
crime %>% ggplot(aes(x = low, y = predict(mod))) + geom_point(alpha = I(1/4)) + geom_smooth()

# equivalent - includes intercept
mod_withIntercept <- lm(tc2009 ~ 1 + low, data = crime)
crime %>% ggplot(aes(x = low, y = predict(mod_withIntercept))) + 
  geom_point(alpha = I(1/4))+ 
  geom_smooth()

# equivalent - removes intercept
mod_noIntercept <- lm(tc2009 ~ low - 1, data = crime)
crime %>% ggplot(aes(x = low, y = predict(mod_noIntercept))) +
  geom_point(alpha = I(1/4)) + 
  geom_smooth()                              

summary(mod)

