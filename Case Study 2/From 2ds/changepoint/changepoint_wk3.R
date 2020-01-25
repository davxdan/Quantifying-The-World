
library(changepoint)

"
Successfully loaded changepoint package version 2.2.2
NOTE: Predefined penalty values changed in version 2.2.  
Previous penalty values with a postfix 1 
i.e. SIC1 are now without 
i.e. SIC and previous penalties without a postfix 
i.e. SIC are now with a postfix 0 
i.e. SIC0. See NEWS and help files for further details.
"

# PG 8
set.seed(10)
m.data <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 0, 1),
            + rnorm(100,  0.2, 1))
ts.plot(m.data,  xlab = "Index")

# PG 9
m.pelt <-  cpt.mean(m.data, method = "PELT")
plot(m.pelt,  type = "l",  cpt.col = "blue", xlab  =  "Index", cpt.width = 4)
cpts(m.pelt)
# [1] 97 192
"
How many changepoints did they find?
[1] 97 192 273 353 362 366
6?
Why the difference?
Oh, right.  Something about penalty values changing in version 2.2.
"

m.pelt <- cpt.mean(m.data, penalty="SIC0", method = "PELT") # we now have to explicitly set the penalty
plot(m.pelt,  type = "l",  cpt.col = "blue", xlab  =  "Index", cpt.width = 4)
cpts(m.pelt)
"
Aha. Now we have the same results.
[1] 97 192 273 353 362 366
"


