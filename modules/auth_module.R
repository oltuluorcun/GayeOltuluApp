library(shinyauthr)
library(dplyr)

# sample logins dataframe with passwords hashed by sodium package
user_base <- tibble(
  user = c("admin", "user2"),
  password = sapply(c("admin", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("admin", "User Two")
)