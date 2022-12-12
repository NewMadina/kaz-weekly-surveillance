#load here
library(here)

#basic folder structure
c(
  here("R"), 
  here("output"), 
  here("resources"),
  here("data"), 
  here("data/raw"),
  here("data/raw/polio"),
  here("data/raw/measles"),
  here("data/int"), 
  here("data/int/polio"),
  here("data/int/measles")
) |> 
  sapply(dir.create)
