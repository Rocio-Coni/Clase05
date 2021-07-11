suma <- function(){}
library(nycflights13)
library(tidyverse)
install.packages("Hmisc")

flights

iris02 <- as_tibble(iris)

dplyr::filter(iris02, Petal.Length <= 5 & Petal.Length >= 2)
dplyr::filter(iris02, Petal.Length <= 5 & Sepal.Width >= 3)
dplyr::filter(iris02, Petal.Length <= 5 | Sepal.Width >= 3)

(filter1 <- dplyr::filter(flights, month ==12, day == 25))

vector1 <- c(55,12)
vector1 %<>% sum()

(iris02 %<>% dplyr::filter(Petal.Length <= 5 | Sepal.Width >= 3))

dplyr::filter(flights, month == 11 | month == 12)
dplyr::filter(flights, month %in% c(9,10))

df01 <- dplyr::filter(flights, month == c(9,10))
df02 <- dplyr::filter(flights, month %in% c(9,10))
table(df01$month)

df03 <- dplyr::filter(flights, ! (month %in% c(9,10,11,12)))
table(df02$month)

view(df01)


