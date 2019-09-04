#install.packages("dslabs")
library(dslabs)

## Using data frames outside of the tidyverse
data(murders)
class(murders)
?data

str(murders)
head(murders)
names(murders)
murders$population
pop <- as.integer(murders$population)
length(pop)
class(pop)
pop

?Comparison

class(murders$region)
levels(murders$region)

mat <- matrix(1:12, 3, 4)
as.data.frame(mat)

vect <- c(1, 2, 3)
vect[2]

sort(murders$total)
murders$abb[order(murders$total)]
max(murders$total)
murders$abb[which.max(murders$total)]


## R programming
a <- 2

#conditioning
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

ifelse(a > 0, 1/a, NA)
b <- c(0,1,2,-4,5)
ifelse(b > 0, 1/b, NA)

#functions in diff namespaces
stats::filter()
dplyr::filter()

#loops
for(i in 1:5){
  print(i)
}

compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
s_n <- vector(length = 25) # create an empty vector
for(n in 1:25){
  s_n[n] <- compute_s_n(n)
}
s_n



## Tidyverse

murders %>%
  mutate( rate = total / population * 100000) %>%
  arrange(desc(rate)) %>%
  head()

data(heights)

heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), std = sd(height)) %>%
  pull(average)

?pull

top_n(murders, 10, rate)
murders %>% group_by(region) %>% class()
as_tibble(murders)
tibble(names = c("John", "Juan", "Jean", "Yao"),
       exam_1 = c(95, 80, 90, 85),
       exam_2 = c(90, 85, 85, 90)
)

#dot notation
filter(murders, region == "South") %>%
  mutate(rate = total / population * 10^5) %>%
  .$rate #similar to pull

# do functions
my_summary <- function(dat){
  x <- quantile(dat$height, c(0, 0.5, 1))
  tibble(min = x[1], median = x[2], max = x[3])
}
heights %>%
  group_by(sex) %>%
  do(my_summary(.)) #here the name of the piped tibble is .

#purr and functional programming
library(purrr)
s_n <- map(n, compute_s_n)
s_n
?map_dbl
compute_s_n <- function(n){
  x <- 1:n
  tibble(sum = sum(x))
}
map_df(n, compute_s_n)


#tidy conditionals
x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ "Negative", x > 0 ~ "Positive", TRUE ~ "Zero")

murders %>%
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "other")) %>%
  group_by(group) %>%
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  arrange(rate)

between(5, 1, 7)


## Data imports
system.file(package = "dslabs")
file.path(system.file("extdata", package = "dslabs") , "murders.csv")
list.files(file.path(system.file(package = "dslabs"), "extdata"))

library(readr)
?readr
setwd(system.file("extdata", package = "dslabs") )
read_lines("murders.csv", n_max = 3)
read_csv("https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv")


## Data Vis
library(ggplot2)
library(ggthemes)
library(ggrepel)

data(murders)
#keep variable names within aes
#aes applies to mappings - things that depend on the data
murders %>%
  ggplot() +
  geom_point(aes(x = population/10^6, y = total), size = 1) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.2)

# or try to define mapping in first step
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_point(size = 3) +
  geom_text_repel(nudge_x = 0.05) + #from ggrepel for better labeling
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  # same as:
  #scale_x_log10() +
  #scale_y_log10()
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

  #geom_text(aes(x = 10, y = 800, label = "Additional Text")) # this is not related to the data, so defined separate aes

r <- murders %>%
  summarize(rate = sum(total) /  sum(population) * 10^6) %>%
  pull(rate) #add a line of best fit

p <- p + geom_point(aes(col=region), size = 3) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey")

p + theme_economist() + scale_color_discrete(name = "Region")  # changes to legend


library(maps)
library(mapproj)
us = map_data("state")
murders %>%
  mutate(rate = total / population/10^6, state = tolower(state)) %>%
  ggplot(aes(map_id = state, fill=total)) +
  geom_map(map=us, color='black') +
  expand_limits(x=us$long, y= us$lat) +
  coord_map() +
  ggtitle('Murders by State')



