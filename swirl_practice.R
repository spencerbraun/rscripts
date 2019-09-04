

## sequences
my_seq <- seq(5, 10, length.out = 30)
#sequences with length of my_seq
seq(along.with = my_seq)
seq_along(my_seq)
rep(c(0, 1, 2), times = 10) #replicate
unique(c(3,4,5,5,5,6,6))

## vectors - only one datatype within a vector
my_char <- c("My", "name", "is")
paste(my_char, collapse = " ")
my_name = c(my_char, "spencer")
paste("Hello", "world!", sep = " ")
paste(LETTERS, 1:4, sep="-")

my_char[1:2] # 1 based indexing!!!
y <- x[!is.na(x)]
x[!is.na(x) & x > 0]
x[c(-2, -10)] # exclusionary indexing
vect <- c(foo = 11, bar = 2, norf = NA) #named vectors
names(vect) <- c("foo", "bar", "norf")
vect["bar"]
identical(vect, vect2) # check indentical

## Missing values
y <-rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y, z), 100)
is.na(my_data)
# NA is missing value, NaN = not a number, eg 0/0

## Matrices and dataframes
my_vector <- 1:20
dim(my_vector) <- c(4, 5)
attributes(my_vector)
my_matrix2 <- matrix(1:20, 4, 5)

patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, my_matrix2)
my_data <- data.frame(patients, my_matrix)
cnames <-c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames

## Logic
isTRUE(6 > 4)
xor(5 == 6, !FALSE) #exclusive or, just one element is true
int <- sample(10)
which(ints > 7)
any(ints < 0)
all(ints < 0)

## Functions
Sys.Date()

boring_function <- function(x) {
  x
}

evaluate <- function(func, dat){
  func(dat)
}
#anonymous function, like a lambda function
evaluate(function(x){x[1]}, c(8, 4, 0))
#elipses as function args
telegram <- function(...){
  paste("START", ..., "STOP")
}
#binary operator
"%p%" <- function(left, right){
  paste(left, right)
}


## Split-Apply-Combine
cls_list <- lapply(flags, class) # list apply - apply class to each column of flags df
as.character(cls_list)

cls_vect <- sapply(flags, class) # simplify - return vector, matrix, etc, function makes a guess at right datatype

flag_colors <- flags[, 11:17] # all rows, columns 11 - 17

lapply(unique_vals, function(elem) elem[2])

vapply(flags, class, character(1)) # specify returned datatype - here character vector of length 1
table(flags$landmass) # flags.groupby(landmass).size()
tapply(flags$animate, flags$landmass, mean) # proportion of animate flags within each landmass group
tapply(flags$population, flags$red, summary)


## Datawork
nrow(plants)
head(plants)
summary(plants)
str(plants) #structure of data

## Simulation
sample(1:6, 4, replace = TRUE) # rolling 4 die
flips <-sample(c(0,1), 100, replace=TRUE, prob=c(0.3, 0.7)) #flips of a biased coin
rbinom(1, size=100, prob=0.7) #equal to flips
flips2 <- rbinom(100, size=1, prob=0.7) #shows all results

rnorm(10)
rnorm(10, mean=100, sd=25)

my_pois <- replicate(100, rpois(5 ,10))
cm <- colMeans(my_pois)
hist(cm)


## Dates and times
as.Date("1969-01-01")
Sys.Date()

Sys.time()
as.POSIXlt(Sys.time())

weekdays(d1)
months(t1)

strptime("October 17, 1986 08:24", "%B %d, %Y %H:%M")
difftime(Sys.time(), t1, units='days')


## Native graphics
data(cars)
plot(cars)
plot(x = cars$speed, y = cars$dist, xlab = "Speed", ylab = "Stopping Distance", main = "My Title")
plot(cars, col = 2, pch = 2) # set colors and shapes
plot(cars, xlim = c(10, 15)) #set range

boxplot(mpg ~ cyl, mtcars) # specified formula, and dataset
hist(mtcars$mpg)

############################

## dplyr
library(dplyr)
read.csv(path2csv, stringsAsFactors = FALSE)
cran <- tbl_df(mydf) # table dataframe - nicer to print, tibble
#select(), filter(), arrange(), mutate(), and summarize()
select(cran, ip_id, package, country) # select dataframe, then columns in this order
select(cran, r_arch:country)
select(cran, -time) #exclude one column
select(cran, -(X:size))
select(cran, -contains("total")) #exclude columns that contain total in name

filter(cran, r_version == "3.1.1", country == "US") #filter df to rows where package is swirl, comma sep conditions
filter(cran, country == "US" | country == "IN")

cran2 <- select(cran, size:ip_id)
arrange(cran2, ip_id) #sort rows by ip_id

mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10) #create column size_mb, size_gb, defined here

summarize(cran, avg_bytes = mean(size)) # collapse to summary row, pass aggregating function

# Grouping and Chaining
by_package <- group_by(cran, package)
summarize(by_package, mean(size)) #cran.groupby('package')['size'].avg()
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))

quantile(pack_sum$count, probs = 0.99) # 99th percentile of count

View(top_counts) # open dataframe as data sheet in scripts section

#chaining
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

cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb)) %>%
  print

## tidyr
#1) Each variable forms a column
#2) Each observation forms a row
#3) Each type of observational unit forms a table

#for variables as columns
gather(students, sex, count, -grade) # data, key, value, exclude from gathering -> grade, sex, count
#for mixed data in a single column - here sex and class, eg male_2
students2 %>%
  gather(sex_class, count, -grade) %>%
  separate(sex_class, c("sex", "class")) %>%
  print
# gather classes into a class column, have "test" midterm and final go from rows to columns
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = parse_number(class)) %>%
  print

#union
bind_rows(passed, failed)


## Dates and Times with Lubridate
this_day <- today()
this_moment <- now()
now("America/New_York")
wday(this_day) #numbered day of the week
ymd("1989-05-17")
dmy(25081985)
ymd_hms("2014-08-23 17:23:02")
update(this_moment, hours = 8, minutes = 34, seconds = 55)

now("America/New_York") + days(2)