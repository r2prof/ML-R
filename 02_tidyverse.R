# CH-02 - Tidying, manipulating, and plotting data
#         with the tidyverse

# Install and load packages -----
# install.packages("tidyverse") # only needed once on any R installation
library(tidyverse)

# first argument of each dplyr function is the data

# Creating tibbles with tibble() ----
my_tib <- tibble(x =  1:4, y = c("london", "beijing", "las vegas", "berlin"))

my_tib

# Converting data frames to tibbles with as_tibble() ----
my_df <- data.frame(x =  1:4, y = c("london", "beijing", "las vegas", "berlin"))

my_df

df_to_tib <- as_tibble(my_df)

df_to_tib 

# Tibbles don't convert strings to factors by default ----
my_df <- data.frame(x =  1:4, y = c("london", "beijing", "las vegas", "berlin"))

my_df_notfactor <- data.frame(x =  1:4, 
                            y = c("london", "beijing", "las vegas", "berlin"),
                            stringsAsFactors = FALSE)

my_tib <- tibble(x =  1:4, 
                y = c("london", "beijing", "las vegas", "berlin"))

class(my_df$y)

class(my_df_notfactor$y)

class(my_tib$y)

# Note: To load the data as a tibble, you use the read_csv() function. 
#       read_csv() comes from the readr package, which is loaded when 
#       you call library(tidyverse)

# If you wnat to create a factor, wrap the c() function insdie factor() ----
my_tib <- tibble(x =  1:4, 
                 y = factor(c("london", "beijing", "las vegas", "berlin")))
my_tib

class(my_tib$y)

# Printing a tibble keeps the output concise ----
data(starwars)

starwars

as.data.frame(starwars)

# Subsetting with [ always returns another tibble ----
my_df[, 1]

my_tib[, 1]

# If you wish to explicitly return a tibble column as a vector, use either 
# the [[ or $ operator instead

# This behavior is desirable because we should be explicit in whether we want 
# a vector or rectangular data structure, to avoid bugs
my_tib[[1]]

my_tib$x

# An exception to this is if you subset a data frame using a single index
# with no comma (such as myDf[1]). 

# In this case, the [ operator will return a single-column data frame, but 
# this method doesn’t allow us to combine row and column subsetting. 




# Variable creation in tibble() is sequential ---
seq_tib <- tibble(items = c(12, 45, 107),
                        cost = c(0.5, 1.2, 1.8),
                        totalWorth = items * cost)

seq_tib

# Load the mtcars dataset using the data() function, convert it into a tibble, 
# and explore it using the summary() function.
data(mtcars)
str(mtcars)

mtcars_tib <- as_tibble(mtcars)

mtcars_tib


# Exploring the CO2 dataset ----
data(CO2)

CO2tib <- as_tibble(CO2)

CO2tib

# select only columns 1, 2, 3, and 5 with select() ----
selectedData <- select(CO2tib, 1, 2, 3, 5)

selectedData

# Select all of the columns of your mtcars tibble except the qsec and vs variables.
mtcars_tib

select_mtcars <- select(mtcars_tib, 1, 2, 3, 4, 5, 6, 9, 10, 11)

select_mtcars <- select(mtcars_tib, -7, -8)

select_mtcars

# Filtering data with filter() ----
# Let’s suppose we wish to filter our data to include only cases whose 
# uptake was greater than 16.
selectedData

filteredData <- filter(selectedData, uptake > 16)

filteredData

# Filter your mtcars tibble to include only cases with a number of 
# cylinders (cyl) not equal to 8.
mtcars_tib

filter_mtcars <- filter(mtcars_tib, cyl!=8)

filter_mtcars

# Groupoing data with group_by() ----
filteredData

View(filteredData)
groupedData <- group_by(filteredData, Plant)

groupedData

# You can remove a grouping structure from a tibble by wrapping it in the
# ungroup() function.
ungroupData <- ungroup(filteredData)

ungroupData

# Summarizing data with summarize() ----
# It shows the mean of the grouped data - 11 groups - 11 values for mean
summarizedData <- summarize(groupedData, mean_uptake = mean(uptake), 
                            sd_uptake = sd(uptake))

summarizedData

# Creating new variable with mutate() ----
# To calculate the coefficient of variation for each group
mutatedData <- mutate(summarizedData,  CV = (sd_uptake / mean_uptake) * 100)

mutatedData

# Arranging data with arrange() ----

# arrange() function will arrange the rows in the data so that the row
# with the smallest value of the new variable is at the top, and the row 
# with the largest value is at the bottom.

# We can arrange by multiple columns by separating them with commas: doing 
# so will arrange the cases in the order of the first variable, and any ties 
# will be ordered based on their value of the second variable, and so on with 
# subsequent ties. 
arrangedData <- arrange(mutatedData, CV)

arrangedData

# If you want to arrange a tibble in descending order of a variable’s values,
# simply wrap the variable in desc(): 
arrange(mutatedData, desc(CV))


# Using the %>% ("pipe") operator ----
c(1, 4, 7, 3, 5) %>% mean()

# Combining dplyr verbs with %>% operator ----
arrangedData <- CO2tib %>%
  select(c(1:3, 5)) %>%
  filter(uptake > 16) %>%
  group_by(Plant) %>%
  summarize(meanUp = mean(uptake), sdUp = sd(uptake)) %>%
  mutate(CV = (sdUp / meanUp) * 100) %>%
  arrange(CV)

arrangedData

# Group the mtcars tibble by the gear variable, summarize the medians of the 
# mpg and disp variables, and mutate a new variable that is the mpg median 
# divided by the disp median, all chained together with the %>% operator.
mtcars_tib
chain_mtcars <- mtcars_tib %>% 
    group_by(gear) %>% 
    summarize(mpg_med = median(mpg), disp_med = median(disp)) %>% 
    mutate(ratio = mpg_med/disp_med )

chain_mtcars


# Plotting the iris dataset with ggplot() ----
data(iris)
head(iris)

my_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  theme_bw()

my_plot

# ADDING ADDITIONAL GEOMETRIC OBJECTS ("GEOMS") AS PLOT LAYERS ----
my_plot +
  geom_density_2d() +
  geom_smooth()

# Mapping species to th shape and color aesthetics ----
# Examples of aesthetics include the x-axis, y-axis, color, shape,
# size, and even transparency of the data points drawn on the plot.
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, shape = Species)) +
  geom_point()  +
  theme_bw()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point()  +
  theme_bw()

# Notice how ggplot() automatically produces a legend when you add
# aesthetic mappings other than x and y.


# Faceting by species ----
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  facet_wrap(~ Species) +
  geom_point()  +
  theme_bw()

# Create a scatter plot of the drat and wt variables from your mtcars tibble, 
# and color the dots by the carb variable. See what happens when you wrap the 
# carb aesthetic mapping in as.factor().

head(mtcars_tib)

ggplot(mtcars_tib, aes(x = drat, y = wt)) +
    facet_wrap(~ carb) +
    geom_point()  +
    theme_bw()

ggplot(mtcars_tib, aes(x = drat, y = wt, col = carb)) +
    geom_point()  +
    theme_bw()


# Creating an untidy tibble ----
patientData <- tibble(Patient = c("A", "B", "C"),
                      Month0 = c(21, 17, 29),
                      Month3 = c(20, 21, 27),
                      Month6 = c(21, 22, 23))

patientData

# Converting untidy data to a tidy form using gather() ----
# The gather() function takes the data as its first argument. The key argument
# defines the name of the new variable that will represent the columns we are
# “gathering.” 

# In this case, the columns we are gathering are named Month0, Month3,
# and Month6, so we call the new column that will hold these keys Month. 

# The value argument defines the name of the new variable that will represent 
# the data from the columns we are gathering. In this case, the values were 
# BMI measurements, so we call the new column that will represent these values 
# BMI. 

# The final argument is a vector defining which variables to gather and 
# convert into the key-value pairs. By using -Patient, we are telling gather() 
# to use all the variables except the identifying variable, Patient.

tidyPatientData <- gather(patientData, key = Month, 
                          value = BMI, -Patient)
tidyPatientData

# The same can be achieved with:
gather(patientData, key = Month, value = BMI, Month0:Month6)

# or:
gather(patientData, key = Month, value = BMI, c(Month0, Month3, Month6))


# Gather the vs, am, gear, and carb variables from your mtcars tibble into a 
# single key value pair.
head(mtcars_tib)

gather(mtcars_tib, key = "variable", value = "value", c(vs, am, gear, carb))



# Converting data into a wide format with spread() ----
spread(tidyPatientData, key = Month, value = BMI)

# Example of pure function vs on with side effects ----
a <- 20

pure <- function() {
  a <- a + 1
  a
}

side_effect <- function() {
  a <<- a + 1
  a
}

c(pure(), pure())

c(side_effect(), side_effect())


# If you’re familiar with the apply() family of base R functions, functions
# from the purrr package help us achieve the same thing, but using a consistent
# syntax and some convenient features.

# Using purr functions for vectorization ----
listOfNumerics <- list(a = rnorm(5), 
                       b = rnorm(9),
                       c = rnorm(10))

listOfNumerics

elementLengths <- vector("list", length = 3)

for(i in seq_along(listOfNumerics)) {
  elementLengths[[i]] <- length(listOfNumerics[[i]])
}

elementLengths

map(listOfNumerics, length)

map_int(listOfNumerics, length)

map_chr(listOfNumerics, length)

map_lgl(listOfNumerics, length)

map_df(listOfNumerics, length)

map(listOfNumerics, ~. + 2)

par(mfrow = c(1, 3))

walk(listOfNumerics, hist)

iwalk(listOfNumerics, ~hist(.x, main = .y))

multipliers <- list(0.5, 10, 3)

map2(.x = listOfNumerics, .y = multipliers, ~.x * .y)

arguments <- expand.grid(n = c(100, 200),
                         mean = c(1, 10),
                         sd = c(1, 10))

arguments

par(mfrow = c(2, 4))

pmap(arguments, rnorm) %>%
  iwalk(~hist(.x, main = paste("Element", .y)))

# SOLUTIONS TO EXERCISES ----
# 1
library(tidyverse)

data(mtcars)

mtcarsTib <- as_tibble(mtcars)

summary(mtcarsTib)

# 2
select(mtcarsTib, c(-qsec, -vs))
# or
select(mtcarsTib, c(-7, -8))

# 3
filter(mtcarsTib, cyl != 8)

# 4
mtcarsTib %>%
  group_by(gear) %>%
  summarize(mpgMed = median(mpg), dispMed = median(disp)) %>%
  mutate(mpgOverDisp = mpgMed / dispMed)

# 5
ggplot(mtcarsTib, aes(drat, wt, col = carb)) +
  geom_point()

ggplot(mtcarsTib, aes(drat, wt, col = as.factor(carb))) +
  geom_point()

# 6
gather(mtcarsTib, key = "variable", value = "value", c(vs, am, gear, carb))
# or
gather(mtcarsTib, key = "variable", value = "value", c(8:11))

# 7
map_lgl(mtcars, ~sum(.) > 1000)
# or
map_lgl(mtcars, function(.) sum(.) > 1000)
