# Load the core Tidyverse packages
library(tidyverse)
library(lubridate)

# Import the pumpkins data
setwd("C:/Users/HP/Documents/repositorio_ML/ML-For-Beginners/2-Regression/data")
pumpkins <- read_csv(file = "US-pumpkins.csv")
# Get a glimpse and dimensions of the data
glimpse(pumpkins)
#?glimpse

# Print the first 15 rows of the data set
pumpkins %>% 
  slice_head(n = 15)

# Return column names
pumpkins %>% 
  names()


#let's explore the [`janitor package`](github.com/sfirke/janitor) that provides simple 
#functions for examining and cleaning dirty data. For instance, let's take a look at the 
#column names for our data


#install.packages("janitor")
library(snakecase)
library(janitor)
# Clean names to the snake_case convention
pumpkins <- pumpkins %>% 
  clean_names(case = "snake")#case default snake

# Return column names,los nombres de la columna los limpia
#ej "% successful (2009)" lo convierte percent_successful_2009
pumpkins %>% 
  names()

# Select desired columns
pumpkins <- pumpkins %>% 
  select(variety, city_name, package, low_price, high_price, date)



# Extract the month from the dates to a new column
pumpkins <- pumpkins %>%
  mutate(date = mdy(date),#mdy función convierte a formato fecha
         month = month(date)) %>% #le decimos que tome el mes
  select(-date)#le decimos que quite la columna date

head(pumpkins)

# Create a new column for average Price
pumpkins <- pumpkins %>% 
  mutate(price = (low_price + high_price)/2)
head(pumpkins)

# Retain only pumpkins with the string "bushel"
new_pumpkins <- pumpkins %>% 
  filter(str_detect(string = package, pattern = "bushel"))
head(new_pumpkins)

# Normalize the pricing so that you show the pricing per bushel, not per 1 1/9 or 1/2 bushel
new_pumpkins <- new_pumpkins %>% 
  mutate(price = case_when(
    str_detect(package, "1 1/9") ~ price/(1.1),
    str_detect(package, "1/2") ~ price*2,
    TRUE ~ price))
head(new_pumpkins)
# Relocate column positions
new_pumpkins <- new_pumpkins %>% 
  relocate(month, .before = variety)#.before is the destination
head(new_pumpkins)

# Display the first 5 rows
new_pumpkins %>% 
  slice_head(n = 5)

LR


# Set theme
theme_set(theme_light())

# Make a scatter plot of month and price
new_pumpkins %>% 
  ggplot(mapping = aes(x = month, y = price)) +
  geom_point(size = 1.6)

#A scatter plot reminds us that we only have month data from August 
#through December. We probably need more data to be able to draw 
#conclusions in a linear fashion.


#What if we wanted to predict the `price` of a pumpkin based on the `city` or `package` 
#columns which are of type character? Or even more simply, how could we find the 
#correlation (which requires both of its inputs to be numeric) between, say, `
#package` and `price`? 

#Machine learning models work best with numeric features rather than text values, 
#so you generally need to convert categorical features into numeric representations.

#This means that we have to find a way to reformat our predictors to make them easier for
#a model to use effectively, a process known as `feature engineering`.


#In this section, we'll explore another amazing Tidymodels package: [recipes](https://tidymodels.github.io/recipes/) 
#- which is designed to help you preprocess your data **before** training your model. At its core, a recipe is an 
#object that defines what steps should be applied to a data set in order to get it ready for modelling.

#Now, let's create a recipe that prepares our data for modelling by substituting a unique integer for 
#all the observations in the predictor columns:

# Alternatively, install just recipes:
#install.packages("recipes"),A recipe is a description of the steps to be applied 
#to a data set in order to prepare it for data analysis
library(recipes)
# Specify a recipe
pumpkins_recipe <- recipe(price ~ ., data = new_pumpkins) %>% 
  step_integer(all_predictors(), zero_based = TRUE)
#step_integer will determine the unique values of each variable from the training set 
#(excluding missing values), order them, and then assign integers to each value.

# Print out the recipe
pumpkins_recipe
#We just created our first recipe that specifies an outcome (price) and its 
#corresponding predictors and that all the predictor columns should be 
#encoded into a set of integers.

#For instance the `price` column has been assigned an `outcome` role while the rest 
#of the columns have been assigned a `predictor` role.

#[`prep()`](https://recipes.tidymodels.org/reference/prep.html): estimates the required parameters 
#from a training set that can be later applied to other data sets. For instance, for a given 
#predictor column, what observation will be assigned integer 0 or 1 or 2 etc

#[`bake()`](https://recipes.tidymodels.org/reference/bake.html): takes a prepped recipe and 
#applies the operations to any data set.

#That said, lets prep and bake our recipes to really confirm that under the hood, 
#the predictor columns will be first encoded before a model is fit.

# Prep the recipe
pumpkins_prep <- prep(pumpkins_recipe)

# Bake the recipe to extract a preprocessed new_pumpkins data
baked_pumpkins <- bake(pumpkins_prep, new_data = NULL)

# Print out the baked data set
baked_pumpkins %>% 
  slice_head(n = 10)
#For instance, let's try to find a good correlation between two points of your data
#to potentially build a good predictive model. We'll use the function `cor()`
#to do this. Type `?cor()` to find out more about the function.

# Find the correlation between the city_name and the price
cor(baked_pumpkins$city_name, baked_pumpkins$price)

# Find the correlation between the package and the price
cor(baked_pumpkins$package, baked_pumpkins$price)

#While we are at it, let's also try and visualize a correlation matrix of all the
#columns using the `corrplot` package.
# Load the corrplot package
library(corrplot)

# Obtain correlation matrix
corr_mat <- cor(baked_pumpkins %>% 
                  # Drop columns that are not really informative
                  select(-c(low_price, high_price)))

# Make a correlation plot between the variables
corrplot(corr_mat, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, addCoef.col = "black", cl.pos = "n", order = "original")


#A good question to now ask of this data will be: '`What price can I expect of a given pumpkin package?`
#' Let's get right into it!

#Now that we have build a recipe, and actually confirmed that the data will be 
#pre-processed appropriately, let's now build a regression model to answer the 
#question: `What price can I expect of a given pumpkin package?`

#As you may have already figured out, the column *price* is the `outcome` variable while 
#the *package* column is the `predictor` variable.

#To do this, we'll first split the data such that 80% goes into training and 20% into 
#test set

set.seed(2056)
library(rsample)
# Split the data into training and test sets
pumpkins_split <- new_pumpkins %>% 
  initial_split(prop = 0.8)


# Extract training and test data
pumpkins_train <- training(pumpkins_split)
pumpkins_test <- testing(pumpkins_split)



# Create a recipe for preprocessing the data
lm_pumpkins_recipe <- recipe(price ~ package, data = pumpkins_train) %>% 
  step_integer(all_predictors(), zero_based = TRUE)


# Alternatively, install just parsnip:
#install.packages("parsnip")
#The goal of parsnip is to provide a tidy, unified interface to models that can be used 
#to try a range of models without getting bogged down in the syntactical minutiae of the underlying packages.
library(parsnip)
# Create a linear model specification
lm_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")


#Good job! Now that we have a recipe and a model specification, we need to find a way of bundling 
#them together into an object that will first preprocess the data (prep+bake behind the scenes), 
#fit the model on the preprocessed data and also allow for potential post-processing activities.

# Hold modelling components in a workflow

#install.packages("workflows")
#A workflow is an object that can bundle together your pre-processing, modeling, and post-processing requests
library("workflows")

lm_wf <- workflow() %>% 
  add_recipe(lm_pumpkins_recipe) %>% 
  add_model(lm_spec)

# Print out the workflow
lm_wf


#a workflow can be fit/trained in much the same way a model can.

# Train the model
lm_wf_fit <- lm_wf %>% 
  fit(data = pumpkins_train)

# Print the model coefficients learned 
lm_wf_fit

#From the model output, we can see the coefficients learned during training. They represent the coefficients of the line of best fit that gives us the lowest overall error between the actual and predicted variable.

#### Evaluate model performance using the test set

#It's time to see how the model performed

# Make predictions for the test set
predictions <- lm_wf_fit %>% 
  predict(new_data = pumpkins_test)


# Bind predictions to the test set
lm_results <- pumpkins_test %>% 
  select(c(package, price)) %>% 
  bind_cols(predictions)


# Print the first ten rows of the tibble
lm_results %>% 
  slice_head(n = 10)

#let's evaluate the model's performance!

#In Tidymodels, we do this using `yardstick::metrics()`

library(yardstick)
# Evaluate performance of linear regression
metrics(data = lm_results,
        truth = price,
        estimate = .pred)

#There goes the model performance. Let's see if we can get a better indication by visualizing a scatter plot of the package and price then use the predictions made to overlay a line of best fit.

#This means we'll have to prep and bake the test set in order to encode the package column then bind this to the predictions made by our model.


# Encode package column
package_encode <- lm_pumpkins_recipe %>% 
  prep() %>% 
  bake(new_data = pumpkins_test) %>% 
  select(package)


# Bind encoded package column to the results
lm_results <- lm_results %>% 
  bind_cols(package_encode %>% 
              rename(package_integer = package)) %>% 
  relocate(package_integer, .after = package)


# Print new results data frame
lm_results %>% 
  slice_head(n = 5)


# Make a scatter plot
lm_results %>% 
  ggplot(mapping = aes(x = package_integer, y = price)) +
  geom_point(size = 1.6) +
  # Overlay a line of best fit
  geom_line(aes(y = .pred), color = "orange", size = 1.2) +
  xlab("package")

#Great! As you can see, the linear regression model does not really well generalize the relationship between a package and its corresponding price.

#Sometimes our data may not have a linear relationship, but we still want to predict an outcome. Polynomial regression can help us make predictions for more complex non-linear relationships.

#Take for instance the relationship between the package and price for our pumpkins data set. While sometimes there's a linear relationship between variables - the bigger the pumpkin in volume, the higher the price - sometimes these relationships can't be plotted as a plane or straight line.


#### Train a polynomial regression model using the training set

#Polynomial regression creates a *curved line* to better fit nonlinear data.

#Let's see whether a polynomial model will perform better in making predictions. We'll follow a somewhat similar procedure as we did before:
  
 # -   Create a recipe that specifies the preprocessing steps that should be carried out on our data to get it ready for modelling i.e: encoding predictors and computing polynomials of degree *n*
  
  #-   Build a model specification

#-   Bundle the recipe and model specification into a workflow

#-   Create a model by fitting the workflow

#-   Evaluate how well the model performs on the test data

# Specify a recipe
poly_pumpkins_recipe <-
  recipe(price ~ package, data = pumpkins_train) %>%
  step_integer(all_predictors(), zero_based = TRUE) %>% 
  step_poly(all_predictors(), degree = 4)


# Create a model specification
poly_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")


# Bundle recipe and model spec into a workflow
poly_wf <- workflow() %>% 
  add_recipe(poly_pumpkins_recipe) %>% 
  add_model(poly_spec)


# Create a model
poly_wf_fit <- poly_wf %>% 
  fit(data = pumpkins_train)


# Print learned model coefficients
poly_wf_fit

#### Evaluate model performance

# Make price predictions on test data
poly_results <- poly_wf_fit %>% predict(new_data = pumpkins_test) %>% 
  bind_cols(pumpkins_test %>% select(c(package, price))) %>% 
  relocate(.pred, .after = last_col())


# Print the results
poly_results %>% 
  slice_head(n = 10)

#Woo-hoo , let's evaluate how the model performed on the test_set using `yardstick::metrics()`.

metrics(data = poly_results, truth = price, estimate = .pred)

#Much better performance.

#The `rmse` decreased from about 7. to about 3. an indication that of a reduced error between the actual price and the predicted price. You can *loosely* interpret this as meaning that on average, incorrect predictions are wrong by around \$3. The `rsq` increased from about 0.4 to 0.8.

#All these metrics indicate that the polynomial model performs way better than the linear model. Good job!

# Bind encoded package column to the results
poly_results <- poly_results %>% 
  bind_cols(package_encode %>% 
              rename(package_integer = package)) %>% 
  relocate(package_integer, .after = package)


# Print new results data frame
poly_results %>% 
  slice_head(n = 5)


# Make a scatter plot
poly_results %>% 
  ggplot(mapping = aes(x = package_integer, y = price)) +
  geom_point(size = 1.6) +
  # Overlay a line of best fit
  geom_line(aes(y = .pred), color = "midnightblue", size = 1.2) +
  xlab("package")



#You can see a curved line that fits your data better! 

#You can make this more smoother by passing a polynomial formula to `geom_smooth` like this:


# Make a scatter plot
poly_results %>% 
  ggplot(mapping = aes(x = package_integer, y = price)) +
  geom_point(size = 1.6) +
  # Overlay a line of best fit
  geom_smooth(method = lm, formula = y ~ poly(x, degree = 4), color = "midnightblue", size = 1.2, se = FALSE) +
  xlab("package")

#Much like a smooth curve!

#Here's how you would make a new prediction:

hypo_tibble <- tibble(package = "bushel baskets")

# Make predictions using linear model
lm_pred <- lm_wf_fit %>% predict(new_data = hypo_tibble)

# Make predictions using polynomial model
poly_pred <- poly_wf_fit %>% predict(new_data = hypo_tibble)

# Return predictions in a list
list("linear model prediction" = lm_pred, 
     "polynomial model prediction" = poly_pred)


