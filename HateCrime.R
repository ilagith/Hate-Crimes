

## Load packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(styler)

styler:::style_active_file()

## Load data
hate_crime <- read.delim("input/hate_crime.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = TRUE
)

## Looking at the Data structure
head(hate_crime)
str(hate_crime)
dim(hate_crime)

## Data cleaning ---------------------------------------------------------------

## Changing column names tolower
names(hate_crime) <- tolower(names(hate_crime))

## Checking each variable using table function
table(hate_crime$multiple_bias, useNA = "always")
table(hate_crime$incident_id, useNA = "always")
table(hate_crime$data_year, useNA = "always")
table(hate_crime$ori, useNA = "always")
table(hate_crime$pub_agency_name, useNA = "always")
table(hate_crime$pub_agency_unit, useNA = "always")
table(hate_crime$agency_type_name, useNA = "always")
table(hate_crime$state_abbr, useNA = "always")
table(hate_crime$state_name, useNA = "always")
table(hate_crime$division_name, useNA = "always")
table(hate_crime$region_name, useNA = "always")
table(hate_crime$population_group_code, useNA = "always")
table(hate_crime$population_group_desc, useNA = "always")
table(hate_crime$incident_date, useNA = "always")
table(hate_crime$adult_victim_count, useNA = "always")
table(hate_crime$juvenile_victim_count, useNA = "always")
table(hate_crime$total_offender_count, useNA = "always")
table(hate_crime$adult_offender_count, useNA = "always")
table(hate_crime$juvenile_offender_count, useNA = "always")
table(hate_crime$offender_race, useNA = "always")
table(hate_crime$offender_ethnicity, useNA = "always")
table(hate_crime$victim_count, useNA = "always")
table(hate_crime$offense_name, useNA = "always")
table(hate_crime$total_individual_victims, useNA = "always")
table(hate_crime$location_name, useNA = "always")
table(hate_crime$bias_desc, useNA = "always")
table(hate_crime$victim_types, useNA = "always")
table(hate_crime$multiple_offense, useNA = "always")

## Replacing empty factor level in offender race with "Unknown"
hate_crime$offender_race[hate_crime$offender_race == ""] <- "Unknown"
hate_crime$offender_race <- droplevels(hate_crime$offender_race, exclude = "")
table(hate_crime$offender_race, useNA = "always")

## Replacing empty factor level in offender ethnicity with "Unknown"
hate_crime$offender_ethnicity[hate_crime$offender_ethnicity == ""] <- "Unknown"
hate_crime$offender_ethnicity <- droplevels(
  hate_crime$offender_ethnicity,
  exclude = ""
)
table(hate_crime$offender_ethnicity, useNA = "always")

## Selecting variables relevant to our research questions. Deleting 1% NA's.
hate_crime_used <- hate_crime %>%
  select(
    data_year, agency_type_name, state_name, division_name,
    region_name, population_group_desc, total_offender_count,
    offender_race, victim_count, offense_name, total_individual_victims,
    bias_desc, victim_types, multiple_offense, multiple_bias
  ) %>%
  drop_na()

## Checking structure and dimensions of the selected dataset
str(hate_crime_used)
dim(hate_crime_used)

## Level the groups of victim count to victim_category:"one victim" (1),
## "groups" (2). The new variable "victim_category" is a binary variable made 
## to anwser to the first research question. 
hate_crime_used$victim_category <- cut(
  hate_crime_used$victim_count,
  c(0, 1, 201)
)
levels(hate_crime_used$victim_category) <- c("one victim", "groups")
table(hate_crime_used$victim_category)

## EDA -------------------------------------------------------------------------

## Hate crimes in the US over the last decade by State
hate_crime_used %>%
  filter(data_year > 2009) %>%
  group_by(state_name) %>%
  summarise(crimes_per_state = n(), .groups = "keep") %>%
  ggplot(aes(
    x = factor(state_name, state_name[order(crimes_per_state)]),
    y = crimes_per_state
  )) +
  geom_bar(stat = "identity") +
  labs(x = "State", y = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) +
  ggtitle("Hate crimes in the US over the last decade") +
  coord_flip()

## Hate crimes by US region over the last decade
hate_crime_used %>%
  filter(data_year > 2009) %>%
  group_by(region_name) %>%
  summarise(crimes_per_region = n(), .groups = "keep") %>%
  ggplot(aes(
    x = factor(region_name, region_name[order(desc(crimes_per_region))]),
    y = crimes_per_region
  )) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "count") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) +
  ggtitle("Hate crimes by US region over the last decade")

## Trend per Region
hate_crime_used %>%
  filter(data_year > 2009) %>%
  ggplot(aes(x = factor(data_year), group = region_name, color = region_name)) +
  geom_line(stat = "count") +
  ggtitle("Hate crimes' trends per US region over the last decade") +
  labs(x = "years", y = "count", colour = "region") +
  theme(
    legend.key.size = unit(0.5, "cm"),
    axis.text.x = element_text(size = 8)
  )

## West trendline of multiple and single offenses over the last decade
hate_crime_used %>%
  filter(data_year > 2009) %>%
  filter(region_name == "West") %>%
  ggplot(aes(
    x = factor(data_year), group = multiple_offense, color = multiple_offense
  )) +
  geom_line(stat = "count") +
  labs(x = "years", colour = "multiple_offense") +
  scale_color_discrete(labels = c("Multiple", "Single")) +
  ggtitle("West multiple and single offenses trends") +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "black", size = 0.05),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

## Trend of number of victims per year in the West
hate_crime_used %>%
  filter(region_name == "West") %>%
  filter(data_year > 2009) %>%
  ggplot(aes(factor(data_year), victim_count)) +
  geom_bar(stat = "identity") +
  labs(x = "years", y = "victims") +
  ggtitle("West victims over the last decade")

## Showing the distribution of offenses per Offender Race
hate_crime_used %>%
  filter(region_name == "West") %>%
  filter(data_year > 2009) %>%
  ggplot(aes(x = offender_race)) +
  geom_bar() +
  ggtitle(" offenses count by offender race") +
  labs(y = "offenses") +
  scale_x_discrete(labels = c(
    "American Indian", "Asian", "Black/African", "Multiple", "Haiwaiian",
    "Unknown", "White"
  )) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8)
  )

## Modeling---------------------------------------------------------------------

## Sample of 10% of data for the West. This was chosen because
## the categories had not enough observations for each group if the sample
## would have been 20%, and it wont run because of too much data if we would not
## have taken a sample.
set.seed(1)
crime_frac <- hate_crime_used %>%
  filter(region_name == "West") %>%
  sample_frac(size = 0.1, replace = FALSE)

## Modeling: first research question--------------------------------------------

## Generating train and test set keeping the outcome "victim_category" variable
## equally distributed across train and test set.
set.seed(1)
trn_index <- createDataPartition(
  y = crime_frac$victim_category, p = 0.70, list = FALSE
)
trn_crime <- crime_frac[trn_index, ]
tst_crime <- crime_frac[-trn_index, ]

## Making sure victim_category has "one victim" as reference group
trn_victims <- trn_crime %>%
  mutate(victim_category = relevel(factor(victim_category),
    ref = "one victim"
  ))

## Fitting logistic regression to predict victim_category over population_group,
## offender_race, offense_name and multiple offense as they are the most
## relevant variables to answer to our research question
set.seed(1)
victim_category_lgr <- train(victim_category ~ population_group_desc +
  offender_race + offense_name + multiple_offense,
method = "glm", data = trn_victims,
family = binomial(link = "logit"),
trControl = trainControl(method = "cv", number = 5)
)

## Check the summary object
summary(victim_category_lgr$results)
summary(victim_category_lgr$trainingData)
summary(victim_category_lgr$resample)
summary(victim_category_lgr$finalModel)
coef(summary(victim_category_lgr))[, "Pr(>|z|)"]

## Create a confusionMatrix for `victim_category_lgr`
set.seed(1)
pred_victim_category_lgr <- predict(victim_category_lgr, tst_crime)
lgr_confM <- confusionMatrix(
  pred_victim_category_lgr,
  as.factor(tst_crime$victim_category)
)
lgr_confM

## Run a KNN model using the same variables to predict victim_category, in order
## to compare results with the previously run Logistic Regression
set.seed(1)
victim_category_knn <- train(victim_category ~ population_group_desc +
  offender_race + offense_name + multiple_offense,
method = "knn", data = trn_victims,
trControl = trainControl(method = "cv", number = 5),
preProcess = c("center", "scale")
)
victim_category_knn$results
victim_category_knn$resample
victim_category_knn$bestTune

## Confusion matrix for `victim_category_knn`
pred_victim_category_knn <- predict(victim_category_knn, tst_crime)
pred_victim_category_knn[1:10]
knn_confM <- confusionMatrix(
  pred_victim_category_knn, as.factor(tst_crime$victim_category)
)
knn_confM

## Fitting a Random Forest model to predict vicitm_category using the same
## variables in order to compare results with Logistic Regression and KNN
set.seed(1)
rf_victim_category <- train(victim_category ~ population_group_desc +
  offender_race + offense_name + multiple_offense,
method = "ranger", data = trn_victims,
trControl = trainControl(method = "cv", number = 5),
preProcess = c("center", "scale")
)
rf_victim_category$results

## Confusion matrix for `rf_victim_category`
rf_victim_category_pred <- predict(rf_victim_category, tst_crime)
rf_victim_category_pred[1:10]
rf_confM <- confusionMatrix(
  rf_victim_category_pred,
  as.factor(tst_crime$victim_category)
)
rf_confM

## Modeling: Second research question-------------------------------------------

## Splitting a train and test set from our sample keeping the outcome
## "multiple_offense" equally distributed across train and test set
set.seed(1)
trn_index_2 <- createDataPartition(
  y = crime_frac$multiple_offense, p = 0.70, list = FALSE
)
trn_crime_2 <- crime_frac[trn_index_2, ]
tst_crime_2 <- crime_frac[-trn_index_2, ]

## Making sure multiple_offense has "M" (multiple offenses) as reference group
trn_victims_2 <- trn_crime_2 %>%
  mutate(multiple_offense = relevel(factor(multiple_offense),
    ref = "M"
  ))

## Fitting a Logistic Regression model to predict multiple_offense over
## population_group_desc, offender_race, offense_name, and victim_category as
## they are the most relevant variables for second research question
set.seed(1)
MO_lgr <- train(multiple_offense ~ population_group_desc +
  offender_race + offense_name + victim_category,
method = "glm",
family = binomial(link = "logit"), data = trn_victims_2,
trControl = trainControl(method = "cv", number = 5)
)

## Check the summary object
summary(MO_lgr$results)
summary(MO_lgr$trainingData)
summary(MO_lgr$resample)
summary(MO_lgr$finalModel)
coef(summary(MO_lgr))[, "Pr(>|z|)"]

## Create a confusionMatrix for `MO_lgr`
set.seed(1)
pred_MO_lgr <- predict(MO_lgr, tst_crime)
lgr_confM_2 <- confusionMatrix(
  pred_MO_lgr,
  as.factor(tst_crime$multiple_offense)
)
lgr_confM_2

## Run a KNN to predict multiple_offense over the same variables to compare
## results with the previously fitted Logistic Regression
set.seed(1)
MO_knn <- train(multiple_offense ~ population_group_desc +
  offender_race + offense_name + victim_category,
method = "knn", data = trn_victims_2,
trControl = trainControl(method = "cv", number = 5),
preProcess = c("center", "scale")
)

## Check the summary object for KNN
MO_knn$results
MO_knn$resample
MO_knn$bestTune

## Confusion matrix for `MO_knn`
pred_MO_knn <- predict(MO_knn, tst_crime)
pred_MO_knn[1:10]
knn_confM_2 <- confusionMatrix(
  pred_MO_knn,
  as.factor(tst_crime$multiple_offense)
)
knn_confM_2

## Fitting a Random Forest model to predict multiple_offense based on the same
## variables we used with the Logistic Regression and KNN to comapre results
set.seed(1)
rf_MO <- train(multiple_offense ~ population_group_desc +
  offender_race + offense_name + victim_category,
method = "ranger", data = trn_victims_2,
trControl = trainControl(method = "cv", number = 5),
preProcess = c("center", "scale")
)

## Check the summary object
rf_MO$results
rf_MO$resample
rf_MO$bestTune

## Confusion matrix for `rf_MO`
rf_MO_pred <- predict(rf_MO, tst_crime)
rf_MO_pred[1:10]
rf_confM_2 <- confusionMatrix(
  rf_MO_pred,
  as.factor(tst_crime$multiple_offense)
)
rf_confM_2

## Evaluation: a general model comparison by Accuracy---------------------------

## Difference between glm, KNN, RF models: first research question
cbind(
  lgr_1 = max(victim_category_lgr$results$Accuracy),
  knn_1 = max(victim_category_knn$results$Accuracy),
  rf_1 = max(rf_victim_category$results$Accuracy)
)

## Difference between glm, KNN, RF models: second research question
cbind(
  lgr_2 = max(MO_knn$results$Accuracy),
  knn_2 = max(MO_lgr$results$Accuracy),
  rf_2 = max(rf_MO$results$Accuracy)
)
