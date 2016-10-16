library(tidyverse)

raw_data <- read_csv(file="raw_data.csv")
#sex is num, and it's a categorical variable, 1=male and 2=female


raw_data$sex <- as.factor(raw_data$sex)
levels(raw_data$sex) <- list("Male"=1,"Female"=2)

#now we want to split up the data 
neg_affect_items <- select(raw_data, afraid, angry, anxious, ashamed)
pos_affect_items <- select(raw_data, delighted, elated, enthusiastic, excited)
View(neg_affect_items)
View(pos_affect_items)
Neuroticism <- select(raw_data, Neuroticism)
Extraversion <- select(raw_data, Extraversion)
View(Neuroticism)
View(Extraversion)
sex <- select(raw_data, sex)

#we're told that adjectives range in score from 0-3, and neuroticism and extraversion range from 0-24
#let's check for out of range values
psych::describe(neg_affect_items)
#have to fix, out of range values
psych::describe(pos_affect_items)
#all good
psych::describe(Neuroticism)
#all good
psych::describe(Extraversion)
#all good

#have to fix na_affect
is_bad_value <- neg_affect_items<0 | neg_affect_items>3
neg_affect_items[is_bad_value] <-NA
View(neg_affect_items)

psych::describe(pos_affect_items)
is_bad_value <- pos_affect_items<0 | pos_affect_items>3
pos_affect_items[is_bad_value] <- NA
# View(pos_affect_items)

is_bad_value <- Neuroticism<0 | Neuroticism>24
Neuroticism[is_bad_value] <- NA

is_bad_value <- Extraversion<0 | Extraversion>24
Extraversion[is_bad_value] <- NA

## To obtain scale scores for the positive and negative affect items (combines them):
pos_affect <- psych::alpha(as.data.frame(pos_affect_items),check.keys=FALSE)$scores
neg_affect <- psych::alpha(as.data.frame(neg_affect_items),check.keys=FALSE)$scores

#now we need to create results for overall data, just males, and just females
#3 data sets with the names analytic_data, analytic_data_male, and analytic_data_female

#analytic data set overall
analytic_data <- cbind(sex, pos_affect, neg_affect, Neuroticism, Extraversion)
View(analytic_data)
#save it
write_csv(analytic_data,path="analytic_data.csv")

#analytic data set male
analytic_data.male <- filter(analytic_data, sex=="Male")
analytic_data_male <- select(analytic_data.male, pos_affect, neg_affect, Neuroticism, Extraversion)
View(analytic_data_male)
#save it
write_csv(analytic_data_male,path="analytic_data_male.csv")

#analytic data set female
analytic_data.female <- filter(analytic_data, sex=="Female")
analytic_data_female <- select(analytic_data.female, pos_affect,neg_affect, Neuroticism, Extraversion)
View(analytic_data_female)
#save it
write_csv(analytic_data_female,path="analytic_data_female.csv")










