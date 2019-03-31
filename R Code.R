quartz()

# define used libraries
libraries_used <- 
  c("lazyeval", "readr","plyr" ,"tidyverse", "dplyr", "readxl", "ggplot2", 
    "funModeling", "scales", "tidyverse", "corrplot", "GGally", "caret",
    "rpart", "randomForest", "pROC", "gbm", "choroplethr", "choroplethrMaps",
    "microbenchmark", "doParallel", "e1071","units","data.table","tidyselect","MASS")
# check missing libraries
libraries_missing <- 
  libraries_used[!(libraries_used %in% installed.packages()[,"Package"])]
# install missing libraries
if(length(libraries_missing)) install.packages(libraries_missing)

#setting environment
sessionInfo()
getwd()
basedir='/Users/harishkumar/Apps/RProgramming/Capstone/'
setwd(paste0(basedir,"Capstone"))
library(data.table)

# Functin to Merge and read all the data files 
multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread))
}
#Subfolder for the Datafiles.
path_Loan <- "DataFiles"
loans <- multmerge(path_Loan)
class(loans)

# Load the Excel workbook that has metadata about the loans data
library(readxl)
meta_loan_stats <- read_excel("./LCDataDictionary.xlsx", sheet = "LoanStats")
warning()
meta_loans <- funModeling::df_status(loans, print_results = FALSE)
knitr::kable(meta_loans)

library(dplyr)
meta_loans <-
  meta_loans %>%
  mutate(uniq_rat = unique / nrow(loans))

meta_loans %>%
  select(variable, unique, uniq_rat) %>%
  mutate(unique = unique, uniq_rat = scales::percent(uniq_rat)) %>%
  knitr::kable()

loans$int_rate <- as.numeric(gsub('\\%','',trimws(loans$int_rate)))
str(loans$int_rate)
tmp <- loans$emp_length
tmp <- gsub('years','',trimws(tmp))
tmp <- gsub('year','',trimws(tmp))
tmp <- gsub('\\+','',trimws(tmp))
tmp <- gsub(' ','',trimws(tmp))
tmp <- gsub('<','',trimws(tmp))
tmp <- gsub('\\>','',trimws(tmp))
head(tmp,100)
str(tmp)
tmp <- as.numeric(tmp)
summary(tmp)
loans$emp_length_num <- tmp
summary(loans$emp_length_num)  
chr_to_num_vars <- 
  c("annual_inc_joint", "mths_since_last_major_derog", "open_acc_6m", "open_il_12m", "open_il_24m", "mths_since_rcnt_il",
    "total_bal_il", "il_util", "open_rv_12m", "open_rv_24m",
    "max_bal_bc", "all_util", "total_rev_hi_lim", "total_cu_tl",
    "inq_last_12m", "dti_joint", "inq_fi", "tot_cur_bal", "tot_coll_amt")

loans <-
  loans %>%
  mutate_at(.funs = funs(as.numeric), .vars = chr_to_num_vars)

chr_to_date_vars <- 
  c("issue_d", "last_pymnt_d", "last_credit_pull_d",
    "next_pymnt_d", "earliest_cr_line", "next_pymnt_d")
names(loans)
loans %>%
  select_(.dots = chr_to_date_vars) %>%
  str()
head(unique(loans$next_pymnt_d))
for (i in chr_to_date_vars){
  print(head(unique(loans[, i])))
}
meta_loans %>% 
  select(variable, q_na) %>% 
  filter(variable %in% chr_to_date_vars)

convert_date <- function(x){
  as.Date(paste0("01-", x), format = "%d-%b-%Y")
} 

loans <-
  loans %>%
  mutate_at(.funs = funs(convert_date), .vars = chr_to_date_vars)

library(dplyr)
num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()

meta_loans %>%
  select(variable, p_zeros, p_na, unique) %>%
  filter_(~ variable %in% num_vars) %>%
  knitr::kable()


na_to_zero_vars <-
  c("mths_since_last_delinq", "mths_since_last_record",
    "mths_since_last_major_derog","emp_length_num")

loans <- 
  loans %>%
  mutate_at(.vars = na_to_zero_vars, .funs = funs(replace(., is.na(.), 0)))


meta_loans <- funModeling::df_status(loans, print_results = FALSE)
meta_loans <-
  meta_loans %>%
  mutate(uniq_rat = unique / nrow(loans))
knitr::kable(meta_loan_stats[,1:2])

dplyr::setdiff(colnames(loans), meta_loan_stats$LoanStatNew)
dplyr::setdiff(meta_loan_stats$LoanStatNew, colnames(loans))


default_vars <- c("loan_status", "delinq_2yrs", "mths_since_last_delinq")
purrr::map(.x = loans[, default_vars], .f = base::unique)

loans %>%
  group_by(loan_status) %>%
  summarize(count = n(), rel_count = count/nrow(loans)) %>%
  knitr::kable()

defaulted <- 
  c("Default", 
    "Charged Off", 
    "In Grace Period", 
    "Late (16-30 days)", 
    "Late (31-120 days)")

loans <-
  loans %>%
  mutate(default = ifelse(!(loan_status %in% defaulted), FALSE, TRUE))


DefaultRate <-
  loans %>%
  summarise(default_freq = sum(default / n()))

table(loans$default) / nrow(loans)
library(plotrix)

pie3D(table(loans$default) / nrow(loans), labels = c('No Default','Default'+as.character(DefaultRate)))


vars_to_remove <- 
  c("annual_inc_joint", "dti_joint", "policy_code", "id", "member_id",
    "emp_title", "url", "desc", "title", "open_acc_6m", 
    "open_il_12m", "open_il_24m", "mths_since_rcnt_il", "total_bal_il", 
    "il_util", "open_rv_12m", "open_rv_24m", "max_bal_bc", "all_util",
    "total_rev_hi_lim", "inq_fi", "total_cu_tl", "inq_last_12m",
    "verification_status_joint", "next_pymnt_d", "sub_grade", "loan_status",
    "orig_projected_additional_accrued_interest", "hardship_dpd", 
    "hardship_length","hardship_amount", "deferral_term", 
    "sec_app_mths_since_last_major_derog", "sec_app_collections_12_mths_ex_med",
    "sec_app_collections_12_mths_ex_med", "sec_app_chargeoff_within_12_mths",
    "sec_app_num_rev_accts","sec_app_open_act_il", "sec_app_revol_util",
    "sec_app_open_acc", "sec_app_mort_acc", "sec_app_inq_last_6mths",
    "sec_app_fico_range_high", "sec_app_fico_range_low", "revol_bal_joint",
    "mths_since_recent_bc_dlq", "mths_since_recent_revol_delinq","dti_joint",
    "mths_since_last_record", "chargeoff_within_12_mths", "delinq_amnt",
    "acc_now_delinq", "acc_now_delinq","collections_12_mths_ex_med", 
    "collection_recovery_fee", "recoveries", "total_rec_late_fee",
    "pub_rec", "mths_since_last_record", "hardship_payoff_balance_amount", "open_act_il", "hardship_last_payment_amount","settlement_amount","settlement_percentage", "settlement_term")

loans <- loans %>% select(-one_of(vars_to_remove))

names(loans)
#loans$hardship_payoff_balance_amount <- NULL

set.seed(100)

train_index <- 
  caret::createDataPartition(y = loans$default, times = 1, 
                             p = .8, list = FALSE)

train <- loans[train_index, ]
test <- loans[-train_index, ]


# EDA
library(scales)

income_vars <- c("annual_inc")
loan_amount_vars <- c("loan_amnt", "funded_amnt", "funded_amnt_inv")

library(ggplot2)
library(tidyr)
train %>%
  select_(.dots = income_vars) %>%
  gather_("variable", "value", gather_cols = income_vars) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  geom_histogram()


train %>%
  select_(.dots = loan_amount_vars) %>%
  gather_("variable", "value", gather_cols = loan_amount_vars) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  geom_histogram()


categorical_vars <- 
  c("term", "grade", "sub_grade", "emp_title", "home_ownership",
    "verification_status", "loan_status", "purpose", "zip_code",
    "addr_state", "application_type", "policy_code")

meta_loans %>%
  select(variable, p_zeros, p_na, type, unique) %>%
  filter_(~ variable %in% categorical_vars) %>%
  knitr::kable()

give_count <- 
  stat_summary(fun.data = function(x) return(c(y = median(x)*1.06,
                                               label = length(x))),
               geom = "text")


give_mean <- 
  stat_summary(fun.y = mean, colour = "darkgreen", geom = "point", 
               shape = 18, size = 3, show.legend = FALSE)

train %>%
  ggplot(aes(grade, loan_amnt)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  give_count +
  give_mean +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ default) +
  labs(title="Loan Amount by Grade", x = "Grade", y = "Loan Amount \n")

head(loans$int_rate)

train %>%
  ggplot(aes(grade, int_rate)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  give_count +
  give_mean +
  scale_y_continuous(labels = comma) +
  labs(title="Interest Rate by Grade", x = "Grade", y = "Interest Rate \n") +
  facet_wrap(~ term)#

train %>%
  ggplot(aes(home_ownership, int_rate)) +
  geom_boxplot(fill = "white", colour = "darkblue", 
               outlier.colour = "red", outlier.shape = 1) +
  give_count +
  give_mean +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ default) +
  labs(title="Interest Rate by Home Ownership", x = "Home Ownership", y = "Interest Rate \n")


funded_amnt <-
  train %>%
  transmute(loan_amnt = loan_amnt, value = funded_amnt, 
            variable = "funded_amnt")

funded_amnt_inv <-
  train %>%
  transmute(loan_amnt = loan_amnt, value = funded_amnt_inv, 
            variable = "funded_amnt_inv")

plot_data <- rbind(funded_amnt, funded_amnt_inv)
# remove unnecessary data using regex
ls()
rm(list = ls()[grep("^funded", ls())])

plot_data %>%
  ggplot(aes(x = loan_amnt, y = value)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  geom_point()

train %>%
  ggplot(aes(x = annual_inc, y = loan_amnt)) +
  geom_point()

train %>%
  select(int_rate, grade) %>%
  group_by(grade) %>%
  summarise(int_rate_mean = mean(int_rate, na.rm = TRUE),
            int_rate_median = median(int_rate, na.rm = TRUE),
            n = n()) %>%
  knitr::kable()

train %>%
  select(int_rate, grade, issue_d) %>%
  group_by(grade, issue_d) %>%
  summarise(int_rate_mean = mean(int_rate, na.rm = TRUE)) %>%
  ggplot(aes(issue_d, int_rate_mean)) +
  geom_line(color= "darkblue", size = 1) +
  facet_wrap(~ grade)

train %>%
  select(loan_amnt, grade, issue_d) %>%
  group_by(grade, issue_d) %>%
  summarise(loan_amnt_mean = mean(loan_amnt, na.rm = TRUE)) %>%
  ggplot(aes(issue_d, loan_amnt_mean)) +
  geom_line(color= "darkblue", size = 1) +
  facet_wrap(~ grade)

geo_vars <- c("zip_code", "addr_state")

meta_loans %>%
  select(variable, p_zeros, p_na, type, unique) %>%
  filter_(~ variable %in% geo_vars) %>%
  knitr::kable()

loans %>%
  select_(.dots = geo_vars) %>%
  str()

default_rate_state <- 
  train %>%
  select(default, addr_state) %>%
  group_by(addr_state) %>%
  summarise(default_rate = sum(default, na.rm = TRUE) / n())

knitr::kable(default_rate_state)

library(choroplethrMaps)
utils::data(state.map)
str(state.map)
unique(state.map$region)
getwd()
states <- read.csv("./Fips.csv")
states <- states[1:3]
states$`FIPS State Numeric Code`<- as.integer(states$`FIPS State Numeric Code`)

str(states)
names(states) <- c("name", "fips_code", "usps_code")
dplyr::setdiff(default_rate_state$addr_state, states$usps_code)

names(default_rate_state)

default_rate_state <-
  default_rate_state %>%
  left_join(states[, c("usps_code", "name")], 
            by = c("addr_state" = "usps_code")) %>%
  rename(region = name, value = default_rate) %>%
  mutate(region = tolower(region)) %>%
  select(region, value)
choroplethr::state_choropleth(df = default_rate_state, 
                              title = "Default rate by State")
#install.packages("units")



num_vars <- 
  train %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()

meta_train <- funModeling::df_status(train, print_results = FALSE)

meta_train %>%
  select(variable, p_zeros, p_na, unique) %>%
  filter_(~ variable %in% num_vars) %>%
  knitr::kable()


#need to check#
library(corrplot)
corrplot(cor(train[, num_vars], use = "complete.obs"), 
                   method = "pie", type = "upper")
corrplot(cor(train[,num_vars]), 
         type = "upper", method = "pie")

caret::findCorrelation(cor(train[, num_vars]), 
                       names = TRUE, cutoff = .5)


vars_to_remove <- 
  c("loan_amnt", "funded_amnt", "funded_amnt_inv", "installment",
    "total_pymnt_inv", "total_rec_prncp", "mths_since_last_delinq", 
    "out_prncp", "total_pymnt", "total_rec_int", "total_acc",
    "mths_since_last_record", "recoveries")

train <- train %>% select(-one_of(vars_to_remove))



library(GGally)
plot_ggpairs <-
  train %>%
  select(annual_inc, term, grade, default) %>%
  mutate(default = as.character(default)) %>%
  ggpairs()

plot_ggpairs



num_vars <- train %>% sapply(is.numeric) %>% which() %>% names()
gc()
train %>%
  select_(.dots = num_vars) %>%
  gather(measure, value) %>%
  mutate(default = factor(rep(x = train$default, 
                              length.out = length(num_vars) * dim(train)[1]), 
                          levels = c("TRUE", "FALSE"))) %>%
  ggplot(data = ., aes(x = value, fill = default, 
                       color = default, order = -default)) +
  geom_density(alpha = 0.3, size = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap( ~ measure, scales = "free", ncol = 3)


winsor_outlier <- function(x, cutoff = .95, na.rm = FALSE){
  quantiles <- quantile(x, cutoff, na.rm = na.rm)
  x[x > quantiles] <- quantiles
  x
}

train %>%
  select_(.dots = num_vars) %>%
  mutate_all(.funs = winsor_outlier, cutoff = .95, na.rm = TRUE) %>%
  gather(measure, value) %>%
  mutate(default = factor(rep(x = train$default, 
                              length.out = length(num_vars)*dim(train)[1]), 
                          levels = c("TRUE", "FALSE"))) %>%
  ggplot(data = ., aes(x = value, fill = default, 
                       color = default, order = -default)) +
  geom_density(alpha = 0.3, size = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  facet_wrap( ~ measure, scales = "free", ncol = 3)


train %>% 
  select(default, out_prncp_inv) %>% 
  filter(out_prncp_inv == 0) %>%
  group_by(default) %>% 
  summarize(n = n())




train_down <- 
  caret::downSample(x = train[, !(names(train) %in% c("default"))], 
                    y = as.factor(train$default), yname = "default")

base::prop.table(table(train_down$default))

names(caret::getModelInfo())

vars_to_mutate <-
  train_down %>%
  select(which(sapply(.,is.character))) %>%
  names()

vars_to_mutate
train_down <-
  train_down %>%
  mutate_at(.funs = make.names, .vars = vars_to_mutate)

test <-
  test %>%
  mutate_at(.funs = make.names, .vars = vars_to_mutate)


vars_to_remove <- c("zip_code")
train_down <- train_down %>% select(-one_of(vars_to_remove))

library(caret)
# train
dummies_train <-
  dummyVars("~.", data = train_down[, !(names(train_down) %in% c("default"))], 
            fullRank = FALSE)

train_down_dummy <-
  train_down %>%
  select(-which(sapply(.,is.character))) %>%
  cbind(predict(dummies_train, newdata = train_down))

# test
dummies_test <-
  dummyVars("~.", data = test[, dummies_train$vars], fullRank = FALSE)

test_dummy <-
  test %>%
  select(one_of(colnames(train_down))) %>%
  select(-which(sapply(.,is.character))) %>%
  cbind(predict(dummies_test, newdata = test))




model_glm_1 <- glm(formula = default ~ grade+term+int_rate+emp_length_num, 
                   family = binomial(link = "logit"), 
                   data = train_down, na.action = na.exclude)
class(model_glm_1)
attributes(model_glm_1)
summary(model_glm_1)
summary(model_glm_1)$coef
model_glm_1_pred <- 
  predict.glm(object = model_glm_1, newdata = test, type = "response")
model_pred_t <- function(pred, t) as.factor(ifelse(pred > t, TRUE, FALSE))
caret::confusionMatrix(data = model_pred_t(model_glm_1_pred, 0.5), 
                       reference = test$default,
                       positive = "TRUE")
test$default <- as.factor(test$default)
class(model_pred_t)
levels(test$default)
class(test$default)
class(model_glm_1)
data <- model_pred_t(model_glm_1,0.5)
model_glm_1
levels(test$default)
class(test$default)
head(test$default)
str(test$default)

names(train_down)
attach(train_down)
model_glm_2 <- glm(formula = default ~ grade+annual_inc, 
                   family = binomial(link = "logit"), 
                   data = train_down, na.action = na.exclude)

model_glm_2_pred <- 
  predict.glm(object = model_glm_2, newdata = test, type = "response")
confusionMatrix(data = model_pred_t(model_glm_2_pred, 0.5), 
                reference = test$default,
                positive = "TRUE")




plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$actual == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$actual == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$actual == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$actual == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data = df, aes(x = actual, y = pred)) + 
    geom_violin(fill = rgb(1, 1 ,1, alpha = 0.6), color = NA) + 
    geom_jitter(aes(color = pred_type), alpha = 0.6) +
    geom_hline(yintercept = threshold, color = "red", alpha = 0.6) +
    scale_color_discrete(name = "type") +
    labs(title = sprintf("Threshold at %.2f", threshold))
}

plot_pred_type_distribution(
  df = tibble::tibble(pred = model_glm_1_pred, actual = test$default), 
  threshold = 0.5)

roc_glm_1 <- pROC::roc(response = test$default, predictor = model_glm_1_pred)
roc_glm_1

pROC::plot.roc(x = roc_glm_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               col = "green", print.auc = FALSE, print.auc.y = .4)

legend(x = "bottomright", legend=c("glm_1 AUC = 0.681"), 
       col = c("green"), lty = 1, cex = 1.0)

full_vars <- colnames(train_down)
full_vars

model_vars <-
  c("term", "grade", "emp_length_num", "home_ownership", "annual_inc",
    "purpose", "pymnt_plan", "delinq_2yrs", "default","int_rate","issue_d","dti","out_prncp_inv")
train_down$out_prncp_inv

ignored_vars <- dplyr::setdiff(full_vars, model_vars)
ignored_vars

ctrl <- 
  trainControl(method = "repeatedcv", 
               number = 10,
               repeats = 5,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               savePredictions = TRUE,
               verboseIter = FALSE,
               allowParallel = TRUE)

model_glm_2 <-
  train_down %>%
  select(model_vars) %>%
  mutate(default = as.factor(ifelse(default == TRUE, "yes", "no"))) %>%
  filter(complete.cases(.)) %>%
  train(default ~ ., 
        data = ., 
        method = "glm", 
        family = "binomial",
        metric = "ROC",
        trControl = ctrl)

model_glm_2
attributes(model_glm_2)
summary(model_glm_2)
predictors(model_glm_2)
varImp(model_glm_2)
plot(varImp(model_glm_2))

test$term <- as.factor(test$term)
class(test$term)
model_glm_2_pred <- 
  predict(model_glm_2, 
          newdata = test %>% 
            select(model_vars) %>%
            mutate(default = as.factor(ifelse(default == TRUE, 
                                              "yes", "no"))) %>%
            filter(complete.cases(.)), 
          type = "prob")

head(model_glm_2_pred[1], 2)
head(test$default, 2)
table(model_glm_2_pred[2],complete.cases(test[,default]))
dim(model_glm_2_pred)
dim(complete.cases(test[,"default"]))
plot(model_glm_2_pred[2])
#install.packages("popbio")
library(popbio)
logi.hist.plot(out_prncp_inv,as.logical(default),boxp=FALSE,type="hist",col="gray")
caret::confusionMatrix(
  data = as.factor(ifelse(model_glm_2_pred[, "yes"] > 0.5, "yes", "no")), 
  reference = as.factor(ifelse(test[complete.cases(test[, model_vars]), 
                                    "default"] == TRUE, "yes", "no")),
  positive = "yes")

temp <- as.factor(ifelse(test[complete.cases(test[, model_vars]),
                              "default"] == TRUE, "yes", "no"))

roc_glm_2 <- 
  pROC::roc(response = temp, 
            predictor = model_glm_2_pred[, "yes"])

roc_glm_2
pROC::plot.roc(x = roc_glm_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA, 
               col = "green")

pROC::plot.roc(x = roc_glm_2, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "blue")

legend(x = "bottomright", legend=c("glm_1 AUC = 0.68", "glm_2 AUC = 0.73"), 
       col = c("green", "blue"), lty = 1, cex = 1.0)

ctrl <- 
  trainControl(method = "repeatedcv", 
               number = 5,
               repeats = 2,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = FALSE,
               allowParallel = TRUE)

 library(rpart)

model_rpart <-
  train_down %>%
  select(model_vars) %>%
  mutate(default = as.factor(ifelse(default == TRUE, "yes", "no"))) %>%
  filter(complete.cases(.)) %>%
  train(default ~ .,
        data = .,
        method = 'rpart',
        metric = "ROC",
        preProc = c("center", "scale"),
        trControl = ctrl)

model_rpart
ggplot(model_rpart)
plot(model_rpart$finalModel, uniform = TRUE, margin = 0.2)
rattle::fancyRpartPlot(model_rpart$finalModel)
graphics::text(model_rpart$finalModel)




ctrl <- 
  trainControl(method = "repeatedcv", 
               number = 5,
               repeats = 2,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = FALSE,
               allowParallel = TRUE,
               search = "random")

model_rpart <-
  train_down %>%
  select(model_vars) %>%
  mutate(default = as.factor(ifelse(default == TRUE, "yes", "no"))) %>%
  filter(complete.cases(.)) %>%
  train(default ~ .,
        data = .,
        method = 'rpart',
        metric = "ROC",
        preProc = c("center", "scale"),
        trControl = ctrl)

model_rpart
plot(model_rpart$finalModel, uniform = TRUE, margin = 0.1)
rattle::fancyRpartPlot(model_rpart$finalModel)
graphics::text(model_rpart$finalModel, cex = 0.5)

model_rpart_pred <- 
  predict(model_rpart, 
          newdata = test %>% 
            select(model_vars) %>%
            mutate(default = as.factor(ifelse(default == TRUE, 
                                              "yes", "no"))) %>%
            filter(complete.cases(.)), 
          type = "prob")

caret::confusionMatrix(
  data = as.factor(ifelse(model_rpart_pred[, "yes"] > 0.5, "yes", "no")), 
  reference = as.factor(ifelse(test[complete.cases(test[, model_vars]), 
                                    "default"] == TRUE, "yes", "no")),
  positive = "yes")
test$default <- as.factor(test$default)
roc_rpart <- 
  pROC::roc(response = temp, 
            predictor = model_rpart_pred[, "yes"])

roc_rpart

pROC::plot.roc(x = roc_glm_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               col = "green")

pROC::plot.roc(x = roc_glm_2, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "blue")

pROC::plot.roc(x = roc_rpart, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "orange")


legend(x = "bottomright", legend=c("glm_1 AUC = 0.68", "glm_2 AUC = 0.73",
                                   "rpart AUC = 0.75"), 
       col = c("green", "blue", "orange"), lty = 1, cex = 1.0)



 library(randomForest)

ctrl <- 
  trainControl(method = "repeatedcv", 
               number = 5,
               repeats = 1,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = FALSE,
               allowParallel = TRUE)

model_rf <-
  train_down %>%
  select(model_vars) %>%
  mutate(default = as.factor(ifelse(default == TRUE, "yes", "no"))) %>%
  filter(complete.cases(.)) %>%
  train(default ~ .,
        data = .,
        method = 'rf',
        ntree = 10,
        metric = "ROC",
        preProc = c("center", "scale"),
        trControl = ctrl)

model_rf
plot(model_rf$finalModel)

model_rf_pred <- 
  predict(model_rf, 
          newdata = test %>% 
            select(model_vars) %>%
            mutate(default = as.factor(ifelse(default == TRUE, 
                                              "yes", "no"))) %>%
            filter(complete.cases(.)), 
          type = "prob")

caret::confusionMatrix(
  data = as.factor(ifelse(model_rf_pred[, "yes"] > 0.5, "yes", "no")), 
  reference = as.factor(ifelse(test[complete.cases(test[, model_vars]), 
                                    "default"] == TRUE, "yes", "no")),
  positive = "yes")


test$default <- as.factor(test$default)
roc_rf <- 
  pROC::roc(response = temp, 
            predictor = model_rf_pred[, "yes"])

roc_rf




 library(gbm)

ctrl <- 
  trainControl(method = "repeatedcv", 
               number = 5,
               repeats = 1,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = FALSE,
               allowParallel = TRUE)

model_gbm_1 <- 
  train_down %>%
  select(model_vars) %>%
  mutate(default = as.factor(ifelse(default == TRUE, "yes", "no"))) %>%
  filter(complete.cases(.)) %>%
  train(default ~ ., 
        data = ., 
        method = "gbm",
        metric = "ROC",
        trControl = ctrl,
        preProc = c("center", "scale"),
        ## This last option is actually one
        ## for gbm() that passes through
        verbose = FALSE)

model_gbm_1
ggplot(model_gbm_1)
model_gbm_1_pred <- 
  predict(model_gbm_1, 
          newdata = test %>% 
            select(model_vars) %>%
            mutate(default = as.factor(ifelse(default == TRUE, 
                                              "yes", "no"))) %>%
            filter(complete.cases(.)), 
          type = "prob")

caret::confusionMatrix(
  data = as.factor(ifelse(model_gbm_1_pred[, "yes"] > 0.5, "yes", "no")), 
  reference = as.factor(ifelse(test[complete.cases(test[, model_vars]), 
                                    "default"] == TRUE, "yes", "no")),
  positive = "yes")
roc_gbm_1 <- 
  pROC::roc(response = temp, 
            predictor = model_gbm_1_pred[, "yes"])

roc_gbm_1

pROC::plot.roc(x = roc_glm_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               col = "green")

pROC::plot.roc(x = roc_glm_2, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "blue")

pROC::plot.roc(x = roc_rpart, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "orange")

pROC::plot.roc(x = roc_gbm_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "brown")

legend(x = "bottomright", legend=c("glm_1 AUC = 0.68", "glm_2 AUC = 0.73",
                                   "rpart AUC = 0.75", "gbm AUC = 0.76"), 
       col = c("green", "blue", "orange", "brown"), lty = 1, cex = 1.0)


ctrl <- 
  trainControl(method = "repeatedcv", 
               number = 10,
               repeats = 5,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = FALSE,
               allowParallel = TRUE,
               search = "random")
model_lda_1 <- 
  train_down %>%
  select(model_vars) %>%
  mutate(default = as.factor(ifelse(default == TRUE, "yes", "no"))) %>%
  filter(complete.cases(.)) %>%
  train(default ~ ., 
        data = ., 
        method = "lda",
        metric = "ROC",
        trControl = ctrl,
        preProc = c("center", "scale"),
        ## This last option is actually one
        ## for gbm() that passes through
        verbose = FALSE)
model_lda_1
summary(model_lda_1)
model_lda_1_pred <- 
  predict(model_lda_1, 
          newdata = test %>% 
            select(model_vars) %>%
            mutate(default = as.factor(ifelse(default == TRUE, 
                                              "yes", "no"))) %>%
            filter(complete.cases(.)), 
          type = "prob")

caret::confusionMatrix(
  data = as.factor(ifelse(model_lda_1_pred[, "yes"] > 0.5, "yes", "no")), 
  reference = as.factor(ifelse(test[complete.cases(test[, model_vars]), 
                                    "default"] == TRUE, "yes", "no")),
  positive = "yes")
varImp(model_glm_2)


roc_lda_1 <- 
  pROC::roc(response = temp, 
            predictor = model_lda_1_pred[, "yes"])

roc_lda_1
pROC::plot.roc(x = roc_glm_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA, 
               col = "green")

pROC::plot.roc(x = roc_glm_2, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "blue")

pROC::plot.roc(x = roc_lda_1, legacy.axes = FALSE, xlim = c(1, 0), asp = NA,
               add = TRUE, col = "black")

legend(x = "bottomright", legend=c("glm_1 AUC = 0.68", "glm_2 AUC = 0.73", "lda_1 AUC = .72"), 
       col = c("green", "blue", "black"), lty = 1, cex = 1.0)


model_vars
names(train_down)



ctrl <- 
  trainControl(method = "repeatedcv", 
               number = 5,
               repeats = 2,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = FALSE,
               allowParallel = TRUE,
               search = "random")
model_ada_1 <- 
  train_down %>%
  select(model_vars) %>%
  mutate(default = as.factor(ifelse(default == TRUE, "yes", "no"))) %>%
  filter(complete.cases(.)) %>%
  train(default ~ ., 
        data = ., 
        method = "ada",
        metric = "ROC",
        trControl = ctrl,
        preProc = c("center", "scale"),
        ## This last option is actually one
        ## for gbm() that passes through
        verbose = FALSE)
model_ada_1
model_ada_1_pred <- 
  predict(model_ada_1, 
          newdata = test %>% 
            select(model_vars) %>%
            mutate(default = as.factor(ifelse(default == TRUE, 
                                              "yes", "no"))) %>%
            filter(complete.cases(.)), 
          type = "prob")

caret::confusionMatrix(
  data = as.factor(ifelse(model_ada_1_pred[, "yes"] > 0.5, "yes", "no")), 
  reference = as.factor(ifelse(test[complete.cases(test[, model_vars]), 
                                    "default"] == TRUE, "yes", "no")),
  positive = "yes")