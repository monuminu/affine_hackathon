library(dplyr)
library(ggplot2)
library(sqldf)

acc = read.csv("Dataset/training dataset/Accounts.csv")
acc_prop = read.csv("Dataset/training dataset/Accounts_properties.csv")
deal_prop = read.csv("Dataset/training dataset/Deals_to_Properties.csv")
Opp = read.csv("Dataset/training dataset/Opportunities.csv")
Opp$id_deals.1 = NULL
prop = read.csv("Dataset/property dataset/Properties.csv")
prop$id_deals = NULL
acc_prop$id_deals = NULL

acc_deal = sqldf("select ap.id_accs ,ap.id_props,dp.id_deals from acc_prop ap left outer join deal_prop dp
                  on (ap.id_props = dp.id_props)")


prop$id_deals = trimws(prop$id_deals)
acc_prop$id_deals = trimws(acc_prop$id_deals)
a = sqldf("select * from prop ad inner join acc_prop a on ad.id_deals = a.id_deals")

deal_accepted = sqldf("select * from Opp o left outer join deal_prop dp on
                        o.id_deals = dp.id_deals")

acc_prop = sqldf("select * from Opp o left outer join deal_prop dp on
                        o.id_deals = dp.id_deals")

View(acc %>% filter(id_accs == "0012A000023YLW0QAO"))

deal_accepted$deal_accepted = ifelse(is.na(deal_accepted$id_props),FALSE,TRUE)
deal_accepted[,c(16,17,18)] = NULL
Opp = unique(deal_accepted)

df = sqldf("select * from deal_accepted o left outer join acc a on o.id_accs = a.id_accs")


dff = sqldf("select * from df d inner join prop p on d.id_deals = p.id_deals")
df$id_accs = NULL
df$id_accs = NULL
df$id_deals = NULL
df$id_deals = NULL
df$id_props = NULL
df$id_props = NULL










acc = read.csv("Dataset/training dataset/Accounts.csv")
acc_prop = read.csv("Dataset/training dataset/Accounts_properties.csv")
deal_prop = read.csv("Dataset/training dataset/Deals_to_Properties.csv")
Opp = read.csv("Dataset/training dataset/Opportunities.csv")
Opp$id_deals.1 = NULL
prop = read.csv("Dataset/property dataset/Properties.csv")
prop$id_deals = NULL
acc_prop$id_deals = NULL


df = sqldf("select o.*,dp.id_props from Opp o inner join deal_prop dp on
                        o.id_deals = dp.id_deals")

df = sqldf("select o.*,dp.id_props as id_propss from df o left outer join acc_prop dp on
                        o.id_props = dp.id_props")

df$Target = ifelse(is.na(df$id_propss),"N","Y")

df$id_propss = NULL

df = sqldf("select * from df o inner join acc a on o.id_accs = a.id_accs")

df$id_accs = NULL
df$id_accs = NULL
df = sqldf("select * from df o inner join prop a on o.id_props = a.id_props")

df$id_props = NULL
df$id_props = NULL

df$id_deals = NULL
df$id_deals = NULL

df$accounting_date = NULL
df$date_closed = NULL
df$closedate = NULL
df$building_tax_expenses = NULL
df$sale_date__c = NULL

source("utilityfunctions.R")
source("metricsComputation.R")

FeatToKeep = df[,!names(df) %in% c("Target","best_initial_bid", "deal_type", "deal_update_flag", "debt_yield", "fiscal", "fiscalquarter",
                                   "fiscalyear", "platform", "property_group", "property_type")]
TFDF0=sapply(FeatToKeep,autodetect_transformations_bnsf)
TFDF=data.frame(t(TFDF0),stringsAsFactors=F)
XTrain=get_ready_for_logistic_bnsf(FeatToKeep,TFDF,train=TRUE)

data = cbind(XTrain, label = df$Target) 

data$label = ifelse(data$label == 1 ,"Y","N")

p=0.8 
N=nrow(data)
set.seed(1951) 
SPTRAIN=sample(1:N,round(N*p),replace=F)

InTrain=(1:N)%in%SPTRAIN

XTR=data[SPTRAIN, ]
LTR=data[SPTRAIN,"label" ]
DFTRAIN=data.frame(Y=LTR,XTR)
DFTRAIN$Y = as.factor(DFTRAIN$Y)
#Test data
DFTRAIN$label = NULL

XTEST=data[-SPTRAIN, ]
LTEST=data[-SPTRAIN,"label" ]

#control <- trainControl(method="repeatedcv", number=10, repeats=3)
#grid <- expand.grid(size=c(5,10,20,50), k=c(1,2,3,4,5))

ctrl <- trainControl(method = "cv",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = FALSE)



grid <- expand.grid(interaction.depth=c(4), # Depth of variable interactions
                    n.trees=c(150, 200),
                    shrinkage=0.10, 
                    n.minobsinnode=c(20,50))	        # Num trees to fit
#											
# set the seed

# Set up to do parallel processing   
#registerDoParallel(4)		# Registrer a parallel backend for train
#getDoParWorkers()



gbm_fit <- train(Y ~ .,
                 data = DFTRAIN,
                 method = "gbm",
                 verbose = TRUE,
                 metric = "ROC",
                 tuneGrid=grid,
                 trControl = ctrl)

#glm_fit=cv.glmnet(y=LTR=='RED',x=as.matrix(XTR),family='binomial')



#Evaluating the test data
#DFTEST =data.frame(Pred=predict(model,newdata=XTEST),Actual=LTEST)
#DFALL  =data.frame(Pred=predict(glm_fit,newx=as.matrix(XT),type = 'response') [,1]   ,Actual=Label, InTrain=InTrain)
DFALL  =data.frame(Pred=predict(gbm_fit,newdata=XTEST,type = 'prob') [,1]   ,Actual=LTEST)

DFALL <- cbind(FeatToKeep, DFALL)
DFALL$Pred = 1 - DFALL$Pred
auc_calc(DFALL$Pred,DFALL$Actual)

df_metrics = computeMetrics(DFALL$Pred,DFALL$Actual)
pred <- prediction(DFALL$Pred,DFALL$Actual)

df_metrics = data.frame(cutoffs = unlist(pred@cutoffs) , 
                        fp = unlist(pred@fp), tp = unlist(pred@tp),
                        tn = unlist(pred@tn), fn = unlist(pred@fn))
df_metrics$f1 = (2*df_metrics$tp)/(2*df_metrics$tp+df_metrics$fp+df_metrics$fn)

df_metrics %>% ggplot(aes(x = cutoffs , y = f1)) +
  geom_point()

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

DFALL$Actual = as.factor(DFALL$Actual)


result <- confusionMatrix(DFALL$Pred,DFALL$Actual)

test =  read.csv("Dataset/Test dataset/Test_data.csv")

test = merge(test,prop)

test = sqldf("select * from test t inner join acc a where t.id_accs = a.id_accs")
test$id_accs = NULL
test_data = test %>% select(colnames(FeatToKeep))

XTest=get_ready_for_logistic_bnsf(test_data,TFDF,train=FALSE)

pred = predict(gbm_fit,newdata=XTest,type = 'prob') [,1] 
test_d = data.frame(test,pred)
test_d$pred = 1- test_d$pred
test_d = test_d %>% select(c("id_accs","id_props","pred"))

test_d = sqldf("select * from test_d t where t.id_props not in (select id_props from acc_prop)")

test_d = test_d %>% group_by(id_accs) %>% mutate(rnk = rank(desc(pred))) %>% 
  filter(rnk < 1500) %>% arrange(id_accs,rnk)


final = test_d %>% filter(pred > 0.9)
final$pred = NULL
final$rnk = NULL
colnames(final) = c("id_accs","id_prop")
write.csv(final,"submission.csv",row.names = F)


save("gbm_fit","TFDF",file="TFDF.Rdata")
