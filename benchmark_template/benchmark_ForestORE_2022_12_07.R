wd <- getwd()
setwd(wd)

library(sqldf)
library(reshape2)
library(tidyverse)


# ..............................................................................................................
# Datasets summary
# ..............................................................................................................

filePaths_data_summary = list.files("./data_summary", "\\.csv$", full.names = TRUE)

grep(list.files(path="./Rules_results", pattern='_prediction_perf.csv'), pattern='_avg_', invert=TRUE, value=TRUE)

B_data_summary0 = do.call(rbind, lapply(filePaths_data_summary, read.csv))
B_data_summary0$name=toupper(B_data_summary0$name)
B_data_summary0$name[which(B_data_summary0$name=="BCO")] ="BRCANCER"
B_data_summary0_path= ".\\benchmark_results\\data_summary\\data_summary0.csv"
write.csv(B_data_summary0,B_data_summary0_path, row.names = FALSE)

B_data_summary=B_data_summary0[,c(1:3,8)]

B_data_summary$ncol=B_data_summary$ncol-1
names(B_data_summary)= c("Dataset","NB_instances","NB_attributes", "NB_classes")
B_data_summary_path= ".\\benchmark_results\\data_summary\\data_summary.csv"
write.csv(B_data_summary,B_data_summary_path, row.names = FALSE)

testF0=read.table(B_data_summary_path,sep=',',header= T, na.strings="?")

# ..............................................................................................................
#  Prediction performance
# ..............................................................................................................


# I- Prediction performance: "RPART", "GRRFR", "STEL", "STELVote", "Pre-Forest-ORE", "Forest-ORE","Forest-ORE+STEL"
# ..............................................................................................................


filePaths_prediction_perf_init = grep(list.files(path="./Rules_results", pattern='_prediction_perf.csv',full.names = TRUE), pattern='_avg_', invert=TRUE, value=TRUE)

B_perf_all_iter_init <- do.call(rbind, lapply(filePaths_prediction_perf_init, read.csv))
# table(B_perf_all_iter_init$pred_set)
# head(B_perf_all_iter_init)
# names(B_perf_all_iter_init)
B_perf_all_iter_init$data =toupper(B_perf_all_iter_init$data)
B_perf_all_iter_init$data[which(B_perf_all_iter_init$data=="BCO")] ="BRCANCER"
# unique(B_perf_all_iter_init$method)
B_perf_all_iter_init$class_rules_nbr=B_perf_all_iter_init$rules_nbr
names(B_perf_all_iter_init)
unique(B_perf_all_iter_init$method)
B_perf_all_iter_init$method=dplyr::recode(B_perf_all_iter_init$method,
                                     CART="RPART",
                                     IntreesRRFVote="GRRFR",
                                     IntreesSTEL="STEL",
                                     IntreesVote= "STELVote",
                                     PreSelectedRules="Pre-Forest-ORE",
                                     OptimalRules="Forest-ORE",
                                     OPTSTEL="Forest-ORE+STEL")
B_perf_all_iter_init$method=recode_factor(B_perf_all_iter_init$method,
                              CART="RPART",
                              IntreesRRFVote="GRRFR",
                              IntreesSTEL="STEL",
                              IntreesVote= "STELVote",
                              PreSelectedRules="Pre-Forest-ORE",
                              OptimalRules="Forest-ORE",
                              OPTSTEL="Forest-ORE+STEL")

str(B_data_summary)
for (data_name in unique(B_perf_all_iter_init$data))
{
  print(data_name)
  data_rows=which(B_perf_all_iter_init$data==data_name & B_perf_all_iter_init$pred_sub_set %in% c("all","covered"))
  nclasses=as.numeric(B_data_summary[which(B_data_summary[,"Dataset"]==data_name),"NB_classes"])
  B_perf_all_iter_init[data_rows,"class_rules_nbr"]= B_perf_all_iter_init[data_rows,"class_rules_nbr"] / nclasses
}

meth_lev_init=c("RPART", "RF", "Pre-Forest-ORE", "Forest-ORE","GRRFR", "STEL", "Forest-ORE+STEL")


B_perf_all_iter_init=B_perf_all_iter_init %>% 
  filter(method  %in% meth_lev_init) %>%  
  mutate(method = factor(method))
table(B_perf_all_iter_init$data, B_perf_all_iter_init$method)
B_perf_all_iter_init_path= ".\\benchmark_results\\rules_results\\B_perf_all_iter_init.csv"
write.csv(B_perf_all_iter_init,B_perf_all_iter_init_path, row.names = FALSE)
testF1=read.table(B_perf_all_iter_init_path,sep=',',header= T, na.strings="?")



# ..............................................................................................................
# II- Prediction performance: "CBA", "RIPPER", "SBRL"
# ..............................................................................................................


filePaths_prediction_perf_add = grep(list.files(path="./Rules_results_add", pattern='_prediction_perf',full.names = TRUE), pattern='_avg_', invert=TRUE, value=TRUE)

B_perf_all_iter_add <- do.call(rbind, lapply(filePaths_prediction_perf_add, read.csv))
# table(B_perf_all_iter$pred_set)
# head(B_perf_all_iter)
# names(B_perf_all_iter)
B_perf_all_iter_add$data =toupper(B_perf_all_iter_add$data)
B_perf_all_iter_add$data[which(B_perf_all_iter_add$data=="BCO")] ="BRCANCER"
# unique(B_perf_all_iter$method)
B_perf_all_iter_add$class_rules_nbr=B_perf_all_iter_add$rules_nbr
names(B_perf_all_iter_add)
unique(B_perf_all_iter_add$method)


str(B_data_summary)


for (data_name in unique(B_perf_all_iter_add$data))
{
  print(data_name)
  data_rows=which(B_perf_all_iter_add$data==data_name & B_perf_all_iter_add$pred_sub_set %in% c("all","covered"))
  nclasses=as.numeric(B_data_summary[which(B_data_summary[,"Dataset"]==data_name),"NB_classes"])
  B_perf_all_iter_add[data_rows,"class_rules_nbr"]= B_perf_all_iter_add[data_rows,"class_rules_nbr"] / nclasses
}

B_perf_all_iter_add_path= ".\\benchmark_results\\rules_results\\B_perf_all_iter_add.csv"
write.csv(B_perf_all_iter_add,B_perf_all_iter_add_path, row.names = FALSE)
testF111=read.table(B_perf_all_iter_add_path,sep=',',header= T, na.strings="?")

table(B_perf_all_iter_add$data,B_perf_all_iter_add$method)

# SBRL can be used only with binary classification data sets.
# We do not report SBRL results for "TITANIC" and "BANANA" because "SBRL" package returned a fatal error for these twe datasets

test_data = as.data.frame(table(B_perf_all_iter_add$data, B_perf_all_iter_add$method))
bin_data=test_data$Var1[which(test_data$Freq!=0 & test_data$Var2=="SBRL")]
length(bin_data)
# 19
36-19
# ..............................................................................................................
# III- Prediction performance: all classifiers
# ..............................................................................................................

# table(B_perf_all_iter$data,B_perf_all_iter$method)
names(B_perf_all_iter_init)
names(B_perf_all_iter_add)

B_perf_all_iter=rbind(B_perf_all_iter_init,B_perf_all_iter_add)
unique(B_perf_all_iter$method)

names(B_perf_all_iter)
unique (B_perf_all_iter$method)
#  fidelity metric
B_perf_all_iter_fidelity = B_perf_all_iter %>%
  filter( method %in% c("RF",  "Pre-Forest-ORE", "Forest-ORE", "Forest-ORE+STEL", "GRRFR", "STEL")) %>%
  select(data, iter, method, data,  pred_set, pred_sub_set, method, coverage, 
         fidelity, fidelity_correct, fidelity_incorrect) %>%
  mutate(method = factor(method))
B_perf_ORE_summary_iter_fidelity = sqldf('select method,iter, pred_set,pred_sub_set,
                          avg(coverage) as mean_coverage, 
                          stdev(coverage) as std_coverage,
                          stdev(coverage)/sqrt(count(coverage)) as SE_coverage,
                          avg(fidelity) as mean_fidelity, 
                          stdev(fidelity) as std_fidelity,
                          stdev(fidelity)/sqrt(count(fidelity)) as SE_fidelity,
                          avg(fidelity_correct) as mean_fidelity_correct, 
                          stdev(fidelity_correct) as std_fidelity_correct,
                          stdev(fidelity_correct)/sqrt(count(fidelity_correct)) as SE_fidelity_correct,
                          avg(fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(fidelity_incorrect)/sqrt(count(fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_all_iter_fidelity group by method,iter,pred_set, pred_sub_set')



B_perf_ORE_summary_perdata_fidelity = sqldf('select method,  data, pred_set,pred_sub_set,
                          avg(coverage) as mean_coverage, 
                          stdev(coverage) as std_coverage,
                          stdev(coverage)/sqrt(count(coverage)) as SE_coverage,
                          avg(fidelity) as mean_fidelity, 
                          stdev(fidelity) as std_fidelity,
                          stdev(fidelity)/sqrt(count(fidelity)) as SE_fidelity,
                          avg(fidelity_correct) as mean_fidelity_correct, 
                          stdev(fidelity_correct) as std_fidelity_correct,
                          stdev(fidelity_correct)/sqrt(count(fidelity_correct)) as SE_fidelity_correct,
                          avg(fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(fidelity_incorrect)/sqrt(count(fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_all_iter_fidelity group by method, data,pred_set,pred_sub_set')


B_perf_ORE_summary_iter_based_fidelity = sqldf('select  pred_set,pred_sub_set, method, 
                          avg(mean_coverage) as mean_coverage, 
                          stdev(mean_coverage) as std_coverage,
                          stdev(mean_coverage)/sqrt(count(mean_coverage)) as SE_coverage,
                          avg(mean_fidelity) as mean_fidelity, 
                          stdev(mean_fidelity) as std_fidelity,
                          stdev(mean_fidelity)/sqrt(count(mean_fidelity)) as SE_fidelity,
                          avg(mean_fidelity_correct) as mean_fidelity_correct, 
                          stdev(mean_fidelity_correct) as std_fidelity_correct,
                          stdev(mean_fidelity_correct)/sqrt(count(mean_fidelity_correct)) as SE_fidelity_correct,
                          avg(mean_fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(mean_fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(mean_fidelity_incorrect)/sqrt(count(mean_fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_ORE_summary_iter_fidelity group by pred_set,pred_sub_set,method')

  
names(B_perf_ORE_summary_iter_based_fidelity)
DF_fidelity = B_perf_ORE_summary_iter_based_fidelity %>% 
  filter(pred_set == "test") 

DF_fidelity[,4:ncol(DF_fidelity)] = format(round(DF_fidelity[,4:ncol(DF_fidelity)],3), nsmall=3)
B_perf_ORE_Fidelity_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_Fidelity.csv"
write.csv(DF_fidelity,B_perf_ORE_Fidelity_path, row.names = FALSE)

#  III-1 Prediction performance on binary classification 
   
# Filter the results based on binary classification datasets
B_perf_all_iter_bin = B_perf_all_iter %>% filter(data %in% bin_data)
B_perf_all_iter_multi = B_perf_all_iter %>% 
  filter(!data abl_mod bin_data) %>% 
  filter(method != "SBRL") %>%
  mutate(method = factor(method))


  
# names(B_perf_all_iter_multi)
# unique(B_perf_all_iter_multi$data)
# table(B_perf_all_iter_multi$data, B_perf_all_iter_multi$method)

# .................................................................................................
# Results summaries  on binary classification
B_perf_ORE_summary_iter_bin = sqldf('select method,iter, pred_set,pred_sub_set,
                          avg(rules_nbr) as mean_rules_nbr, 
                          stdev(rules_nbr) as std_rules_nbr,
                          stdev(rules_nbr)/sqrt(count(rules_nbr)) as SE_rules_nbr,
                          avg(class_rules_nbr) as mean_class_rules_nbr, 
                          stdev(class_rules_nbr) as std_class_rules_nbr,
                          stdev(class_rules_nbr)/sqrt(count(class_rules_nbr)) as SE_class_rules_nbr,
                          avg(coverage) as mean_coverage, 
                          stdev(coverage) as std_coverage,
                          stdev(coverage)/sqrt(count(coverage)) as SE_coverage,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          stdev(macroPrecision)/sqrt(count(macroPrecision)) as SE_macroPrecision,
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          stdev(macroRecall)/sqrt(count(macroRecall)) as SE_macroRecall,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa,
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw,
                          stdev(kappa_w)/sqrt(count(kappa_w)) as SE_kappa_w,
                          avg(fidelity) as mean_fidelity, 
                          stdev(fidelity) as std_fidelity,
                          stdev(fidelity)/sqrt(count(fidelity)) as SE_fidelity,
                          avg(fidelity_correct) as mean_fidelity_correct, 
                          stdev(fidelity_correct) as std_fidelity_correct,
                          stdev(fidelity_correct)/sqrt(count(fidelity_correct)) as SE_fidelity_correct,
                          avg(fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(fidelity_incorrect)/sqrt(count(fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_all_iter_bin group by method,iter,pred_set, pred_sub_set')



B_perf_ORE_summary_perdata_bin = sqldf('select method,  data, pred_set,pred_sub_set,
                          avg(rules_nbr) as mean_rules_nbr, 
                          stdev(rules_nbr) as std_rules_nbr,
                          stdev(rules_nbr)/sqrt(count(rules_nbr)) as SE_rules_nbr,
                          avg(class_rules_nbr) as mean_class_rules_nbr, 
                          stdev(class_rules_nbr) as std_class_rules_nbr,
                          stdev(class_rules_nbr)/sqrt(count(class_rules_nbr)) as SE_class_rules_nbr,
                          avg(coverage) as mean_coverage, 
                          stdev(coverage) as std_coverage,
                          stdev(coverage)/sqrt(count(coverage)) as SE_coverage,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          stdev(macroPrecision)/sqrt(count(macroPrecision)) as SE_macroPrecision,
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          stdev(macroRecall)/sqrt(count(macroRecall)) as SE_macroRecall,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa,
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw,
                          stdev(kappa_w)/sqrt(count(kappa_w)) as SE_kappa_w,
                          avg(fidelity) as mean_fidelity, 
                          stdev(fidelity) as std_fidelity,
                          stdev(fidelity)/sqrt(count(fidelity)) as SE_fidelity,
                          avg(fidelity_correct) as mean_fidelity_correct, 
                          stdev(fidelity_correct) as std_fidelity_correct,
                          stdev(fidelity_correct)/sqrt(count(fidelity_correct)) as SE_fidelity_correct,
                          avg(fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(fidelity_incorrect)/sqrt(count(fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_all_iter_bin group by method, data,pred_set,pred_sub_set')


B_perf_ORE_summary_iter_based_bin = sqldf('select  pred_set,pred_sub_set, method, 
                          avg(mean_rules_nbr) as mean_rules_nbr, 
                          stdev(mean_rules_nbr) as std_rules_nbr,
                          stdev(mean_rules_nbr)/sqrt(count(mean_rules_nbr)) as SE_rules_nbr,
                          avg(mean_class_rules_nbr) as mean_class_rules_nbr, 
                          stdev(mean_class_rules_nbr) as std_class_rules_nbr,
                          stdev(mean_class_rules_nbr)/sqrt(count(mean_class_rules_nbr)) as SE_class_rules_nbr,
                          avg(mean_coverage) as mean_coverage, 
                          stdev(mean_coverage) as std_coverage,
                          stdev(mean_coverage)/sqrt(count(mean_coverage)) as SE_coverage,
                          avg(mean_acc) as mean_acc, 
                          stdev(mean_acc) as std_acc, 
                          stdev(mean_acc)/sqrt(count(mean_acc)) as SE_acc,
                          avg(mean_Precision) as mean_Precision, 
                          stdev(mean_Precision) as std_Precision, 
                          stdev(mean_Precision)/sqrt(count(mean_Precision)) as SE_macroPrecision,
                          avg(mean_Recall) as mean_Recall, 
                          stdev(mean_Recall) as std_Recall, 
                          stdev(mean_Recall)/sqrt(count(mean_Recall)) as SE_macroRecall,
                          avg(mean_F1) as mean_F1, 
                          stdev(mean_F1) as std_F1,
                          stdev(mean_F1)/sqrt(count(mean_F1)) as SE_F1,
                          avg(mean_Kappa) as mean_Kappa, 
                          stdev(mean_Kappa) as std_Kappa, 
                          stdev(mean_Kappa)/sqrt(count(mean_Kappa)) as SE_kappa,
                          avg(mean_Kappaw) as mean_Kappaw, 
                          stdev(mean_Kappaw) as std_Kappaw,
                          stdev(mean_Kappaw)/sqrt(count(mean_Kappaw)) as SE_kappa_w,
                          avg(mean_fidelity) as mean_fidelity, 
                          stdev(mean_fidelity) as std_fidelity,
                          stdev(mean_fidelity)/sqrt(count(mean_fidelity)) as SE_fidelity,
                          avg(mean_fidelity_correct) as mean_fidelity_correct, 
                          stdev(mean_fidelity_correct) as std_fidelity_correct,
                          stdev(mean_fidelity_correct)/sqrt(count(mean_fidelity_correct)) as SE_fidelity_correct,
                          avg(mean_fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(mean_fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(mean_fidelity_incorrect)/sqrt(count(mean_fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_ORE_summary_iter_bin group by pred_set,pred_sub_set,method')


B_perf_ORE_summary_iter_bin_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_summary_iter_bin.csv"
write.csv(B_perf_ORE_summary_iter_bin,B_perf_ORE_summary_iter_bin_path, row.names = FALSE)
testF20=read.table(B_perf_ORE_summary_iter_bin_path,sep=',',header= T, na.strings="?")

B_perf_ORE_summary_perdata_bin_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_summary_perdata_bin.csv"
write.csv(B_perf_ORE_summary_perdata_bin,B_perf_ORE_summary_perdata_bin_path, row.names = FALSE)
testF30=read.table(B_perf_ORE_summary_perdata_bin_path,sep=',',header= T, na.strings="?")

B_perf_ORE_summary_iter_based_bin_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_summary_iter_based_bin.csv"
write.csv(B_perf_ORE_summary_iter_based_bin,B_perf_ORE_summary_iter_based_bin_path, row.names = FALSE)
testF40=read.table(B_perf_ORE_summary_iter_based_bin_path,sep=',',header= T, na.strings="?")

# Initialization of Friedman rank sum test results

Friedman_rank_sum_test_DF=data.frame(matrix(NA,ncol=7))
colnames(Friedman_rank_sum_test_DF)= c("Classification issue","coverage", "method","tested_data","statistic",
                                       "parameter","pvalue")  


# statistical test on binary classification
# accuracy_all
# Dataperf_bin_appendicitis=B_perf_all_iter_bin_test[which(B_perf_all_iter_bin_test[,"pred_set"]=="test" & B_perf_all_iter_bin_test[,"pred_sub_set"]=="all" & B_perf_all_iter_bin_test[,"data"]== "APPENDICITIS"),]

Dataperf_bin = B_perf_ORE_summary_perdata_bin %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "all")

table(Dataperf_bin$method, Dataperf_bin$data)

names(Dataperf_bin)

DF=Dataperf_bin %>%select(method, mean_acc, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_acc)
DFspread_acc= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)])
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )
DF_rank_acc = DFrank

# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_acc[nrow(DF_rank_acc)+1,1] = "Total Rank"
DF_rank_acc[nrow(DF_rank_acc),2:ncol(DF_rank_acc)] = colSums(DFrank[,2:ncol(DFspread)])
DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_acc ~ method | data, data = DF)
RFtest_vect = c("Binary classification", "all", FRtest$method,FRtest$data.name, FRtest$statistic, 
                FRtest$parameter,FRtest$p.value)


Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 35.682, df = 9, p-value = 4.514e-05



DFspread_acc[,2:ncol(DFspread_acc)] = format(DFspread_acc[,2:ncol(DFspread_acc)], nsmall = 3)
DFspread_rank_acc_bin = DF_rank_acc
for (row in 1:nrow(DFspread_acc)) 
{
  for (col in 2:ncol(DFspread_acc)) 
  {
    DFspread_rank_acc_bin[row, col] = paste0(DFspread_acc[row, col], " (",DF_rank_acc[row, col], ")")
  }
  
}
DFspread_rank_acc_bin_path= ".\\benchmark_results\\rules_results\\DFspread_rank_acc_bin.csv"
write.csv(DFspread_rank_acc_bin,DFspread_rank_acc_bin_path, row.names = FALSE)
# .........................................
# accuracy_cover

Dataperf_bin_cov = B_perf_ORE_summary_perdata_bin %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "covered")


DF=Dataperf_bin_cov %>%select(method, mean_acc, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_acc)
names(Dataperf_bin_cov)

DFspread_acc_cov= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)])
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )

DF_rank_acc_cov = DFrank
# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_acc_cov[nrow(DF_rank_acc_cov)+1,1] = "Total Rank"
DF_rank_acc_cov[nrow(DF_rank_acc_cov),2:ncol(DF_rank_acc_cov)] = colSums(DFrank[,2:ncol(DFrank)])

DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_acc ~ method | data, data = DF)
RFtest_vect = c("Binary classification", "covered", FRtest$method,FRtest$data.name, FRtest$statistic, 
                FRtest$parameter,FRtest$p.value)


Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 45.103, df = 9, p-value = 8.829e-07

DFspread_acc_cov[,2:ncol(DFspread_acc_cov)] = format(DFspread_acc_cov[,2:ncol(DFspread_acc_cov)], nsmall = 3)
DFspread_rank_acc_cov_bin = DF_rank_acc_cov
for (row in 1:nrow(DFspread_acc_cov)) 
{
  for (col in 2:ncol(DFspread_acc_cov)) 
  {
    DFspread_rank_acc_cov_bin[row, col] = paste0(DFspread_acc_cov[row, col], " (",DF_rank_acc_cov[row, col], ")")
  }
  
}
DFspread_rank_acc_cov_bin_path= ".\\benchmark_results\\rules_results\\DFspread_rank_acc_cov_bin.csv"
write.csv(DFspread_rank_acc_cov_bin,DFspread_rank_acc_cov_bin_path, row.names = FALSE)
# ............................
# coverage_cover

Dataperf_bin_cov = B_perf_ORE_summary_perdata_bin %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "covered")

names(Dataperf_bin_cov)
DF=Dataperf_bin_cov %>%select(method, mean_coverage, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_coverage)
names(Dataperf_bin_cov)

DFspread_coverage_cov= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)])
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )

DF_rank_coverage_cov = DFrank
# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_coverage_cov[nrow(DF_rank_coverage_cov)+1,1] = "Total Rank"
DF_rank_coverage_cov[nrow(DF_rank_coverage_cov),2:ncol(DF_rank_coverage_cov)] = colSums(DFrank[,2:ncol(DFrank)])

DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_coverage ~ method | data, data = DF)
RFtest_vect = c("Binary classification", "covered", FRtest$method,FRtest$data.name, FRtest$statistic, 
                FRtest$parameter,FRtest$p.value)


Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 42.669, df = 9, p-value = 2.479e-06

DFspread_coverage_cov[,2:ncol(DFspread_coverage_cov)] = format(DFspread_coverage_cov[,2:ncol(DFspread_coverage_cov)], nsmall = 3)
DFspread_rank_coverage_cov_bin = DF_rank_coverage_cov
for (row in 1:nrow(DFspread_coverage_cov)) 
{
  for (col in 2:ncol(DFspread_coverage_cov)) 
  {
    DFspread_rank_coverage_cov_bin[row, col] = paste0(DFspread_coverage_cov[row, col], " (",DF_rank_coverage_cov[row, col], ")")
  }
  
}
DFspread_rank_coverage_cov_bin_path= ".\\benchmark_results\\rules_results\\DFspread_rank_coverage_cov_bin.csv"
write.csv(DFspread_rank_coverage_cov_bin,DFspread_rank_coverage_cov_bin_path, row.names = FALSE)

# NBrules_cover

Dataperf_bin_cov = B_perf_ORE_summary_perdata_bin %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "covered")

# piv= pivot_wider(data = Dataperf_bin_cov, 
#             id_cols = data, 
#             names_from = method, 
#             values_from = c("mean_rules_nbr", "std_rules_nbr"))
names(Dataperf_bin_cov)
DF=Dataperf_bin_cov %>%select(method, mean_rules_nbr, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_rules_nbr)
names(Dataperf_bin_cov)

DFspread_coverage_NBrules= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)])
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )

DF_rank_coverage_NBrules = DFrank
# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_coverage_NBrules[nrow(DF_rank_coverage_NBrules)+1,1] = "Total Rank"
DF_rank_coverage_NBrules[nrow(DF_rank_coverage_NBrules),2:ncol(DF_rank_coverage_NBrules)] = colSums(DFrank[,2:ncol(DFrank)])

DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_rules_nbr ~ method | data, data = DF)
RFtest_vect = c("Binary classification", "covered", FRtest$method, FRtest$data.name, FRtest$statistic, 
                FRtest$parameter, FRtest$p.value)
  

Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)

str(FRtest)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 42.669, df = 9, p-value = 2.479e-06

# ..........................................
#  Multi-class classification
# Results summaries  on multi-class classification
B_perf_ORE_summary_iter_multi = sqldf('select method,iter, pred_set,pred_sub_set,
                          avg(rules_nbr) as mean_rules_nbr, 
                          stdev(rules_nbr) as std_rules_nbr,
                          stdev(rules_nbr)/sqrt(count(rules_nbr)) as SE_rules_nbr,
                          avg(class_rules_nbr) as mean_class_rules_nbr, 
                          stdev(class_rules_nbr) as std_class_rules_nbr,
                          stdev(class_rules_nbr)/sqrt(count(class_rules_nbr)) as SE_class_rules_nbr,
                          avg(coverage) as mean_coverage, 
                          stdev(coverage) as std_coverage,
                          stdev(coverage)/sqrt(count(coverage)) as SE_coverage,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          stdev(macroPrecision)/sqrt(count(macroPrecision)) as SE_macroPrecision,
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          stdev(macroRecall)/sqrt(count(macroRecall)) as SE_macroRecall,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa,
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw,
                          stdev(kappa_w)/sqrt(count(kappa_w)) as SE_kappa_w,
                          avg(fidelity) as mean_fidelity, 
                          stdev(fidelity) as std_fidelity,
                          stdev(fidelity)/sqrt(count(fidelity)) as SE_fidelity,
                          avg(fidelity_correct) as mean_fidelity_correct, 
                          stdev(fidelity_correct) as std_fidelity_correct,
                          stdev(fidelity_correct)/sqrt(count(fidelity_correct)) as SE_fidelity_correct,
                          avg(fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(fidelity_incorrect)/sqrt(count(fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_all_iter_multi group by method,iter,pred_set, pred_sub_set')



B_perf_ORE_summary_perdata_multi = sqldf('select method,  data, pred_set,pred_sub_set,
                          avg(rules_nbr) as mean_rules_nbr, 
                          stdev(rules_nbr) as std_rules_nbr,
                          stdev(rules_nbr)/sqrt(count(rules_nbr)) as SE_rules_nbr,
                          avg(class_rules_nbr) as mean_class_rules_nbr, 
                          stdev(class_rules_nbr) as std_class_rules_nbr,
                          stdev(class_rules_nbr)/sqrt(count(class_rules_nbr)) as SE_class_rules_nbr,
                          avg(coverage) as mean_coverage, 
                          stdev(coverage) as std_coverage,
                          stdev(coverage)/sqrt(count(coverage)) as SE_coverage,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          stdev(macroPrecision)/sqrt(count(macroPrecision)) as SE_macroPrecision,
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          stdev(macroRecall)/sqrt(count(macroRecall)) as SE_macroRecall,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa,
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw,
                          stdev(kappa_w)/sqrt(count(kappa_w)) as SE_kappa_w,
                          avg(fidelity) as mean_fidelity, 
                          stdev(fidelity) as std_fidelity,
                          stdev(fidelity)/sqrt(count(fidelity)) as SE_fidelity,
                          avg(fidelity_correct) as mean_fidelity_correct, 
                          stdev(fidelity_correct) as std_fidelity_correct,
                          stdev(fidelity_correct)/sqrt(count(fidelity_correct)) as SE_fidelity_correct,
                          avg(fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(fidelity_incorrect)/sqrt(count(fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_all_iter_multi group by method, data,pred_set,pred_sub_set')


B_perf_ORE_summary_iter_based_multi = sqldf('select  pred_set,pred_sub_set, method, 
                          avg(mean_rules_nbr) as mean_rules_nbr, 
                          stdev(mean_rules_nbr) as std_rules_nbr,
                          stdev(mean_rules_nbr)/sqrt(count(mean_rules_nbr)) as SE_rules_nbr,
                          avg(mean_class_rules_nbr) as mean_class_rules_nbr, 
                          stdev(mean_class_rules_nbr) as std_class_rules_nbr,
                          stdev(mean_class_rules_nbr)/sqrt(count(mean_class_rules_nbr)) as SE_class_rules_nbr,
                          avg(mean_coverage) as mean_coverage, 
                          stdev(mean_coverage) as std_coverage,
                          stdev(mean_coverage)/sqrt(count(mean_coverage)) as SE_coverage,
                          avg(mean_acc) as mean_acc, 
                          stdev(mean_acc) as std_acc, 
                          stdev(mean_acc)/sqrt(count(mean_acc)) as SE_acc,
                          avg(mean_Precision) as mean_Precision, 
                          stdev(mean_Precision) as std_Precision, 
                          stdev(mean_Precision)/sqrt(count(mean_Precision)) as SE_macroPrecision,
                          avg(mean_Recall) as mean_Recall, 
                          stdev(mean_Recall) as std_Recall, 
                          stdev(mean_Recall)/sqrt(count(mean_Recall)) as SE_macroRecall,
                          avg(mean_F1) as mean_F1, 
                          stdev(mean_F1) as std_F1,
                          stdev(mean_F1)/sqrt(count(mean_F1)) as SE_F1,
                          avg(mean_Kappa) as mean_Kappa, 
                          stdev(mean_Kappa) as std_Kappa, 
                          stdev(mean_Kappa)/sqrt(count(mean_Kappa)) as SE_kappa,
                          avg(mean_Kappaw) as mean_Kappaw, 
                          stdev(mean_Kappaw) as std_Kappaw,
                          stdev(mean_Kappaw)/sqrt(count(mean_Kappaw)) as SE_kappa_w,
                          avg(mean_fidelity) as mean_fidelity, 
                          stdev(mean_fidelity) as std_fidelity,
                          stdev(mean_fidelity)/sqrt(count(mean_fidelity)) as SE_fidelity,
                          avg(mean_fidelity_correct) as mean_fidelity_correct, 
                          stdev(mean_fidelity_correct) as std_fidelity_correct,
                          stdev(mean_fidelity_correct)/sqrt(count(mean_fidelity_correct)) as SE_fidelity_correct,
                          avg(mean_fidelity_incorrect) as mean_fidelity_incorrect, 
                          stdev(mean_fidelity_incorrect) as std_fidelity_incorrect,
                          stdev(mean_fidelity_incorrect)/sqrt(count(mean_fidelity_incorrect)) as SE_fidelity_incorrect
                          from B_perf_ORE_summary_iter_multi group by pred_set,pred_sub_set,method')


B_perf_ORE_summary_iter_multi_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_summary_iter_multi.csv"
write.csv(B_perf_ORE_summary_iter_multi,B_perf_ORE_summary_iter_multi_path, row.names = FALSE)
testF20=read.table(B_perf_ORE_summary_iter_multi_path,sep=',',header= T, na.strings="?")

B_perf_ORE_summary_perdata_multi_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_summary_perdata_multi.csv"
write.csv(B_perf_ORE_summary_perdata_multi,B_perf_ORE_summary_perdata_multi_path, row.names = FALSE)
testF30=read.table(B_perf_ORE_summary_perdata_multi_path,sep=',',header= T, na.strings="?")

B_perf_ORE_summary_iter_based_multi_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_summary_iter_based_multi.csv"
write.csv(B_perf_ORE_summary_iter_based_multi,B_perf_ORE_summary_iter_based_multi_path, row.names = FALSE)
testF40=read.table(B_perf_ORE_summary_iter_based_multi_path,sep=',',header= T, na.strings="?")

# statistical test on multiclass classification
# accuracy_all


Dataperf_multi = B_perf_ORE_summary_perdata_multi %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "all")

table(Dataperf_multi$method, Dataperf_multi$data)

names(Dataperf_multi)

DF=Dataperf_multi %>%select(method, mean_acc, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_acc)
DFspread_acc_multi= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)])
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )

DF_rank_acc_multi = DFrank
# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_acc_multi[nrow(DF_rank_acc_multi)+1,1] = "Total Rank"
DF_rank_acc_multi[nrow(DF_rank_acc_multi),2:ncol(DF_rank_acc_multi)] = colSums(DFrank[,2:ncol(DFspread)])
DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_acc ~ method | data, data = DF)
RFtest_vect = c("Multiclass classification", "all", FRtest$method,FRtest$data.name, FRtest$statistic, 
                FRtest$parameter,FRtest$p.value)


Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 35.682, df = 9, p-value = 4.514e-05

# .........................................

DFspread_acc_multi[,2:ncol(DFspread_acc_multi)] = format(DFspread_acc_multi[,2:ncol(DFspread_acc_multi)], nsmall = 3)
DFspread_rank_acc_multi = DF_rank_acc_multi
for (row in 1:nrow(DFspread_acc_multi)) 
{
  for (col in 2:ncol(DFspread_acc_multi)) 
  {
    DFspread_rank_acc_multi[row, col] = paste0(DFspread_acc_multi[row, col], " (",DF_rank_acc_multi[row, col], ")")
  }
  
}
DFspread_rank_acc_multi_path= ".\\benchmark_results\\rules_results\\DFspread_rank_acc_multi.csv"
write.csv(DFspread_rank_acc_multi,DFspread_rank_acc_multi_path, row.names = FALSE)

# accuracy_cover

Dataperf_multi_cov = B_perf_ORE_summary_perdata_multi %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "covered")


DF=Dataperf_multi_cov %>%select(method, mean_acc, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_acc)
names(Dataperf_multi_cov)

DFspread_acc_cov_multi= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)],na.last = FALSE)
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )
getOption("na.action")
DF_rank_acc_cov_multi = DFrank
# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_acc_cov_multi[nrow(DF_rank_acc_cov_multi)+1,1] = "Total Rank"
DF_rank_acc_cov_multi[nrow(DF_rank_acc_cov_multi),2:ncol(DF_rank_acc_cov_multi)] = colSums(DFrank[,2:ncol(DFrank)])

DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
# friedman test does not handle NAs. CBA classifier did not generate any rule 
# for vowel dataset (just a default rule). DF contains NAs for covered instance in this case .
#  we ignore vowel dataset for friedman test
DF = DF %>% filter (data != "VOWEL")
DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_acc ~ method | data, data = DF)
RFtest_vect = c("Multiclass classification", "covered", FRtest$method,FRtest$data.name, FRtest$statistic, 
                FRtest$parameter,FRtest$p.value)


Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)

DFspread_acc_multi[,2:ncol(DFspread_acc_cov_multi)] = format(DFspread_acc_cov_multi[,2:ncol(DFspread_acc_cov_multi)], nsmall = 3)
DFspread_rank_acc_cov_multi = DF_rank_acc_cov_multi
for (row in 1:nrow(DFspread_acc_cov_multi)) 
{
  for (col in 2:ncol(DFspread_acc_cov_multi)) 
  {
    DFspread_rank_acc_cov_multi[row, col] = paste0(DFspread_acc_cov_multi[row, col], " (",DF_rank_acc_cov_multi[row, col], ")")
  }
  
}
DFspread_rank_acc_cov_multi_path= ".\\benchmark_results\\rules_results\\DFspread_rank_acc_cov_multi.csv"
write.csv(DFspread_rank_acc_cov_multi,DFspread_rank_acc_cov_multi_path, row.names = FALSE)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 45.103, df = 9, p-value = 8.829e-07


# ............................
# coverage_cover

Dataperf_multi_cov = B_perf_ORE_summary_perdata_multi %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "covered")

names(Dataperf_multi_cov)
DF=Dataperf_multi_cov %>%select(method, mean_coverage, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_coverage)
names(Dataperf_multi_cov)

DFspread_coverage_cov_multi= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)])
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )

DF_rank_coverage_cov_multi = DFrank
# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_coverage_cov_multi[nrow(DF_rank_coverage_cov_multi)+1,1] = "Total Rank"
DF_rank_coverage_cov_multi[nrow(DF_rank_coverage_cov_multi),2:ncol(DF_rank_coverage_cov_multi)] = colSums(DFrank[,2:ncol(DFrank)])

DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_coverage ~ method | data, data = DF)
RFtest_vect = c("Multiclass classification", "covered", FRtest$method,FRtest$data.name, FRtest$statistic, 
                FRtest$parameter,FRtest$p.value)


Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 42.669, df = 9, p-value = 2.479e-06

DFspread_coverage_cov_multi[,2:ncol(DFspread_coverage_cov_multi)] = format(DFspread_coverage_cov_multi[,2:ncol(DFspread_coverage_cov_multi)], nsmall = 3)
DFspread_rank_coverage_cov_multi = DF_rank_coverage_cov_multi
for (row in 1:nrow(DFspread_coverage_cov_multi)) 
{
  for (col in 2:ncol(DFspread_coverage_cov_multi)) 
  {
    DFspread_rank_coverage_cov_multi[row, col] = paste0(DFspread_coverage_cov_multi[row, col], " (",DF_rank_coverage_cov_multi[row, col], ")")
  }
  
}
DFspread_rank_coverage_cov_multi_path= ".\\benchmark_results\\rules_results\\DFspread_rank_coverage_cov_multi.csv"
write.csv(DFspread_rank_coverage_cov_multi,DFspread_rank_coverage_cov_multi_path, row.names = FALSE)
# Friedman rank sum test
Friedman_rank_sum_test_DF=Friedman_rank_sum_test_DF[-1,]
Friedman_rank_sum_test_DF_path= ".\\benchmark_results\\rules_results\\Friedman_rank_sum_test_DF.csv"
write.csv(Friedman_rank_sum_test_DF,Friedman_rank_sum_test_DF_path, row.names = FALSE)
# NBrules_cover

Dataperf_multi_cov = B_perf_ORE_summary_perdata_multi %>%
  filter(pred_set == "test") %>%
  filter (pred_sub_set == "covered")

# piv= pivot_wider(data = Dataperf_multi_cov, 
#             id_cols = data, 
#             names_from = method, 
#             values_from = c("mean_rules_nbr", "std_rules_nbr"))
names(Dataperf_multi_cov)
DF=Dataperf_multi_cov %>%select(method, mean_class_rules_nbr, data) 
DFspread= DF %>% mutate_if(is.numeric, round, 3) %>% spread(key=method, value=mean_class_rules_nbr)
names(Dataperf_multi_cov)

DFspread_coverage_NBrules_multi= DFspread


DFrank=DFspread
for (row in 1: NROW(DFspread)) 
{
  DFrank[row,2:ncol(DFspread)] = rank(DFspread[row,2:ncol(DFspread)])
}
colSums(DFrank[,2:ncol(DFspread)]) 
length(colSums(DFrank[,2:ncol(DFspread)]) )

DF_rank_coverage_NBrules_multi = DFrank
# DF_rank_acc[nrow(DF_rank_acc)+1,] = NA
DF_rank_coverage_NBrules_multi[nrow(DF_rank_coverage_NBrules_multi)+1,1] = "Total Rank"
DF_rank_coverage_NBrules_multi[nrow(DF_rank_coverage_NBrules_multi),2:ncol(DF_rank_coverage_NBrules_multi)] = colSums(DFrank[,2:ncol(DFrank)])

DF= DF %>% mutate(data = factor(data)) %>% mutate(method = factor(method))
FRtest=friedman.test(mean_class_rules_nbr ~ method | data, data = DF)
RFtest_vect = c("Multiclass classification", "covered", FRtest$method, FRtest$data.name, FRtest$statistic, 
                FRtest$parameter, FRtest$p.value)


Friedman_rank_sum_test_DF= rbind(Friedman_rank_sum_test_DF, RFtest_vect)
Friedman_rank_sum_test_DF$pvalue= round(Friedman_rank_sum_test_DF$pvalue, 2)
str(FRtest)
# Friedman rank sum test
# 
# data:  mean_acc and method and data
# Friedman chi-squared = 42.669, df = 9, p-value = 2.479e-06




# ...........................................
#  Rules length 
# ...........................................
# I- Rules length : "RPART", "GRRFR", "STEL", "STELVote", "Pre-Forest-ORE", "Forest-ORE","Forest-ORE+STEL"
# ..............................................................................................................

filePaths_att_length_init = list.files(path="./Rules_results", pattern='_var_used_stat.csv',full.names = TRUE)

B_perf_att_length_init <- do.call(rbind, lapply(filePaths_att_length_init, read.csv))
# table(B_perf_all_iter$pred_set)
# head(B_perf_all_iter)
# names(B_perf_all_iter)
B_perf_att_length_init$data =toupper(B_perf_att_length_init$data)
B_perf_att_length_init$data[which(B_perf_att_length_init$data=="BCO")] ="BRCANCER"
# str(B_perf_all_iter$method)
unique(B_perf_att_length_init$method)
B_perf_att_length_init$method=dplyr::recode(B_perf_att_length_init$method,
                              CART="RPART",
                              IntreesRRFVote="GRRFR",
                              IntreesSTEL="STEL",
                              IntreesVote= "STELVote",
                              PreSelectedRules="Pre-Forest-ORE",
                              OptimalRules="Forest-ORE",
                              OPTSTEL="Forest-ORE+STEL")
B_perf_att_length_init$method=recode_factor(B_perf_att_length_init$method,
                                     CART="RPART",
                                     IntreesRRFVote="GRRFR",
                                     IntreesSTEL="STEL",
                                     IntreesVote= "STELVote",
                                     PreSelectedRules="Pre-Forest-ORE",
                                     OptimalRules="Forest-ORE",
                                     OPTSTEL="Forest-ORE+STEL")

table(B_perf_att_length_init$iter, B_perf_att_length_init$method)




B_perf_att_length_init_path= ".\\benchmark_results\\rules_results\\B_perf_att_length_init.csv"
write.csv(B_perf_att_length_init,B_perf_att_length_init_path, row.names = FALSE)
testF5=read.table(B_perf_att_length_init_path,sep=',',header= T, na.strings="?")
names(B_perf_att_length_init)


# ..............................................................................................................
# II- Rules length : "CBA", "RIPPER", "SBRL"
# ..............................................................................................................

filePaths_att_length_add = list.files(path="./Rules_results_add", pattern='_var_used_stat_add_classifiers.csv',full.names = TRUE)

B_perf_att_length_add <- do.call(rbind, lapply(filePaths_att_length_add, read.csv))
# table(B_perf_all_iter$pred_set)
# head(B_perf_all_iter)
# names(B_perf_all_iter)
B_perf_att_length_add$data =toupper(B_perf_att_length_add$data)
B_perf_att_length_add$data[which(B_perf_att_length_add$data=="BCO")] ="BRCANCER"
# str(B_perf_all_iter$method)
unique(B_perf_att_length_add$method)

table(B_perf_att_length_add$data, B_perf_att_length_add$method)
# for vowel data set, CBA classifier had not produced any rule for any iteration (see "B_perf_all_iter_add")



B_perf_att_length_add_path= ".\\benchmark_results\\rules_results\\B_perf_att_length_add.csv"
write.csv(B_perf_att_length_add,B_perf_att_length_add_path, row.names = FALSE)
testF5=read.table(B_perf_att_length_add_path,sep=',',header= T, na.strings="?")
names(B_perf_att_length_add)

# ...............................................................
B_perf_att_length = rbind(B_perf_att_length_init, B_perf_att_length_add)
# ...............................................................


B_perf_ORE_att_length_perdata = sqldf('select method,  data,
                          avg(mean) as mean_att_nbr, 
                          stdev(mean) as std_att_nbr,
                          stdev(mean)/sqrt(count(mean)) as SE_att_nbr,
                          avg(min) as mean_min_att_nbr, 
                          stdev(min) as std_min_att_nbr,
                          stdev(min)/sqrt(count(min)) as SE_min_att_nbr,
                          avg(max) as mean_max_att_nbr, 
                          stdev(max) as std_max_att_nbr,
                          stdev(max)/sqrt(count(max)) as SE_max_att_nbr,
                          avg(median) as mean_median_att_nbr, 
                          stdev(median) as std_median_att_nbr, 
                          stdev(median)/sqrt(count(median)) as SE_median_att_nbr
                          from B_perf_att_length group by method, data')

B_perf_ORE_att_length_summary = sqldf('select method,  
                          avg(mean_att_nbr) as mean_att_nbr, 
                          stdev(mean_att_nbr) as std_att_nbr,
                          stdev(mean_att_nbr)/sqrt(count(mean_att_nbr)) as SE_att_nbr,
                          avg(mean_min_att_nbr) as mean_min_att_nbr, 
                          stdev(mean_min_att_nbr) as std_min_att_nbr,
                          stdev(mean_min_att_nbr)/sqrt(count(mean_min_att_nbr)) as SE_min_att_nbr,
                          avg(mean_max_att_nbr) as mean_max_att_nbr, 
                          stdev(mean_max_att_nbr) as std_max_att_nbr,
                          stdev(mean_max_att_nbr)/sqrt(count(mean_max_att_nbr)) as SE_max_att_nbr,
                          avg(mean_median_att_nbr) as mean_median_att_nbr, 
                          stdev(mean_median_att_nbr) as std_median_att_nbr, 
                          stdev(mean_median_att_nbr)/sqrt(count(mean_median_att_nbr)) as SE_median_att_nbr
                          from B_perf_ORE_att_length_perdata group by method')



B_perf_ORE_att_length_summary[,-1]= round(B_perf_ORE_att_length_summary[,-1],1)
B_perf_ORE_att_length_summary=B_perf_ORE_att_length_summary[,-c(3,6,9,12)]
B_perf_ORE_att_length_summary=B_perf_ORE_att_length_summary[,c(1,2,4,6,8)]
B_perf_ORE_att=melt(B_perf_ORE_att_length_summary,id=c("method"),variable.name = "length")



B_perf_att_length_summary_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_att_length_summary.csv"
write.csv(B_perf_ORE_att_length_summary,B_perf_att_length_summary_path, row.names = FALSE)
testF6=read.table(B_perf_att_length_summary_path,sep=',',header= T, na.strings="?")
B_perf_ORE_att_length_perdata_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_att_length_perdata.csv"
write.csv(B_perf_ORE_att_length_perdata,B_perf_ORE_att_length_perdata_path, row.names = FALSE)
testF7=read.table(B_perf_ORE_att_length_perdata_path,sep=',',header= T, na.strings="?")

# ............Results on binary classification /multiclass classification
B_perf_att_length_bin = B_perf_att_length %>% filter(data %in% bin_data)
B_perf_att_length_multi = B_perf_att_length %>% 
  filter(!data %in% bin_data) %>% 
  filter(method != "SBRL") %>%
  mutate(method = factor(method))

B_perf_ORE_att_length_perdata_bin = sqldf('select method,  data,
                          avg(mean) as mean_att_nbr, 
                          stdev(mean) as std_att_nbr,
                          stdev(mean)/sqrt(count(mean)) as SE_att_nbr,
                          avg(min) as mean_min_att_nbr, 
                          stdev(min) as std_min_att_nbr,
                          stdev(min)/sqrt(count(min)) as SE_min_att_nbr,
                          avg(max) as mean_max_att_nbr, 
                          stdev(max) as std_max_att_nbr,
                          stdev(max)/sqrt(count(max)) as SE_max_att_nbr,
                          avg(median) as mean_median_att_nbr, 
                          stdev(median) as std_median_att_nbr, 
                          stdev(median)/sqrt(count(median)) as SE_median_att_nbr
                          from B_perf_att_length_bin group by method, data')

B_perf_ORE_att_length_summary_bin = sqldf('select method,  
                          avg(mean_att_nbr) as mean_att_nbr, 
                          stdev(mean_att_nbr) as std_att_nbr,
                          stdev(mean_att_nbr)/sqrt(count(mean_att_nbr)) as SE_att_nbr,
                          avg(mean_min_att_nbr) as mean_min_att_nbr, 
                          stdev(mean_min_att_nbr) as std_min_att_nbr,
                          stdev(mean_min_att_nbr)/sqrt(count(mean_min_att_nbr)) as SE_min_att_nbr,
                          avg(mean_max_att_nbr) as mean_max_att_nbr, 
                          stdev(mean_max_att_nbr) as std_max_att_nbr,
                          stdev(mean_max_att_nbr)/sqrt(count(mean_max_att_nbr)) as SE_max_att_nbr,
                          avg(mean_median_att_nbr) as mean_median_att_nbr, 
                          stdev(mean_median_att_nbr) as std_median_att_nbr, 
                          stdev(mean_median_att_nbr)/sqrt(count(mean_median_att_nbr)) as SE_median_att_nbr
                          from B_perf_ORE_att_length_perdata_bin group by method')
B_perf_ORE_att_length_summary_bin[,-1]= round(B_perf_ORE_att_length_summary_bin[,-1],1)
B_perf_ORE_att_length_summary_bin=B_perf_ORE_att_length_summary_bin[,-c(3,6,9,12)]
B_perf_ORE_att_length_summary_bin=B_perf_ORE_att_length_summary_bin[,c(1,2,4,6,8)]

B_perf_att_length_summary_bin_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_att_length_summary_bin.csv"
write.csv(B_perf_ORE_att_length_summary_bin,B_perf_att_length_summary_bin_path, row.names = FALSE)
testF6=read.table(B_perf_att_length_summary_bin_path,sep=',',header= T, na.strings="?")
B_perf_ORE_att_length_perdata_bin_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_att_length_perdata_bin.csv"
write.csv(B_perf_ORE_att_length_perdata_bin,B_perf_ORE_att_length_perdata_bin_path, row.names = FALSE)
testF7=read.table(B_perf_ORE_att_length_perdata_bin_path,sep=',',header= T, na.strings="?")

B_perf_ORE_att_length_perdata_multi = sqldf('select method,  data,
                          avg(mean) as mean_att_nbr, 
                          stdev(mean) as std_att_nbr,
                          stdev(mean)/sqrt(count(mean)) as SE_att_nbr,
                          avg(min) as mean_min_att_nbr, 
                          stdev(min) as std_min_att_nbr,
                          stdev(min)/sqrt(count(min)) as SE_min_att_nbr,
                          avg(max) as mean_max_att_nbr, 
                          stdev(max) as std_max_att_nbr,
                          stdev(max)/sqrt(count(max)) as SE_max_att_nbr,
                          avg(median) as mean_median_att_nbr, 
                          stdev(median) as std_median_att_nbr, 
                          stdev(median)/sqrt(count(median)) as SE_median_att_nbr
                          from B_perf_att_length_multi group by method, data')

B_perf_ORE_att_length_summary_multi = sqldf('select method,  
                          avg(mean_att_nbr) as mean_att_nbr, 
                          stdev(mean_att_nbr) as std_att_nbr,
                          stdev(mean_att_nbr)/sqrt(count(mean_att_nbr)) as SE_att_nbr,
                          avg(mean_min_att_nbr) as mean_min_att_nbr, 
                          stdev(mean_min_att_nbr) as std_min_att_nbr,
                          stdev(mean_min_att_nbr)/sqrt(count(mean_min_att_nbr)) as SE_min_att_nbr,
                          avg(mean_max_att_nbr) as mean_max_att_nbr, 
                          stdev(mean_max_att_nbr) as std_max_att_nbr,
                          stdev(mean_max_att_nbr)/sqrt(count(mean_max_att_nbr)) as SE_max_att_nbr,
                          avg(mean_median_att_nbr) as mean_median_att_nbr, 
                          stdev(mean_median_att_nbr) as std_median_att_nbr, 
                          stdev(mean_median_att_nbr)/sqrt(count(mean_median_att_nbr)) as SE_median_att_nbr
                          from B_perf_ORE_att_length_perdata_multi group by method')
B_perf_ORE_att_length_summary_multi[,-1]= round(B_perf_ORE_att_length_summary_multi[,-1],1)
B_perf_ORE_att_length_summary_multi=B_perf_ORE_att_length_summary_multi[,-c(3,6,9,12)]
B_perf_ORE_att_length_summary_multi=B_perf_ORE_att_length_summary_multi[,c(1,2,4,6,8)]

B_perf_att_length_summary_multi_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_att_length_summary_multi.csv"
write.csv(B_perf_ORE_att_length_summary_multi,B_perf_att_length_summary_multi_path, row.names = FALSE)
testF6=read.table(B_perf_att_length_summary_multi_path,sep=',',header= T, na.strings="?")
B_perf_ORE_att_length_perdata_multi_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_att_length_perdata_multi.csv"
write.csv(B_perf_ORE_att_length_perdata_multi,B_perf_ORE_att_length_perdata_multi_path, row.names = FALSE)
testF7=read.table(B_perf_ORE_att_length_perdata_multi_path,sep=',',header= T, na.strings="?")

# ...............................
# rules length
# .....................................
# binary classification
B_perf_ORE_summarycomplex_data_based_bin = sqldf('select  pred_set,pred_sub_set, method, 
                          avg(mean_rules_nbr) as mean_rules_nbr, 
                          min(mean_rules_nbr) as min_rules_nbr, 
                          max(mean_rules_nbr) as max_rules_nbr,
                          stdev(mean_rules_nbr) as std_rules_nbr,
                          stdev(mean_rules_nbr)/sqrt(count(mean_rules_nbr)) as SE_rules_nbr,
                          avg(mean_class_rules_nbr) as mean_class_rules_nbr, 
                          min(mean_class_rules_nbr) as min_class_rules_nbr,
                          max(mean_class_rules_nbr) as max_class_rules_nbr,
                          stdev(mean_class_rules_nbr) as std_class_rules_nbr,
                          stdev(mean_class_rules_nbr)/sqrt(count(mean_class_rules_nbr)) as SE_class_rules_nbr,
                          avg(mean_coverage) as mean_coverage, 
                          min(mean_coverage) as min_coverage,
                          max(mean_coverage) as max_coverage,
                          stdev(mean_coverage) as std_coverage,
                          stdev(mean_coverage)/sqrt(count(mean_coverage)) as SE_coverage
                          from B_perf_ORE_summary_perdata_bin Where pred_set ="test" and pred_sub_set="covered" group by method')
B_perf_ORE_summarycomplex_data_based_bin[,-c(1,2,3)]=round(B_perf_ORE_summarycomplex_data_based_bin[,-c(1,2,3)],1)

B_perf_rule_length_summary_bin_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_rule_length_summary_bin.csv"
write.csv(B_perf_ORE_summarycomplex_data_based_bin,B_perf_rule_length_summary_bin_path, row.names = FALSE)
# multiclass classification
B_perf_ORE_summarycomplex_data_based_multi = sqldf('select  pred_set,pred_sub_set, method, 
                          avg(mean_rules_nbr) as mean_rules_nbr, 
                          min(mean_rules_nbr) as min_rules_nbr, 
                          max(mean_rules_nbr) as max_rules_nbr,
                          stdev(mean_rules_nbr) as std_rules_nbr,
                          stdev(mean_rules_nbr)/sqrt(count(mean_rules_nbr)) as SE_rules_nbr,
                          avg(mean_class_rules_nbr) as mean_class_rules_nbr, 
                          min(mean_class_rules_nbr) as min_class_rules_nbr,
                          max(mean_class_rules_nbr) as max_class_rules_nbr,
                          stdev(mean_class_rules_nbr) as std_class_rules_nbr,
                          stdev(mean_class_rules_nbr)/sqrt(count(mean_class_rules_nbr)) as SE_class_rules_nbr,
                          avg(mean_coverage) as mean_coverage, 
                          min(mean_coverage) as min_coverage,
                          max(mean_coverage) as max_coverage,
                          stdev(mean_coverage) as std_coverage,
                          stdev(mean_coverage)/sqrt(count(mean_coverage)) as SE_coverage
                          from B_perf_ORE_summary_perdata_multi Where pred_set ="test" and pred_sub_set="covered" group by method')
B_perf_ORE_summarycomplex_data_based_multi[,-c(1,2,3)]=round(B_perf_ORE_summarycomplex_data_based_multi[,-c(1,2,3)],1)

B_perf_rule_length_summary_multi_path= ".\\benchmark_results\\rules_results\\B_perf_ORE_rule_length_summary_multi.csv"
write.csv(B_perf_ORE_summarycomplex_data_based_multi,B_perf_rule_length_summary_multi_path, row.names = FALSE)

# ....................................

# XOR perf
# .........................
names(XOR_perf11)
XOR_perf11= B_perf_ORE_summary_perdata_bin %>% 
  filter(data=="XOR" & pred_set=="test" & pred_sub_set %in% c("all")) %>%
  select("method", "mean_acc", "SE_acc", "mean_F1", "SE_F1",  "mean_Kappa", "SE_kappa")
XOR_perf11[,-1]= round(XOR_perf11[,-1],2)
XOR_perf12= B_perf_ORE_summary_perdata_bin %>% 
  filter(data=="XOR" & pred_set=="test" & pred_sub_set %in% c("covered")) %>%
  select("method", "mean_rules_nbr" ,"SE_rules_nbr", "mean_class_rules_nbr", "SE_class_rules_nbr","mean_coverage", "SE_coverage")
XOR_perf12[,c(2,4)]=round(XOR_perf12[,c(2,4)],1)
XOR_perf12[,c(3,5,6,7)]=round(XOR_perf12[,c(3,5,6,7)],2)
XOR_perf2= B_perf_ORE_att_length_perdata %>%
  filter(data=="XOR") %>%
  select ("method","mean_att_nbr","SE_att_nbr")
XOR_perf2[,2] = round(XOR_perf2[,2],1)
XOR_perf2[,3] = round(XOR_perf2[,3],2)
XOR_perf= sqldf('select XOR_perf12.* , XOR_perf2.* 
                from XOR_perf12 left join XOR_perf2 on XOR_perf12.method=XOR_perf2.method')
XOR_perf=XOR_perf[,-8]
XOR_perf= sqldf('select XOR_perf.* , XOR_perf11.* 
                from XOR_perf left join XOR_perf11 on XOR_perf.method=XOR_perf11.method')
XOR_perf= XOR_perf[,-10]
XOR_perf_path= ".\\benchmark_results\\rules_results\\XOR_perf.csv"
write.csv(XOR_perf,XOR_perf_path, row.names = FALSE)
testF8=read.table(XOR_perf_path,sep=',',header= T, na.strings="?")

# wilcoxon score
names(B_perf_all_iter)
meth_lev=c("RPART", "RF", "Pre-Forest-ORE", "Forest-ORE","GRRFR", "STEL", "Forest-ORE+STEL")          

B_perf_all_iter=B_perf_all_iter %>% filter(method %in%meth_lev)
Dataperf0=B_perf_all_iter[which(B_perf_all_iter[,"pred_set"]=="test"),]

# wilcox test score in Accuracy
# Binary classification

Att_perf="accuracy"

# accuracy_all
# Dataperf=B_perf_all_iter[which(B_perf_all_iter[,"pred_set"]=="test" & B_perf_all_iter[,"pred_sub_set"]=="all"),]
Dataperf=B_perf_all_iter_bin[which(B_perf_all_iter_bin[,"pred_set"]=="test" & B_perf_all_iter_bin[,"pred_sub_set"]=="all"),]
unique(Dataperf$pred_set)

L=unique(Dataperf$method)
LL=1:length(L)

wilcox_accuracy= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_accuracy)=L
colnames(wilcox_accuracy)=L
wilcox_accuracy_pval=wilcox_accuracy
wilcox_accuracy_shift=wilcox_accuracy
wilcox_accuracy_contint_min=wilcox_accuracy
wilcox_accuracy_contint_max=wilcox_accuracy
wilcox_accuracy_score=wilcox_accuracy
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
    {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_accuracy_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_accuracy_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_accuracy_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_accuracy_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
    }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_accuracy_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=0
    } else if (wilcox_accuracy_contint_min[id_method_A,id_method_B] > 0 & wilcox_accuracy_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=1
    } else if (wilcox_accuracy_contint_min[id_method_A,id_method_B] < 0 & wilcox_accuracy_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_accuracy=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_accuracy)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_accuracy[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_accuracy[i,"Wins_count"]=length(which(wilcox_accuracy_score[i,]==1)) 
  count_wilcox_accuracy[i,"Ties_count"]=length(which(wilcox_accuracy_score[i,]==0))
  count_wilcox_accuracy[i,"Loss_count"]=length(which(wilcox_accuracy_score[i,]==-1))
}
count_wilcox_accuracy_bin = count_wilcox_accuracy
# accuracy_covered

Dataperf=B_perf_all_iter_bin[which(B_perf_all_iter_bin[,"pred_set"]=="test" & B_perf_all_iter_bin[,"pred_sub_set"]=="covered"),]
unique(Dataperf$pred_set)

L=unique(Dataperf$method)
LL=1:length(L)
# wilcox test score in Accuracy
Att_perf="accuracy"
wilcox_accuracy_cov= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_accuracy_cov)=L
colnames(wilcox_accuracy_cov)=L
wilcox_accuracy_cov_pval=wilcox_accuracy_cov
wilcox_accuracy_cov_shift=wilcox_accuracy_cov
wilcox_accuracy_cov_contint_min=wilcox_accuracy_cov
wilcox_accuracy_cov_contint_max=wilcox_accuracy_cov
wilcox_accuracy_cov_score=wilcox_accuracy_cov
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_accuracy_cov_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_accuracy_cov_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_accuracy_cov_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_accuracy_cov_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_accuracy_cov_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=0
    } else if (wilcox_accuracy_cov_contint_min[id_method_A,id_method_B] > 0 & wilcox_accuracy_cov_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=1
    } else if (wilcox_accuracy_cov_contint_min[id_method_A,id_method_B] < 0 & wilcox_accuracy_cov_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_accuracy_cov=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_accuracy_cov)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_accuracy_cov[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_accuracy_cov[i,"Wins_count"]=length(which(wilcox_accuracy_cov_score[i,]==1)) 
  count_wilcox_accuracy_cov[i,"Ties_count"]=length(which(wilcox_accuracy_cov_score[i,]==0))
  count_wilcox_accuracy_cov[i,"Loss_count"]=length(which(wilcox_accuracy_cov_score[i,]==-1))
}

count_wilcox_accuracy_cov_bin=count_wilcox_accuracy_cov

wilcox_accuracy_pval
wilcox_accuracy_shift
wilcox_accuracy_contint_min
wilcox_accuracy_contint_max
wilcox_accuracy_score
count_wilcox_accuracy
# write.csv(wilcox_accuracy_pval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_pval.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_shift,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_shift.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_contint_min,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_min.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_contint_max,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_max.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_score,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_score.csv", row.names = FALSE)
# write.csv(count_wilcox_accuracy,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_accuracy.csv", row.names = FALSE)

# wilcox test score in F1
Att_perf="macroF1"

# macroF1_all
Dataperf=B_perf_all_iter_bin[which(B_perf_all_iter_bin[,"pred_set"]=="test" & B_perf_all_iter_bin[,"pred_sub_set"]=="all"),]
unique(Dataperf$pred_set)

L=unique(Dataperf$method)
LL=1:length(L)

wilcox_macroF1= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_macroF1)=L
colnames(wilcox_macroF1)=L
wilcox_macroF1_pval=wilcox_macroF1
wilcox_macroF1_shift=wilcox_macroF1
wilcox_macroF1_contint_min=wilcox_macroF1
wilcox_macroF1_contint_max=wilcox_macroF1
wilcox_macroF1_score=wilcox_macroF1
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_macroF1_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_macroF1_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_macroF1_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_macroF1_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_macroF1_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=0
    } else if (wilcox_macroF1_contint_min[id_method_A,id_method_B] > 0 & wilcox_macroF1_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=1
    } else if (wilcox_macroF1_contint_min[id_method_A,id_method_B] < 0 & wilcox_macroF1_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_macroF1=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_macroF1)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_macroF1[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_macroF1[i,"Wins_count"]=length(which(wilcox_macroF1_score[i,]==1)) 
  count_wilcox_macroF1[i,"Ties_count"]=length(which(wilcox_macroF1_score[i,]==0))
  count_wilcox_macroF1[i,"Loss_count"]=length(which(wilcox_macroF1_score[i,]==-1))
}
count_wilcox_macroF1_bin = count_wilcox_macroF1
# macroF1_covered

Dataperf=B_perf_all_iter_bin[which(B_perf_all_iter_bin[,"pred_set"]=="test" & B_perf_all_iter_bin[,"pred_sub_set"]=="covered"),]
unique(Dataperf$pred_set)

L=unique(Dataperf$method)
LL=1:length(L)
# wilcox test score in macroF1
Att_perf="macroF1"
wilcox_macroF1_cov= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_macroF1_cov)=L
colnames(wilcox_macroF1_cov)=L
wilcox_macroF1_cov_pval=wilcox_macroF1_cov
wilcox_macroF1_cov_shift=wilcox_macroF1_cov
wilcox_macroF1_cov_contint_min=wilcox_macroF1_cov
wilcox_macroF1_cov_contint_max=wilcox_macroF1_cov
wilcox_macroF1_cov_score=wilcox_macroF1_cov
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_macroF1_cov_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_macroF1_cov_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_macroF1_cov_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_macroF1_cov_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_macroF1_cov_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=0
    } else if (wilcox_macroF1_cov_contint_min[id_method_A,id_method_B] > 0 & wilcox_macroF1_cov_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=1
    } else if (wilcox_macroF1_cov_contint_min[id_method_A,id_method_B] < 0 & wilcox_macroF1_cov_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_macroF1_cov=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_macroF1_cov)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_macroF1_cov[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_macroF1_cov[i,"Wins_count"]=length(which(wilcox_macroF1_cov_score[i,]==1)) 
  count_wilcox_macroF1_cov[i,"Ties_count"]=length(which(wilcox_macroF1_cov_score[i,]==0))
  count_wilcox_macroF1_cov[i,"Loss_count"]=length(which(wilcox_macroF1_cov_score[i,]==-1))
}

count_wilcox_macroF1_cov_bin = count_wilcox_macroF1_cov

# wilcox test score in kappa
Att_perf="kappa"
# kappa_all
Dataperf=B_perf_all_iter_bin[which(B_perf_all_iter_bin[,"pred_set"]=="test" & B_perf_all_iter_bin[,"pred_sub_set"]=="all"),]
unique(Dataperf$pred_set)

L=unique(Dataperf$method)
LL=1:length(L)

wilcox_kappa= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_kappa)=L
colnames(wilcox_kappa)=L
wilcox_kappa_pval=wilcox_kappa
wilcox_kappa_shift=wilcox_kappa
wilcox_kappa_contint_min=wilcox_kappa
wilcox_kappa_contint_max=wilcox_kappa
wilcox_kappa_score=wilcox_kappa
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_kappa_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_kappa_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_kappa_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_kappa_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_kappa_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_kappa_score[id_method_A,id_method_B]=0
    } else if (wilcox_kappa_contint_min[id_method_A,id_method_B] > 0 & wilcox_kappa_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_kappa_score[id_method_A,id_method_B]=1
    } else if (wilcox_kappa_contint_min[id_method_A,id_method_B] < 0 & wilcox_kappa_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_kappa_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_kappa_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_kappa=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_kappa)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_kappa[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_kappa[i,"Wins_count"]=length(which(wilcox_kappa_score[i,]==1)) 
  count_wilcox_kappa[i,"Ties_count"]=length(which(wilcox_kappa_score[i,]==0))
  count_wilcox_kappa[i,"Loss_count"]=length(which(wilcox_kappa_score[i,]==-1))
}

count_wilcox_kappa_bin = count_wilcox_kappa

# kappa_covered

Dataperf=B_perf_all_iter_bin[which(B_perf_all_iter_bin[,"pred_set"]=="test" & B_perf_all_iter_bin[,"pred_sub_set"]=="covered"),]
unique(Dataperf$pred_set)
str(Dataperf$kappa)
L=unique(Dataperf$method)
LL=1:length(L)
# wilcox test score in kappa
Att_perf="kappa"
wilcox_kappa_cov= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_kappa_cov)=L
colnames(wilcox_kappa_cov)=L
wilcox_kappa_cov_pval=wilcox_kappa_cov
wilcox_kappa_cov_shift=wilcox_kappa_cov
wilcox_kappa_cov_contint_min=wilcox_kappa_cov
wilcox_kappa_cov_contint_max=wilcox_kappa_cov
wilcox_kappa_cov_score=wilcox_kappa_cov
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_kappa_cov_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_kappa_cov_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_kappa_cov_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_kappa_cov_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_kappa_cov_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=0
    } else if (wilcox_kappa_cov_contint_min[id_method_A,id_method_B] > 0 & wilcox_kappa_cov_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=1
    } else if (wilcox_kappa_cov_contint_min[id_method_A,id_method_B] < 0 & wilcox_kappa_cov_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_kappa_cov=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_kappa_cov)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_kappa_cov[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_kappa_cov[i,"Wins_count"]=length(which(wilcox_kappa_cov_score[i,]==1)) 
  count_wilcox_kappa_cov[i,"Ties_count"]=length(which(wilcox_kappa_cov_score[i,]==0))
  count_wilcox_kappa_cov[i,"Loss_count"]=length(which(wilcox_kappa_cov_score[i,]==-1))
}

count_wilcox_kappa_cov_bin = count_wilcox_kappa_cov

# save files
for (perf_indic in c("accuracy","accuracy_cov", "macroF1", "macroF1_cov", "kappa", "kappa_cov")) 
{
  file1 = paste0("wilcox_",perf_indic,"_pval")
  file2 = paste0("wilcox_",perf_indic,"_shift")
  file3 = paste0("wilcox_",perf_indic,"_contint_min")
  file4 = paste0("wilcox_",perf_indic,"_contint_max")
  file5 = paste0("wilcox_",perf_indic,"_score")
  file6 = paste0("count_wilcox_",perf_indic)
  for (file in c(file1, file2, file3, file4,file5, file6)) 
  {
    file_path= paste0(".\\benchmark_results\\rules_results\\wilcox\\",file,"_bin",".csv")
    write.csv(eval(parse(text=file)),file_path, row.names = FALSE)
  }
}



# Multi class classification

# wilcox test score in Accuracy


Att_perf="accuracy"

# accuracy_all
# Dataperf=B_perf_all_iter[which(B_perf_all_iter[,"pred_set"]=="test" & B_perf_all_iter[,"pred_sub_set"]=="all"),]
Dataperf=B_perf_all_iter_multi[which(B_perf_all_iter_multi[,"pred_set"]=="test" & B_perf_all_iter_multi[,"pred_sub_set"]=="all"),]
Dataperf = Dataperf %>% filter (data != "VOWEL")
unique(Dataperf$pred_set)

L=unique(Dataperf$method)
LL=1:length(L)

wilcox_accuracy= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_accuracy)=L
colnames(wilcox_accuracy)=L
wilcox_accuracy_pval=wilcox_accuracy
wilcox_accuracy_shift=wilcox_accuracy
wilcox_accuracy_contint_min=wilcox_accuracy
wilcox_accuracy_contint_max=wilcox_accuracy
wilcox_accuracy_score=wilcox_accuracy
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_accuracy_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_accuracy_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_accuracy_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_accuracy_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_accuracy_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=0
    } else if (wilcox_accuracy_contint_min[id_method_A,id_method_B] > 0 & wilcox_accuracy_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=1
    } else if (wilcox_accuracy_contint_min[id_method_A,id_method_B] < 0 & wilcox_accuracy_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_accuracy_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_accuracy=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_accuracy)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_accuracy[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_accuracy[i,"Wins_count"]=length(which(wilcox_accuracy_score[i,]==1)) 
  count_wilcox_accuracy[i,"Ties_count"]=length(which(wilcox_accuracy_score[i,]==0))
  count_wilcox_accuracy[i,"Loss_count"]=length(which(wilcox_accuracy_score[i,]==-1))
}
count_wilcox_accuracy_multi = count_wilcox_accuracy
# accuracy_covered

Dataperf=B_perf_all_iter_multi[which(B_perf_all_iter_multi[,"pred_set"]=="test" & B_perf_all_iter_multi[,"pred_sub_set"]=="covered"),]
unique(Dataperf$pred_set)
Dataperf = Dataperf %>% filter (data != "VOWEL")
L=unique(Dataperf$method)
LL=1:length(L)
# wilcox test score in Accuracy
Att_perf="accuracy"
wilcox_accuracy_cov= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_accuracy_cov)=L
colnames(wilcox_accuracy_cov)=L
wilcox_accuracy_cov_pval=wilcox_accuracy_cov
wilcox_accuracy_cov_shift=wilcox_accuracy_cov
wilcox_accuracy_cov_contint_min=wilcox_accuracy_cov
wilcox_accuracy_cov_contint_max=wilcox_accuracy_cov
wilcox_accuracy_cov_score=wilcox_accuracy_cov
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_accuracy_cov_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_accuracy_cov_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_accuracy_cov_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_accuracy_cov_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_accuracy_cov_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=0
    } else if (wilcox_accuracy_cov_contint_min[id_method_A,id_method_B] > 0 & wilcox_accuracy_cov_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=1
    } else if (wilcox_accuracy_cov_contint_min[id_method_A,id_method_B] < 0 & wilcox_accuracy_cov_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_accuracy_cov_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_accuracy_cov=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_accuracy_cov)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_accuracy_cov[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_accuracy_cov[i,"Wins_count"]=length(which(wilcox_accuracy_cov_score[i,]==1)) 
  count_wilcox_accuracy_cov[i,"Ties_count"]=length(which(wilcox_accuracy_cov_score[i,]==0))
  count_wilcox_accuracy_cov[i,"Loss_count"]=length(which(wilcox_accuracy_cov_score[i,]==-1))
}

count_wilcox_accuracy_cov_multi=count_wilcox_accuracy_cov


# write.csv(wilcox_accuracy_pval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_pval.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_shift,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_shift.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_contint_min,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_min.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_contint_max,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_max.csv", row.names = FALSE)
# write.csv(wilcox_accuracy_score,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_score.csv", row.names = FALSE)
# write.csv(count_wilcox_accuracy,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_accuracy.csv", row.names = FALSE)

# wilcox test score in F1
Att_perf="macroF1"

# macroF1_all
Dataperf=B_perf_all_iter_multi[which(B_perf_all_iter_multi[,"pred_set"]=="test" & B_perf_all_iter_multi[,"pred_sub_set"]=="all"),]
Dataperf = Dataperf %>% filter (data != "VOWEL")
unique(Dataperf$pred_set)

L=unique(Dataperf$method)
LL=1:length(L)

wilcox_macroF1= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_macroF1)=L
colnames(wilcox_macroF1)=L
wilcox_macroF1_pval=wilcox_macroF1
wilcox_macroF1_shift=wilcox_macroF1
wilcox_macroF1_contint_min=wilcox_macroF1
wilcox_macroF1_contint_max=wilcox_macroF1
wilcox_macroF1_score=wilcox_macroF1
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_macroF1_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_macroF1_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_macroF1_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_macroF1_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_macroF1_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=0
    } else if (wilcox_macroF1_contint_min[id_method_A,id_method_B] > 0 & wilcox_macroF1_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=1
    } else if (wilcox_macroF1_contint_min[id_method_A,id_method_B] < 0 & wilcox_macroF1_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_macroF1_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_macroF1=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_macroF1)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_macroF1[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_macroF1[i,"Wins_count"]=length(which(wilcox_macroF1_score[i,]==1)) 
  count_wilcox_macroF1[i,"Ties_count"]=length(which(wilcox_macroF1_score[i,]==0))
  count_wilcox_macroF1[i,"Loss_count"]=length(which(wilcox_macroF1_score[i,]==-1))
}
count_wilcox_macroF1_multi = count_wilcox_macroF1
# macroF1_covered

Dataperf=B_perf_all_iter_multi[which(B_perf_all_iter_multi[,"pred_set"]=="test" & B_perf_all_iter_multi[,"pred_sub_set"]=="covered"),]
unique(Dataperf$pred_set)
Dataperf = Dataperf %>% filter (data != "VOWEL")
L=unique(Dataperf$method)
LL=1:length(L)
# wilcox test score in macroF1
Att_perf="macroF1"
wilcox_macroF1_cov= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_macroF1_cov)=L
colnames(wilcox_macroF1_cov)=L
wilcox_macroF1_cov_pval=wilcox_macroF1_cov
wilcox_macroF1_cov_shift=wilcox_macroF1_cov
wilcox_macroF1_cov_contint_min=wilcox_macroF1_cov
wilcox_macroF1_cov_contint_max=wilcox_macroF1_cov
wilcox_macroF1_cov_score=wilcox_macroF1_cov
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_macroF1_cov_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_macroF1_cov_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_macroF1_cov_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_macroF1_cov_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_macroF1_cov_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=0
    } else if (wilcox_macroF1_cov_contint_min[id_method_A,id_method_B] > 0 & wilcox_macroF1_cov_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=1
    } else if (wilcox_macroF1_cov_contint_min[id_method_A,id_method_B] < 0 & wilcox_macroF1_cov_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_macroF1_cov_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_macroF1_cov=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_macroF1_cov)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_macroF1_cov[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_macroF1_cov[i,"Wins_count"]=length(which(wilcox_macroF1_cov_score[i,]==1)) 
  count_wilcox_macroF1_cov[i,"Ties_count"]=length(which(wilcox_macroF1_cov_score[i,]==0))
  count_wilcox_macroF1_cov[i,"Loss_count"]=length(which(wilcox_macroF1_cov_score[i,]==-1))
}

count_wilcox_macroF1_cov_multi = count_wilcox_macroF1_cov

# wilcox test score in kappa
Att_perf="kappa"
# kappa_all
Dataperf=B_perf_all_iter_multi[which(B_perf_all_iter_multi[,"pred_set"]=="test" & B_perf_all_iter_multi[,"pred_sub_set"]=="all"),]
unique(Dataperf$pred_set)
Dataperf = Dataperf %>% filter (data != "VOWEL")
L=unique(Dataperf$method)
LL=1:length(L)

wilcox_kappa= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_kappa)=L
colnames(wilcox_kappa)=L
wilcox_kappa_pval=wilcox_kappa
wilcox_kappa_shift=wilcox_kappa
wilcox_kappa_contint_min=wilcox_kappa
wilcox_kappa_contint_max=wilcox_kappa
wilcox_kappa_score=wilcox_kappa
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_kappa_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_kappa_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_kappa_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_kappa_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_kappa_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_kappa_score[id_method_A,id_method_B]=0
    } else if (wilcox_kappa_contint_min[id_method_A,id_method_B] > 0 & wilcox_kappa_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_kappa_score[id_method_A,id_method_B]=1
    } else if (wilcox_kappa_contint_min[id_method_A,id_method_B] < 0 & wilcox_kappa_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_kappa_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_kappa_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_kappa=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_kappa)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_kappa[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_kappa[i,"Wins_count"]=length(which(wilcox_kappa_score[i,]==1)) 
  count_wilcox_kappa[i,"Ties_count"]=length(which(wilcox_kappa_score[i,]==0))
  count_wilcox_kappa[i,"Loss_count"]=length(which(wilcox_kappa_score[i,]==-1))
}

count_wilcox_kappa_multi = count_wilcox_kappa

# kappa_covered

Dataperf=B_perf_all_iter_multi[which(B_perf_all_iter_multi[,"pred_set"]=="test" & B_perf_all_iter_multi[,"pred_sub_set"]=="covered"),]
unique(Dataperf$pred_set)
Dataperf = Dataperf %>% filter (data != "VOWEL")
Dataperf = Dataperf %>%mutate(method=factor(method))
str(Dataperf$kappa)
L=unique(Dataperf$method)
LL=1:length(L)
# wilcox test score in kappa
Att_perf="kappa"
wilcox_kappa_cov= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_kappa_cov)=L
colnames(wilcox_kappa_cov)=L
wilcox_kappa_cov_pval=wilcox_kappa_cov
wilcox_kappa_cov_shift=wilcox_kappa_cov
wilcox_kappa_cov_contint_min=wilcox_kappa_cov
wilcox_kappa_cov_contint_max=wilcox_kappa_cov
wilcox_kappa_cov_score=wilcox_kappa_cov
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_kappa_cov_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_kappa_cov_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_kappa_cov_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_kappa_cov_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_kappa_cov_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=0
    } else if (wilcox_kappa_cov_contint_min[id_method_A,id_method_B] > 0 & wilcox_kappa_cov_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=1
    } else if (wilcox_kappa_cov_contint_min[id_method_A,id_method_B] < 0 & wilcox_kappa_cov_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_kappa_cov_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_kappa_cov=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_kappa_cov)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_kappa_cov[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_kappa_cov[i,"Wins_count"]=length(which(wilcox_kappa_cov_score[i,]==1)) 
  count_wilcox_kappa_cov[i,"Ties_count"]=length(which(wilcox_kappa_cov_score[i,]==0))
  count_wilcox_kappa_cov[i,"Loss_count"]=length(which(wilcox_kappa_cov_score[i,]==-1))
}

count_wilcox_kappa_cov_multi = count_wilcox_kappa_cov

# coverage
# 
table(Dataperf$method, Dataperf$coverage)
Dataperf=B_perf_all_iter_multi[which(B_perf_all_iter_multi[,"pred_set"]=="test" & B_perf_all_iter_multi[,"pred_sub_set"]=="covered"),]
unique(Dataperf$pred_set)
Dataperf = Dataperf %>% filter (data != "VOWEL") 
Dataperf = Dataperf %>%mutate(method=factor(method))
Dataperf$coverage=round(Dataperf$coverage,3)
L=unique(Dataperf$method)
LL=1:length(L)
names(Dataperf)
Att_perf="coverage"
table(Dataperf$method, Dataperf$iter)
wilcox_coverage_cov= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_coverage_cov)=L
colnames(wilcox_coverage_cov)=L
wilcox_coverage_cov_pval=wilcox_coverage_cov
wilcox_coverage_cov_shift=wilcox_coverage_cov
wilcox_coverage_cov_contint_min=wilcox_coverage_cov
wilcox_coverage_cov_contint_max=wilcox_coverage_cov
wilcox_coverage_cov_score=wilcox_coverage_cov
for (id_method_A in (1:length(L)))
{
  A_name=L[id_method_A]
  A=Dataperf[which(Dataperf[,"method"]==A_name),Att_perf]
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    B_name=L[id_method_B]
    B=Dataperf[which(Dataperf[,"method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_coverage_cov_pval[id_method_A,id_method_B]=wilcox_AB$p.value
    wilcox_coverage_cov_shift[id_method_A,id_method_B]=wilcox_AB$estimate
    wilcox_coverage_cov_contint_min[id_method_A,id_method_B]=wilcox_AB$conf.int[1]
    wilcox_coverage_cov_contint_max[id_method_A,id_method_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_method_A in (1:length(L)))
{
  
  for (id_method_B in setdiff(LL,id_method_A)) 
  {
    if (wilcox_coverage_cov_pval[id_method_A,id_method_B] >= 0.05) 
    {
      wilcox_coverage_cov_score[id_method_A,id_method_B]=0
    } else if (wilcox_coverage_cov_contint_min[id_method_A,id_method_B] > 0 & wilcox_coverage_cov_contint_max[id_method_A,id_method_B]> 0 ) 
    {
      wilcox_coverage_cov_score[id_method_A,id_method_B]=1
    } else if (wilcox_coverage_cov_contint_min[id_method_A,id_method_B] < 0 & wilcox_coverage_cov_contint_max[id_method_A,id_method_B]< 0 )
    {
      wilcox_coverage_cov_score[id_method_A,id_method_B]=-1  
    }else 
    {
      wilcox_coverage_cov_score[id_method_A,id_method_B]=0     
    }
  }
}
count_wilcox_coverage_cov=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_coverage_cov)=c("Method","Wins_count","Ties_count","Loss_count")
count_wilcox_coverage_cov[,"Method"]=L
for (i in (1:length(L)))
{
  count_wilcox_coverage_cov[i,"Wins_count"]=length(which(wilcox_coverage_cov_score[i,]==1)) 
  count_wilcox_coverage_cov[i,"Ties_count"]=length(which(wilcox_coverage_cov_score[i,]==0))
  count_wilcox_coverage_cov[i,"Loss_count"]=length(which(wilcox_coverage_cov_score[i,]==-1))
}

count_wilcox_coverage_cov_multi=count_wilcox_coverage_cov

# save files
for (perf_indic in c("accuracy","accuracy_cov", "macroF1", "macroF1_cov", "kappa", "kappa_cov","coverage")) 
{
  file1 = paste0("wilcox_",perf_indic,"_pval")
  file2 = paste0("wilcox_",perf_indic,"_shift")
  file3 = paste0("wilcox_",perf_indic,"_contint_min")
  file4 = paste0("wilcox_",perf_indic,"_contint_max")
  file5 = paste0("wilcox_",perf_indic,"_score")
  file6 = paste0("count_wilcox_",perf_indic)
  for (file in c(file1, file2, file3, file4,file5, file6)) 
  {
    file_path= paste0(".\\benchmark_results\\rules_results\\wilcox\\",file,"_multi",".csv")
    write.csv(eval(parse(text=file)),file_path, row.names = FALSE)
  }
}



filePaths_exetime = grep(list.files(path="./Rules_results", pattern='_exe_time.csv',full.names = TRUE), pattern='_avg_',invert=TRUE, value=TRUE)

B_exetime_all_iter <- do.call(rbind, lapply(filePaths_exetime, read.csv))
names(B_exetime_all_iter)
# table(B_perf_all_iter$pred_set)
# head(B_perf_all_iter)
# names(B_perf_all_iter)
B_exetime_all_iter$data =toupper(B_exetime_all_iter$data)
B_exetime_all_iter$data[which(B_exetime_all_iter$data=="BCO")] ="BRCANCER"
# B_exetime_all_iter=B_exetime_all_iter[,-7]
colnames(B_exetime_all_iter) = c("Data","iter","extract_rules","preselect_rules",
                                 "prepare_opt_input","run_opt","RF","RPART","STEL")         
# unique(B_perf_all_iter$method)
B_exetime_ORE_summary_perdata = sqldf('select Data,
                          avg(extract_rules) as mean_extract_rules, 
                          stdev(extract_rules) as std_extract_rules,
                          stdev(extract_rules)/sqrt(count(extract_rules)) as SE_extract_rules,
                          avg(preselect_rules) as mean_preselect_rules, 
                          stdev(preselect_rules) as std_preselect_rules,
                          stdev(preselect_rules)/sqrt(count(preselect_rules)) as SE_preselect_rules,
                          avg(preselect_rules) as mean_preselect_rules, 
                          stdev(preselect_rules) as std_preselect_rules,
                          stdev(preselect_rules)/sqrt(count(preselect_rules)) as SE_preselect_rules,
                          avg(prepare_opt_input) as mean_prepare_opt_input, 
                          stdev(prepare_opt_input) as std_prepare_opt_input, 
                          stdev(prepare_opt_input)/sqrt(count(prepare_opt_input)) as SE_prepare_opt_input,
                          avg(run_opt) as mean_run_opt, 
                          stdev(run_opt) as std_run_opt, 
                          stdev(run_opt)/sqrt(count(run_opt)) as SE_run_opt,
                          avg(RF) as mean_RF, 
                          stdev(RF) as std_RF, 
                          stdev(RF)/sqrt(count(RF)) as SE_RF,
                          avg(RPART) as mean_RPART, 
                          stdev(RPART) as std_RPART,
                          stdev(RPART)/sqrt(count(RPART)) as SE_RPART,
                          avg(STEL) as mean_STEL, 
                          stdev(STEL) as std_STEL, 
                          stdev(STEL)/sqrt(count(STEL)) as SE_STEL
                          from B_exetime_all_iter group by Data')

B_exetime_ORE_summary_perdata = sqldf('select Data,
                          avg(extract_rules) as mean_extract_rules, 
                          
                          avg(preselect_rules) as mean_preselect_rules, 
                         
                          avg(preselect_rules) as mean_preselect_rules, 
                          
                          avg(prepare_opt_input) as mean_prepare_opt_input, 
                          
                          avg(run_opt) as mean_run_opt, 
                         
                          avg(RF) as mean_RF, 
                          
                          avg(RPART) as mean_RPART, 
                         
                          avg(STEL) as mean_STEL 
                          
                          from B_exetime_all_iter group by Data')
B_exetime_ORE_summary_perdata = sqldf('select Data,
                          avg(extract_rules) as mean_extract_rules, 
                          
                          stdev(extract_rules)/sqrt(count(extract_rules)) as SE_extract_rules,
                          avg(preselect_rules) as mean_preselect_rules, 
                          
                          stdev(preselect_rules)/sqrt(count(preselect_rules)) as SE_preselect_rules,
                          avg(preselect_rules) as mean_preselect_rules, 
                          
                          stdev(preselect_rules)/sqrt(count(preselect_rules)) as SE_preselect_rules,
                          avg(prepare_opt_input) as mean_prepare_opt_input, 
                          
                          stdev(prepare_opt_input)/sqrt(count(prepare_opt_input)) as SE_prepare_opt_input,
                          avg(run_opt) as mean_run_opt, 
                          
                          stdev(run_opt)/sqrt(count(run_opt)) as SE_run_opt,
                          median(run_opt) as median_run_opt,
                          avg(RF) as mean_RF, 
                          
                          stdev(RF)/sqrt(count(RF)) as SE_RF,
                          avg(RPART) as mean_RPART, 
                          
                          stdev(RPART)/sqrt(count(RPART)) as SE_RPART,
                          avg(STEL) as mean_STEL, 
                          
                          stdev(STEL)/sqrt(count(STEL)) as SE_STEL,
                          median(STEL) as median_STEL
                          from B_exetime_all_iter group by Data')

B_exetime_ORE_path= ".\\benchmark_results\\rules_results\\B_exetime_ORE_perdata.csv"
write.csv(B_exetime_ORE_summary_perdata,B_exetime_ORE_path, row.names = FALSE)
B_exetime_ORE_path= ".\\benchmark_results\\rules_results\\B_exetime_ORE_perdata.csv"
write.csv(B_exetime_ORE_summary_perdata,B_exetime_ORE_path, row.names = FALSE)