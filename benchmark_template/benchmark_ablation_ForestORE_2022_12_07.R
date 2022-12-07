wd <- getwd()
setwd(wd)

library(sqldf)
library(reshape2)
library(tidyverse)


# ..............................................................................................................

# ..............................................................................................................
filePaths_prediction_perf_abl = grep(list.files(path="./Rules_results_abl", pattern='_prediction_perf_abl.csv',full.names = TRUE), pattern='_avg_', invert=TRUE, value=TRUE)

B_perf_all_iter_abl <- do.call(rbind, lapply(filePaths_prediction_perf_abl, read.csv))
# table(B_perf_all_iter_init$pred_set)
# head(B_perf_all_iter_init)
# names(B_perf_all_iter_init)
B_perf_all_iter_abl$data =toupper(B_perf_all_iter_abl$data)
B_perf_all_iter_abl$data[which(B_perf_all_iter_abl$data=="BCO")] ="BRCANCER"
# unique(B_perf_all_iter_abl$method)
B_perf_all_iter_abl$class_rules_nbr=B_perf_all_iter_abl$rules_nbr
names(B_perf_all_iter_abl)
unique(B_perf_all_iter_abl$method)
unique(B_perf_all_iter_abl$abl_model)
B_perf_all_iter_abl$method=dplyr::recode(B_perf_all_iter_abl$method,
                                          OptimalRules="Forest-ORE",
                                          OPTSTEL="Forest-ORE+STEL")
B_perf_all_iter_abl$method=recode_factor(B_perf_all_iter_abl$method,
                                          OptimalRules="Forest-ORE",
                                          OPTSTEL="Forest-ORE+STEL")

str(B_data_summary)
for (data_name in unique(B_perf_all_iter_abl$data))
{
  print(data_name)
  data_rows=which(B_perf_all_iter_abl$data==data_name & B_perf_all_iter_abl$pred_sub_set %in% c("all","covered"))
  nclasses=as.numeric(B_data_summary[which(B_data_summary[,"Dataset"]==data_name),"NB_classes"])
  B_perf_all_iter_abl[data_rows,"class_rules_nbr"]= B_perf_all_iter_abl[data_rows,"class_rules_nbr"] / nclasses
}




B_perf_all_iter_abl=B_perf_all_iter_abl %>% 
  mutate(method = factor(method))
table(B_perf_all_iter_abl$data, B_perf_all_iter_abl$method)
B_perf_all_iter_abl_path= ".\\benchmark_results\\rules_results_abl\\B_perf_all_iter_abl.csv"
write.csv(B_perf_all_iter_abl,B_perf_all_iter_abl_path, row.names = FALSE)
testF1=read.table(B_perf_all_iter_abl_path,sep=',',header= T, na.strings="?")
names(B_perf_all_iter_abl)

B_perf_ORE_summary_iter_abl = sqldf('select method,iter, pred_set,pred_sub_set,abl_model,
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
                          from B_perf_all_iter_abl group by method, abl_model, iter,pred_set, pred_sub_set')



B_perf_ORE_summary_perdata_abl = sqldf('select method,  data, pred_set,pred_sub_set,abl_model,
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
                          from B_perf_all_iter_abl group by method, abl_model, data,pred_set,pred_sub_set')


B_perf_ORE_summary_iter_based_abl = sqldf('select  pred_set,pred_sub_set, method, abl_model, 
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
                          from B_perf_ORE_summary_iter_abl group by pred_set,pred_sub_set,method, abl_model')


B_perf_ORE_summary_iter_abl_path= ".\\benchmark_results\\rules_results_abl\\B_perf_ORE_summary_iter_abl.csv"
write.csv(B_perf_ORE_summary_iter_abl,B_perf_ORE_summary_iter_abl_path, row.names = FALSE)
testF20=read.table(B_perf_ORE_summary_iter_abl_path,sep=',',header= T, na.strings="?")

B_perf_ORE_summary_perdata_abl_path= ".\\benchmark_results\\rules_results_abl\\B_perf_ORE_summary_perdata_abl.csv"
write.csv(B_perf_ORE_summary_perdata_abl,B_perf_ORE_summary_perdata_abl_path, row.names = FALSE)
testF30=read.table(B_perf_ORE_summary_perdata_abl_path,sep=',',header= T, na.strings="?")

B_perf_ORE_summary_iter_based_abl_path= ".\\benchmark_results\\rules_results_abl\\B_perf_ORE_summary_iter_based_abl.csv"
write.csv(B_perf_ORE_summary_iter_based_abl,B_perf_ORE_summary_iter_based_abl_path, row.names = FALSE)
testF40=read.table(B_perf_ORE_summary_iter_based_abl_path,sep=',',header= T, na.strings="?")
names(B_perf_ORE_summary_iter_based_abl)

B_abl_summary_perf = B_perf_ORE_summary_iter_based_abl %>% 
  filter(pred_set=="test") %>% 
  filter(pred_sub_set=="covered") %>% 
  filter(abl_model %in% c("abl_conf","abl_supp", "abl_length","abl_mod", "no_abl", "abl_preselect")) %>%
  select(abl_model,method, mean_acc,SE_acc,mean_F1, SE_F1, mean_fidelity, SE_fidelity, mean_coverage, SE_coverage)
B_abl_summary_perf[,3:ncol(B_abl_summary_perf)] = format(round(B_abl_summary_perf[,3:ncol(B_abl_summary_perf)],3),nsmall=3)

B_abl_summary_perf_path= ".\\benchmark_results\\rules_results_abl\\B_abl_summary_perf.csv"
write.csv(B_abl_summary_perf,B_abl_summary_perf_path, row.names = FALSE)



filePaths_prediction_perf_abl_opt = grep(list.files(path="./Rules_results_abl", pattern='_OptRules_metrics_df_abl.csv',full.names = TRUE), pattern='_avg_', invert=TRUE, value=TRUE)

B_perf_all_iter_abl_opt <- do.call(rbind, lapply(filePaths_prediction_perf_abl_opt, read.csv))
# table(B_perf_all_iter_init$pred_set)
# head(B_perf_all_iter_init)
# names(B_perf_all_iter_init)
B_perf_all_iter_abl_opt$data =toupper(B_perf_all_iter_abl_opt$data)
B_perf_all_iter_abl_opt$data[which(B_perf_all_iter_abl_opt$data=="BCO")] ="BRCANCER"
# unique(B_perf_all_iter_abl_opt$method)
B_perf_all_iter_abl_opt$class_rules_nbr=B_perf_all_iter_abl_opt$rules_nbr
names(B_perf_all_iter_abl_opt)

unique(B_perf_all_iter_abl_opt$abl_model)

table(B_perf_all_iter_abl_opt$data, B_perf_all_iter_abl_opt$abl_model)
B_perf_all_iter_abl_opt_path= ".\\benchmark_results\\rules_results_abl\\B_perf_all_iter_abl_opt.csv"
write.csv(B_perf_all_iter_abl_opt,B_perf_all_iter_abl_opt_path, row.names = FALSE)
testF1=read.table(B_perf_all_iter_abl_opt_path,sep=',',header= T, na.strings="?")
names(B_perf_all_iter_abl_opt)
library(dplyr)
B_perf_ORE_summary_iter_abl_opt = sqldf('select data, abl_model,iter,
                          count(id) as rules_nbr,
                          sum(confidence) as cumul_conf,
                          avg(confidence) as avg_conf,
                          sum(support) as cumul_support,
                          avg(support) as avg_support,
                          sum(class_suppport) as cumul_class_support,
                          avg(class_suppport) as avg_class_support,
                          sum(var_length) as cumul_var_length,
                          avg(var_length) as avg_var_length,
                          sum(var_nbr_scaled) as cumul_var_nbr_scaled,
                          avg(var_nbr_scaled) as avg_var_nbr_scaled,
                          sum(levels_length) as cumul_levels_length,
                          avg(levels_length) as avg_levels_length,
                          sum(levels_nbr_scaled) as cumul_levels_nbr_scaled,
                          avg(levels_nbr_scaled) as avg_levels_nbr_scaled
                          from B_perf_all_iter_abl_opt group by data, abl_model, iter')


B_perf_ORE_summary_iter_based_abl_opt = sqldf('select  abl_model,
                          avg(rules_nbr) as mean_rules_nbr, 
                          stdev(rules_nbr)/sqrt(count(rules_nbr)) as SE_rules_nbr,
                          
                          avg(cumul_conf) as mean_cumul_conf, 
                          stdev(cumul_conf)/sqrt(count(cumul_conf)) as SE_cumul_conf,
                          avg(avg_conf) as mean_avg_conf, 
                          stdev(avg_conf)/sqrt(count(avg_conf)) as SE_avg_conf,
                          
                          avg(cumul_support) as mean_cumul_support, 
                          stdev(cumul_support)/sqrt(count(cumul_support)) as SE_cumul_support,
                          avg(avg_support) as mean_avg_support,
                          stdev(avg_support)/sqrt(count(avg_support)) as SE_avg_support,
                          
                          avg(cumul_class_support) as mean_cumul_class_support, 
                          stdev(cumul_class_support)/sqrt(count(cumul_class_support)) as SE_cumul_class_support,
                          avg(avg_class_support) as mean_avg_class_support, 
                          stdev(avg_class_support)/sqrt(count(avg_class_support)) as SE_avg_class_support,
                          
                          avg(cumul_var_length) as mean_cumul_var_length, 
                          stdev(cumul_var_length)/sqrt(count(cumul_var_length)) as SE_cumul_var_length,
                          avg(avg_var_length) as mean_avg_var_length, 
                          stdev(avg_var_length)/sqrt(count(avg_var_length)) as SE_avg_var_length,
                          
                          
                          avg(cumul_var_nbr_scaled) as mean_cumul_var_nbr_scaled, 
                          stdev(cumul_var_nbr_scaled)/sqrt(count(cumul_var_nbr_scaled)) as SE_cumul_var_nbr_scaled,
                          avg(avg_var_nbr_scaled) as mean_avg_var_nbr_scaled, 
                          stdev(avg_var_nbr_scaled)/sqrt(count(avg_var_nbr_scaled)) as SE_avg_var_nbr_scaled,
                          
                          avg(cumul_levels_length) as mean_cumul_levels_length, 
                          stdev(cumul_levels_length)/sqrt(count(cumul_levels_length)) as SE_cumul_levels_length,
                          avg(avg_levels_length) as mean_avg_levels_length, 
                          stdev(avg_levels_length)/sqrt(count(avg_levels_length)) as SE_avg_levels_length,
                            
                          avg(cumul_levels_nbr_scaled) as mean_cumul_levels_nbr_scaled, 
                          stdev(cumul_levels_nbr_scaled)/sqrt(count(cumul_levels_nbr_scaled)) as SE_cumul_levels_nbr_scaled,
                          avg(avg_levels_nbr_scaled) as mean_avg_levels_nbr_scaled, 
                          stdev(avg_levels_nbr_scaled)/sqrt(count(avg_levels_nbr_scaled)) as SE_avg_levels_nbr_scaled
                          
                          from B_perf_ORE_summary_iter_abl_opt group by abl_model')

unique(B_perf_ORE_summary_iter_based_abl_opt$abl_model)
B_abl_summury_w = B_perf_ORE_summary_iter_based_abl_opt %>% 
  filter(abl_model %in% c("abl_conf","abl_supp", "abl_length","abl_mod", "no_abl", "abl_preselect"))
names(B_abl_summury_w)

B_abl_summury_w[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)] = format(round(B_abl_summury_w [,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)],3),nsmall=3)
B_abl_summury_w[,-c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28)] = format(round(B_abl_summury_w [,-c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28)],4),nsmall=4)
B_abl_summury_w_path= ".\\benchmark_results\\rules_results_abl\\B_abl_summury_w.csv"
write.csv(B_abl_summury_w,B_abl_summury_w_path, row.names = FALSE)

# B_perf_ORE_summary_perdata_abl_opt = sqldf('select data, abl_model,iter,
#                           avg(rules_nbr) as mean_rules_nbr, 
#                           stdev(rules_nbr) as std_rules_nbr,
#                           stdev(rules_nbr)/sqrt(count(rules_nbr)) as SE_rules_nbr,
#                           avg(cumul_conf) as mean_cumul_conf, 
#                           stdev(cumul_conf) as std_cumul_conf,
#                           stdev(cumul_conf)/sqrt(count(cumul_conf)) as SE_cumul_conf,
#                           avg(cumul_support) as mean_cumul_support, 
#                           stdev(cumul_support) as std_cumul_support,
#                           stdev(cumul_support)/sqrt(count(cumul_support)) as SE_cumul_support,
#                           avg(cumul_class_support) as mean_cumul_class_support, 
#                           stdev(cumul_class_support) as std_cumul_class_support,
#                           stdev(cumul_class_support)/sqrt(count(cumul_class_support)) as SE_cumul_class_support,
#                           avg(cumul_var_length) as mean_cumul_var_length, 
#                           stdev(cumul_var_length) as std_cumul_var_length,
#                           stdev(cumul_var_length)/sqrt(count(cumul_var_length)) as SE_cumul_var_length,
#                           avg(cumul_var_nbr_scaled) as mean_cumul_var_nbr_scaled, 
#                           stdev(cumul_var_nbr_scaled) as std_cumul_var_nbr_scaled,
#                           stdev(cumul_var_nbr_scaled)/sqrt(count(cumul_var_nbr_scaled)) as SE_cumul_var_nbr_scaled,
#                            avg(cumul_levels_length) as mean_cumul_levels_length, 
#                           stdev(cumul_levels_length) as std_cumul_levels_length,
#                           stdev(cumul_levels_length)/sqrt(count(cumul_levels_length)) as SE_cumul_cumul_levels_length,
#                           avg(cumul_levels_nbr_scaled) as mean_cumul_levels_nbr_scaled, 
#                           stdev(cumul_levels_nbr_scaled) as std_cumul_levels_nbr_scaled,
#                           stdev(cumul_levels_nbr_scaled)/sqrt(count(cumul_levels_nbr_scaled)) as SE_cumul_cumul_levels_nbr_scaled
#                          
#                           from B_perf_ORE_summary_iter_abl_opt group by data, abl_model')
# 
# names(B_perf_ORE_summary_iter_abl_opt)
# B_perf_ORE_summary_iter_based_abl_opt = sqldf('select  abl_model,iter,
#                           avg(rules_nbr) as mean_rules_nbr, 
#                           stdev(rules_nbr) as std_rules_nbr,
#                           stdev(rules_nbr)/sqrt(count(rules_nbr)) as SE_rules_nbr,
#                           avg(cumul_conf) as mean_cumul_conf, 
#                           stdev(cumul_conf) as std_cumul_conf,
#                           stdev(cumul_conf)/sqrt(count(cumul_conf)) as SE_cumul_conf,
#                           avg(cumul_support) as mean_cumul_support, 
#                           stdev(cumul_support) as std_cumul_support,
#                           stdev(cumul_support)/sqrt(count(cumul_support)) as SE_cumul_support,
#                           avg(cumul_class_support) as mean_cumul_class_support, 
#                           stdev(cumul_class_support) as std_cumul_class_support,
#                           stdev(cumul_class_support)/sqrt(count(cumul_class_support)) as SE_cumul_class_support,
#                           avg(cumul_var_length) as mean_cumul_var_length, 
#                           stdev(cumul_var_length) as std_cumul_var_length,
#                           stdev(cumul_var_length)/sqrt(count(cumul_var_length)) as SE_cumul_var_length,
#                           avg(cumul_var_nbr_scaled) as mean_cumul_var_nbr_scaled, 
#                           stdev(cumul_var_nbr_scaled) as std_cumul_var_nbr_scaled,
#                           stdev(cumul_var_nbr_scaled)/sqrt(count(cumul_var_nbr_scaled)) as SE_cumul_var_nbr_scaled,
#                            avg(cumul_levels_length) as mean_cumul_levels_length, 
#                           stdev(cumul_levels_length) as std_cumul_levels_length,
#                           stdev(cumul_levels_length)/sqrt(count(cumul_levels_length)) as SE_cumul_cumul_levels_length,
#                           avg(cumul_levels_nbr_scaled) as mean_cumul_levels_nbr_scaled, 
#                           stdev(cumul_levels_nbr_scaled) as std_cumul_levels_nbr_scaled,
#                           stdev(cumul_levels_nbr_scaled)/sqrt(count(cumul_levels_nbr_scaled)) as SE_cumul_cumul_levels_nbr_scaled
#                          
#                           from B_perf_ORE_summary_iter_abl_opt group by iter, abl_model')
# 
# B_perf_ORE_summary_iter_based_abl_opt_summary = sqldf('select  abl_model,
#                           avg(mean_rules_nbr) as mean_rules_nbr, 
#                           stdev(mean_rules_nbr) as std_rules_nbr,
#                           stdev(mean_rules_nbr)/sqrt(count(mean_rules_nbr)) as SE_rules_nbr,
#                           avg(mean_cumul_conf) as mean_cumul_conf, 
#                           stdev(mean_cumul_conf) as std_cumul_conf,
#                           stdev(mean_cumul_conf)/sqrt(count(mean_cumul_conf)) as SE_cumul_conf,
#                           avg(mean_cumul_support) as mean_cumul_support, 
#                           stdev(mean_cumul_support) as std_cumul_support,
#                           stdev(mean_cumul_support)/sqrt(count(mean_cumul_support)) as SE_cumul_support,
#                           avg(mean_cumul_class_support) as mean_cumul_class_support, 
#                           stdev(mean_cumul_class_support) as std_cumul_class_support,
#                           stdev(mean_cumul_class_support)/sqrt(count(mean_cumul_class_support)) as SE_cumul_class_support,
#                           avg(mean_cumul_var_length) as mean_cumul_var_length, 
#                           stdev(mean_cumul_var_length) as std_cumul_var_length,
#                           stdev(mean_cumul_var_length)/sqrt(count(mean_cumul_var_length)) as SE_cumul_var_length,
#                           avg(mean_cumul_var_nbr_scaled) as mean_cumul_var_nbr_scaled, 
#                           stdev(mean_cumul_var_nbr_scaled) as std_cumul_var_nbr_scaled,
#                           stdev(mean_cumul_var_nbr_scaled)/sqrt(count(mean_cumul_var_nbr_scaled)) as SE_cumul_var_nbr_scaled,
#                            avg(mean_cumul_levels_length) as mean_cumul_levels_length, 
#                           stdev(mean_cumul_levels_length) as std_cumul_levels_length,
#                           stdev(mean_cumul_levels_length)/sqrt(count(mean_cumul_levels_length)) as SE_cumul_cumul_levels_length,
#                           avg(mean_cumul_levels_length) as mean_cumul_levels_nbr_scaled, 
#                           stdev(mean_cumul_levels_length) as std_cumul_levels_nbr_scaled,
#                           stdev(mean_cumul_levels_length)/sqrt(count(mean_cumul_levels_length)) as SE_cumul_cumul_levels_nbr_scaled
#                          
#                           from B_perf_ORE_summary_iter_based_abl_opt group by abl_model')
# 
# B_perf_ORE_summary_iter_abl_opt_path= ".\\benchmark_results\\rules_results_abl\\B_perf_ORE_summary_iter_abl_opt.csv"
# write.csv(B_perf_ORE_summary_iter_abl_opt,B_perf_ORE_summary_iter_abl_opt_path, row.names = FALSE)
# testF20=read.table(B_perf_ORE_summary_iter_abl_opt_path,sep=',',header= T, na.strings="?")
# 
# B_perf_ORE_summary_perdata_abl_opt_path= ".\\benchmark_results\\rules_results_abl\\B_perf_ORE_summary_perdata_abl_opt.csv"
# write.csv(B_perf_ORE_summary_perdata_abl_opt,B_perf_ORE_summary_perdata_abl_opt_path, row.names = FALSE)
# testF30=read.table(B_perf_ORE_summary_perdata_abl_opt_path,sep=',',header= T, na.strings="?")
# 
# B_perf_ORE_summary_iter_based_abl_opt_path= ".\\benchmark_results\\rules_results_abl\\B_perf_ORE_summary_iter_based_abl_opt.csv"
# write.csv(B_perf_ORE_summary_iter_based_abl_opt,B_perf_ORE_summary_iter_based_abl_opt_path, row.names = FALSE)
# testF40=read.table(B_perf_ORE_summary_iter_based_abl_opt_path,sep=',',header= T, na.strings="?")

# .........................................
filePaths_MIP_summary_abl = grep(list.files(path="./Rules_results_abl", pattern='_MIP_summary_abl.csv',full.names = TRUE), pattern='_avg_', invert=TRUE, value=TRUE)

B_MIP_SOL_all_iter_abl <- do.call(rbind, lapply(filePaths_MIP_summary_abl, read.csv))
# table(B_MIP_SOL_all_iter_init$pred_set)
# head(B_MIP_SOL_all_iter_init)
# names(B_MIP_SOL_all_iter_init)
B_MIP_SOL_all_iter_abl$data =toupper(B_MIP_SOL_all_iter_abl$data)
B_MIP_SOL_all_iter_abl$data[which(B_MIP_SOL_all_iter_abl$data=="BCO")] ="BRCANCER"

names(B_MIP_SOL_all_iter_abl)

unique(B_MIP_SOL_all_iter_abl$abl_model)


str(B_data_summary)

B_MIP_SOL_all_iter_abl=B_MIP_SOL_all_iter_abl %>% 
  mutate(abl_model = factor(abl_model))

table(B_MIP_SOL_all_iter_abl$data, B_MIP_SOL_all_iter_abl$abl_model)
B_MIP_SOL_all_iter_abl_path= ".\\benchmark_results\\rules_results_abl\\B_MIP_SOL_all_iter_abl.csv"
write.csv(B_MIP_SOL_all_iter_abl,B_MIP_SOL_all_iter_abl_path, row.names = FALSE)
testF1=read.table(B_MIP_SOL_all_iter_abl_path,sep=',',header= T, na.strings="?")
names(B_MIP_SOL_all_iter_abl)


B_MIP_SOL_ORE_summary_iter_abl = sqldf('select abl_model,data, iter,
                          avg(init_Rules_nbr) as mean_init_Rules_nbr, 
                          stdev(init_Rules_nbr) as std_init_Rules_nbr, 
                          stdev(init_Rules_nbr)/sqrt(count(init_Rules_nbr)) as SE_init_Rules_nbr,
                          avg(Rules_nbr) as mean_rules_nbr, 
                          stdev(Rules_nbr) as std_rules_nbr,
                          stdev(Rules_nbr)/sqrt(count(Rules_nbr)) as SE_rules_nbr,
                          avg(coverage_ratio) as mean_coverage_ratio, 
                          stdev(coverage_ratio) as std_coverage_ratio,
                          stdev(coverage_ratio)/sqrt(count(coverage_ratio)) as SE_coverage_ratio,
                          avg(covered_error_ratio) as mean_covered_error_ratio, 
                          stdev(covered_error_ratio) as std_covered_error_ratio,
                          stdev(covered_error_ratio)/sqrt(count(covered_error_ratio)) as SE_covered_error_ratio,
                          avg(overlapping_ratio) as mean_overlapping_ratio, 
                          stdev(overlapping_ratio) as std_overlapping_ratio, 
                          stdev(overlapping_ratio)/sqrt(count(overlapping_ratio)) as SE_overlapping_ratio,
                          avg(opt_run_time) as mean_opt_run_time, 
                          stdev(opt_run_time) as std_opt_run_time, 
                          stdev(opt_run_time)/sqrt(count(opt_run_time)) as SE_opt_run_time,
                          avg(run_time) as mean_run_time, 
                          stdev(run_time) as std_run_time,
                          stdev(run_time)/sqrt(count(run_time)) as SE_run_time
                          from B_MIP_SOL_all_iter_abl group by  abl_model, data, iter')



B_MIP_SOL_ORE_summary_perdata_abl = sqldf('select abl_model,data, 
                          avg(init_Rules_nbr) as mean_init_Rules_nbr, 
                          stdev(init_Rules_nbr) as std_init_Rules_nbr, 
                          stdev(init_Rules_nbr)/sqrt(count(init_Rules_nbr)) as SE_init_Rules_nbr,
                          avg(Rules_nbr) as mean_rules_nbr, 
                          stdev(Rules_nbr) as std_rules_nbr,
                          stdev(Rules_nbr)/sqrt(count(Rules_nbr)) as SE_rules_nbr,
                          avg(coverage_ratio) as mean_coverage_ratio, 
                          stdev(coverage_ratio) as std_coverage_ratio,
                          stdev(coverage_ratio)/sqrt(count(coverage_ratio)) as SE_coverage_ratio,
                          avg(covered_error_ratio) as mean_covered_error_ratio, 
                          stdev(covered_error_ratio) as std_covered_error_ratio,
                          stdev(covered_error_ratio)/sqrt(count(covered_error_ratio)) as SE_covered_error_ratio,
                          avg(overlapping_ratio) as mean_overlapping_ratio, 
                          stdev(overlapping_ratio) as std_overlapping_ratio, 
                          stdev(overlapping_ratio)/sqrt(count(overlapping_ratio)) as SE_overlapping_ratio,
                          avg(opt_run_time) as mean_opt_run_time, 
                          stdev(opt_run_time) as std_opt_run_time, 
                          stdev(opt_run_time)/sqrt(count(opt_run_time)) as SE_opt_run_time,
                          avg(run_time) as mean_run_time, 
                          stdev(run_time) as std_run_time,
                          stdev(run_time)/sqrt(count(run_time)) as SE_run_time
                          from B_MIP_SOL_all_iter_abl group by  abl_model, data')


B_MIP_SOL_ORE_summary_iter_based_abl = sqldf('select abl_model,iter,
                          avg(init_Rules_nbr) as mean_init_Rules_nbr, 
                          stdev(init_Rules_nbr) as std_init_Rules_nbr, 
                          stdev(init_Rules_nbr)/sqrt(count(init_Rules_nbr)) as SE_init_Rules_nbr,
                          avg(Rules_nbr) as mean_rules_nbr, 
                          stdev(Rules_nbr) as std_rules_nbr,
                          stdev(Rules_nbr)/sqrt(count(Rules_nbr)) as SE_rules_nbr,
                          avg(coverage_ratio) as mean_coverage_ratio, 
                          stdev(coverage_ratio) as std_coverage_ratio,
                          stdev(coverage_ratio)/sqrt(count(coverage_ratio)) as SE_coverage_ratio,
                          avg(covered_error_ratio) as mean_covered_error_ratio, 
                          stdev(covered_error_ratio) as std_covered_error_ratio,
                          stdev(covered_error_ratio)/sqrt(count(covered_error_ratio)) as SE_covered_error_ratio,
                          avg(overlapping_ratio) as mean_overlapping_ratio, 
                          stdev(overlapping_ratio) as std_overlapping_ratio, 
                          stdev(overlapping_ratio)/sqrt(count(overlapping_ratio)) as SE_overlapping_ratio,
                          avg(opt_run_time) as mean_opt_run_time, 
                          stdev(opt_run_time) as std_opt_run_time, 
                          stdev(opt_run_time)/sqrt(count(opt_run_time)) as SE_opt_run_time,
                          avg(run_time) as mean_run_time, 
                          stdev(run_time) as std_run_time,
                          stdev(run_time)/sqrt(count(run_time)) as SE_run_time
                          from B_MIP_SOL_all_iter_abl group by  abl_model, iter')

B_MIP_SOL_ORE_summary_iter_based_abl_summary = sqldf('select abl_model,
                          avg(mean_init_Rules_nbr) as mean_init_Rules_nbr, 
                          stdev(mean_init_Rules_nbr) as std_init_Rules_nbr, 
                          stdev(mean_init_Rules_nbr)/sqrt(count(mean_init_Rules_nbr)) as SE_init_Rules_nbr,
                          avg(mean_rules_nbr) as mean_rules_nbr, 
                          stdev(mean_rules_nbr) as std_rules_nbr,
                          stdev(mean_rules_nbr)/sqrt(count(mean_rules_nbr)) as SE_rules_nbr,
                          avg(mean_coverage_ratio) as mean_coverage_ratio, 
                          stdev(mean_coverage_ratio) as std_coverage_ratio,
                          stdev(mean_coverage_ratio)/sqrt(count(mean_coverage_ratio)) as SE_coverage_ratio,
                          avg(mean_covered_error_ratio) as mean_covered_error_ratio, 
                          stdev(mean_covered_error_ratio) as std_covered_error_ratio,
                          stdev(mean_covered_error_ratio)/sqrt(count(mean_covered_error_ratio)) as SE_covered_error_ratio,
                          avg(mean_overlapping_ratio) as mean_overlapping_ratio, 
                          stdev(mean_overlapping_ratio) as std_overlapping_ratio, 
                          stdev(mean_overlapping_ratio)/sqrt(count(mean_overlapping_ratio)) as SE_overlapping_ratio,
                          avg(mean_opt_run_time) as mean_opt_run_time, 
                          stdev(mean_opt_run_time) as std_opt_run_time, 
                          stdev(mean_opt_run_time)/sqrt(count(mean_opt_run_time)) as SE_opt_run_time,
                          avg(mean_run_time) as mean_run_time, 
                          stdev(mean_run_time) as std_run_time,
                          stdev(mean_run_time)/sqrt(count(mean_run_time)) as SE_run_time
                          from B_MIP_SOL_ORE_summary_iter_based_abl group by  abl_model')

B_MIP_SOL_ORE_summary_iter_abl_path= ".\\benchmark_results\\rules_results_abl\\B_MIP_SOL_ORE_summary_iter_abl.csv"
write.csv(B_MIP_SOL_ORE_summary_iter_abl,B_MIP_SOL_ORE_summary_iter_abl_path, row.names = FALSE)
testF20=read.table(B_MIP_SOL_ORE_summary_iter_abl_path,sep=',',header= T, na.strings="?")

B_MIP_SOL_ORE_summary_perdata_abl_path= ".\\benchmark_results\\rules_results_abl\\B_MIP_SOL_ORE_summary_perdata_abl.csv"
write.csv(B_MIP_SOL_ORE_summary_perdata_abl,B_MIP_SOL_ORE_summary_perdata_abl_path, row.names = FALSE)
testF30=read.table(B_MIP_SOL_ORE_summary_perdata_abl_path,sep=',',header= T, na.strings="?")

B_MIP_SOL_ORE_summary_iter_based_abl_path= ".\\benchmark_results\\rules_results_abl\\B_MIP_SOL_ORE_summary_iter_based_abl.csv"
write.csv(B_MIP_SOL_ORE_summary_iter_based_abl,B_MIP_SOL_ORE_summary_iter_based_abl_path, row.names = FALSE)
testF40=read.table(B_MIP_SOL_ORE_summary_iter_based_abl_path,sep=',',header= T, na.strings="?")

B_MIP_SOL_ORE_summary_abl_smmary= B_MIP_SOL_ORE_summary_iter_based_abl_summary %>%
  filter(abl_model %in% c("abl_conf","abl_supp", "abl_length","abl_mod", "no_abl", "abl_preselect"))


B_MIP_SOL_ORE_summary_abl_smmary[,2:22] = format(round(B_MIP_SOL_ORE_summary_abl_smmary[,2:22],3),nsmall=3)

B_MIP_SOL_ORE_summary_abl_smmary_path= ".\\benchmark_results\\rules_results_abl\\B_MIP_SOL_ORE_summary_abl_smmary.csv"
write.csv(B_MIP_SOL_ORE_summary_abl_smmary,B_MIP_SOL_ORE_summary_abl_smmary_path, row.names = FALSE)
  

B_MIP_SOL_ORE_summary_iter_based_abl_path= ".\\benchmark_results\\rules_results_abl\\B_MIP_SOL_ORE_summary_iter_based_abl.csv"
write.csv(B_MIP_SOL_ORE_summary_iter_based_abl,B_MIP_SOL_ORE_summary_iter_based_abl_path, row.names = FALSE)
testF40=read.table(B_MIP_SOL_ORE_summary_iter_based_abl_path,sep=',',header= T, na.strings="?")
# ..........................................
# execution time
# .......................................

filePaths_exetime_abl = grep(list.files(path="./Rules_results_abl", pattern='_exe_time_abl.csv',full.names = TRUE), pattern='_avg_',invert=TRUE, value=TRUE)

B_exetime_abl_all_iter <- do.call(rbind, lapply(filePaths_exetime_abl, read.csv))
names(B_exetime_abl_all_iter)
# table(B_perf_all_iter$pred_set)
# head(B_perf_all_iter)
# names(B_perf_all_iter)
B_exetime_abl_all_iter$data =toupper(B_exetime_abl_all_iter$data)
B_exetime_abl_all_iter$data[which(B_exetime_abl_all_iter$data=="BCO")] ="BRCANCER"

B_exetime_abl_all_iter$build_opt= B_exetime_abl_all_iter$buildandrun_opt - B_exetime_abl_all_iter$run_opt
colnames(B_exetime_abl_all_iter) 
# unique(B_perf_all_iter$method)
B_exetime_abl_ORE_summary_perdata = sqldf('select data, abl_model,
                          avg(extract_rules) as mean_extract_rules, 
                          stdev(extract_rules) as std_extract_rules,
                          stdev(extract_rules)/sqrt(count(extract_rules)) as SE_extract_rules,
                          
                          avg(prepare_opt_input) as mean_prepare_opt_input, 
                          stdev(prepare_opt_input) as std_prepare_opt_input, 
                          stdev(prepare_opt_input)/sqrt(count(prepare_opt_input)) as SE_prepare_opt_input,
                          avg(build_opt) as mean_build_opt, 
                          stdev(build_opt) as std_build_opt, 
                          stdev(build_opt)/sqrt(count(build_opt)) as SE_build_opt,
                          avg(run_opt) as mean_run_opt, 
                          stdev(run_opt) as std_run_opt, 
                          stdev(run_opt)/sqrt(count(run_opt)) as SE_run_opt,
                          avg(buildandrun_opt) as mean_buildandrun_opt, 
                          stdev(buildandrun_opt) as std_buildandrun_opt, 
                          stdev(buildandrun_opt)/sqrt(count(run_opt)) as SE_buildandrun_opt
                          
                          
                          from B_exetime_abl_all_iter group by Data, abl_model')

B_exetime_abl_ORE_summary_periter = sqldf('select iter,abl_model,
                          avg(extract_rules) as mean_extract_rules, 
                          
                          stdev(extract_rules)/sqrt(count(extract_rules)) as SE_extract_rules,
                          
                          avg(preselect_rules) as mean_preselect_rules, 
                          
                          stdev(preselect_rules)/sqrt(count(preselect_rules)) as SE_preselect_rules,
                          avg(prepare_opt_input) as mean_prepare_opt_input, 
                          
                          stdev(prepare_opt_input)/sqrt(count(prepare_opt_input)) as SE_prepare_opt_input,
                          avg(build_opt) as mean_build_opt, 
                          
                          stdev(build_opt)/sqrt(count(build_opt)) as SE_build_opt,
                          avg(run_opt) as mean_run_opt, 
                           
                          stdev(run_opt)/sqrt(count(run_opt)) as SE_run_opt,
                          avg(buildandrun_opt) as mean_buildandrun_opt, 
                          
                          stdev(buildandrun_opt)/sqrt(count(run_opt)) as SE_buildandrun_opt
                          
                          from B_exetime_abl_all_iter group by iter, abl_model ')

B_exetime_abl_ORE_summary_5 = sqldf('select abl_model,
                          avg(mean_extract_rules) as mean_extract_rules, 
                          
                          stdev(mean_extract_rules)/sqrt(count(mean_extract_rules)) as SE_extract_rules,
                          
                          avg(mean_preselect_rules) as mean_preselect_rules, 
                          
                          stdev(mean_preselect_rules)/sqrt(count(mean_preselect_rules)) as SE_preselect_rules,
                          avg(mean_prepare_opt_input) as mean_prepare_opt_input, 
                          
                          avg(mean_extract_rules+mean_preselect_rules) as mean_extract_preselect_rules, 
                          
                          stdev(mean_extract_rules+mean_preselect_rules)/sqrt(count(mean_extract_rules+mean_preselect_rules)) as SE_extract_preselect_rules,
                          
                          stdev(mean_prepare_opt_input)/sqrt(count(mean_prepare_opt_input)) as SE_prepare_opt_input,
                          avg(mean_build_opt) as mean_build_opt, 
                          
                          stdev(mean_build_opt)/sqrt(count(mean_build_opt)) as SE_build_opt,
                          avg(mean_run_opt) as mean_run_opt, 
                           
                          stdev(mean_run_opt)/sqrt(count(mean_run_opt)) as SE_run_opt,
                          avg(mean_buildandrun_opt) as mean_buildandrun_opt, 
                          
                          stdev(mean_buildandrun_opt)/sqrt(count(mean_buildandrun_opt)) as SE_buildandrun_opt
                          
                          from B_exetime_abl_ORE_summary_periter group by  abl_model ')
colnames(B_exetime_abl_ORE_summary_perdata)
unique(B_exetime_abl_ORE_summary_perdata$abl_model)
B_exetime_abl_ORE_summary_5= B_exetime_abl_ORE_summary_5 %>%
  filter(abl_model %in% c("abl_conf","abl_supp", "abl_length","abl_mod", "no_abl", "abl_preselect"))


B_exetime_abl_ORE_summary_5[,2:15] = format(round(B_exetime_abl_ORE_summary_5[,2:15],2),nsmall=2)

B_exetime_abl_ORE_summary_5_path= ".\\benchmark_results\\rules_results_abl\\B_exetime_abl_ORE_summary_5.csv"
write.csv(B_exetime_abl_ORE_summary_5,B_exetime_abl_ORE_summary_5_path, row.names = FALSE)


# B_exetime_abl_ORE_summary_perdata = B_exetime_abl_ORE_summary_perdata %>% mutate(abl_model = factor(abl_model))
B_exetime_abl_ORE_path= ".\\benchmark_results\\rules_results_abl\\B_exetime_abl_ORE_perdata.csv"
write.csv(B_exetime_abl_ORE_summary_perdata,B_exetime_abl_ORE_path, row.names = FALSE)
B_exetime_abl_ORE_path= ".\\benchmark_results\\rules_results_abl\\B_exetime_abl_ORE_perdata.csv"
write.csv(B_exetime_abl_ORE_summary_perdata,B_exetime_abl_ORE_path, row.names = FALSE)
