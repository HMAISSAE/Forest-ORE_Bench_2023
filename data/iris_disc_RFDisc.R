
Iris=read.table("iris_text_na.txt",sep='',header= T, na.strings="?")
library(randomForest)

Iris=na.roughfix(Iris)
summary(Iris)
name="iris"
id_target=5
summary(Iris)

library(randomForest)
Iris=na.roughfix(Iris)
summary(Iris)



Data_cont=Iris
id_target=5


name="Iris"
Data_cont=na.roughfix(Data_cont)
class_attr=get_class_attribute(Data=Data_cont,id_target=id_target)

cont_attr=class_attr$continuous_att
cat_attr=class_attr$categorical_att

Data_orig=Data_cont

DataSet=Data_orig

dataset_summary=data.frame(matrix(ncol=8,nrow=1))
colnames(dataset_summary)=c("name","nrow","ncol","ncont_att","ncat_att","cont","cat","nclass")
dataset_summary[1,]=c(name,nrow(DataSet),
                      ncol(DataSet),length(cont_attr),
                      length(cat_attr),paste0(as.character(cont_attr),collapse=",") ,
                      paste0(as.character(cat_attr),collapse=","),length(unique(DataSet[,id_target])))
write.csv(dataset_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\Iris_dataset_summary.csv", row.names = FALSE)
test0=read.table("performance\\Iris_dataset_summary.csv",sep=',',header= T, na.strings="?")
vec_seed=c(012,123,234,345,456,567,678,789,890,901)

perf_all_iter=data.frame()
all_iter_splits=data.frame()
all_splitsselection=data.frame()


for (I in (1:length(vec_seed)))

{
  iteration=I
  print(paste0(Sys.time()," Iter ",I," in progress"))
  Select_seed=vec_seed[I]
  Data=DATA_SET(XDataset=DataSet[,-id_target],
                YDataset=DataSet[,id_target],
                SplitRatio=0.7,seed=Select_seed)
  Data_cont=Data$training_set
  
  class_attr=get_class_attribute(Data=DataSet,id_target=id_target)
  cont_attr=class_attr$continuous_att
  unique(perf_all_iter$disc_method)
  cont_Data=Data_cont
  
  for (meth in c("RFDiscret"))

  {
    if (meth == "RFDiscret") 
    {
      # RF_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=50,max_splits=10,opt_meth="RFDiscret")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "RFDisc"
    }else if (meth == "Forest_disc_50") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=50,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_50"
    }
      
      
    
    end.time = Sys.time()
    time.taken = difftime(end.time,start.time,units="secs")
    
    # time.taken = end.time - start.time
    # C=difftime(end.time,start.time,units="secs")
    # time_taken = time.taken
    # tr=perf_one_iter
    # tr[,"tr"]= C
    if ( meth %in% c("RFDiscret","Forest_disc_25","Forest_disc_50","Forest_disc_75","Forest_disc_100",
                     "Forest_disc_125","Forest_disc_150","Forest_disc_175","Forest_disc_200",
                     "Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
                     "Forest_disc_DIR50","Forest_disc_DIR100","Forest_disc_DIR200"))
    {
      
      iter_splits=RFdisc$opt_results
      iter_splits[,"iter"]=I
      iter_splits[,"forest"]=disc_method
      all_iter_splits=rbind(all_iter_splits,iter_splits)
      
      splitsselection=RFdisc$cut_points
      splitsselection[,"iter"]=I
      splitsselection[,"forest"]=disc_method
      all_splitsselection=rbind(all_splitsselection,splitsselection)
    }
    # mean_nb_interval=(length(cont_cutp)+sum(lengths(cont_cutp,use.names = TRUE)))/length(cont_cutp)
    # NB_interval=mean_nb_interval
    # disc_NB_interval=NB_interval
    # Data_disc=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
    # Data_disc_train=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
    # colnames(Data_disc_train)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
    # Data_disc_train=Data_disc_train[,c(colnames(Data_cont))]
    Id_target=id_target
    
    
    nulcut_var=vector()
    for (i in 1:length(cont_cutp))
    {
      if (!is.numeric(cont_cutp[[i]]) || is.na(cont_cutp[[i]]) || length(cont_cutp[[i]]) == 0)
      {
        nulcut_var=c(nulcut_var,cont_attr[i])
      }
    }
    disc_NB_interval=(length(cont_cutp)+sum(lengths(cont_cutp,use.names = TRUE)) - length(nulcut_var))/length(cont_cutp)
    # for the variables which are discretized into more than 53 bins, 
    # the bins are clustered into 52 bins to avoid some classifiers errors(RF for example does not support variables whith more than 53 levels)
    
    long_disc=which(lengths(cont_cutp)>25)
    
    if (length(long_disc)>0) 
    {
      for (a in long_disc)
       
      {
        d=bin(as.numeric(discret_Data[,cont_attr[a]]), nbins =20,method = "content")
        discret_Data[,cont_attr[a]]=d
      }
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
    }
    modif_nb_interv=(length(cont_cutp)+sum(lengths(cont_cutp,use.names = TRUE)) - length(nulcut_var))/length(cont_cutp)
    Data_disc_train=Data$training_set
    for (i in 1:length(cont_cutp))
    {
      splitpoints=unique(as.numeric(cont_cutp[[i]]))
      Data_disc_train[,cont_attr[i]]=cut(Data_disc_train[,cont_attr[i]],c(-Inf,as.numeric(splitpoints),Inf))
    }
    # summary(Data_disc_tst)
    Data_disc_tst=Data$test_set
    for (i in 1:length(cont_cutp))
    {
      splitpoints=unique(as.numeric(cont_cutp[[i]]))
      Data_disc_tst[,cont_attr[i]]=cut(Data_disc_tst[,cont_attr[i]],c(-Inf,as.numeric(splitpoints),Inf))
      
    }
    
    # remove attribute thtat had been discretized into only one level (to avoid error when using SVM and naivebayes classifier)
    if (!is.logical(nulcut_var))
    {
      Data_disc_train=Data_disc_train[,-nulcut_var]
      Data_disc_tst=Data_disc_tst[,-nulcut_var]
      Id_target=id_target-length(nulcut_var)
    }
    
    
    Data_train=Data_disc_train
    Data_tst=Data_disc_tst 
    perf_one_iter=perf_data_itera(Data_train,Data_tst,Id_target,disc_meth,disc_NB_interval)
    perf_one_iter[,"modif_nb_inter"]=modif_nb_interv
    perf_all_iter=rbind(perf_all_iter,perf_one_iter) 
  }
}

4320/120

# test=read.table("RFDiscperformance\\iris_01_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
write.csv(perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\iris_01_All_perf_Iterations.csv", row.names = FALSE)
write.csv(all_iter_splits,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\iris_01_Forestdisc_perfopt_Iterations.csv", row.names = FALSE)
write.csv(all_splitsselection,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\iris_01_Forestdisc_splitsselection.csv", row.names = FALSE)

perf_all_iter[,"name"]=name
perf_all_iter[,"exetimepervar"]=perf_all_iter[,"exe_time"]/length(cont_attr)
# all_iter_splits=read.table("RFDiscperformance\\iris_01_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
# all_splitsselection=read.table("RFDiscperformance\\iris_01_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")
  
all_iter_splits[,"name"]=name
all_splitsselection["name"]=name
write.csv(perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\RFD_iris_All_perf_Iterations.csv", row.names = FALSE)
write.csv(all_iter_splits,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\RFD_iris_Forestdisc_perfopt_Iterations.csv", row.names = FALSE)
write.csv(all_splitsselection,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\RFD_iris_Forestdisc_splitsselection.csv", row.names = FALSE)
test1=read.table("RFDiscperformance\\RFD_iris_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
test2=read.table("RFDiscperformance\\RFD_iris_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
test3=read.table("RFDiscperformance\\RFD_iris_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")



# perf_all_iter=perf_all_iter_forestdisc

perf_forestdisc= perf_all_iter[which(perf_all_iter[,"disc_method"] %in% 
                                       c("RFDiscret","Forest_disc_25","Forest_disc_50","Forest_disc_75",
                                         "Forest_disc_100","Forest_disc_125","Forest_disc_150",
                                         "Forest_disc_175","Forest_disc_200","Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
                                         "Forest_disc_DIR50","Forest_disc_DIR100","Forest_disc_DIR200")),]
A=all_splitsselection[,c(1,4,5,6)]
B=perf_forestdisc

A1=sqldf('select forest, var, iter, opt_value from A group by forest,var,iter' )
A11 = sqldf('select forest, iter, avg(opt_value) as opt_value from A1 group by forest,iter' )
perf_optsplit_summary = sqldf('select forest, avg(opt_value) as mean_opt_value, 
                              stdev(opt_value) as std_opt_value 
                              from A11 group by forest' )
# names(OSS_summary)
perf_disc_forest_nb = sqldf('select name, data, disc_method, 
                            avg(NB_interval) as mean_NB_interval, 
                            stdev(NB_interval) as std_NB_interval,
                            avg(exe_time) as mean_exe_time, 
                            stdev(exe_time) as std_exe_time,
                            avg(inconsistency) as mean_incons, 
                            stdev(inconsistency) as std_incons, 
                            avg(accuracy) as mean_acc, 
                            stdev(accuracy) as std_acc, 
                            avg(macroPrecision) as mean_Precision, 
                            stdev(macroPrecision) as std_Precision, 
                            avg(macroRecall) as mean_Recall, 
                            stdev(macroRecall) as std_Recall, 
                            avg(macroF1) as mean_F1, 
                            stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
                            stdev(kappa) as std_Kappa, 
                            avg(kappa_w) as mean_Kappaw, 
                            stdev(kappa_w) as std_Kappaw 
                            from B WHERE data="test" group by data,disc_method ') 
perf_disc_forest_summary= sqldf('select perf_disc_forest_nb.* , perf_optsplit_summary.* 
                                from perf_disc_forest_nb left join perf_optsplit_summary
                                on perf_disc_forest_nb.disc_method=perf_optsplit_summary.forest ' )


write.csv(perf_disc_forest_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\RFD_iris_perf_disc_RFD_summary.csv", row.names = FALSE)
test4=read.table("RFDiscperformance\\RFD_iris_perf_disc_RFD_summary.csv",sep=',',header= T, na.strings="?")

perf_disc_summary = sqldf('select  name, data, disc_method, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(exe_time) as mean_exe_time, 
                          stdev(exe_time) as std_exe_time,
                          avg(inconsistency) as mean_incons, 
                          stdev(inconsistency) as std_incons, 
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw, 
                          avg(modif_nb_inter) as mean_NB_interval, 
                          stdev(modif_nb_inter) as std_NB_interval
                          from perf_all_iter group by data,disc_method')

perf_disc_perclassifier = sqldf('select  name, data, class_method, disc_method, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(exe_time) as mean_exe_time, 
                          stdev(exe_time) as std_exe_time,
                          avg(inconsistency) as mean_incons, 
                          stdev(inconsistency) as std_incons, 
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw, 
                          avg(modif_nb_inter) as mean_NB_interval, 
                          stdev(modif_nb_inter) as std_NB_interval
                          from perf_all_iter group by data,class_method,disc_method')
write.csv(perf_disc_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\RFD_iris_perf_disc_summary.csv", row.names = FALSE)
write.csv(perf_disc_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\RFDiscperformance\\RFD_iris_perf_disc_perclassifier.csv", row.names = FALSE)
test5=read.table("RFDiscperformance\\RFD_iris_perf_disc_summary.csv",sep=',',header= T, na.strings="?")
test6=read.table("RFDiscperformance\\RFD_iris_perf_disc_perclassifier.csv",sep=',',header= T, na.strings="?")

