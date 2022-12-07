
wd <- getwd()
setwd(wd)

data_name = "haberman"


# import data
data_path = "./data/Haberman.data.txt"
original_data=read.table(data_path,sep=',',header=FALSE,na.strings="?")

str(original_data)
summary(original_data)

for (i in c(4)) 
{
  original_data[,i]=as.factor(original_data[,i])
}

id_target = 4
original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)
original_data[,id_target]= as.factor(original_data[,id_target])

if (original_data_summary$ncat_att >=1)
{
  for (i in eval(parse(text=paste0("c(",original_data_summary$cat,")"))))
  {
    original_data[,i]=as.factor(original_data[,i])
    levels(original_data[,i])=gsub("_","-",levels(original_data[,i]))
  }
}

if (original_data_summary$ncont_att >=1)
{
  for (i in eval(parse(text=paste0("c(",original_data_summary$cont,")"))))
  {
    original_data[,i]=as.numeric(original_data[,i])
    
  }
}

original_data=na.roughfix(original_data)
if (original_data_summary$ncont_att >=1)
{
  
  discretize_data = ForestDisc(data=original_data, id_target=id_target,max_splits = 4)
  original_data = discretize_data$data_disc
  
}
str(original_data)


source("./otherclassifiers3_execute_iterations_script_2022.R")

