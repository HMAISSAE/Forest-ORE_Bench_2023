wd <- getwd()
setwd(wd)

data_name = "TICTACTOE"



# import data
data_path = "./data/tic-tac-toe.data.txt"
original_data=read.table(data_path,sep=",",header=FALSE, na.strings = "?")
summary(original_data)


# cleaning

# target class
id_target =10
dim(original_data)


table(original_data$V10)
for (i in 1:ncol(original_data)) 
{
    original_data[,i]=as.factor(original_data[,i])
    levels(original_data[,i])=gsub("_","-",levels(original_data[,i]))
}

str(original_data)

# 
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

