
wd <- getwd()
setwd(wd)

data_name = "texture"

# import data
data_path = "./data/texture.dat"
original_data=read.table(data_path,sep=",",header=FALSE, na.strings = "?")


for (i in c(41) )
{
    original_data[,i]=as.factor(original_data[,i])
}
id_target = 41

original_data_summary= summarize_data(DataSet=original_data,name=data_name,id_target=id_target)
data_summary_Path = paste0(".\\data_summary\\", data_name, "_datasummary.csv")
write.csv(original_data_summary,data_summary_Path, row.names = FALSE)
read.csv(data_summary_Path)


str(original_data)

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



# --------------------------------------------------------------------------
source("execute_iterations_sript_without_abl.R")
