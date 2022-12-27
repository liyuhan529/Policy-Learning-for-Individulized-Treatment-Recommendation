#-----------------------------------------------
# Data proproecssing
#-----------------------------------------------
source("process_source.R")

#---------------------------------
# 1. Extract data from xml

data_path = ".../OhioT1DM/2020/train/"

###training data
raw_train = list()
file_names = c("540-ws-training.xml", 
               "544-ws-training.xml", 
               "552-ws-training.xml", 
               "567-ws-training.xml", 
               "584-ws-training.xml", 
               "596-ws-training.xml")
nperson = length(file_names)
for (n in 1:nperson){

  data = xmlParse(paste(data_path, file_names[n],sep=""))
  rootnode = xmlRoot(data)
  variables = xmlChildren(rootnode)
  nvar = xmlSize(rootnode)
  raw_train[[n]] = list()
  varnames = rep("",nvar)
  for (i in 1:nvar){
    vari = variables[[i]]
    varnames[i] = xmlName(variables[[i]])
    data_var = t(as.matrix(xmlSApply(vari, xmlAttrs)))
    raw_train[[n]][[i]] = data_var
  }
  print(varnames)
  print(n)
}

names(raw_train[[1]])
raw_train[[1]][[13]]
head(raw_train[[1]][[19]],500)
head(raw_train[[1]][[12]],4)
raw_train[[1]][[1]]

str(variables)
str(raw_train[[1]])

str(raw_train[[3]][[12]])

head(raw_train[[6]][[19]],20)

str(raw_train[[1]][[16]])

str(raw_train[[3]][[2]])



raw_train[[6]][[17]]
str(raw_train[[5]][[12]])


# ### test data ###
# 
# raw_test = list()
# 
# data_path = ".../OhioT1DM/2020/test/"
# file_names = c("540-ws-testing.xml", 
#                "544-ws-testing.xml", 
#                "552-ws-testing.xml", 
#                "567-ws-testing.xml", 
#                "584-ws-testing.xml", 
#                "596-ws-testing.xml")
# nperson = length(file_names)
# for (n in 1:nperson){
#   data = xmlParse(paste(data_path, file_names[n],sep=""))
#   rootnode = xmlRoot(data)
#   variables = xmlChildren(rootnode)
#   nvar = xmlSize(rootnode)
#   raw_test[[n]] = list()
#   varnames = rep("",nvar)
#   for (i in 1:nvar){
#     vari = variables[[i]]
#     varnames[i] = xmlName(variables[[i]])
#     data_var = t(as.matrix(xmlSApply(vari, xmlAttrs)))
#     raw_test[[n]][[i]] = data_var
#   }
# }
# 
# 
# str(raw_test[[1]])
# str(raw_test[[1]][[18]])
# head(raw_test[[1]][[18]],20)
# str(raw_test[[1]][[1]])
# 
# raw_train[[4]][[9]]
#-----------------------------------------------
# 2. Preprocess data

# Into 30 min intervals 
interval = 30 #separate by half an hour
data_train = list()
data_test  = list()
data_full  = list()
for (n in 1:nperson){
  #training dat
  train0 = preprocess0(raw_train[[n]],n)
  train1 = preprocess1(train0, interval_min = interval)
  data_train[[n]] = preprocess2(train1, interval_min = interval)

  #testing data
  startdate = date(dmy_hms(train0$glucose[1, 1]))
  startbasal = data_train[[n]]$basal[length(data_train[[n]]$basal)]
 # test0 = preprocess0(raw_test[[n]],n)
  #test1 = preprocess1(test0, interval_min = interval, startdate = startdate)
  #data_test[[n]] = preprocess2(test1, interval_min = interval, startbasal = startbasal)
  
  #combine training and testing  
  #data_full[[n]] = rbind(data_train[[n]], data_test[[n]])
  data_full[[n]] = data_train[[n]]
  
  #generate extra variables
  #average basal rate of 4-8 hours before the current time point
  data_full[[n]] = preprocess3_past(data_full[[n]], 
                                    variable = "basal",
                                    interval_min = interval,
                                    lag_time_range = c(240, 480),
                                    new_var_name = "basal_4_8")
  #total amount of bolus from last time interval 
  data_full[[n]] = preprocess3_past(data_full[[n]],
                                    variable = "bolus",
                                    interval_min = interval,
                                    lag_time_range = c(30, 60),
                                    new_var_name = "bolus_lag_halfhour",
                                    FUN = sum)
  #total amount of carb input in the next half an hour
  data_full[[n]] = preprocess3_future(data_full[[n]],
                                      variable = "meal",
                                      interval_min = interval,
                                      lag_time_range = c(0,30),
                                      new_var_name = "meal_future_halfhour",
                                      FUN = sum)
  print(n)
}


nperson

names(data_full[[5]])


sum(data_full[[3]]$bolus != 0)

train0$glucose[200:240]

data_full[[1]]$steps

save.image("data_preprocessed-halfhour.Rdata")

#--------
#Another option is to preprocess into 5min intervals
interval = 5 #separate by half an hour
data_train = list()
data_test  = list()
data_full  = list()
for (n in 1:nperson){
  #training data
  train0 = preprocess0(raw_train[[n]],n)
  train1 = preprocess1(train0, interval_min = interval)
  data_train[[n]] = preprocess2(train1, interval_min = interval)
  
  #testing data
  startdate = date(dmy_hms(train0$glucose[1, 1]))
  startbasal = data_train[[n]]$basal[length(data_train[[n]]$basal)]
  test0 = preprocess0(raw_test[[n]],n)
  test1 = preprocess1(test0, interval_min = interval, startdate = startdate)
  data_test[[n]] = preprocess2(test1, interval_min = interval, startbasal = startbasal)
  
  #combine training and testing  
  data_full[[n]] = rbind(data_train[[n]], data_test[[n]])
  print(n)
}



