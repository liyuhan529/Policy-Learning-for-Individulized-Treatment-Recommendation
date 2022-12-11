dosegrid_func <- function(bolus_input,gridlen){
  
  dose_grid = bolus_input
  dose_grid[bolus_input == 0] = 0 
  dose_grid[bolus_input > 0  & bolus_input <= 1*gridlen] = 1 
  dose_grid[bolus_input > 1*gridlen  & bolus_input <= 2*gridlen] = 2 
  dose_grid[bolus_input > 2*gridlen  & bolus_input <= 3*gridlen] = 3 
  dose_grid[bolus_input > 3*gridlen & bolus_input <= 4*gridlen] = 4 
  dose_grid[bolus_input > 4*gridlen &bolus_input <= 5*gridlen] = 5 
  dose_grid[bolus_input > 5*gridlen &bolus_input <= 6*gridlen] = 6 
  dose_grid[bolus_input > 6*gridlen &bolus_input <= 7*gridlen] = 7 
  dose_grid[bolus_input > 7*gridlen &bolus_input <= 8*gridlen] = 8 
  dose_grid[bolus_input > 8*gridlen &bolus_input <= 9*gridlen] = 9 
  dose_grid[bolus_input > 9*gridlen &bolus_input <= 10*gridlen] = 10 
  dose_grid[bolus_input > 10*gridlen &bolus_input <= 11*gridlen] = 11 
  dose_grid[bolus_input > 11*gridlen &bolus_input <= 12*gridlen] = 12 
  dose_grid[bolus_input > 12*gridlen ] = 13 
  
  return(dose_grid)
}


#############################################

devtools::install_github("vincentskywalkers/ProximalDTR-/proximalDTR")
trainset <- data_full[[1]]

dim(trainset)
names(trainset)

#train <- data.frame(trainset$day,trainset$tt,trainset$glucose,trainset$heart,
#                    trainset$bolus,trainset$meal,trainset$icg)


train <- data.frame(trainset$day,trainset$tt,trainset$glucose,
                    trainset$bolus,trainset$meal,trainset$acceleration,
                    trainset$icg)

trainset

train1=train


## patient 1
i = 0
count.index = c()
while (i*48 < nrow(train1)){
	if ( sum(is.na(train1[(i*48+1):((i+1)*48),] )) <= 16){
		count.index = c(count.index,i)
	}
	i  = i+1
}
count.index

complete_train1 = train1[train1$trainset.day %in% count.index,]
colnames(complete_train1) = c("day","tt","glucose","bolus","meal","acceleration","icg")

complete_train1$acceleration[is.na(complete_train1$acceleration)] = mean(complete_train1$acceleration,na.rm = T)
complete_train1$glucose[is.na(complete_train1$glucose)] = mean(complete_train1$glucose,na.rm = T)
complete_train1$icg[is.na(complete_train1$icg)] = mean(complete_train1$icg,na.rm = T)


complete_train1$icg
des=rep(0.9^(0:47),13)
obs_rew=colSums(matrix(des*complete_train1$icg,nrow=48))
obs_rew
mean(obs_rew)
result=c()
obs=c()
#set.seed(11)
for (i in 1:50){
	set.seed(i*10)
	uni = unique(complete_train1$day)
	ind = sample(uni,11)
	print(ind)
	indd=complete_train1$day %in% ind
	train_new = complete_train1[indd,]
	train_new1 = train_new[order(train_new$tt),]
	X = train_new1[,c(3,5,6)]
	#A is the longitudinal assigned treatment 
	A = train_new1[-c((dim(X)[1]-10):dim(X)[1]),4]
	length(A)
	#R is the longitudinal reward 
	R = train_new1[-c((dim(X)[1]-10):dim(X)[1]),7]
	N=11
	fit1 = proximalDTR(X = X, A=A, R=R, n_ID =11 , stage=48, gamma=0.9,
										 lambda.set = c(4.8),step.beta = 0.01, step.omega = 0.01,
										 desc.rate = 0.0001, max.iter = 3000, max.iter.cv = 1000, bw = 1,
										 cv=F, trace =F)
	obs[i] = mean(obs_rew[which(uni%in%ind)])
	result[i] = fit1$para.value[length(fit1$para.value)]
	print(c(i,obs[i],result[i]))
}





unique(complete_train1$day)
sample(13,10)
X = train_new1[,c(3,5,6)]
#A is the longitudinal assigned treatment 
A = train_new1[-c((dim(X)[1]-12):dim(X)[1]),4]
length(A)
#R is the longitudinal reward 
R = train_new1[-c((dim(X)[1]-12):dim(X)[1]),7]
N=13
fit1 = proximalDTR(X = X, A=A, R=R, n_ID =13 , stage=48, gamma=0.9,
									 lambda.set = c(4),step.beta = 0.01, step.omega = 0.01,
									 desc.rate = 0.0001, max.iter = 3000, max.iter.cv = 1000, bw = 1,
							 cv=F, trace =TRUE)
fit1$para.value


#############################################################################

