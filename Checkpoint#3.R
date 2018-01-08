#setwd('C:/Users/Student/workspace')

rm(list=ls())
dat <- read.csv("/Users/Dhan/Downloads/Consumer_Complaints.csv", header = TRUE)
headers=names(dat)
headers

# Company response to consumer analysis 

in_progress <- which(sapply(dat[15], function(x) (x=="In progress")))
untimely <- which(sapply(dat[15], function(x) (x=="Untimely response")))

head(dat[17])
dat <- dat[-c(in_progress,untimely),]
##650807


summary(dat)

#Company.response.to.consumer
#Closed with explanation        :484563       
#Closed with non-monetary relief: 83332       
#Closed with monetary relief    : 44460       
#Closed without relief          : 17863       
#Closed                         : 15291       
#Closed with relief             :  5298       
#(Other)                        :     0  


levels(dat$Company.response.to.consumer)
summary(dat$Company.response.to.consumer)

levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with explanation"] <- "No"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed without relief"] <- "No"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed"] <- "No"



levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with monetary relief"] <- "Yes"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with non-monetary relief"] <- "Yes"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with relief"] <- "Yes"


levels(dat$Company.response.to.consumer)
summary(dat$Company.response.to.consumer)

#### Removed other levels, now only NO and YES are left
dat$Company.response.to.consumer <- factor(dat$Company.response.to.consumer)

table(dat$Company.response.to.consumer)
# Comparing likelihood of response"Yes" from the company
Uniform_prior <- table(dat[15])[2]/(table(dat[15])[1] + table(dat[15])[2])
Uniform_prior
summary(dat[15])


#table(dat[15])[1]+ table(dat[15])[2]
# Creating a table with company and response for dat
j<-with(dat,table(Company.response.to.consumer,Company))
#print(j)
dim(j)
# 2 3890
j1<-j[1,]+j[2,]
print(j1)
j2<-j1>25 
# Gives a boolean value for the given expression(Ans:[1]   2 908)
print(j2)
j3<-j[,j2]
print(j3[2,2])
#dim(j3)
chisqv<-array(0,dim =c(dim(j3)[2],2))
for (i in 1:dim(j3)[2]) {
  T=j3[1,i]+j3[2,i]
  Exp_val=T*Uniform_prior
  Yes=j3[2,i]
  chisqvalue=(Yes-Exp_val)^2/Exp_val
  chisqv[i,1]=1- pchisq(chisqvalue,1)
  chisqv[i,2]=(Yes/T)/Uniform_prior
  
}
rownames(chisqv)<-names(j3[1,])

lift_sort<-sort(chisqv[,2],decreasing = TRUE,index.return=TRUE)$ix
pval_sort<-sort(chisqv[,1],decreasing = TRUE,index.return=TRUE)$ix
lift_value<-chisqv[lift_sort,]
p_value<-chisqv[pval_sort,]
prob_fraud<-names(lift_value[,1])
j[,prob_fraud]
lift_list<-cbind(lift_value,array(0,dim = c(908,2)))
lift_list[,3:4]<-t(j[,prob_fraud])
print(lift_list)

# User experiences

Uniform_prior1 <- table(dat[17])[2]/(table(dat[17])[1] + table(dat[17])[2])
Uniform_prior1
summary(dat[15])


#table(dat[15])[1]+ table(dat[15])[2]
# Creating a table with company and response for dat
jj<-with(dat,table(Product,Consumer.disputed.))
h<-chisq.test(jj)
h
print(jj)
dim(jj)

chisqv1<-array(0,dim =c(dim(jj)[1],2))
for (i in 1:dim(jj)[1]) {
  T1=jj[i,1]+jj[i,2]
  Exp_val1=T1*Uniform_prior1
  Yes=jj[i,2]
  chisqvalue1=(Yes-Exp_val1)^2/Exp_val1
  chisqv1[i,1]=1- pchisq(chisqvalue1,1)
  chisqv1[i,2]=(Yes/T1)/Uniform_prior1
  
}
rownames(chisqv1)<-names(jj[,1])


#pval_sort<-sort(chisqv[,1],decreasing = TRUE,index.return=TRUE)$ix
lift_value<-chisqv1
lift_value
lift_list1<-cbind(lift_value,array(0,dim=dim(lift_value)))
dim(lift_list1)

lift_list1[,3:4]<- jj[,2:3]
print(lift_list1)
lift_sort<-sort(chisqv1[,2],decreasing = TRUE,index.return=TRUE)$ix
lift_list1<-lift_list1[lift_sort,]
#Predictor builder
yesrows=which(dat[15]=="Yes")
norows=which(dat[15]=="No")
rows=c(yesrows[1:5000],norows[1:5000])
rows1
dim(rows1)
rm(rows1)
outcome_ind=15
toInclude=c(2,3,4,5,9,13)
testdat<-dat[rows,toInclude]
dim(testdat)
#head(testdat[6])
#head(dat[17])
#names(dat)
outcome<-dat[rows,outcome_ind]
#rm(array)
array1<-model.matrix(~.,data = testdat[1])
for (j in 2:length(toInclude)) {
  model<-model.matrix(~.,data = testdat[j])
  array2<-cbind(array1,model)
  
}

library(randomForest)
forest<-randomForest(x=array2,y=outcome)

forest$confusion
forest$proximity
forest$err.rate
forest$oob.times
forest$ntree
forest$mtry
