library(readr)
library(tidyverse)
require(deaR)
require(lpSolve)

#https://search.r-project.org/CRAN/refmans/deaR/html/00Index.html
setwd("C:/Users/Kevin Palomino/OneDrive - Universidad del Norte/TESIS/BERTHA VILLALOBOS/DEA/Data")

BD_DEA <- read_csv("BD_DEA.csv")

#x1<-c(8,11,14,12,11,18)
#x2<-c(8,15,12,13,18,20)
#y1<-c(14,25,8,25,40,24)
#y2<-c(20,42,30,8,22,30)
#BD_DEA<-data.frame(x1,x2,y1,y2)
#row.names(BD_DEA)<-c("A","B","C","D","E","F")
head(BD_DEA)


#################################### Form 1
data <- select(BD_DEA,"...1", "AG","EN","CH","CS","IN","MF","DS1","DS2","DE")
model <- make_deadata(data, ni=6, no=3, dmus = 1, inputs = 2:7,outputs =8:10 )
result <- model_basic(model,orientation = "io", rts = "crs",dmu_eval = 1:120,dmu_ref = 1:120) # nolint
#print(result)
print(efficiencies(result))
print(targets(result))
#summary(result)
plot(result)

#################################### Form 2

inputs <- data.frame(BD_DEA[c(2,3,4,5,6,7)]) # input variable
outputs<-data.frame(BD_DEA[c(8,9,10)]) # output variables
N <- dim(BD_DEA)[1] # the number of DMUs
s <- dim(inputs)[2] # number of input variables
m <- dim(outputs)[2] # number of output variables,

f.rhs <- c(rep(0,1,N),1)
f.dir <- c(rep("<=",1,N),"=")
aux <- cbind(-1*inputs,outputs)


for (i in 1:N) {
  f.obj <- c(0*rep(1,s),as.numeric(outputs[i,]))
  f.con <- rbind(aux ,c(as.numeric(inputs[i,]), rep(0,1,m)))
  results <- lp ("max",as.numeric(f.obj), f.con, f.dir, f.rhs,scale=0, compute.sens=TRUE)
  if (i==1) {
    weights <- results$solution
    effcrs <- results$objval
    lambdas <- results$duals[seq(1,N)]
  } else {
    weights <- rbind(weights, results$solution)
    effcrs <- rbind(effcrs , results$objval)
    lambdas <- rbind(lambdas, results$duals[seq(1,N)] )
  }
} 

#rown= matrix(nrow = 120,ncol=1)

#for (i in 1:120){
#  rown[i,1]=paste0("Emp",i)
#}


##Results
# merge the efficiency and multipliers
spreadsheet <- cbind(effcrs,weights)
# assign the utilities’ names to the spreadsheet rows
rownames(spreadsheet) <- row.names(BD_DEA)
# assign the variables names to the spreadsheet columns
colnames(spreadsheet) <- c('efficiency',names(inputs),names(outputs))
write.csv(spreadsheet ,"RESULT_DEA_PRIMAL.csv",col.names = TRUE)


# duals variables
spreadsheet2<-lambdas
# assign the utilities’ names to the spreadsheet rows and columns
rownames(spreadsheet2)<-row.names(BD_DEA)
colnames(spreadsheet2)<- row.names(BD_DEA)
write.csv(spreadsheet2 ,"RESULT_DEA_DUAL.csv",col.names = TRUE)


# CONTIBUCION INPUT/OUTPUT A LA EVALUACION DE LA EFICIENCIA

opt1=matrix(ncol = N, nrow = 1)
opt2=matrix(ncol = N, nrow = 1)
inp1=matrix(ncol = N, nrow = 1)
inp2=matrix(ncol = N, nrow = 1)
peso =t(spreadsheet[,-1])
for( i in 1:N){
  opt1[1,i]=round(peso[3,i]/(peso[3,i]+peso[4,i])*100,2)
  opt2[1,i]=round(peso[4,i]/(peso[3,i]+peso[4,i])*100,2)
  inp1[1,i]=round(peso[1,i]/(peso[1,i]+peso[2,i])*100,2)
  inp2[1,i]=round(peso[2,i]/(peso[1,i]+peso[2,i])*100,2)
}
rbind(opt1,opt2,inp1,inp2)




#Graphs
par(mar=c(10,5,1,10),xpd=TRUE) # set plot margin
palette(gray(0:8 / 8)) # set color palette
virtual<-weights[,(s+1):(s+m)]*outputs # virtual outputs
rownames(virtual)<-rown # assign utilities’ names to the rows of the object virtual
barplot(t(virtual),col=palette()[c(1,4,7)],ylab="Efficiency",cex.axis=1,cex.lab=1,cex.names=1,las=3)
legend("topright",inset=c(-0.45,0),colnames(virtual),fill=palette()[c(1,4,7)],bty="n") # add legend

