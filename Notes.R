### Basic R ###
##### 
class(x) #Check class
c(a,b,c) #Vectors(same type)
matrix(data = ,nrow = ,col = ) #Matrix(same type)
cbind(a,b) + rbind(a,b) #combine by col and row respectively
list(a,b,c) #List(diff type)
data.frame(c(a,b,c)) #Data frame(diff type)
names(BDAD) = c('Months','Defects') #headers for data frame
min,max,mean,median,quantile,IQR,var,sd,sqrt,cov,cor #Functions available
#sd calculates sample sd. Covert to population via:
#  sqrt(  var()*(n-1)(n)  )
as.factor()/as.numeric()/as.character()/as.data.frame() #converting
#####

#### Generating new variable ###
#####
df1$Checking.Status=NA
attach(df1)
df1$Checking.Status[df1$Checking.<250] = 'low'
df1$Checking.Status[df1$Checking.>=250&df1$Checking.<2000] = 'medium'
df1$Checking.Status[df1$Checking.>2000] = 'high'
detach(df1)
table(asd)
ifelse #to generate categorical variables
center = TRUE #subtract mean form each data point
scale = TRUE #divide points by sd
set.seed() #set the seed for same set of random numbers. Only works once (reset again to generate same set)
#####

### Frequency distribution ###
#####
bins = seq(10,35,5)
mpg.cut = cut(mtcars$mpg, bin, right = TRUE) #cut into bins for large intervals, right true to include
mpg.freq = table(mpg.cut) #frequency distribution
cbind(mpg.freq)
mpg.relfreq = mpg.freq/nrow(mtcars) #rel freq
cbind(mpg.relfreq)
cbind(mpg.freq,mpg.relfreq)
mpg.cumrelfreq = c(0,cumsum(mpg.relfreq)) #cumulative rel freq. First data have no cumsum. MAYBE NO NEED 0
cbind(mpg.freq,mpg.relfreq,mpg.cumrelfreq)
#####

### cross-tabulation bar plot ###
#####
counts = c(a,b)
barplot(couns,main,xlab,col = c('red','blue'))
#####

### Histogram ###
#####
hist(mtcars$mpg,main,xlab,xlim,breaks = seq(0,90,10)) #xlim is the range of values
#used for plotting, not to define the bins
hist(wt,probability = TRUE) #proportions
lines(density(wt)) #density plot
#####

### ogive ###
#####
#ONLY FOR WHEN THERE IS A NEED FOR BINS
mon = df1$Months.Customer
breaks = seq(0,80,10) #use range to determine values
mon.cut = cut(mon,breaks,right = TRUE)
mon.freq = table(mon.cut) #frequency
cbind(mon.freq)
mon.relfreq = mon.freq/nrow(df1) #rel freq
cbind(mon.freq,mon.relfreq)
mon.cumrelfreq = c(0,cumsum(mon.relfreq)) #cum rel freq
plot(breaks,mon.cumrelfreq)
lines(breaks,mon.cumrelfreq)
#alt
plot(ecdf(wt),do.points = FALSE,verticals = TRUE)
x = seq(min(wt),man(wt),0.01)
lines(x,pnorm(x,mean = mean(wt),sd = sd(wt),lty = 3)) #fit a norm dist
#alternatively,
plot.ecdf(data,verticals = TRUE) #density graph
#####

### boxplot ###
#####
boxplot() #points beyond 1.5*IQR from Q3 or Q1 are outliers
points(mean(),pch = 5,col = ) #to insert the mean
q3 = unname(quantile(data,0.75)) # get the q3
upper = q3+ 1.5*IQR(data) #upper limit. Beyond are outliers
cost = df1[which(data<=upper),'data'] #removing the outliers
mean(cost)+c(-2*sd(cost),2*sd(cost)) #2-sigma emperical rule
#multiple boxplot
boxplot(mpg~cyl,data = )
means = aggregate(mtcars$mpg~mtcars$cyl,FUN = 'mean')
points(1:3,means$mtcars$mpg,pch,col)
#####

###line graph ###
#####
plot(data,xlab,main,ylab,type,col,ylim = c(4,6))
points(data,pch =)
lines()
abline() # h= value for horizontal line, v=value for vertical line
# control limts is +- 3 sd
legend('topright',c('header1','header2','header3'),col = c('blue','black','red'),lwd = c(1,1,1),cex = 0.7)
legend(1,2,c('header1','header2','header3'),col = c('blue','black','red'),cex = 0.7,lty = 1) #alternatively
#####

### pivot table ###
#####
rpivotTable(df3,rows = 'Payor',cols='Value.Rating',aggregatorName = 'Average',vals='Wedding.cost')
#####

### subset ###
#####
order_no=c('Aug11008','Sep11023','Oct11020') #what you want to find
subset(df2, Order.No.%in%order_no, select=c(Order.No.,Item.No.,Cost.per.order))
item1369 = subset(df2,df2$Item.No.==1369,select=c(Item.No.,Cost.per.order))
item1369 = df2[which(df2$itemno == 1369),c('header1','header2')] #alternatively, can use this
#####

### merging data frames ###
#####
df3 = merge(df1,df2,by = 'header')
#####

### conditions ###
#####
mtcars[which(mtcars$am == 0),] #the rows of obs
mtcars[which(unique(mtcars$am) == 0),] #to prevent the duplicate of datas
subset(mtcars,mtcars$am == 0)
#####

### test for normality ###
#####
shapiro.test(wt) #p-value<0.05 means not normal 
#####

### samples ###
#####
mtcars[sample(1:nrows(mtcars),10),] #select 10 random sample rows
#####

### CI for mean with KNOWN population sd ###
#####
mu = mean(df2$) #population mean
sd(df2$) #SAMPLE sd
sigma = sd(df2$)*sqrt((nrow(df3)-1)/nrow(df3)) #POPULATION sd
xbar = mean(cr2$) #sample mean
n = nrow(cr2) #sample size
z.alpha = qnorm(1-(0.05)/2) #normal dist
err1 = z.alpha*sigma/sqrt(n) #margin of error
xbar + c(-err1,err1) #95% CI
#####

### CI for mean with UNKNOWN population sd ###
#####
xbar = mean(cr2$) #sample mean
s = sd(cr2$) #SAMPLE sd
t.alpha = qt(1-(0.05)/2,df = n-1) #t-distribution
err2 = t.alpha*s/sqrt(n)
xbar +c(-err2,err2)
#####

### CI for proportion ###
#####
p = nrow(df2[which(df2$Graduate.Degree. == 'Y'),])/nrow(df2)
z.alpha = qnorm(1-alpha/2)
err3 = z.alpha*(sqrt((p*(1-p))/n))
p + c(-err3,err3)

### Prediction interval ###
#####
err = t.alpha * (s*sqrt(1+(1/n)))
xbar + c(-err,err)
#####

### hypo testing ###
#####
### one sample test ###
#two tailed test, unknown population sd
t.test(y,mu = mu0) #h0:mu=mu0, what the qn is asking for is in alt hypo
#lower tailed test, unknown population sd
t.test(y,mu = mu0,alt = 'less')
#upper tailed test, unknown population sd
t.test(y,mu = mu0,alt = 'greater')
#two tailed test for proportion
prop.test(x,n,p=pi10) #x is the count of successes, n is the total trials (all in vector)
#lower tailed test for proportion
prop.test(x,n,p=pi0,alt = 'less')
#upper tailed test for proportion
prop.test(x,n,p=pi0,alt='greater')
### two sample test###
#two independent samples
t.test(y~x) #y is numeric, x is factor
t.test(y1,y2) #y1 and y2 are numeric
t.test(y~x, var.equal = TRUE) #equal variance(you know that it is equal or qn says so)
#two paired sample
t.test(y1,y2,paired = TRUE)
#variance test
var.test(y~x)

#one sample test for the mean using t and crit value
##if t/z lie between/beyond crit vales, we fail to reject null hypo
#unknown popn sd
#1) state the null and alt hypo
mu0 = 4500 #(upper tailed test)
#2)level of significance
alpha = 0.05
#3) get the test statistics (use t test as popn sd unknown)
xbar = mean(df2$profit)
s = sd(df2$profit)
n = nrow(df2$profit)
t = (xbar-mu0)/(s/sqrt(n))
t.alpha = qt(1-alpha,df = n-1) #qt is crit value, t test. pt is p value, t test
#since t<t.alpha(crit value) and this is an upper tailed test, fail to reject h0
##alternatively, use p-value##
pval = pt(t,df=n-1,lower.tail = FALSE) #popn sd UNKNOWN,t-test
##alternatively, simply use t.test
t.test(df2$profit,mu=4500,alt = 'greater')

#KNOWN POPN SD
z = (xbar???mu0)/(sigma/sqrt(n)) #popn sd KNOWN, z-test
z.alpha = qnorm(1-alpha) #1 tailed
z.half.alpha = qnorm(1-alpha/2) #2 tailed
c(-z.half.alpha,z.half.alpha) #popn sd KNOWN, z-test,two-tailed
pval = pnorm(z) #popn sd KNOWN,z-test, lower tailed
pval = pnorm(z,lower.tail = FALSE) #upper tailed
pval = 2*pnorm(z) #two-tailed

#ANOVA test
your.aov = aov(x~y) #null hypo is that assume all the means are equal to each other
your.aov
summary(your.aov)
#####

#Trendline and regression analysis
#####
#1.plot linear model
#2.get p-values to decide if significant or not
#3.examine residuals via use plot(fit)
#4.decide if you should use other models
#5.upon settling on a model, If linear, 1)do plot(fit) to examine residuals and 2)compare p-values. Else, get R2
#6.produce regression coefficient via summary(fit)
#parameters
par(mfrow = c(2,2)) #2 by 2
plot(fit) #where fit = lm(), use to check if residuals have a linear or non-linear pattern
summary(fit) #to check p-values. 1. determine if signifiant. 2. compare to other p-values to see which better

#Linear
fit = lm(y~x)
plot(x,y,pch = 20,main = 'Linear ',xlab = 'samples',ylab = 'Production time')
abline(fit,col ='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#multiple regression
fit = lm(y~x1+x2+x3+x4) #use summary(fit) to produce coefficients! (under Estimate). Afterwards, use plot(fit) again.

#Exponential
f = function(x,a,b){a*exp(b*x)}
model0 = lm(log(y)~x)#setting the starting values
start0 = list(a=exp(coef(model0)[1]),b=coef(model0)[2])#setting the starting values
fit = nls(y~f(x,a,b),start = start0)
co = coef(fit)
plot(x,y,pch = 20,main = 'Exponential ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Logarithmic
f=function(x,a,b){a*log(x)+b}
fit = nls(y~f(x,a,b),start = c(a=1, b=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Logarithmic ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Polynomial 2nd
f = function(x,a,b,d){a*x**2+b*x+d}
fit = nls(y~f(x,a,b,d),start = c(a=1,b=1,d=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 2nd ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2],d=co[3]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#Polynomial 3rd degree
f = function(x,a,b,d,e){(a*x^3)+(b*x^2)+(d*x)+e}
fit = nls(y~f(x,a,b,d,e),start = c(a=1,b=1,d=1,e=1))
co = coef(fit)
plot(x,y,pch = 20,main = 'Polynomial 3rd ',xlab = 'samples',ylab = 'Production time')
curve(f(x,a=co[1],b=co[2],d=co[3],e=co[4]),add=TRUE,col='red')
#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
R2

#extrapolation
curve(f(x,a=co[1],b=co[2],d=co[3],e=co[4]),from=0,to=60,main = 'Defects without implementation',xlab = 'number of months',ylab = 'number of')

#computing errors
SSE = sum(residuals(fit)^2)
SST = sum((y-mean(y))^2)
R2 = 1-SSE/SST #Higher R2 means better fit
#####

### Forecasting ###
#####
#observing seasonality and trend
plot(stl(myts,s.window = 'period'))
plot(decompose(myts))
###
#If time series does not have a definite spread of years, use linear model, find coefficient and manually forecast

#time series data
myts = ts(df1$Rate,start = c(2010,1),end = c(2013,12),frequency = 12) #start is for date, not data. frequency is number of obs per unit time
myts
myts = window(myts,start=c()) #to choose a certain period
plot(myts)
ma.forecast = ma(myts,order = 3,centre = FALSE) #centre for even k
ma.ttr3 = SMA(myts,n=3) #n is number of past value (period)  no seasonality, no trend
ma.ttr3 = append(ma.ttr3,NA,after =0)
myts = append(myts,NA,after = length(ts))
plot(myts)
lines(ma.ttr3, col = 'red') #without appending

#Holt winter
fit # to find the smoothing constant
#simple exponential - models level
fit = HoltWinters(myts,beta = FALSE,gamma = FALSE) # no seasonality, no trend
#double exponential - models level and trend
fit = HoltWinters(myts,gamma = FALSE)
#Triple exponential - models level and trend and seasonality, ADDITIVE
fit = HoltWinters(myts)
#Triple exponential - models level and trend and seasonality, MULPLICATIVE
fit = HoltWinters(myts,seasonal = 'mult')
#known smoothing constant
fit = HoltWinters(myts,alpha = 0.3,beta = FALSE,gamma = FALSE)
accuracy(forecast(fit))#check accuracy of forecast, fit is holtwinter-ed
forecast(fit,8) #8 values of forecast
plot(forecast(fit,8))

#multiple regression forecasting, no trend, got seasonality
feb = ifelse(($df1$months == 'Feburary'),1,0) #do for 11 months
fit = lm(y~time+feb+wed...) # jan is the reference month.
summary(fit)
# keep removing variables and re-checking p values and R2 to find best model

#computing error matrics
MAD = mean(abs(ttr-ts),na.rm = TRUE)
MSE = mean((ttr-ts)^2,na.rm = TRUE)
MAPE = mean(abs(ttr-ts)/ts,na.rm = TRUE)
#####

### Data Mining ###
#####
#categorical/interval data
ggparallel(vars = list(),data,order = 1)
#Ratio data
parcoord(df,col=rainbow(nrow(df)),var.label = TRUE) #parallel coordinate
ggcoord(df,columns = 1:ncol(df),scale = 'globalminmax')
#Scatterplot matrix
pairs(~mpg+dist+drat+...,data =mtcars)
scatterplotMatrix(~mpg+dist+drat...,data = mtcars,) #use reg.line = FALSE and smoother = FALSE to remove lines

#k-NN
df2.labelled = df2[1:63,1:6] #labelled data, to be split into 70% and 30% later
df2.new = df2[63:70,1:6] #neww data, to predict the outcome later
##splitting data into 70% and 30%
set.seed(10)
rnd = sample(2,nrow(df2.labelled),replace = TRUE,prob = c(0.7,0.3)) #random sample with 2 outcomes, number to generate is nrow
df2.train = df2[rnd == 1,]
df2.test = df2[rnd == 2,]
#make sure training labels are factors
class(df2.train$Status)
#apply knn
status.pred = knn(train = df2.train[,1:5], test = df2.test[,1:5], cl = df2.train$Status, k=3, prob = TRUE) #[,1:5] is to det which
#variables to be used to predict
status.pred
#accuracy of your knn
table(df2.test$Status,status.pred) #confusion table, comparing original test data and test results
mean(df2.test$Status == status.pred) #total % correct
#predict for new data
new.pred = knn(train = df2.train[,1:5], test = df2.new[,1:5], cl = df2.train$Status, k=3, prob = TRUE)
new.pred

#Discriminant Analysis
df2.labelled = df2[1:63,1:6] #labelled data, to be split into 70% and 30% later
df2.new = df2[63:70,1:6] #new data, to predict the outcome later
##splitting data into 70% and 30%
set.seed(10)
rnd = sample(2,nrow(df2.labelled),replace = TRUE,prob = c(0.7,0.3)) #random sample with 2 outcomes, number to generate is nrow
df2.train = df2[rnd == 1,]
df2.test = df2[rnd == 2,]
#Derive classification rule using lda
lda.fit = lda(Decision~credit.Score+Years.Of.Credit+..., data = df3$train) #training data
lda.fit #each of the data have their discriminant function
#Apply classifiction rule to test data
lda.pred.test = predict(lda.fit, newdata = df3$test) #test data
lda.pred.test$class
#accuracy of lda
table(df3.test$Decision,lda.pred.test$class) 
mean(df3.$test.Decision == lda.pred.test$class)
#classify new data
lda.pred.new = prefict(lda.fit,newdata = df2.new)
lda.pred.new$class


#Logistic Regression
df2.labelled = df2[1:63,1:6] #labelled data, to be split into 70% and 30% later
df2.new = df2[63:70,1:6] #new data, to predict the outcome later
##splitting data into 70% and 30%
set.seed(10)
rnd = sample(2,nrow(df2.labelled),replace = TRUE,prob = c(0.7,0.3)) #random sample with 2 outcomes, number to generate is nrow
df2.train = df2[rnd == 1,]
df2.test = df2[rnd == 2,]
#Deriving classisfication rule
glm.fit = glm(Decision~credit.Score+Years.Of.Credit+..., data = df3$train,family = binomial) #note family = binomial
summary(glm.fit)
#applying the classification rule to test data
glm.pred.test = predict(glm.fit,newdata = df3.test,type = 'response') #
glm.pred.test
#accuracy of prediction
table(df3$.test$Decision,glm.pred.test > 0.5)
mean(df3$.test$Decision, (glm.pred.test > 0.5))
#classify new data
glm.pred.new = predict(glm.fit,newdata = df3.new, type = 'response')
glm.pred.new
pred.class = as.numeric(glm.pred.new>0.5)
#####


#Monte Carlo
#####
#eg 1
#supp cost
set.seed(12345)
unit.supp.cost = rnorm(10000,mean = 175, sd = 12)
summary(unit.supp.cost)
hist(unit.supp.cost, main = 'Unit Supplier Cost')
#prod vol
set.seed(12345)
prod.vol = rtriang(10000,min = 800,max = 1700, mode = 1400)
summary(prod.vol, main = 'Production Volume')
hist(prod.vol)
#given datas
fixed.cost = 50000
unit.var.cost = 125
total.mfg.cost = unit.var.cost*prod.vol + fixed.cost #total manufacture cost
hist(total.mfg.cost, main = 'Total Manufacture Cost')
total.supp.cost = unit.supp.cost*prod.vol #total cost if bought from supplier
hist(total.supp.cost, main = 'Total Supplier Cost')
cost.diff = total.mfg.cost-total.supp.cost #should outsouce if cost.diff > 0
hist(cost.diff, main = 'Cost difference')
abline(v=0,col='red') #to the right is outsource, to the left is manu in house
cf = ecdf(cost.diff) #fit a density graph
cf(0) #P(cost.diff <= 0)
1-cf(0) #P(cost.diff >0)

#eg 2 CI for the mean
set.seed(12345)
RD.cost = rtriang(10000,min = 500000000, max = 800000000, mode = 700000000)
set.seed(12345)
clinic.trial.cost = rtriang(10000,min = 135000000, max = 160000000, mode = 150000000)
total.proj.cost = RD.cost + clinic.trial.cost
set.seed(12345)
mrkt.size = rnorm(10000,mean = 2000000,sd = 250000)
set.seed(12345)
mrkt.share = runif(10000,min = 0.06,max = 0.10)
disc = 0.09
unit.rev = 130 #monthly
unit.cost = 40 #monthly
mrkt.growth = 1.03
mrkt.share.growth = 1.2
#year 1
sales1 = mrkt.size*mrkt.share
rev1 = sales1*unit.rev*12
annual.cost.1 = sales1*unit.cost*12
profit1 = rev1-annual.cost.1
cnp1 = profit1-total.proj.cost
z.alpha = qnorm(1-0.05/2)
mean(cnp1)+c(-z.alpha*((sd(cnp1))/sqrt(10000)),z.alpha*((sd(cnp1))/sqrt(10000)))

#eg 3 npv
set.seed(12345)
cf1 = rnorm(10000,mean = 8000,sd = 500)
cf2 = rnorm(10000,mean = 8000,sd = 500)
cf3 = rnorm(10000,mean = 8000,sd = 500)
cf4 = rnorm(10000,mean = 8000,sd = 500)
cf5 = rnorm(10000,mean = 8000,sd = 500)
invest = 30000 #initial investment
disc = 0.08 #discount rate
npv = cf1/(1+disc)+cf2/(1+disc)^2+cf3/(1+disc)^3+cf4/(1+disc)^4+cf5/(1+disc)^5 - invest #formula for npv
hist(npv,main = 'Net Present Value')
abline(v=0,col = 'red') # breakeven point, no difference in investing or not investing
cdf = ecdf(npv)
cdf(0) #P(X<=0), where X is profits. This is the prob of making a loss. If high, means risky
1-cdf(0) #P(X>0), probability of making a profit. If high, means it is not so risky
#####


#Linear optimisation
#####
#lpSolve
ads.obj = c(350,800)
ads.constr = matrix(c(400,2000,-7,3), ncol = 2, byrow = TRUE)
ads.dir = c('<=','>=')
ads.rhs = c(25000,0)
ads.sol = lp('max',ads.obj,ads.constr,ads.dir,ads.rhs,compute.sens = 1)
ads.sol$solution #decision variable values
ads.sol$objval #objective functon value
ads.sol$sens.coef.from #sensitivity from
ads.sol$sens.coef.to #sensitivity to

#lpSovleAPI
lp.model = make.lp(ncol = 2)
lp.control(lp.model,sense='max')
set.objfn(lp.model, c(350,800))
cstr1 = c(400,2000)
add.constraint(lp.model,cstr1,type = '<=',rhs = 25000)
cstr2 = c(-7,3)
add.constraint(lp.model,cstr2,type = '>=',rhs = 0)
solve(lp.model)
get.variables(lp.model)
get.objective(lp.model)
get.sensitivity.obj(lp.model)#sensitivity analysis

#table with rhs on both sides
obj.fun = c(8,6,3,2,4,9)
m=2
n=3
constr = matrix(0,n+m,n*m)
for (i in 1:m) {
  for (j in 1:n) {
    constr[i,n*(i-1)+j] = 1
    constr[m+j,n*(i-1)+j] = 1
  }
}
constr.dir = c(rep('<=',m),rep('>=',n))
rhs = c(70,40,35,25)

#multiple optimal solution
#solving model
rec <-solve(lps.model)
sol <-get.variables(lps.model)
obj<-get.objective(lps.model)
#alternative optimal solutions
sols <-list()
xt<-c(-75, 50)
cons <-xt%*%sol
while(TRUE) {
  sols <-c(sols, list(sol))
  cons <-cons -1
  add.constraint(lps.model, xt, "<", cons)
  rec <-solve(lps.model)
  if (rec!=0) break;
  if (get.objective(lps.model)<obj) break;
  sol <-get.variables(lps.model)
  obj<-get.objective(lps.model)
}
sols
#####


#Integer optimisation
#####
#working hours lpsolveAPI
n.obj = c(1,1,1,1,1)
n.constr = matrix(c(1,0,0,0,0,
                    1,1,0,0,0,
                    0,1,1,0,0,
                    0,0,1,1,0,
                    0,0,0,1,1,
                    0,0,0,0,1),ncol = 5,byrow = TRUE)
n.dir = c(rep('>=',6))
n.rhs = c(5,12,14,8,14,10)
n.sol = lp('min',n.obj,n.constr,n.dir,n.rhs,int.vec = 1:5)
n.sol$objval
n.sol$solution
#to restrict, add add constraint: X1+X2+X3+X4+X5 = 33(n.sol$objval)
#to prevent overstaffing on first shift, set new obj function to minimise
#X1 (using lpsolveAPI?)


#projects lpsolve
s.obj = c(23520,72912,12054,32340,70560,57232,79184,32340)
s.constr = matrix(c(80,248,41,10,240,195,269,110,
                    67,208,34,92,202,164,226,92),ncol = 8 ,byrow = TRUE)
s.dir = c('<=','<=')
s.rhs = c(1150,900)
s.sol = lp('max',s.obj,s.constr,s.dir,s.rhs,binary.vec = 1:8)
s.sol$objective
s.sol$solution
#projects lpsolveAPI
s.model = make.lp(0,8)
lp.control(s.model,sense = 'max')
set.objfn(s.model,c(23520,72912,12054,32340,70560,57232,79184,32340))
dec = c(80,248,41,10,240,195,269,110)
tes = c(67,208,34,92,202,164,226,92)
add.constraint(s.model,dec,'<=',1150)
add.constraint(s.model,tes,'<=',900)
set.type(s.model, col = 1:8, 'binary')
solve(s.model)
get.objective(s.model)
get.variables(s.model)


#destinations lpSovleAPI
c.model = make.lp(10,28) #10 constrains, 28 decision variables
lp.control(c.model,sense = 'min')
set.objfn(c.model,c(80,15,30,70,40,120,
                    60,85,35,10,20,60,
                    20,70,20,15,30,40,
                    40,30,22,30,26,100,
                    430,300,370,180))
#set LHS of constrain 1-4
#coef for the cols not in indices are set to 0
set.row(c.model,1,c(1,1,1,1,1,1,-40),indices = c(1:6,25))
set.row(c.model,2,c(1,1,1,1,1,1,-30),indices = c(7:12,26))
set.row(c.model,3,c(1,1,1,1,1,1,-50),indices = c(13:18,27))
set.row(c.model,4,c(1,1,1,1,1,1,-45),indices = c(19:24,28))
#set RHS of constraints 1-4
set.constr.value(c.model,rhs = c(0,0,0,0),constraints = 1:4)
#set type of constraints 1-4
set.constr.type(c.model,type = rep('<=',4),constraints = 1:4)
#set LHS of constraints 5-10
for (k in 1:6){
  set.row(c.model,k+4,c(1,1,1,1),indices = c(k,k+6,k+12,k+18))
}
#set RHS of constraints 5-10
set.constr.value(c.model,rhs = c(20,10,15,7,9,25),constraints = 5:10)
#set type of constraints 5-10
set.constr.type(c.model,type = rep('=',6),constraints = 5:10)
#set type of decision variables
set.type(c.model,columns = 1:24, 'integer')
set.type(c.model,columns = 25:28, 'binary')
solve(c.model)
get.variables(c.model)
get.objective(c.model)


### libraries ###
#####
library('xlsx')
library('readxl')
library('psych') #describe for prob distribution
library('spatialEco') #trend.line
library('forecast') #ma
library('TTR') #SMA
library('ggparallel') #ggparallel
library('MASS') #parcoord
library('GGally') #ggparcoord
library('car') #scatterplotmatrix
library('class') #k-NN
library('mc2d') #rtriag
library('lpSolve') #Linear optimisation
library('lpSolveAPI') #Linear optimisation
#####