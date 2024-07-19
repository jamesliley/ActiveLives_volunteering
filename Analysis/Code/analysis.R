#---------------------------------------------------#
#  Analysis of ActiveLives data ---------------------
#---------------------------------------------------#
##
## James Liley
## 28 Feb 2022
##
## Assumes that working directory contains directories
##  'Original' and 'R', where 'Original' contains
##  original SPSS .sav and MS Excel .xlsx files.
##

#---------------------------------------------------#
#  Setup --------------------------------------------
#---------------------------------------------------#

# Random seed
set.seed(48373)

# Set working directory if on James's machine
if (file.exists("~/ActiveLives/Data/")) setwd("~/ActiveLives/")

# Directories
dir_save="Data/R/"            # Directory with saved data
dir_code="ActiveLives/Code/"  # Directory with code
dir_object="R_objects"        # Directory with R objects

# Libraries
library(fst)          # Compressed format allowing fast read access
library(glmnet)       # Penalised generalised linear models
library(glmmLasso)    # Penalised linear mixed models
library(lme4)         # Linear mixed models
library(topicmodels)  # Topic models

# Scripts
source(paste0(dir_code,"functions.R"))

# Names of saved data
maintable_name="main"
lookup_name="lookup"

# Response variable
#yname="Volint_ANY"   # Volunteered in past year
yname="VOLMTH_POP"   # Volunteered in past month

# Suffix to save
if (yname=="Volint_ANY") save_suffix="year"
if (yname=="VOLMTH_POP") save_suffix="month"

# Force redo or not
force_redo=FALSE


#---------------------------------------------------#
#  Load previously saved data if it exists ----------
#---------------------------------------------------#

all_save_file=paste0(dir_object,"/mod_save_",save_suffix,".RData")
if (file.exists(all_save_file) & !force_redo) {
  load(all_save_file)
  force_redo=FALSE
}

#---------------------------------------------------#
#  Read reference data ------------------------------
#---------------------------------------------------#

# Read reference; first 20 rows of test data. Straightforward to just
#  reproduce original metadata with rbind (e.g. rbind(ref,dat)) if needed.
load(paste0(dir_save,"ref.RDS"))


#---------------------------------------------------#
#  Fit model to estimate coefficients ---------------
#---------------------------------------------------#

# Read training data
train_name=paste0(dir_save,maintable_name,"_train.fst")
dat=read_fst(train_name)
gc()

# Remove extraneous variables, filter >16, impute etc
xdat=filters(dat,
             f_var="Age16plus", # Filter >16yo
             c_var=c("wt_final",yname), # Filter non-NA weight and target
             r_var=c(setdiff(c(mgrep(c("vol","Vol","Filter","wt_","LA_"),colnames(dat)),"VOLFRQ_POP","VOLMTH_POP"),
                             c("wt_final",yname,"LA_2021"))), # Remove volunteering-related variables, filters, other weights, other area codes
             i_var=setdiff(colnames(dat),c("serial","wt_final",yname,"LA_2021")), # Mean-value impute everything
             p_var=c(), # Don't calculate principal components 
             o_var=c("serial","wt_final",yname,"LA_2021"), # Don't normalise these variables
             m_thresh=0.5, # Remove missing>50%
             sd_thresh=1e-5, # Remove SD<1e-5 post imputation
             mvec=NA, # Mean value impute
             onehot=TRUE, # Recode yes/no variables as one-hot
             normalise=TRUE, # Normalise to zero mean, unit SD
             addpca=0, # No principal components or topic features
             addtopics=0, # No principal components or topic features
             onehot_ref=ref, # Reference for whether variables are yes/no
             mu_t=NA, # Normalise
             sd_t=NA, # Normalise
             pca_transform=NA, # Compute PCS
             topic_transformer=topic_transformer_ActiveLives, # Function to transform to topic matrix
             topic_model=NA) # Fit new topic model
dat1=xdat$dat
xdat$dat=NULL
rm(dat)
gc()

# Prepare for GLM
x_rm=match(c("wt_final",yname,"serial","LA_2021"),colnames(dat1))
X=as.matrix(dat1[,-x_rm]);
Y=dat1[[yname]] # target
W=dat1$wt_final # weights
w_in=which(!is.na(W+Y)); 
X=X[w_in,]; Y=Y[w_in]; W=W[w_in]; 

# Model to determine variables for which coefficients will be estimated:
#  L1 penalised regression fitted to training dataset
if (force_redo | !exists("modc")) modc=cv.glmnet(X,Y,weights=W,trace.it=TRUE,type.measure="auc",family=binomial(link="logit"))
n_trainc=dim(X)[1]; p_trainc=dim(X)[2]

# Which variables have nonzero coefficients?
bmin=modc$glmnet.fit$beta[,which.min(abs(modc$lambda-modc$lambda.min))]
mnames=names(bmin)[which(bmin!=0)]

# Clean up
rm(list=c("X","Y","W","dat1"))
gc()


#---------------------------------------------------#
#  Estimate coefficients ----------------------------
#---------------------------------------------------#

# Read validation and test data
val_name=paste0(dir_save,maintable_name,"_val.fst")
val0=read_fst(val_name)

# Read in validation set
test_name=paste0(dir_save,maintable_name,"_test.fst")
test0=read_fst(test_name)

# Combined test and validation set
tv=rbind(val0,test0)
rm(val0); rm(test0)
gc()

# Filter/process
tv1=filters(tv,
            f_var="Age16plus", # Filter >16yo
            c_var=c("wt_final",yname), # Filter non-NA weight and target
            r_var=xdat$x_rm,
            i_var=setdiff(colnames(tv),c("serial","wt_final",yname)), # Mean-value impute everything
            p_var=setdiff(colnames(tv),c("serial","wt_final",yname)), # Principal components calculated for all variables except weights and target
            o_var=c("serial","wt_final",yname),
            m_thresh=NA,
            sd_thresh=NA,
            mvec=xdat$mvec,
            onehot=TRUE,
            normalise=TRUE,
            addpca=0,
            addtopics=0,
            onehot_ref=ref,
            mu_t=xdat$mu_t,
            sd_t=xdat$sd_t,
            pca_transform=NA, #xdat$PRM,
            topic_transformer=topic_transformer_ActiveLives, 
            topic_model=NA) #xdat$topic_model)
gc()

# Check (should be FALSE)
tv1$train_flag


# Prepare to fit model
X1=as.matrix(tv1$dat[,mnames]); 
Y1=tv1$dat[[yname]] # target
W1=tv1$dat$wt_final # weights
w_in=which(!is.na(W1+Y1)); 
X1=X1[w_in,]; Y1=Y1[w_in]; W1=W1[w_in];

# Fit GLM to estimate coefficients
modc2=glm(Y1~.,data=data.frame(cbind(X1,Y1)),weights=W1,family=binomial(link="logit"))
n_testc=dim(X1)[1]; 

# Coefficient, SE, p-val table
cc=as.data.frame(summary(modc2)$coefficients)
colnames(cc)=c("Coefficient","SE","Z","P")
cc=cc[order(-abs(cc$Coefficient/cc$SE)),]

# Add description
desc=rep("",dim(cc)[1])
for (i in 1:dim(cc)[1]) {
  nm=rownames(cc)[i]
  if (nm %in% colnames(ref)) desc[i]=attributes(ref[[nm]])$label else
    desc[i]="-"
}
cc$description=desc

# Coefficients reaching Sidak-corrected significance threshold
cc$significant=(cc$P< 1-(1-0.05)^(1/dim(cc)[1]))

# Reorder
cc=cc[,c("description","Coefficient","SE","Z","P","significant")]

# Write to file
write.csv(format(cc,digits=3),paste0("Output/coefficients_",save_suffix,".csv"),row.names=T,quote=T)



#---------------------------------------------------#
#  Close analysis of coefficients -------------------
#---------------------------------------------------#

sink(paste0("Output/coefficients_",save_suffix,"_detail.txt"))
cat("Significant coefficients in logistic regression to predict ",yname,": \n\n")
Y=tv[[yname]]; mY=mean(Y,na.rm=T)
vsub=rownames(cc)[which(cc$significant)]
for (i in 1:length(vsub)) {
  if (vsub[i] %in% colnames(ref)) {
    vx=attributes(ref[[vsub[i]]])$labels; vx=vx[which(vx>0)]
    XX=tv[[vsub[i]]]; XX[which(XX<0)]=NA
    tx=table(Y,XX,useNA="ifany"); txr=tx[2,]/(tx[1,]+tx[2,])
    xn=names(vx)[match(colnames(tx),vx)]
    if (all(is.na(xn))) xn=colnames(tx)
    if ("0" %in% setdiff(colnames(tx),names(vx))) xn[which(colnames(tx)==0)]="No"
    if (any(is.na(colnames(tx)))) xn[which(is.na(colnames(tx)))]="Missing"
    w1=which(txr>= mY); nmore=xn[w1]; xmore=signif(100*txr[w1],digits=3)
    w2=which(txr< mY); nless=xn[w2]; xless=signif(100*txr[w2],digits=3)
    cmore=paste0(nmore," (",xmore,"%)"); 
    cless=paste0(nless," (",xless,"%)")
    cat(paste0("Variable ",vsub[i]," (",attributes(ref[[vsub[i]]])$label,"):\n"))
    cat(paste0("More Y than average: ",paste(cmore,collapse=", "),"\n"))
    cat(paste0("Less Y than average: ",paste(cless,collapse=", "),"\n"))
    cat("\n") 
  }
}
sink()

# Clean up
rm(list=c("X1","Y1","W1","tv1","tv"))


#---------------------------------------------------#
#  Fit model to evaluate predictions ----------------
#---------------------------------------------------#

# Read training data
train_name=paste0(dir_save,maintable_name,"_train.fst")
train0=read_fst(train_name)

# Read validation data
val_name=paste0(dir_save,maintable_name,"_val.fst")
val0=read_fst(val_name)

# Combine
dat=rbind(train0,val0)
rm(train0); rm(val0); gc()

# Remove extraneous variables, filter >16, impute etc
pdat=filters(dat,
             f_var="Age16plus", # Filter >16yo
             c_var=c("wt_final",yname), # Filter non-NA weight and target
             r_var=c(setdiff(c(mgrep(c("vol","Vol","Filter","wt_","LA_"),colnames(dat)),"VOLFRQ_POP","VOLMTH_POP"),
                             c("wt_final",yname,"LA_2021"))), # Remove volunteering-related variables, filters, other weights, other area codes
             i_var=setdiff(colnames(dat),c("serial","wt_final",yname,"LA_2021")), # Mean-value impute everything
             p_var=c(), # Don't calculate principal components 
             o_var=c("serial","wt_final",yname,"LA_2021"), # Don't normalise these variables
             m_thresh=0.5, # Remove missing>50%
             sd_thresh=1e-5, # Remove SD<1e-5 post imputation
             mvec=NA, # Mean value impute
             onehot=TRUE, # Recode yes/no variables as one-hot
             normalise=TRUE, # Normalise to zero mean, unit SD
             addpca=0, # No principal components or topic features
             addtopics=0, # No principal components or topic features
             onehot_ref=ref, # Reference for whether variables are yes/no
             mu_t=NA, # Normalise
             sd_t=NA, # Normalise
             pca_transform=NA, # Compute PCS
             topic_transformer=topic_transformer_ActiveLives, # Function to transform to topic matrix
             topic_model=NA) # Fit new topic model
dat1=pdat$dat
pdat$dat=NULL
rm(dat)
gc()

# Prepare for GLM
x_rm=match(c("wt_final",yname,"serial","LA_2021"),colnames(dat1))
X=as.matrix(dat1[,-x_rm]);
Y=dat1[[yname]] # target
W=dat1$wt_final # weights
w_in=which(!is.na(W+Y)); 
X=X[w_in,]; Y=Y[w_in]; W=W[w_in]; 

# Model to determine variables for which coefficients will be estimated:
#  L1 penalised regression fitted to training dataset
if (force_redo | !exists("modp")) modp=cv.glmnet(X,Y,weights=W,trace.it=TRUE,type.measure="auc",family=binomial(link="logit"))
cXp=colnames(X)
n_trainp=dim(X)[1]; p_trainp=dim(X)[2]


# Clean up
rm(list=c("X","Y","W","dat1"))
gc()


#---------------------------------------------------#
#  Evaluate predictions -----------------------------
#---------------------------------------------------#

# Read test data
test_name=paste0(dir_save,maintable_name,"_test.fst")
test=read_fst(test_name)

# Filter/process
test1=filters(test,
            f_var="Age16plus", # Filter >16yo
            c_var=c("wt_final",yname), # Filter non-NA weight and target
            r_var=pdat$x_rm,
            i_var=setdiff(colnames(test),c("serial","wt_final",yname,"LA_2021")), # Mean-value impute everything
            p_var=setdiff(colnames(test),c("serial","wt_final",yname,"LA_2021")), # Principal components calculated for all variables except weights and target
            o_var=c("serial","wt_final",yname,"LA_2021"),
            m_thresh=NA,
            sd_thresh=NA,
            mvec=pdat$mvec,
            onehot=TRUE,
            normalise=TRUE,
            addpca=0,
            addtopics=0,
            onehot_ref=ref,
            mu_t=pdat$mu_t,
            sd_t=pdat$sd_t,
            pca_transform=NA, #xdat$PRM,
            topic_transformer=topic_transformer_ActiveLives, 
            topic_model=NA) #xdat$topic_model)
rm(test); gc()

# Check (should be FALSE)
test1$train_flag

# Get Y and weights
testY=test1$dat[[yname]]
testwt=test1$dat$wt_final
testloc=test1$dat$LA_2021

# Restrict to correct column names and weights
w1=which(!is.na(testY + testwt))
Xt=test1$dat[w1,cXp]; test1$dat=NULL
testY=testY[w1]; testwt=testwt[w1]; testloc=testloc[w1]

# Predictive analytics (on validation set)
ypred=predict(modp,as.matrix(Xt),lambda=modp$lambda.min,type="response")
n_testp=dim(Xt)[1]; 

# ROC and PRC curve
roc=getroc(testY,ypred)
prc=getprc(testY,ypred)

# Calibration
cal=plotcal(testY,ypred)

# Frequency of estimates
yfreq=table(cut(ypred,breaks = seq(0,1,length=11)))
xf=0.05 + seq(0,0.9,length=10)

# Plot
xplot=function() {
  par(mar=c(5.1,4.1,4.1,4.1))
  par(mfrow=c(1,3))
  plot(roc)
  plot(prc)

  plot(0:1,xlim=0:1,ylim=0:1,type="n",xlab="Predicted",ylab="True")
  polygon(c(cal$x,rev(cal$x)),c(cal$lower,rev(cal$upper)),border=NA,col="gray")
  lines(cal)
  
  # Add frequencies
  ap=pretty(0:max(yfreq))
  points(xf,yfreq/max(ap),pch=16)
  axis(4,at=seq(0,1,length=length(ap)),labels=ap)
  mtext("Frequency",4,line=3,cex=0.5)
  
  abline(0,1,col="red")
}

# Save as PDF and PNG, and print
pdf(paste0("Output/predictive_analysis_",save_suffix,".pdf"),width=9,height=3)
xplot()
dev.off()
png(paste0("Output/predictive_analysis_",save_suffix,".png"),width=900,height=300)
xplot()
dev.off()
xplot()


# Calibration by local area
xla=attributes(ref$LA_2021)$labels
ctab=data.frame(
  la=xla,
  true_y_freq=rep(NA,length(xla)),
  pred_y_freq=rep(NA,length(xla)),
  n_vol=rep(NA,length(xla)),
  total=rep(0,length(xla)),
  pval=rep(1,length(xla))
)
for (i in 1:length(xla)) {
  w=which(testloc==i)
  ctab$true_y_freq[i]=mean(testY[w])
  ctab$pred_y_freq[i]=mean(ypred[w])
  ctab$n_vol[i]=sum(testY[w])
  ctab$total[i]=length(w)
  if (length(w)>0) {
    testp=binom.test(sum(testY[w]),length(w),p = mean(ypred[w]),alternative = "two.sided")
    ctab$pval[i]=testp$p.value
    ctab$ci95_lower[i]=testp$conf.int[1]
    ctab$ci95_upper[i]=testp$conf.int[2]
  } else {
    ctab$pval[i]=1
    ctab$ci95_lower[i]=0
    ctab$ci95_upper[i]=1
  }
}

# Write table
write.csv(format(ctab,digits=3),paste0("Output/la_calibration_",save_suffix,".csv"),row.names=T,quote=T)

# Clean up
rm(list=c("Xt")); gc()


#---------------------------------------------------#
#  Final model --------------------------------------
#---------------------------------------------------#

# Read all
all_name=paste0(dir_save,maintable_name,".fst")
dat=read_fst(all_name)
y_all=dat[[yname]]
la_all=dat[["LA_2021"]]
s_all=dat[['serial']]
w_all=dat[['wt_final']]
a16_all=dat[['Age16plus']]

# Data with missing yname
msub=which(is.na(y_all))
imp=dat[msub,]


# Remove extraneous variables, filter >16, impute etc
fdat=filters(dat,
             f_var="Age16plus", # Filter >16yo
             c_var=c("wt_final",yname), # Filter non-NA weight and target
             r_var=c(setdiff(c(mgrep(c("vol","Vol","Filter","wt_","LA_"),colnames(dat)),"VOLFRQ_POP","VOLMTH_POP"),
                             c("wt_final",yname,"LA_2021"))), # Remove volunteering-related variables, filters, other weights, other area codes
             i_var=setdiff(colnames(dat),c("serial","wt_final",yname,"LA_2021")), # Mean-value impute everything
             p_var=c(), # Don't calculate principal components 
             o_var=c("serial","wt_final",yname,"LA_2021"), # Don't normalise these variables
             m_thresh=0.5, # Remove missing>50%
             sd_thresh=1e-5, # Remove SD<1e-5 post imputation
             mvec=NA, # Mean value impute
             onehot=TRUE, # Recode yes/no variables as one-hot
             normalise=TRUE, # Normalise to zero mean, unit SD
             addpca=0, # No principal components or topic features
             addtopics=0, # No principal components or topic features
             onehot_ref=ref, # Reference for whether variables are yes/no
             mu_t=NA, # Normalise
             sd_t=NA, # Normalise
             pca_transform=NA, # Compute PCS
             topic_transformer=topic_transformer_ActiveLives, # Function to transform to topic matrix
             topic_model=NA) # Fit new topic model
dat1=fdat$dat
fdat$dat=NULL
rm(dat)
gc()

# Prepare for GLM
x_rm=match(c("wt_final",yname,"serial","LA_2021"),colnames(dat1))
X=as.matrix(dat1[,-x_rm]);
Y=dat1[[yname]] # target
W=dat1$wt_final # weights
w_in=which(!is.na(W+Y)); 
X=X[w_in,]; Y=Y[w_in]; W=W[w_in]; 

# Final model for imputation
#  L1 penalised regression fitted to whole dataset
if (force_redo | !exists("modf")) modf=cv.glmnet(X,Y,weights=W,trace.it=TRUE,type.measure="auc",family=binomial(link="logit"))
cXf=colnames(X)
n_trainf=dim(X)[1]; p_trainf=dim(X)[2]

# Clean up
rm(list=c("X","Y","W"))


#---------------------------------------------------#
#  Impute -------------------------------------------
#---------------------------------------------------#

# Filter/process
imp1=filters(imp,
              f_var="Age16plus", # Filter >16yo
              c_var=c(), #c("wt_final",yname), # Don't filter non-NA weight and target
              r_var=fdat$x_rm,
              i_var=setdiff(colnames(imp),c("serial","wt_final",yname)), # Mean-value impute everything
              p_var=setdiff(colnames(imp),c("serial","wt_final",yname)), # Principal components calculated for all variables except weights and target
              o_var=c("serial","wt_final",yname),
              m_thresh=NA,
              sd_thresh=NA,
              mvec=fdat$mvec,
              onehot=TRUE,
              normalise=TRUE,
              addpca=0,
              addtopics=0,
              onehot_ref=ref,
              mu_t=fdat$mu_t,
              sd_t=fdat$sd_t,
              pca_transform=NA, #xdat$PRM,
              topic_transformer=topic_transformer_ActiveLives, 
              topic_model=NA) #xdat$topic_model)
rm(imp); gc()

# Check (should be FALSE)
imp1$train_flag

# Matrix to predict on
sYi=imp1$dat$serial
Xi=as.matrix(imp1$dat[,cXf]);
imp1$dat=NULL

# Imputation
Yi=predict(modf,as.matrix(Xi),lambda=modf$lambda.min,type="response")
n_testf=dim(Xi)[1]; 

# Compute imputed values
isn=is.na(y_all)
m_s=match(sYi,s_all)
y_all[m_s]=Yi

# Final output
final0=data.frame(serial=s_all,vol=y_all,la=la_all,wt=w_all,imputed=isn)
f_rm=which(is.na(final0$wt+final0$serial) | (a16_all==0)) # to remove

# Clean up
rm(list=c("Xi","Yi"))
gc()



#---------------------------------------------------#
#  Write table --------------------------------------
#---------------------------------------------------#

xla=attributes(ref$LA_2021)$labels
final=final0[-f_rm,]

tab=data.frame(Code=xla,
               Yes=rep(NA,length(xla)),
               No=rep(NA,length(xla)),
               Unav=rep(NA,length(xla)),
               Raw_pct_yes=rep(NA,length(xla)),
               Raw_pct_yes_se=rep(NA,length(xla)),
               Raw_wt_pct_yes=rep(NA,length(xla)),
               Raw_wt_pct_yes_se=rep(NA,length(xla)),
               Raw_pct_na=rep(NA,length(xla)),
               Raw_pct_na_se=rep(NA,length(xla)),
               Raw_wt_pct_na=rep(NA,length(xla)),
               Raw_wt_pct_na_se=rep(NA,length(xla)),
               Imp_pct_yes=rep(NA,length(xla)),
               Imp_pct_yes_se=rep(NA,length(xla)),
               Imp_wt_pct_yes=rep(NA,length(xla)),
               Imp_wt_pct_yes_se=rep(NA,length(xla)),
               Final_pct_yes=rep(NA,length(xla)),
               Final_pct_yes_se=rep(NA,length(xla)),
               Final_wt_pct_yes=rep(NA,length(xla)),
               Final_wt_pct_yes_se=rep(NA,length(xla)),
               Total=rep(NA,length(xla)))

for (i in 1:length(xla)) {
  # Volunteering status and weight for imputed/nonimputed/all in each area
  w=which(final$la==xla[i])
  isn=final$imputed[w] # Imputed indicator
  w_imp=which(isn) # Which samples are imputed
  w_raw=setdiff(1:length(w),w_imp) # Which samples are not imputed

  dat_all=final$vol[w]
  wt_all=final$wt[w]
  dat_raw=dat_all[w_raw]
  wt_raw=wt_all[w_raw]
  dat_imp=dat_all[w_imp]
  wt_imp=wt_all[w_imp]
  
  # Raw yes/no/unavailable answers
  tab$Yes[i]=length(which(dat_raw==1))
  tab$No[i]=length(which(dat_raw==0))
  tab$Unav[i]=length(w_imp)
  
  # Raw percentage of Yes answers
  lfd0=length(dat_raw)
  ry=length(which(dat_raw==1))/lfd0
  tab$Raw_pct_yes[i]=100*ry
  tab$Raw_pct_yes_se[i]=100*sqrt(ry*(1-ry)/lfd0)

  # Raw percentage of NA answers
  ld0=length(dat_all)
  rn=length(dat_imp)/ld0
  tab$Raw_pct_na[i]=100*rn
  tab$Raw_pct_na_se[i]=100*sqrt(rn*(1-rn)/ld0)
  
  # Weighted percentage of Yes answers
  wsub=which(is.finite(wt_raw))
  tab$Raw_wt_pct_yes[i]=100*weighted.mean(dat_raw[wsub],w=wt_raw[wsub],na.rm=T)
  tab$Raw_wt_pct_yes_se[i]=100*b_se_wt(dat_raw[wsub],wt_raw[wsub]) # bootstrap
  
  # Weighted percentage of NA answers
  wsub=which(is.finite(wt_all))
  isn=dat_all[isn]
  tab$Raw_wt_pct_na[i]=100*weighted.mean(isn[wsub],w=wt_all[wsub],na.rm=T)
  tab$Raw_wt_pct_na_se[i]=100*b_se_wt(isn[wsub],w=wt_all[wsub])
  
  # Average imputed 'yes' answers, unweighted
  ni=length(dat_imp)
  tab$Imp_pct_yes[i]=100*mean(dat_imp)
  tab$Imp_pct_yes_se[i]=100*sd(dat_imp)/sqrt(ni)
  
  # Average imputed 'yes' answers, weighted
  wsub=which(is.finite(wt_imp))
  tab$Imp_wt_pct_yes[i]=100*weighted.mean(x=dat_imp[wsub],w=wt_imp[wsub])
  tab$Imp_wt_pct_yes_se[i]=100*b_se_wt(dat_imp[wsub],wt_imp[wsub])

  # Average total 'yes' answers, unweighted
  nx=length(dat_all)
  tab$Final_pct_yes[i]=100*mean(dat_all)
  tab$Final_pct_yes_se[i]=100*sd(dat_all)/sqrt(nx)

  # Average total 'yes' answers, weighted
  wsub=which(is.finite(wt_all))
  tab$Final_wt_pct_yes[i]=100*weighted.mean(x=dat_all[wsub],w=wt_all[wsub])
  tab$Final_wt_pct_yes_se[i]=100*b_se_wt(dat_all[wsub],wt_all[wsub])
  
  tab$Total[i]=length(dat_all)
}

write.csv(format(tab,digits=3),paste0("Output/tab_",save_suffix,".csv"),row.names=T,quote=T)


#---------------------------------------------------#
#  Print model details ------------------------------
#---------------------------------------------------#

## Number of training samples, variables, and lambda values in each LASSO model
tab_model=c()
for (suff in c("c","p","f")) {
  mod=get(paste0("mod",suff))
  nobs=mod$glmnet.fit$nobs
  lambda=mod$lambda.min
  beta=mod$glmnet.fit$beta[,which.min(abs(mod$lambda-mod$lambda.min))]
  nvar_total=dim(mod$glmnet.fit$beta)[1]
  nvar=length(which(abs(beta)>0))
  tab_model=rbind(tab_model,c(nobs,lambda,nvar_total,nvar))
}
tab_model=data.frame(tab_model)
rownames(tab_model)=c("Coef_estimation","Predictive_analysis","Final")
colnames(tab_model)=c("N_training_observations","L1_hyperparameter","N_predictors","N_included_predictors")

write.csv(format(tab_model,digits=3),paste0("Output/model_details_",save_suffix,".csv"),row.names=T,quote=T)




#---------------------------------------------------#
#  Save image ---------------------------------------
#---------------------------------------------------#

save.image(file=paste0(dir_object,"/mod_save_",save_suffix,".RData"))
