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
if (file.exists("~/Research/ActiveLives/Data/")) setwd("~/Research/ActiveLives/")

# Directories
dir_save="../Analysis/Data/R/"            # Directory with saved data
dir_code="../Analysis/ActiveLives/Code/"  # Directory with code
dir_object="../Analysis/R_objects"        # Directory with R objects

# Libraries
library(fst)          # Compressed format allowing fast read access
library(glmnet)       # Penalised generalised linear models
library(glmmLasso)    # Penalised linear mixed models
library(lme4)         # Linear mixed models
library(topicmodels)  # Topic models
library(rstanarm)     # Bayesian linear models

# Scripts
source(paste0(dir_code,"functions.R"))

# Names of saved data
maintable_name="main"
lookup_name="lookup"

# Response variable
yname="Volint_ANY"   # Volunteered in past year
#yname="VOLMTH_POP"   # Volunteered in past month

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
#  Estimate importance of variables by correlation ----
#---------------------------------------------------#

# Read training and testing data and combine
train_name=paste0(dir_save,maintable_name,"_train.fst")
train_name=paste0(dir_save,maintable_name,"_test.fst")
dat_train=read_fst(train_name)
dat_test=read_fst(test_name)
dat=rbind(dat_train,dat_test)
rm(list=c("dat_train","dat_test"))
gc()

# Remove extraneous variables, filter >16, impute etc
xdat=filters(dat,
             f_var="Age16plus", # Filter >16yo
             c_var=c("LA_2021"), # Filter non-NA weight and target
             r_var=c(setdiff(c(mgrep(c("vol","Vol","Filter","wt_","LA_"),colnames(dat)),"VOLFRQ_POP","VOLMTH_POP"),
                             c("wt_final",yname,"LA_2021"))), # Remove volunteering-related variables, filters, other weights, other area codes
             i_var=c(), #setdiff(colnames(dat),c("serial","wt_final",yname,"LA_2021")), # Mean-value impute everything
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

# Restrict to only major variables
vcsv=paste0("../Analysis/Output/coefficients_",save_suffix,".csv")
xcoef=read.csv(vcsv)
dat1=dat1[,c("serial","LA_2021",intersect(xcoef[,1],colnames(dat1)))]
dat1$vol=final$vol[match(dat1$serial,final$serial)]

# Evaluate correlation in each local area
las=unique(dat1$LA_2021)

out=data.frame()
vars=setdiff(colnames(dat1),c("LA_2021","vol","serial"))
for (i in 1:length(las)) {
  w=which(dat1$LA_2021==las[i])
  vec=c()
  x=dat1$vol[w]
  for (j in 1:length(vars)) {
    y=dat1[[vars[j]]][w]
    if (length(which(is.finite(y)))>10)
      vec[j]=cor(x,y,use="complete.obs",method="spearman")
  }
  out=rbind(out,vec)
}
colnames(out)=vars

# Quick look
cm=colMeans(out,na.rm=T)
cs=colSds(as.matrix(out),na.rm=T)

save(out,file=paste0("Generate/data/out_cor_",save_suffix,".RData"))

#plot(cm,pch=16,cex=0.5)
#segments(1:length(cm),cm-2*cs,1:length(cm),cm+2*cs,col="red")




#---------------------------------------------------#
#  Estimate importance of variables by LMM-then-Bayes ----
#---------------------------------------------------#

# Read training and testing data and combine
train_name=paste0(dir_save,maintable_name,"_train.fst")
train_name=paste0(dir_save,maintable_name,"_test.fst")
dat_train=read_fst(train_name)
dat_test=read_fst(test_name)
dat=rbind(dat_train,dat_test)
rm(list=c("dat_train","dat_test"))
gc()

# Remove extraneous variables, filter >16, impute etc
dat0=dat; for (i in 1:dim(dat)[2]) dat0[,i]=is.na(dat[,i]); dat0$serial=dat$serial
xdat=filters(dat,
             f_var="Age16plus", # Filter >16yo
             c_var=c("LA_2021"), # Filter non-NA weight and target
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

# Restrict to only major variables
vcsv=paste0("Output/coefficients_",save_suffix,".csv")
xcoef=read.csv(vcsv)
dat1=dat1[,c("serial","LA_2021",intersect(xcoef[,1],colnames(dat1)))]
dat1$vol=final$vol[match(dat1$serial,final$serial)]
dat1$volY=rbinom(length(dat1$vol),1,prob=dat1$vol) # Probabilistically reconstruct volunteering rate

# For each variable, fit random effects model
#  Quick simulation to check
#   nn=100000; np=round(sqrt(nn))
# dx=data.frame(X=rnorm(nn),FF=sample(1:np,nn,rep=T))
# c0=2; cs=1.3; rsd=0.5; ix=3; #gp=rnorm(np,sd=cs)
# dx$Y=(c0 + gp[dx$FF])*dx$X + rnorm(nn,sd=rsd) + ix
# g2=lmer(Y~X + (X|FF),dat=dx)

# Put vars in a useful order and only look at first 50
vars=setdiff(colnames(dat1),c("LA_2021","vol","serial","volY"))
varsub=c("CULFRQ_1_9_POP", "Child4", "CULMTH_1_9_POP", 
         "motivb_POP", "Age1640", "motivd_POP", "InDevTry_GR", 
         "AgeTGC", "UrbRur6", "UrbRur6_LA", "Age2_46", 
         "WorkStat5", "UrbRur2", "Eth2", "mode")
var1=setdiff(vars,varsub)
dvar=rep(NA,length(var1))
for (i in 1:length(var1)) {
  x=dat1[[var1[i]]]
  if (length(unique(x))<= 3) {
    dvar[i]=min(length(which(x==min(x))),length(which(x==max(x))))
  } else if (length(unique(x)) <= 10) {
    dvar[i]=-var(as.numeric(as.factor(x)))
  } else {
    dvar[i]=-var(x)
  }
}
var2=var1[order(-dvar)[1:30]]
vars=c(varsub,var2)

d0=dat0[match(dat1$serial,dat0$serial),]
out=matrix(NA,length(vars),length(las))
for (i in 1:length(vars)) {
  fmiss=length(which(d0[[vars[i]]]==1))/dim(d0)[1]
  sub=which(!is.na(dat1[[vars[i]]] + dat1$LA_2021 + dat1$volY))
  if (fmiss<0.2 & length(sub)>10) {
    fx=paste0("volY~",vars[[i]], "+ (",vars[[i]],"|LA_2021)")
    gx=glmer(as.formula(fx),data=dat1[sub,],family=binomial(link="logit"))
    mu=summary(gx)$coefficients[2,1]
    sig=sqrt(summary(gx)$varcor[[1]][2,2])
    for (j in 1:length(las)) {
      w=which(dat1$LA_2021==las[j])
      x=dat1[[vars[i]]][w]
      if ((var(x) > 0.1)) {
        log<-submod<- tryCatch(stan_glm(as.formula(paste0("volY~",vars[i])),data=dat1[w,],
                                            prior=normal(mu,sig,autoscale=FALSE)),error=function(e) NA)
        if (length(submod)>1) out[i,j]=submod$coefficients[2]
        save(out,file=paste0("Generate/Data/out_lmm_temp_",save_suffix,".RData"))
        print(c(i,j))
      }
    }
  }
}

rownames(out)=vars
ax=attributes(ref$LA_2021)$labels
colnames(out)=names(ax)[match(las,ax)]
out=as.data.frame(out)
save(out,file=paste0("Generate/Data/out_lmm_",save_suffix,".RData"))

save.image(file=paste0("Generate/Data/all_lmm_data_",save_suffix,".RData"))

