# auxiliary function to list column names containing any of strings
mgrep=function(l,s) {
  out=c()
  for (i in 1:length(l)) out=c(out,grep(l[i],s,val=TRUE))
  return(unique(out))
}


#' @name filters
#' @description Define as a function which can be applied to any of the three dataset
#'  partitions (train, test, validation)
#'  In order: filters on variables f_var being equal to 1 and c_var being non-NA, removes 
#'   variables r_var and variables with missingness > m_thresh, if 'onehot=TRUE' converts 
#'   yes/no variables to one- hot, imputes variables i_var using either a set vector or 
#'   mean-value imputation, removes variables with SD<s_thresh (after imputation), 
#'   normalises variables or applies a normalising transformation, generates 
#'   principal components or applies a given transform to do so, and generates 
#'   closeness to topic features according to a newly-fitted or given topic model.
#'  Returns a list: the transformed dataset, all removed variables, impute fill values
#'   for all imputed variables, principal component transform matrix, topic model, 
#'   normalising means and SDs, and a flag indicating whether any properties of the 
#'   dataset have been used in processing it (which should be 0 for any test/validation 
#'   matrix).
#'  Default parameters will lead to nothing being done to the dataset.
#' @param dat starting dataset
#' @param f_var vector of variable names; filter on these variables being equal to 1
#' @param c_var vector of variable names; filter on these variables being finite
#' @param r_var vector of variable names; remove these variables
#' @param i_var vector of variable names; either impute to values in mvec, or mean-value impute if mvec[i] is NA
#' @param p_var vector of variable names; compute principal components over these variables
#' @param o_var vector of variable names; do not normalise these
#' @param m_thresh real number 0-1 or NA: remove variables with missingness >= this if non-NA
#' @param sd_thresh real number >=0 or NA: remove variables with SD < this after imputation if non-NA
#' @param onehot if TRUE, converts variables to one-hot if they are yes/no. Uses attributes of object 'onehot_ref' to determine this.
#' @param normalise if TRUE, normalises variables to mean 0, standard deviation 1, or transforms by subtracting mu_t and dividing by sd_t.
#' @param addpca if nonzero, adds this many PCAs to matrix. If pca_transform is non-null, uses this matrix to produce PCAs, otherwise fits to data.
#' @param addtopics if nonzero, adds closeness to this many topic features to matrix. If topicmodel is non-null, uses this topic model to produce topic model, otherwise fits to data, using topic_transformer function first if specified.
#' @param onehot_ref a list with names corresponding to column names of dat, for which 'attributes(onehot_ref[[i]])$labels' is a vector with names corresponding to the meanings of the variable values.
#' @param mvec vector of real numbers with names including i_var or NA: impute to these values or mean-value impute. If a vector can be part-real, part-NA.
#' @param mu_t a real vector of means to subtract, or NULL to normalise to 0 mean
#' @param sd_t a real vector of SDs to divide out, or NULL to normalise to 1 SD
#' @param pca_transform if addpca=k, either a matrix of dimension Qxk or NULL where Q is the dat matrix at this stage of processing. If a matrix, multiplies the processed dat matrix by pca_transform, otherwise generates one using PCA.
#' @param topic_transformer if addtopics=k>0, a function taking a data frame and returning one to which a topic model can be fitted.
#' @param topic_model if addtopics=k>0, a topic model with k topics from which features will be generated.
#' @param verbose print progress or not; defaults to TRUE
#' @return a list with elements 
#'   dat: the transformed data matrix, 
#'   x_rm: all removed variables, 
#'   mvec: impute fill values for all imputed variables, 
#'   mu_t: normalising means,
#'   sd_t: normalising SDs, 
#'   PRM: principal component transform matrix, 
#'   topic_model: the topic model fitted,
#'   newref: new object like onehot_ref with correct attributes
#'   train_flag: a flag which is TRUE if any properties of the dataset have been have been used in processing it (should be FALSE for any test/validation matrix).
#' @examples

#'   dat=read_fst(train_name)
#'   dat0=dat[1:10000,]
#'   
#'   # Test datasets: identical first row, different otherwise
#'   dat1=dat[10001:15000,]
#'   dat2=dat[c(10001,15002:20000),]
#'   rm(dat)
#'   
#'   dat=dat0[1:10,]
#'   dat0a=filters(dat0,
#'                 f_var="Age16plus", 
#'                 c_var=c("wt_final","VOLMTH_POP"),
#'                 r_var=c(setdiff(mgrep(c("vol","Vol","Filter","wt_"),colnames(dat)),
#'                                 c("wt_final","VOLMTH_POP")),
#'                         "VOLFRQ_POP"),
#'                 i_var=setdiff(colnames(dat),c("serial","wt_final","VOLMTH_POP")),
#'                 p_var=setdiff(colnames(dat),c("serial","wt_final","VOLMTH_POP")),
#'                 o_var=c("serial","wt_final","VOLMTH_POP"),
#'                 m_thresh=0.5,
#'                 sd_thresh=1e-5,
#'                 mvec=NA,
#'                 onehot=TRUE,
#'                 normalise=TRUE,
#'                 addpca=10,
#'                 addtopics=50,
#'                 onehot_ref=ref,
#'                 mu_t=NA,
#'                 sd_t=NA,
#'                 pca_transform=NA,
#'                 topic_transformer=topic_transformer_ActiveLives,
#'                 topic_model=NA)
#'   
#'   
#'   dat1a=filters(dat1,
#'                 f_var="Age16plus", 
#'                 c_var=c("wt_final","VOLMTH_POP"),
#'                 r_var=dat0a$x_rm,
#'                 i_var=setdiff(colnames(dat),c("serial","wt_final","VOLMTH_POP")),
#'                 p_var=setdiff(colnames(dat),c("serial","wt_final","VOLMTH_POP")),
#'                 o_var=c("serial","wt_final","VOLMTH_POP"),
#'                 m_thresh=NA,
#'                 sd_thresh=NA,
#'                 mvec=dat0a$mvec,
#'                 onehot=TRUE,
#'                 normalise=TRUE,
#'                 addpca=10,
#'                 addtopics=50,
#'                 onehot_ref=ref,
#'                 mu_t=dat0a$mu_t,
#'                 sd_t=dat0a$sd_t,
#'                 pca_transform=dat0a$PRM,
#'                 topic_transformer=topic_transformer_ActiveLives,
#'                 topic_model=dat0a$topic_model)
#'   
#'   dat2a=filters(dat2,
#'                 f_var="Age16plus", 
#'                 c_var=c("wt_final","VOLMTH_POP"),
#'                 r_var=dat0a$x_rm,
#'                 i_var=setdiff(colnames(dat),c("serial","wt_final","VOLMTH_POP")),
#'                 p_var=setdiff(colnames(dat),c("serial","wt_final","VOLMTH_POP")),
#'                 o_var=c("serial","wt_final","VOLMTH_POP"),
#'                 m_thresh=NA,
#'                 sd_thresh=NA,
#'                 mvec=dat0a$mvec,
#'                 onehot=TRUE,
#'                 normalise=TRUE,
#'                 addpca=10,
#'                 addtopics=50,
#'                 onehot_ref=ref,
#'                 mu_t=dat0a$mu_t,
#'                 sd_t=dat0a$sd_t,
#'                 pca_transform=dat0a$PRM,
#'                 topic_transformer=topic_transformer_ActiveLives,
#'                 topic_model=dat0a$topic_model)
#'   
#'   dat0a$train_flag
#'   dat1a$train_flag
#'   dat2a$train_flag
#'   
#'   # Transformation when train_flag=FALSE is independent of other rows in the dataset
#'   all(dat1a$dat[1,]==dat2a$dat[1,])
filters=function(dat,
                 f_var=c(), 
                 c_var=c(),
                 r_var=c(),
                 i_var=c(),
                 p_var=c(),
                 o_var=c(),
                 m_thresh=NA,
                 sd_thresh=NA,
                 onehot=FALSE,
                 normalise=FALSE,
                 addpca=0,
                 addtopics=0,
                 onehot_ref=NA,
                 mvec=NA,
                 mu_t=NA,
                 sd_t=NA,
                 pca_transform=NA,
                 topic_transformer=NA,
                 topic_model=NA,
                 verbose=TRUE
) {
  
  print("Starting")
  orig_names=colnames(dat)
  
  # Suitability for use on test dataset: essentially, transformation must be independent of other rows of test dataset
  train_flag=!(
    is.na(m_thresh) & # we cannot remove based on missingness, as this is a property of the data
      is.na(sd_thresh) & # the same for SD
      (normalise==FALSE | all(is.finite(mu_t+sd_t))) & # We either need to not normalise, or normalise based on existing data
      (length(i_var)==0 | all(is.finite(mvec))) & # We need to either not impute, or impute to pre-set values
      (addpca==0 | is.matrix(pca_transform) & # We need to either not generate PCA features, or generate them based on a previously generated rotation matrix.      
      (addtopics==0 | ("LDA_VEM" %in% class(topic_model)))) # We need to either not generate topic features, or generate them based on a previously fitted topic model.
  )
  
  # Filter
  print("Applying filters")
  if (length(f_var)>0) {
  for (i in 1:length(f_var)) 
    if (f_var[[i]] %in% colnames(dat)) {
      dat=dat[which(dat[[f_var[i]]]==1),];
      gc()
    }
  }
  gc(); 
  if (length(c_var)>0) {
  for (i in 1:length(c_var)) 
    if (c_var[[i]] %in% colnames(dat)) {
      dat=dat[which(is.finite(dat[[c_var[i]]])),];
      gc()
    }
  }
  gc()
  
  # Remove other variables
  print("Removing specified variables")
  if (length(r_var)>0) {
    r_var=intersect(r_var,colnames(dat))
    dat=dat[,-match(r_var,colnames(dat))]
  }
  
  # Remove excessive missingness
  print("Removing variables with excessive missingness")
  ntot=dim(dat)[1]
  xvar=c()
  if (is.finite(m_thresh)) {
    for (i in 1:dim(dat)[2]) {
      if (length(which(!is.finite(dat[[i]])))/ntot >= m_thresh)  xvar=c(xvar,colnames(dat)[i])
    }
    dat=dat[,-match(xvar,colnames(dat))]
  }
  
  # Convert yes/no variables to one-hot
  print("Converting variables to one-hot")
  if (onehot) {
    newref=onehot_ref
    for (i in 1:dim(dat)[2]) {
      lb=attributes(onehot_ref[[colnames(dat)[i]]])$labels
      lbp=sort(names(lb)[which(lb>=0)])
      if (setequal(lbp,c("No","Yes"))) {
        orig=dat[,i]
        wy=which(orig==lb["Yes"])
        wn=which(orig==lb["No"])
        orig[wy]=1
        orig[wn]=0
        dat[,i]=orig
        lbn=lb; lbn["Yes"]=1; lbn["No"]=0
        attributes(newref[[colnames(dat)[i]]])$labels=lbn
      }
    }
  } else newref=onehot_ref
  
  
  # Mean value imputation
  print("Imputing (mean-value)")
  if (length(mvec)==1 & is.na(mvec[1]) & length(i_var)>0) {
    mvec=rep(NA,length(i_var)); 
    names(mvec)=i_var
  }
  if (length(i_var)>0) {
    i_var=intersect(i_var,colnames(dat))
    i_var=intersect(i_var,names(mvec))
    mvec=mvec[i_var]
    for (i in 1:length(i_var)) {
      if (verbose & ((i %% 500)==0)) print(paste0("Completed ",i," of ",length(i_var)))      
      if (!is.finite(mvec[i_var[i]])) 
        m=mean(dat[[i_var[i]]],na.rm=T) else m=mvec[i_var[i]]
        dat[[i_var[i]]][which(!is.finite(dat[[i_var[i]]]))]=m
        mvec[i_var[i]]=m; 
    }
  }
  
  # Remove excessive missingness
  print("Removing variables with low variation")
  svar=c()
  if (is.finite(sd_thresh)) {
    for (i in 1:dim(dat)[2]) 
      if (sd(dat[[i]],na.rm=T) <= sd_thresh)  svar=c(svar,colnames(dat)[i])
    dat=dat[,-match(svar,colnames(dat))]
  }
  
  
  # Normalise
  print("Normalising")
  if (normalise) {
    if (length(mu_t)<= 1 & !is.finite(mu_t[1])) mu_t=rep(NA,dim(dat)[2])
    if (length(sd_t)<= 1 & !is.finite(sd_t[1])) sd_t=rep(NA,dim(dat)[2])
    for (i in setdiff(1:dim(dat)[2],match(o_var,colnames(dat)))) {
      orig=dat[,i]
      nm=colnames(dat)[i]
      if (!is.finite(mu_t[nm])) mu_t[nm]=mean(orig,na.rm=T)
      if (!is.finite(sd_t[nm])) sd_t[nm]=sd(orig,na.rm=T)
      orig=(orig-mu_t[nm])/sd_t[nm]
      dat[,i]=orig
    }
    if (length(o_var)>0) {
      mu_t[o_var]=0
      sd_t[o_var]=0
    }
    mu_t=mu_t[intersect(names(mu_t),colnames(dat))]
    sd_t=sd_t[intersect(names(sd_t),colnames(dat))]
  }
  
  # Principal components
  print("Generating principal component features")
  if (addpca>0) {
    if (length(pca_transform)==1) {
      p_var=intersect(p_var,colnames(dat))
      pca_transform=prcomp(dat[,p_var])$rotation
    }
    pcas=(as.matrix(dat[,rownames(pca_transform)]) %*% pca_transform)[,1:addpca]
    colnames(pcas)=paste0("PC",1:addpca)
    dat=cbind(dat,pcas)
  }

  # Topic features
  print("Generating topic features")
  if (addtopics>0) {
    if (suppressWarnings(is.na(topic_model))) {
      dat2=topic_transformer(dat)
      dat2=dat2[which(rowSums(dat2)>0),]
      if (verbose) topic_model=LDA(dat2,k = addtopics,control=list(seed=746253,verbose=1)) else {
        topic_model=LDA(dat2,k = addtopics,control=list(seed=746253))
      }
      rm(list=c("dat2")); gc()
    }
    topics=posterior(topic_model,newdata = topic_transformer(dat))$topics
    colnames(topics)=paste0("topic",1:addtopics)
    dat=cbind(dat,topics)
    gc()
  }
  
  
    
  
  # All removed variables
  x_rm=setdiff(orig_names,colnames(dat))
  
  # return
  return(list(
    dat=dat,
    x_rm=x_rm,
    mvec=mvec, 
    mu_t=mu_t,
    sd_t=sd_t,
    PRM=pca_transform, 
    topic_model=topic_model,
    newref=newref,
    train_flag=train_flag))
}






##' integral() 
##' Quick form for trapezoidal integration over range of x
##'
##' @param x x co-ordinates, or nx2 matrix of points 
##' @param y y co-ordinates
##' @return trapezoidal estimate of integral of y[x] over range of x.
integral=function(x,y=NULL) {
  if (is.null(y)) {
    y=x[,2]; x=x[,1]
  }
  ox=order(x); xs=x[ox]; ys=y[ox]
  sum((xs[-1]-xs[-length(xs)])*(ys[-1]+ys[-length(ys)]))/2
}



#' Transformer for topic model fitting
#' 
#' @param X dataset
#' @return transformed dataset
topic_transformer_ActiveLives=function(X) {
  
  #  Set of variables to use. These are all reduced to one-hot (Yes/No)
  lda_var=mgrep(c("ACTY","DAYS","DUR_","DURA","FREQ","HABI",
                  "INOU","MEMS","MINS","MONT","SETI","SETO","SURF"),
                colnames(X))
  
  X2=X[,lda_var]
  
  X2$Male=(X$Gend3==1)
  X2$Female=(X$Gend3==2)
  X2$U40=(X$Age1640==1)
  X2$O40=(X$Age1640==2)
  X2$High_IMD=(X$IMD10>5)
  X2$Low_IMD=(X$IMD10<6)
  X2$ChildU13=(X$Child3U13>0)
  X2$NoChildU13=(X$Child3U13==0)
  X2$Qual_34=(X$Educ6<3)
  X2$Qual_012=(X$Educ6>2)
  X2$LimitingDisability=(X$Disab3==1)
  X2$NoLimitingDisability=(X$Disab3==3)
  X2$Working=(X$WorkStat5==1)
  
  # Round X2
  X2=round(X2)
  
  # 1 or 0 answers
  X2=1*(X2>0)
  
  # Return
  return(X2)
}






##' getroc() 
##' Comprehensive plotting function for receiver-operator characteristic curve. Also calculates AUROC and standard error. 
##' 
##' Rather than returning points corresponding to every cutoff, only returns a representative sample of equally-spaced points along the curve.
##'
##' SE of AUROC with no CV structure is from Hanley and McNeil 1982. SE of AUROC with CV folds is from LeDell et al 2012
##'
##' Does not plot anything. Object can be plotted in a default way.
##'
##' @param y class labels, 0/1 or logical
##' @param ypred predictions Pr(Y=1), numeric vector
##' @param cv cross-validation fold assignments, if relevant. Changes estimate of standard error.
##' @param res resolution. Returns this many equally-spaced points along the curve. Set res to null to return all points.
##' @return list containing: spec, specificity for res points in every cv fold; sens, sensitivity for res points in every cv fold; auc, areas under the curve for each fold and average (note length is 1 greater than number of CV folds); se, standard error for AUC in each fold and standard error for average auc (note length is 1 greater than number of CV folds)
getroc=function(y,ypred,cv=NULL,res=100,addauc=FALSE,cols=NULL) {
  if (is.null(cv)) cv=rep(1,length(y))
  if (!(length(y)==length(ypred))) stop("Parameters y and ypred should have the same length")
  
  sens=c(); spec=c(); auc=c(); se=c()
  for (i in 1:max(cv)) {
    y0=y[which(cv==i)]; 
    ypred0=ypred[which(cv==i)]
    
    yt=sum(y0); yl=length(y0)
    opred=order(ypred0)
    #ipred=order(opred) # can use ipred to reorder in the order of original ypred
    
    sy=y0[opred]; sp=ypred0[opred]
    
    sens0=1- (cumsum(sy)/yt)
    spec0=cumsum(1-sy)/(yl-yt) 
    auc0=integral(sens0,spec0)
    se0=aucse(as.numeric(yt),as.numeric(yl-yt),auc0)
    
    if (!is.null(res)) {
      ds=cumsum(sqrt((spec0[1:(yl-1)]-spec0[2:yl])^2 + (sens0[1:(yl-1)]-sens0[2:yl])^2))
      ds=ds/ds[yl-1]
      lsp=(1:(yl-1))/yl
      sub=round(yl*approx(ds,lsp,n=res)$y)
      sens0=sens0[sub]
      spec0=spec0[sub]
    }
    
    auc=c(auc,auc0)
    se=c(se,se0)
    spec=rbind(spec,spec0)
    sens=rbind(sens,sens0)
  }
  
  if (length(auc)>1) {
    auc=c(auc,mean(auc))
    se=c(se,ci.cvAUC(ypred,y,folds=cv)$se)
  }
  
  out=list(sens=sens,spec=spec,auc=auc,se=se)
  class(out)="xROC"
  return(out)
}

# Internal function to compute SE of AUC
aucse=function(n1,n2,auc) {
  q1=auc/(2-auc); q2=2*(auc^2)/(1+auc)
  num=auc*(1-auc) + (n1-1)*(q1- (auc^2)) + (n2-1)*(q2-(auc^2))
  return(sqrt(num/(n1*n2)))
}



##' Plot function for class above
##' @param out output from getroc()
##' @param addauc set to TRUE to add text to the plot showing the (mean) AUC and SE.
##' @param cols colour to draw lines
plot.xROC=function(out,addauc=FALSE,cols=rep("black",dim(out$sens)[1]),...) {
  plot(0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="1-Spec.",ylab="Sens.",...)
  ncv=dim(out$spec)[1]
  for (i in 1:ncv) lines(1-out$spec[i,],out$sens[i,],col=cols[i])
  abline(0,1,col="red",lty=2)
  auc=out$auc[length(out$auc)]
  se=out$se[length(out$se)]
  txx=paste0(signif(auc,digits=2),"+/-",signif(se,digits=2))
  if (addauc) text(0.6,0.4,txx)
}



##' getprc() 
##' Comprehensive plotting function for precision-recall curve. Also calculates AUPRC and standard error. 
##' 
##' Rather than returning points corresponding to every cutoff, only returns a representative sample of equally-spaced points along the curve.
##'
##' Does not plot anything. Object can be plotted in a default way.
##'
##' @param y class labels, 0/1 or logical
##' @param ypred predictions Pr(Y=1), numeric vector
##' @param cv cross-validation fold assignments, if relevant. Changes estimate of standard error.
##' @param res resolution. Returns this many equally-spaced points along the curve. Set res to null to return all points.
##' @return list containing: ppv, ppv for res points in every cv fold; sens, sensitivity for res points in every cv fold; auc, areas under the curve for each fold and average (note length is 1 greater than number of CV folds); se, standard error for AUC in each fold and standard error for average auc (note length is 1 greater than number of CV folds)
getprc=function(y,ypred,cv=NULL,res=100,addauc=FALSE,cols=NULL) {
  if (is.null(cv)) cv=rep(1,length(y))
  if (!(length(y)==length(ypred))) stop("Parameters y and ypred should have the same length")
  
  sens=c(); ppv=c(); auc=c(); se=c()
  for (i in 1:max(cv)) {
    y0=y[which(cv==i)]; 
    ypred0=ypred[which(cv==i)]
    
    yt=sum(y0); yl=length(y0)
    opred=order(ypred0)
    #ipred=order(opred) # can use ipred to reorder in the order of original ypred
    sy=y0[opred]; sp=ypred0[opred]
    
    ppv0=rev(cumsum(rev(sy))/(1:length(sy)))
    sens0=1- (cumsum(sy)/yt)
    
    auc0=integral(sens0,ppv0)
    
    
    se0=sqrt(auc0*(1-auc0)/sum(y0))
    
    if (!is.null(res)) {
      ds=cumsum(sqrt((ppv0[1:(yl-1)]-ppv0[2:yl])^2 + (sens0[1:(yl-1)]-sens0[2:yl])^2))
      ds=ds/ds[yl-1]
      lsp=(1:(yl-1))/yl
      sub=suppressWarnings(round(yl*approx(ds,lsp,n=res)$y))
      sens0=sens0[sub]
      ppv0=ppv0[sub]
    }
    
    auc=c(auc,auc0)
    se=c(se,se0)
    ppv=rbind(ppv,ppv0)
    sens=rbind(sens,sens0)
  }
  
  if (length(auc)>1) {
    auc=c(auc,mean(auc))
    se=c(se,mean(se)/sqrt(3))
  }
  
  
  out=list(sens=sens,ppv=ppv,auc=auc,se=se)
  class(out)="xPRC"
  return(out)
}


##' Plot function for class above
##' @param out output from getprc()
##' @param addauc set to TRUE to add text to the plot showing the (mean) AUC and SE.
##' @param cols colour to draw lines
plot.xPRC=function(out,addauc=FALSE,cols=rep("black",dim(out$sens)[1]),...) {
  plot(0,type="n",xlim=c(0,1),ylim=c(0,1),xlab="Recall",ylab="Precision",...)
  ncv=dim(out$sens)[1]
  for (i in 1:ncv) lines(out$sens[i,],out$ppv[i,],col=cols[i])
  auc=mean(out$auc)
  se=mean(out$se)/sqrt(3)
  txx=paste0(signif(auc,digits=2),"+/-",signif(se,digits=2))
  if (addauc) text(0.6,0.4,txx)
}



##' plotcal() 
##' Produces a set of points for a calibration plot, and optionally plots them.
##' Uses either a binning method or a kernel method to determine height of points. 
##' In both method, divides unit interval into subintervals [0,1/n], [1/n,2/n), ... [(n-1)/n,1). 
##' For bin \[a,b\)
##'    x co-ordinate is (a+b)/2
##'   For binning method
##'    y co_ordinate is mean({y:predicted value for y is in [a,b)})
##'   For kernel method
##'    y co-ordinate is weighted mean of all y values with the weight of value yi given by dnorm(y-yi,sd=kernel_sd)
##'
##' @param y class labels, 0/1 or logical
##' @param ypred predictions Pr(Y=1), numeric vector
##' @param n number of subintervals/points
##' @param kernel set to TRUE to use kernel method
##' @param kernel_sd kernel width for kernel method; see above
##' @param conf include a confidence interval; alpha, c0, c2 are only relevant if conf=TRUE
##' @param alpha return a pointwise confidence envolope for conservative 1-alpha confidence interval
##' @param c0 for computing maximum bias; assume true covariance function is of the form a0+ a1x + a2x^2, with |a0|<c0, |a2|<c2 (c1 does not matter)
##' @param c2 for computing maximum bias; assume true covariance function is of the form a0+ a1x + a2x^2, with |a0|<c0, |a2|<c2 (c1 does not matter)
##' @param plot set to FALSE to suppress plot
##' @param ... further parameters passed to plot()
##' @return n x 2 matrix containing co-ordinates of points on the curve.
plotcal=function(y,ypred,n=10,kernel=F,kernel_sd=0.05,alpha=0.05,c0=0,c2=0.1,plot=TRUE,conf=TRUE,...)  {
  if (!kernel) {
    ycal=rep(0,n); xcal=ycal
    xup=rep(0,n); xdown=rep(0,n)
    for (i in 1:n) {
      sub=which((ypred> (i-1)/n) & (ypred< i/n))
      ycal[i]=mean(y[sub])
      xcal[i]=mean(ypred[sub])
      if (conf) {
        xse=sqrt(ycal[i]*(1-ycal[i])/length(sub))
        xup[i]=ycal[i] -qnorm(alpha/2)*xse
        xdown[i]=ycal[i] + qnorm(alpha/2)*xse
      }
    }
    if (plot) plot(xcal,ycal,...)
    return(list(x=xcal,y=ycal,upper=xup,lower=xdown))
  } else {  # use kernel method with given sd
    ypredsub=seq(0,1,length=n)
    kern=function(x,y) dnorm(x-y,sd=kernel_sd)
    wt=outer(ypredsub,ypred,kern)
    x1=(wt %*% ypred)
    y1=(wt %*% y)
    csub=ypredsub*y1/x1
    csub=pmax(pmin(csub,1),0)
    
    if (plot) plot(ypredsub,csub,...)
    
    if (conf) {
      # Confidence intervals
      wts=ypredsub*wt/as.vector(x1) # now csub= wts %*% y
      yvar=(wts^2 %*% as.vector(ypred*(1-ypred)))
      
      # Max bias
      bias=rowSums(outer(ypredsub,ypred,function(x,y) kern(x,y)*(c0*(y-x) + c2*(x^2 * y - y^2 * x))))/x1
      
      upper=csub - qnorm(alpha/2)*sqrt(yvar) + bias
      lower=csub + qnorm(alpha/2)*sqrt(yvar) - bias
      
      return(list(x=ypredsub,y=csub,lower=lower,upper=upper))
    } else {
      return(cbind(ypredsub,csub))
    }
  }
}



#' Permutation test for equality of weighted means
#' Simulate under the null that 'yn' depends on 'wt' but not on 'cat',
#'  by permuting 'yn' values within intervals of 'wt'.
#' This will only really work on >>100 samples.
#' 
#' @param yn 1/0 events to be tested
#' @param wt weights, should be positive
#' @param cat categories 1 through c
#' @return p-value, statistic, and empirical null values
perm_wt_means=function(yn,wt,cat,m=1000,seed=382739,verbose=FALSE) {
  # Set seed
  set.seed(seed)
  
  # Preprocess
  w=which(!is.na(wt+cat))
  yn=yn[w]; wt=wt[w]; cat=cat[w]
  
  # Number of categories
  ncat=max(cat)
  
  # wlist[i] is the indices of weights in a given centile
  qn=c(min(wt),quantile(wt,(1:100)/101),max(wt)+1)
  wlist=list()
  for (i in 1:100) wlist[[i]]=which(wt>= qn[i] & wt<qn[i+1])
  
  # clist[j] is the indices of category j
  clist=list()
  for (j in 1:ncat) clist[[j]]=which(cat==j)
  
  # Effective sample size in each category
  esize=c()
  for (j in 1:ncat) esize[j]=sum(wt[clist[[j]]])
  sub=which(esize>0)
  
  # Now simulate and recalculate test statistic each time
  stats=rep(NA,m)
  for (i in 1:m) {
    
    # Shuffle yn within weight centiles
    yn2=yn
    for (j in 1:length(wlist)) yn2[wlist[[j]]]=sample(yn[wlist[[j]]])
    
    # Get weighted sum of yn2 within categories
    csum=rep(0,ncat)
    for (j in 1:ncat) csum[j]=sum(yn2[clist[[j]]]*wt[clist[[j]]])
    
    # Compute test statistic, approximately chi-squared distributed (exactly chi-square if all weights are 1)
    stats[i]=suppressWarnings(prop.test(csum[sub],esize[sub])$statistic)
    
    if (verbose & ((i %% 100)==0)) print(i)
  }
  
  # Now our test value: get weighted sum of yn2 within categories
  csum0=rep(0,ncat) 
  for (j in 1:ncat) csum0[j]=sum(yn[clist[[j]]]*wt[clist[[j]]])
  stat=suppressWarnings(prop.test(csum0[sub],esize[sub])$statistic)
  
  # Return
  out=list(statistic=stat, null=stats,p.value=min(1,max(1/m,1-ecdf(stats)(stat))))
}







#' Bootstrap estimator for standard error of weighted mean
#'
#' @param x sample values
#' @param w sample weights
#' @param n number of trials
#' @param s random seed
b_se_wt=function(x,w,n=500,s=3762) {
  set.seed(s)
  nx=length(x)
  out=rep(NA,n)
  for (i in 1:n) {
    sub=sample(nx,rep=T)
    out[i]=weighted.mean(x[sub],w[sub],na.rm=T)
  }
  return(sd(out))
}
