#---------------------------------------------------#
#  Analysis of ActiveLives topic model  -------------
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
dir_object="R_objects/"        # Directory with R objects

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

# Number of topics
ntopics=50

# Force redo
force_redo=FALSE

#---------------------------------------------------#
#  Read training data -------------------------------
#---------------------------------------------------#

# Read reference; first 20 rows of test data. Straightforward to just
#  reproduce original metadata with rbind (e.g. rbind(ref,dat)) if needed.
load(paste0(dir_save,"ref.RDS"))

# Read training data
train_name=paste0(dir_save,maintable_name,"_train.fst")
dat=read_fst(train_name)
gc()




#---------------------------------------------------#
#  Convert to topic model fitting matrix ------------
#---------------------------------------------------#

# Remove variables with low variability/high missingness, and impute
xdat=filters(dat,
            f_var="Age16plus", # Filter >16yo
            i_var=colnames(dat), # Mean-value impute everything
            m_thresh=0.5, # remove if missingness > 0.5
            sd_thresh=1e-5, # or if SD<1e-5
            onehot=TRUE, # convert to one-hot
            normalise=TRUE, # normalise
            onehot_ref=ref) 
dat1=xdat$dat
xdat$dat=NULL

# See documentation of function for details - convert to topic-model form
dat1=topic_transformer_ActiveLives(dat1)
dat1=dat1[which(rowSums(dat1)>0),]

# Fit topic model. Takes some sixteen hours - redo at peril!
topicfile=paste0(dir_object,"activeLives_topicmodel.RData")
if (force_redo | (!file.exists(topicfile))) {
  XLDA=LDA(dat1,k = ntopics,control=list(seed=746253,verbose=1)) 
  save(XLDA,file=topicfile)
} else load(topicfile)

# Clean up
rm(list=c("dat","dat1"))
gc()

#---------------------------------------------------#
#  Predictive analytics -----------------------------
#---------------------------------------------------#

topic_posterior_file=paste0(dir_object,"activeLives_topicmodel_posterior.RData")
if (force_redo | (!file.exists(topic_posterior_file))) {
  
  # Read in validation set
  val_name=paste0(dir_save,maintable_name,"_val.fst")
  val0=read_fst(val_name)
  
  # Read in test set
  test_name=paste0(dir_save,maintable_name,"_test.fst")
  test0=read_fst(test_name)
  
  # Combine
  tv=rbind(val0,test0)
  rm(list=c("test0","val0"))
  gc()
  
  # Remove variables with low variability/high missingness, and impute
  xval=filters(tv,
               f_var="Age16plus", # Filter >16yo
               i_var=setdiff(names(xdat$mvec),c("Volint_ANY","VOLMTH_POP","LA_2021","wt_final")), # Mean-value impute everything
               r_var=xdat$x_rm,
               o_var=c("Volint_ANY","VOLMTH_POP","LA_2021","wt_final"),
               onehot=TRUE, # convert to one-hot
               normalise=TRUE, # normalise
               onehot_ref=ref,
               mu_t=xdat$mu_t,
               sd_t=xdat$sd_t)
  tv1=xval$dat
  xval$dat=NULL
  rm(tv)
  
  # Check (should be FALSE)
  xval$train_flag
  
  
  # General variables
  vyear=tv1$Volint_ANY
  vmonth=tv1$VOLMTH_POP
  vla=tv1$LA_2021
  vwt=tv1$wt_final
  
  
  
  # Process into matrix for which posterior over topics can be calculated
  tval=topic_transformer_ActiveLives(tv1)
  
  # Compute posterior over topics
  vtopics=posterior(XLDA,newdata = tval)$topics
  
  # Save
  save(vtopics,vyear,vmonth,vla,vwt,file=topic_posterior_file)

  # Clean up
  rm(list=c("tv1"))
  gc()
  
} else load(topic_posterior_file)




#---------------------------------------------------#
#  Look at topic model ------------------------------
#---------------------------------------------------#

# Look at betas; beta[i,j]=log prob that word j is in topic i
beta=XLDA@beta
ntopics=XLDA@k

# Include term in topic summary if posterior probability of inclusion in topic
#  is greater than pthresh (noting posterior probabilities sum to 1 over 'words')
pthresh=0.01

topic_summary=list()
for (i in 1:ntopics) { 
  w=which(exp(beta)[i,]>pthresh)
  beta_w=exp(beta)[i,w]
  xterms=XLDA@terms[w[order(-beta_w)]]
  terms=c()
  topic_summary[[i]]=list(terms=xterms,prob=sort(beta_w,dec=T))
}

# Label topics
## TBD
topic_labels=c("Unk", "Mix", "Running", "Bowls", "Watersport", "Walking_active", 
               "Dance_golf", "Unk", "Walking", "Walking_active", "Gym_class", 
               "Gym_general", "Walking_leisure", "Walking_leisure", "Gym_cycling", 
               "Adventure", "Walking_leisure", "Winter_sport", "Walking_active", 
               "Walking_leisure", "Gym_weights", "Leisure", "Unk", "Gym_general", 
               "Walking_leisure", "Tabletennis", "Equestrian", "Trampolining", 
               "Dance", "Walking_leisure", "Mix", "Walking_active", "Gardening", 
               "Cycling_travel", "Walking_active", "Mix", "Rackets", 
               "Swimming", "Dance", "Unk", "Intervals", "Combat", "Running", "Unk",
               "Cycling", "Gardening", "Gym_general", "Walking_leisure", "Football", 
               "Adventure")

topic_labels=make.names(topic_labels,unique=TRUE)
names(topic_summary)=topic_labels

# Ordering
xorder=order(topic_labels)


#---------------------------------------------------#
#  Analyse topic associations -----------------------
#---------------------------------------------------#

vtopics=round(100*vtopics) # nearest percent
tab_topic=data.frame(
  topic_label=topic_labels,
  stat_year=rep(0,ntopics), # test statistics (Hodges-Lehmann estiamtor) for association with volunteering status in past year
  p_year=rep(0,ntopics), # p-value for same
  m0_year=rep(0,ntopics), # mean topic prob for 'no' to Volint_ANY
  m1_year=rep(0,ntopics), # mean topic prob for 'yes' to Volint_ANY
  stat_month=rep(0,ntopics), # test statistics (Hodges-Lehmann estimator) for association with volunteering status in past month
  p_month=rep(0,ntopics), # p-value for same
  m0_month=rep(0,ntopics), # mean topic prob for 'no' to VOLMTH_POP
  m1_month=rep(0,ntopics), # mean topic prob for 'yes' to VOLMTH_POP
  topic_terms=rep("",ntopics) # Topic terms with probability > 0.1
)

wy1=which(vyear==1); wy0=which(vyear==0)
wm1=which(vmonth==1); wm0=which(vmonth==0)
for (i in 1:ntopics) {
  
  # By year
  ty=wilcox.test(vtopics[wy0,i],vtopics[wy1,i],conf.int=T)
  tab_topic$stat_year[i]=ty$estimate
  tab_topic$p_year[i]=ty$p.value
  tab_topic$m0_year[i]=mean(vtopics[wy0,i])
  tab_topic$m1_year[i]=mean(vtopics[wy1,i])
  
  # By month
  tm=wilcox.test(vtopics[wm0,i],vtopics[wm1,i],conf.int=T)
  tab_topic$stat_month[i]=tm$estimate
  tab_topic$p_month[i]=tm$p.value
  tab_topic$m0_month[i]=mean(vtopics[wm0,i])
  tab_topic$m1_month[i]=mean(vtopics[wm1,i])
  
  tab_topic$topic_terms[i]=paste(topic_summary[[i]]$terms,collapse=" ")
  print(i) 
}
tab_topic=tab_topic[xorder,]

# Write table
write.csv(format(tab_topic,digits=3),paste0("Output/topic_table.csv"),row.names=T,quote=T)


# Sidak-corrected significance threshold to control FWER at <0.05 with 100 independent tests
p_thresh=1-(1-0.05)^(1/(2*ntopics))

# Topics with higher probability amongst individuals answering 'Y' to volunteering in last year
y_year=tab_topic$topic_label[which(tab_topic$stat_year<0 & tab_topic$p_year<p_thresh)]

# Topics with higher probability amongst individuals answering 'N' to volunteering in last year
n_year=tab_topic$topic_label[which(tab_topic$stat_year>0 & tab_topic$p_year<p_thresh)]

# Topics with higher probability amongst individuals answering 'Y' to volunteering in last month
y_month=tab_topic$topic_label[which(tab_topic$stat_month<0 & tab_topic$p_month<p_thresh)]

# Topics with higher probability amongst individuals answering 'N' to volunteering in last month
n_month=tab_topic$topic_label[which(tab_topic$stat_month>0 & tab_topic$p_month<p_thresh)]
