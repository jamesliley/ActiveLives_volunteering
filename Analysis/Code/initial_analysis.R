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


#---------------------------------------------------#
#  Read reference data ------------------------------
#---------------------------------------------------#

# Read reference; first 20 rows of test data. Straightforward to just
#  reproduce original metadata with rbind (e.g. rbind(ref,dat)) if needed.
load(paste0(dir_save,"ref.RDS"))


#---------------------------------------------------#
#  Basic exploratory analysis -----------------------
#---------------------------------------------------#

# Read full dataset
all_name=paste0(dir_save,maintable_name,".fst")
dat=read_fst(all_name); gc()

# Basic info
dim(dat)
tab_raw_na_Volint_ANY=table((is.na(dat$Volint_ANY)))
tab_raw_na_VOLMTH_POP=table((is.na(dat$VOLMTH_POP)))
tab_raw_na_wt_final=table((is.na(dat$wt_final)))
tab_raw_na_LA_2021=table((is.na(dat$LA_2021)))

# NA weights with age<16
n_wt_na_u16=length(which(dat$Age16plus==0 & is.na(dat$wt_final)))

# Remove U16s and NA weight
dat=dat[which(dat$Age16plus==1 & is.finite(dat$wt_final)),]

#---------------------------------------------------#
#  Tables of raw volunteering rates by LA -----------
#---------------------------------------------------#

xla=attributes(ref$LA_2021)$labels

tab_year=data.frame(Code=xla,
                    Yes_year=rep(NA,length(xla)),
                    No_year=rep(NA,length(xla)),
                    Unav_year=rep(NA,length(xla)),
                    Raw_pct_yes=rep(NA,length(xla)),
                    Raw_wt_pct_yes=rep(NA,length(xla)),
                    Raw_pct_na=rep(NA,length(xla)),
                    Raw_wt_pct_na=rep(NA,length(xla)),
                    Total=rep(NA,length(xla)))
for (i in 1:length(xla)) {
  w=which(dat$LA_2021==xla[i])
  d0=dat$Volint_ANY[w]
  w0=dat$wt_final[w]
  wsub=which(is.finite(w0))
  tab_year$Yes_year[i]=length(which(d0==1))
  tab_year$No_year[i]=length(which(d0==0))
  tab_year$Unav_year[i]=length(which(is.na(d0)))
  tab_year$Raw_pct_yes[i]=100*length(which(d0==1))/length(which(is.finite(d0)))
  tab_year$Raw_wt_pct_yes[i]=100*weighted.mean(d0[wsub],w=w0[wsub],na.rm=T)
  tab_year$Raw_pct_na[i]=100*length(which(is.na(d0)))/length(w)
  tab_year$Raw_wt_pct_na[i]=100*weighted.mean(is.na(d0)[wsub],w=w0[wsub],na.rm=T)
  tab_year$Total[i]=length(d0)
}

# Table of last-month volunteering rates by local authority area
tab_month=data.frame(Code=xla,
                     Yes_month=rep(NA,length(xla)),
                     No_month=rep(NA,length(xla)),
                     Unav_month=rep(NA,length(xla)),
                     Raw_pct_yes=rep(NA,length(xla)),
                     Raw_wt_pct_yes=rep(NA,length(xla)),
                     Raw_pct_na=rep(NA,length(xla)),
                     Raw_wt_pct_na=rep(NA,length(xla)),
                     Total=rep(NA,length(xla)))
for (i in 1:length(xla)) {
  w=which(dat$LA_2021==xla[i])
  d0=dat$VOLMTH_POP[w]
  w0=dat$wt_final[w]
  wsub=which(is.finite(w0))
  tab_month$Yes_month[i]=length(which(d0==1))
  tab_month$No_month[i]=length(which(d0==0))
  tab_month$Unav_month[i]=length(which(is.na(d0)))
  tab_month$Raw_pct_yes[i]=100*length(which(d0==1))/length(which(is.finite(d0)))
  tab_month$Raw_wt_pct_yes[i]=100*weighted.mean(d0[wsub],w=w0[wsub],na.rm=T)
  tab_month$Raw_pct_na[i]=100*length(which(is.na(d0)))/length(w)
  tab_month$Raw_wt_pct_na[i]=100*weighted.mean(is.na(d0)[wsub],w=w0[wsub],na.rm=T)
  tab_month$Total[i]=length(d0)
}
# These tables are not saved, as they are identical to those generated at the end of analysis.R

# Does the percentage of individuals answering NA differ between LAs?
# Unweighted
p_raw_na_year=prop.test(tab_year$Unav_year,tab_year$Total)$p.value
p_raw_na_month=prop.test(tab_month$Unav_month,tab_month$Total)$p.value

# Does the percentage of individuals answering Yes or No differ between LAs?
# Unweighted
p_raw_yes_year=prop.test(tab_year$Yes_year,tab_year$Yes_year + tab_year$No_year)$p.value
p_raw_yes_month=prop.test(tab_month$Yes_month,tab_month$Yes_month + tab_month$No_month)$p.value



# Small data frame to test things:
dsub=data.frame(MY1=is.na(dat$Volint_ANY),
                MY2=is.na(dat$VOLMTH_POP),
                Y1=dat$Volint_ANY,
                Y2=dat$VOLMTH_POP,
                wt=dat$wt_final,
                la=factor(dat$LA_2021))


# Does local area have an independent effect from weight on missingness in volunteering rate?
l2y=glm(MY1~wt + la,data=dsub,family=binomial(link=logit));
l1y=glm(MY1~wt,data=dsub,family=binomial(link=logit));
l2m=glm(MY2~wt + la,data=dsub,family=binomial(link=logit));
l1m=glm(MY2~wt,data=dsub,family=binomial(link=logit));
p_la_miss_wt_year=anova(l2y,l1y,test="LRT")$`Pr(>Chi)`[2]
p_la_miss_wt_month=anova(l2m,l1m,test="LRT")$`Pr(>Chi)`[2]

# Under the assumption that missingness is independent of LA given weight, do volunteering rates differ by LA?
l2y=glm(Y1~wt + la,data=dsub,family=binomial(link=logit),na.action=na.omit);
l1y=glm(Y1~wt,data=dsub,family=binomial(link=logit),na.action=na.omit);
l2m=glm(Y2~wt + la,data=dsub,family=binomial(link=logit),na.action=na.omit);
l1m=glm(Y2~wt,data=dsub,family=binomial(link=logit),na.action=na.omit);
p_la_wt_year=anova(l2y,l1y,test="LRT")$`Pr(>Chi)`[2]
p_la_wt_month=anova(l2m,l1m,test="LRT")$`Pr(>Chi)`[2]



#---------------------------------------------------#
#  Univariate associations with vounteering ---------
#---------------------------------------------------#

# Variables to test
vars=c("mode", "month", "CSP", "AGE", "Child4", "Disab3", "Educ6", 
       "Eth7", "Gend3", "IMD10", "nADULT", "ONS_Group", "ONS_SuperGroup", 
       "Orient4", "Relig7", "UrbRur2", "WorkStat10", "Motiva_POP", "motivb_POP", 
       "motivc_POP", "motivd_POP", "motive_POP", "READYAB1_POP", "READYAB2_POP", 
       "READYOP1_POP", "READYOP2_POP", "anxious", "comm1", "happy", 
       "indev", "INDEVTRY", "lifesat", "worthw", "CULFRQ_1_9_POP")

# Variable types (o=ordinal, f=factor, b=binary)
vtypes=c("b", "f", "f", "o", "o", "b", "o", "f", "f", "o", "o", "f", 
         "f", "f", "f", "b", "f", "o", "o", "o", "o", "o", "o", "o", "o", 
         "o", "o", "o", "o", "o", "o", "o", "o", "o")

wy1=which(dat$Volint_ANY==1); wy0=which(dat$Volint_ANY==0)
wm1=which(dat$VOLMTH_POP==1); wm0=which(dat$VOLMTH_POP==0)

tab=c()
for (i in 1:length(vars)) {
  P=dat[[vars[i]]]; MP=is.na(P)
  mpy=prop.test(c(sum(MP[wy1]),sum(MP[wy0])),c(length(wy1),length(wy0)))$p.value # Is missingness in predictor associated with volunteering status answer?
  mpm=prop.test(c(sum(MP[wm1]),sum(MP[wm0])),c(length(wm1),length(wm0)))$p.value # Is missingness in predictor associated with volunteering status answer?
  if (sum(MP)<5) {
    mpy=1; mpm=1
  }
  
  uP=unique(P); uP=uP[which(is.finite(uP))]; uP=sort(uP)
  
  if (vtypes[i]=="b") { # Binary; basic proportional test
    py=prop.test(
      c(length(which(P[wy1]==uP[1])),length(which(P[wy0]==uP[1]))),
      c(length(which(is.finite(P[wy1]))),length(which(is.finite(P[wy0]))))
    )$p.value
    pm=prop.test(
      c(length(which(P[wm1]==uP[1])),length(which(P[wm0]==uP[1]))),
      c(length(which(is.finite(P[wm1]))),length(which(is.finite(P[wm0]))))
    )$p.value
  }
  if (vtypes[i]=="f") { # Factor; test proportions between factors
    t1=as.vector(table(factor(P[wy1],levels=uP)))
    t2=as.vector(table(factor(P[c(wy0,wy1)],levels=uP)))
    ww=which(t2>0)
    py=tryCatch(prop.test(t1[ww],t2[ww])$p.value,error=function(e) NA)
    x1=as.vector(table(factor(P[wm1],levels=uP)))
    x2=as.vector(table(factor(P[c(wm0,wm1)],levels=uP)))
    ww=which(x2>0)
    pm=tryCatch(prop.test(x1[ww],x2[ww])$p.value,error=function(e) NA)
  }
  if (vtypes[i]=="o") { # Ordinal
    py=wilcox.test(P[wy1],P[wy0],na.action=na.omit)$p.value
    pm=wilcox.test(P[wm1],P[wm0],na.action=na.omit)$p.value
  }
  tab=rbind(tab,c(mpy,mpm,py,pm))
}
rownames(tab)=vars
colnames(tab)=c("miss_year","miss_month","P_year","P_month")
tab=data.frame(tab)

desc=rep("",length(rownames(tab)))
for (i in 1:length(desc)) {
  rti=rownames(tab)[i]
  if (rti %in% colnames(ref)) {
    ax=attributes(ref[[rti]])$label 
    if (length(ax)==1) desc[i]=ax else desc[i]="-"
  } else desc[i]="-"
}
tab$description=desc


# Table for last-year. Order by validity, then p-value
tab_year=tab[,c("description","miss_year","P_year")]
w1=which(tab_year$miss_year>0.05); w2=setdiff(1:dim(tab_year)[1],w1)
w1=w1[order(tab_year$P_year[w1])]
tab_year=tab_year[c(w1,w2),]
tab_year$significant=(tab_year$miss_year>0.05 & tab_year$P_year< 0.05/(2*length(vars)))
tab_year$significant[which(is.na(tab_year$significant))]=FALSE

# Write to file
write.csv(format(tab_year,digits=3),paste0("Output/univ_analysis_year.csv"),row.names=T,quote=T)


# Table for last-month. Order by validity, then p-value
tab_month=tab[,c("description","miss_month","P_month")]
w1=which(tab_month$miss_month>0.05); w2=setdiff(1:dim(tab_month)[1],w1)
w1=w1[order(tab_month$P_month[w1])]
tab_month=tab_month[c(w1,w2),]
tab_month$significant=(tab_month$miss_month>0.05 & tab_month$P_month< 0.05/(2*length(vars)))
tab_month$significant[which(is.na(tab_month$significant))]=FALSE

# Write to file
write.csv(format(tab_month,digits=3),paste0("Output/univ_analysis_month.csv"),row.names=T,quote=T)


#---------------------------------------------------#
#  Closer analysis of associations ------------------
#---------------------------------------------------#

# By year
sink("Output/univ_analysis_year_detail.txt")
cat("Univariate associations with Volint_ANY (volunteering status in past year): \n\n")
Y=dat$Volint_ANY; mY=mean(Y,na.rm=T)
vsub=rownames(tab_year)[which(tab_year$significant)]
vtsub=vtypes[match(vsub,vars)]
for (i in 1:length(vsub)) {
    vx=attributes(ref[[vsub[i]]])$labels; vx=vx[which(vx>0)]
    tx=table(Y,dat[[vsub[i]]]); txr=tx[2,]/(tx[1,]+tx[2,])
    xn=names(vx)[match(colnames(tx),vx)]
    if ("0" %in% colnames(tx)) xn[which(colnames(tx)==0)]="No"
    w1=which(txr>= mY); nmore=xn[w1]; xmore=signif(100*txr[w1],digits=3)
    w2=which(txr< mY); nless=xn[w2]; xless=signif(100*txr[w2],digits=3)
    cmore=paste0(nmore," (",xmore,"%)"); 
    cless=paste0(nless," (",xless,"%)")
    cat(paste0("Variable ",vsub[i]," (",attributes(ref[[vsub[i]]])$label,"):\n"))
    cat(paste0("More Y than average: ",paste(cmore,collapse=", "),"\n"))
    cat(paste0("Less Y than average: ",paste(cless,collapse=", "),"\n"))
    cat("\n") 
}
sink()

# By month
sink("Output/univ_analysis_month_detail.txt")
cat("Univariate associations with VOLMTH_POP (volunteering status in past month): \n\n")
Y=dat$VOLMTH_POP; mY=mean(Y,na.rm=T)
vsub=rownames(tab_month)[which(tab_month$significant)]
vtsub=vtypes[match(vsub,vars)]
for (i in 1:length(vsub)) {
  vx=attributes(ref[[vsub[i]]])$labels; vx=vx[which(vx>0)]
  tx=table(Y,dat[[vsub[i]]]); txr=tx[2,]/(tx[1,]+tx[2,])
  xn=names(vx)[match(colnames(tx),vx)]
  if ("0" %in% colnames(tx)) xn[which(colnames(tx)==0)]="No"
  w1=which(txr>= mY); nmore=xn[w1]; xmore=signif(100*txr[w1],digits=3)
  w2=which(txr< mY); nless=xn[w2]; xless=signif(100*txr[w2],digits=3)
  cmore=paste0(nmore," (",xmore,"%)"); 
  cless=paste0(nless," (",xless,"%)")
  cat(paste0("Variable ",vsub[i]," (",attributes(ref[[vsub[i]]])$label,"):\n"))
  cat(paste0("More Y than average: ",paste(cmore,collapse=", "),"\n"))
  cat(paste0("Less Y than average: ",paste(cless,collapse=", "),"\n"))
  cat("\n") 
}
sink()