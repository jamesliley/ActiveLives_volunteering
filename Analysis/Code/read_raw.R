#---------------------------------------------------#
#  Analysis of ActiveLives data ---------------------
#---------------------------------------------------#
##
## James Liley
## 28 Feb 2022
##
## Read in files from .sav and .xlsx and save to R
##
## Assumes that working directory contains directories
##  'Original' and 'R', where 'Original' contains
##  original SPSS .sav and MS Excel .xlsx files.
##
## This script reads original files and saves them as
##  R/ref.RDS: small table containing first 20 rows of training data with
##   all SPSS metadata included
##  R/main_train.fst,R/main_test.fst,R/main_val.fst: main SPSS tables, split 
##   into train, test and validation sets
##  R/lookup_instructions.txt: instrunctions for excel
##  R/lookup_measures.RDS: lookup tables for measures
##  R/lookup_avtivities.RDS: lookup tables for activities
##  R/lookup_composites.RDS: lookup tables for activity composites

#---------------------------------------------------#
#  Setup --------------------------------------------
#---------------------------------------------------#

# Libraries
library(haven)   # Read and write to SPSS
library(readxl)  # Read and write to Excel
library(fst)     # Compressed format allowing fast read access

# Directories
dir_orig="./Data/Original/"
dir_save="./Data/R/"

# Set working directory if on James's machine
if (file.exists("~/ActiveLives/Data/")) setwd("~/ActiveLives/")

# Names of raw data
sav_name="20220209 Active Lives Survey_May 17-18 data_Shared.sav"
xl_name="Active Lives Adult Code Book_Mid year 3_V1_final.xlsx" 

# Names of saved data
maintable_name="main"
lookup_name="lookup"

# Set to TRUE if available memory <14G
lowmem=TRUE


#---------------------------------------------------#
#  Read main table ----------------------------------
#---------------------------------------------------#

# Read in. Takes about 5 minutes on my machine
dat_main=haven::read_sav(paste0(dir_orig,sav_name))

# Write as R object
write_fst(dat_main,paste0(dir_save,maintable_name,".fst"))


#---------------------------------------------------#
#  Split into training, test, and validation --------
#---------------------------------------------------#

# Split randomly in 3:1:1 ratio

# Set seed
set.seed(24763)

# Proportions in each class
p_train=0.6
p_val=0.2
p_test=1-p_train-p_val

# Numbers in each class
n_tot=dim(dat_main)[1]
n_train=round(p_train*n_tot)
n_val=round(p_val*n_tot)
n_test=n_tot-n_train-n_val

# Assignments to training, testing, validation sets
ttv=c(rep("train",n_train),rep("val",n_val),rep("test",n_test))
ttv=ttv[order(runif(n_tot))] # Random order
w_train=which(ttv=="train")
w_val=which(ttv=="val")
w_test=which(ttv=="test")


#---------------------------------------------------#
#  Split data and save ------------------------------
#---------------------------------------------------#

# Save metadata as first 20 rows of test data; fst will only save data frames.
ref=dat_main[w_train[1:20],]
save(ref,file=paste0(dir_save,"ref.RDS"))

# High memory version
if (!lowmem) {
  
  # Split data
  dat_train=dat_main[w_train,]
  dat_val=dat_main[w_val,]
  dat_test=dat_main[w_test,]
  
  # Save to R. 20 seconds, 600Mb
  write_fst(dat_train,paste0(dir_save,maintable_name,"_train.fst"))
  write_fst(dat_val,paste0(dir_save,maintable_name,"_validation.fst"))
  write_fst(dat_test,paste0(dir_save,maintable_name,"_test.fst"))
}

# Alternative lower-memory version; slower
if (lowmem) {
  dir.create(paste0(dir_save,"temp"))
  mainname=paste0(dir_save,"temp/",maintable_name,".fst")
  write_fst(dat_main,mainname)
  rm(dat_main); gc()
  nstep=5000; nmax=(ceiling(n_tot/nstep))
  for (i in 1:nmax) {
    rmin=((i-1)*nstep + 1); rmax=min(i*nstep,n_tot) # read these rows only
    xtrain=intersect(w_train,rmin:rmax)-rmin+1
    xval=intersect(w_val,rmin:rmax)-rmin+1
    xtest=intersect(w_test,rmin:rmax)-rmin+1
    datsub=read_fst(mainname, from=rmin,to=rmax)
    trainsub=datsub[xtrain,]
    valsub=datsub[xval,]
    testsub=datsub[xtest,]
    write_fst(trainsub,paste0(dir_save,"temp/",maintable_name,"_train",i,".fst"))
    write_fst(valsub,paste0(dir_save,"temp/",maintable_name,"_val",i,".fst"))
    write_fst(testsub,paste0(dir_save,"temp/",maintable_name,"_test",i,".fst"))
    print(i)
    gc()
  }
  for (xclass in c("train","val","test")) {
    for (i in 1:nmax) {
      dat0=read_fst(paste0(dir_save,"temp/",maintable_name,"_",xclass,i,".fst"))
      if (i==1) dat=dat0 else dat=rbind(dat,dat0)
      print(i)
      gc()
    }
    write_fst(dat,paste0(dir_save,maintable_name,"_",xclass,".fst"))
  }
  
  
}

#---------------------------------------------------#
#  Read and save lookup tables ----------------------
#---------------------------------------------------#

# Read in (avoiding tidyverse formats). 
#  First sheet is just text
lookup1=names(as.data.frame(readxl::read_excel(paste0(dir_orig,xl_name),sheet=1)))

# Second sheet has several subtables
lookup2=as.data.frame(readxl::read_excel(paste0(dir_orig,xl_name),sheet=2))
w=c(which(is.na(lookup2[,2])),1+dim(lookup2)[1])
l2=list(); for (i in 1:(length(w)-1)) l2[[i]]=lookup2[(w[i]+1):(w[i+1]-1),]
names(l2)=lookup2[w[1:(length(w)-1)],1]

# Third sheet, straight table
lookup3=as.data.frame(readxl::read_excel(paste0(dir_orig,xl_name),sheet=3))

# Fourth sheet; straight table but will need some further parsing
lookup4=as.data.frame(readxl::read_excel(paste0(dir_orig,xl_name),sheet=4))


# Write to file
# Write first sheet as text file
sink(paste0(dir_save,lookup_name,"_instructions.txt"))
cat(lookup1)
sink()

# Write second, third and fourth sheets as R objects
saveRDS(l2,file=paste0(dir_save,lookup_name,"_measures.RDS"))
saveRDS(lookup3,file=paste0(dir_save,lookup_name,"_activities.RDS"))
saveRDS(lookup4,file=paste0(dir_save,lookup_name,"_composites.RDS"))
