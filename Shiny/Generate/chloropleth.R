##**********************************************************************
##  ActiveLives paper                                               ####
##  Draw chloropleth maps                      
##  James Liley                                                         
##  Nov 2023                                                              
##**********************************************************************

# Run ./la_associations.R first.

##**********************************************************************
## Libraries, scripts, and settings                                 ####
##**********************************************************************

# Libraries
library(geojsonio)  # Read geoJSON
library(sp)         # Plot geospatial data
library(readxl)     # Read excel file

# Whether to save figures
savefigures=TRUE

##**********************************************************************
## Read and simplify geospatial data                                ####
##**********************************************************************


reduce_poly=function(posx,athresh=1e-4) {
  xp=abs(polyarea(posx[,1],posx[,2]))
  for (xx in 1:5) {
    rmi=c()
    for (i in 2:(dim(posx)[1]-1)) {
      if ((i%%2)==0) {
        p2=abs(polyarea(posx[-i,1],posx[-i,2]))
        if (abs(log(abs(p2/xp)))<athresh) rmi=c(rmi,i)
      }
    }
    posx=posx[setdiff(1:dim(posx)[1],rmi),]
  }
  return(posx)
}


# Geospatial data
xpolyfile="App/uk_coarse.RData"
if (!file.exists(xpolyfile)) {
  la_json="Generate/Data/local_areas_uk.json" # With thanks from https://martinjc.github.io/UK-GeoJSON/
  las=geojson_read(la_json,what="sp")
  xpoly=list()
  xr=c(); yr=c()
  nr=c(); ind=1
  cnames=c()
  for (i in 1:length(las)) {
    nmax=length(las@polygons[[i]]@Polygons)
    for (j in 1:nmax) {
      posx=las@polygons[[i]]@Polygons[[j]]@coords
      if (dim(posx)[1]>5) posx2=reduce_poly(posx,athresh=1e-3) else posx2=posx
      las@polygons[[i]]@Polygons[[j]]@coords=posx2
      xpoly[[ind]]=posx2
      nr[ind]=las$LAD13CD[i]
      cnames[ind]=las$LAD13NM[i]
      xr=c(xr,posx2[,1])
      yr=c(yr,posx2[,2])
      ind=ind+1
    }
  }
  
  names(xpoly)=nr
  names(cnames)=nr
  save(xpoly,cnames,xr,yr,file=xpolyfile) 
} else load(xpolyfile)



##**********************************************************************
## Read LA volunteering data                                        ####
##**********************************************************************

# LA volunteering rate data per month and year
supp_tabs="../Analysis/ActiveLives_supplementary_tables.xlsx"
lav_m=data.frame(suppressMessages(read_excel(path = supp_tabs,sheet=1,range="A14:V324")))
lav_y=data.frame(suppressMessages(read_excel(path = supp_tabs,sheet=2,range="A14:V324")))
for (suff in c("m","y")) {
  lav=get(paste0("lav_",suff))
  cn=colnames(lav)[2:dim(lav)[2]]
  rn=lav[2:dim(lav)[1],1]
  lav=lav[2:dim(lav)[1],2:dim(lav)[2]]
  rownames(lav)=unlist(lapply(strsplit(rn," "),function(x) x[1])); 
  assign(paste0("la_names_",suff),unlist(lapply(strsplit(rn," "),function(x) paste(x[2:length(x)],collapse=" ")))); 
  assign(paste0("la_codes_",suff),unlist(lapply(strsplit(rn," "),function(x) paste(x[1],collapse=" ")))); 
  colnames(lav)=cn
  for (i in 1:dim(lav)[2]) lav[,i]=as.numeric(lav[,i])
  assign(paste0("lav_",suff),lav)
}



##**********************************************************************
## Auxiliary function to convert LA codes                           ####
##**********************************************************************

links=c(
  "Bournemouth"="Bournemouth, Christchurch and Poole",
  "Poole"="Bournemouth, Christchurch and Poole",
  "Aylesbury Vale"="Buckinghamshire",
  "Chiltern"="Buckinghamshire",
  "South Bucks"="Buckinghamshire",
  "Wycombe"="Buckinghamshire",
  "Christchurch"="Dorset", 
  "East Dorset"="Dorset", 
  "North Dorset"="Dorset",
  "Purbeck"="Dorset", 
  "West Dorset"="Dorset",
  "Weymouth and Portland"="Dorset",
  "Corby"="Northamptonshire", 
  "Daventry"="Northamptonshire", 
  "East Northamptonshire"="Northamptonshire", 
  "Kettering"="Northamptonshire",
  "Northampton"="Northamptonshire", 
  "South Northamptonshire"="Northamptonshire", 
  "Wellingborough"="Northamptonshire",
  "Taunton Deane"="Somerset West and Taunton",
  "West Somerset"="Somerset West and Taunton", 
  "Forest Heath"="West Suffolk", 
  "St Edmundsbury"="West Suffolk",
  "Suffolk Coastal"="East Suffolk",
  "Waveney"="East Suffolk",
  "Shepway"="Folkestone and Hythe",
  "King's Lynn and West Norfolk"="Kings Lynn and West Norfolk")



##**********************************************************************
## Read other data                                                  ####
##**********************************************************************

# IMD
imd=read.csv("Generate/Data/Local_Authority_(Lower_Tier)_IMD_2019_(OSGB1936).csv")
rownames(imd)=imd$lad19cd

# ONS data on disparity; from https://www.ons.gov.uk/visualisations/dvc1371/#/E07000223
ons_dis=read.csv("Generate/Data/ons_data.txt",header=TRUE)
ons_dis$la_code=names(cnames)[match(ons_dis$Area,cnames)]
for (i in 1:length(links)) {
  w=which(ons_dis$Area==links[i])
  if (length(w)>0) ons_dis$la_code[w]=names(links)[i]
}
rownames(ons_dis)=ons_dis$la_code

# NSSEC (see app for reference)
nsec0=read.csv("Generate/Data/TS062-2021-3.csv")
n0=dim(nsec0)[1]/10
nsec=nsec0[10*(1:n0)-9,c(1,2,5)]
for (i in 1:9) nsec=cbind(nsec,nsec0[10*(1:n0)-9+i,5])
colnames(nsec)=c("Code","Name",paste0("NS-SeC ",c("DNA",
                 nsec0[2:10,4])))
nsec[3:12]=nsec[3:12]/rowSums(nsec[,3:12])  

# Regional association strength (per month)
load("Generate/Data/out_lmm_month.RData") # called 'out'
lar=t(out)
rownames(lar)=unlist(lapply(strsplit(rownames(lar)," "),function(x) x[1]))
lar_reg=unlist(lapply(strsplit(rownames(lar)," "),function(x) x[2]))

# Regional association strength (per year)
load("Generate/Data/out_lmm_year.RData") # called 'out'
lar_y=t(out)
rownames(lar_y)=unlist(lapply(strsplit(rownames(lar_y)," "),function(x) x[1]))
lar_y_reg=unlist(lapply(strsplit(rownames(lar_y)," "),function(x) x[2]))


# Mean values in local areas
if (!file.exists("Generate/Data/local_area_means.RData")) {
  library(fst)
  all_name="../Analysis/Data/R/main.fst"
  dat=read_fst(all_name); gc()
  load("../Analysis/Data/R/ref.RDS")
  xla=attributes(ref$LA_2021)$labels
  LA=dat$LA_2021; # Local areas (index; need to use reference)
  wt=dat$wt_final # Weights
  xnames=union(colnames(lar),colnames(lar_y))
  dat=dat[,xnames]
  lam=data.frame()
  for (i in 1:length(rownames(lar))) {
    nn=grep(paste0("^",rownames(lar)[i]),names(xla),val=TRUE)
    if (!(length(nn)==1)) print(paste0("Problem with ",rownames(lar)[i]))
    sub=which(LA==xla[nn])
    out=rep(0,dim(dat)[2])
    wtx=wt[sub]; wtx[which(is.na(wtx))]=0
    for (j in 1:length(out)) out[j]=weighted.mean(dat[sub,j],w=wtx,na.rm=TRUE)
    lam=rbind(lam,out)
  }
  rownames(lam)=rownames(lar)
  colnames(lam)=xnames
  save(lam,file="Generate/Data/local_area_means.RData")
} else load("Generate/Data/local_area_means.RData")
lam_reg=unlist(lapply(strsplit(rownames(lam)," "),function(x) x[2]))



##**********************************************************************
## Save stuff                                                       ####
##**********************************************************************

# IMD
ximd=imd[match(names(xpoly),rownames(imd)),]
w=which(is.na(ximd$lad19cd))
for (i in 1:length(w)) {
  w2=which(names(links)==cnames[w[i]])
  if (length(w2)>0) {
    ximd[w[i],]=imd[which(imd$lad19nm==links[w2])[1],]
  }
}


# Volint. by month/year/other ActiveLives stuff
vol=data.frame(id=rownames(lav_m),
               cname=la_names_m[match(rownames(lav_m),la_codes_m)],
               v_month=lav_m$Final_wt_pct_yes,
               v_year=lav_y$Final_wt_pct_yes)
xvol=vol[match(names(xpoly),vol$id),]
w=which(is.na(xvol$id))
for (i in 1:length(w)) {
  w2=which(names(links)==cnames[w[i]])
  if (length(w2)>0) {
    xvol[w[i],]=vol[which(vol$cname==links[w2])[1],]
  }
}

# IMD distribution within-local-area
xons=ons_dis[match(names(xpoly),rownames(ons_dis)),]
w=which(is.na(xons$la_code))
for (i in 1:length(w)) {
  w2=which(names(links)==cnames[w[i]])
  if (length(w2)>0) {
    xons[w[i],]=ons_dis[which(ons_dis$Area==links[w2])[1],]
  }
}

# NSSEC
xnsec=nsec[match(names(xpoly),nsec$Code),]
w=which(is.na(xnsec$Code))
for (i in 1:length(w)) {
  w2=which(names(links)==cnames[w[i]])
  if (length(w2)>0) {
    xnsec[w[i],]=nsec[which(nsec$Name==links[w2])[1],]
  }
}
cn2=c("Corby", "Daventry", "East Northamptonshire", "Kettering", 
      "Northampton", "South Northamptonshire", "Wellingborough")
names(cn2)=paste0(c("North", "West","North","North","West","West","North")," Northamptonshire")
for (i in 1:length(cn2)) {
  w=which(cnames==cn2[i])
  xnsec[w,]=nsec[which(nsec$Name==names(cn2)[i]),]
}


# Local area mean statistics
xlam=lam[match(names(xpoly),rownames(lam)),]
w=which(is.na(rownames(xlam)))
for (i in 1:length(w)) {
  w2=which(names(links)==cnames[w[i]])
  if (length(w2)>0) {
    w3=which(la_names_m==links[w2])[1]
    w4=which(rownames(lam)==la_codes_m[w3])[1]
    xlam[w[i],]=lam[w4,]
    rownames(xlam)[w[i]]=rownames(lam)[w4]
  }
}
colnames(xlam)=paste0("lam_",colnames(xlam))



# Local area association strength (per month)
xlar=lar[match(names(xpoly),rownames(lar)),]
w=which(is.na(rownames(xlar)))
for (i in 1:length(w)) {
  w2=which(names(links)==cnames[w[i]])
  if (length(w2)>0) {
    w3=which(la_names_m==links[w2])[1]
    w4=which(rownames(lar)==la_codes_m[w3])[1]
    xlar[w[i],]=lar[w4,]
    rownames(xlar)[w[i]]=rownames(lar)[w4]
  }
}
colnames(xlar)=paste0("var_month_",colnames(xlar))

# Local area association strength (per year)
xlar_y=lar_y[match(names(xpoly),rownames(lar_y)),]
w=which(is.na(rownames(xlar_y)))
for (i in 1:length(w)) {
  w2=which(names(links)==cnames[w[i]])
  if (length(w2)>0) {
    w3=which(la_names_m==links[w2])[1]
    w4=which(rownames(lar_y)==la_codes_m[w3])[1]
    xlar_y[w[i],]=lar_y[w4,]
    rownames(xlar_y)[w[i]]=rownames(lar_y)[w4]
  }
}
colnames(xlar_y)=paste0("var_year_",colnames(xlar_y))



# Save dataset
dat=cbind(ximd,xons,xvol,xnsec,xlar,xlar_y,xlam)
save(dat,file="App/la_data.RData")

# Copy over ref
file.copy("../Analysis/Data/R/ref.RDS",to="Paper/AL_vol_example/")
