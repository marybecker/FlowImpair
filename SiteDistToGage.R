library(geosphere)

setwd("P:/Projects/GitHub_Prj/FlowImpair")
camsites<-read.csv("TrailCamSites2018.csv",header=TRUE)

######Function to read in data from USGS data service correctly###############################
######assume one header line and tab delimited structure, with # as a comment out to skip#####
parse_fstat<-function(fstat_lines,skip='#',delim='\t'){
  x<-1;
  while(x<length(fstat_lines) && startsWith(fstat_lines[x],skip)){
    x<-x+1;
  }
  header<-strsplit(fstat_lines[x],delim)[[1]];
  D<-as.data.frame(matrix('',ncol=length(header),nrow=length(fstat_lines)-x),stringsAsFactors=F);
  colnames(D)<-header;
  for(i in x+2:length(fstat_lines)){
    r<-strsplit(fstat_lines[i],delim)[[1]];
    D[i-x-1,1:length(r)]<-r;
  }
  D
}

#character string from right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#####Includes active daily value gages################
site_url<-'https://waterservices.usgs.gov/nwis/site/?format=rdb&stateCd=ct&siteType=ST&siteStatus=active&hasDataTypeCd=dv'
sites<-parse_fstat(readLines(site_url))
sites<- sites[!is.na(sites$site_no),]
sites<-sites[1:70,]#Can't figure out why keeping SID


sdist<-data.frame(dist_M=double(),site_no=character(),STA_SEQ=numeric(),stringsAsFactors=FALSE)
sdistmin<-data.frame(dist_M=double(),site_no=character(),STA_SEQ=numeric(),stringsAsFactors=FALSE)


####Calculate geodesic distance (WGS84)#######################
####Stores closest gage based on min dist######################
####Does not take into account drainage add at later date######

for(j in 1:dim(camsites)[1]){

for (i in 1:dim(sites)[1])  {
  
    sx<-as.numeric(sites[i,c(6,5)])
    sy<-as.numeric(camsites[j,c(4,3)])
    d<-distm(sx,sy)
    sdist[i,1]<-d
    sdist[i,2]<-sites[i,2]
    sdist[i,3]<-camsites$STA_SEQ[j]
    #write.csv(sdist,paste0("sdist",camsites$STA_SEQ[j],".csv"))
}
  
  sdistmin[j,]<-sdist[sdist$dist_M==min(sdist$dist_M,na.rm=TRUE),]
  
}

sdistmin<-merge(sdistmin,camsites,by="STA_SEQ")
sdistmin<-merge(sdistmin,sites,byx="site_no")
write.csv(sdistmin,"sdistmin.csv",row.names=FALSE)