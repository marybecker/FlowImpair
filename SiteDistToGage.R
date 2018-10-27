library(geosphere)

setwd("")
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


#####Includes active daily value gages################
site_url<-'https://waterservices.usgs.gov/nwis/site/?format=rdb&stateCd=ct&siteType=ST&siteStatus=active&hasDataTypeCd=dv'
sites<-parse_fstat(readLines(site_url))
sites<- sites[!is.na(sites$site_no),]
sites<-sites[1:70,]#Can't figure out why keeping SID

indexgage<-read.csv("usgsindexgage.csv",header=TRUE)
indexgage$site_no<-paste("0",indexgage$site_no,sep="")


####Calculate geodesic distance (WGS84)#######################
####Stores closest gage based on min dist######################
####Does not take into account drainage add at later date######

#Empty dataframe
sdist<-data.frame(dist_M=double(),site_no=character(),STA_SEQ=numeric(),stringsAsFactors=FALSE)
sdistmin<-data.frame(dist_M=double(),site_no=character(),STA_SEQ=numeric(),stringsAsFactors=FALSE)

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


####Calculate geodesic distance (WGS84)#######################
####Stores closest REFERENCE gage based on min dist######################

#Empty dataframe
rsdist<-data.frame(dist_M=double(),site_no=character(),STA_SEQ=numeric(),stringsAsFactors=FALSE)
rsdistmin<-data.frame(dist_M=double(),site_no=character(),STA_SEQ=numeric(),stringsAsFactors=FALSE)

for(j in 1:dim(camsites)[1]){
  
  for (i in 1:dim(indexgage)[1])  {
    
    sx<-as.numeric(indexgage[i,c(5,6)])
    sy<-as.numeric(camsites[j,c(4,3)])
    d<-distm(sx,sy)
    rsdist[i,1]<-d
    rsdist[i,2]<-indexgage[i,1]
    rsdist[i,3]<-camsites$STA_SEQ[j]
    #write.csv(sdist,paste0("sdist",camsites$STA_SEQ[j],".csv"))
  }
  
  rsdistmin[j,]<-rsdist[rsdist$dist_M==min(rsdist$dist_M,na.rm=TRUE),]
  
}

rsdistmin<-merge(rsdistmin,camsites,by="STA_SEQ")
rsdistmin<-merge(rsdistmin,indexgage,by.x="site_no",by.y="SiteNumber")
write.csv(rsdistmin,"rsdistmin.csv",row.names=FALSE)










