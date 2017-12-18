rm(list = ls())
library(foreign) # read dataset
library(KernSmooth) # kernel smoothing
library(RColorBrewer) # plot color control 

# user-defined kernel matrix generator
setwd("E:\\GoogleDrive\\R\\research\\thesis\\SCF")
source("k_epan.R")
source("k_mat.R")
source("k_regress.R")

label.font.size = 1.5
axis.font.size  = 1.5
point.size      = 1.5
point.type      = "*"
lab.dist.from.axis = 2.5
chart.blackline.lwd = 2
chart.blueline.lwd = 3
leg.line.lwd    = 2
upper.x.limit   = 70
leg.font.size   = 1.3
low.lim.palette = 28
up.lim.palette  = 100
add.common.kreg = 1 # 1, yes; 0, no

# edit(getAnywhere('locpoly')) # trick to see source code

# !! Be sure you have consistent "baseyear" setting here with those in SCF_clearning.R.

baseyear = 1983 # see SCF_clearning.R for parameter setting. 
endyear  = 1998  # 2001
dollaryear = 2013
lbdage  = 19 # we look at people 19 years old or older
lbd.age.bin.one = 20 # lower bound of the age bin of the earliest survey in the base year 1983
lbd.age.bin.last = 65 # lower bound of the age bin of the earliest survey in the base year 1983 #<> 4
length.bin = 5
num.bins = (lbd.age.bin.last-lbd.age.bin.one)/length.bin + 1 # num.bins actually indicates the number of cohorts considered in the plot
h = 5 # the bandwidth parameter
middle1 = 39
middle2 = 41
middle  = 40

maxage.xaxis = 80 # the rightmost point on the x-axis (the same as in Yang)
x = seq(20,maxage.xaxis,0.5)

varlist = c(
  "eqh", 
  "hom",
  "fin",
  "gfn",
  "net"
)

titlelist = c(
  "Housing asset per adult equivalent",
  "Housing asset",
  "Net worth - housing asset",
  "Financial assets",
  "Net worth (wealth)"
)

statlist = c(
  "mean",
  "p75",
  "median",
  "p25"
)

stat.2.list = c(
  "mean",
  "3rd quartile",
  "median",
  "1st quartile"
)

worklist = c( # for read and write string combination
  "wok",  
  "ent",
  "cmb"
)

work.2.list = c( # for plot
  "worker",  
  "entrepreneur",
  "full sample"
)

# ~= before entering ouktput_deaton.M function in Yang.

yeardim = (endyear-baseyear)/3+1 # number survey years
y.ray   = seq(baseyear,endyear,3) # in reversed order

for( i in seq(1,5,by=1) ){ # 1:length(varlist) ){ # indicators, categories of assets. [varlist]: "eqh", "hom", "dur", "fin", "gfn", "net"
  for( j in c(4,6) ){ # statistics (column number) ==>> 4: mean, 5: q75, 6: q50, 7: q25 : [statlist]
    for( l in 0:2 ){ # 1 worker=0, entrepreneur=1, combined=2 (for plotting purpose) : [worklist]
      
      # stack data by career across surveys
      for( k in seq(yeardim,1,-1) ){ # years (in descending order from young to old survey)
        yr = baseyear+(k-1)*3 # start with the most recent suvery, 1998, instead of the oldest survey 1983.
        setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Cross section")
        
        if(l<2){ # that is, we look at data by occupation (and statistics)
          ds = read.csv(paste(y.ray[k],"enw",varlist[i],".csv",sep="")) # 1983enweqh.csv, for example
          ds = ds[complete.cases(ds),] # trick to remove observations having NA
          
          # combination across survey
          if(k==yeardim){ # starting with the most recent survey (say, 1998, instead of the oldest survey 1983)
            dx = ds[ds$smlbuz==l,] # initialization the container "dx" and only keep the occupation of interest
          }else{
            ds = ds[ds$smlbuz==l,] # continue using the cotainer "dx" and only keep the occupation of interest
            dx = rbind(dx,ds)
          }
          
        }else{ # condition for comparison of worker and boss, or show the whole sample
          
          ds = read.csv(paste(y.ray[k],"com",varlist[i],".csv",sep="")) # 1983comfin.csv, for example
          ds = ds[complete.cases(ds),]
          if(k==yeardim){
            dx = ds
          }else{
            dx = rbind(dx,ds)
          }
          
        } # l
      } # k
      
      # get the boundary of each survey in the integrated data set "dx"
      I=which(diff(dx[,1])<0) # trick to work on column "Age"
      data=array(0,dim=c(yeardim,num.bins)) # initialize an array. year.dim times cohortdim 
      agem=array(NA,dim=c(yeardim,num.bins))
      
      for(m in 1:yeardim){ # make data of a specific year as row vector such that the resulting stacked matrix "data" neatly has column vectors ordered in cohort years
        if(m==1){ # stack row vector with the youngest survey (containing the "oldest" population) on the top.
          if(l<2){
            data[m, 1:I[m]] = t(dx[1:I[m], j-1])
          }else{
            data[m, 1:I[m]] = t(dx[1:I[m], j-2])
          }
          agem[m,1:I[m]] = t(dx[1:I[m],1])
        }else if(m==yeardim){
          if(l<2){
            data[m,1:(nrow(dx)-I[m-1])] = t(dx[(I[m-1]+1):nrow(dx),j-1])
          }else{
            data[m,1:(nrow(dx)-I[m-1])] = t(dx[(I[m-1]+1):nrow(dx),j-2])
          }
          agem[m,1:(nrow(dx)-I[m-1])] = t(dx[(I[m-1]+1):nrow(dx),1])
        }else{
          if(l<2){
            data[m,1:(I[m]-I[m-1])] = t(dx[(I[m-1]+1):I[m],j-1])
          }else{
            data[m,1:(I[m]-I[m-1])] = t(dx[(I[m-1]+1):I[m],j-2])
          }
          agem[m,1:(I[m]-I[m-1])] = t(dx[(I[m-1]+1):I[m],1])
        }
      } # m
      write.csv(data,file=paste("cohortdiverge_",varlist[i],statlist[j-3],worklist[l+1],".csv",sep=""),row.names = FALSE) # dataset = [age, data, year]
      
      # give age of NA a value
      na.idx = which(is.na(agem),arr.ind=TRUE) # get 2-D location index, rather than 1-D sequential index
      if(length(na.idx)>0){
        for( n in 1:nrow(na.idx) ){
          agem[na.idx[n,1],na.idx[n,2]] = agem[na.idx[n,1],na.idx[n,2]-1] + 1
        }
      }
      
      # ~= entering output_deaton.M in Yang.
      # num.bins actually indicates the number of cohorts considered in the plot
      # Here, I clearly identify the corresponding cohort year for every elements in agem and data with a cohort year matrix "yearmat"
      yearmat = array(NA,dim=c(yeardim,num.bins)) 
      y2.ray = seq(endyear,baseyear,-3) # For consistency, starting with the most recent suvery, 1998, instead of the oldest survey 1983.
      for( m in seq(1,yeardim,1) ){ 
        yearmat[m,] = y2.ray[m] # when moving from the top to the bottom of the matrix yearmat, the survey year gets smaller (the oldest survey is on the bottom)
      }
      
      yearray=cbind(c(t(yearmat))) # Trick to turn matrix to 1 dimenional array
      agemray=cbind(c(t(agem)))
      dataray=cbind(c(t(data)))
      
      ageprofile=cbind(agemray,dataray,yearray) # long format. column 1, age; column 2, statistic; column 3, survey year.
      resv.ageprofile = ageprofile
      
      # debug.long table across survies (assemble)
      write.csv(resv.ageprofile,file=paste("asm_",varlist[i],statlist[j-3],worklist[l+1],".csv",sep=""),row.names = FALSE) # dataset = [age, data, year]
      
      # matrix inputs prepared for kernel smoothing
      ageprofile=ageprofile[order(ageprofile[,1]),] # sort by agegroup (useless)
      cohortray =ageprofile[,3]-ageprofile[,1] # cohort variable (a person's date of birth)
      ageprofile=cbind(ageprofile,cohortray) # addition of the 4th column
      
      ageprofile=ageprofile[order(-ageprofile[,4]),] # trick to sort in descending order on "cohortray"
      ageprofile=ageprofile[ageprofile[,4]>1910,] # 90 years old in 2001
      
      unique.cohort=unique(ageprofile[,4]) # trick to know the unique elements in a vector
      cohortdim=length(unique.cohort)
      
      # generate dummy variables (I will drop the 1st column of respective set of dummy variables)
      dumcohort=array(0,dim=c(nrow(ageprofile),cohortdim))
      dumyear=array(0,dim=c(nrow(ageprofile),yeardim))
      for( m in 1:cohortdim ){
        dumcohort[,m]=as.numeric(ageprofile[,4]==unique.cohort[m]) # trick to generate dummy variable, 1 or 0.
      }
      for( m in 1:yeardim ){ # the index increases with the suvery year, so the newer the survey the greater the index.
        dumyear[,m]=as.numeric(ageprofile[,3]==y.ray[m]) # either one or zero; Note: the order should be from the oldest to the most recent survey. Acc. to Deaton
      }
      ageprofile=cbind(ageprofile,dumcohort,dumyear) # 1-3: age, statistic, year. 
      resv.dum.ageprofile = ageprofile
      ageprofile = ageprofile[,-4] # remove column "cohort" not "cohort dummy"
      
      # Deaton estimation -- I
      y = ageprofile[,2] # dependent, Statistic
      z = ageprofile[,1] # independent, Age 11-16-2017 it should be age group.
      s1 = k_mat(z,h) # s1$smat # smoother matrix for ordinary spline smoothing.
      
      # Deaton estimation -- II 11-6-2017 The formulation is wrong!! See the latest version in Python.
      for(m in seq(ncol(ageprofile),3+cohortdim+3,-1)){ # why plus 3? It's because we lost two coefficients as the term on the RHS.
        # see Deaton's book. Analysis of household surveys. page 126.
        ageprofile[,m] = ageprofile[,m] - (m-3-cohortdim-1)*ageprofile[,3+cohortdim+1] + (m-3-cohortdim-2)*ageprofile[,3+cohortdim]  
        # 11-16-2017 the second term should be indexed as 3+cohortdim+2, and the third term should be corrected as 3+cohortdim+1.
      }		
      
      # Deaton estimation -- III
      # drop the first column in each set of dummay variables---cohort dummies and year dummies.
      ageprofile = ageprofile[,-4] # drop the first cohort "dummy" 
      ageprofile = ageprofile[,-((3+cohortdim):(3+cohortdim+1))] # drop the first "two" year "dummies" (because they have been incorporated into the formula of step II)
      
      # Deaton estimation -- III, Yang (2006) technical appendix page 9.
      # see P. Speckman (1988) p.414. the second method.
      y_res  = (diag(nrow(ageprofile))-s1$smat)%*%y
      dummat = ageprofile[,-c(1:3)] # 1-3: age, statistic, year.  dummat is the design matrix for the parametric part of the partial linear model.
      x_res  = (diag(nrow(ageprofile))-s1$smat)%*%dummat 
      beta   = solve(t(x_res)%*%x_res)%*%t(x_res)%*%y_res
      
      ## useless in this project
      #b2 = 0
      #for(m in seq(yeardim+cohortdim-3,cohortdim,-1)){
      #  b2 = b2 - (i-cohortdim+2)*beta[m]
      #}
      #b1 = -b2
      #for(m in seq(yeardim+cohortdim-3,cohortdim,-1)){
      #  b1 = b1 - beta[m]
      #}
      
      # Deaton estimation -- IV
      # see P. Speckman (1988) p.415.
      # Trick: ageprofile[,-c(1:3)] = ageprofile[,4:ncol(ageprofile)]
      fval = k_regress(x, s1$smat%*%(y-dummat%*%beta), z, h) # Speckman, page 414.
      
      pred.data = cbind(c(fval$sray))	  
      
      output.pred = cbind(x,pred.data)
      
      setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel data")
      write.csv(output.pred,file=paste("p_",varlist[i],statlist[j-3],worklist[l+1],".csv",sep=""),row.names=FALSE)
      
      ##------------------------------------new ### ----- 
      fit  = locpoly(z,y,bandwidth=5)
      xrange = range(c(10,x,z))
      yrange = range(c(fval$sray,fit$y,y))
      
      if(0.5*(yrange[1]+yrange[2])>1000){
        scale.factor = 1000
      }else{
        scale.factor = 1
      }    
      
      fval$sray = fval$sray/scale.factor # partially linear model
      fit$y     = fit$y/scale.factor # ordinary kernel regression
      y         = y/scale.factor       
      
      ## Yang's adjustment on the fitted age profile
      Iagem = which(agem<=middle2&agem>=middle1,arr.ind = T)
      data.Iagem = data/scale.factor
      mean.Iagem.data = mean(data.Iagem[Iagem])
      fval.middle     = fval$sray[x==middle]
      adjfac.cohort   = mean.Iagem.data/fval.middle
      fval$sray       = fval$sray*adjfac.cohort       
      
      yrange = range(c(fval$sray,fit$y,y))
      
      setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel plot")
      # single variable plot --- kernel smoothing with and without controlling cohort and time effects.
      if(l<2){
        
        # png(paste("LocalSmooth_",worklist[l+1],varlist[i],statlist[j-3],".png",sep=""))
        svg(paste("LocalSmooth_",worklist[l+1],varlist[i],statlist[j-3],".svg",sep=""))
        
        plot(xrange,yrange,type="n",xlab="",ylab="",xaxt="n",yaxt="n",main=titlelist[i],cex.main=label.font.size) # http://stackoverflow.com/questions/3778084/how-to-adjust-the-size-of-y-axis-labels-only-in-r
        axis(2,cex.axis=axis.font.size) # font size, y axis
        axis(1,cex.axis=axis.font.size)        
        
        colvec = brewer.pal(9,"Set1") # rainbow(12)
        #points(z,y,cex=point.size,pch=point.type)
        
        mtext(paste("Age",sep=""), side=1, line=lab.dist.from.axis, cex=label.font.size)
        grid (NULL,NULL, lty = 6, col = "cornsilk2") # grid line consistent with tick locations        
        
        if(scale.factor==1){
          mtext(paste("(",dollaryear," Dollars)",sep=""), side=2, line=lab.dist.from.axis, cex=label.font.size) # y label, position, font size
        }else{
          mtext(paste("(Thousdans of ",dollaryear," Dollars)",sep=""), side=2, line=lab.dist.from.axis, cex=label.font.size) # y label, position, font size
        }
        #message(paste(work.2.list[l+1],", ",stat.2.list[j-3],", SCF ",baseyear,"-",endyear,"pseudo panel",sep=" "))
        mtext(paste("The ",stat.2.list[j-3]," ",work.2.list[l+1],", SCF ",baseyear,"-",endyear," pseudo panel",sep=""), cex=label.font.size)
        
        # -------------------- kernel smoothing
        lines(fit,col="black",lty=2,lwd=chart.blackline.lwd)           
        
        #legend("bottomright", legend=c("pseudo panel","kernel smoothing", "age profile"),
        #       col=c("black","black", "red"), lty=c(NA,2,1), pch=c(point.type,NA,NA),cex=leg.font.size,lwd=rep(leg.line.lwd,3),
        #       bg="transparent",box.lty=0)  
        legend("bottomright", legend=c("age effect only","mixed effects of age, cohort, year"),
               col=c("red","black"), lty=c(1,2), pch=c(NA,NA),cex=leg.font.size,lwd=c(2.5,leg.line.lwd),
               bg="transparent",box.lty=0)        
        
        ## -------------- if you don't like cohort plots overlapping with regression curve. comment out this block
        bypar = (up.lim.palette-low.lim.palette)/ncol(data)
        color1 = colorRampPalette(brewer.pal(9,"GnBu"))(100)[seq(low.lim.palette,up.lim.palette,by=bypar)] 
        if(add.common.kreg==1){
          for(k in 1:ncol(data)){
            lines(agem[,k],data[,k]/scale.factor,type="l",lty=1,col=color1[k],lwd=2.5)
            lines(agem[,k],data[,k]/scale.factor,type="p",lty=1,col=color1[k],lwd=2.5)
            #scatter(agem[,k],data[,k]/scale.factor)
            #lines(agem[,k],data[,k]/scale.factor,lwd=0)
          }    
        }    
        
        # -------------------- age profile
        lines(x,fval$sray,col="red",lwd=chart.blueline.lwd) # I only look at 22-67.
        
        ## -------------- legend of cohort segments
        cohort_vec = baseyear - agem[nrow(agem),] # baseyear is the first survey year evoked in the plot
        legend("topleft",legend=cohort_vec,col=color1[1:ncol(data)],lty=rep(1,ncol(data)),title="cohort",cex=leg.font.size,lwd=rep(leg.line.lwd,3),
               bg="transparent",box.lty=0)
        
        dev.off()
      }
      
      ## kernel smoothing with controlling; one plot with lines for each occupation and the population
      #if(l==0){ # store worker's data
      #  wok.pred.y = fval$sray
      #  wok.raw.y = y
      #}else if(l==1){
      #  ent.pred.y = fval$sray
      #  ent.raw.y = y
      #}else{
      #  cmb.pred.y = fval$sray
      #}
      #
      #col.wok.vec=brewer.pal(7,"Blues")
      #col.ent.vec=brewer.pal(7,"PuRd")
      #col.cmb.vec=brewer.pal(7,"Greens")      
      #
      ## combined plot -- worker and entrepreneur
      #if(l==2){
      #  png(paste("com_",varlist[i],statlist[j-3],".png",sep=""))
      #  xrange = range(c(x,z))
      #  yrange = range(c(wok.pred.y, wok.raw.y, ent.pred.y, ent.raw.y, cmb.pred.y))
      #  # plot(xrange,yrange,type="n",xlab="Age group",ylab=paste(varlist[i],"1983 U.S. dollars",sep=""))
      #  plot(xrange,yrange,type="n",xlab="age",ylab=paste(dollaryear," U.S. dollars",sep=""))
      #  points(z,wok.raw.y,col=col.wok.vec[3]) # blue
      #  points(z,ent.raw.y,col=col.ent.vec[3]) 
      #  lines(x,wok.pred.y,col=col.wok.vec[7]) # red
      #  lines(x,ent.pred.y,col=col.ent.vec[7])
      #  lines(x,cmb.pred.y,col=col.cmb.vec[5]) # golden
      #  mtext(paste(stat.2.list[j-3],varlist[i],sep=" "))
      #  legend("topleft",legend=work.2.list,col=c(col.wok.vec[7],col.ent.vec[7],col.cmb.vec[7]),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),bty = "n")
      #  dev.off()        
      #}
      
    } # l
  } # j
} # i

setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel data")
write.csv(agem,file=paste("agem",".csv",sep=""))

# kernel smoothing with controlling, comparison of the consumption of house, financial assets and durables by career ================ part II
#subtitlelist = titlelist[c(2,4,5)] 
subtitlelist = titlelist[c(2,3)] 
for(i in 1:3){ # career
  for(j in 1:3){ # statistic # mean, and q50
    
    setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel data")
    #for(k in c(2,4,5)){ # indicator
    for(k in 2:3){ # indicator
      da = read.csv(paste("p_",varlist[k],statlist[j],worklist[i],".csv",sep=""))
      if(k==2){ # k==2 home value
        x=da$x
        y1=da[,2]
      }else if(k==3){ # k==3, fin assets, not wealth net of home value
        y3=da[,2]
      }else{ # k==5, net
        y4=da[,2]
      }
    } # k
    
    setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel plot")
    color=brewer.pal(8,"Pastel1") 
    svg(paste("3econ_",worklist[i],"_",statlist[j],".svg",sep=""))
    xrange = range(c(10,x))
    #yrange = range(c(y1,y3,y4))
    yrange = range(c(y1,y3))
    
    if(mean(yrange)>5000){
      scaler1 = 1000
      #ylabel = paste("(Thousands of ",dollaryear," Dollars)",sep="")
    }else{
      scaler1 = 1
      #ylabel = paste("(",dollaryear," Dollars)",sep="")
    }
    
    #yrange = range(c(y1,y3,y4)/scaler1) # new yrange
    yrange = range(c(c(y1,y3)/scaler1,200)) # new yrange 
    plot(xrange,yrange,type="n",xlab="",ylab="",xaxt="n",yaxt="n",main="Age profile of asset holdings",cex.main=label.font.size)
    grid (NULL,NULL, lty = 6, col = "cornsilk2") # grid line consistent with tick locations     
    lines(x,y1/scaler1,col="black",lwd=chart.blueline.lwd) # home
    lines(x,y3/scaler1,col="red",lwd=chart.blueline.lwd) # fina
    abline(h=0,lty="dashed")
    #lines(x,y4/scaler1,col="blue",lwd=chart.blueline.lwd) # wealth  
    
    if(scaler1==1){
      mtext(paste(dollaryear," Dollars",sep=""), side=2, line=lab.dist.from.axis, cex=label.font.size) # y label, position, font size
    }else{
      mtext(paste("Thousdans of ",dollaryear," Dollars",sep=""), side=2, line=lab.dist.from.axis, cex=label.font.size) # y label, position, font size
    }    
    
    mtext(paste("Age",sep=""), side=1, line=lab.dist.from.axis, cex=label.font.size)
    
    axis(2,cex.axis=axis.font.size) # font size, y axis
    axis(1,cex.axis=axis.font.size)     
    
    mtext(paste("The ",stat.2.list[j]," ",work.2.list[i],", SCF ",baseyear,"-",endyear," pseudo panel",sep=""),cex=label.font.size)
    #legend("topleft",legend=subtitlelist,col=c("black","red","blue"),lty=rep(1,3),lwd=rep(chart.blueline.lwd,3),cex=leg.font.size,bty = "n",bg="transparent",box.lty=0)
    legend("topleft",legend=subtitlelist,col=c("black","red"),lty=rep(1,2),lwd=rep(chart.blueline.lwd,2),cex=leg.font.size,bty = "n",bg="transparent",box.lty=0)
    dev.off()    
  } # j
} # i 

## single asset holdings across "cohort"-group  ================================ part III
#setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel data")
#agem = read.csv(paste("agem",".csv",sep="")) 
#agem.r = agem[nrow(agem):1,]
#agem.r$X = NULL
#for( i in 1:6 ){ # 5 indicators, categories of assets. varlist
#  for( j in 1:4 ){ # 4 statistics 1: mean, 2: q75, 3: q50, 4: q25. statlist
#    for( l in 1:3){
#      
#      # single career
#      setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Cross section")
#      data = read.csv(paste("cohortdiverge_",varlist[i],statlist[j],worklist[l],".csv",sep="")) # asm_gfnqn25cmb.csv, for example
#      data.r = data[nrow(data):1,]
#      data.r$X = NULL
#      
#      setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel plot")
#      
#      bypar = (100-27)/cohortdim
#      if(l==1){
#        color = colorRampPalette(brewer.pal(9,"Blues"))(100)[seq(28,100,by=bypar)]
#      }else if(l==2){
#        color = colorRampPalette(brewer.pal(9,"Reds"))(100)[seq(28,100,by=bypar)] 
#      }else{
#        color = colorRampPalette(brewer.pal(9,"Greens"))(100)[seq(28,100,by=bypar)]
#      }
#      png(paste("cohort_diverge_",varlist[i],statlist[j],worklist[l],".png",sep=""))
#      xrange = range(agem.r)  
#      yrange = range(data.r)
#      # Scale chaning block ----- start ------------------------------------
#      # reset y-axis scaler
#      scaler1 = 1
#      # condition on if scale change takes place
#      if(mean(yrange)>5000){
#        scaler1 = 1000
#        ylabel = paste("Thousands of ",dollaryear," dollars",sep="")
#      }else{
#        ylabel = paste(dollaryear," dollars",sep="")
#      }	 
#      yrange = range(data.r/scaler1)	  
#      plot(xrange,yrange,type="n",xlab="age",ylab=ylabel)      
#      for( k in 1:cohortdim){
#        lines(agem.r[,k],data.r[,k]/scaler1,col=color[k],lwd=2.5)
#      } # k
#      title(titlelist[i])
#      mtext(paste(stat.2.list[j]," ",work.2.list[l],sep=""))
#      legend("topleft",legend=unique.cohort,col=color[1:cohortdim],lty=rep(1,cohortdim),lwd=rep(2.5,cohortdim),bty = "n",title="cohort")	  
#      dev.off()
#    } # l  
#  } # j
#} # i


## "combination" chart of asset holdings across "cohort"-group 
## graph filename, for example, cohort_diverge_durmean.png.
#setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel data")
#agem = read.csv(paste("agem",".csv",sep="")) 
#agem.r = agem[nrow(agem):1,]
#agem.r$X = NULL
#for( i in 1:6 ){ # 5 indicators, categories of assets. varlist
#  for( j in 1:4 ){ # 4 statistics 1: mean, 2: q75, 3: q50, 4: q25. statlist
#    for( l in 1:2){
#      
#      # comparison
#      setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Cross section")
#      data = read.csv(paste("cohortdiverge_",varlist[i],statlist[j],worklist[l],".csv",sep="")) # asm_gfnqn25cmb.csv, for example
#      data.r = data[nrow(data):1,]
#      data.r$X = NULL
#      
#      setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Panel plot")
#      if(l==1){
#        data.r1 = data.r
#        color1 = colorRampPalette(brewer.pal(9,"Blues"))(100)[seq(28,100,by=bypar)] 
#      }else if(l==2){
#        data.r2 = data.r
#        color2 = colorRampPalette(brewer.pal(9,"Reds"))(100)[seq(28,100,by=bypar)] 
#      }else{
#        data.r3 = data.r
#        color3 = colorRampPalette(brewer.pal(9,"Greens"))(100)[seq(28,100,by=bypar)] 
#      }
#      
#      # generate graph
#      if(l==2){
#        png(paste("cohort_diverge_",varlist[i],statlist[j],".png",sep=""))
#        xrange = range(agem.r)  
#        yrange = range(data.r1,data.r2)
#        
#        # Scale changing block ----- start ------------------------------------
#        # reset y-axis scaler
#        scaler1=1
#        scaler2=scaler1        
#        # condition on if scale change takes place
#        if(mean(yrange)>5000){
#          scaler1 = 1000
#          scaler2 = scaler1
#          ylabel = paste("Thousands of ",dollaryear," dollars",sep="")
#        }else{
#          ylabel = paste(dollaryear," dollars",sep="")
#        }
#        
#        yrange = range(data.r1/scaler1,data.r2/scaler2) # new yrange         
#        plot(xrange,yrange,type="n",xlab="age",ylab=ylabel) # new scale label     
#        for( k in 1:cohortdim){
#          lines(agem.r[,k],data.r1[,k]/scaler1,col=color1[k],lwd=2.5) # scaler
#          lines(agem.r[,k],data.r2[,k]/scaler2,col=color2[k],lwd=2.5) # scaler
#        } # k
#        # Scale changing block ----- end --------------------------------------
#        
#        title(titlelist[i])
#        mtext(paste(stat.2.list[j],sep=""))
#        legend("topleft",legend=c("salary worker","small business owner"),col=c(color1[cohortdim],color2[cohortdim]),lty=c(1,1),lwd=c(2.5,2.5),bty = "n")
#        dev.off()
#      }
#    } # l  
#  } # j
#} # i