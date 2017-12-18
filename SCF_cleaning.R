# Li-Pin Juan, 03032016 GOOD

# We use edited (and imputed) and summary version of data. 
# there are three folders to access the raw dataset: 
# For 1983-1986, editd_data; 
# For 1989-2013, full_data and summary_data.
#
# Trick 1 -- search for a particular variable
# x=grepl("age",names(data))
# str(ds[,x])

rm(list = ls())
library(foreign)

# environment and parameters set-up ============================================
# !! check whether the parameter setting is consistent with that in SCF_Deaton.R
# baseyear and lastyear should be the elements in the set {1983, 1986, ..., 1989}
# Krueger suggests to skip survey of 1983 and 1989 because of methodological difference in the survey

baseyear = 1983
lastyear = 2013
dolyr    = 2013 # WARNING: "dolyr" must fall into the years covered by the CPI series below.

lbdage  = 19
lbd.age.bin.one  = 20
lbd.age.bin.last = 65 # the default value also used by Yang.
length.bin = 5
num.bins = (lbd.age.bin.last-lbd.age.bin.one)/length.bin + 1 # num.bins = number of cohorts. 
flag.subset.positive.home.value = 1 #true, 1; false, other than 1.

# I only follow people in the oldest survey who are less than 70 years old ('cz lbd.age.bin.last = 65)

# CPI 1983-2013 -----------------------------------------------------------------------------
# CPI semiannual1983-; the series has to start with January, 1983.
setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\CPI_BLS")
cpi=read.csv(text=readLines("cpi_monthly_1983_2015.csv")[-(1:3)])
names(cpi)[1]="month"
names(cpi)[3]="value"
cpi[,2]=NULL # remove column 2
cpi$value=as.numeric(levels(cpi$value))[cpi$value] #<--- Trick to transform factor to numeric

# Warning message will show for introducing NAs by coercion. Don't worry.

# To be consistant with the range of CPI input data.
no.cpi.years = (2013-1983)/3+1
yearlist = seq(1983,2013,by=3) 

# empty data.frame
aulcpi = data.frame(yearlist, yearlist) # annual cpi empty box
names(aulcpi) = row.names=c("year","value")
aulcpi$value=0

for( i in 1:no.cpi.years ){
  cpi.seven.months.vec = cpi$value[((i-1)*12*3+2):((i-1)*12*3+8)]
  aulcpi[aulcpi$year==yearlist[i],2] = (prod(cpi.seven.months.vec))^(1/7) # annual CPI
}

# delfated to the CPI sereis' base year 1983.
dfr=aulcpi # deflator
dfr$value=1/dfr$value
# inflated to the new reference year of interest (say, the last survey year = 2013)
dfr$value=dfr$value*aulcpi$value[aulcpi$year==dolyr]

# ------ year 1983 ----------------------------------------------------------------------
if(baseyear<=1983){
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\editd_data")
  data = read.dta("scf83b.dta") 
  the_year = 1983
  anl.dfr = dfr$value[dfr$year==the_year] # CPI deflator of a specific year
  data$year   = the_year
  data$weight = data$b3016    # data$b3005/5 # full sample 1983 composite weight (Yang uses b3004) [[1]]
  data$numppl = data$b3101   # total number of persons in household (primary family)
  data$hhhage = data$b4503   # age of head by date of birth
  data$homnet = data$b3710*anl.dfr   # net equity in home (can be negative)
  data$finnet = data$b3321*anl.dfr   # financial net worth (total paper assets minus total debt)
  data$netwrt = data$b3324*anl.dfr   # net worth (gross_assets-pensions+pv(pension)-total_debt)
  data$bznval = data$b3502*anl.dfr + data$b3501*anl.dfr # net value of business
  data$hhginc = data$b3201*anl.dfr   # total 1982 household income (gross i.e. pre-tax)
  data$wageinc = data$b3205*anl.dfr
  data$bosstp = data$b4540   # type of employer; slef-employed=8.
  data$homval = data$b3708*anl.dfr   # current value of home
  data$totfin = data$b3303*anl.dfr   # total paper assets (including cash value of life insurance, loans owned to househol, etc.)
  data$totast = data$b3305*anl.dfr   # The sum of paper assets plus current value of home plus gross value of other properties plus total value of vehicles plus net value of businesses with and without management interest
  data$totrel = data$b3305*anl.dfr - data$b3303*anl.dfr # Business values+home value+other properties value+vehicles value
  data$jobstt = data$b4510 
  data$nhnfin = (data$totrel - data$homval)*anl.dfr
  
  data$totdbt = data$b3318*anl.dfr + data$b3319*anl.dfr # total real estate debt and total consumer debt
  data$reldbt = data$b3318*anl.dfr   # real estate debt
  data$condbt = data$b3319*anl.dfr   # consumer debt
  data$vehicl = data$b3902*anl.dfr   # total value of vehicles
  
  if(flag.subset.positive.home.value==1){
    data = subset(data,homval>0) # only 1337 out of 4262 household has positive home value. 126 out of 4262 households' home value is NA.
  }    
  data$hom = data$homval
  data$dur = data$nhnfin - data$bznval # NFIN = (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN - HOUSES) - BUS
  data$fin = data$netwrt - data$homval
  
  data$gfn = data$totfin
  data$net = data$netwrt 
  data$inc = data$hhginc   
  
  #data=data[,-(grep("\\d",colnames(data)))] # remove all the original columns (those followed by numeric numbers)
  
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data")
  write.csv(data,file=paste("subset_",the_year,".csv",sep=""),row.names = FALSE)
}

# ------ year 1986 ------------------------------------------------
# strongly recommended that households with heads aged less than 25 in 1986 be deleted.
if(baseyear<=1986){
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\editd_data")
  data = read.dta("scf86b.dta") 
  the_year = 1986
  anl.dfr=dfr$value[dfr$year==the_year]
  # there is no wage income in survey 1986
  data$year   = the_year
  data$weight = data$c1014 # data$c1014/5 # FRB 1986 weight #2
  data$numppl = data$c1101   # total number of persons in household (primary family)
  data$hhhage = data$c1113   # age of head by date of birth
  data$homnet = data$c1515*anl.dfr   # net equity in home minus mortgage (can be negative)
  data$finnet = data$c1445*anl.dfr - data$c1455*anl.dfr # financial net worth: total paper assets minus total debt
  data$netwrt = data$c1457*anl.dfr   # net worth (excluding pensions)
  data$bznval = data$c1419*anl.dfr   # net value (asset) of business
  data$hhginc = data$c1301*anl.dfr   # total 1985 household income
  data$bosstp = data$c1810   # type of employer; slef-employed=6.
  data$homval = data$c1512*anl.dfr  # current value of home
  data$totfin = data$c1445*anl.dfr   # total paper assets
  data$totrel = data$c1447*anl.dfr   # total real assets (including home, businesses, vehicles, and so on)
  data$totast = data$c1449*anl.dfr   # total assets
  data$jobstt = data$c1637 
  data$nhnfin = (data$totrel - data$homval)*anl.dfr
  
  data$totdbt = data$c1455*anl.dfr
  data$reldbt = data$c1451*anl.dfr
  data$condbt = data$c1453*anl.dfr
  data$vehicl = data$c1421*anl.dfr
  
  if(flag.subset.positive.home.value==1){
    data = subset(data,homval>0)  
  }   
  data$hom = data$homval
  data$dur = data$nhnfin - data$bznval # NFIN = (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN - HOUSES) - BUS
  data$fin = data$netwrt - data$homval
  
  data$gfn = data$totfin
  data$net = data$netwrt 
  data$inc = data$hhginc   
  
  data=subset(data,hhhage>=25) #
  data$index=NULL
  data_rev=data[,-(grep("\\d",colnames(data)))] # remove all the original columns (with numeric numbers)
  
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data")
  write.csv(data_rev,file=paste("subset_",the_year,".csv",sep=""),row.names = FALSE)
}

## ------ year 1989-2013 ---------------------------------------------

year4to2<-function(x){
  y = x-floor(x/100)*100
  return(y)
}

library(stringr) # padding zero in string

varlist = c( # variables to be kept in the worspace
  "wgt",
  "age",
  "homeeq",
  "fin",
  "debt",
  "networth",
  "bus",
  "income",
  "houses",
  "asset",
  "nfin",
  "mrthel",
  "resdbt",
  "othloc",
  "ccbal",
  "install",
  "odebt",
  "vehic",
  "wageinc",
  "saving",
  "nhnfin"
)

if(baseyear<=1989){
  syear = 1989
}else{
  syear = baseyear
}

if(lastyear>=2013){
  eyear = 2013
}else{
  eyear = lastyear
}

for(i in seq(syear,eyear,3)){
  the_year = i
  # survey of 1989-2013 is already denoted in 2013 dollars in the original data set from SCF.
  anl.dfr = dfr$value[dfr$year==the_year]*(aulcpi$value[aulcpi$year==the_year]/aulcpi$value[aulcpi$year==2013]) # double checked.
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\summary_data")
  ds = read.dta(paste("rscfp",the_year,".dta",sep=""))
  
  the_year2 = year4to2(the_year)
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\full_data")
  df = read.dta(paste("p",str_pad(the_year2,2,pad="0"),"i6.dta",sep=""))
  
  if(i==1989){
    ds = subset(ds,select=c("X1",varlist))
    df = subset(df,select=c(X1,X101,X4106,X4100)) # X4106 boss type, X4100 job category
    d  = merge(df,ds,"X1")
    d$X1=NULL
  }else{
    ds = subset(ds,select=c("Y1",varlist))
    df = subset(df,select=c(Y1,X101,X4106,X4100))     
    d  = merge(df,ds,"Y1")
  }
  
  d$year = the_year  
  # generate missing variables
  d$reldbt = (d$mrthel+d$resdbt+d$othloc)*anl.dfr
  d$condbt = (d$ccbal+d$install+d$odebt)*anl.dfr
  d$finnet = (d$fin - d$debt)*anl.dfr
  
  # rename
  names(d)[names(d)=="X101"] ="numppl"
  names(d)[names(d)=="X4106"]="bosstp"
  names(d)[names(d)=="X4100"]="jobstt"  
  names(d)[names(d)=="wgt"] ="weight"
  names(d)[names(d)=="age"] ="hhhage"
  names(d)[names(d)=="homeeq"]   ="homnet"
  names(d)[names(d)=="networth"] ="netwrt"
  names(d)[names(d)=="bus"]    ="bznval"
  names(d)[names(d)=="income"] ="hhginc"
  names(d)[names(d)=="houses"] ="homval"
  names(d)[names(d)=="fin"]    ="totfin"
  names(d)[names(d)=="asset"]  ="totast"
  names(d)[names(d)=="nfin"]   ="totrel"
  names(d)[names(d)=="debt"] ="totdbt" 
  names(d)[names(d)=="vehic"]="vehicl"
  names(d)[names(d)=="wageinc"]="wagein"
  
  d$homnet = d$homnet*anl.dfr
  d$netwrt = d$netwrt*anl.dfr
  d$bznval = d$bznval*anl.dfr
  d$hhginc = d$hhginc*anl.dfr
  d$homval = d$homval*anl.dfr
  d$totfin = d$totfin*anl.dfr
  d$totast = d$totast*anl.dfr
  d$totrel = d$totrel*anl.dfr
  d$totdbt = d$totdbt*anl.dfr
  d$vehicl = d$vehicl*anl.dfr
  d$nhnfin = d$nhnfin*anl.dfr
  d$wagein = d$wagein*anl.dfr
  d$saving = d$saving*anl.dfr
  
  # model variable (just following Yang's definition)
  # ds$dur = ds$vehicl   
  if(flag.subset.positive.home.value==1){
    d = subset(d,homval>0) # remember to uncomment when used for quantitative model targets.  #<<<< very important. 2016-10-29
  }
  
  d$hom = d$homval
  d$dur = d$nhnfin - d$bznval # NFIN=VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN - (HOUSES + BUS)
  d$fin = d$netwrt - d$homval
  
  d$gfn = d$totfin
  d$net = d$netwrt 
  d$inc = d$hhginc   
  
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data")
  write.csv(d,file=paste("subset_",the_year,".csv",sep=""),row.names = FALSE)  
  message(paste("subset_",the_year,".csv"))
}

# cohort generating ========================================================

library(plyr) # ddply
library(reshape2)
library(Hmisc) # wtd...

for(i in seq(baseyear,lastyear,3)){
  the_year = i
  setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data")
  ds = read.csv(paste("subset_",the_year,".csv",sep=""))
  ds = ds[complete.cases(ds),] # trick to remove observations having NA
  
  # equivalence scale
  ds$divider = 1
  ds$divider[ds$numppl==2] = 1.1
  ds$divider[ds$numppl==3] = 1.2   
  ds$divider[ds$numppl==4] = 1.3   
  ds$divider[ds$numppl==5] = 1.4   
  ds$divider[ds$numppl==6] = 1.5   
  ds$divider[ds$numppl==7] = 1.6   
  ds$divider[ds$numppl==8] = 1.7   
  ds$divider[ds$numppl==9] = 1.8   
  ds$divider[ds$numppl==10] = 1.9    
  ds$divider[ds$numppl==11] = 2    
  ds$divider[ds$numppl==12] = 2.1
  ds$eqh = ds$homval/ds$divider
  
  # identify small business owner
  ds$smlbuz = 0
  if(the_year==1983){
    ds$smlbuz[ds$bosstp==8] = 1
  }else if(the_year==1986){
    ds$smlbuz[ds$bosstp==6] = 1
  }else {
    ds$smlbuz[ds$bosstp==2] = 1 # see codebk2013.txt for work status categories for head (line 451). Sefl-employed and partnership.
  }  
  
  write.csv(ds,file=paste("III_subset_",the_year,".csv",sep=""),row.names = FALSE) # contain "smlbuz"
  
  # Only keep those whoe are older than (lbdage+shift) where shift is determined by the number of years past by after the oldest survey.
  shift = the_year - baseyear
  ds = subset(ds,hhhage>lbdage+shift) # hhhage: household head's age
  
  # --- kernel smoothing prep --- AGE GROUp has to be increasing. [[nees correction.]]
  # age grouping
  ds$agegrp = 0
  for(j in seq(num.bins,1,-1)){ # assign new value to elements whose value is satisfied with the criteria 
    ds$agegrp[ds$hhhage<=j*length.bin+lbdage+shift] = (j*length.bin+lbdage+shift)-2 # midpoint of interval [20,24] is 22, etc.   
  }
  
  # method 1: only keep observations younger than num.bins*length.bin+lbdage+shift. 
  ds=subset(ds,hhhage<=num.bins*length.bin+lbdage+shift)
  
  write.csv(ds,file=paste("II_subset_",the_year,".csv",sep=""),row.names = FALSE) 
  
  varlist = c(
    "eqh",
    "hom",
    "dur",
    "fin",
    "gfn",
    "net"
  )
  
  for(j in 1:6){ # the number of indicators
    # subgroup 
    s.mean = ddply(ds,.(agegrp,smlbuz),function(d)data.frame(smean=weighted.mean(d[[varlist[j]]],d$weight)))
    
    # it works: s[paste("age","grp",sep="")]
    s.qn75 = ddply(ds,.(agegrp,smlbuz),function(d)data.frame(sqn75=wtd.quantile(d[[varlist[j]]],weights=d$weight,probs=0.75)))     
    s.qn50 = ddply(ds,.(agegrp,smlbuz),function(d)data.frame(sqn50=wtd.quantile(d[[varlist[j]]],weights=d$weight,probs=0.50)))
    s.qn25 = ddply(ds,.(agegrp,smlbuz),function(d)data.frame(sqn25=wtd.quantile(d[[varlist[j]]],weights=d$weight,probs=0.25)))      
    
    s.mean = s.mean[order(s.mean$smlbuz,s.mean$agegrp),]
    s.qn75 = s.qn75[order(s.qn75$smlbuz,s.qn75$agegrp),]
    s.qn50 = s.qn50[order(s.qn50$smlbuz,s.qn50$agegrp),]
    s.qn25 = s.qn25[order(s.qn25$smlbuz,s.qn25$agegrp),]
    
    s = c(s.mean,s.qn75,s.qn50,s.qn25) # concatenate objects as well as remove duplicates.
    s = data.frame(s)
    
    s=s[,-(grep("^agegrp.",colnames(s)))] # trick to remove column starting with a certain string
    s=s[,-(grep("^smlbuz.",colnames(s)))] 
    
    names(s)[names(s)=="x"]="mean"
    names(s)[names(s)=="x.1"]="qt75"
    names(s)[names(s)=="x.2"]="qt50"
    names(s)[names(s)=="x.3"]="qt25"
    
    # save the cleaned data set
    setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Cross section")
    write.csv(s,file=paste(the_year,"enw",varlist[j],".csv",sep=""),row.names = FALSE)
    message(paste(the_year,"enw",varlist[j],".csv",sep=""))
    
    # # plot (age profile without controlling for potential problems) -------
    # png(paste(the_year,"enw",varlist[j],".png",sep=""))
    # xrange=range(s$agegrp)
    # yrange=range(s[,2:5]) # we have four types of statistics: mean, quantiles 25, 50 and 75.
    # plot(xrange,yrange,type="n",xlab="Age group",ylab=paste(varlist[j]," in ",dolyr," U.S. dollars",sep=""))
    # colors=rep(rainbow(4),each=2)
    # linetype=rep(c(1,2),4)
    # plotchar=rep(c(2,16),4)
    # for(k in 1:4){
    #   lines(s$agegrp[s$smlbuz==0],s[s$smlbuz==0,k+2],type="b",lwd=1.5,lty=linetype[(k-1)*2+1],col=colors[(k-1)*2+1],pch=plotchar[(k-1)*2+1])
    #   lines(s$agegrp[s$smlbuz==1],s[s$smlbuz==1,k+2],type="b",lwd=1.5,lty=linetype[(k-1)*2+2],col=colors[(k-1)*2+2],pch=plotchar[(k-1)*2+2])
    # }  
    # 
    # legend(xrange[1],yrange[2],c("mean","mean","qn75","qn75","qn50","qn50","qn25","qn25"),cex=0.8,col=colors,pch=plotchar,lty=linetype,title="home")
    # mtext(paste(the_year,"Survey of Consumer Finance",sep=" "))
    # dev.off()    
    
    # whole group ---------------------------------------------------------------
    # Trick 1.
    # s.mean = ddply(ds,.(agegrp),function(d)smean=weighted.mean(d$eqh,d$weight))1    
    # Trick 2.
    
    s.mean = ddply(ds,.(agegrp),function(d)data.frame(smean=weighted.mean(d[[varlist[j]]],d$weight)))
    agegrp=s.mean$agegrp
    
    # it works: s[paste("age","grp",sep="")]
    s.qn75 = ddply(ds,.(agegrp),function(d)data.frame(sqn75=wtd.quantile(d[[varlist[j]]],weights=d$weight,probs=0.75)))     
    s.qn50 = ddply(ds,.(agegrp),function(d)data.frame(sqn50=wtd.quantile(d[[varlist[j]]],weights=d$weight,probs=0.50)))
    s.qn25 = ddply(ds,.(agegrp),function(d)data.frame(sqn25=wtd.quantile(d[[varlist[j]]],weights=d$weight,probs=0.25)))    
    
    s = c(s.mean,s.qn75,s.qn50,s.qn25)
    s = data.frame(s)
    
    s=s[,-(grep("^agegrp",colnames(s)))] 
    s=cbind(agegrp,s)    
    names(s)[names(s)=="Group.1"]="agegrp"
    names(s)[names(s)=="x"]="mean"
    names(s)[names(s)=="x.1"]="qt75"
    names(s)[names(s)=="x.2"]="qt50"
    names(s)[names(s)=="x.3"]="qt25"
    
    # save
    setwd("C:\\Users\\libin\\Desktop\\final_Yang\\data\\clean_data\\Cross section")    
    write.csv(s,file=paste(the_year,"com",varlist[j],".csv",sep=""),row.names = FALSE)
    message(paste(the_year,"com",varlist[j],".csv",sep=""))
    
    # # plot
    # png(paste(the_year,"com",varlist[j],".png",sep=""))
    # xrange=range(s$agegrp)
    # yrange=range(s[,2:5]) # we have four types of statistics: mean, quantiles 25, 50 and 75.
    # plot(xrange,yrange,type="n",xlab="Age group",ylab=paste(varlist[j]," in ",dolyr," U.S. dollars",sep=""))
    # colors=rainbow(4)
    # linetype=c(1:4)
    # plotchar=seq(18,18+4,1)
    # for(k in 1:4){
    #   lines(s$agegrp,s[,k+1],type="b",lwd=1.5,lty=linetype[k],col=colors[k],pch=plotchar[k])
    # }  
    # legend(xrange[1],yrange[2],c("mean","qn75","qn50","qn25"),cex=0.8,col=colors,pch=plotchar,lty=linetype,title="home")
    # mtext(paste(the_year,"Survey of Consumer Finance",sep=" "))
    # dev.off()
  } # end of indicator loop
} # end of survey year loop
