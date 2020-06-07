rm(list=ls())

library(data.table)
library(plotly)
library(magrittr)
library(bit64)
library(readxl)
library(purrr)
name_as = function(x) {names(x) = x;x}

#population data
popurl = "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv"
popFile = "./data/populations.zip"
download.file(popurl,popFile)
unzip(popFile,exdir = "./data/popdata")

popurl = "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel"
popFile = "./data/populations.xls"
download.file(popurl,popFile)
pop = readxl::read_xls(popFile,skip = 3) %>% as.data.frame
pop$latest = vapply(seq_len(nrow(pop)),function(i)  {
  x =pop[i,(ncol(pop)-(25:0))] %>% unlist %>% na.omit %>% tail(1)
  if(!length(x)) x = NA
  x
},0)
pop = data.table(pop,key="Country Name")


#corona data
urlName = "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv"
fileName = "./data/countriesAggregated.csv"
download.file(urlName,fileName)
dt = fread(fileName)
dt$pop = dt[,pop[Country]$latest]
setkey(dt,"Country")

dt[Country=="US",]$pop=308745538
dt$DPM = dt[,Deaths/pop*1E6]
CN = dt$Country %>% name_as

cn = CN[c("Brazil","Denmark","Colombia","US","Italy","Spain","Sweden","France")]

cn = CN[c("Brazil","Denmark","US","Italy","Sweden","France")]

fig = plot_ly(
  dt[Country==cn[1],],
  x=~tail(Date,-1),y=~diff(DPM) %>% frollmean(7),
  mode="markers",
  type="scatter",
  name=cn[1]
) %>% layout(
  yaxis = list(type = "log",title="daily deaths/million"),
  xaxis = list(title = "date")
) 
for(i in cn[-1]) fig %<>% add_trace(data = dt[Country==i,],name=i) 
fig


for(i in cn) {
  localmax=0
  nrecord=0
  sapply(dt[i,diff(DPM)],function(x) if(x>localmax) {
    nrecord <<- nrecord + 1
    localmax <<- x

  })
  cat(i," ",nrecord,"\n")
}


flist = list()
for(i in list.files("./data/dktemp/",full.names = TRUE)) {
  flist[[length(flist)+1]] = fread(i)
}
dktemp = do.call(rbind,flist) %>% data.table
str(dktemp)
setorder(dktemp,"DateTime")
localmax=0
nrecord=0
sapply(dktemp$Højeste,function(x) if(x>localmax) {
  nrecord <<- nrecord + 1
  localmax <<- x
})
nrecord
View(dktemp)


