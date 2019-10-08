library(textreadr)

months = list.files(directory)

get_ads_by_month = function(date){
  all_files = list.files(paste0(directory, date))
  data = lapply(all_files, function(x) c(read_pdf(paste0(directory,date,'/',x))[,'text'] ,paste0(directory,date,'/',x) ) )
  data = lapply(data, function(x) data.frame(meta_data = x[1], ad = x[2], data_loc = x[3]))
  return(data)
}
russian_ads = lapply(months, get_ads_by_month)


all_data = do.call(rbind,lapply(russian_ads, function(x) do.call(rbind,x)))
all_data[,1] = as.character(all_data[,1])
all_data[,2] = as.character(all_data[,2])
all_data[,1] = gsub('\n',' ',all_data[,1])
all_data[,2] = gsub('\n',' ',all_data[,2])


headers = c("Ad ID", "Ad Text","Ad Landing Page", "Ad Targeting Location",
            "Ad Impressions", "Ad Clicks", "Ad Spend", "Ad Creation Date","Redactions Completed")

all_data_split = lapply(all_data[,1], function(y) 
  sapply(1:(length(headers)-1), function(x) gsub(paste0(".*",headers[x] ,"(.+)",headers[x+1],".*"), "\\1", y)) )

all_data_split = do.call(rbind,all_data_split)
colnames(all_data_split) = headers[1:8]

all_data_split = as.data.frame(all_data_split)

all_data_split[,'date'] = substr(all_data_split[,'Ad Creation Date'], 1,10)
all_data_split[,'date'] = gsub(' ', '',all_data_split[,'date'])
all_data_split[,'date'] = as.Date(all_data_split[,'date'], "%m/%d/%y")

all_data_split[,'numeric_date'] = as.numeric(all_data_split[,'date'])
all_data_split = all_data_split[-which(is.na(all_data_split[,'date'])),]
write.csv(all_data_split, paste0(directory,'/russian_ads2.csv'))
