library(textreadr)
library(ggplot2)
library(tm)
library(stringr)
library(ranger)
library(reshape)
library(zoo)


all_data_split = read.csv('/users/sweiss/downloads/russian_ads2.csv')
ad_meta_data_headers = c(': ', 'Age: ',"Language","Placements","People Who Match", "   ")
all_data_split_meta_data = lapply(all_data_split[,'Ad Targeting Location'], function(y) 
  sapply(1:(length(ad_meta_data_headers)-1), function(x) gsub(paste0(".*",ad_meta_data_headers[x] ,"(.+)",ad_meta_data_headers[x+1],".*"), "\\1", y) ) )
all_data_split_meta_data = do.call(rbind,all_data_split_meta_data)



split_text = as.character(str_replace_all(tolower(paste(as.character(all_data_split[,'Ad.Text']), all_data_split[,'Ad.Targeting.Location'])) , "[^[:alnum:]]", " ") )
split_text = gsub("[^0-9A-Za-z///' ]","'" , split_text ,ignore.case = TRUE)
corpus_text <- Corpus(VectorSource(tolower(paste(as.character(all_data_split[,'Ad.Text']), all_data_split[,'Ad.Targeting.Location'])) ))
tdm_text = (((DocumentTermMatrix(corpus_text))))
tdm_text = as.matrix(tdm_text)
tdm_text = tdm_text[,which(colSums(tdm_text)>10)]
numeric_date = as.numeric(all_data_split[,'date'])

tdm_text = as.data.frame(tdm_text)
colnames(tdm_text) = paste0('x',colnames(tdm_text))
colnames(tdm_text)= gsub('\-','1',colnames(tdm_text))

as.Date(all_data_split[,'date'])>as.Date('2016-7-31')
cut(as.numeric(as.Date(all_data_split[,'date'])), breaks = c(as.numeric(as.Date('2016-5-01')), as.numeric(as.Date('2016-7-31'))))

new_response = rep(0, nrow(all_data_split))
new_response[which(as.Date(all_data_split[,'date'])>as.Date('2016-7-31'))] = 1
rf = ranger(new_response ~ . , data = tdm_text[,-grep('\\—',(colnames(tdm_text)))], importance = 'impurity')

rf$variable.importance[order(-rf$variable.importance)][1:10]

return_grep = function(x){
  temp = rep(0, length(new_response))
  temp[grep(x,tolower(paste(all_data_split[,"Ad.Text"],all_data_split[,"Ad.Targeting.Location"])))] = 1
  return(temp)
}



important_words = c('living',
                    '1954',
                    'civil rights',
                    'martin luther king',
                    'malcolm',
                    'panther')

important_words_df = data.frame(do.call(cbind,lapply(important_words, return_grep)))
colnames(important_words_df) = important_words
important_words_df[,'Date'] = all_data_split[,'date']


#strftime(date, '%V')
important_words_df_agg_day = aggregate(.~Date, data = important_words_df, FUN = sum)
total_adverts_day = aggregate(rep(1,nrow(important_words_df))~Date, data = important_words_df, FUN = sum)
colnames(total_adverts_day) = c("Date","Number_Ads")

ads_time = ggplot(subset(total_adverts_day, as.Date(Date)> as.Date('2016-01-01') & as.Date(Date)< as.Date('2017-01-01')), aes(x = as.Date(Date), y = Number_Ads))+
  geom_point() + geom_smooth(se = FALSE) + theme_minimal() + ggtitle("Number of Russian Facebook Advertisements by Day")+
  ylab('Number of Facebook Ads Purchased by Russia') + xlab("Date (Day)")

important_words_df_agg_day[,-1] = important_words_df_agg_day[,-1] / total_adverts_day[,2]
important_words_df_agg_day = important_words_df_agg_day[-1,]
melt_important_words_df_agg_day = melt(important_words_df_agg_day, 'Date')

ggplot(melt_important_words_df_agg_day, aes(x = as.Date(Date), y = value))+geom_point()+
  facet_grid(variable~.)+geom_smooth(se = FALSE)+
  geom_vline(xintercept=as.Date('2016-4-11'), colour = 'blue')+geom_vline(xintercept=as.Date('2016-8-02') ,colour = 'red')+theme_minimal()+
  ggtitle("Selected Word Percentages by Day") + xlab("Date (Day)")

