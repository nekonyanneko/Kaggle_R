library(dplyr)

library(readr)
data <- read_csv2(file = "./../Kaggle_data/titanic/train.csv")  #わざと
data %>% print()
data %>% class() %>% print()
# 通常はread_csv

#install.packages('tidyverse')  #stringrの利用　https://heavywatal.github.io/rstats/stringr.html
library(stringr)
all_pattern     <- c(',','"')  # 変換対象文字
all_replacement <- c(' ','')   # 変換後文字
all_counter     <- length(all_pattern)
tmp_Num <- 0
for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    data[i,j] <- data[i,j] %>% str_to_lower()  # 小文字に修正
    # 全てのマッチを変換
    for(z in 1:all_counter){
      data[i,j] <- data[i,j] %>% str_replace_all(all_pattern[z], all_replacement[z])
    }
    # splitで分割した後の文字を列として結合する
    tmp_c <- unlist(data[i,j] %>% str_split(" "))
    if(tmp_Num < length(tmp_c)){
      tmp_Num <- length(tmp_c)
    }
  }
}

# 分割した新しいデータを作成
new_data <- data.frame(matrix(rep(NA,tmp_Num),nrow=1))[-1,]
# new_data %>% print()
# for(i in 1:nrow(data)){
#   for(j in 1:ncol(data)){
#     tmp_c <- unlist(data[i,j] %>% str_split(" "))
#     new_data <- rbind(new_data,c(tmp_c,rep(NA,tmp_Num-length(tmp_c)))) %>% print()
#   }
# }

#色々クレンジングしたらMeCabでもいいかも
#もしくはカンマ区切りでcsvで出力とか

