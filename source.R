### TOPIC 1. 


suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(ggplot2)
  
})

###################################################################################################
### Data import, manipulation
###################################################################################################

demo <- fread("Be the L.BA 주제1/1-1. Demo.txt")
order <- fread("Be the L.BA 주제1/1-2. 구매내역정보.txt")

order %>% select(ID) %>% unique()

merged <- merge(demo, order, by = "ID")

# name of variables should be 'numeric'
numeric_var <- c("ID", "성별", "연령", "구매시점", "구매시간", "구매요일", "구매건수", "구매금액",
  "취소여부", "평균기온")

for ( i in 1:length(numeric_var) ) {
  
  merged[[numeric_var[i]]] <- as.numeric(merged[[numeric_var[i]]])  
  
}


### 카테고리, 구매요일 제거



merged %>% filter(카테고리 == "감자스낵") -> merged_potato

merged_potato %>% group_by(구매시점, 상품구분) %>% 
  summarise( n = n() ) %>% arrange(구매시점, 상품구분) %>% filter(상품구분 == "상품D") -> df

ggplot(df, aes(x = 구매시점, y = n)) +
  geom_point()

