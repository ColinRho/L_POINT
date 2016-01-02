### TOPIC 2.

## 분석목적 : 구매행태 데이터 기반으로 실버세대의 라이프스타일을 
##  이해할 수 있는 특징을 분석 및 추정하고 이와 관련된 마케팅 기획을 제시 
##  (예시) 실버세대 고객 세분화, 여가 소비분석을 통한 구매 활성화 방안 등 

## 평가기준
# - 구조화(30) : 분석 결과물에 대한 명확한 표현 및 도식화
# - 도식화(30) : 분석 설계에 따른 스토리라인
# - 통계지표 활용(30) : 다양한 통계지수 사용
# - 프레젠테이션(10) : 효과적인 의사전달 

###################################################################################################
### Preliminaries
###################################################################################################

suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(ggplot2)
  
})

###################################################################################################
### Data import, manipulation, detect outliers
###################################################################################################

## two sets of data and merged data
demo <- fread("TOPIC2/1-1. Demo.txt")
order <- fread("TOPIC2//1-2. 구매내역정보.txt")


## variable type modification 
demo %<>% arrange(ID)
demo$성별 %<>% as.numeric(.)
demo$연령 %<>% as.integer(.)

# variables should be treated as 'numeric'
numeric_var <- c("구매시간", "구매수량", "구매금액", "취소여부")

for ( i in 1:length(numeric_var) ) {
  
  order[[numeric_var[i]]] <- as.numeric(order[[numeric_var[i]]])  
  
}

# date coding
order$구매일자 %<>% as.Date(., format = "%Y%m%d")

# let product categories be factor
order$상품대분류명 %<>% factor(., levels = sort(unique(.)) )
order$상품중분류명 %<>% factor(., levels = sort(unique(.)) )


## extract product category
order %>% select(상품대분류명, 상품중분류명) %>% table %>% 
  apply(., 2, function(x) {
    names(x[x != 0])
  }) -> categories

s.cat <- names(categories)
l.cat <- unique(categories)

<<<<<<< HEAD
cat.list <- list()
for ( i in 1:length(l.cat) ) {
  cat.list[[l.cat[i]]] <- names(categories[categories == l.cat[i]])
}
=======
>>>>>>> 3115152e72f692ce0fb4f2ed54136d13a07787f5

## merged data set (just in case)
merged <- merge(demo, order, by = "ID")

###################################################################################################
<<<<<<< HEAD
### Analysis 0. Pre-step
=======
### Analysis 0. Preliminaries
>>>>>>> 3115152e72f692ce0fb4f2ed54136d13a07787f5
###################################################################################################

## extend 'demo' with purchase info

demo <- data.frame(demo)
demo1 <- data.table(demo) ; demo2 <- data.table(demo)
demo <- data.table(demo)

# by '상품중분류명'
order %>% group_by(ID, 상품중분류명) %>% summarise( amount = sum(구매금액) ) %>%
  arrange(ID) -> by_product

for (k in 1:length(s.cat)) {
  
  foo <- by_product %>% filter(상품중분류명 == s.cat[k])
  demo1[ID %in% foo$ID, s.cat[k] := foo$amount]
  
}

demo1[is.na(demo1)] <- 0


# by '상품대분류명'
order %>% group_by(ID, 상품대분류명) %>% summarise( amount = sum(구매금액) ) %>%
  arrange(ID) -> by_category

for (k in 1:length(l.cat)) {
  
  foo <- by_category %>% filter(상품대분류명 == l.cat[k])
  demo2[ID %in% foo$ID, l.cat[k] := foo$amount]
  
}

demo2[is.na(demo2)] <- 0


###################################################################################################
### Analysis 1. PCA
###################################################################################################

<<<<<<< HEAD
## try to group product categories

num.demo <- demo1 %>% select(-(ID:거주지역))

demo.pc1 <- prcomp(x = num.demo, center = TRUE, scale. = TRUE)

num.demo <- demo2 %>% select(-(ID:거주지역))
=======

num.demo <- demo1 %>% select(-ID, -거주지역)

demo.pc1 <- prcomp(x = num.demo, center = TRUE, scale. = TRUE)

num.demo <- demo2 %>% select(-ID, -거주지역)
>>>>>>> 3115152e72f692ce0fb4f2ed54136d13a07787f5

demo.pc2 <- prcomp(x = num.demo, center = TRUE, scale. = TRUE)

###################################################################################################
### Analysis 2. k-means 
###################################################################################################


library(cluster)


fit <- kmeans(num.demo, 3)
<<<<<<< HEAD



=======
>>>>>>> 3115152e72f692ce0fb4f2ed54136d13a07787f5
