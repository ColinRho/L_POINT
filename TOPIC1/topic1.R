### TOPIC 1. 

## 분석목적: 상품 출시 후 반응 진단 및 향후 매출 예측

## 평가기준
# - 모델 적합성(45) : 분석 데이터 가공, 통계적 적용에 따른 최적의 방법론
# - 독창성(20) : 분석 방법의 다양성 및 창의성
# - 결과활용도(25) : 분석 결과 해석의 적합성 및 활용성
# - 프레젠테이션(10) : 명확한 표현 및 효과적인 의사전달 

suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(ggplot2)
  
})

###################################################################################################
### Data import, manipulation, detect outliers
###################################################################################################

# two sets of data and merged data
demo <- fread("TOPIC1/1-1. Demo.txt")
order <- fread("TOPIC1//1-2. 구매내역정보.txt")
merged <- merge(demo, order, by = "ID")

# variables should be treated as 'numeric'
numeric_var <- c("성별", "연령", "구매시점", "구매시간", "구매요일", "구매건수", "구매금액",
  "취소여부", "평균기온")

for ( i in 1:length(numeric_var) ) {
  
  merged[[numeric_var[i]]] <- as.numeric(merged[[numeric_var[i]]])  
  
}

# to delete 'canceled' purchases  
merged %>% select(취소여부) %>% table()
merged %<>% filter(취소여부 != 1) %>% select(-취소여부)

# choosing the category
merged %>% filter(카테고리 == "감자스낵") -> merged_potato

merged %>% select(성별) %>% table

merged %>% select(연령) %>% table

merged %>% select(구매시간) %>% table

###################################################################################################
### Modularized functions
###################################################################################################

plotter <- 
  
  # drawing time vs number of purchaser given categories
  # product
  
  function(df, product = "all", gender = "all", location = "all") {
    
    # filtering by inputs and counting a number of purchase(regardless of volume of purchase)
    # or the volume should be considered? 
    # depends on how to define the diffusion model. 
    
    if (product != "all") { 
      
      df %<>% filter(상품구분 == product)
        
    }
    
    if (gender != "all") {
      
      df %<>% filter(성별 == gender)
      
    }
    
    if (location != "all") {
      
      df %<>% filter(구매지역 == location)
      
    }
      
    df %<>% group_by(구매시점) %>% summarise(n = n()) %>% arrange(구매시점) 
    
    # plotting 
    ggplot(df, aes(x = 구매시점, y = n)) +
      geom_point() 
    
  }

plotter(df = merged_potato, product = "상품B", gender = 2, location = "부산")


merged_potato %>% filter(구매시점 == 1 & 상품구분 == "상품B") %>% group_by(구매지역) %>% 
  summarise( range(평균기온) )

nls()