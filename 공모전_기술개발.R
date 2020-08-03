  library(tidyverse)
  library(broom)
  library(knitr)
  library(ggfortify)
  library(readxl)
  library(readr)
  library(dplyr)
  library(agricolae)

"""
전체적인 흐름은 다음과 같다.
1. 데이터 불러오기 : 원자료 데이터 중에서 19년 기술통계조사 데이터와 19년 실태조사_제조업 데이터를 사용
2. 전체적인 EDA = 청년 인턴제도에 대한 인식 및 필요성 등을 파악
3. 변수 중요도 파악 및 변수 선택
4. 모델 fitting
5. 모델 검정 및 유의성 파악
6. 결론
"""

# 1. 데이터 불러오기

data_skill <- read_excel('C:/정책 공모전/자료/기술통계19.xlsx') # 19년 기술 통계조사 데이터
data_search <- read_excel('C:/정책 공모전/자료/실태조사_제조업19.xlsx') # 19년 제조업 실태조사 데이터
data_out <- read_excel('C:/정책 공모전/자료/12.2.1 성장성에 관한 지표(제1.xlsx') #외부데이터
data_out[,'x3'] <- lapply(data_out[,'x3'], as.numeric)
str(data_skill)
data_out$x3

data_skill = data_skill[1:3801,]
#제조업으로한정
data_skill <- data_skill %>% filter(x3 !=58 & x3 !=62 & x3!=63 & x3!=70 & x3!=72 & x3!=73)
length(unique(data_skill$global_id)) # 3198
length(data_skill$global_id) # 3198
# 기술통계조사는 총 3198 회사가 참여
length(unique(data_search$global_id)) #7500
length(data_search$global_id) # 7500
# 실태조사_제조업 데이터는 총 7500 회사가 참여
a = sort(unique(data_skill$global_id)) # 기술데이터 참여 회사
b = sort(unique(data_search$global_id)) # 실태조사 참여 회사
length(intersect(a,b)) #833개가 동시 응답한 회사 
#제조업으로 한정

# 2. EDA

"""
x1: 지역 x2: 규모, x3: 산업분류, I6_1 : 정부에서 지원해야할 기술인력 정책 (높을수록 저학력, 3번이 인턴)
"""
size_num = data_skill %>% filter(is.na(x2)==FALSE & is.na(I6_1)==FALSE) %>% group_by(x2) %>% select(I6_1) %>% summarise(y=n())  #총 기업 규모별 수
intern_num = data_skill %>% filter(I6_1==3) %>% group_by(x2, I6_1) %>% summarise(y=n()) 
percent = intern_num$y/size_num$y
percent # 규모에 상관없이 대부분의 기업들이 높은 비중으로 선택했다는 것을 알 수 있다(23% ~ 27%)

"""
P1: 29세 이하 연구개발직 비율, P2: 30대 비율, P3: 40대  비율, P4: 50대 비율, P5: 60대 이상 비율
"""
data_skill %>% filter(is.na(x2)==FALSE)  %>% mutate(P1=B3S1/B3S6,
                                                    P2=B3S2/B3S6,
                                                    P3=B3S3/B3S6,
                                                    P4=B3S4/B3S6,
                                                    P5=B3S5/B3S6) %>% group_by(x2) %>% summarise(p1 = mean(P1),p2 = mean(P2),p3 = mean(P3), p4 =mean(P4),p5= mean(P5)) 
# 기업 규모가 작을수록 20대 이상의 연구직 평균 비율이 작음, 40 대 비율이 가장 높음, 규모가 작을수록 60대 이상 비율이 높음

"""
산업 분류별 EDA 
10	식료품
11	음료
13	섬유제품
14	의복 및 모피제품
15	가죽, 가방 및 신발
16	목재 및 나무제품
17	펄프 및 종이제품
18	인쇄, 기록매체 복제
20	화학물질 및 화학제품
21	의료용물질 및 의약품
22	고무제품 및 플라스틱
23	비금속광물제품
24	1차금속
25	금속가공제품
26	전자, 컴퓨터, 영상, 통신장비
27	의료, 정밀, 광학기기
28	전기장비
29	기타기계 및 장비
30	자동차 및 트레일러
311	선박 및 보트건조업
312	철도장비 제조업
313	항공기, 우주선 및 부품제조업
319	그 외 기타 운송장비 제조업
32	가구
33	기타제품
34	산업용 기계 및 장비수리업
58	출판업(소프트웨어 및 개발공급업만 포함)
62	컴퓨터 프로그래밍, 시스템통합 및 관리업
63	정보서비스업
70	연구개발업
72	건축기술, 엔지니어링 및 기타 과학기술서비스업
73	기타 전문, 과학 및 기술서비스업
"""
data_skill %>% filter(is.na(x3)==FALSE & is.na(I6_1)==FALSE) %>% group_by(x3) %>% summarise(n=n()) %>% arrange(desc(n))
# 전기장비, 기타기계 및 장비, 고무제품 및 플라스틱 업종의 비율이 가장 높음

"""
x1: 지역 x2: 규모, x3: 산업분류, I6_1 : 정부에서 지원해야할 기술인력 정책 (높을수록 저학력, 3번이 인턴)
"""
size_num1 = data_skill %>% filter(is.na(x3)==FALSE) %>% group_by(x3) %>% select(I6_1) %>% summarise(y=n())  #총 기업 규모별 수
size_num1  
intern_num1 = data_skill %>% filter(I6_1==3) %>% group_by(x3, I6_1) %>% summarise(y=n()) 
percent1 = intern_num1$y/size_num1$y
percent1 # 업종별로 비중이 매우 다르다는 것을 볼 수 있다. 따라서 대학 산학연계에 있어서 업종별로 다른 접근은 유의미해 보임
size_num1$x3[which.min(percent1)] # 인쇄, 기록매체 복제 업종의 선택 비율이 제일 낮음
size_num1$x3[which.max(percent1)] # 전기정비가 가장 높음
# 식료품, 인쇄 등의 업종에서보다 전기, 기기 쪽의 인턴 선택 비율이 가장 높음을 알 수 있다.

"""
P1: 29세 이하 연구개발직 비율, P2: 30대 비율, P3: 40대  비율, P4: 50대 비율, P5: 60대 이상 비율
"""
type_percent = data_skill %>% filter(is.na(x3)==FALSE)  %>% mutate(P1=B3S1/B3S6,
                                                                   P2=B3S2/B3S6,
                                                                   P3=B3S3/B3S6,
                                                                   P4=B3S4/B3S6,
                                                                   P5=B3S5/B3S6) %>% group_by(x3) %>% 
  summarise(p1 = mean(P1),p2 = mean(P2),p3 = mean(P3), p4 =mean(P4),p5= mean(P5)) %>% select(x3, p1) %>% arrange(p1)
# 업종별로 29세 이하 연구개발직 비율
type_percent
mean(type_percent$p1) # 평균 비율은 0.06
type_percent[type_percent$x3==28,] # 전기정비에서의 비율은 평균보다 높음 
type_percent[type_percent$x3==311,] # 선박 및 보트건조업 업종에서는 비율이 매우 낮음 실제로
type_percent$x3[which.max(type_percent$p1)] #의료용물질 및 의약품에서의 비율이 가장 높음
type_percent$x3[which.min(type_percent$p1)] #선박 및 보트건조업에서 비율이 가장 낮음

#시각화

label=c("전문, 과학 및 기술서비스업","정보통신업","제조업")
type_percent %>% 
  ggplot(aes(factor(x3),p1)) + 
  geom_bar(stat='identity') +
  geom_hline(yintercept=mean(type_percent$p1), linetype='dashed', color= 'red')+
  labs(title = "업종별 29세 이하 연구개발직 비율",
       x = "세부업종코드",
       y = "29세 이하 연구개발직비율") +
  scale_fill_discrete(name="업종분류",labels=label)
  
"""
10	식료품
11	음료
13	섬유제품
14	의복 및 모피제품
15	가죽, 가방 및 신발
16	목재 및 나무제품
17	펄프 및 종이제품
18	인쇄, 기록매체 복제
20	화학물질 및 화학제품
21	의료용물질 및 의약품
22	고무제품 및 플라스틱
23	비금속광물제품
24	1차금속
25	금속가공제품
26	전자, 컴퓨터, 영상, 통신장비
27	의료, 정밀, 광학기기
28	전기장비
29	기타기계 및 장비
30	자동차 및 트레일러
311	선박 및 보트건조업
312	철도장비 제조업
313	항공기, 우주선 및 부품제조업
319	그 외 기타 운송장비 제조업
32	가구
33	기타제품
34	산업용 기계 및 장비수리업

"""





"""
기술데이터 EDA 결과
<규모별>
1. 정부에서 지원해야할 기술인력 정책에서 대학생 인턴 비율이 전 업종별로 높게 나옴을 알 수 있다. 평균적으로 1/4의 기업이 선택
2. 다만 규모가 클수록 비중이 높은 것을 알 수 있다.
3. 또한 29세 이하 연구개발직 비율에서 규모별로 다른데, 규모가 클수록 29세 이하 연구개발직 비율이 높음을 알 수 있다. (큰 차이는 X)
4. 규모가 작은 기업일 수록 대학생 인턴의 형식보다는 실질적인 경력자, 숙련된 기술개발 인력 지원이 필요해 보임을 알 수 있다.

<업종별>
1. 규모의 경우와 달리, 정부에서 지원해야할 기술인력 정책에서 대학생 인턴 선택 비율이 매우 상이함을 알 수 있다.
2. 따라서 업종별로 산학연계에 있어서 업종별로 다른 접근은 유의미해 보임
3. 식료품,인쇄, 기록 매체 복제 업종의 선택이 가장 낮고, 전기정비 업종에서 선택이 가장 높음
4. 29세 이하 연구개발직 비율에서도 업종별로 차이가 큰데, 정보서비스업에서 비율이 가장 높고, 선박 보트업에서 가장 낮음음 (평균은 대락 0.07)
5. 전기정비에서의 비율은 평균보다 낮은 것을 보여줘서, 이 업종에게는 많이 대학생 인턴이 많이 필요해 보임
6. 인쇄, 기록매체 복제 업종에서는 비율이 실제로 매우 낮은 것을 볼 수 있다.
"""


#3 변수 중요도 파악 및 변수 선택

library(caret)
library(randomForest)

forest_data_small= data_skill %>%filter(x2==1|x2==2) %>% select(F2S1,D3S1:D3S10N)
index_small <- createDataPartition(y = forest_data_small$F2S1, p=0.85, list=F) #불확실성을 위해 0.15 percent의 임의 데이터는 drop
train_small <- forest_data_small[index_small,] # train data set 형성
train_small[is.na(train_small)] <- 0
test_small<- forest_data_small[-index_small,]
forest_s <- randomForest(train_small$F2S1~.,data=train_small) # random forest 형성
forest_s$importance
#소규모 기업일때 제품 기획능력의 중요도가 제일 낮고,  유지보수능력이 가장 중요한 요인

forest_data_m= data_skill %>%filter(x2==3|x2==4|x2==5) %>% select(F2S1,D3S1:D3S10N)
index_m <- createDataPartition(y = forest_data_m$F2S1, p=0.85, list=F) #불확실성을 위해 0.15 percent의 임의 데이터는 drop
train_m <- forest_data_m[index_m,] # train data set 형성
train_m[is.na(train_m)] <- 0
test_m<- forest_data_m[-index_m,]
forest_m <- randomForest(train_m$F2S1~.,data=train_m) # random forest 형성
forest_m$importance
#보통 규모 기업일때 제품 기획능력의 중요도가 제일 낮고,  부품및공정설계능력이 가장 중요한 요인

forest_data_l= data_skill %>%filter(x2==6|x2==7) %>% select(F2S1,D3S1:D3S10N)
index_l <- createDataPartition(y = forest_data_l$F2S1, p=0.85, list=F) #불확실성을 위해 0.15 percent의 임의 데이터는 drop
train_l <- forest_data_l[index_l,] # train data set 형성
train_l[is.na(train_l)] <- 0
test_l<- forest_data_l[-index_l,]
forest_l <- randomForest(train_l$F2S1~.,data=train_l) # random forest 형성
forest_l$importance
#큰 규모 기업일때 기술개발사업의 중요도가 제일 낮고,  제품설계능력이 가장 중요한 요인화




#EDA
#업종별
data_skill %>% group_by(x3) %>% summarise(n.x3=n()) %>%
  ggplot(aes(factor(x3),n.x3)) + 
  geom_bar(stat='identity') +
  labs(title = "업종별 업체수",
       x = "세부업종코드",
       y = "업체수")

#규모별

data_skill %>% group_by(x2) %>% summarise(n.x2=n()) %>%
  ggplot(aes(factor(x2),n.x2)) + 
  geom_bar(stat='identity') +
  labs(title = "규모별 업체수",
       x = "규모",
       y = "업체수")


"""
기업의 규모가 작을수록 기존의 기술과 공정의 유지 및 보수능력 매출액에 가장 주요한 요인으로 나오고
기업의 규모가 클수록 제품의 유지 및 보수 능력도 중요하지만, 제품설계의 중요도도 올라갔음 
"""
"""
y :매출액 2017 매출액 : 공변량(공통)
모델 :ancova
1.통째로 fitting x는 20대 연구자 비율 + 성비 비율(연구개발직 여자 /총합)+ 기술능력 (국내) 10개 + 기술 매출액 % + 공변량(업종별 성장률) + 2017 매출액 +기술개발 인력지원 활용경험
2. x2 = 1,2 소규모 x2 = 3, 4, 5 중 x2 = 6,7 상:
  20대 연구자 비율 + 성비 비율(연구개발직 여자 /총합)+ 기술능력 (국내) 10개 + 기술 매출액 %
+기술개발 인력지원 활용경험
3.업종별 (제조업 안에서 나눈거 기준):
  20대 연구자 비율 + 성비 비율(연구개발직 여자 /총합)+ 기술능력 (국내) 10개 + 기술 매출액 %
+기술개발 인력지원 활용경험
"""




#1
#데이터 정리(기술통계)





#기준:간격 fivenum




#20대연구자비율
type_percent_tmp_1 = data_skill   %>% mutate(P1=ifelse(B3S1/B3S6>=0.8,"Very high",ifelse(B3S1/B3S6>=0.6,"High",ifelse(B3S1/B3S6>=0.4,"Normal",ifelse(B3S1/B3S6>=0.2,"Low","Very low")))))

#여성 비율
type_percent_tmp_2 = data_skill   %>% mutate(P_F=ifelse(PEOAF/PEOAT>=0.8,"Very high",ifelse(PEOAF/PEOAT>=0.6,"High",ifelse(PEOAF/PEOAT>=0.4,"Normal",ifelse(PEOAF/PEOAT>=0.2,"Low","Very low")))))

#기술개발에 의한 매출비율
type_percent_tmp_3 = data_skill    %>% mutate(F2N1_P=ifelse(F2N1>=0.8,"Very high",ifelse(F2N1>=0.6,"High",ifelse(F2N1>=0.4,"Normal",ifelse(F2N1>=0.2,"Low","Very low")))))

#기준: fivenum
options(digits=8)
f1<-fivenum(data_skill$B3S1/data_skill$B3S6,na.rm=TRUE)
f2<-fivenum(data_skill$PEOAF/data_skill$PEOAT)
f3<-fivenum(data_skill$F2N1,na.rm=TRUE)
five

#결측값:3개
data_skill %>% filter(is.na(F2N1)==TRUE)

#20대연구자비율
type_percent_tmp_11 = data_skill  %>% filter(is.na(F2N1_P)==FALSE) %>% mutate(P1=ifelse(B3S1/B3S6 > f1[4],"Very high",
                                                        ifelse(B3S1/B3S6>f1[3],"High",
                                                        ifelse(B3S1/B3S6>f1[2],"Normal",
                                                        ifelse(B3S1/B3S6>f1[1],"Low","Very low")))))
#여성비율
type_percent_tmp_22 = data_skill  %>% filter(is.na(F2N1_P)==FALSE) %>% mutate(P_F=ifelse(PEOAF/PEOAT > f2[4],"Very high",
                                                         ifelse(PEOAF/PEOAT > f2[3],"High",
                                                         ifelse(PEOAF/PEOAT > f2[2],"Normal",
                                                         ifelse(PEOAF/PEOAT>f2[1],"Low","Very low")))))

#기술개발에 의한 매출비율
type_percent_tmp_33 = data_skill %>% filter(is.na(F2N1)==FALSE) %>% mutate(F2N1_P=ifelse(F2N1>f3[4],"Very high",
                                                                                  ifelse(F2N1 > f3[3],"High",
                                                                                  ifelse(F2N1>f3[2],"Normal",
                                                                                  ifelse(F2N1>f3[1],"Low","Very low")))))

#결측값:3개
data_skill %>% filter(is.na(F2N1)==TRUE)


"""
#연구원비율분포
type_percent_tmp_1 %>% group_by(P1) %>% summarise(n.P1=n()) %>%
  ggplot(aes(factor(P1),n.P1)) + 
  geom_bar(stat='identity') +
  labs(title = "29세미만 연구원비율 분포",
       x = "29세미만 연구원비율",
       y = "업체수")

#성비분포
type_percent_tmp_2 %>% group_by(P_F) %>% summarise(n.P_F=n()) %>%
  ggplot(aes(factor(P_F),n.P_F)) + 
  geom_bar(stat='identity') +
  labs(title = "연구원中 여성비율 분포",
       x = "연구원中 여성비율",
       y = "업체수")

#매출비율분포
type_percent_tmp_3 %>% filter(is.na(F2N1_P)==FALSE) %>% group_by(F2N1_P) %>% summarise(n.F2N1_P=n()) %>% arrange((n.F2N1_P)) %>%
  ggplot(aes(factor(F2N1_P),n.F2N1_P)) + 
  geom_bar(stat='identity') +
  labs(title = "기술개발에 의한 매출비율 분포",
       x = "기술개발에 의한 매출비율",
       y = "업체수")

"""

#기준: fivenum일때 bar chart

# 20대 연구자 비율
type_percent_tmp_11 %>% group_by(P1) %>% summarise(n.P1=n()) %>%
  ggplot(aes(factor(P1),n.P1)) + 
  geom_bar(stat='identity') +
  labs(title = "20대 연구자 비율에 따른 업체수(기준:fivenum)",
       x = "20대 연구자 비율",
       y = "업체수")

#여성 비율

type_percent_tmp_22 %>% group_by(P_F) %>% summarise(n.PF=n()) %>%
  ggplot(aes(factor(P_F),n.PF)) + 
  geom_bar(stat='identity') +
  labs(title = "연구개발직中 여성비율 분포(기준:fivenum)",
       x = "연구개발직中 여성비율",
       y = "업체수")






# 기술개발에 의한 매출비율
type_percent
type_percent_tmp_33 %>%filter(is.na(F2N1_P)==TRUE)

type_percent_tmp_33 %>% filter(is.na(F2N1_P)==FALSE) %>% group_by(F2N1_P) %>% summarise(n.F2=n()) %>%
  ggplot(aes(factor(F2N1_P),n.F2)) + 
  geom_bar(stat='identity') +
  labs(title = "기술개발에 의한 매출비율(기준:fivenum)",
       x = "기술개발에 의한 매출비율",
       y = "업체수")




data_total=(inner_join(data_skill,data_out))

data_skill %>% select(D3S1N)


ff1 <- fivenum(data_skill$D3S1N)
ff1 <- fivenum(data_skill$D3S1N)
ff2 <- fivenum(data_skill$D3S2N)
ff3 <- fivenum(data_skill$D3S3N)
ff4 <- fivenum(data_skill$D3S4N)
ff5 <- fivenum(data_skill$D3S5N)
ff6 <- fivenum(data_skill$D3S6N)
ff7 <- fivenum(data_skill$D3S7N)
ff8 <- fivenum(data_skill$D3S8N)
ff9 <- fivenum(data_skill$D3S9N)
ff10 <- fivenum(data_skill$D3S10N)


data_skill = data_skill   %>% mutate(D3S1N_P=ifelse(D3S1N>ff1[4],"Very high",ifelse(D3S1N>ff1[3],"High",ifelse(D3S1N>ff1[2],"Normal",ifelse(D3S1N>ff1[1],"Low","Very low")))),
                                     D3S2N_P=ifelse(D3S2N>ff2[4],"Very high",ifelse(D3S2N>ff2[3],"High",ifelse(D3S2N>ff2[2],"Normal",ifelse(D3S2N>ff2[1],"Low","Very low")))),
                                     D3S3N_P=ifelse(D3S3N>ff3[4],"Very high",ifelse(D3S3N>ff3[3],"High",ifelse(D3S3N>ff3[2],"Normal",ifelse(D3S3N>ff3[1],"Low","Very low")))),
                                     D3S4N_P=ifelse(D3S4N>ff4[4],"Very high",ifelse(D3S4N>ff4[3],"High",ifelse(D3S4N>ff4[2],"Normal",ifelse(D3S4N>ff4[1],"Low","Very low")))),
                                     D3S5N_P=ifelse(D3S5N>ff5[4],"Very high",ifelse(D3S5N>ff5[3],"High",ifelse(D3S5N>ff5[2],"Normal",ifelse(D3S5N>ff5[1],"Low","Very low")))),
                                     D3S6N_P=ifelse(D3S6N>ff6[4],"Very high",ifelse(D3S6N>ff6[3],"High",ifelse(D3S6N>ff6[2],"Normal",ifelse(D3S6N>ff6[1],"Low","Very low")))),
                                     D3S7N_P=ifelse(D3S7N>ff7[4],"Very high",ifelse(D3S7N>ff7[3],"High",ifelse(D3S7N>ff7[2],"Normal",ifelse(D3S7N>ff7[1],"Low","Very low")))),
                                     D3S8N_P=ifelse(D3S8N>ff8[4],"Very high",ifelse(D3S8N>ff8[3],"High",ifelse(D3S8N>ff8[2],"Normal",ifelse(D3S8N>ff8[1],"Low","Very low")))),
                                     D3S9N_P=ifelse(D3S9N>ff9[4],"Very high",ifelse(D3S9N>ff9[3],"High",ifelse(D3S9N>ff9[2],"Normal",ifelse(D3S9N>ff9[1],"Low","Very low")))),
                                     D3S10N_P=ifelse(D3S10N>ff10[4],"Very high",ifelse(D3S10N>ff10[3],"High",ifelse(D3S10N>ff10[2],"Normal",ifelse(D3S10N>ff10[1],"Low","Very low")))))







#변수설정 
# y:2018매출액 c1:2017매출액 c2:업종별 매출성장률 
# x1:29세미만 연구자비율 x2:여성비율 x3:기술개발에 의한 매출비율 
# x4~x13:기술능력



#기술개발에 의한 매출비율에 NA값이 존재하는 업체 제외 (3개)
data_skill <- data_skill %>% filter(is.na(F2N1)==FALSE)
data_total <- data_total %>% filter(is.na(F2N1)==FALSE)
type_percent_tmp_11 <- type_percent_tmp_11 %>% filter(is.na(F2N1)==FALSE)
type_percent_tmp_22 <- type_percent_tmp_22 %>% filter(is.na(F2N1)==FALSE)

y<-(data_total$F2S1)
c1 <- data_total$g_2018
c2 <- data_skill$F2S1P
x1<- type_percent_tmp_11$P1
x2<- type_percent_tmp_22$P_F
x3<-type_percent_tmp_33$F2N1_P
x4<- data_skill$D3S1N_P
x5<- data_skill$D3S2N_P
x6<- data_skill$D3S3N_P
x7<- data_skill$D3S4N_P
x8<- data_skill$D3S5N_P
x9<- data_skill$D3S6N_P
x10<- data_skill$D3S7N_P
x11<- data_skill$D3S8N_P
x12<- data_skill$D3S9N_P
x13<- data_skill$D3S10N_P


library(car)

model_total<-aov(y~c1+c2+factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+
                   c1*factor(x1)+c1*factor(x2)+c1*factor(x3)+c1*factor(x4)+c1*factor(x5)+c1*factor(x6)+c1*factor(x7)+c1*factor(x8)+c1*factor(x9)+c1*factor(x10)+c1*factor(x11)+c1*factor(x12)+c1*factor(x13)+
                   c2*factor(x1)+c2*factor(x2)+c2*factor(x3)+c2*factor(x4)+c2*factor(x5)+c2*factor(x6)+c2*factor(x7)+c2*factor(x8)+c2*factor(x9)+c2*factor(x10)+c2*factor(x11)+c2*factor(x12)+c2*factor(x13))

summary(model_total)

Anova(model_total,type="3",singular.ok = TRUE)

duncan.test(result, "x10", group=TRUE, console=TRUE)

#2
"""
2. x2 = 1,2 소규모 x2 = 3, 4, 5 중 x2 = 6,7 상:
  20대 연구자 비율 + 성비 비율(연구개발직 여자 /총합)+ 기술능력 (국내) 10개 + 기술 매출액 %
+기술개발 인력지원 활용경험
"""

#데이터정리(규모별 분류)

scale_data_skill= data_skill %>% mutate(scale=ifelse(x2==1 | x2==2,"Small",ifelse(x2==3 | x2==4| x2==5,"Regular","Large")))
data_skill_S= scale_data_skill %>% filter(is.na(F2N1)==FALSE)  %>% filter(scale=="Small")
data_skill_R= scale_data_skill %>% filter(is.na(F2N1)==FALSE) %>% filter(scale=="Regular")
data_skill_L= scale_data_skill %>% filter(is.na(F2N1)==FALSE) %>% filter(scale=="Large")


data_skill_Sd=inner_join(data_skill_S,data_total)
data_skill_Sd=inner_join(data_skill_Sd,type_percent_tmp_11)
data_skill_Sd=inner_join(data_skill_Sd,type_percent_tmp_22)
data_skill_Sd=inner_join(data_skill_Sd,type_percent_tmp_33)


#변수설정 
#(y:2018매출액 c1:2017매출액 c2:업종별 매출성장률 x1:29세미만 연구자비율 x2:여성비율 x3:기술개발에 의한 매출비율 x4~x13:기술능력)

#2-1 (소규모)
y<-(data_skill_Sd$F2S1-data_skill_Sd$F2S1P)/data_skill_Sd$F2S1P

x1<- data_skill_Sd$P1
x2<- data_skill_Sd$P_F
x3<-data_skill_Sd$F2N1_P
x4<- data_skill_Sd$D3S1N_P
x5<- data_skill_Sd$D3S2N_P
x6<- data_skill_Sd$D3S3N_P
x7<- data_skill_Sd$D3S4N_P
x8<- data_skill_Sd$D3S5N_P
x9<- data_skill_Sd$D3S6N_P
x10<- data_skill_Sd$D3S7N_P
x11<- data_skill_Sd$D3S8N_P
x12<- data_skill_Sd$D3S9N_P
x13<- data_skill_Sd$D3S10N_P
t1<- data_skill_Sd$A3S1
t1[is.na(t1)]<-0
model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result = aov(y~x10)
duncan.test(result, "x10", group=TRUE, console=TRUE)



#데이터 join

data_skill_R=inner_join(data_skill_R,data_total)
data_skill_R=inner_join(data_skill_R,type_percent_tmp_11)
data_skill_R=inner_join(data_skill_R,type_percent_tmp_22)
data_skill_R=inner_join(data_skill_R,type_percent_tmp_33)


#2-2 (중규모)
y<-(data_skill_R$F2S1-data_skill_R$F2S1P)/data_skill_R$F2S1P
x1<- data_skill_R$P1
x2<- data_skill_R$P_F
x3<-data_skill_R$F2N1_P
x4<- data_skill_R$D3S1N_P
x5<- data_skill_R$D3S2N_P
x6<- data_skill_R$D3S3N_P
x7<- data_skill_R$D3S4N_P
x8<- data_skill_R$D3S5N_P
x9<- data_skill_R$D3S6N_P
x10<- data_skill_R$D3S7N_P
x11<- data_skill_R$D3S8N_P
x12<- data_skill_R$D3S9N_P
x13<- data_skill_R$D3S10N_P
t1<- data_skill_R$A3S1
t1[is.na(t1)]<-0
model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result1 <- aov(y~x3)
result2 <- aov(y~x7)

duncan.test(result1, "x3", group=TRUE, console=TRUE)
duncan.test(result2, "x7", group=TRUE, console=TRUE)

#2-3 (대규모)

data_skill_L=inner_join(data_skill_L,data_total)
data_skill_L=inner_join(data_skill_L,type_percent_tmp_11)
data_skill_L=inner_join(data_skill_L,type_percent_tmp_22)
data_skill_L=inner_join(data_skill_L,type_percent_tmp_33)



y<-(data_skill_L$F2S1-data_skill_L$F2S1P)/data_skill_L$F2S1P
x1<- data_skill_L$P1
x2<- data_skill_L$P_F
x3<-data_skill_L$F2N1_P
x4<- data_skill_L$D3S1N_P
x5<- data_skill_L$D3S2N_P
x6<- data_skill_L$D3S3N_P
x7<- data_skill_L$D3S4N_P
x8<- data_skill_L$D3S5N_P
x9<- data_skill_L$D3S6N_P
x10<- data_skill_L$D3S7N_P
x11<- data_skill_L$D3S8N_P
x12<- data_skill_L$D3S9N_P
x13<- data_skill_L$D3S10N_P
t1<- data_skill_L$A3S1
t1[is.na(t1)]<-0
model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result3 <- aov(y~x3)
result4 <- aov(y~x7)

duncan.test(result3, "x3", group=TRUE, console=TRUE)
duncan.test(result4, "x7", group=TRUE, console=TRUE)

#3
"""
3.업종별 (제조업 안에서 나눈거 기준):
  20대 연구자 비율 + 성비 비율(연구개발직 여자 /총합)+ 기술능력 (국내) 10개 + 기술 매출액 %
+기술개발 인력지원 활용경험

10	식료품
11	음료

13	섬유제품
14	의복 및 모피제품
15	가죽, 가방 및 신발

16	목재 및 나무제품
17	펄프 및 종이제품
18	인쇄, 기록매체 복제

20	화학물질 및 화학제품
21	의료용물질 및 의약품

22	고무제품 및 플라스틱
23	비금속광물제품
24	1차금속
25	금속가공제품

26	전자, 컴퓨터, 영상, 통신장비
27	의료, 정밀, 광학기기
28	전기장비
29	기타기계 및 장비

30	자동차 및 트레일러

311	선박 및 보트건조업
312	철도장비 제조업
313	항공기, 우주선 및 부품제조업
319	그 외 기타 운송장비 제조업

32	가구

33	기타제품

34	산업용 기계 및 장비수리업

"""




#데이터정리(업종을 1~11로 병합)
Sector_data_skill= data_skill %>% filter(is.na(F2N1)==FALSE) %>% mutate(scale=ifelse(x3==10 | x3==11,1,
                                               ifelse(x3==13 | x3==14 | x3==15, 2 ,
                                               ifelse(x3==16 | x3==17 | x3==18 , 3 , 
                                               ifelse(x3==20 | x3==21, 4 ,
                                               ifelse(x3==22 | x3==23 | x3==24 | x4==25 , 5 ,
                                               ifelse(x3==26 | x3==27 | x3==28 | x4==29 , 6 ,
                                               ifelse(x3==30, 7 ,
                                               ifelse(x3==311 | x3==312 | x3==313 | x3==319 ,8 ,
                                               ifelse(x3==32 , 9 ,
                                               ifelse(x3==33 , 10 , 11)))))))))))
Sector_data_skill <- inner_join(Sector_data_skill,data_total)
Sector_data_skill <- inner_join(Sector_data_skill,type_percent_tmp_11)
Sector_data_skill <- inner_join(Sector_data_skill,type_percent_tmp_22)
Sector_data_skill <- inner_join(Sector_data_skill,type_percent_tmp_33)


data_skill_1= Sector_data_skill %>% filter(scale==1)
data_skill_2= Sector_data_skill %>% filter(scale==2)
data_skill_3= Sector_data_skill %>% filter(scale==3)
data_skill_4= Sector_data_skill %>% filter(scale==4)
data_skill_5= Sector_data_skill %>% filter(scale==5)
data_skill_6= Sector_data_skill %>% filter(scale==6)
data_skill_7= Sector_data_skill %>% filter(scale==7)
data_skill_8= Sector_data_skill %>% filter(scale==8)
data_skill_9= Sector_data_skill %>% filter(scale==9)
data_skill_10= Sector_data_skill %>% filter(scale==10)
data_skill_11= Sector_data_skill %>% filter(scale==11)



#11개의 업종별 업체수

Sector_data_skill %>% group_by(scale) %>% summarise(n.s=n()) %>%
  ggplot(aes(factor(scale),n.s)) + 
  geom_bar(stat='identity') +
  labs(title = "11개의 업종별 업체수",
       x = "업종",
       y = "업체수")


#변수설정 

#3-1(업종1)
y<-(data_skill_1$F2S1-data_skill_1$F2S1P)/data_skill_1$F2S1P
x1<- data_skill_1$P1
x2<- data_skill_1$P_F
x3<-data_skill_1$F2N1_P
x4<- data_skill_1$D3S1N_P
x5<- data_skill_1$D3S2N_P
x6<- data_skill_1$D3S3N_P
x7<- data_skill_1$D3S4N_P
x8<- data_skill_1$D3S5N_P
x9<- data_skill_1$D3S6N_P
x10<- data_skill_1$D3S7N_P
x11<- data_skill_1$D3S8N_P
x12<- data_skill_1$D3S9N_P
x13<- data_skill_1$D3S10N_P
t1<- data_skill_1$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)


#3-2 업종2
y<-(data_skill_2$F2S1-data_skill_2$F2S1P)/data_skill_2$F2S1P
x1<- data_skill_2$P1
x2<- data_skill_2$P_F
x3<- data_skill_2$F2N1_P
x4<- data_skill_2$D3S1N_P
x5<- data_skill_2$D3S2N_P
x6<- data_skill_2$D3S3N_P
x7<- data_skill_2$D3S4N_P
x8<- data_skill_2$D3S5N_P
x9<- data_skill_2$D3S6N_P
x10<- data_skill_2$D3S7N_P
x11<- data_skill_2$D3S8N_P
x12<- data_skill_2$D3S9N_P
x13<- data_skill_2$D3S10N_P
t1<- data_skill_2$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result5 <- aov(y~x10)
result6 <- aov(y~x12)

duncan.test(result5, "x10", group=TRUE, console=TRUE)
duncan.test(result6, "x12", group=TRUE, console=TRUE)




#3-3 업종3
y<-(data_skill_3$F2S1-data_skill_3$F2S1P)/data_skill_3$F2S1P
x1<- data_skill_3$P1
x2<- data_skill_3$P_F
x3<- data_skill_3$F2N1_P
x4<- data_skill_3$D3S1N_P
x5<- data_skill_3$D3S2N_P
x6<- data_skill_3$D3S3N_P
x7<- data_skill_3$D3S4N_P
x8<- data_skill_3$D3S5N_P
x9<- data_skill_3$D3S6N_P
x10<- data_skill_3$D3S7N_P
x11<- data_skill_3$D3S8N_P
x12<- data_skill_3$D3S9N_P
x13<- data_skill_3$D3S10N_P
t1<- data_skill_3$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)


result7 <- aov(y~x3)

duncan.test(result7, "x3", group=TRUE, console=TRUE)




#3-4 업종4
y <- (data_skill_4$F2S1-data_skill_4$F2S1P)/data_skill_4$F2S1P
x1<- data_skill_4$P1
x2<- data_skill_4$P_F
x3<- data_skill_4$F2N1_P
x4<- data_skill_4$D3S1N_P
x5<- data_skill_4$D3S2N_P
x6<- data_skill_4$D3S3N_P
x7<- data_skill_4$D3S4N_P
x8<- data_skill_4$D3S5N_P
x9<- data_skill_4$D3S6N_P
x10<- data_skill_4$D3S7N_P
x11<- data_skill_4$D3S8N_P
x12<- data_skill_4$D3S9N_P
x13<- data_skill_4$D3S10N_P
t1<- data_skill_4$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result8 <- aov(y~x6)

duncan.test(result8, "x6", group=TRUE, console=TRUE)


#3-5 업종5
y <- (data_skill_5$F2S1-data_skill_5$F2S1P)/data_skill_5$F2S1P
x1<- data_skill_5$P1
x2<- data_skill_5$P_F
x3<- data_skill_5$F2N1_P
x4<- data_skill_5$D3S1N_P
x5<- data_skill_5$D3S2N_P
x6<- data_skill_5$D3S3N_P
x7<- data_skill_5$D3S4N_P
x8<- data_skill_5$D3S5N_P
x9<- data_skill_5$D3S6N_P
x10<- data_skill_5$D3S7N_P
x11<- data_skill_5$D3S8N_P
x12<- data_skill_5$D3S9N_P
x13<- data_skill_5$D3S10N_P
t1<- data_skill_5$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)
summary(model_total)
result9 <- aov(y~x8)

result10 <- aov(y~x11)

duncan.test(result9, "x8", group=TRUE, console=TRUE)
duncan.test(result10, "x11", group=TRUE, console=TRUE)




#3-6 업종6
y <- (data_skill_6$F2S1-data_skill_6$F2S1P)/data_skill_6$F2S1P
x1<- data_skill_6$P1
x2<- data_skill_6$P_F
x3<- data_skill_6$F2N1_P
x4<- data_skill_6$D3S1N_P
x5<- data_skill_6$D3S2N_P
x6<- data_skill_6$D3S3N_P
x7<- data_skill_6$D3S4N_P
x8<- data_skill_6$D3S5N_P
x9<- data_skill_6$D3S6N_P
x10<- data_skill_6$D3S7N_P
x11<- data_skill_6$D3S8N_P
x12<- data_skill_6$D3S9N_P
x13<- data_skill_6$D3S10N_P
t1<- data_skill_6$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

#3-7 업종7
y <- (data_skill_7$F2S1-data_skill_7$F2S1P)/data_skill_7$F2S1P
x1<- data_skill_7$P1
x2<- data_skill_7$P_F
x3<- data_skill_7$F2N1_P
x4<- data_skill_7$D3S1N_P
x5<- data_skill_7$D3S2N_P
x6<- data_skill_7$D3S3N_P
x7<- data_skill_7$D3S4N_P
x8<- data_skill_7$D3S5N_P
x9<- data_skill_7$D3S6N_P
x10<- data_skill_7$D3S7N_P
x11<- data_skill_7$D3S8N_P
x12<- data_skill_7$D3S9N_P
x13<- data_skill_7$D3S10N_P
t1<- data_skill_7$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result11 <- aov(y~x3)

result12 <- aov(y~x7)

result13 <- aov(y~x9)

duncan.test(result11, "x3", group=TRUE, console=TRUE)
duncan.test(result12, "x7", group=TRUE, console=TRUE)
duncan.test(result13, "x9", group=TRUE, console=TRUE)



#3-8 업종8
y <- (data_skill_8$F2S1-data_skill_8$F2S1P)/data_skill_8$F2S1P
x1<- data_skill_8$P1
x2<- data_skill_8$P_F
x3<- data_skill_8$F2N1_P
x4<- data_skill_8$D3S1N_P
x5<- data_skill_8$D3S2N_P
x6<- data_skill_8$D3S3N_P
x7<- data_skill_8$D3S4N_P
x8<- data_skill_8$D3S5N_P
x9<- data_skill_8$D3S6N_P
x10<- data_skill_8$D3S7N_P
x11<- data_skill_8$D3S8N_P
x12<- data_skill_8$D3S9N_P
x13<- data_skill_8$D3S10N_P
t1<- data_skill_8$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result14 <- aov(y~x11)

result15 <- aov(y~x13)

duncan.test(result14, "x11", group=TRUE, console=TRUE)
duncan.test(result15, "x13", group=TRUE, console=TRUE)

#3-9 업종9
y <- (data_skill_9$F2S1-data_skill_9$F2S1P)/data_skill_9$F2S1P
x1<- data_skill_9$P1
x2<- data_skill_9$P_F
x3<- data_skill_9$F2N1_P
x4<- data_skill_9$D3S1N_P
x5<- data_skill_9$D3S2N_P
x6<- data_skill_9$D3S3N_P
x7<- data_skill_9$D3S4N_P
x8<- data_skill_9$D3S5N_P
x9<- data_skill_9$D3S6N_P
x10<- data_skill_9$D3S7N_P
x11<- data_skill_9$D3S8N_P
x12<- data_skill_9$D3S9N_P
x13<- data_skill_9$D3S10N_P
t1<- data_skill_9$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)
result16 <- aov(y~x7)

result17 <- aov(y~x13)

duncan.test(result16, "x7", group=TRUE, console=TRUE)
duncan.test(result17, "x13", group=TRUE, console=TRUE)



#3-10 업종10
y <- (data_skill_10$F2S1-data_skill_10$F2S1P)/data_skill_10$F2S1P
x1<- data_skill_10$P1
x2<- data_skill_10$P_F
x3<- data_skill_10$F2N1_P
x4<- data_skill_10$D3S1N_P
x5<- data_skill_10$D3S2N_P
x6<- data_skill_10$D3S3N_P
x7<- data_skill_10$D3S4N_P
x8<- data_skill_10$D3S5N_P
x9<- data_skill_10$D3S6N_P
x10<- data_skill_10$D3S7N_P
x11<- data_skill_10$D3S8N_P
x12<- data_skill_10$D3S9N_P
x13<- data_skill_10$D3S10N_P
t1<- data_skill_10$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)


#3-11 업종11
y <- (data_skill_11$F2S1-data_skill_11$F2S1P)/data_skill_11$F2S1P
x1<- data_skill_11$P1
x2<- data_skill_11$P_F
x3<- data_skill_11$F2N1_P
x4<- data_skill_11$D3S1N_P
x5<- data_skill_11$D3S2N_P
x6<- data_skill_11$D3S3N_P
x7<- data_skill_11$D3S4N_P
x8<- data_skill_11$D3S5N_P
x9<- data_skill_11$D3S6N_P
x10<- data_skill_11$D3S7N_P
x11<- data_skill_11$D3S8N_P
x12<- data_skill_11$D3S9N_P
x13<- data_skill_11$D3S10N_P
t1<- data_skill_11$A3S1
t1[is.na(t1)]<-0

model_total<-lm(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+factor(t1))
anova(model_total)

result18 <- aov(y~x1)

result19 <- aov(y~x7)

result20 <- aov(y~x12)

duncan.test(result18, "x1", group=TRUE, console=TRUE)
duncan.test(result19, "x7", group=TRUE, console=TRUE)
duncan.test(result20, "x12", group=TRUE, console=TRUE)

