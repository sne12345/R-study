setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("Telecom.csv",header = T)

set.seed(1) # 랜덤넘버 발생시킨것과 발생시키지 않은것의 차이가 뭘까요?
            # -> 교수님과 같은 랜덤순서를 갖게 하기 위해 실행
            # set.seed 안의 매개변수가 동일하면 동일한 순서로 됨됨
train.rows <- sample(rownames(housing.df,),dim(housing.df)[1]*0.5)
train.data <- housing.df[train.rows,]

valid.rows <- sample(setdiff(rownames(housing.df),train.rows),dim(housing.df)[1]*0.25)
valid.data <- housing.df[valid.rows,]

test.rows <- setdiff(rownames(housing.df),union(train.rows,valid.rows))
test.data <- housing.df[test.rows,]
