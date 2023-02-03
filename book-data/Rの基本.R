1+1
x<-2
x+1
sqrt(4)
#ベクトルの作成
vector_1<-c(1,2,3,4,5)
vector_1
1:10
matrix_1<-matrix(data=1:10,
                 nrow=2,
                 byrow=TRUE)
matrix_1
rownames(matrix_1)<-c("Row1","Row2")
colnames(matrix_1)<-c("Col1","Col2","Col3","Col4","Col5")
matrix_1
array_1<-array(
  data=1:30,
  dim=c(3,5,2)
)
array_1
data_frame_1<-data.frame(
  col1=c("A","B","C","D","E"),
  col2=c(1,2,3,4,5)
)
data_frame_1
nrow(data_frame_1)
list_1<-list(
  chara=c("A","B","C"),
  matrix=matrix_1,
  df=data_frame_1
)
list_1
vector_1[1]
matrix_1[1,1]
array_1[1,1,2]
matrix_1[1,]
matrix_1[2,2:4]
dim(matrix_1)
dim(array_1)
dimnames(matrix_1)
matrix_1["Row1","Col1"]
data_frame_1$col2[2]
head(data_frame_1,n=2)
list_1[[2]]
data_frame_2<-data.frame(data=1:24)
data_frame_2
ts_1<-ts(data_frame_2,
         start=c(2010,1),
         frequency =12)
ts_1
birds<-read.csv("2-1-1-birds.csv")
cor(birds$body_length,birds$feather_length)
Nile
acf(Nile,
    type="covariance",
    plot=F,
    lag.max=5)
acf(Nile)
install.packages("ggplot2")
library(ggplot2)
fish<-read.csv("2-2-1-fish.csv")
head(fish,n=3)
ggplot(data=fish,mapping=aes(x=length))+
  geom_histogram(alpha=0.5,bins=20)+
  labs(title="ヒストグラム")+
  theme_gray (base_family = "HiraKakuPro-W3")
ggplot(data=fish,mapping=aes(x=length),geom_text(size = 6, family = "HiraKakuPro-W3"))+
  geom_density(linewidth=1.5)+
  labs(title="カーネル密度推定")+
  theme_gray (base_family = "HiraKakuPro-W3")
ggplot(data=fish,mapping=aes(x=length,y=..density..),
       geom_text(size = 6, family = "HiraKakuPro-W3"))+
  geom_histogram(alpha=0.5,bins=20)+
  geom_density(linewidth=1.5)+
  labs(title="グラフの重ね合わせ")+
  theme_gray (base_family = "HiraKakuPro-W3")
library(gridExtra)
p_hist<-ggplot(data=fish,mapping=aes(x=length))+
  geom_histogram(alpha=0.5,bins=20)+
  labs(title="ヒストグラム")
p_density<-ggplot(data=fish,mapping=aes(x=length),geom_text(size = 6, family = "HiraKakuPro-W3"))+
  geom_density(linewidth=1.5)+
  labs(title="カーネル密度推定")
grid.arrange(p_hist, p_density, ncol=2)
head(iris,n=3)
p_box<-ggplot(data=iris,
              mapping=aes(x=Species,y=Petal.Length))+
  geom_boxplot()+
  labs(title="箱ひげ図")+
  theme_gray (base_family = "HiraKakuPro-W3")
p_violin<-ggplot(data=iris,
                 mapping=aes(x=Species,y=Petal.Length))+
  geom_violin()+
  labs(title="バイオリンプロット")+
  theme_gray (base_family = "HiraKakuPro-W3")
grid.arrange(p_box,p_violin,ncol=2)
ggplot(iris,
       aes(x=Petal.Width,y=Petal.Length,color=Species))+
  geom_point()
nile_data_frame<-data.frame(
  year=1871:1970,
  Nile=as.numeric(Nile)
)
head(nile_data_frame,n=3)
ggplot(nile_data_frame,aes(x=year,y=Nile))+
  geom_line()
library(ggfortify)
autoplot(Nile)
