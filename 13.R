library(tibble)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)

Fo1 <- c("Faculty", "of")
PM1 <- c("Policy", "Management")
EI1 <- c("Environment", "and", "Information", "Studies")


str_c("Keio","SFC",sep=" ")

str_c(PM1,collapse=" ")


Fo2 <- str_c(Fo1, collapse = " ")
PM2 <- str_c(PM1, collapse = " ")
EI2 <- str_c(EI1, collapse = " ")
string <- c(Fo2, PM2, EI2)


str_c(Fo2,c(PM2,EI2),sep=" ")


str_split(string,pattern = " ")

str_split(string,pattern= " ", simplify=TRUE)

str_split(string,pattern= " ", simplify=TRUE,n=3)


str_detect(PM1,pattern="o")

str_detect(PM1,pattern="o",negate=TRUE)

str_detect(EI1,pattern="n$")


str_subset(EI1,pattern="en")


str_replace(EI1, pattern="n",replacement="*")

str_replace_all(EI1, pattern="n", replacement="*")


str_extract(EI1, pattern="en")

str_extract_all(EI1,pattern="en|o",simplify=TRUE)


str_sub(EI1,start=1,end=3)

str_sub(EI1,start=-3,end=-1)


str_locate(string,pattern=" ")


str_length(EI1)


num = 0
for (i in 1:100) {
  num <- num + i
}
num

system.time({
  num=0
  for(i in 1:1000)
    num<-num+i
})
system.time(sum(1:1000))


s<-0
i<-1
while(s<1000){
  s<-s+i
  i<-i+1
}
s


list(1,2,3)%>%
  purrr::map(exp)

l1<-iris %>%
  group_split(Species)

l2<-iris %>%
  split(.$Species)

l1 %>%
  map(summary)

l1 %>%
  map(~lm(Petal.Width ~Petal.Length,data=.x))


list1 <- iris %>% 
  split(.$Species) %>% 
  map(select, Petal.Length) %>% 
  map(mutate, No = row_number())

list2 <- iris %>% 
  split(.$Species) %>% 
  map(select, Petal.Width) %>% 
  map(mutate, No = row_number())

res <- map2(list1, list2, left_join, by = "No")

res2 <- map2(list1, list2, 
             left_join, by = "No") %>% 
  map_dfr(select, -No, .id = "Species") %>% 
  mutate_at(vars(Species), as.factor) %>% 
  select(Petal.Length, Petal.Width, Species)


iris %>% 
  select(Petal.Length, Petal.Width, Species) %>% 
  all.equal(res2)

list.files(pattern=".R",full.names = TRUE)


