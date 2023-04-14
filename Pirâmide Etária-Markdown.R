if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

library(pacman)
pacman::p_load(dplyr,ggplot2, readxl, stringr, reshape2)

dados <- read.csv("C:/Users/jgararuna/Downloads/arquivo-piramide.xlsx - Plan1.csv") 
colnames(dados)<-c("Idade","Homens","Mulheres") 
dados <- dados[-1,]

dados2 <- melt(dados,id.vars = names(dados)[1])
names(dados2)<-c("Idade","Sexo","Valor")

dados2$Valor <- as.numeric(dados2$Valor)
dados3 <- dados2 %>% 
  mutate(Valor2 = ifelse(Sexo == "Homens", - Valor , Valor)) %>% 
  select(1,2,4)


dados3$Idade <- factor(dados$Idade, levels = c("0 a 4 anos","5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                         "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos",
                         
                         "40 a 44 anos", "45 a 49 anos", "50 a 54 anos", "55 a 59 anos", "60 a 64 anos",
                         
                         "65 a 69 anos", "70 a 74 anos", "75 a 79 anos", "80 a 84 anos",
                         "85 a 89 anos", "90 anos ou mais"), ordered = TRUE)
options(scipen = 999) 

B<-dados3
names(B)<-c("Classe","Sexo","MT")


B$MT2<-abs(B$MT)+ 100000
B <- B %>% 
  mutate(MT2 = ifelse(Sexo == "Homens", - MT2 , MT2))

#CODIGO DO GRAFICO
ggplot(B,aes(
  x = Classe, y =MT,
  fill = Sexo)
) + 
  geom_col(alpha = 0.9) +
  coord_flip()+
  geom_text(aes(x=B$Classe, y= B$MT2,label=abs(B$MT))) +  
  theme_minimal() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("#6699CC", "#DDCC77")) +  
  theme(legend.position = "bottom")+  
  labs(y = "Populaçao")
