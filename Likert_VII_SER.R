
#VII SER - likert em português
# pacotes

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse,scales, likert,gt)

# dados

data(pisaitems)

dim(pisaitems)

# Examinando uma coluna

pisaitems %>% select(ST24Q01) %>% summary()

# Subconjunto com as perguntas em português

pisa_subset <- pisaitems  %>% 
  select(1:6) %>% 
  setNames(c(
    "País",
    "Eu só leio se eu tiver que ler.",
    "Ler é um dos meus hobbies favoritos.",
    "Gosto de conversar sobre livros com outras pessoas.",
    "Acho difícil terminar livros.",
    "Fico feliz se receber um livro de presente."
  ))

#  estrutura do subconjunto de estudo

pisa_subset %>% glimpse()

# obtendo o primeiro gráfico

likert_out<- select(pisa_subset,2:6)

likert_out<-likert(as.data.frame(likert_out))

plot(likert_out)

# problemas 
# posição da legenda; título e rótulos da legenda e título dos eixos

color <- c("firebrick","#C08A8B","#92C492","forestgreen")

legend_order <- c("Strongly disagree", "Disagree", "Agree", "Strongly agree") 

plot(likert_out, 
     centered=TRUE,)+
  scale_fill_manual(name="Respostas", 
                    values=color,
                    breaks=legend_order,
                    labels=c('discordo totalmente', 'discordo', 
                             'concordo', 'concordo totalmente'))+
  ylab("Porcentagem") +
  xlab("Itens")+
  theme(text=element_text(size=11),
        legend.position = "bottom",
        legend.justification = "right",
        legend.direction = "horizontal",
        legend.margin = margin(t = 3),
        axis.text.y = element_text(angle = 0),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 11),
        plot.margin = margin(t = 15, r = 5, b = 10, l = 5))



# por país com apenas 3 itens


likert_out_group<-likert(as.data.frame(pisa_subset[,2:4]), grouping=pisa_subset$País)

plot(likert_out_group, group.order=c('Mexico','Canada','United States')) +
  scale_x_discrete(labels = c('México', 'Canadá', 'Estados Unidos'))+
  scale_fill_manual(name="Respostas", 
                    values=color,
                    breaks=legend_order,
                    labels=c('discordo totalmente', 'discordo', 
                             'concordo', 'concordo totalmente'))+
  ylab("Porcentagem") +
  xlab("Paises")
    

# calor

likert_out<- select(pisa_subset,2:6)

likert_out<-likert(as.data.frame(likert_out))

(heat<-plot(likert_out, type="heat"))
heat

# retirada da coluna mean
dados_calor <- likert_out$results %>%
  pivot_longer(cols = -Item) %>%
  filter(name != "Mean") %>%
  mutate(name = factor(name, levels = rev(unique(name)))) %>% 
mutate(name = factor(name, levels = c("Strongly disagree",
                                      "Disagree",
                                      "Agree", 
                                      "Strongly agree")))

 # gráfico de calor

dados_calor %>% ggplot(aes(x = name, y = Item, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = muted("red"), mid = "white", 
                       high = muted("blue"), midpoint = 0) +
  geom_text(aes(label = round(value, digits = 1))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels=c('Discordo totalmente', 'Discordo',
                            'Concordo','Concordo totalmente'))+
  xlab("Respostas")+
  ylab("Perguntas")+
  guides(fill = guide_colorbar(title = "Valores"))
  

# tabelas 

likert_out$results[, 2:5] <- round(likert_out$results[, 2:5],
                                   digits = 2)
gt(likert_out$results)


# Em português

likert_out$results[, 2:5] <- round(likert_out$results[, 2:5], digits = 2)
colnames(likert_out$results) <- c("Itens", "Discordo totalmente", "Discordo", "Concordo", "Concordo totalmente")
gt(likert_out$results)

#Ou desta forma

likert_out$results <- likert_out$results %>%
  mutate(across(2:5, ~paste0(sprintf("%.2f", .), "%")))
colnames(likert_out$results) <- c("Itens", "Discordo totalmente", "Discordo", "Concordo", "Concordo totalmente")
gt(likert_out$results)




