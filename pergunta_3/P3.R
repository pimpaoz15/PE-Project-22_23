# bibliotecas necessárias
library(dplyr)
library(ggplot2)

setwd('/Users/pimpao/Library/CloudStorage/OneDrive-Personal/Code/R/Projeto PE 22_23/pergunta_3')

#ler o ficheiro e colocar os dados numa tabela
data <- read.delim("GENDER_EMP_19032023152556091.txt")

#filtrar os dados
data_norway_2019_emp2_Sex <- data %>%
  filter(COU == "NOR", IND == "EMP2",Time==2019, Sex %in% c("Men", "Women"), AGE %in% c("1524", "2554", "5564"))

#criar conjunto de dados a usar no gráfico
df <- data.frame(Sex=c("Men", "Women","Men", "Women","Men", "Women"),
                 Age = c("15-24", "15-24", "25-54","25-54", "55-64", "55-64"),
                 EMP2=as.numeric(as.character(data_norway_2019_emp2_Sex$Value)))

# Call the pdf command to start the plot - with the directory i want to save in + plot width + plot height (both in inches)
pdf(file='/Users/pimpao/Library/CloudStorage/OneDrive-Personal/Code/R/Projeto PE 22_23/pergunta_3/Rplot.pdf', width = 10, height = 10)

#construção do gráfico
p <- ggplot(df, aes(x=Age, y=EMP2, fill=Sex)) +
  geom_bar(stat="identity" , position=position_dodge()) + 
  geom_text(aes(label = round(EMP2, 1)), position = position_dodge(width = 0.9), vjust = -0.5, color = "black", size=3
            ) +
  theme_minimal()
p + scale_fill_manual(values=c("blueviolet", "turquoise1"))+
  ylab("EMP2, Norway 2019")

dev.off()
