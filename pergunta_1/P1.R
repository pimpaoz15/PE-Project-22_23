# Load required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Set path where all data is located || latter it will be the directory where the plot will be saved
# If u use RStudio as long as all files are on the same folder, you don't need this line, i'm using VSCode/NeoVIM
setwd('/Users/pimpao/Library/CloudStorage/OneDrive-Personal/Code/R/Projeto PE 22_23/pergunta_1')

# Load the data
data <- read_xlsx("econ.xlsx", sheet = 1)
  
# Select data for years >= 2000
data1984 <- data %>% filter(year(tempo) >= 1984)

# Calculate mean and standard deviation for each variable
mean_ddesemp <- mean(data1984$ddesemp)
sd_ddesemp <- sd(data1984$ddesemp)
mean_tpp <- mean(data1984$tpp)
sd_tpp <- sd(data1984$tpp)


# Apply the transformation to the data associated with each variable
data_transformed <- data1984 %>% mutate(z_ddesemp = (ddesemp - mean_ddesemp) / sd_ddesemp, z_tpp = (tpp - mean_tpp) / sd_tpp)

# Call the pdf command to start the plot - with the directory i want to save in + plot width + plot height (both in inches)
pdf(file='/Users/pimpao/Library/CloudStorage/OneDrive-Personal/Code/R/Projeto PE 22_23/pergunta_1/Rplot.pdf', width = 6, height = 4)

# Plot the graph
ggplot(data_transformed, aes(x = tempo)) + 
  geom_line(aes(y = z_ddesemp, color = "ddesemp"), linewidth = 0.7) +
  geom_line(aes(y = z_tpp, color = "tpp"), linewidth = 0.7) +
  scale_color_manual(values = c("ddesemp" = "red", "tpp" = "blue"), 
                     name = "Variável") +
  labs(x = "Ano", y = "Valor Padrão",
       title = "Evolução da Duração Mediana do Desemprego (ddesemp) e \n da Taxa de Poupança Pessoal (TPP) desde 1984") +
  theme_bw()

dev.off()





