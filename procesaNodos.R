setwd("G:\\Mi unidad\\NSDA\\Proyecto_final\\Entrega_2")

# Libraries
library(data.table)
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(xtable)
library(openxlsx)


color <- c("Morado", "Verde", "Rojo", "Azul", "Naranja", "Lila", "Siam", "Negro", rep("Gris", 9))
levls <- c(4, 1, 2, 0, 7, 6, 3, 11, 5, 8, 9, 13, 10, 12, 14, 15, 16)
dbCol <- data.table(color = color, modularity_class = levls)


# Load node data
db <- fread("nodos.csv")
setnames(db, names(db), gsub(" ", "_", names(db)))


db[, table(modularity_class)]

summary(lm(Hub~weighted_degree, data = db))
# Nodo con pesos demasiado altos
db[weighted_degree > 499,]
# Modelos excluyendo a este nodo
summary(lm(scale(Hub)~scale(weighted_degree), data = db[weighted_degree < 499,]))

# Tabla de categorias de las comunidades encontradas
dbSumm <- db[, .(.N, mnWI = round(mean(weighted_indegree), 2), mnWO = round(mean(weighted_outdegree), 2)), by = modularity_class][order(N, decreasing = T)]
dbSumm[, perc := round((N/sum(N)*100), 2)]
# Exportar a latex y excel la tabla resumane
dbSumm <- merge(dbSumm, dbCol, by = "modularity_class")[order(N, decreasing = T)]
xtable(dbSumm)
write.xlsx(dbSumm, file = "commOut.xlsx")
# Sources
# source("multiplotplot.R")

# # Functions
degrePlot <- function(data, namPlot, varName){
	ggplot(data, aes(x=eval(parse(text=varName)), y=N, color = "red")) + 
	    geom_point(size = 4) +
	    xlab("Number of Partners") + ylab("Number of Nodes") +
	    theme_minimal() +
	    theme(legend.position = "None", 
	    	  axis.title.x = element_text(size=20),
	    	  axis.text.x = element_text(size=15),
	    	  axis.title.y = element_text(size=20),
	    	  axis.text.y = element_text(size=15))
	ggsave(file.path(outPath, paste0(namPlot, ".png")), width = 10, height = 8)
}

degrePlotWgts <- function(data, namPlot, varName){
	ggplot(data, aes(x=eval(parse(text=varName)))) + 
	    geom_histogram(binwidth = 0.1) +
	    	    theme_minimal() +
	    	    theme(legend.position = "None", 
	    	    	  axis.title.x = element_blank(),
	    	    	  axis.text.x = element_text(size=15),
	    	    	  axis.title.y = element_blank(),
	    	    	  axis.text.y = element_text(size=15))
	ggsave(file.path(outPath, paste0(namPlot, ".png")), width = 10, height = 8)
}
# # Paths
outPath <- "graficos"





# Degree distribution
degrePlot(db[,.N, by = degree], "dgreDistrNoWeights", "degree")
# In-Degree distribution
degrePlot(db[,.N, by = indegree], "inDgreDistrNoWeights", "indegree")
# Out-Degree distribution
degrePlot(db[,.N, by = outdegree], "outDgreDistrNoWeights", "outdegree")

# Degree distribution with Weights
degrePlotWgts(db, "dgreDistrWeights", "weighted_degree")
# Indegree distribution with Weights
degrePlotWgts(db, "InDgreDistrWeights", "weighted_indegree")
# Outdegree distribution with Weights
degrePlotWgts(db, "outDgreDistrWeights", "weighted_outdegree")