setwd("G:\\Mi unidad\\NSDA\\Proyecto_final\\Entrega_2")

library(data.table)
library(caret)

db <- fread("soc-sign-bitcoinalpha.csv")

# Source;Target;Type;Id;Label;timeset;Weight
dbOut <- db[,.(Source = V1, 
			   Target = V2, 
			   Type = "Directed", 
			   Id = 1:nrow(db), 
			   Label = "",
			   timeset = "",
			   # V3,
			   Weight = ((V3)-min(V3))/(max(V3)-min(V3)))]
dbOut[Weight == 0, Weight := 0.01]


write.csv(dbOut, "paraGephi.csv", quote = F, row.names = F)