##### Matrices de Precision de Interacciones Espacio-Temporales RW1 ####

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011) |> 
  filter(Año < 2024)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |>
  mutate(AñoMes = paste(Año,Mes,sep = "-")) |>
  select(-c(Año,Mes))

H = inla.read.graph("VecindadProvSantiago.graph")

sp = length(unique(BaseModelo$Comuna))
tm = length(unique(BaseModelo$AñoMes))

NSp = diag(sp) # Espacial No Estructurado
NTm = diag(tm) # Temporal No Estructurado

SpM = inla.graph2matrix(H) # Espacial Estructurado
SpM = (Diagonal(nrow(SpM), colSums(SpM)) - SpM) |> as.matrix()

Dt = diff(diag(tm), differences = 1)
Qt = t(Dt) %*% Dt # Temporal Estructurado

R1 = kronecker(NSp,NTm) # Int I
R2 = kronecker(NSp,Qt) # Int II
R3 = kronecker(SpM,NTm) # Int III
R4 = kronecker(SpM,Qt) # Int IV

eigens1 = eigen(R1, symmetric = T)
eigens2 = eigen(R2, symmetric = T)
eigens3 = eigen(R3, symmetric = T)
eigens4 = eigen(R4, symmetric = T)

(round(eigens1$values, 10) == 0) |> sum()
(round(eigens2$values, 10) == 0) |> sum()
(round(eigens3$values, 10) == 0) |> sum()
(round(eigens4$values, 10) == 0) |> sum()

sp * tm - sp*tm # Int I def
sp * tm - (sp * (tm - 1)) # Int II def
sp * tm - ((sp - 1) * tm) # Int III def
sp * tm - ((sp - 1) * (tm - 1)) # Int IV def

index1 = ((eigens1$values |> round(10)) == 0)
index2 = ((eigens2$values |> round(10)) == 0)
index3 = ((eigens3$values |> round(10)) == 0)
index4 = ((eigens4$values |> round(10)) == 0)

sum(index1)
sum(index2)
sum(index3)
sum(index4)

A1 = eigens1$vectors[,index1]
A2 = eigens2$vectors[,index2]
A3 = eigens3$vectors[,index3]
A4 = eigens4$vectors[,index4]

indexaux1 = which(R1 != 0, arr.ind = T)
C1 = data.frame(indexaux1, Value = R1[indexaux1]) |> arrange(row, col)

indexaux2 = which(R2 != 0, arr.ind = T)
C2 = data.frame(indexaux2, Value = R2[indexaux2]) |> arrange(row, col)

indexaux3 = which(R3 != 0, arr.ind = T)
C3 = data.frame(indexaux3, Value = R3[indexaux3]) |> arrange(row, col)

indexaux4 = which(R4 != 0, arr.ind = T)
C4 = data.frame(indexaux4, Value = R4[indexaux4]) |> arrange(row, col)

save(A1, file = "A1p.Rdata")
save(A2, file = "A2p.Rdata")
save(A3, file = "A3p.Rdata")
save(A4, file = "A4p.Rdata")
save(C1, file = "C1p.Rdata")
save(C2, file = "C2p.Rdata")
save(C3, file = "C3p.Rdata")
save(C4, file = "C4p.Rdata")
save(R1, file = "R1p.Rdata")
save(R2, file = "R2p.Rdata")
save(R3, file = "R3p.Rdata")
save(R4, file = "R4p.Rdata")

##### Matrices de Precision de Interacciones Espacio-Temporales RW2 ####

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011) |> 
  filter(Año < 2024)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |>
  mutate(AñoMes = paste(Año,Mes,sep = "-")) |> 
  select(-c(Año,Mes))

H = inla.read.graph("VecindadProvSantiago.graph")

sp = length(unique(BaseModelo$Comuna))
tm = length(unique(BaseModelo$AñoMes))

NSp = diag(sp) # Espacial No Estructurado
NTm = diag(tm) # Temporal No Estructurado

SpM = inla.graph2matrix(H) # Espacial Estructurado
SpM = (Diagonal(nrow(SpM), colSums(SpM)) - SpM) |> as.matrix()

Dt = diff(diag(tm), differences = 2)
Qt = t(Dt) %*% Dt # Temporal Estructurado

R1 = kronecker(NSp,NTm) # Int I
R2 = kronecker(NSp,Qt) # Int II
R3 = kronecker(SpM,NTm) # Int III
R4 = kronecker(SpM,Qt) # Int IV

eigens1 = eigen(R1, symmetric = T)
eigens2 = eigen(R2, symmetric = T)
eigens3 = eigen(R3, symmetric = T)
eigens4 = eigen(R4, symmetric = T)

(round(eigens1$values, 10) == 0) |> sum()
(round(eigens2$values, 10) == 0) |> sum()
(round(eigens3$values, 10) == 0) |> sum()
(round(eigens4$values, 10) == 0) |> sum()

sp * tm - sp*tm # Int I def
sp * tm - (sp * (tm - 2)) # Int II def
sp * tm - ((sp - 1) * tm) # Int III def
sp * tm - ((sp - 1) * (tm - 2)) # Int IV def

index1 = ((eigens1$values |> round(10)) == 0)
index2 = ((eigens2$values |> round(10)) == 0)
index3 = ((eigens3$values |> round(10)) == 0)
index4 = ((eigens4$values |> round(10)) == 0)

sum(index1)
sum(index2)
sum(index3)
sum(index4)

A1 = eigens1$vectors[,index1]
A2 = eigens2$vectors[,index2]
A3 = eigens3$vectors[,index3]
A4 = eigens4$vectors[,index4]

indexaux1 = which(R1 != 0, arr.ind = T)
C1 = data.frame(indexaux1, Value = R1[indexaux1]) |> arrange(row, col)

indexaux2 = which(R2 != 0, arr.ind = T)
C2 = data.frame(indexaux2, Value = R2[indexaux2]) |> arrange(row, col)

indexaux3 = which(R3 != 0, arr.ind = T)
C3 = data.frame(indexaux3, Value = R3[indexaux3]) |> arrange(row, col)

indexaux4 = which(R4 != 0, arr.ind = T)
C4 = data.frame(indexaux4, Value = R4[indexaux4]) |> arrange(row, col)

save(A1, file = "A1r2p.Rdata")
save(A2, file = "A2r2p.Rdata")
save(A3, file = "A3r2p.Rdata")
save(A4, file = "A4r2p.Rdata")
save(C1, file = "C1r2p.Rdata")
save(C2, file = "C2r2p.Rdata")
save(C3, file = "C3r2p.Rdata")
save(C4, file = "C4r2p.Rdata")
save(R1, file = "R1r2p.Rdata")
save(R2, file = "R2r2p.Rdata")
save(R3, file = "R3r2p.Rdata")
save(R4, file = "R4r2p.Rdata")