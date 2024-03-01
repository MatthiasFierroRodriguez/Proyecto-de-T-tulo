rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

####RW1 Poisson####

formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05))
      )
  ) +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(
      theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid", 
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI,
                family = "poisson",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001)
                )

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C2, 
    constr = T,
    extraconstr = list(A = A2,
                       e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII,
                 family = "poisson",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T,
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C3,
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIII = inla(formula.intIII,
                  family = "poisson",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T, 
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, 
                                         waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C4,
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV, 
                 family = "poisson", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T, 
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T,
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = rep("Poisson", 4)

RW = rep(1, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####RW2 Poisson####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2", 
    scale.model = T, 
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI,
                family = "poisson", 
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C2, 
    constr = T,
    extraconstr = list(A = A2, e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII,
                 family = "poisson",
                 data = BaseModelo,
                 E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C3,
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIII = inla(formula.intIII, 
                  family = "poisson",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C4,
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV,
                 family = "poisson",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = rep("Poisson", 4)

RW = rep(2, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()


####RW1 ZIP0####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI, 
                family = "zeroinflatedpoisson0", 
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T,
                                       waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C2, constr = T,
    extraconstr = list(A = A2, e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson0",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T,
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C3,
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIII = inla(formula.intIII,
                  family = "zeroinflatedpoisson0",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T, 
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T,
                                         waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C4, 
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson0", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T, 
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))


Verosimilitud = rep("ZIP0", 4)

RW = rep(1, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####RW2 ZIP0####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI,
                family = "zeroinflatedpoisson0",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C2,
    constr = T,
    extraconstr = list(A = A2,
                       e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson0",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C3,
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIII = inla(formula.intIII,
                  family = "zeroinflatedpoisson0",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C4, 
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson0",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = rep("ZIP0", 4)

RW = rep(2, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####RW1 ZIP1####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1 ,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI, 
                family = "zeroinflatedpoisson1",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C2, 
    constr = T,
    extraconstr = list(A = A2, e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson1", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C3,
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIII = inla(formula.intIII, 
                  family = "zeroinflatedpoisson1",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, 
                                         waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))
mod.intIII |> summary()

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C4, 
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson1", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = rep("ZIP1", 4)

RW = rep(1, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####RW2 ZIP1####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI, 
                family = "zeroinflatedpoisson1",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T,
                                       waic = T,
                                       config = T),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C2,
    constr = T,
    extraconstr = list(A = A2, e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII,
                 family = "zeroinflatedpoisson1",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C3,
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIII = inla(formula.intIII,
                  family = "zeroinflatedpoisson1", 
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C4,
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson1",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = rep("ZIP1", 4)

RW = rep(2, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####RW1 ZIP2####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI, 
                family = "zeroinflatedpoisson2",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, 
                                       waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C2,
    constr = T,
    extraconstr = list(A = A2, e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII,
                 family = "zeroinflatedpoisson2", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C3, 
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    
    )

mod.intIII = inla(formula.intIII, 
                  family = "zeroinflatedpoisson2",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C4,
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV,
                 family = "zeroinflatedpoisson2",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = rep("ZIP2", 4)

RW = rep(1, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####RW2 ZIP2####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C1,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intI = inla(formula.intI, 
                family = "zeroinflatedpoisson2", 
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C2,
    constr = T,
    extraconstr = list(A = A2, e = rep(1e-5, dim(A2)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson2",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0",
    Cmatrix = C3,
    constr = T,
    extraconstr = list(A = A3, e = rep(1e-5, dim(A3)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIII = inla(formula.intIII, 
                  family = "zeroinflatedpoisson2",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  f(IDTIEMPO, model = "generic0", 
    Cmatrix = C4, 
    constr = T,
    extraconstr = list(A = A4, e = rep(1e-5, dim(A4)[1])),
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )

mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson2",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = rep("ZIP2", 4)

RW = rep(2, 4)

Int = c("I","II","III","IV")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####PAR####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.par =  Casos ~ 1 + 
  
  f(ID, model = "bym2", 
    graph = H, 
    constr = T,
    scale.model = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(ID2, Tiempo, model = "iid",
    constr = T,
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) +
  
  Tiempo

mod.par = inla(formula.par, family = "poisson",
               data = BaseModelo, E = Ei,
               control.predictor = list(compute = T),
               control.compute = list(dic = T,
                                      cpo = T,
                                      return.marginals.predictor = T,
                                      po = T,
                                      waic = T,
                                      config = T),
               control.inla = list(strategy = "laplace"),
               control.fixed = list(mean.intercept = 0, 
                                    prec.intercept = 0.001,
                                    mean = 0,
                                    prec = 0.001))


mod.par1 = inla(formula.par, family = "zeroinflatedpoisson0",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = TRUE),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001,
                                     mean = 0,
                                     prec = 0.001))

mod.par2 = inla(formula.par, family = "zeroinflatedpoisson1",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = TRUE),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T,
                                       waic = T,
                                       config = T),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001,
                                     mean = 0,
                                     prec = 0.001))

mod.par3 = inla(formula.par, family = "zeroinflatedpoisson2",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = TRUE),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T,
                                       waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001,
                                     mean = 0,
                                     prec = 0.001))


Verosimilitud = c("Poisson", "ZIP0", "ZIP1", "ZIP2")

DIC = c(
  mod.par$dic$dic,
  mod.par1$dic$dic,
  mod.par2$dic$dic,
  mod.par3$dic$dic
)

P_d = c(
  mod.par$dic$p.eff,
  mod.par1$dic$p.eff,
  mod.par2$dic$p.eff,
  mod.par3$dic$p.eff
)

barD = c(
  mod.par$dic$mean.deviance,
  mod.par1$dic$mean.deviance,
  mod.par2$dic$mean.deviance,
  mod.par3$dic$mean.deviance
)

WAIC = c(
  mod.par$waic$waic,
  mod.par1$waic$waic,
  mod.par2$waic$waic,
  mod.par3$waic$waic
)

CPO = c(
  mod.par$cpo$cpo |> log() |> sum(),
  mod.par1$cpo$cpo |> log() |> sum(),
  mod.par2$cpo$cpo |> log() |> sum(),
  mod.par3$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####Sin Interaccion RW1####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1 ,0.01)))
  ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
  ) 

mod.intI = inla(formula.intI, 
                family = "poisson",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
  ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
  )

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson0", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
  ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
  ) 

mod.intIII = inla(formula.intIII, 
                  family = "zeroinflatedpoisson1",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, 
                                         waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
  ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
  ) 

mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson2", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = c("Poisson", "ZIP0", "ZIP1", "ZIP2")

RW = rep(1, 4)

Int = rep("NO", 4)

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####Sin Interaccion RW2####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2", graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intI = inla(formula.intI,
                family = "poisson",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, 
                                       waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intII = inla(formula.intII,
                 family = "zeroinflatedpoisson0",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )+
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intIII = inla(formula.intIII, 
                  family = "zeroinflatedpoisson1",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T,
                                         waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) +
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )


mod.intIV = inla(formula.intIV,
                 family = "zeroinflatedpoisson2",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = c("Poisson" , "ZIP0", "ZIP1", "ZIP2")

RW = rep(2, 4)

Int = rep("No", 4)

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, Int, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####Sin Temp####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(ID, model = "bym2",
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) 

mod.intI = inla(formula.intI, 
                family = "poisson", 
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T,
                                       waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) 

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson0",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  )

mod.intIII = inla(formula.intIII, 
                  family = "zeroinflatedpoisson1",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T,
                                         waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(ID, model = "bym2", 
    graph = H,
    scale.model = T,
    constr = T,
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)),
      prec = list(prior = "pc.prec", param = c(0.75, 0.05)))
  ) 


mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson2", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = c("Poisson" , "ZIP0", "ZIP1", "ZIP2")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####Sin Esp RW1####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(Tiempo, 
    model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intI = inla(formula.intI, 
                family = "poisson", 
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson0", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(Tiempo, model = "rw1",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intIII = inla(formula.intIII, 
                  family = "zeroinflatedpoisson1",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(Tiempo, model = "rw1", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )


mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson2", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = c("Poisson" , "ZIP0", "ZIP1", "ZIP2")

RW = rep(1, 4)

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()

####Sin Esp RW2####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 +
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intI = inla(formula.intI, 
                family = "poisson", 
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, 
                                       waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

formula.intII = Casos ~ 1 +
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intII = inla(formula.intII, 
                 family = "zeroinflatedpoisson0",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

formula.intIII = Casos ~ 1 +
  
  f(Tiempo, model = "rw2", 
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    ) 

mod.intIII = inla(formula.intIII,
                  family = "zeroinflatedpoisson1",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T, waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

formula.intIV = Casos ~ 1 +
  
  f(Tiempo, model = "rw2",
    scale.model = T,
    constr = T,
    hyper = list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
    ) +
  
  f(Tiempo2, model = "iid",
    hyper = list(theta = list(prior = "loggamma", param = c(1, 5e-05)))
    )


mod.intIV = inla(formula.intIV, 
                 family = "zeroinflatedpoisson2", 
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = c("Poisson" , "ZIP0", "ZIP1", "ZIP2")

RW = rep(2, 4)

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, RW, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()


####Modelos Nulos####

rm(list = ls())

set.seed(1)

library(tidyverse)
library(INLA)

load("Base de Datos para Modelo.Rdata")

BaseModelo = BaseModelo |>
  separate(col = "AñoMes", into = c("Año","Mes"), sep = "-") |>
  mutate(Año = as.numeric(Año)) |>
  filter(Año > 2011)

BaseModelo$Tiempo = BaseModelo$Tiempo - 120
BaseModelo$Tiempo2 = BaseModelo$Tiempo2 - 120
BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)

BaseModelo = BaseModelo |> mutate(AñoMes = paste(Año,Mes,sep = "-")) |> select(-c(Año,Mes))

load("A1p.Rdata")
load("C1p.Rdata")
load("A2p.Rdata")
load("C2p.Rdata")
load("A3p.Rdata")
load("C3p.Rdata")
load("A4p.Rdata")
load("C4p.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

H = inla.read.graph("VecindadProvSantiago.graph")

ID.area.int = BaseModelo$ID
ID.year.int = BaseModelo$Tiempo

BaseModelo$IDTIEMPO = 1:nrow(BaseModelo)


formula.intI =  Casos ~ 1 

mod.intI = inla(formula.intI, 
                family = "poisson",
                data = BaseModelo, E = Ei,
                control.predictor = list(compute = T),
                control.compute = list(dic = T,
                                       cpo = T,
                                       return.marginals.predictor = T,
                                       po = T, 
                                       waic = T,
                                       config = T),
                control.inla = list(strategy = "laplace"),
                control.fixed = list(mean.intercept = 0, 
                                     prec.intercept = 0.001))

mod.intII = inla(formula.intI,
                 family = "zeroinflatedpoisson0",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

mod.intIII = inla(formula.intI, 
                  family = "zeroinflatedpoisson1",
                  data = BaseModelo, E = Ei,
                  control.predictor = list(compute = T),
                  control.compute = list(dic = T,
                                         cpo = T,
                                         return.marginals.predictor = T,
                                         po = T,
                                         waic = T,
                                         config = T),
                  control.inla = list(strategy = "laplace"),
                  control.fixed = list(mean.intercept = 0, 
                                       prec.intercept = 0.001))

mod.intIV = inla(formula.intI, 
                 family = "zeroinflatedpoisson2",
                 data = BaseModelo, E = Ei,
                 control.predictor = list(compute = T),
                 control.compute = list(dic = T,
                                        cpo = T,
                                        return.marginals.predictor = T,
                                        po = T, 
                                        waic = T,
                                        config = T),
                 control.inla = list(strategy = "laplace"),
                 control.fixed = list(mean.intercept = 0, 
                                      prec.intercept = 0.001))

Verosimilitud = c("Poisson" , "ZIP0", "ZIP1", "ZIP2")

DIC = c(
  mod.intI$dic$dic,
  mod.intII$dic$dic,
  mod.intIII$dic$dic,
  mod.intIV$dic$dic
)

P_d = c(
  mod.intI$dic$p.eff,
  mod.intII$dic$p.eff,
  mod.intIII$dic$p.eff,
  mod.intIV$dic$p.eff
)

barD = c(
  mod.intI$dic$mean.deviance,
  mod.intII$dic$mean.deviance,
  mod.intIII$dic$mean.deviance,
  mod.intIV$dic$mean.deviance
)

WAIC = c(
  mod.intI$waic$waic,
  mod.intII$waic$waic,
  mod.intIII$waic$waic,
  mod.intIV$waic$waic
)

CPO = c(
  mod.intI$cpo$cpo |> log() |> sum(),
  mod.intII$cpo$cpo |> log() |> sum(),
  mod.intIII$cpo$cpo |> log() |> sum(),
  mod.intIV$cpo$cpo |> log() |> sum()
)

cbind(Verosimilitud, DIC, P_d, barD, WAIC, CPO) |>
  data.frame() |> 
  mutate_at(vars(DIC:CPO), as.numeric) |> 
  xtable::xtable()
