## -----------------------------------------------------------------------------------------------------------------
library(pROC)
library(dplyr)
library(car)
library(marginaleffects)
library(broom)
library(margins)
library(ggplot2)
library(forcats)
library(modelsummary)


## -----------------------------------------------------------------------------------------------------------------
df <- read.csv("prado_variables.csv", stringsAsFactors = TRUE)

df$sop_montaje <- as.factor(df$sop_montaje)
df$serie <- as.factor(df$serie)

str(df)


## -----------------------------------------------------------------------------------------------------------------
# 0) Regla estructural: excluir "cuadrado"
df <- subset(df, orientacion != "cuadrado")
df$orientacion <- droplevels(df$orientacion)
dim(df)


## -----------------------------------------------------------------------------------------------------------------
# 1) Niveles de los factores binarios 
df$sop_montaje <- factor(df$sop_montaje, levels = c(0, 1), labels = c("no", "si"))
df$serie       <- factor(df$serie,       levels = c(0, 1), labels = c("no", "si"))

# 2) tam_cat nominal con referencia "pequeno"
df$tam_cat <- factor(df$tam_cat, levels = c("pequeno", "mediano", "grande"))
df$tam_cat <- relevel(df$tam_cat, ref = "pequeno")

# 3) Binarios con referencia "no"
df$sop_montaje <- relevel(df$sop_montaje, ref = "no")
df$serie       <- relevel(df$serie, ref = "no")

# 4) Referencias = nivel más frecuente (para el resto de factores)
ref_orientacion <- names(sort(table(df$orientacion), decreasing = TRUE))[1]
df$orientacion  <- relevel(df$orientacion, ref = ref_orientacion)

ref_soporte <- names(sort(table(df$soporte_grp), decreasing = TRUE))[1]
df$soporte_grp <- relevel(df$soporte_grp, ref = ref_soporte)

ref_tecnica <- names(sort(table(df$tecnica), decreasing = TRUE))[1]
df$tecnica  <- relevel(df$tecnica, ref = ref_tecnica)

ref_autor <- names(sort(table(df$tipo_autor), decreasing = TRUE))[1]
df$tipo_autor <- relevel(df$tipo_autor, ref = ref_autor)

ref_tema <- names(sort(table(df$tema), decreasing = TRUE))[1]
df$tema <- relevel(df$tema, ref = ref_tema)

str(df)


## -----------------------------------------------------------------------------------------------------------------
# Modelo nulo (solo intercepto)
m0 <- glm(exito ~ 1, data = df, family = binomial(link = "logit"))

# Resumen del modelo
cat("\nResumen del modelo:\n")
summary(m0)

# Probabilidad media estimada de éxito (a partir del intercepto)
cat("\nProbabilidad estimada de éxito:\n")
p0 <- plogis(coef(m0)[1])
p0


## -----------------------------------------------------------------------------------------------------------------
library(splines)

# Transformación log1p(fecha_ancho)
df$log_ancho <- log1p(df$fecha_ancho)

# Modelo A: control lineal en log_ancho
m1_lin <- glm(exito ~ ns(fecha_est, 3) + log_ancho,
              data = df, family = binomial(link = "logit"))

# Modelo B: control flexible (spline) en log_ancho
m1_spl <- glm(exito ~ ns(fecha_est, 3) + ns(log_ancho, 3),
              data = df, family = binomial(link = "logit"))

# Resúmenes (por si aparecen warnings / coeficientes raros / SE enormes)
cat("\nResumen del modelo m1_lin (control lineal):\n")
summary(m1_lin)
cat("\nResumen del modelo m1_spl (control flexible spline):\n")
summary(m1_spl)


## -----------------------------------------------------------------------------------------------------------------
# Comparación --> aporte del Bloque 1

# Información
cat("\nAporte de información del Bloque 1 (control lineal):\n")
anova(m0, m1_lin, test="Chisq")
cat("\nAporte de información del Bloque 1 (control flexible spline):\n")
anova(m0, m1_spl, test="Chisq")

# Complejidad
cat("\nAporte de complejidad:\n")
AIC(m0, m1_lin, m1_spl)
BIC(m0, m1_lin, m1_spl)


## -----------------------------------------------------------------------------------------------------------------
# Comparación --> formas funcionales
cat("\nAporte de información entre opciones (m1_lin vs m1_spl)\n")
anova(m1_lin, m1_spl, test="Chisq")


## -----------------------------------------------------------------------------------------------------------------
m1_lin_fecha <- glm(exito ~ fecha_est + log_ancho,
              data = df, family = binomial(link = "logit"))


## -----------------------------------------------------------------------------------------------------------------
# Comparación
# Información
cat("\nAporte de la opción fecha_est flexible:\n")
anova(m1_lin_fecha, m1_lin, test="Chisq")

# Complejidad
cat("\nComplejidad de la opción fecha_est flexible:\n")
AIC(m1_lin,m1_lin_fecha)
BIC(m1_lin,m1_lin_fecha)


## -----------------------------------------------------------------------------------------------------------------
m1 <- m1_lin_fecha


## -----------------------------------------------------------------------------------------------------------------
# Trnasformación log(area)
df$log_area <- log(df$area)

# BLOQUE 2 (principal candidato): log(area) + orientacion
m2_area <- update(m1, . ~ . + log_area + orientacion)

# BLOQUE 2 (secundario): tam_cat + orientacion
m2_tamcat <- update(m1, . ~ . + tam_cat + orientacion)

# Resúmenes (por si aparecen warnings / coeficientes raros / SE enormes)
cat("\nResumen del modelo m2_area (+ log_area + orientacion):\n")
summary(m2_area)

cat("\nResumen del modelo m2_tamcat (+ tam_cat + orientacion):\n")
summary(m2_tamcat)

# Comparación --> aporte del Bloque 2

# Información
cat("\nAporte de información del Bloque 2 (tamaño continuo):\n")
anova(m1, m2_area, test="Chisq")

cat("\nAporte de información del Bloque 2 (tamaño categórico):\n")
anova(m1, m2_tamcat, test="Chisq")

# Complejidad
cat("\nAporte de complejidad:\n")
AIC(m1, m2_area, m2_tamcat)
BIC(m1, m2_area, m2_tamcat)


## -----------------------------------------------------------------------------------------------------------------
# Version flexible de log(area)
m2_area_spl <- update(m1, . ~ . + splines::ns(log_area, 3) + orientacion)

# Resúmenes (por si aparecen warnings / coeficientes raros / SE enormes)
summary(m2_area_spl)

# Comparaciones
cat("\nAporte de la opción log(area) flexible")
anova(m1, m2_area_spl, test="Chisq")

cat("\nComplejidad de la opción log(area) flexible")
AIC(m2_area, m2_area_spl, m2_tamcat)
BIC(m2_area, m2_area_spl, m2_tamcat)


## -----------------------------------------------------------------------------------------------------------------
m2 <- m2_tamcat


## -----------------------------------------------------------------------------------------------------------------
# BLOQUE 3 (versión base): soporte + técnica
m3_base <- update(m2, . ~ . + soporte_grp + tecnica)

# Resúmenes (por si aparecen warnings / coeficientes raros / SE enormes)
cat("\nResumen del modelo m3_base (soporte_grp + tecnica):\n")
summary(m3_base)

# Comparación--> aporte del Bloque 3

# Información
cat("\nAporte de información del Bloque 3 (base):\n")
anova(m2, m3_base, test = "Chisq")

# Complejidad
cat("\nAporte de complejidad:\n")
AIC(m2, m3_base)
BIC(m2, m3_base)


## -----------------------------------------------------------------------------------------------------------------
# BLOQUE 3 (extendido): + sop_montaje
m3_montaje <- update(m3_base, . ~ . + sop_montaje)

# Resúmenes (por si aparecen warnings / coeficientes raros / SE enormes)
cat("\nResumen del modelo m3_montaje (+ sop_montaje):\n")
summary(m3_montaje)

# Comparación--> aporte de sop_montaje

# Información
cat("\nAporte de informacion de sop_montaje (m3_montaje vs m3_base):\n")
anova(m3_base, m3_montaje, test = "Chisq")

cat("\nComplejidad de sop_montaje:\n")
AIC(m2, m3_base, m3_montaje)
BIC(m2, m3_base, m3_montaje)


## -----------------------------------------------------------------------------------------------------------------
m3 <- m3_montaje


## -----------------------------------------------------------------------------------------------------------------
# BLOQUE 4: + tema
m4 <- update(m3, . ~ . + tema)

# Resumen (por si aparecen warnings / coeficientes raros / SE enormes)
cat("\nResumen del modelo m4 (+ tema):\n")
summary(m4)

# Comparación--> aporte del Bloque 4
# Información
cat("\nAporte de información del Bloque 4:\n")
anova(m3, m4, test = "Chisq")

# Complejidad
cat("\nAporte de complejidad:\n")
AIC(m3, m4)
BIC(m3, m4)


## -----------------------------------------------------------------------------------------------------------------
# BLOQUE 5: + tipo_autor + serie
m5 <- update(m3, . ~ . + tipo_autor + serie)

# Resumen (por si aparecen warnings / coeficientes raros / SE enormes)
cat("\nResumen del modelo m5 (+ tipo_autor + serie):\n")
summary(m5)

# Comparación--> aporte del Bloque 5
# Información
cat("\nAporte de información del Bloque 5:\n")
anova(m3, m5, test = "Chisq")

# Complejidad
cat("\nAporte de complejidad:\n")
AIC(m3, m5)
BIC(m3, m5)



## -----------------------------------------------------------------------------------------------------------------
# modelos parciales del Bloque 5
m5_serie <- update(m3, . ~ . + serie)
m5_autor <- update(m3, . ~ . + tipo_autor)

# Comparaciones --> aportes de los mdoelos parciales del Bloque 5
# Información
cat("\nAporte de información de 'serie':\n")
anova(m3, m5_serie, test="Chisq")
cat("\nAporte de información de 'tipo_autor':\n")
anova(m3, m5_autor, test="Chisq")

# Complejidad
cat("\nAporte de complejidad:\n")
AIC(m3, m5_serie, m5_autor, m5)
BIC(m3, m5_serie, m5_autor, m5)


## -----------------------------------------------------------------------------------------------------------------
m5 <- m5_serie


## -----------------------------------------------------------------------------------------------------------------
m_final <- glm(
  exito ~ fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie,
  data = df, family = binomial(link = "logit")
)
summary(m_final)


## -----------------------------------------------------------------------------------------------------------------
# niveles de referencia factores ([1] porque ya se especificó al inicio del análisis)
ref_tam_cat      <- levels(df$tam_cat)[1]
ref_orientacion  <- levels(df$orientacion)[1]
ref_soporte_grp  <- levels(df$soporte_grp)[1]
ref_tecnica      <- levels(df$tecnica)[1]
ref_sop_montaje  <- levels(df$sop_montaje)[1]
ref_serie        <- levels(df$serie)[1]

# niveles de referencia numéricas (mediana)
fecha0     <- median(df$fecha_est)
log_ancho0 <- median(df$log_ancho)


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# 1) soporte_grp × fecha_est
# =========================================================
m_int_SopFecha <- glm(
  exito ~ fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie +
    soporte_grp:fecha_est,
  data = df, family = binomial(link = "logit")
)

x <- seq(min(df$fecha_est, na.rm = TRUE), max(df$fecha_est, na.rm = TRUE), length.out = 200)
lev <- levels(df$soporte_grp)
cols <- grDevices::hcl.colors(length(lev), palette = "Dark 3")

nd <- expand.grid(
  fecha_est = x,
  soporte_grp = lev,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

nd$log_ancho   <- log_ancho0
nd$tam_cat     <- ref_tam_cat
nd$orientacion <- ref_orientacion
nd$tecnica     <- ref_tecnica
nd$sop_montaje <- ref_sop_montaje
nd$serie       <- ref_serie

eta <- predict(m_int_SopFecha, newdata = nd, type = "link")

plot(range(x), range(eta), type = "n",
     xlab = "fecha_est", ylab = "logit{P(exito=1)}",
     main = "Interacción: soporte_grp × fecha_est (escala logit)")

for (i in seq_along(lev)) {
  idx <- nd$soporte_grp == lev[i]
  lines(nd$fecha_est[idx], eta[idx], col = cols[i], lty = 1, lwd = 2)
}
legend("bottomright", legend = lev, col = cols, lty = 1, lwd = 2, bty = "n")


## -----------------------------------------------------------------------------------------------------------------
# Rangos observados por soporte
tapply(df$fecha_est, df$soporte_grp, range)

# Cortes de fecha_cat
grp_fecha <- cut(df$fecha_est,
breaks = c(1100, 1400, 1700, 2000),
include.lowest = TRUE)

cat("\nFrecuencias por combinación:\n")
xtabs(~ soporte_grp + grp_fecha, data = df)

cat("\nRecuento de éxitos por combinación:\n")
xtabs(exito ~ soporte_grp + grp_fecha, data = df)


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# 2) tecnica × fecha_est
# =========================================================
m_int_TecFecha <- glm(
  exito ~ fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie +
    tecnica:fecha_est,
  data = df, family = binomial(link = "logit")
)

x <- seq(min(df$fecha_est, na.rm = TRUE), max(df$fecha_est, na.rm = TRUE), length.out = 200)
lev <- levels(df$tecnica)
cols <- grDevices::hcl.colors(length(lev), palette = "Dark 3")

nd <- expand.grid(
  fecha_est = x,
  tecnica = lev,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

nd$log_ancho   <- log_ancho0
nd$tam_cat     <- ref_tam_cat
nd$orientacion <- ref_orientacion
nd$soporte_grp <- ref_soporte_grp
nd$sop_montaje <- ref_sop_montaje
nd$serie       <- ref_serie

eta <- predict(m_int_TecFecha, newdata = nd, type = "link")

plot(range(x), range(eta), type = "n",
     xlab = "fecha_est", ylab = "logit{P(exito=1)}",
     main = "Interacción: tecnica × fecha_est (escala logit)")

for (i in seq_along(lev)) {
  idx <- nd$tecnica == lev[i]
  lines(nd$fecha_est[idx], eta[idx], col = cols[i], lty = 1, lwd = 2)
}
legend("bottomright", legend = lev, col = cols, lty = 1, lwd = 2, bty = "n")


## -----------------------------------------------------------------------------------------------------------------
cat("\nFrecuencias por combinación:\n")
xtabs(~ tecnica + grp_fecha, data=df)

cat("\nRecuento de éxitos por combinación:\n")
xtabs(exito ~ tecnica + grp_fecha, data=df)


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# 3) tecnica × tam_cat
# =========================================================
m_int_TecTam <- glm(
  exito ~ fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie +
    tecnica:tam_cat,
  data = df, family = binomial(link = "logit")
)

nd <- expand.grid(
  tam_cat = levels(df$tam_cat),
  tecnica = levels(df$tecnica),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

nd$fecha_est    <- fecha0
nd$log_ancho    <- log_ancho0
nd$orientacion  <- ref_orientacion
nd$soporte_grp  <- ref_soporte_grp
nd$sop_montaje  <- ref_sop_montaje
nd$serie        <- ref_serie

nd$eta <- predict(m_int_TecTam, newdata = nd, type = "link")

interaction.plot(x.factor = nd$tam_cat, trace.factor = nd$tecnica,
                 response = nd$eta, type = "b", pch = 19,
                 xlab = "tam_cat", ylab = "logit{P(exito=1)} ajustada",
                 main = "Interacción: tecnica × tam_cat (escala logit)")


## -----------------------------------------------------------------------------------------------------------------
cat("\nFrecuencias por combinación:\n")
xtabs(~ tecnica + tam_cat, data=df)

cat("\nRecuento de éxitos por combinación:\n")
xtabs(exito ~ tecnica + tam_cat, data=df)


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# 4) soporte_grp × tam_cat
# =========================================================
m_int_SopTam <- glm(
  exito ~ fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie +
    soporte_grp:tam_cat,
  data = df, family = binomial(link = "logit")
)

nd <- expand.grid(
  tam_cat     = levels(df$tam_cat),
  soporte_grp = levels(df$soporte_grp),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

nd$fecha_est    <- fecha0
nd$log_ancho    <- log_ancho0
nd$orientacion  <- ref_orientacion
nd$tecnica      <- ref_tecnica
nd$sop_montaje  <- ref_sop_montaje
nd$serie        <- ref_serie

nd$eta <- predict(m_int_SopTam, newdata = nd, type = "link")

interaction.plot(x.factor = nd$tam_cat, trace.factor = nd$soporte_grp,
                 response = nd$eta, type = "b", pch = 19,
                 xlab = "tam_cat", ylab = "logit{P(exito=1)} ajustada",
                 main = "Interacción: soporte_grp × tam_cat (escala logit)")


## -----------------------------------------------------------------------------------------------------------------
cat("\nFrecuencias por combinación:\n")
xtabs(~ soporte_grp + tam_cat, data=df)

cat("\nRecuento de éxitos por combinación:\n")
xtabs(exito ~ soporte_grp + tam_cat, data=df)


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# 5) soporte_grp × orientacion
# =========================================================
m_int_SopOri <- glm(
  exito ~ fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie +
    soporte_grp:orientacion,
  data = df, family = binomial(link = "logit")
)

nd <- expand.grid(
  orientacion = levels(df$orientacion),
  soporte_grp = levels(df$soporte_grp),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

nd$fecha_est    <- fecha0
nd$log_ancho    <- log_ancho0
nd$tam_cat      <- ref_tam_cat
nd$tecnica      <- ref_tecnica
nd$sop_montaje  <- ref_sop_montaje
nd$serie        <- ref_serie

nd$eta <- predict(m_int_SopOri, newdata = nd, type = "link")

interaction.plot(x.factor = nd$orientacion, trace.factor = nd$soporte_grp,
                 response = nd$eta, type = "b", pch = 19,
                 xlab = "orientacion", ylab = "logit{P(exito=1)} ajustada",
                 main = "Interacción: soporte_grp × orientacion (escala logit)")



## -----------------------------------------------------------------------------------------------------------------
cat("\nFrecuencias por combinación:\n")
xtabs(~ soporte_grp + orientacion, data=df)

cat("\nRecuento de éxitos por combinación:\n")
xtabs(exito ~ soporte_grp + orientacion, data=df)


## -----------------------------------------------------------------------------------------------------------------
# soporte_grp x tam_cat
cat("\n==============================\n soporte_grp x tam_cat \n==============================\n")
m_soporte_tamcat <- update(m_final, . ~ . + soporte_grp:tam_cat)
anova(m_final, m_soporte_tamcat, test = "Chisq")
AIC(m_final, m_soporte_tamcat)
BIC(m_final, m_soporte_tamcat)

# soporte_grp x orientacion
cat("\n==============================\n soporte_grp x orientacion \n==============================\n")
m_soporte_orientacion <- update(m_final, . ~ . + soporte_grp:orientacion)
anova(m_final, m_soporte_orientacion, test = "Chisq")
AIC(m_final, m_soporte_orientacion)
BIC(m_final, m_soporte_orientacion)


## -----------------------------------------------------------------------------------------------------------------
m_completo <- update(m_soporte_tamcat, . ~ . + soporte_grp:orientacion)

anova(m_final, m_soporte_tamcat, test = "Chisq")
AIC(m_final, m_soporte_tamcat)
BIC(m_final, m_soporte_tamcat)


## -----------------------------------------------------------------------------------------------------------------
m_completo <- glm(
  exito ~
    fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie +
    soporte_grp:tam_cat,
  data = df,
  family = binomial(link = "logit")
)


## -----------------------------------------------------------------------------------------------------------------
m_reducido <- glm(
  exito ~
    fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie,
  data = df,
  family = binomial(link = "logit")
)


## -----------------------------------------------------------------------------------------------------------------
summary(m_completo)


## -----------------------------------------------------------------------------------------------------------------
y <- model.response(model.frame(m_completo))
E <- sum(y == 1)
N <- length(y)
p <- length(coef(m_completo))

cat("N =", N, "  Eventos (1) =", E, "  Prevalencia =", round(E/N, 4), "\n")
cat("Num coeficientes (incluye dummies) =", p, "\n")
cat("EPV aprox (eventos por coef) =", round(E/p, 3), "\n")



## -----------------------------------------------------------------------------------------------------------------
Y <- xtabs(exito ~ soporte_grp + tam_cat, data = df)
N <- xtabs(~ soporte_grp + tam_cat, data = df)
P <- Y / N

cat("\nFrecuencias de soporte\n")
table(df$soporte_grp)
cat("\nFrecuencias de tamaño\n")
table(df$tam_cat)

cat("\nÉxitos Y:\n"); print(Y)
cat("\nTotales N:\n"); print(N)
cat("\nProporción P=Y/N:\n"); print(round(P, 3))

cat("\nCeldas con 0 éxitos (Y==0):\n")
print((Y == 0) & (N > 0))

cat("\nCeldas con todos éxitos (Y==N):\n")
print((Y == N) & (N > 0))


## -----------------------------------------------------------------------------------------------------------------
library(car)
vif(m_completo)


## -----------------------------------------------------------------------------------------------------------------
n <- nobs(m_completo)
p <- length(coef(m_completo))

cat("Devianza ajustada:",deviance(m_completo) / (n - p))


## -----------------------------------------------------------------------------------------------------------------
df2 <- df
df2$exito <- if (is.factor(df2$exito)) as.integer(df2$exito == levels(df2$exito)[2]) else as.integer(df2$exito)

m_df2 <- update(m_completo, data = df2)

X <- model.matrix(m_df2)
y <- model.response(model.frame(m_df2))
ds <- detectseparation::detect_separation(X, y)
print(ds)


## -----------------------------------------------------------------------------------------------------------------
library(brglm2)

m_firth <- glm(
  formula(m_completo),
  data   = df,
  family = binomial("logit"),
  method = "brglmFit",
)

summary(m_firth)


## -----------------------------------------------------------------------------------------------------------------
library(brglm2)

ctrl <- brglm2::brglmControl(
  maxit = 2000,          # + iteraciones (default 100)
  slowit = 0.5,          # pasos más pequeños
  response_adjustment = 0.5  # arranque con ajuste tipo 0.5 en binomial
)

m_firth2 <- glm(
  formula(m_completo),
  data   = df,
  family = binomial("logit"),
  method = "brglmFit",
  type   = "AS_mean",
  control = ctrl
)

cat("Convergencia:", m_firth2$converged)
cat("\nNúmero de iteraciones:", m_firth2$iter)
summary(m_firth2)



## -----------------------------------------------------------------------------------------------------------------
se_ml <- summary(m_completo)$coefficients[,2]
se_fi <- summary(m_firth2)$coefficients[,2]

cat("\nTop 10 SE (MLE):\n")
print(head(sort(se_ml, decreasing=TRUE), 10))

cat("\nTop 10 SE (Firth/AS):\n")
print(head(sort(se_fi, decreasing=TRUE), 10))

cat("Separación:\n")
m_firth2_df2 <- update(m_firth2, data = df2)

X <- model.matrix(m_firth2_df2)
y <- model.response(model.frame(m_firth2_df2))
ds <- detectseparation::detect_separation(X, y)
print(ds)


## -----------------------------------------------------------------------------------------------------------------
library(car)
vif(m_firth2)


## -----------------------------------------------------------------------------------------------------------------
ctrl <- brglm2::brglmControl(
  maxit = 2000,          
  slowit = 0.5,          
  response_adjustment = 0.5  
)

m_principal <- glm(
  formula(exito ~
    fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie +
    soporte_grp:tam_cat),
  data   = df,
  family = binomial("logit"),
  method = "brglmFit",
  type   = "AS_mean",
  control = ctrl
)

summary(m_principal)


## -----------------------------------------------------------------------------------------------------------------
summary(m_principal)$null.deviance
summary(m_principal)$deviance
nobs(m_principal)
# Pseudo R² de McFadden
1 - summary(m_principal)$deviance / summary(m_principal)$null.deviance



## -----------------------------------------------------------------------------------------------------------------
# Probabilidades predichas
prob_principal <- predict(m_principal, type = "response")

# Curva ROC
roc_principal <- roc(df$exito, prob_principal)

# AUC
auc(roc_principal)

# Gráfico ROC
plot(roc_principal,
     main = "Curva ROC – Modelo principal",
     col = "black",
     lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")


## -----------------------------------------------------------------------------------------------------------------
res_pearson <- residuals(m_principal, type = "pearson")
summary(res_pearson)

plot(prob_principal, res_pearson,
     xlab = "Probabilidad predicha",
     ylab = "Residuos de Pearson",
     main = "Residuos de Pearson vs probabilidad predicha")
abline(h = 0, lty = 2)



## -----------------------------------------------------------------------------------------------------------------
cook <- cooks.distance(m_principal)
plot(cook, type = "h",
     main = "Distancia de Cook – Modelo principal",
     ylab = "Cook's distance")
abline(h = 4 / length(cook), lty = 2, col = "red")

## -----------------------------------------------------------------------------------------------------------------
df %>%
  count(tam_cat, soporte_grp, exito) %>%
  tidyr::pivot_wider(names_from = exito, values_from = n, values_fill = 0)


## -----------------------------------------------------------------------------------------------------------------
m_reducido <- glm(
  exito ~
    fecha_est + log_ancho +
    tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje +
    serie,
  data = df,
  family = binomial(link = "logit")
)


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# MODELOS ALTERNATIVOS CON TEMA (resumen compacto)

# =========================================================
# 1) ESPECIFICACIÓN DE MODELOS (sin summary)
# =========================================================
cat("\n==============================\n1) ESPECIFICACION DE MODELOS\n==============================\n")

# Modelo final principal (sin tema, con Bloque 3, tamaño categórico)
m_final <- glm(
  exito ~ ns(fecha_est, 3) + log_ancho + tam_cat + orientacion +
    soporte_grp + tecnica + sop_montaje + serie,
  data = df, family = binomial(link = "logit")
)
cat("\n[m_final] creado: ajuste completo + tam_cat (SIN tema)\n")

# Ajuste completo + tema (tam_cat)
mT_full_tam <- update(m_final, . ~ . + tema)
cat("[mT_full_tam] creado: ajuste completo + tam_cat (CON tema)\n")

# Modelo final equivalente con log_area (sin tema)
m_final_area <- glm(
  exito ~ ns(fecha_est, 3) + log_ancho + log_area + orientacion +
    soporte_grp + tecnica + sop_montaje + serie,
  data = df, family = binomial(link = "logit")
)
cat("[m_final_area] creado: ajuste completo + log_area (SIN tema)\n")

# Ajuste completo + tema (log_area)
mT_full_area <- update(m_final_area, . ~ . + tema)
cat("[mT_full_area] creado: ajuste completo + log_area (CON tema)\n")

# Sin Bloque 3 + tam_cat (sin tema)
m_noB3_tam <- glm(
  exito ~ ns(fecha_est, 3) + log_ancho + tam_cat + orientacion + serie,
  data = df, family = binomial(link = "logit")
)
cat("[m_noB3_tam] creado: SIN Bloque 3 + tam_cat (SIN tema)\n")

# Sin Bloque 3 + tema (tam_cat)
mT_noB3_tam <- update(m_noB3_tam, . ~ . + tema)
cat("[mT_noB3_tam] creado: SIN Bloque 3 + tam_cat (CON tema)\n")

# Sin Bloque 3 + log_area (sin tema)
m_noB3_area <- glm(
  exito ~ ns(fecha_est, 3) + log_ancho + log_area + orientacion + serie,
  data = df, family = binomial(link = "logit")
)
cat("[m_noB3_area] creado: SIN Bloque 3 + log_area (SIN tema)\n")

# Sin Bloque 3 + tema (log_area)
mT_noB3_area <- update(m_noB3_area, . ~ . + tema)
cat("[mT_noB3_area] creado: SIN Bloque 3 + log_area (CON tema)\n")

# =========================================================
# 2) COMPARACIONES ANOVA (mínimas, aporte de informacion)
# =========================================================
cat("\n==============================\n2) COMPARACIONES (LRT - ANOVA)\n==============================\n")

cat("\n[2.1] Aporte de tema (ajuste completo, tam_cat): m_final vs mT_full_tam\n")
print(anova(m_final, mT_full_tam, test = "Chisq"))

cat("\n[2.2] Aporte de tema (ajuste completo, log_area): m_final_area vs mT_full_area\n")
print(anova(m_final_area, mT_full_area, test = "Chisq"))

cat("\n[2.3] Aporte de tema (SIN Bloque 3, tam_cat): m_noB3_tam vs mT_noB3_tam\n")
print(anova(m_noB3_tam, mT_noB3_tam, test = "Chisq"))

cat("\n[2.4] Aporte de tema (SIN Bloque 3, log_area): m_noB3_area vs mT_noB3_area\n")
print(anova(m_noB3_area, mT_noB3_area, test = "Chisq"))


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# 3) TABLA AIC/BIC (todos los modelos)
# =========================================================
cat("\n==============================\n3) TABLA AIC / BIC\n==============================\n")

mods <- list(
  m_final      = m_final,
  mT_full_tam  = mT_full_tam,
  m_final_area = m_final_area,
  mT_full_area = mT_full_area,
  m_noB3_tam   = m_noB3_tam,
  mT_noB3_tam  = mT_noB3_tam,
  m_noB3_area  = m_noB3_area,
  mT_noB3_area = mT_noB3_area
)

ic_tab <- data.frame(
  modelo = names(mods),
  df     = sapply(mods, function(m) attr(logLik(m), "df")),
  AIC    = sapply(mods, AIC),
  BIC    = sapply(mods, BIC),
  row.names = NULL
)

print(ic_tab[order(ic_tab$AIC), ])


## -----------------------------------------------------------------------------------------------------------------
# =========================================================
# Dataframe de deltas (con tema - sin tema) para AIC y BIC
# =========================================================

delta_ic <- data.frame(
  situacion = c(
    "Ajuste completo + tam_cat",
    "Ajuste completo + log_area",
    "Sin Bloque 3 + tam_cat",
    "Sin Bloque 3 + log_area"
  ),
  modelo_sin_tema = c("m_final", "m_final_area", "m_noB3_tam", "m_noB3_area"),
  modelo_con_tema = c("mT_full_tam", "mT_full_area", "mT_noB3_tam", "mT_noB3_area"),
  AIC_sin = c(AIC(m_final),      AIC(m_final_area),  AIC(m_noB3_tam),  AIC(m_noB3_area)),
  AIC_con = c(AIC(mT_full_tam),  AIC(mT_full_area),  AIC(mT_noB3_tam), AIC(mT_noB3_area)),
  BIC_sin = c(BIC(m_final),      BIC(m_final_area),  BIC(m_noB3_tam),  BIC(m_noB3_area)),
  BIC_con = c(BIC(mT_full_tam),  BIC(mT_full_area),  BIC(mT_noB3_tam), BIC(mT_noB3_area)),
  stringsAsFactors = FALSE
)

# Deltas: (con tema - sin tema)
delta_ic$delta_AIC <- delta_ic$AIC_con - delta_ic$AIC_sin
delta_ic$delta_BIC <- delta_ic$BIC_con - delta_ic$BIC_sin

# (Opcional) Redondeo para informe
delta_ic$AIC_sin   <- round(delta_ic$AIC_sin, 3)
delta_ic$AIC_con   <- round(delta_ic$AIC_con, 3)
delta_ic$BIC_sin   <- round(delta_ic$BIC_sin, 3)
delta_ic$BIC_con   <- round(delta_ic$BIC_con, 3)
delta_ic$delta_AIC <- round(delta_ic$delta_AIC, 3)
delta_ic$delta_BIC <- round(delta_ic$delta_BIC, 3)

# Mostrar tabla ordenada (por mejora AIC: más negativo = mejor)
delta_ic <- delta_ic[order(delta_ic$delta_AIC), ]

delta_ic
delta_ic[c("situacion", "delta_AIC", "delta_BIC")]


## -----------------------------------------------------------------------------------------------------------------
# modelo reducido (sin interacciones)
mA1 <- m_reducido

# tema ajustado
mA2 <- mT_full_tam

# tema global
mA3 <- mT_noB3_tam


## -----------------------------------------------------------------------------------------------------------------
cat("Separación para mA1 (sin interacciones):\n")
mA1_df2 <- update(mA1, data = df2)

X <- model.matrix(mA1_df2)
y <- model.response(model.frame(mA1_df2))
ds <- detectseparation::detect_separation(X, y)
print(ds)

cat("Separación para mA2 (tema ajustado):\n")
mA2_df2 <- update(mA2, data = df2)

X <- model.matrix(mA2_df2)
y <- model.response(model.frame(mA2_df2))
ds <- detectseparation::detect_separation(X, y)
print(ds)

cat("Separación para mA3 (tema global):\n")
mA3_df2 <- update(mA3, data = df2)

X <- model.matrix(mA3_df2)
y <- model.response(model.frame(mA3_df2))
ds <- detectseparation::detect_separation(X, y)
print(ds)


## -----------------------------------------------------------------------------------------------------------------
prob_mA1 <- predict(mA1, type = "response")
prob_mA2 <- predict(mA2, type = "response")
prob_mA3 <- predict(mA3, type = "response")


## -----------------------------------------------------------------------------------------------------------------
c(mA1   = summary(mA1)$deviance,
  mA2  = summary(mA2)$deviance,
  mA3  = summary(mA3)$deviance)
pseudoR2 <- function(m) {
  1 - summary(m)$deviance / summary(m)$null.deviance
}

c(
  mA1 = pseudoR2(mA1),
  mA2 = pseudoR2(mA2),
  mA3 = pseudoR2(mA3)
)


## -----------------------------------------------------------------------------------------------------------------
roc_mA1   <- roc(df$exito, prob_mA1)
roc_mA2   <- roc(df$exito, prob_mA2)
roc_mA3   <- roc(df$exito, prob_mA3)
auc(roc_mA1)
auc(roc_mA2)
auc(roc_mA3)


## -----------------------------------------------------------------------------------------------------------------
plot(roc_mA1, col = "blue", lwd = 2, main = "Curvas ROC – Modelos Alternativos")
plot(roc_mA2, col = "red",  lwd = 2, add = TRUE)
plot(roc_mA3, col = "black",  lwd = 2, add = TRUE)

legend("bottomright",
       legend = c("mA1", "mA2", "mA3"),
       col = c( "blue", "red", "black"),
       lwd = 1)


## -----------------------------------------------------------------------------------------------------------------
res_pearson <- residuals(mA1, type = "pearson")
summary(res_pearson)

plot(prob_mA1, res_pearson,
     xlab = "Probabilidad predicha",
     ylab = "Residuos de Pearson",
     main = "Residuos de Pearson vs probabilidad predicha")
abline(h = 0, lty = 2)

res_pearson <- residuals(mA2, type = "pearson")
summary(res_pearson)

plot(prob_mA2, res_pearson,
     xlab = "Probabilidad predicha",
     ylab = "Residuos de Pearson",
     main = "Residuos de Pearson vs probabilidad predicha")
abline(h = 0, lty = 2)

res_pearson <- residuals(mA3, type = "pearson")
summary(res_pearson)

plot(prob_mA3, res_pearson,
     xlab = "Probabilidad predicha",
     ylab = "Residuos de Pearson",
     main = "Residuos de Pearson vs probabilidad predicha")
abline(h = 0, lty = 2)


## -----------------------------------------------------------------------------------------------------------------
modelos <- list(mA1 = mA1,mA2 = mA2,mA3 = mA3)
lapply(modelos, vif)


## -----------------------------------------------------------------------------------------------------------------
summary(m_principal)


## -----------------------------------------------------------------------------------------------------------------
library(emmeans)
emm_tam <- emmeans(
  m_principal,
  specs = "tam_cat",
  type = "response"   # convierte de log-odds a probabilidad
)

emm_tam;pairs(emm_tam)


## -----------------------------------------------------------------------------------------------------------------
pairs(emmeans(m_principal, ~ tam_cat | soporte_grp))


## -----------------------------------------------------------------------------------------------------------------
emm_ori <- emmeans(
  m_principal,
  specs = "orientacion",
  type = "response"
)

emm_ori;pairs(emm_ori)


## -----------------------------------------------------------------------------------------------------------------
emm_mont <- emmeans(
  m_principal,
  specs = "sop_montaje",
  type = "response"
)

emm_mont
pairs(emm_mont)


## -----------------------------------------------------------------------------------------------------------------
emm_soporte <- emmeans(
  m_principal,
  specs = "soporte_grp",
  type = "response"
)

emm_soporte
pairs(emm_soporte)


## -----------------------------------------------------------------------------------------------------------------
emm_fecha <- emmeans(
  m_principal,
  specs = "fecha_est",
  at = list(fecha_est = quantile(df$fecha_est, probs = c(0.1, 0.5, 0.9))),
  type = "response"
)

emm_fecha;pairs(emm_fecha)


## -----------------------------------------------------------------------------------------------------------------
summary(mA1)


## -----------------------------------------------------------------------------------------------------------------
library(emmeans)
library(dplyr)

pairs_mA1 <- bind_rows(

  pairs(emmeans(mA1, ~ tam_cat)) %>%
    as.data.frame() %>%
    mutate(Variable = "Tamaño"),

  pairs(emmeans(mA1, ~ orientacion)) %>%
    as.data.frame() %>%
    mutate(Variable = "Orientación"),

  pairs(emmeans(mA1, ~ soporte_grp)) %>%
    as.data.frame() %>%
    mutate(Variable = "Soporte"),

  pairs(emmeans(mA1, ~ sop_montaje)) %>%
    as.data.frame() %>%
    mutate(Variable = "Soporte de montaje"),

  pairs(emmeans(mA1, ~ serie)) %>%
    as.data.frame() %>%
    mutate(Variable = "Serie")
)

tabla_pairs_mA1 <- pairs_mA1 %>%
  transmute(
    Variable,
    Comparación = contrast,
    OR = round(exp(estimate), 2),
    `IC 95%` = paste0(
      "[",
      round(exp(estimate - 1.96 * SE), 2),
      ", ",
      round(exp(estimate + 1.96 * SE), 2),
      "]"
    ),
    `p-valor` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  )

library(gt)

tabla_pairs_mA1 %>%
  gt() %>%
  tab_header(
    title = "Efectos marginales comparados (odds ratios)",
    subtitle = "Comparaciones por pares – modelo mA1"
  ) %>%
  cols_label(
    Variable = "Variable",
    Comparación = "Comparación",
    OR = "OR",
    `IC 95%` = "IC 95%",
    `p-valor` = "p-valor"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `p-valor`,
      rows = `p-valor` %in% c("<0.001")
    )
  ) %>%
  cols_align(
    align = "center",
    columns = c(OR, `IC 95%`, `p-valor`)
  ) %>%
  opt_all_caps() %>%
  opt_table_outline() %>%
  opt_row_striping()




## -----------------------------------------------------------------------------------------------------------------
summary(mA2)

## -----------------------------------------------------------------------------------------------------------------
library(emmeans)
library(dplyr)
library(gt)

pairs_mA2 <- bind_rows(

  pairs(emmeans(mA2, ~ tam_cat)) %>%
    as.data.frame() %>%
    mutate(Variable = "Tamaño"),

  pairs(emmeans(mA2, ~ orientacion)) %>%
    as.data.frame() %>%
    mutate(Variable = "Orientación"),

  pairs(emmeans(mA2, ~ soporte_grp)) %>%
    as.data.frame() %>%
    mutate(Variable = "Soporte"),

  pairs(emmeans(mA2, ~ sop_montaje)) %>%
    as.data.frame() %>%
    mutate(Variable = "Soporte de montaje"),

  pairs(emmeans(mA2, ~ serie)) %>%
    as.data.frame() %>%
    mutate(Variable = "Serie"),

  pairs(emmeans(mA2, ~ tema)) %>%
    as.data.frame() %>%
    mutate(Variable = "Tema")
)

tabla_pairs_mA2 <- pairs_mA2 %>%
  transmute(
    Variable,
    Comparación = contrast,
    OR = round(exp(estimate), 2),
    `IC 95%` = paste0(
      "[",
      round(exp(estimate - 1.96 * SE), 2),
      ", ",
      round(exp(estimate + 1.96 * SE), 2),
      "]"
    ),
    `p-valor` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  )

tabla_pairs_mA2 %>%
  gt() %>%
  tab_header(
    title = "Efectos marginales comparados (odds ratios)",
    subtitle = "Comparaciones por pares – modelo mA2"
  ) %>%
  cols_label(
    Variable = "Variable",
    Comparación = "Comparación",
    OR = "OR",
    `IC 95%` = "IC 95%",
    `p-valor` = "p-valor"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `p-valor`,
      rows = `p-valor` %in% c("<0.001")
    )
  ) %>%
  cols_align(
    align = "center",
    columns = c(OR, `IC 95%`, `p-valor`)
  ) %>%
  opt_all_caps() %>%
  opt_table_outline() %>%
  opt_row_striping()



## -----------------------------------------------------------------------------------------------------------------
library(gt)

tabla_pairs_mA1 %>%
  gt() %>%
  tab_header(
    title = "Efectos marginales comparados (odds ratios)",
    subtitle = "Comparaciones por pares – modelo mA1"
  ) %>%
  cols_label(
    Variable = "Variable",
    Comparación = "Comparación",
    OR = "OR",
    `IC 95%` = "IC 95%",
    `p-valor` = "p-valor"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `p-valor`,
      rows = `p-valor` %in% c("<0.001")
    )
  ) %>%
  cols_align(
    align = "center",
    columns = c(OR, `IC 95%`, `p-valor`)
  ) %>%
  opt_all_caps() %>%
  opt_table_outline() %>%
  opt_row_striping()



## -----------------------------------------------------------------------------------------------------------------
summary(mA3)

## -----------------------------------------------------------------------------------------------------------------
library(emmeans)
library(dplyr)
library(gt)

# Calcular efectos marginales por pares para mA3
pairs_mA3 <- bind_rows(
  pairs(emmeans(mA3, ~ tam_cat)) %>%
    as.data.frame() %>%
    mutate(Variable = "Tamaño"),
  
  pairs(emmeans(mA3, ~ orientacion)) %>%
    as.data.frame() %>%
    mutate(Variable = "Orientación"),
  
  pairs(emmeans(mA3, ~ serie)) %>%
    as.data.frame() %>%
    mutate(Variable = "Serie"),
  
  pairs(emmeans(mA3, ~ tema)) %>%
    as.data.frame() %>%
    mutate(Variable = "Tema")
)

# Crear tabla final con OR, IC y p-valor
tabla_pairs_mA3 <- pairs_mA3 %>%
  transmute(
    Variable,
    Comparación = contrast,
    OR = round(exp(estimate), 2),
    `IC 95%` = paste0(
      "[",
      round(exp(estimate - 1.96 * SE), 2),
      ", ",
      round(exp(estimate + 1.96 * SE), 2),
      "]"
    ),
    `p-valor` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  )

# Mostrar tabla visualmente atractiva con gt
tabla_pairs_mA3 %>%
  gt() %>%
  tab_header(
    title = "Efectos marginales comparados (odds ratios)",
    subtitle = "Comparaciones por pares – modelo mA3"
  ) %>%
  cols_label(
    Variable = "Variable",
    Comparación = "Comparación",
    OR = "OR",
    `IC 95%` = "IC 95%",
    `p-valor` = "p-valor"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = `p-valor`,
      rows = `p-valor` %in% c("<0.001")
    )
  ) %>%
  cols_align(
    align = "center",
    columns = c(OR, `IC 95%`, `p-valor`)
  ) %>%
  opt_all_caps() %>%
  opt_table_outline() %>%
  opt_row_striping()



## -----------------------------------------------------------------------------------------------------------------
m_fecha <- lm(fecha_est ~ tema + soporte_grp + tam_cat + orientacion+tipo_autor+tecnica+serie*tam_cat, data = df)
anova(m_fecha)
summary(m_fecha)
par(mfrow = c(2, 2))
plot(m_fecha)

