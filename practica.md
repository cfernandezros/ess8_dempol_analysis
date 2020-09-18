Construcción de modelos multivariantes sobre la European Social Survey
================
Carlos Fernández Rosauro
16/6/2020

### Trabajo realizado como práctica final de la asignatura “Análisis multivariante” del Grado en Ciencia de datos aplicada de la UOC.

En este documento se realiza un análisis sobre variables demográficas y
políticas tomando datos de la Encuesta Social Europea. Los factores
analizados son los siguientes:

1 - Factores subyacentes a la imagen que los hombres tienen de sí mismos
(sección H1 del cuestionario).

2 - Variables demográficas (edad, género, haber nacido en el país o no y
años de educación completados) que expliquen el sentirse cercano a un
partido político. Orden de importancia de los predictores.

3 - Variables demográficas (edad, género, haber nacido en el país o no,
años de educación completados) que son predictivas del número de
personas a cargo en el trabajo. Orden de importancia de los predictores.

4 - Perfiles de votantes del Partido Popular en las elecciones de 2016,
utilizando como variables: género, edad, años de educación completada,
número de personas que viven habitualmente en el hogar y el decil de
los ingresos (del hogar).

### Primera parte.

A la hora de determinar los factores subyacentes a la imagen que los
hombres tienen de si mismos, la mejor herramienta que tenemos al alcance
es el análisis factorial. Trataremos de analizar como se comportan las
variables recogidas de forma optimizada en n factores.

``` r
library(plyr)
library(dummies)
library(FactoMineR)
library(foreign)
library(tidyverse)
dataset = read.spss("/Users/carlosfernandez/Desktop/ESS8ES.sav", to.data.frame=TRUE)
```

Primero debemos recodificar todas las variables asignando un valor a la
escala categórica de respuestas. Se ha optado por asignar 1 a “No se
parece nada a mí” hasta 6 a “Se parece mucho a mí”. A continuación se
han estandarizado los valores, ya que aunque la escala de medida sea la
misma para todas las variables nos ahorramos cualquier problema por
sesgos.

``` r
firstcol = which(colnames(dataset)=="ipcrtiv")
lastcol = which(colnames(dataset)=="impfun")
df1 <- dataset[c(firstcol:lastcol)]
df1["gndr"] <- dataset["gndr"]
df1_1 <- df1[df1$gndr == "Male", ]
df1_1 <- df1_1[complete.cases(df1_1), ]
df1_1 <- df1_1[, -22]
df1_2 <- dummy.data.frame(df1_1, sep = ".")
tabla <- apply(df1_1[colnames(df1_1)], 2, table)
aux <- df1_1
aux$ipcrtiv = revalue(aux$ipcrtiv, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$imprich = revalue(aux$imprich, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipeqopt = revalue(aux$ipeqopt, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipshabt = revalue(aux$ipshabt, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$impsafe = revalue(aux$impsafe, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$impdiff = revalue(aux$impdiff, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipfrule = revalue(aux$ipfrule, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipudrst = revalue(aux$ipudrst, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipmodst = revalue(aux$ipmodst, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipgdtim = revalue(aux$ipgdtim, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$impfree = revalue(aux$impfree, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$iphlppl = revalue(aux$iphlppl, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipsuces = revalue(aux$ipsuces, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipstrgv = revalue(aux$ipstrgv, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipadvnt = revalue(aux$ipadvnt, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$ipbhprp = revalue(aux$ipbhprp, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$iprspot = revalue(aux$iprspot, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$iplylfr = revalue(aux$iplylfr, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$impenv = revalue(aux$impenv, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$imptrad = revalue(aux$imptrad, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))
aux$impfun = revalue(aux$impfun, c("Not like me at all"='1',"Not like me"='2',"A little like me"='3',"Somewhat like me"='4',"Like me"='5',"Very much like me"='6'))

aux[] <- lapply(aux, function(x) as.numeric(as.character(x)))
```

A continuación, implementamos un análisis de componentes principales
para obtener una orientación en cuanto a qué cantidad de factores
deberíamos utilizar posteriormente. Como hemos obtenido que 3
componentes se diferencian considerablemente del resto, pero existen 21
variables, se ha decidido realizar el análisis factorial con 4 factores.

``` r
scaled_aux <- scale(aux)
pca <- princomp(scaled_aux)
pca
```

    ## Call:
    ## princomp(x = scaled_aux)
    ## 
    ## Standard deviations:
    ##    Comp.1    Comp.2    Comp.3    Comp.4    Comp.5    Comp.6    Comp.7    Comp.8 
    ## 1.9727679 1.6900610 1.4264748 1.1026792 1.0302022 0.9598121 0.9528323 0.9185363 
    ##    Comp.9   Comp.10   Comp.11   Comp.12   Comp.13   Comp.14   Comp.15   Comp.16 
    ## 0.8740271 0.8649835 0.8220582 0.7977278 0.7835434 0.7677364 0.7476561 0.7366635 
    ##   Comp.17   Comp.18   Comp.19   Comp.20   Comp.21 
    ## 0.7021747 0.6794685 0.6546785 0.6347924 0.5722225 
    ## 
    ##  21  variables and  876 observations.

Los resultados del análisis factorial resultan ciertamente interesantes,
si seguimos el orden de las variables según aparecen en el dataset,
podríamos resumirlas de la siguiente forma:

  - 1 - originalidad y creatividad
  - 2 - dinero
  - 3 - igualdad
  - 4 - habilidades personales
  - 5 - seguridad
  - 6 - aventuras
  - 7 - obediencia
  - 8 - comprensión
  - 9 - humildad
  - 10 - diversion
  - 11 - independencia
  - 12 - ayudar
  - 13 - éxito
  - 14 - Estado
  - 15 - riesgos
  - 16 - comportarse
  - 17 - hacerse respetar
  - 18 - fidelidad
  - 19 - medioambiente
  - 20 - tradición
  - 21 - placer

Según los resultados, los factores explican las variables de la
siguiente forma:

Factor 1: 5, 7, 9, 14, 16, 20: imagen de seguridad, obediencia,
servilismo, tradición.

Factor 2: 1, 3, 8, 12, 18, 19: imagen de creatividad, igualdad,
comprensión, fidelidad.

Factor 3: 6, 10, 15, 21: imagen sobre el placer, diversión, riesgos.

Factor 4: 2, 4, 13, 17: imagen sobre el dinero, éxito, hacerse respetar.

``` r
fa_aux <- factanal(scaled_aux, 4, rotation = "varimax")
fa_aux
```

    ## 
    ## Call:
    ## factanal(x = scaled_aux, factors = 4, rotation = "varimax")
    ## 
    ## Uniquenesses:
    ## ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim 
    ##   0.812   0.698   0.650   0.491   0.669   0.618   0.741   0.667   0.700   0.470 
    ## impfree iphlppl ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr  impenv imptrad 
    ##   0.820   0.639   0.450   0.675   0.578   0.570   0.750   0.674   0.737   0.723 
    ##  impfun 
    ##   0.320 
    ## 
    ## Loadings:
    ##         Factor1 Factor2 Factor3 Factor4
    ## ipcrtiv          0.344   0.167   0.192 
    ## imprich         -0.110   0.226   0.489 
    ## ipeqopt          0.590                 
    ## ipshabt          0.112           0.695 
    ## impsafe  0.503   0.178           0.209 
    ## impdiff          0.297   0.486   0.230 
    ## ipfrule  0.504                         
    ## ipudrst          0.570                 
    ## ipmodst  0.382   0.323          -0.224 
    ## ipgdtim          0.112   0.691   0.199 
    ## impfree          0.352   0.190   0.142 
    ## iphlppl  0.306   0.507                 
    ## ipsuces  0.152           0.288   0.667 
    ## ipstrgv  0.526   0.164           0.141 
    ## ipadvnt -0.215   0.160   0.521   0.281 
    ## ipbhprp  0.618   0.201                 
    ## iprspot  0.282                   0.406 
    ## iplylfr  0.304   0.465   0.117         
    ## impenv   0.244   0.426   0.131         
    ## imptrad  0.520                         
    ## impfun           0.126   0.807         
    ## 
    ##                Factor1 Factor2 Factor3 Factor4
    ## SS loadings      2.002   1.942   1.910   1.697
    ## Proportion Var   0.095   0.092   0.091   0.081
    ## Cumulative Var   0.095   0.188   0.279   0.360
    ## 
    ## Test of the hypothesis that 4 factors are sufficient.
    ## The chi square statistic is 532.43 on 132 degrees of freedom.
    ## The p-value is 1.69e-49

Podemos concluir que el modelo ha recogido exitosamente el contenido de
las 21 variables sobre la imagen que tienen los hombres de si mismos en
4 factores bien diferenciados. 

### Segunda parte.

Teniendo en cuenta la naturaleza del análisis que queremos realizar:
relación de variable dependiente dicotómica frente a 5 variables
independientes, la herramienta idónea es una regresión logística.
Convertimos los datos a valores numéricos y transformamos las variables
cualitativas en variables independientes dicotómicas (género, nacido/a
en España), con valores 0 y 1. También asignamos estructura binaria a
nuestra variable dependiente: ‘se siente cercano/a a un partido
político’ \<- 1 y ‘no se siente cercano/a a un partido político’ \<-
0.

``` r
library(ResourceSelection)
df2 <- dataset[, c('clsprty', 'gndr', 'brncntr', 'agea', 'eduyrs')]
df_s <- df2[complete.cases(df2), ]
df2_1 <- df2[complete.cases(df2), c('agea', 'eduyrs')]
df2_1[] <- lapply(df2_1, function(x) as.numeric(as.character(x)))
df2_1["clsprty"] <- df_s["clsprty"]
df2_1["gndr"] <- df_s["gndr"]
df2_1["brncntr"] <- df_s["brncntr"]
df2_1['clsprty'] <- data.frame(lapply(df2_1['clsprty'], as.character), stringsAsFactors=FALSE)
df2_1['gndr'] <- data.frame(lapply(df2_1['gndr'], as.character), stringsAsFactors=FALSE)
df2_1['brncntr'] <- data.frame(lapply(df2_1['brncntr'], as.character), stringsAsFactors=FALSE)
df2_1$clsprty[df2_1$clsprty == "No"] <- '0'
df2_1$clsprty[df2_1$clsprty == "Yes"] <- '1'
df2_1$gndr[df2_1$gndr == "Female"] <- '0'
df2_1$gndr[df2_1$gndr == "Male"] <- '1'
df2_1$brncntr[df2_1$brncntr == "No"] <- '0'
df2_1$brncntr[df2_1$brncntr == "Yes"] <- '1'
df2_1['clsprty'] <- data.frame(lapply(df2_1['clsprty'], as.numeric))
df2_1['brncntr'] <- data.frame(lapply(df2_1['brncntr'], as.numeric))
df2_1['gndr'] <- data.frame(lapply(df2_1['gndr'], as.numeric))
```

Para valorar si es adecuado utilizar el modelo de la regresión
logísitca, primero estructuramos el modelo y le aplicamos el test de
Hosmer-Lemeshow. Como buscamos la no significación, asumimos con el
p-valor obtenido que el modelo es adecuado. Además, como observamos que
la diferencia entre las funciones de máxima verosimilitud de un modelo
simple (solo formado por el intercepto) y del modelo logístico no es
nula, podemos argumentar que el modelo cuenta con significatividad
global.

``` r
fit <- glm(data=df2_1,clsprty~gndr+brncntr+agea+eduyrs,family = binomial)
hoslem.test(fit$y, fitted(fit), g=10)
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  fit$y, fitted(fit)
    ## X-squared = 10.012, df = 8, p-value = 0.2642

``` r
fit$null.deviance - fit$deviance
```

    ## [1] 71.16728

Finalmente, observamos la significancia estadística indvidual de los
factores. Primero debemos descartar como significante la variable género
debido a su alto p-valor. De las variables válidas, no podemos asignar
un orden de importancia basándonos en los coeficientes, ya que los
valores de cada uno de estos factores no están distribuidos de la misma
forma (distinta varianza) y además utilizan diferentes medidas. Por
tanto, utilizando la puntuación Z o los p-valores, el orden de los
factores por importancia es el siguiente, de mayor a menor: número de
años de educación recibida, edad, haber nacido o no en España. Con
respecto al signo asociado a los coeficientes y puntuaciones Z, debemos
argumentar que la interpretación del modelo es la siguiente:

  - A más años de educación, menor probabilidad de sentirse cercano a un
    partido.

  - A mayor edad, menor probabilidad de sentirse cercano a un partido.

  - Aquellas personas que han nacido en españa tienen mayor probabilidad
    de sentirse cercanas a un partido.

<!-- end list -->

``` r
summary(fit)
```

    ## 
    ## Call:
    ## glm(formula = clsprty ~ gndr + brncntr + agea + eduyrs, family = binomial, 
    ##     data = df2_1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9228  -1.2599   0.8415   1.0342   1.6758  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.87597    0.27864  -6.732 1.67e-11 ***
    ## gndr         0.12221    0.09589   1.274  0.20249    
    ## brncntr      0.57589    0.15430   3.732  0.00019 ***
    ## agea         0.01678    0.00299   5.612 2.00e-08 ***
    ## eduyrs       0.06662    0.01036   6.433 1.25e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2531.7  on 1869  degrees of freedom
    ## Residual deviance: 2460.5  on 1865  degrees of freedom
    ## AIC: 2470.5
    ## 
    ## Number of Fisher Scoring iterations: 4

Por otro lado, observando los ‘odd Ratio’ de la variable dependiente con
respecto al crecimiento de las variables independientes es muy similar
para todas ellas. Esto podría indicarnos, que a pesar de poder
describirse facilmente un orden concreto de importancia de las
variables, ninguna sobresale de forma excesiva con respecto al resto a
la hora de predecir en el modelo.

Podríamos concluir que el modelo descrito aporta utilidad a la hora de
predecir si existe cercanía a algún partido político. Si quisiéramos
ampliar las aptitudes del modelo, podríamos plantear un conjunto de
regresiones logísticas orientados a, por ejemplo, cada uno de los
principales partidos políticos, para analizar cuáles de estas variables
(u otras) inciden más a la hora de posicionarse en el espectro político.

``` r
exp(coef(fit))
```

    ## (Intercept)        gndr     brncntr        agea      eduyrs 
    ##   0.1532069   1.1299895   1.7787103   1.0169200   1.0688873

### Tercera parte.

En el caso que se presenta en este ejercicio, parece adecuado utilizar
como modelo una regresión múltiple, tenemos una variable cuantitativa y
contínua dependiente y cuatro variables independientes (dos contínuas y
dos categóricas binarias). Para encontrar un modelo lineal que se ajuste
a la relación de interdependencia planteada, observaremos el
comportamiento de las variables independientes en el modelo inicial y
finalmente comprobaremos si se cumplen los supuestos necesarios para
validar tal modelo.

Comenzamos codificando las variables categóricas de forma binaria.

``` r
df3 <- dataset[, c('njbspv', 'gndr', 'brncntr', 'agea', 'eduyrs')]
df_t <- df3[complete.cases(df3), ]
df3_1 <- df3[complete.cases(df3), c('njbspv', 'agea', 'eduyrs')]
df3_1[] <- lapply(df3_1, function(x) as.numeric(as.character(x)))
df3_1["gndr"] <- df_t["gndr"]
df3_1["brncntr"] <- df_t["brncntr"]
df3_1['gndr'] <- data.frame(lapply(df3_1['gndr'], as.character), stringsAsFactors=FALSE)
df3_1['brncntr'] <- data.frame(lapply(df3_1['brncntr'], as.character), stringsAsFactors=FALSE)
df3_1$gndr[df3_1$gndr == "Female"] <- '0'
df3_1$gndr[df3_1$gndr == "Male"] <- '1'
df3_1$brncntr[df3_1$brncntr == "No"] <- '0'
df3_1$brncntr[df3_1$brncntr == "Yes"] <- '1'
df3_1['gndr'] <- data.frame(lapply(df3_1['gndr'], as.numeric))
df3_1['brncntr'] <- data.frame(lapply(df3_1['brncntr'], as.numeric))
```

Al plantear el modelo inicial con todas las variables, obervamos en este
caso que no podemos considerar como significativas “brncntr” y “gndr”.
Para establecer el orden de importancia de las variables significativas,
podemos utilizar los estadísticos t individuales ya que las variables no
están estandarizadas. Orden de importancia de mayor a menor de las
variables con valor predictivo: edad, años de educación recibidos. Si
además quisiéramos incorporar el resto de variables, el orden seguiría:
género y haber nacido o no en España. Dados los resultados, podemos
valorar el modelo de la siguiente manera (variables con significancia):

  - A mayor edad, más probabilidad de tener más personas a cargo en el
    trabajo.

  - A mayor cantidad de años trabajados, más probabilidad de tener más
    personas a cargo en el trabajo.

<!-- end list -->

``` r
emp_fit <- lm(formula=njbspv~gndr+brncntr+agea+eduyrs,data=df3_1)
summary(emp_fit)
```

    ## 
    ## Call:
    ## lm(formula = njbspv ~ gndr + brncntr + agea + eduyrs, data = df3_1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -215.01  -36.16  -12.24    9.68 2893.31 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -153.9673    40.3035  -3.820  0.00015 ***
    ## gndr          16.6884    14.7125   1.134  0.25720    
    ## brncntr       -0.7649    23.3664  -0.033  0.97390    
    ## agea           2.0242     0.4861   4.164 3.67e-05 ***
    ## eduyrs         4.4249     1.2745   3.472  0.00056 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 157 on 513 degrees of freedom
    ## Multiple R-squared:  0.04836,    Adjusted R-squared:  0.04094 
    ## F-statistic: 6.517 on 4 and 513 DF,  p-value: 4.031e-05

Para optimizar el modelo, retiraremos las variables que carecen de
significancia estadística y analizaremos algunos de los supuestos de los
modelos lineales:

Al no observarse demasiado grado de correlación entre las dos variables
independientes, podemos decir que se cumple el supuesto de no
multicolinealidad. Por otro lado, el test de Shapiro-Wilk nos indica que
los resíduos no se distribuyen normalmente, por lo que el supuesto de
normalidad no se cumple.

``` r
corr_fit <- lm(formula=njbspv~agea+eduyrs,data=df3_1)
cor(df3_1$agea, df3_1$eduyrs)
```

    ## [1] -0.3401797

``` r
shapiro.test(corr_fit$residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  corr_fit$residuals
    ## W = 0.20138, p-value < 2.2e-16

Podríamos concluir que el modelo obtenido resulta un buen comienzo para
predecir la cantidad de personas a cargo en el trabajo. Para optimizar
el modelo, podríamos buscar otras variables del ‘dataset’ que: mostrasen
de forma individual un buen grado de predicción y que contribuyesen a
cumplir los supuestos del modelo lineal.

### Cuarta parte.

Para realizar los perfiles de los votantes del PP solicitados,
utilizaremos un método jerárquico de análisis clúster, concretamente el
método de Ward. El resultado, observado en un dendograma junto con las
tablas de las frecuencias absolutas de las variables, nos permitirán
determinar el número de perfiles (conglomerados) adecuados en función de
los factores utilizados, observando además el porqué.

En primer lugar codificamos las variables. Utilizaremos solamente casos
completos para que al normalizar los datos, los valores queden
adecuadamente
parametrizados.

``` r
df4 <- dataset[, c('gndr','agea', 'eduyrs', 'hhmmb', 'hinctnta', 'prtvtdes')]
df4_1 <- df4[df4$prtvtdes == "Partido Popular - PP", ]
df4_1 <- df4_1[complete.cases(df4_1), ]
df4_2 <- df4_1[c("agea", "eduyrs", "hhmmb")]
df4_2[] <- lapply(df4_2, function(x) as.numeric(as.character(x)))
df4_2["gndr"] <- df4["gndr"]
df4_2["hinctnta"] <- df4["hinctnta"]
df4_2['gndr'] <- data.frame(lapply(df4_2['gndr'], as.character), stringsAsFactors=FALSE)
df4_2['hinctnta'] <- data.frame(lapply(df4_2['hinctnta'], as.character), stringsAsFactors=FALSE)
df4_2$gndr[df4_2$gndr == "Female"] <- '0'
df4_2$gndr[df4_2$gndr == "Male"] <- '1'
df4_2$hinctnta[df4_2$hinctnta == "J - 1st decile"] <- '1'
df4_2$hinctnta[df4_2$hinctnta == "R - 2nd decile"] <- '2'
df4_2$hinctnta[df4_2$hinctnta == "C - 3rd decile"] <- '3'
df4_2$hinctnta[df4_2$hinctnta == "M - 4th decile"] <- '4'
df4_2$hinctnta[df4_2$hinctnta == "F - 5th decile"] <- '5'
df4_2$hinctnta[df4_2$hinctnta == "S - 6th decile"] <- '6'
df4_2$hinctnta[df4_2$hinctnta == "K - 7th decile"] <- '7'
df4_2$hinctnta[df4_2$hinctnta == "P - 8th decile"] <- '8'
df4_2$hinctnta[df4_2$hinctnta == "D - 9th decile"] <- '9'
df4_2$hinctnta[df4_2$hinctnta == "H - 10th decile"] <- '10'

df4_2['gndr'] <- data.frame(lapply(df4_2['gndr'], as.numeric))
df4_2['hinctnta'] <- data.frame(lapply(df4_2['hinctnta'], as.numeric))
```

Calculamos las distancias euclídeas y les aplicamos el método de Ward.
El resultado nos muestra tres grupos bien diferenciados.

``` r
scaled <- scale(df4_2)
distancia <- dist(scaled, method="euclidean")
cluster <- hclust(distancia, method="ward.D2")
plot(cluster)
rect.hclust(cluster , k = 3, border = 2:6) 
```

![](practica_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Podríamos argumentar a través del dendograma, que 3 sería un número
óptimo de clúster. Al observar las frecuencias absolutas de las tablas
de frecuencia de las variables nos damos cuenta que:

  - La población se distribuye centralmente en edad y número de años de
    educación recibidos.

  - Los votantes se acumulan en perfiles familiares de 2, 3 y 4
    personas.

  - Los votantes se acumulan entre los deciles 2, 3, 4 y de ingresos.

<!-- end list -->

``` r
table(df4_2$eduyrs)
```

    ## 
    ##  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
    ##  5  1 10  6  5  9  8 10 39 11 33 13 23 15 20 15 16  6 14 10 14  5  4  3  3  1 
    ## 26 28 30 50 
    ##  2  1  2  1

``` r
table(df4_2$hhmmb)
```

    ## 
    ##   1   2   3   4   5   6   7   8 
    ##  26 111  71  73  17   4   2   1

``` r
table(df4_2$hinctnta)
```

    ## 
    ##  1  2  3  4  5  6  7  8  9 10 
    ## 26 35 30 26 26 13 15 17 13 14

De esta forma, podríamos asumir que los tres clúster definidos, dividen
a los votantes en función de la cantidad de miembros en su hogar y su
decil de ingresos, siendo generalmente los votantes con más ingresos los
que se tiene más personas por vivienda y mayor nivel educativo.
