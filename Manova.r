library(rstatix)

MANOVA_class <- function(dfw)
  {
    #-> détection automatique des variables numériques
   id_num <- which(sapply(dfw, is.numeric))
   X <- dfw[,id_num]

   #-> .. on cherche la variable NON numérique ..
   id_string  <- which(!sapply(dfw, is.numeric))

   #-> pour faciliter et uniformiser les recherches des classes, on renomme cette variable
   names(dfw)[id_string] <- 'fac' 

   # ici code à compléter en utilisant ce qui a été développé dans la première partie
   #  création de la liste
  
    p = ncol(X)
    l = nrow(X)
    
    df_g1 <- X[1:(l/2), 1:p]

    df_g2 <- X[(l/2+1):l, 1:p]
    
  
   # calcul de la variance commune
    
    G1 <- list(data = df_g1,n = nrow(df_g1), mean = colMeans(df_g1), S2 = cov(df_g1))
    
    G2 <- list(data = df_g2,n = nrow(df_g2), mean = colMeans(df_g2), S2 = cov(df_g2))

    Sd2 <- ((G1$n - 1) * (G1$S2) + (G2$n -1 )*(G2$S2)) / (G1$n + G2$n - 2)
    
    L <- list(G1=G1, G2=G2)
 
   # matrice des différences des moyennes
    
    Y1Y2 <- G1$mean - G2$mean
 
   # calcul du T2 de Hotteling

    T2 <- t(Y1Y2) %*% solve(((1/G1$n)+(1/G2$n))*Sd2) %*% Y1Y2
    
    Fobs <- (G1$n + G2$n - p - 1) / (G1$n + G2$n - 2) / p * T2
 
   # calcul de la p value
    
    pvalue <- df(Fobs, 1, G1$n + G2$n - p - 1)
 
   # les résultats sont stockés dans une liste
 
 test_result        <- data.frame('T² Hotteling' = T2, 'Fobs' = Fobs, 'p value' = pvalue )
 variance           <- data.frame(Sd2) 
 colnames(variance) <- names(df)[id_num] ; rownames(variance) <- names(df)[id_num]
 class_mean         <- data.frame(t(sapply(L, function(x){return(x$mean)})))
 
 ret   <- list(test = test_result, variance = variance, class_mean = class_mean)
 
 return(ret)
 
 }
