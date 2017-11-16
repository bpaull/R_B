library(tidyverse)

compte_modalite <- function(x){
  length(unique(x))
}


resume <- function(df){
  nb_moda <- c()
  nb_NA <- c()
  nb_obs <- c()
  type_var <- c()
  for(i in 1:length(df)){
    nb_moda <- c(nb_moda, compte_modalite(df[[i]]))
    nb_NA <- c(nb_NA, sum(is.na(df[[i]])))
    nb_obs <- c(nb_obs, length(df[[i]])-nb_NA[i])
    type_var <- c(type_var, class(df[[i]])[1])
  }
  cbind.data.frame(nom = names(df), type_var, nb_moda, nb_obs, nb_NA)
}

recap_facteur <- function(facteur){
  longeur <- compte_modalite(facteur)
  #retour <- matrix(nrow = longeur, ncol = 3)
  #dimnames(retour) <- list(1:longeur, c("modalite", "nb_obs", "frequence"))
  #retour[, 1] <- unique(facteur)
  r <- rep(NA, longeur)
  for(i in 1:longeur){
    r[i] <- sum(facteur == unique(facteur)[i], na.rm = T)
  }
  retour <- cbind.data.frame(unique(facteur), r, round(r/length(facteur), 3))
  names(retour) <- c("modalite", "nb_obs", "frequence")
  return(retour)
}


statistique <- function(df){
  taille <- length(df)
  y <- resume(df)$nb_moda
  y[y > 9] <- 6
  longeur <- max(y)
  #largeur <- 2*taille
  #retour <- NA
  #dimnames(retour) <- list(1:longeur, )
  for(i in 1:taille){
    nb_moda <- compte_modalite(df[[i]])
    if(nb_moda < 10){
      r <- recap_facteur(df[[i]])[, c(1,3)]
      while(nrow(r) < longeur){
        r <- rbind(r, rep(NA, 2))
      }
      names(r) <- c(paste0("modalite:",names(df)[i]),i)
    }
    else {
      x <- summary(df[[i]])
      r <- cbind.data.frame(names(x),as.numeric(x))
      while(nrow(r) < longeur){
        r <- rbind(r, rep(NA, 2))
      }
      names(r) <- c(paste0("statistique:",names(df)[i]),i)
    }

    if (i == 1){
      retour <- r
    }
    else {
    retour <- cbind(retour,r)
    }
  }
  return(retour)
}
