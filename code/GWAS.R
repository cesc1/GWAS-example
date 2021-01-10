##### Conjunt de funcions per facilitar GWAS #####
## Principalment s'utilitza el paquet snpStats



#### Data Cleaning


### Name: id_match
## Desc:
## Packages: NA
## Arguments:
  # data: data.frame, on els noms de les files hauria de coincidir amb id
  # id: vector de ids integer a fer match
## Retorna: data.frame, amb data ordenada segons id

id_match <- function(data, id){
  # Length check
  if(nrow(data) != length(id)){
    stop(sprintf("Files de data (%i) no coincideix amb llargada id (%i).", nrow(data), length(id)))
  }
  # Mirar si id esta en data
  in_matches <- rownames(data) %in% id
  if(!all(in_matches)){
    stop(sprintf("Hi ha %i IDs que no coincideixen.", sum(!in_matches)))
  }
  # Mirar que cada fila de data coincideixi amb el seu id.
  matches <- rownames(data) == id
  if(!all(matches)){
    cat("Ordenant IDs...")
    data <- data[as.character(id),]
  } else{
    cat("Els IDs coincideixen!")
  }
  return(data)
}


#####
## IMPORTANT: El codi de les funcions que utilitzen col.summary() no es eficient perque l'executen més 
# d'un cop. Pensar en posar el conjunt de funcions en classes, per emmagatzemar la informació.
#####

### Name: remove_indNA
## Desc: Elimina els individus amb un call rate inferior a cert valor
## Packages: snpStats
## Arguments:
  # data: SnpMatrix.
  # minCallr: CallRate minim per individu.
## Retorna: SnpMatrix reduida.


remove_indNA <- function(data, minCallr = 0.95){
  infoG <- col.summary(data)
  matches <- infoG$Call.rate < minCallr
  data <- data[, !matches]
  cat(sprintf("- remove_indNA(): S'han eliminat %i individus\n", sum(matches)))
  return(data)
}


### Name: remove_HWE
## Desc: Elimina els SNPs que no compleixen les condicions de HWE
## Packages: snpStats
## Arguments:
  # data: SnpMatrix.
  # minCallr: CallRate minim per individu.
## Retorna: SnpMatrix reduida.


remove_HWE <- function(data, signif = 0.05){
  info <- col.summary(data)
  info$pvalHWE <- 1 - pnorm(info$z.HWE)
  matches <- info$pvalHWE < signif
  # Per el print
  noHWE <- sum(matches, na.rm = T)
  noNA <- sum(is.na(matches))
  # Si hi ha NAs, es marquen com que no han pasat el test
  matches[is.na(matches)] <- TRUE
  
  data <- data[, !matches]
  cat(sprintf("- remove_HWE(): S'han eliminat %i individus, dels quals:\n", sum(matches)))
  cat(sprintf("  - %i tenien un p-valor < %.3f\n", noHWE, signif))
  cat(sprintf("  - %i tenien un test NA\n", noNA))
  return(data)
}


### Name: remove_MAF
## Desc: Elimina els SNPs amb una freqüència d'al·lels minoritaris no suficient
## Packages: snpStats
## Arguments:
  # data: SnpMatrix.
  # minFreq:  minim per individu.
## Retorna: SnpMatrix reduida.

remove_MAF <- function(data, minFreq = 0.01){
  info <- col.summary(data)
  matches <- info$MAF < minFreq
  # Per el print
  noMAF <- sum(matches, na.rm = T)
  noNA <- sum(is.na(matches))
  # Si hi ha NAs, es marquen com que no han pasat el test
  matches[is.na(matches)] <- TRUE  
  
  data <- data[, !matches]
  cat(sprintf("- remove_MAF(): S'han eliminat %i individus, dels quals:\n", sum(matches)))
  cat(sprintf("  - %i tenien un MAF < %.3f\n", noMAF, minFreq))
  cat(sprintf("  - %i tenien un MAF NA\n", noNA))
  return(data)  
}


### Name: rounding_Xtable
## Desc: Arrodoneix els digits de les columnes que volem, i els transforma tipus char (per Xtable).
   # També es pot ordenar una de les columnes a priori. 
## Packages: NA
## Arguments: 
  # data: Objecte data.frame
  # digits: Llista amb el nombre de digits que volem com en C. Ha de ser una llista anomenada
  # amb la columna corresponent: ej: list(OR = ".2", P_value = 0.3)
## Retorna: Taula amb les columnes arrodonides com a characters. data.frame.

rounding_Xtable <- function(data, digits, ordenar = NULL){
  # Datacheck
  if(!is.data.frame(data)){
    if(is.matrix(data)){
      data <- as.data.frame(data)
    }else{
      stop("La classe de data ha de ser matrix/data.frame")
    }
  }
  # Digit names check
  if(any(!names(digits) %in% names(data))){
    stop("Algun dels noms de digits no coincideix amb data")
  }
  # Ordenant, si ho volem
  if(!is.null(ordenar)){
    # Length ordenar check
    if(length(ordenar) != 1)
      stop("L'argument ordenar només pot tenir un valor.")
    #  Name ordenar check
    if(!ordenar %in% names(data))
      stop(sprintf("Variable %s no esta en data.", ordenar))
    # Ordena
    data <- data[order(data[, ordenar]), ]
  }
  # Arrodonint
  columnes_arrod <- sapply(names(digits), function(colNom){
    outString <- paste0("%", digits[[colNom]], "f")
    return(sprintf(outString, data[, colNom]))
  })
  # Substituint columnes
  data[, names(digits)] <- data.frame(columnes_arrod)
  return(data)
}




### Name: 
## Desc:
## Packages: 
## Arguments:

## Retorna: 
