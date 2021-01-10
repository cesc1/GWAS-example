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
