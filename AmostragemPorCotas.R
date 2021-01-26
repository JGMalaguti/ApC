ApC=function(Dados,n,...){
  require(dplyr)
  
  var_est=select(Dados,...)
  
  nvar=dim.data.frame(var_est)[2] 
  tam=nrow(Dados)
  
  #Criação de um data.frame auxiliar com os fatores originais da população.
  
  aux=rep(list(NA),nvar)
  
  for(i in 1:nvar){
    aux[[i]]=levels(var_est[[i]])}
  
  teste=expand.grid(aux)
  cat=nrow(teste)
  
  #Computa e calcula as cotas possíveis e sua quantidade total
  
  tab=table(var_est)/tam[1]
  tab=as.vector(tab)
  TamCot=ceiling(tab*n)
  
  #Calcula o tamanho de amostra de cada cota.
  
  cont.aux=rep(1,cat)
  
  cots=rep(list(0),cat)
  
  for(i in 1:cat){
    cots[[i]]=rep(0,TamCot[i])}
  
  PdL=sample(1:tam[1],1,replace=T)
  
  #Escolha de um ponto de largada para garantir amostras diferentes.
  
  if(PdL>=tam[1]-3*n){
    var_est=rbind(var_est,var_est)
    tam=2*tam}
  
  #Para garantir que a amostra seja completada mesmo quando "PdL" é alto.
  
  #Contador auxiliar e lista onde será armazenada as identificações dos elementos
  #escolhidos para a amostra

  for(k in PdL:tam[1]){
    for(j in 1:cat){
      if(cont.aux[j]<=TamCot[j]){
        
        vet.test=(var_est[k,]==teste[j,])
        
        if(all(vet.test)){
          cots[[j]][cont.aux[j]]=as.numeric(rownames(var_est[k,]))
          cont.aux[j]=cont.aux[j]+1}
      }
    }
    
    if(all(cont.aux==TamCot+1)){
      break
    }
  }
  
  #Esse loop percorre o banco de dados auxiliar até encontrar o número certo de pessoas para cada
  #cota definida, armazenando as ids numa lista.
  
  cotas=unlist(cots)
  cotas=as.vector(cotas)
  cotas=sort(cotas)
  
  #Organizando o vetor para poder utilizar a função vetgetdat(). 
  
  return(cotas)
  
}
