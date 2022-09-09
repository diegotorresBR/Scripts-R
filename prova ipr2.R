#Questao 1

v1 = as.matrix(c(3,3,4,6,4))
v2 = as.matrix(c(2,2,3,6,5))
v3 = as.matrix(c(4,2,3,5,5))
v4 = as.matrix(c(3,1,2,1,2))
v5 = as.matrix(c(2,5,5,1,5))
v6 = as.matrix(c(6,3,4,6,3))

matriz_votos = rbind(v1, v2, v3, v4, v5, v6)

#letra a e b

cand1 = 0
cand2 = 0
cand3 = 0
cand4 = 0
nulo = 0
branco = 0



for (i in 1:nrow(matriz_votos)){
  for (j in 1:ncol(matriz_votos)){
    if(matriz_votos[i,j] == 1){
      cand1 = cand1 + 1
    }else if(matriz_votos[i,j] == 2){
      cand2 = cand2 + 1
    }else if(matriz_votos[i,j] == 3){
      cand3 = cand3 + 1
    }else if(matriz_votos[i,j] == 4){
      cand4 = cand4 + 1
    }else if(matriz_votos[i,j] == 5){
      nulo = nulo + 1
    }else if(matriz_votos[i,j] == 6){
      branco = branco + 1
    }
  }
}


vet_votos = c(cand1, cand2, cand3, cand4, nulo, branco)
for(x in 1:6){
  if(x == 5){
    cat("Nulos:", vet_votos[x], "votos.", vet_votos[x]*100/sum(vet_votos),"% dos votos totais\n")
  }else if(x == 6){
    cat("Brancos:", vet_votos[x], "votos.", vet_votos[x]*100/sum(vet_votos),"% dos votos totais\n")
  }else{
    cat("Candidato",x,":", vet_votos[x], "votos.", vet_votos[x]*100/sum(vet_votos),"% dos votos totais\n")
  }

}

#letra c. Nulos e Brancos são inválidos

vet_votos_validos = c(cand1, cand2, cand3, cand4)
seg_turno = TRUE

candi1_segtur1 = 0
candi2_segtur1 = 0

for(x in 1:length(vet_votos_validos)){
  if(x >= (sum(vet_votos_validos)/2)+1){
    cat("O candidato", x, "eh o vencedor")
    seg_turno = FALSE
  }
}



if(seg_turno){
#primeiro lugar  
  for(x in 1:length(vet_votos_validos)){
    if(vet_votos_validos[x] == max(vet_votos_validos) && candi1_segtur1==0){
      candi1_segtur1 = x
      vet_votos_validos[x] = 0
    }
  }
#segundo lugar
  for(x in 1:length(vet_votos_validos)){
    if(vet_votos_validos[x] == max(vet_votos_validos) && candi1_segtur1!=0){
      candi2_segtur1 = x
    }
  }
  
  cat("Segundo Turno entre", candi1_segtur1, "e o candidato", candi2_segtur1)
}

#Questão 2
valor = -10:10
vetor_aleat = sample(valor[valor!=0], 20)
print(vetor_aleat)

funcao_positiva = function(x){
  ifelse(x>0, x, FALSE)
}

funcao_negativa = function(x){
  ifelse(x<0, x, FALSE)
}

positivos = function(x){
  x[which(vetor_aleat(x)==TRUE)]
}

negativos = function(x){
  x[which(funcao_negativa(x)==TRUE)]
}


#letra a
sapply(vetor_aleat, funcao_positiva)
sum(positivos(vetor_aleat))

#letra b
sapply(vetor_aleat, funcao_negativa)
sum(negativos(vetor_aleat))

