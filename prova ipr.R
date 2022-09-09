install.packages("Matrix", type = "source")
#professora, tive q instalar esse pct para contar ocorrencias na ultima questao
#Aluno: Diego Torres

a = 2; b = 3; c = 5
#Questao 1, letra a

x = (60/a*(-7-3*b+c)**a) + (3*c+(b/(-1*a))-1)
print(x)

#letra b
y = (sqrt(125/b)+(a+b)**2-2*a)/(b**a+sqrt(c-b+a))
print(y)

#letra c
R = !(x>=y) & ((x/y)!=0) | (y<=x) & (y<=sqrt(x))
print(R)

#questao 2
x_v = 1:16
X = matrix(x_v, 4, 4, byrow = T)

y_v1 = c(1,0,0,0)
y_v2 = c(0,2,0,0)
y_v3 = c(0,0,3,0)
y_v4 = c(0,0,0,4)
Y = rbind(y_v1, y_v2, y_v3, y_v4)

v_1 = 4:1
v = matrix(v_1, 4, 1, byrow = T)


#letra a

l_a = X %*% (solve(Y)) -  (t(X)-Y) %*% solve(Y%*%v+det(Y))
print(l_a)

#letra b
which(X%%2==0, arr.ind=T)


#Questao 3

resp_alu_1 = c("a", "b", "a", "d", "c")
resp_alu_2 = c("b", "a", "c", "d", "c")
resp_alu_3 = c("a", "b", "c", "d", "a")

resp = rbind(resp_alu_1, resp_alu_2, resp_alu_3)
gaba = rbind("a", "b", "c", "d", "a")

resultado_acertos_al1 = length(which(resp_alu_1==gaba))
resultado_erros_al1 = length(which(resp_alu_1!=gaba))
result_aluno1 = resultado_acertos_al1-resultado_erros_al1

resultado_acertos_al2 = length(which(resp_alu_2==gaba))
resultado_erros_al2 = length(which(resp_alu_2!=gaba))
result_aluno2 = resultado_acertos_al2-resultado_erros_al2

resultado_acertos_al3 = length(which(resp_alu_3==gaba))
resultado_erros_al3 = length(which(resp_alu_3!=gaba))
result_aluno3 = resultado_acertos_al3-resultado_erros_al3

resultado = rbind(result_aluno1, result_aluno2, result_aluno3)
resultado