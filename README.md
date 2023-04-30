# Titanic 

A regressão logística objetiva modelar o comportamento da
variável binária condicionada a variáveis explicativas através de uma estrutura regressiva que
permite estimar a probabilidade de ocorrência do evento de interesse. Esse modelo classifica-se
como um caso particular dos MLGs sendo que a distribuição da variável de interesse (binomial)
pertence à família de distribuições exponencial. 

Neste trabalho foi utilizada a regressão logística para modelar a probabilidade de sobrevivência dos tripulantes a partir de suas idades, sexo, classe. A análise de diagnóstico foi realizada com envelopes simulados dos resíduos e seu comportamento, alavancagem, DFFIT, distância de Cook, teste RESET (para validar a função de ligação) e pseudo R2.

[Modelagem MLG Titanic](TitanicRMD.pdf)

Uma função para aplicação do modelo final é apresentada abaixo.
```{r}
survive<- function(Sexo, Idade, Classe)
{  
  SEX <- Sexo # Feminino: 1 , Masc:0
  AGE <- Idade
  CLASSE <- Classe
  if(CLASSE == 1){
    PCLASS1<-1
    PCLASS2<-0
  } 
  else if(CLASSE == 2){
    PCLASS1<-0
    PCLASS2<-1
  }
  else if(CLASSE == 3){
    PCLASS1<-0
    PCLASS2<-0
  }
  else print(c("Classe Incorreta. Utilize classe 1, 2 ou 3"), quote = F)
  
  Int1 = SEX*PCLASS1
  Int2 = SEX*PCLASS2
  Int3 = AGE*PCLASS2
  
  g = -0.8686 -0.0349*(AGE)+1.4597*(PCLASS1)+1.7522*(PCLASS2)+2.2073*(Int1)+3.9433*(Int2)-0.076*(Int3)
  Eg <- (exp(g))/(1+exp(g))
  prob <-round(Eg,3)
  CH <- Eg/(1-Eg)
  
  print(c("Sua probabilidade de sobrevivência é de", prob), quote = F)
  
  if(CH >= 1) print(c("E sua chande de sobreviver é de", CH, "vezes maior!"),quote = F)
  else print(c("E sua chance de sobreviver é de", CH, "vezes menor!"), quote = F)
}
```
Por exemplo, para obter a minha probabilidade de sobrevivência estando na primeira classe:

> survive(Sexo=0, Idade=24, Classe=1)

Sua probabilidade de sobrevivência é de 0.439
E sua chance de sobreviver é de 0.782 vezes menor! 

> survive(Sexo=0, Idade=24, Classe=2)

Sua probabilidade de sobrevivência é de 0.145
E sua chance de sobreviver é de 0.169 vezes menor! 


Para calcular a razão de chances (comparar a chance de sucesso de um grupo em relação a outro grupo) fizemos:

$$ \frac{ProbSucesso]{ProbFracasso} $$
