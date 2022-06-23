#ESTA FUNCION GENERA LOS ARCHIVOS PEDIDOS EXPERIMENTO A Y EXPERIMENTO B
experimentos<-function(){
  n<-as.integer(readline("Tamaño: "))
  print("Experimento A")
  x_a<-as.integer(readline("media: "))
  sd_a<-as.integer(readline("desviación estandar: "))
  a<-rnorm(n, mean = x_a, sd = sd_a)
  exp_a<-data.frame(a)
  write.csv(exp_a, "Experimento_a.csv")
  Experimento_a<<-read.csv("Experimento_a.csv")
  print("Experimento B")
  x_b<-as.integer(readline("media: "))
  sd_b<-as.integer(readline("desviación estandar: "))
  b<-rnorm(n, mean = x_b, sd = sd_b)
  exp_b<-data.frame(b)
  write.csv(exp_b,"Experimento_b.csv")
  Experimento_b<<-read.csv("Experimento_b.csv")
}
#ESTA FUNCION EJECUTA E IMPRIME DISTINTAS FUNCIONES SOBRE LOS ARCHIVOS PREVIAMENTE GENERADOS
#FUNCIONES RELACIONADAS CON LA RELACIÓN ENTRE AMBOS EXPERIMENTOS
inferencia<-function(){
  print("1. indicar si la diferencia en la media de los datos es estadísticamente significativa.")
  print("2. mostrar en pantalla la correlación de Pearson y Spearman de los datos.")
  print("3. graficar el diagrama de dispersión y la línea recta que aproxime los datos calculada por una regresión lineal por mínimos cuadrados.")
  val<-as.integer(readline("Escoja una una opción: "))
  if(val == 1){
    
    prueba<-t.test(Experimento_a$a,Experimento_b$b, var.equal = TRUE)
    pvalor<-round(prueba$p.value,digits = 3)
    if(prueba$p.value > 0.05){
      print(paste("El p valor, ",pvalor, ", es mayor que 0.05 por lo que la diferencia entre medias no es estadisticamente significativa"))
    }else if(prueba$p.value <= 0.05){
      print(paste("El p valor, ",pvalor, ", es menor o igual que 0.05 por lo que la diferencia entre medias es estadisticamente significativa"))
    }
  }
  else if(val == 2){
    prsn<-cor(Experimento_a$a, Experimento_b$b, method = "pearson")
    sprmn<-cor(Experimento_a$a, Experimento_b$b, method = "spearman")
    print(paste("Correlación de Pearson: ", prsn))
    print(paste("Correlación de Spearman: ", sprmn))
  }
  else if(val == 3){
    exps<-data.frame(a = Experimento_a$a, b =  Experimento_b$b)
    ggplot(exps, aes(x = a, y = b))+geom_point()+
      geom_smooth(method = "lm", se = FALSE)
  }
  else{
    print("nada")
  }
}

