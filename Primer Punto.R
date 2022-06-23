menu2<- function(){
  
  i<-0
  while (i!=1) {
    print("1. Guardar gapminder")
    print("2. Leer el archivo gapminder.xlsx")
    print("3. Diagrama de dispersión lifeExp vs pop")
    print("4. Diagrama de dispersión gdpPercap vs pop.")
    print("5. diagramas de cajas de la variable gdpPercap discriminados por continentes desde 1990 a 2007.")
    print("6. Salir")
    val<- as.integer(readline("Escoja una opción: "))
    if(val == 1){
      write.xlsx(gapminder, file = "gapminder.xlsx")
      
    }else if(val == 2){
      
      df_nuevo<<-read_excel("gapminder.xlsx")
      porcentaje<-round(length(df_nuevo$country)*0.1)
      indices<-sample(1:length(df_nuevo$country),porcentaje,replace = F )
      df_nuevo[indices, c("lifeEXP","pop", "gdpPercap")]<-NA
      print("El archivo se ha guardado como df_nuevo")
      
      
      
      
    }else if(val==3){
        lvp<<-ggplot(df_nuevo, aes(x= lifeExp, y= pop))+
        geom_point()+
        ggtitle("lifeExp vs pop")
        return(lvp)
    }else if(val == 4){
      gvp<<-ggplot(df_nuevo, aes(x= gdpPercap, y= pop))+
        geom_point()+
        ggtitle("gdpPercap vs pop")
      return(gvp)
    }else if(val == 5){
      bxplt<<- df_nuevo%>%
        filter(year>=1990 & year<=2007)%>%
        ggplot(aes(x = continent, y = gdpPercap, fill = continent))+
        geom_boxplot()+
        ggtitle("gdpPercap por continente")
      return(bxplt)
    }else if(val == 6){
      break
    }else{
      print("no hago nada")
    }  
  }
  
  
  }
