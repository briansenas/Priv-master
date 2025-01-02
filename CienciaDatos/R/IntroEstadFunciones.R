GraficoCajas = function (datos, variable_depend, variable_factor, titulo = ""){
  ggboxplot(datos, x =  variable_factor , y = variable_depend, 
            color = variable_factor, palette = "npg") + ggtitle(titulo)
}


GraficoCajasTest = function (datos, variable_depend, variable_factor, 
                            es_param, es_mr, test){
  grafico.cajas <- GraficoCajas(datos, variable_depend, variable_factor)
  titulo = MyTituloAnovaTest(es_param, es_mr, test)
  grafico.cajas + ggtitle(titulo)
}

GraficoCajasPost = function(datos, variable_depend, variable_factor, 
                            es_param, es_mr, test, 
                            control = "", estrellas = FALSE){
  grafico.cajas <- GraficoCajas(datos, variable_depend, variable_factor,)
  test <- test %>% add_xy_position() 
  titulo = MyTituloAnova(es_param, es_mr)
  titulo = paste0(titulo, "\nPost Hoc (pairwise comparisons -pwc-) para ")
  
  if (es_param){
    tit = "ANOVA"
  }
  else{
    if (!es_mr)
      tit = "Kruskal Wallis"
    else
      tit = "Friedmann"
  }
  
  titulo = paste0(titulo, tit, "\n")
  titulo = paste0(titulo, get_pwc_label(test, type = "text"))
  
  if (control != "")
    titulo = paste0(titulo, "\nGrupo de control: ", control)
  
  titulo = paste0(titulo, "\n")
  
  if (!estrellas)
    salida = "p.adj"
  else
    salida = "p.adj.signif"
  
  grafico.cajas + 
    stat_pvalue_manual(test, label = salida) +
    ggtitle(titulo)
}

MyTituloAnova = function(es_param = TRUE, es_mr = FALSE){
  titulo = "Test"
  
  if (es_param == FALSE)
    titulo = paste0(titulo, " no")
  
  titulo = paste0(titulo, " paramÃ©trico")
  
  if (es_mr == TRUE)
    titulo = paste0(titulo, " de medidas repetidas")
  
  titulo
}

MyTituloAnovaTest = function(es_param = TRUE, es_mr = FALSE, test){
  titulo = MyTituloAnova(es_param, es_mr)
  titulo = paste0(titulo, "\n", get_test_label(test,  
                                               detailed = TRUE, 
                                               type = "text"))
  # TambiÃ©n podrÃ­amos haber usado description
  titulo
}
