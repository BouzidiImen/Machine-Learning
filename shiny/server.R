
library(shiny)
library(plotly)
library(shinydashboard)
library(dashboardthemes)

library(plotly)
library(caret)


shinyServer(function(input, output) {
  ############ Hedonic Data #############
 
  output$sum <-renderPrint({
    summary(base)
  })
  output$plot <-renderPlotly({
    switch(input$Var,
           Area={
             
             t=table(base$QIAREA)
             t=round(prop.table(t)*100,1)
             t=data.frame(t)
             Area=t$Var1
             Frequency=t$Freq
             p=plot_ly(x=Area,y = Frequency,name = "Region",marker = list(color = c('#009999','#34495e')),
                       type = "bar")%>%layout(title='Distribution of the population by Area',xaxis=list(title ='Area'),yaxis = list(title = 'Frequency'))
             p
           }
           ,
           Genre={
             t=table(base$Genre)
             t=data.frame(t)
             Genre=t$Var1
             Frequency=t$Freq
             plot_ly(t, labels =Genre , values =Frequency, type = 'pie',hole=0.6,
                     insidetextfont = list(color = '#FFFFFF'),
                     marker = list(colors = c('#45b39d',' #424949'),
                                   line = list(color = '#FFFFFF', width = 0.5))) %>%
               layout(title = "Distribution of the population by Genre",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
           },Age={
             Age=base$Age
             p <- plot_ly( y = ~Age, type = "box") 
             p
             
           }
           ,Matrimonial_status={
             
             t=table(base$Statut_mat)
             t=round(prop.table(t)*100,1)
             t=data.frame(t)
             Status=t$Var1
             Frequency=t$Freq
             p=plot_ly(x=Status,y = Frequency,name = "Region",marker = list(color = c('#424949 ')),
                       type = "bar")%>%layout(title='Distribution of the population by Martial status',yaxis = list(title = 'Frequency'),xaxis=list(title='Status'))
             p
           },
           Profession={
             t=table(base$Profession)
             t=round(prop.table(t)*100,1)
             t=data.frame(t)
             Profession=t$Var1
             Frequency=t$Freq
             p=plot_ly(x=Profession,y = Frequency,name = "Region",marker = list(color = c('#424949 ')),
                       type = "bar")%>%layout(title='Distribution of the population by Profession',yaxis = list(title = 'Frequency'),xaxis=list(title='Profession'))
             p
             
           },
           Physical_Activity={
             ## physical act 
             t=table(base$activite_physique)
             t=data.frame(t)
             Act_phys=t$Var1
             Frequency=t$Freq
             plot_ly(t, labels =Act_phys , values =Frequency, type = 'pie',hole=0.6,
                     insidetextfont = list(color = '#FFFFFF'),
                     marker = list(colors = c('#45b39d',' #424949','#34495e'),
                                   line = list(color = '#FFFFFF', width = 0.5))) %>%
               layout(title = "Distribution of the population by Physical activity",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
             
             
           },
           Alcohol_consumption={
             t=table(base$Conso_alcohol)
             t=data.frame(t)
             Fum=t$Var1
             Frequency=t$Freq
             plot_ly(t, labels =Fum, values =Frequency, type = 'pie',hole=0.6,
                     insidetextfont = list(color = '#FFFFFF'),
                     marker = list(colors = c('#45b39d',' #424949','#34495e'),
                                   line = list(color = '#FFFFFF', width = 0.5))) %>%
               layout(title = "Distribution of the population by Consumption of cigarettes",
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
           },
            Education_level={
              t=table(base$Education)
              t=round(prop.table(t)*100,1)
              t=data.frame(t)
              Ed_level=t$Var1
              Frequency=t$Freq
              p=plot_ly(x=Ed_level,y = Frequency,name = "Region",marker = list(color = c('#424949 ')),
                        type = "bar")%>%layout(title='Distribution of the population by Education level',yaxis = list(title = 'Frequency'),xaxis=list(title='Education level'))
              p
            },
Diet={t=table(base$alim)
t=data.frame(t)
Fum=t$Var1
Frequency=t$Freq
plot_ly(t, labels =Fum, values =Frequency, type = 'pie',hole=0.6,
        insidetextfont = list(color = '#FFFFFF'),
        marker = list(colors = c('#45b39d',' #424949','#34495e'),
                      line = list(color = '#FFFFFF', width = 0.5))) %>%
  layout(title = "Distribution of the population by Balanced Diet",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))}
,Tabagism={t=table(base$Fum)
t=data.frame(t)
Fum=t$Var1
Frequency=t$Freq
plot_ly(t, labels =Fum, values =Frequency, type = 'pie',hole=0.6,
        insidetextfont = list(color = '#FFFFFF'),
        marker = list(colors = c('#45b39d',' #424949','#34495e'),
                      line = list(color = '#FFFFFF', width = 0.5))) %>%
  layout(title = "Distribution of the population by Consumption of cigarettes",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))}
           
           ,
Diabetes={
  
  
  t=table(base$Diab_G)
  t=data.frame(t)
  Fum=t$Var1
  Frequency=t$Freq
  plot_ly(t, labels =Fum, values =Frequency, type = 'pie',hole=0.6,
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(colors = c('#45b39d',' #424949','#34495e'),
                        line = list(color = '#FFFFFF', width = 0.5))) %>%
    layout(title = "Distribution of the population by Diabetes",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}
           
           )
  })
  output$tabl <- DT::renderDataTable({
    switch(input$Var2,
           Area={t=table(base$QIAREA,base$Diab_G)},
           Genre={t=table(base$Genre,base$Diab_G)},
           Age={return()}
           ,Matrimonial_status={t=table(base$Statut_mat,base$Diab_G)},
           Profession={t=table(base$Profession,base$Diab_G)},
           Physical_Activity={t=table(base$activite_physique,base$Diab_G)},
           Alcohol_consumption={t=table(base$Conso_alcohol,base$Diab_G)},
           Education_level={t=table(base$Education,base$Diab_G) },
           Diet={t=table(base$alim,base$Diab_G)},
           Tabagism={t=table(base$Fum,base$Diab_G)}
           
    )
    DT::datatable(t,options = list(scrollX = TRUE))
  })
  output$x3 = downloadHandler('table.csv' , content = function(file) {
    switch(input$Var2,
           Area={t=table(base$QIAREA,base$Diab_G)},
           Genre={t=table(base$Genre,base$Diab_G)},
           Age={return()}
           ,Matrimonial_status={t=table(base$Statut_mat,base$Diab_G)},
           Profession={t=table(base$Profession,base$Diab_G)},
           Physical_Activity={t=table(base$activite_physique,base$Diab_G)},
           Alcohol_consumption={t=table(base$Conso_alcohol,base$Diab_G)},
           Education_level={t=table(base$Education,base$Diab_G) },
           Diet={t=table(base$alim,base$Diab_G)},
           Tabagism={t=table(base$Fum,base$Diab_G)}
           
    )
    write.table(t, file,sep=";",row.names = F)
  })
  
  
  
  
  output$plot2 <-renderPlotly({
    switch(input$Var2,
           Area={
             t=table(base$QIAREA,base$Diab_G)
           t=data.frame(t)
           f=t$Freq[which(t$Var1=='Rural')]
           f=c(round((f/sum(f))*100,2))
           h=t$Freq[which(t$Var1=='Urban')]
           h=c(round((h/sum(h))*100,2))
           type=c('Urban','Rural')
           No=c(h[1],f[1])
           Yes=c(h[2],f[2])
           
           plot_ly(x = type, y = No, type = 'bar', name = 'No',
                   marker = list(color = '#154360')) %>%
             add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
             layout(title = "Diabetes by Area",
                    xaxis = list(title = 'Frequency'), barmode = 'stack')},
           Genre={
             
             t=table(base$Genre,base$Diab_G)
             t=data.frame(t)
             f=t$Freq[which(t$Var1=='Female')]
             f=c(round((f/sum(f))*100,2))
             h=t$Freq[which(t$Var1=='Male')]
             h=c(round((h/sum(h))*100,2))
             type=c('Male','Female')
             No=c(h[1],f[1])
             Yes=c(h[2],f[2])
             
             plot_ly(x = type, y = No, type = 'bar', name = 'No',
                     marker = list(color = '#154360')) %>%
               add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
               layout(title = "Diabetes by genre",
                      xaxis = list(title = 'Frequency'), barmode = 'stack')
             
             
           },
           Age={
             No=base$Age[which(base$Diab_G=='No')]
             Yes=base$Age[which(base$Diab_G=='Yes')]
             p <- plot_ly() %>%
               add_boxplot(y = No, marker = list(color = 'rgb(7,40,89)'),name = "No") %>%
               add_boxplot(y = Yes, name = "Yes",
                           marker = list(color = 'rgb(9,56,125)')) %>%
               layout(title = "Box plot of Diabetes by age",yaxis=list(title='Age'))
             
           }
           ,Matrimonial_status={t=table(base$Statut_mat,base$Diab_G)
           
           t=data.frame(t)
           f=t$Freq[which(t$Var1=='Currently married')]
           f=c(round((f/sum(f))*100,2))
           h=t$Freq[which(t$Var1=='Never married')]
           h=c(round((h/sum(h))*100,2))
           l=t$Freq[which(t$Var1=='Separated/ Divorced')]
           l=c(round((l/sum(l))*100,2))
           k=t$Freq[which(t$Var1=='Widowed')]
           k=c(round((k/sum(k))*100,2))
           type=c('Never married','Currently married','Separated/ Divorced','Widowed')
           No=c(h[1],f[1],l[1],k[1])
           Yes=c(h[2],f[2],l[2],k[2])
           
           plot_ly(x = type, y = No, type = 'bar', name = 'No',
                   marker = list(color = '#154360')) %>%
             add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
             layout(title = "Diabetes by Martial Status",
                    xaxis = list(title = 'Frequency'), barmode = 'stack')
           },
           Profession={t=table(base$Profession,base$Diab_G)
           t=data.frame(t)
           f=t$Freq[which(t$Var1=='Elementary')]
           f=c(round((f/sum(f))*100,2))
           h=t$Freq[which(t$Var1=='Intermediate')]
           h=c(round((h/sum(h))*100,2))
           l=t$Freq[which(t$Var1=='No occupation')]
           l=c(round((l/sum(l))*100,2))
           k=t$Freq[which(t$Var1=='other')]
           k=c(round((k/sum(k))*100,2))
           s=t$Freq[which(t$Var1=='Retired')]
           s=c(round((s/sum(s))*100,2))
           a=t$Freq[which(t$Var1=='Senior')]
           a=c(round((a/sum(a))*100,2))
           
           type=c('Intermediate','Elementary','No occupation','other','Retired ','Senior')
           No=c(h[1],f[1],l[1],k[1],s[1],a[1])
           Yes=c(h[2],f[2],l[2],k[2],s[2],a[2])
           
           plot_ly(x = type, y = No, type = 'bar', name = 'No',
                   marker = list(color = '#154360')) %>%
             add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
             layout(title = "Diabetes by Profession",
                    xaxis = list(title = 'Frequency'), barmode = 'stack')},
           Physical_Activity={
             t=table(base$activite_physique,base$Diab_G)
             t=data.frame(t)
             f=t$Freq[which(t$Var1=='Hight')]
             f=c(round((f/sum(f))*100,2))
             h=t$Freq[which(t$Var1=='Low')]
             h=c(round((h/sum(h))*100,2))
             l=t$Freq[which(t$Var1=='Moderate')]
             l=c(round((l/sum(l))*100,2))
             
             
             type=c('Low','Hight','Moderate')
             No=c(h[1],f[1],l[1])
             Yes=c(h[2],f[2],l[2])
             
             plot_ly(x = type, y = No, type = 'bar', name = 'No',
                     marker = list(color = '#154360')) %>%
               add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
               layout(title = "Diabetes by Physical activity",
                      xaxis = list(title = 'Frequency'), barmode = 'stack')
           },
           Alcohol_consumption={
             t=table(base$Conso_alcohol,base$Diab_G)
             t=data.frame(t)
             f=t$Freq[which(t$Var1=='No never')]
             f=c(round((f/sum(f))*100,2))
             h=t$Freq[which(t$Var1=='Refusal')]
             h=c(round((h/sum(h))*100,2))
             l=t$Freq[which(t$Var1=='Yes')]
             l=c(round((l/sum(l))*100,2))
             
             type=c('Refusal','No never','Yes')
             No=c(h[1],f[1],l[1])
             Yes=c(h[2],f[2],l[2])
             
             plot_ly(x = type, y = No, type = 'bar', name = 'No',
                     marker = list(color = '#154360')) %>%
               add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
               layout(title = "Diabetes by Alcohol consumption",
                      xaxis = list(title = 'Frequency'), barmode = 'stack')
             
           },
           Education_level={t=table(base$Education,base$Diab_G)
           t=data.frame(t)
           f=t$Freq[which(t$Var1=='Not educated')]
           f=c(round((f/sum(f))*100,2))
           h=t$Freq[which(t$Var1=='Primary')]
           h=c(round((h/sum(h))*100,2))
           l=t$Freq[which(t$Var1=='Secondary')]
           l=c(round((l/sum(l))*100,2))
           k=t$Freq[which(t$Var1=='University')]
           k=c(round((k/sum(k))*100,2))
           type=c('Primary','Not educated','Secondary','University')
           No=c(h[1],f[1],l[1],k[1])
           Yes=c(h[2],f[2],l[2],k[2])
           
           plot_ly(x = type, y = No, type = 'bar', name = 'No',
                   marker = list(color = '#154360')) %>%
             add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
             layout(title = "Diabetes by Education level",
                    xaxis = list(title = 'Frequency'), barmode = 'stack')
           
           },
           Diet={
             t=table(base$alim,base$Diab_G)
             t=data.frame(t)
             f=t$Freq[which(t$Var1=='No')]
             f=c(round((f/sum(f))*100,2))
             h=t$Freq[which(t$Var1=='Yes')]
             h=c(round((h/sum(h))*100,2))
             
             type=c('Yes','No')
             No=c(h[1],f[1])
             Yes=c(h[2],f[2])
             
             plot_ly(x = type, y = No, type = 'bar', name = 'No',
                     marker = list(color = '#154360')) %>%
               add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
               layout(title = "Diabetes by Balanced Diet ",
                      xaxis = list(title = 'Frequency'), barmode = 'stack')
           },
           Tabagism={
             t=table(base$Fum,base$Diab_G)
             t=data.frame(t)
             h=t$Freq[which(t$Var1=='Not a smoker')]
             h=c(round((h/sum(h))*100,2))
             
             f=t$Freq[which(t$Var1=='Ex-smoker')]
             f=c(round((f/sum(f))*100,2))
             
             l=t$Freq[which(t$Var1=='Smoker')]
             l=c(round((l/sum(l))*100,2))
             type=c('Not a smoker','Ex-smoker','Smoker')
             No=c(h[1],f[1],l[1])
             Yes=c(h[2],f[2],l[2])
             
             plot_ly(x = type, y = No, type = 'bar', name = 'No',
                     marker = list(color = '#154360')) %>%
               add_trace(y = Yes, name = 'Yes',marker = list(color = '#45b39d ')) %>%
               layout(title = "Diabetes by Tabagism",
                      xaxis = list(title = 'Frequency'), barmode = 'stack')
             
           }
           
    )
   
  })
  
  output$chisq<-renderPrint({
    switch(input$Var2,
           Area={t=table(base$QIAREA,base$Diab_G)
           print(chisq.test(t))},
           Genre={t=table(base$Genre,base$Diab_G)
           print(chisq.test(t))},
           Age={No=base$Age[which(base$Diab_G=='No')]
           Yes=base$Age[which(base$Diab_G=='Yes')]
           print(shapiro.test(No))
           print(shapiro.test(Yes))
           print(wilcox.test(base$Age ~base$Diab_G))
           }
           ,Matrimonial_status={t=table(base$Statut_mat,base$Diab_G)
           print(chisq.test(t))},
           Profession={t=table(base$Profession,base$Diab_G)
           print(chisq.test(t))},
           Physical_Activity={t=table(base$activite_physique,base$Diab_G)
           print(chisq.test(t))},
           Alcohol_consumption={t=table(base$Conso_alcohol,base$Diab_G)
           print(chisq.test(t))},
           Education_level={t=table(base$Education,base$Diab_G)
           print(chisq.test(t)) },
           Diet={t=table(base$alim,base$Diab_G)
           print(chisq.test(t))
           },
           Tabagism={t=table(base$Fum,base$Diab_G)
           print(chisq.test(t))}
           
    )
  })

 

  })
