# загрузка пакетов
library('shiny')
library('dplyr')
library('data.table')
library('zoo')
library('lubridate')
library('ggplot2')
library('rjson')
library('lattice')
library('png')


DT.Code<-data.table(read.csv2('./data/Code.csv',sep = ';',header=TRUE, encoding = "UTF-8", as.is = T))
DT.Countries<-data.table(read.csv('./data/Countries.csv',sep = ';',header=TRUE,encoding="UTF-8"))



# функция, реализующая API (источник: UN COMTRADE)
source("https://raw.githubusercontent.com/alesalij/R-data/1f7e155d277fa7a8480326cf8ae405c3770969e7/API/comtrade_API.R")





# Функция shinyServer() определяет описывает действия и события,
# которые происходят при обновлении значений интерфейса

shinyServer(function(input, output) {
 
 
   
  
#   output$sp.text <- renderText({
#    paste('Вы выбрали вид ирисов: "',
#          # переменная, связанная со списком видов ирисов
#          input$spesies.to.plot,
#          '"', sep = '')
#  })
# Загрузка данных.
  observeEvent(input$make.plot, {
    for(i in 2:length(DT.Code[[1]])){
    if(input$name.prod==DT.Code$Name[i]){code.prod<-DT.Code$X.U.FEFF.Code[i]}
    }
    if(input$name.prod==DT.Code$Name[1]){code.prod<-"040510"}
    for(i in 1:length(DT.Countries[[1]])){
      if(input$name.country==DT.Countries$X.U.FEFF.Name[i]){code.country<-DT.Countries$ISO[i]}
    }

   
   
   
  # загрузка данных в цикле
  for (i in 2010:2015) {
    # таймер для ограничения API: не более запроса в секунду
    Sys.sleep(5)
    s2 <- get.Comtrade(r = 'all', p = code.country,
                       ps = as.character(i), freq="M",
                       rg = '1', cc = code.prod,
                       fmt="csv")
    # имя файла для сохранения
    file.name <- paste('./data/comtrade_', i, '.csv',
                       sep = '')
    # записать данные в файл
    write.csv(s2$data, file.name, row.names = F)
    # вывести сообщение в консоль
     print(paste('Данные за', i, 'год сохранены в файл',              file.name))
    # сделать запись в лог
    write(paste('Файл',
                paste('comtrade_', i, '.csv', sep = ''),
                'загружен', Sys.time()),
          file = './data/download.log', append = T)
  }
  
  # читаем всѐ в одну таблицу
  # флаг: является ли этот файл первым?
  flag.is.first <- T
  # цикл по номеру года (он есть в именах файлов)
  for (i in 2010:2015) {
    # собираем имя файла
    file.name <- paste('./data/comtrade_', i, '.csv',
                       sep = '')
    # читаем данные во фрейм
    df <- read.csv(file.name, header = T, as.is = T)
    if (flag.is.first) {
      # если это первый файл, просто копируем его
      DT <- df
      # и снимаем флаг
      flag.is.first <- F
    } else {
      # если это не первый файл,
      # добавляем строки в конец таблицы
      DT <- rbind(DT, df)
    }
    # переводим в формат data.table
    DT <- data.table(DT)
    # пишем сообщение в консоль
    print(paste('Файл ', file.name, ' прочитан.'))
  }
  
  
  # копируем имена в символьный вектор,
  # чтобы ничего не испортить
  DT<-as.data.frame(DT)
  nms <- colnames(DT)
  # заменить серии из двух и более точек на одну
  nms <- gsub('[.]+', '.', nms)
  # убрать все хвостовые точки
  nms <- gsub('[.]+$', '', nms)
  # заменить US на USD
  nms <- gsub('Trade.Value.US', 'Trade.Value.USD', nms)
  # проверяем, что всѐ получилось, и заменяем имена столбцов
  colnames(DT) <- nms
  DT<-as.data.table(DT)
  DT[, Netweight.kg:=as.double(Netweight.kg)]
  # считаем медианы и округляем до целого, как исходные данные
  DT[, round(median(.SD$Netweight.kg, na.rm = T), 0), by = Year]
  
  # заменяем пропуски на медианы
  DT[, Netweight.kg.median:=round(median(.SD$Netweight.kg,na.rm = T), 0),by = Year]
  
  # смотрим результат
  #DT[is.na(Netweight.kg), Year, Netweight.kg.median]
  
  # заменяем пропуски на медианы
  for(i in seq(1,length(DT[[1]]))){
    if(!is.na(DT$Netweight.kg[i])){
      DT$Netweight.kg.median[i]<-DT$Netweight.kg[i]
    }
  }
  # удаляем временные переменные
  rm(df, file.name, flag.is.first, i) 
   
  # делаем такой подсчѐт по каждому столбцу
  na.num <- apply(DT, 2, function(x) length(which(is.na(x))))
  # в каких столбцах все наблюдения пропущены?
  col.remove <- na.num == dim(DT)[1]
  DT <- DT[, !col.remove, with = F]
  
  output$table <- renderDataTable({
    DT
  }, options = list(lengthMenu = c(5, 10, 20), pageLength = 5))

   Plotg<-xyplot(Netweight.kg.median ~ Trade.Value.USD | as.factor(Year), data = DT,
                 ylab = 'Масса поставки',
                 xlab = 'Стоимость поставки',
                 main = 'График разброса стоимости поставки от массы поставки',
                 panel = function(x, y, ...) {
                   # вызов функции по умолчанию (график разброса)
                   panel.xyplot(x, y, ...)
                   # затем накладываем линии регрессии
                   panel.lmline(x, y, col = 'red') })
  output$ts.plot <- renderPlot({ Plotg
                                       
  })
  })


  observeEvent(input$save.png, {
    output$ts.plot <- renderPlot({ Plotg
      
    })
    # читаем всѐ в одну таблицу
    # флаг: является ли этот файл первым?
    flag.is.first <- T
    # цикл по номеру года (он есть в именах файлов)
    for (i in 2010:2015) {
      # собираем имя файла
      file.name <- paste('./data/comtrade_', i, '.csv',
                         sep = '')
      # читаем данные во фрейм
      df <- read.csv(file.name, header = T, as.is = T)
      if (flag.is.first) {
        # если это первый файл, просто копируем его
        DT <- df
        # и снимаем флаг
        flag.is.first <- F
      } else {
        # если это не первый файл,
        # добавляем строки в конец таблицы
        DT <- rbind(DT, df)
      }
      # переводим в формат data.table
      DT <- data.table(DT)
      # пишем сообщение в консоль
      print(paste('Файл ', file.name, ' прочитан.'))
    }
    
    
    # копируем имена в символьный вектор,
    # чтобы ничего не испортить
    DT<-as.data.frame(DT)
    nms <- colnames(DT)
    # заменить серии из двух и более точек на одну
    nms <- gsub('[.]+', '.', nms)
    # убрать все хвостовые точки
    nms <- gsub('[.]+$', '', nms)
    # заменить US на USD
    nms <- gsub('Trade.Value.US', 'Trade.Value.USD', nms)
    # проверяем, что всѐ получилось, и заменяем имена столбцов
    colnames(DT) <- nms
    DT<-as.data.table(DT)
    DT[, Netweight.kg:=as.double(Netweight.kg)]
    # считаем медианы и округляем до целого, как исходные данные
    DT[, round(median(.SD$Netweight.kg, na.rm = T), 0), by = Year]
    
    # заменяем пропуски на медианы
    DT[, Netweight.kg.median:=round(median(.SD$Netweight.kg,na.rm = T), 0),by = Year]
    
    # смотрим результат
    #DT[is.na(Netweight.kg), Year, Netweight.kg.median]
    
    # заменяем пропуски на медианы
    for(i in seq(1,length(DT[[1]]))){
      if(!is.na(DT$Netweight.kg[i])){
        DT$Netweight.kg.median[i]<-DT$Netweight.kg[i]
      }
    }
    # удаляем временные переменные
    rm(df, file.name, flag.is.first, i) 
    
    # делаем такой подсчѐт по каждому столбцу
    na.num <- apply(DT, 2, function(x) length(which(is.na(x))))
    # в каких столбцах все наблюдения пропущены?
    col.remove <- na.num == dim(DT)[1]
    DT <- DT[, !col.remove, with = F]
    Plotg<-xyplot(Netweight.kg.median ~ Trade.Value.USD | as.factor(Year), data = DT,
                  ylab = 'Масса поставки',
                  xlab = 'Стоимость поставки',
                  main = 'График разброса стоимости поставки от массы поставки',
                  panel = function(x, y, ...) {
                    # вызов функции по умолчанию (график разброса)
                    panel.xyplot(x, y, ...)
                    # затем накладываем линии регрессии
                    panel.lmline(x, y, col = 'red') })
    png(file = "./data/Rplot.png", bg = 'transparent')
    print(Plotg)
    dev.off()
 
       
  })
  # пока что здесь пусто
})