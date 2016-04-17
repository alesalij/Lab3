# загрузка пакетов
library('shiny')
library('data.table')
# создаѐм директорию для данных, если она ещѐ не существует:
if (!file.exists('./data')) {
  dir.create('./data')
}
# создаѐм файл с логом загрузок, если он ещѐ не существует:
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}

fileURL <- 'https://raw.githubusercontent.com/alesalij/Rdata/master/Countries.csv'
# загружаем файл, если он ещѐ не существует,
# и делаем запись о загрузке в лог:
if (!file.exists('./data/Countries.csv')) {
  download.file(fileURL,
                './data/Countries.csv')
  # сделать запись в лог
  write(paste('Файл "Countries.csv" загружен',
              Sys.time()),
        file = './data/download.log', append = T)
}

fileURL <- 'https://raw.githubusercontent.com/alesalij/Rdata/master/Code.csv'
# загружаем файл, если он ещѐ не существует,
# и делаем запись о загрузке в лог:
if (!file.exists('./data/Code.csv')) {
  download.file(fileURL,
                './data/Code.csv')
  # сделать запись в лог
  write(paste('Файл "Code.csv" загружен',
              Sys.time()),
        file = './data/download.log', append = T)
}
DT.Code<-data.table(read.csv2('./data/Code.csv',sep = ';',header=TRUE, encoding = "UTF-8", as.is = T))
DT.Countries<-data.table(read.csv('./data/Countries.csv',sep = ';',header=TRUE,encoding="UTF-8"))

# размещение всех объектов на странице
shinyUI(
  # создать страницу с боковой панелью
  # и главной областью для отчѐтов
  pageWithSidebar(
    # название приложения:
    headerPanel('Построение графика'),
    # боковая панель:
    sidebarPanel(
      # выпадающее меню: вид ирисов
      # для фильтра наблюдений
      selectInput('name.country', # переменная
                  'Выберите страну', # подпись списка
                  # сам список:
                  c(as.vector(DT.Countries$X.U.FEFF.Name))),
      selectInput('name.prod', # переменная
                  'Выберите продукт', # подпись списка
                  # сам список:
                  c(as.vector(DT.Code$Name))),
      actionButton('make.plot', 'Загрузить данные и график')
    ),
    # главная область (пока пустая)
 
        
        # главная область
        mainPanel(
          # график ряда
          plotOutput('ts.plot'),
          # таблица данных
          dataTableOutput('table'),
          # кнопка сохранения данных
          actionButton('save.png', 'Сохранить график')
        )
      )
    )