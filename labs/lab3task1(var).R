library("ggplot2")
library("dplyr")
library("tidyverse")

get_tidy_data <- function() {
  unite_and_imp_cargo_data <- function(source_tib) {
    tidy_data <- source_tib %>% unite("Продовольственный", "ПродЭкспорт", "ПродИмпорт", sep=exp_imp_sep)
    tidy_data <- tidy_data %>% unite("ТЭК", "ТЭКЭкспорт", "ТЭКИмпорт", sep=exp_imp_sep)
    tidy_data <- tidy_data %>% unite("Химический", "ХимЭкспорт", "ХимИмпорт", sep=exp_imp_sep)
    tidy_data <- tidy_data %>% unite("Древесный", "ДревЭкспорт", "ДревИмпорт", sep=exp_imp_sep)
    tidy_data <- tidy_data %>% unite("Металлургический", "МетЭкспорт", "МетИмпорт", sep=exp_imp_sep)
    tidy_data <- tidy_data %>% unite("Машиностроение", "МашЭкспорт", "МашИмпорт", sep=exp_imp_sep)
    return(tidy_data)
  }
  
  load(file='ExpImp.RData')
  data = ExpImp
  data <- data  %>% mutate_all(funs(gsub("-", "0", .)))
  data <- data  %>% mutate_all(coalesce, "0") %>% filter(!str_detect(ExpImp$Регион, 
                                                                     'Федерация|федеральный округ|в том числе'))
  
  exp_imp_sep <- "_"
  tidy_data <- unite_and_imp_cargo_data(data)
  tidy_data <- tidy_data %>% pivot_longer(c("Продовольственный",
                                            "ТЭК",
                                            "Химический",
                                            "Древесный",
                                            "Металлургический",
                                            "Машиностроение"),
                                          names_to="Груз",
                                          values_to="Экспорт_Импорт")
  tidy_data <- tidy_data %>% separate("Экспорт_Импорт",
                                      into=c("Экспорт", "Импорт"),
                                      sep=exp_imp_sep)
  tidy_data <- tidy_data %>% pivot_longer(c("Экспорт", "Импорт"),
                                          names_to="Операция",
                                          values_to="Объём")
  return(tidy_data)
}


my_chart_1 <- function(tidy_data) {
  replace_NA_by_null <- function() {
    ind <- 1
    while (ind <= nrow(metall_data)) {
      if (metall_data$`Объём`[ind] == "-") {
        metall_data$`Объём`[ind] <<- "0"
      }
      ind <- ind + 1
    }
  }
  
  metall_data <- tidy_data %>% filter(Груз=="Металлургический")
  replace_NA_by_null()
  my_plot <- ggplot(data=metall_data,
                    aes(x=`Регион`, y=as.numeric(metall_data$`Объём`), 
                        fill=`Операция`))
  
  my_plot <- my_plot + geom_col(position="Dodge")
  my_plot <- my_plot + scale_fill_manual(labels = c("Импорт", "Экспорт"), values = c("blue", "orange"))
  my_plot <- my_plot + labs(title="Суммарный экспорт и импорт металлургической продукции в субъектах РФ",
                            x="Субъект РФ",
                            y="Объем, млн долл. США")
  my_plot <- my_plot + coord_flip()
  return(my_plot)
}


my_chart_2 <- function(tidy_data) {
  replace_NA_by_null <- function() {
    ind <- 1
    while (ind <= nrow(metall_data)) {
      if (metall_data$`Объём`[ind] == "-") {
        metall_data$`Объём`[ind] <<- "0"
      }
      ind <- ind + 1
    }
  }
  
  get_percents_and_reverse_import <- function() {
    exports <- metall_data %>% filter(Операция=="Экспорт")
    sum_export <- sum( as.numeric(exports$`Объём`) )
    
    imports <- metall_data %>% filter(Операция=="Импорт")
    sum_import <- sum( as.numeric(imports$`Объём`) )
    
    percents <- vector("character", 0)
    ind = 1
    while (ind <= nrow(metall_data)) {
      amount <- as.numeric(metall_data$`Объём`[ind])
      label <- ""
      if (metall_data$`Операция`[ind] == "Экспорт") {
        percent <- round(amount/sum_export*100, 1)
        if (percent >= min_shown_percent) {
          label <- paste0(as.character(amount), " (", as.character(percent), " %)")
        }
      }
      else {
        percent <- round(amount/sum_import*100, 1)
        if (percent >= min_shown_percent) {
          label = paste0(as.character(amount), " (", as.character(percent), " %)")
        }
        metall_data$`Объём`[ind] <<- -amount
      }
      percents <- c(percents, label)
      ind <- ind + 1
    }
    return(percents)
  }
  
  min_shown_percent <- 0.0
  metall_data <- tidy_data %>% filter(Груз=="Металлургический")
  replace_NA_by_null()
  percents <- get_percents_and_reverse_import()
  my_plot <- ggplot(data=metall_data, aes(x=`Регион`,
                                          y=as.numeric(metall_data$`Объём`),
                                          fill=`Операция`,
                                          percents=percents))
  my_plot <- my_plot + geom_col(position="Dodge")
  my_plot <- my_plot + scale_fill_manual(labels = c("Импорт", "Экспорт"), values = c("blue", "orange"))
  my_plot <- my_plot + labs(title="Суммарный экспорт и импорт металлургической продукции в субъектах РФ",
                            x="Субъект РФ",
                            y="Объем, млн долл. США")
  my_plot <- my_plot + geom_text(aes(label=percents), nudge_y=-1000)
  my_plot <- my_plot + coord_flip()
  return(my_plot)
}

tidy_data <- get_tidy_data()
my_chart_1(tidy_data)
my_chart_2(tidy_data)
