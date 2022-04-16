#Filter Nation by using MADE
library(dplyr)
brazil = filter(owid_covid_data, location == "Brazil")
chile = filter(owid_covid_data, location == "Chile")
venezuela = filter(owid_covid_data, location == "Venezuela")
#Set day to correct form
brazil$date <- as.Date(brazil$date, format= "%m/%d/%Y")
chile$date <- as.Date(chile$date, format= "%m/%d/%Y")
venezuela$date <- as.Date(venezuela$date, format= "%m/%d/%Y")
#Ques 6
#Filter by month brazil

#Plot
#5.1-5.2-5.4-5.5
library("ggplot2")
foo2020 <- function(dataframe , xx, content, namefile){
  
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
  tikz(namefile, width = 4, height = 2, standAlone = FALSE)
  colors <- c("Tháng 5" = "blue", "Tháng 9" = "red")
  plot1 <- ggplot() + geom_line(data = filter(dataframe, date >= "2020-05-01" & date <= "2020-05-31"), mapping = aes(x = 1:31,  y = {{xx}}, color = "Tháng 5")) + 
    geom_line(data = filter(dataframe, date >= "2020-09-01" & date <= "2020-09-30"), mapping = aes(x = 1:30,  y = {{xx}}, color = "Tháng 9")) +
    labs(x = "Ngày", y = content, color = "Tháng") +
    scale_color_manual(values = colors)
  print(plot1)
  dev.off()
  return (plot1)
}

foo2021 <- function(dataframe, xx, content, namefile)
{
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
  tikz(namefile, width = 4, height = 2, standAlone = FALSE)  
  colors <- c("Tháng 1" = "black","Tháng 2" = "green","Tháng 5" = "blue", "Tháng 9" = "red")
    plot2 <- ggplot() + 
      geom_line(data = filter(dataframe, date >= "2021-01-01" & date <= "2021-01-31"), mapping = aes(x = 1:31, y = {{xx}}, color = "Tháng 1")) + 
      geom_line(data = filter(dataframe, date >= "2021-02-01" & date <= "2021-02-28"), mapping = aes(x = 1:28, y = {{xx}}, color = "Tháng 2")) +
      geom_line(data = filter(dataframe, date >= "2021-05-01" & date <= "2021-05-31"), mapping = aes(x = 1:31, y = {{xx}}, color = "Tháng 5")) +
      geom_line(data = filter(dataframe, date >= "2021-09-01" & date <= "2021-09-30"), mapping = aes(x = 1:30, y = {{xx}}, color = "Tháng 9")) +
      labs(x = "Ngày", y = content, color = "Tháng") +
      scale_color_manual(values = colors)
    print(plot2)
    dev.off()
    return (plot2) 
}

foo2022 <- function(dataframe, xx, content, namefile)
{
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
  tikz(namefile, width = 4, height = 2, standAlone = FALSE)  
  colors <- c("Tháng 1" = "blue", "Tháng 2" = "red")
    plot3 <- ggplot() + 
      geom_line(data = filter(dataframe, date >= "2022-01-01" & date <= "2022-01-31"), mapping = aes(x = 1:31, y = {{xx}}, color = "Tháng 1")) + 
      geom_line(data = filter(dataframe, date >= "2022-02-01" & date <= "2022-02-19"), mapping = aes(x = 1:19, y = {{xx}}, color = "Tháng 2")) +
      labs(x = "Ngày", y = content, color = "Tháng") +
      scale_color_manual(values = colors)
  print(plot3)
  dev.off()
    return (plot3)
}

foocombine <- function(dataframe, xx, namefile)
{
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
  tikz(namefile, width = 4, height = 2, standAlone = FALSE)
  plot20 <- ggplot(dataframe, aes(x = date, y = {{xx}})) + geom_bar(stat = "identity") + 
    labs(x = "Ngày", y = "Số ca tháng 11 - 12", color = "Tháng")
  print(plot20)
  dev.off()
  return (plot20)
}
#5.1
#Brazil
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('d.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(brazil,new_cases, "Số ca nhiễm bệnh 2020") 
dev.off()

options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('e.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(brazil,new_cases, "Số ca nhiễm bệnh 2021")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('f.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(brazil,new_cases, "Số ca nhiễm bệnh 2022")
dev.off()
#Chile
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('j.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(chile,new_cases, "Số ca nhiễm bệnh 2020") 
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('k.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(chile,new_cases, "Số ca nhiễm bệnh 2021")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('i.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(chile,new_cases, "Số ca nhiễm bệnh 2022")
dev.off()
#Venezuela
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('h.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(venezuela,new_cases, "Số ca nhiễm bệnh 2020")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('t.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(venezuela,new_cases, "Số ca nhiễm bệnh 2021")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('g.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(venezuela,new_cases, "Số ca nhiễm bệnh 2022")
dev.off()
#5.2
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('l.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(brazil,new_deaths, "Số ca tử vong 2020") 
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('m.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(brazil,new_deaths, "Số ca tử vong 2021")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('cc.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(brazil,new_deaths, "Số ca tử vong 2022")
dev.off()
#Chile
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('bb.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(chile,new_deaths, "Số ca tử vong 2020") 
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('dd.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(chile,new_deaths, "Số ca tử vong 2021")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ee.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(chile,new_deaths, "Số ca tử vong 2022")
dev.off()
#Venezuela
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ff.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(venezuela,new_deaths, "Số ca tử vong 2020") 
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('gg.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(venezuela,new_deaths, "Số ca tử vong 2021")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('hh.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(venezuela,new_deaths, "Số ca tử vong 2022")
dev.off()
#5.4
#Brazil
month11.12.2020b <- filter(brazil, date >= "2020-11-01" & date <= "2020-12-31")
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('c.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2020b, new_cases)
dev.off()

month11.12.2021b <- filter(brazil, date >= "2021-11-01" & date <= "2021-12-31")
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('xx.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2021b, new_cases)
dev.off()
#Chile
month11.12.2020c <- filter(chile, date >= "2020-11-01" & date <= "2020-12-31")
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('yy.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2020c, new_cases)
dev.off()

month11.12.2021c <- filter(chile, date >= "2021-11-01" & date <= "2021-12-31")
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('zz.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2021c, new_cases)
dev.off()
#Venezuela
month11.12.2020v <- filter(venezuela, date >= "2020-11-01" & date <= "2020-12-31")
month11.12.2021v <- filter(venezuela, date >= "2021-11-01" & date <= "2021-12-31")
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('kkk.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2020v, new_cases)
dev.off()
foocombine(month11.12.2021v, new_cases)

#5.5
#brazil
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ss.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2020b, new_deaths)
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('rr.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2021b, new_deaths)
dev.off()
#Chile
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ww.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2020c,  new_deaths)
dev.off()

options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('aaa.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2021c,  new_deaths)
dev.off()
#Venezuela
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('bbb.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2020v,  new_deaths)
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ccc.tex', width = 4, height = 2, standAlone = FALSE)
foocombine(month11.12.2021v,  new_deaths)
dev.off()
#5.7
#Filter and find cumsum
fooCumSum <- function(nation)
{
  cumsth <- bind_rows(data_frame(date = filter(nation, date >= "2020-5-01" & date <= "2020-5-31")$date,case = cumsum(filter(nation, date >= "2020-5-01" & date <= "2020-5-31")$new_cases), death = cumsum(filter(nation, date >= "2020-5-01" & date <= "2020-5-31")$new_deaths)),
                      data_frame(date = filter(nation, date >= "2020-9-01" & date <= "2020-9-30")$date,case = cumsum(filter(nation, date >= "2020-9-01" & date <= "2020-9-30")$new_cases), death = cumsum(filter(nation, date >= "2020-9-01" & date <= "2020-9-30")$new_deaths)),
                      data_frame(date = filter(nation, date >= "2021-1-01" & date <= "2021-1-31")$date,case = cumsum(filter(nation, date >= "2021-1-01" & date <= "2021-1-31")$new_cases), death = cumsum(filter(nation, date >= "2021-1-01" & date <= "2021-1-31")$new_deaths)),
                      data_frame(date = filter(nation, date >= "2021-2-01" & date <= "2021-2-28")$date,case = cumsum(filter(nation, date >= "2021-2-01" & date <= "2021-2-28")$new_cases), death = cumsum(filter(nation, date >= "2021-2-01" & date <= "2021-2-28")$new_deaths)),
                      data_frame(date = filter(nation, date >= "2021-5-01" & date <= "2021-5-31")$date,case = cumsum(filter(nation, date >= "2021-5-01" & date <= "2021-5-31")$new_cases), death = cumsum(filter(nation, date >= "2021-5-01" & date <= "2021-5-31")$new_deaths)),
                      data_frame(date = filter(nation, date >= "2021-9-01" & date <= "2021-9-30")$date,case = cumsum(filter(nation, date >= "2021-9-01" & date <= "2021-9-30")$new_cases), death = cumsum(filter(nation, date >= "2021-9-01" & date <= "2021-9-30")$new_deaths)),
                      data_frame(date = filter(nation, date >= "2022-1-01" & date <= "2022-1-31")$date,case = cumsum(filter(nation, date >= "2022-1-01" & date <= "2022-1-31")$new_cases), death = cumsum(filter(nation, date >= "2022-1-01" & date <= "2022-1-31")$new_deaths)),
                      data_frame(date = filter(nation, date >= "2022-2-01" & date <= "2022-2-19")$date,case = cumsum(filter(nation, date >= "2022-2-01" & date <= "2022-2-19")$new_cases), death = cumsum(filter(nation, date >= "2022-2-01" & date <= "2022-2-19")$new_deaths)))
  return (cumsth)
}
cumbrazil <- fooCumSum(brazil)
cumchile <- fooCumSum(chile)
cumvenezuela <- fooCumSum(venezuela)
#Brazil
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ddd.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(cumbrazil, case, "Số ca nhiễm tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('eee.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(cumbrazil, case, "Số ca nhiễm tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('fff.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(cumbrazil, case, "Số ca nhiễm tích lũy")
dev.off()
#Chile
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ggg.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(cumchile, case, "Số ca nhiễm tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('hhh.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(cumchile, case, "Số ca nhiễm tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('iii.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(cumchile, case, "Số ca nhiễm tích lũy")
dev.off()
#Venezuela
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('jjj.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(cumvenezuela, case, "Số ca nhiễm tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ttt.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(cumvenezuela, case, "Số ca nhiễm tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('lll.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(cumvenezuela, case, "Số ca nhiễm tích lũy")
dev.off()

#5.8
#brazil
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('mmm.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(cumbrazil, death, "Số ca tử vong tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('nnn.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(cumbrazil, death, "Số ca tử vong tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ooo.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(cumbrazil, death, "Số ca tử vong tích lũy")
dev.off()

#Chile
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('ppp.tex', width = 4, height = 2, standAlone = FALSE)
foo2020(cumchile, death, "Số ca tử vong tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('qqq.tex', width = 4, height = 2, standAlone = FALSE)
foo2021(cumchile, death, "Số ca tử vong tích lũy")
dev.off()
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('rrr.tex', width = 4, height = 2, standAlone = FALSE)
foo2022(cumchile, death, "Số ca tử vong tích lũy")
dev.off()
#Venezuela
foo2020(cumvenezuela, death, "Số ca nhiễm tích lũy")
foo2021(cumvenezuela, death, "Số ca nhiễm tích lũy")
foo2022(cumvenezuela, death, "Số ca nhiễm tích lũy")

#6.1
#Change date to 7 days average
foo <- function(name)
{
  month5Average7 <- name
  for(i in 1:nrow(month5Average7))
  {
    if(is.na(month5Average7$new_cases[i]))
    {
      month5Average7$new_cases[i] = 0
    }
    
    if(is.na(month5Average7$new_deaths[i]))
    {
      month5Average7$new_deaths[i] = 0
    }
  }
  temp <- month5Average7$new_cases
  temp1 <- month5Average7$new_deaths
  for(i in 1:6)
  {
    sum <- 0
    sum1 <- 0
    for(j in 1:i)
    {
      sum <- sum + month5Average7$new_cases[j]
      sum1 <- sum1 + month5Average7$new_deaths[j]
    }
    temp[i] <- ceiling(sum/i)
    temp1[i] <- ceiling(sum1/i)
    format(temp1[i], scientific = FALSE)
    format(temp[i], scientific = FALSE)
  }
  
  for(i in 1:(nrow(month5Average7) - 6))
  {
    sum <- 0
    sum1 <- 0
    for(j in i:(i+6))
    {
      sum <- sum + month5Average7$new_cases[j]
      sum1 <- sum1 + month5Average7$new_deaths[j]
    }
    temp[i+6] <- ceiling(sum/7)
    temp1[i+6] <- ceiling(sum1/7)
    format(temp1[i+6], scientific = FALSE)
    format(temp[i+6], scientific = FALSE)
  }
  month5Average7$new_cases <- temp
  month5Average7$new_deaths <- temp1
  return (month5Average7)
}
sevendayBrazil <- foo(brazil)
sevendayChile <- foo(chile)
sevendayVenezuela <- foo(venezuela)
#filter
#brazil
#plot
#VI-1
#6.1
#Brazil
foo2020(sevendayBrazil,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'uuu.tex') 
foo2021(sevendayBrazil,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'vvv.tex')
foo2022(sevendayBrazil,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'yyy.tex')
#Chile
foo2020(sevendayChile,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'aaaa.tex') 
foo2021(sevendayChile,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'new2020c.tex')
foo2022(sevendayChile,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'new2022c.tex')
#Venezuela
foo2020(sevendayVenezuela,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'new2020v.tex') 
foo2021(sevendayVenezuela,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'new2021v.tex')
foo2022(sevendayVenezuela,new_cases, "Số ca nhiễm 7 ngày gần nhất", 'new2022v.tex')

#6.2
#Brazil
foo2020(sevendayBrazil,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2020b.tex') 
foo2021(sevendayBrazil,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2021b.tex')
foo2022(sevendayBrazil,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2022b.tex')
#Chile
foo2020(sevendayChile,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2020c.tex') 
foo2021(sevendayChile,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2021c.tex')
foo2022(sevendayChile,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2022c.tex')
#Venezuela
foo2020(sevendayVenezuela,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2020v.tex') 
foo2021(sevendayVenezuela,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2021v.tex')
foo2022(sevendayVenezuela,new_deaths, "Số ca tử vong 7 ngày gần nhất", 'death2022v.tex')

#6.4
#Brazil
smonth11.12.2020b <- filter(sevendayBrazil, date >= "2020-11-01" & date <= "2020-12-31")
smonth11.12.2021b <- filter(sevendayBrazil, date >= "2021-11-01" & date <= "2021-12-31")
foocombine(smonth11.12.2020b,  new_cases, 'n1112b.tex')
foocombine(smonth11.12.2021b,  new_cases, 'n1112b21.tex')

#Chile
smonth11.12.2020c <- filter(sevendayChile, date >= "2020-11-01" & date <= "2020-12-31")
smonth11.12.2021c <- filter(sevendayChile, date >= "2021-11-01" & date <= "2021-12-31")
foocombine(smonth11.12.2020c,  new_cases, 'n1112c.tex')
foocombine(smonth11.12.2021c,  new_cases, 'n1112c21.tex')

#Venezuela
smonth11.12.2020v <- filter(venezuela, date >= "2020-11-01" & date <= "2020-12-31")
smonth11.12.2021v <- filter(venezuela, date >= "2021-11-01" & date <= "2021-12-31")
foocombine(smonth11.12.2020v, new_cases, 'n1112v.tex')
foocombine(smonth11.12.2021v, new_cases, 'n1112v21.tex')
#6.5
#Brazil
foocombine(smonth11.12.2020b,  new_deaths, 'b6.5.2020.tex')
foocombine(smonth11.12.2021b,  new_deaths, 'b6.5.2021.tex')
#Chile
foocombine(smonth11.12.2020c,  new_deaths, 'c6.5.2020.tex')
foocombine(smonth11.12.2021c,  new_deaths, 'c6.5.2021.tex')

#Venezuela
foocombine(smonth11.12.2020v,  new_deaths, 'v6.5.2020.tex')
foocombine(smonth11.12.2021v,  new_deaths, 'v6.5.2021.tex')
#6.7
scumbrazil <- fooCumSum(sevendayBrazil)
scumchile <- fooCumSum(sevendayChile)
scumvenezuela <- fooCumSum(sevendayVenezuela)

#Brazil
foo2020(scumbrazil, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums1.tex')
foo2021(scumbrazil, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums2.tex')
foo2022(scumbrazil, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums3.tex')
#Chile
foo2020(scumchile, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums4.tex')
foo2021(scumchile, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums5.tex')
foo2022(scumchile, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums6.tex')
#Venezuela
foo2020(scumvenezuela, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums7.tex')
foo2021(scumvenezuela, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums8.tex')
foo2022(scumvenezuela, case, "Số ca nhiễm tích lũy 7 ngày gần nhất", 'cums9.tex')
#6.8
foo2020(scumbrazil, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum10.tex')
foo2021(scumbrazil, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum11.tex')
foo2022(scumbrazil, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum12.tex')
#Chile======
foo2020(scumchile, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum13.tex')
foo2021(scumchile, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum14.tex')
foo2022(scumchile, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum15.tex')
#Venezuela
foo2020(scumvenezuela, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum16.tex')
foo2021(scumvenezuela, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum17.tex')
foo2022(scumvenezuela, death, "Số ca tử vong tích lũy 7 ngày gần nhất", 'cum18.tex')
#X-2 
compareBrazil <- filter(month2.2022b, date >= "2022-02-13" & date <= "2022-02-19")
compareChile <- filter(month2.2022c, date >= "2022-02-13" & date <= "2022-02-19")
compareVenezuela <- filter(month2.2022v, date >= "2022-02-13" & date <= "2022-02-19")
colors <- c("Brazil" = "black","Chile" = "green","Venezuela" = "blue")

library(tikzDevice)
require(tikzDevice)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
tikz('b.tex', width = 5, height = 3, standAlone = FALSE)
plotCompare <- ggplot() + 
  geom_line(data = compareBrazil, mapping = aes(x = 13:19, y = new_deaths, color = "Brazil")) + 
  geom_line(data = compareChile, mapping = aes(x = 13:19, y = new_deaths, color = "Chile")) +
  geom_line(data = compareVenezuela, mapping = aes(x = 13:19, y = new_deaths, color = "Venezuela")) +
  labs(x = "Ngay", y = "Số ca tu vong 7 ngày cuoi 2022", color = "Thang") +
  scale_color_manual(values = colors)
print(plotCompare)
dev.off()

#5.6
foo11.12 <- function(dataframe, namefile)
{
  month11.12 = dataframe
  caseDeath <- data.frame(date = month11.12$date, number = month11.12$new_cases, type = "Nhiễm bệnh")
  caseDeath1 <- data.frame(date = month11.12$date, number = month11.12$new_deaths, type = "Tử vong")
  temp <- bind_rows(caseDeath1,caseDeath)
  options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
  tikz(namefile, width = 4, height = 2, standAlone = FALSE)  
  plot <- ggplot(temp, aes(x = date, y = number, fill = type)) +
    geom_col(position = position_stack(reverse = TRUE), colour = "black") +
    guides(fill = guide_legend(reverse = TRUE)) + labs(x = "Ngày", y = "Nhiễm bệnh và tử vong")
  print(plot)
  dev.off()
  return (plot)
}
foo11.12(month11.12.2020b, 'nhi.tex')
foo11.12(month11.12.2021b, 'nhi1.tex')

foo11.12(month11.12.2020c, 'nhi2.tex')
foo11.12(month11.12.2021c, 'nhi3.tex')

foo11.12(month11.12.2020v, 'nhi4.tex')
foo11.12(month11.12.2021v, 'nhi5.tex')
#5.3

#6.3

#6.6
foo11.12(smonth11.12.2020b, 'nhi6.tex')
foo11.12(smonth11.12.2021b, 'nhi7.tex')

foo11.12(smonth11.12.2020c, 'nhi8.tex')
foo11.12(smonth11.12.2021c, 'nhi9.tex')

foo11.12(smonth11.12.2020v, 'nhi10.tex')
foo11.12(smonth11.12.2021v, 'nhi11.tex')
#Comment of plot : Nhìn vào biểu đồ, ta có thể thấy được, số ca tử vong từ ngày 13 đến ngày 19 tháng 12 năm 2022 của Brazil chiếm số lượng lớn so với Chile và Venezuela. Số ca tử vong tại Brazil ngày 13 tháng 12 là 300 ca, trong khi số liệu liệu này tại Chile là dưới 150 ca và tại Brazil là gần như bằng 0. Trong khoảng thời gian này, tại Brazil, số ca tử vong tăng mạnh và đạt đỉnh điểm vào ngày 18 tháng 12 ( gần 1200 ca ) và sau đó giảm xuống gần 900 ca vào ngày cuối cùng. Trong khi đó, tại Chile, số ca tử vong dao động dưới 300 ca trong khoảng thời gian cuối năm 2022. Điều đặc biệt, số ca tử vong tại Venezuela gần như là không thay đổi trong khoảng thời gian trên và có tăng nhẹ vào ngày 18 tháng 12 nhưng sau đó giảm lại vào ngày cuối cùng.
