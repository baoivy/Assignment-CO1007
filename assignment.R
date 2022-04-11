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
foo2020 <- function(dataframe , xx, content){
  colors <- c("Tháng 5" = "blue", "Tháng 9" = "red")
  plot1 <- ggplot() + geom_line(data = filter(dataframe, date >= "2020-05-01" & date <= "2020-05-31"), mapping = aes(x = 1:31,  y = {{xx}}, color = "Tháng 5")) + 
    geom_line(data = filter(dataframe, date >= "2020-09-01" & date <= "2020-09-30"), mapping = aes(x = 1:30,  y = {{xx}}, color = "Tháng 9")) +
    labs(x = "Ngày", y = content, color = "Tháng") +
    scale_color_manual(values = colors)
  return (plot1)
}

foo2021 <- function(dataframe, xx, content)
{
    colors <- c("Tháng 1" = "black","Tháng 2" = "green","Tháng 5" = "blue", "Tháng 9" = "red")
    plot2 <- ggplot() + 
      geom_line(data = filter(dataframe, date >= "2021-01-01" & date <= "2021-01-31"), mapping = aes(x = 1:31, y = {{xx}}, color = "Tháng 1")) + 
      geom_line(data = filter(dataframe, date >= "2021-02-01" & date <= "2021-02-28"), mapping = aes(x = 1:28, y = {{xx}}, color = "Tháng 2")) +
      geom_line(data = filter(dataframe, date >= "2021-05-01" & date <= "2021-05-31"), mapping = aes(x = 1:31, y = {{xx}}, color = "Tháng 5")) +
      geom_line(data = filter(dataframe, date >= "2021-09-01" & date <= "2021-09-30"), mapping = aes(x = 1:30, y = {{xx}}, color = "Tháng 9")) +
      labs(x = "Ngày", y = content, color = "Tháng") +
      scale_color_manual(values = colors)
    return (plot2) 
}

foo2022 <- function(dataframe, xx, content)
{
    colors <- c("Tháng 1" = "blue", "Tháng 2" = "red")
    plot3 <- ggplot() + 
      geom_line(data = filter(dataframe, date >= "2022-01-01" & date <= "2022-01-31"), mapping = aes(x = 1:31, y = {{xx}}, color = "Tháng 1")) + 
      geom_line(data = filter(dataframe, date >= "2022-02-01" & date <= "2022-02-19"), mapping = aes(x = 1:19, y = {{xx}}, color = "Tháng 2")) +
      labs(x = "Ngày", y = content, color = "Tháng") +
      scale_color_manual(values = colors)
    return (plot3)
}

foocombine <- function(dataframe1, dataframe2, x)
{
  month11.12 <- bind_rows(dataframe1, dataframe2)
  plot20 <- ggplot(month11.12, aes(x = date, y = {{x}})) + geom_bar(stat = "identity") + 
    labs(x = "Ngày", y = "Số ca tháng 11 - 12", color = "Tháng")
  return (plot20)
}
#5.1
#Brazil
foo2020(brazil,new_cases, "Số ca nhiễm bệnh") 
foo2021(brazil,new_cases, "Số ca nhiễm bệnh")
foo2022(brazil,new_cases, "Số ca nhiễm bệnh")
#Chile
foo2020(chile,new_cases, "Số ca nhiễm bệnh") 
foo2021(chile,new_cases, "Số ca nhiễm bệnh")
foo2022(chile,new_cases, "Số ca nhiễm bệnh")
#Venezuela
foo2020(venezuela,new_cases, "Số ca nhiễm bệnh") 
foo2021(venezuela,new_cases, "Số ca nhiễm bệnh")
foo2022(venezuela,new_cases, "Số ca nhiễm bệnh")

#5.2
foo2020(brazil,new_deaths, "Số ca tử vong") 
foo2021(brazil,new_deaths, "Số ca tử vong")
foo2022(brazil,new_deaths, "Số ca tử vong")
#Chile
foo2020(chile,new_deaths, "Số ca tử vong") 
foo2021(chile,new_deaths, "Số ca tử vong")
foo2022(chile,new_deaths, "Số ca tử vong")
#Venezuela
foo2020(venezuela,new_deaths, "Số ca tử vong") 
foo2021(venezuela,new_deaths, "Số ca tử vong")
foo2022(venezuela,new_deaths, "Số ca tử vong")

#5.4
#Brazil
month11.2020b <- filter(brazil, date >= "2020-11-01" & date <= "2020-11-30")
month12.2020b <- filter(brazil, date >= "2020-12-01" & date <= "2020-12-31")
foocombine(month11.2020b, month12.2020b, new_cases)

month11.2021b <- filter(brazil, date >= "2021-11-01" & date <= "2021-11-30")
month12.2021b <- filter(brazil, date >= "2021-12-01" & date <= "2021-12-31")
foocombine(month11.2021b, month12.2021b, new_cases)
#Chile
month11.2020c <- filter(chile, date >= "2020-11-01" & date <= "2020-11-30")
month12.2020c <- filter(chile, date >= "2020-12-01" & date <= "2020-12-31")
foocombine(month11.2020c, month12.2020c, new_cases)

month11.2021c <- filter(chile, date >= "2021-11-01" & date <= "2021-11-30")
month12.2021c <- filter(chile, date >= "2021-12-01" & date <= "2021-12-31")
foocombine(month11.2021c, month12.2021c, new_cases)
#Venezuela
month11.2020v <- filter(venezuela, date >= "2020-11-01" & date <= "2020-11-30")
month12.2020v <- filter(venezuela, date >= "2020-12-01" & date <= "2020-12-31")
foocombine(month11.2020v, month12.2020v, new_cases)

month11.2021v <- filter(venezuela, date >= "2021-11-01" & date <= "2021-11-30")
month12.2021v <- filter(venezuela, date >= "2021-12-01" & date <= "2021-12-31")
foocombine(month11.2021v, month12.2021v, new_cases)

#5.5
#brazil
foocombine(month11.2020b, month12.2020b, new_deaths)
foocombine(month11.2020b, month12.2020b, new_deaths)
#Chile
foocombine(month11.2020b, month12.2020b, new_deaths)
foocombine(month11.2020b, month12.2020b, new_deaths)

#Venezuela
foocombine(month11.2020b, month12.2020b, new_deaths)
foocombine(month11.2020b, month12.2020b, new_deaths)

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
foo2020(cumbrazil, case, "Số ca nhiễm tích lũy")
foo2021(cumbrazil, case, "Số ca nhiễm tích lũy")
foo2022(cumbrazil, case, "Số ca nhiễm tích lũy")
#Chile
foo2020(cumchile, case, "Số ca nhiễm tích lũy")
foo2021(cumchile, case, "Số ca nhiễm tích lũy")
foo2022(cumchile, case, "Số ca nhiễm tích lũy")
#Venezuela
foo2020(cumvenezuela, case, "Số ca nhiễm tích lũy")
foo2021(cumvenezuela, case, "Số ca nhiễm tích lũy")
foo2022(cumvenezuela, case, "Số ca nhiễm tích lũy")

#5.8
#brazil
foo2020(cumbrazil, death, "Số ca nhiễm tích lũy")
foo2021(cumbrazil, death, "Số ca nhiễm tích lũy")
foo2022(cumbrazil, death, "Số ca nhiễm tích lũy")
#Chile
foo2020(cumchile, death, "Số ca nhiễm tích lũy")
foo2021(cumchile, death, "Số ca nhiễm tích lũy")
foo2022(cumchile, death, "Số ca nhiễm tích lũy")
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
foo2020(sevendayBrazil,new_cases, "Số ca nhiễm 7 ngày gần nhất") 
foo2021(sevendayBrazil,new_cases, "Số ca nhiễm 7 ngày gần nhất")
foo2022(sevendayBrazil,new_cases, "Số ca nhiễm 7 ngày gần nhất")
#Chile
foo2020(sevendayChile,new_cases, "Số ca nhiễm 7 ngày gần nhất") 
foo2021(sevendayChile,new_cases, "Số ca nhiễm 7 ngày gần nhất")
foo2022(sevendayChile,new_cases, "Số ca nhiễm 7 ngày gần nhất")
#Venezuela
foo2020(sevendayVenezuela,new_cases, "Số ca nhiễm 7 ngày gần nhất") 
foo2021(sevendayVenezuela,new_cases, "Số ca nhiễm 7 ngày gần nhất")
foo2022(sevendayVenezuela,new_cases, "Số ca nhiễm 7 ngày gần nhất")

#6.2
#Brazil
foo2020(sevendayBrazil,new_deaths, "Số ca tử vong 7 ngày gần nhất") 
foo2021(sevendayBrazil,new_deaths, "Số ca tử vong 7 ngày gần nhất")
foo2022(sevendayBrazil,new_deaths, "Số ca tử vong 7 ngày gần nhất")
#Chile
foo2020(sevendayChile,new_deaths, "Số ca tử vong 7 ngày gần nhất") 
foo2021(sevendayChile,new_deaths, "Số ca tử vong 7 ngày gần nhất")
foo2022(sevendayChile,new_deaths, "Số ca tử vong 7 ngày gần nhất")
#Venezuela
foo2020(sevendayVenezuela,new_deaths, "Số ca tử vong 7 ngày gần nhất") 
foo2021(sevendayVenezuela,new_deaths, "Số ca tử vong 7 ngày gần nhất")
foo2022(sevendayVenezuela,new_deaths, "Số ca tử vong 7 ngày gần nhất")

#6.4
#Brazil
smonth11.2020b <- filter(sevendayBrazil, date >= "2020-11-01" & date <= "2020-11-30")
smonth12.2020b <- filter(sevendayBrazil, date >= "2020-12-01" & date <= "2020-12-31")
foocombine(smonth11.2020b, smonth12.2020b, new_cases)

smonth11.2021b <- filter(sevendayBrazil, date >= "2021-11-01" & date <= "2021-11-30")
smonth12.2021b <- filter(sevendayBrazil, date >= "2021-12-01" & date <= "2021-12-31")
foocombine(smonth11.2021b, smonth12.2021b, new_cases)
#Chile
smonth11.2020c <- filter(sevendayChile, date >= "2020-11-01" & date <= "2020-11-30")
smonth12.2020c <- filter(sevendayChile, date >= "2020-12-01" & date <= "2020-12-31")
foocombine(smonth11.2020c, smonth12.2020c, new_cases)

smonth11.2021c <- filter(sevendayChile, date >= "2021-11-01" & date <= "2021-11-30")
smonth12.2021c <- filter(sevendayChile, date >= "2021-12-01" & date <= "2021-12-31")
foocombine(smonth11.2021c, smonth12.2021c, new_cases)
#Venezuela
smonth11.2020v <- filter(sevendayVenezuela, date >= "2020-11-01" & date <= "2020-11-30")
smonth12.2020v <- filter(sevendayVenezuela, date >= "2020-12-01" & date <= "2020-12-31")
foocombine(smonth11.2020v, smonth12.2020v, new_cases)

smonth11.2021v <- filter(sevendayVenezuela, date >= "2021-11-01" & date <= "2021-11-30")
smonth12.2021v <- filter(sevendayVenezuela, date >= "2021-12-01" & date <= "2021-12-31")
foocombine(smonth11.2021v, smonth12.2021v, new_cases)

#6.5
#Brazil
foocombine(month11.2020b, month12.2020b, new_deaths)
foocombine(month11.2020b, month12.2020b, new_deaths)
#Chile
foocombine(month11.2020b, month12.2020b, new_deaths)
foocombine(month11.2020b, month12.2020b, new_deaths)

#Venezuela
foocombine(month11.2020b, month12.2020b, new_deaths)
foocombine(month11.2020b, month12.2020b, new_deaths)
#6.7
scumbrazil <- fooCumSum(sevendayBrazil)
scumchile <- fooCumSum(sevendayChile)
scumvenezuela <- fooCumSum(sevendayVenezuela)

#Brazil
foo2020(scumbrazil, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2021(scumbrazil, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2022(scumbrazil, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
#Chile
foo2020(scumchile, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2021(scumchile, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2022(scumchile, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
#Venezuela
foo2020(scumvenezuela, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2021(scumvenezuela, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2022(scumvenezuela, case, "Số ca nhiễm tích lũy 7 ngày gần nhất")
#6.8
foo2020(scumbrazil, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2021(scumbrazil, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2022(scumbrazil, death, "Số ca nhiễm tích lũy")
#Chile
foo2020(scumchile, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2021(scumchile, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2022(scumchile, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
#Venezuela
foo2020(scumvenezuela, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2021(scumvenezuela, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
foo2022(scumvenezuela, death, "Số ca nhiễm tích lũy 7 ngày gần nhất")
#X-2 
compareBrazil <- filter(month2.2022b, date >= "2022-02-13" & date <= "2022-02-19")
compareChile <- filter(month2.2022c, date >= "2022-02-13" & date <= "2022-02-19")
compareVenezuela <- filter(month2.2022v, date >= "2022-02-13" & date <= "2022-02-19")
colors <- c("Brazil" = "black","Chile" = "green","Venezuela" = "blue")
plotCompare <- ggplot() + 
  geom_line(data = compareBrazil, mapping = aes(x = 13:19, y = new_deaths, color = "Brazil")) + 
  geom_line(data = compareChile, mapping = aes(x = 13:19, y = new_deaths, color = "Chile")) +
  geom_line(data = compareVenezuela, mapping = aes(x = 13:19, y = new_deaths, color = "Venezuela")) +
  labs(x = "Ngày", y = "Số ca tử vong 7 ngày cuối năm cuối", color = "Tháng") +
  scale_color_manual(values = colors)
plotCompare

#Comment of plot : Nhìn vào đồ thị, ta có thể thấy được, số ca tử vong từ ngày 13 đến ngày 19 tháng 12 năm 2022 của Brazil chiếm số lượng lớn so với Chile và Venezuela. Số ca tử vong tại Brazil ngày 13 tháng 12 là 300 ca, trong khi số liệu liệu này tại Chile là dưới 150 ca và tại Brazil là gần như bằng 0. Trong khoảng thời gian này, tại Brazil, số ca tử vong tăng mạnh và đạt đỉnh điểm vào ngày 18 tháng 12 ( gần 1200 ca ) và sau đó giảm xuống gần 900 ca vào ngày cuối cùng. Trong khi đó, tại Chile, số ca tử vong dao động dưới 300 ca trong khoảng thời gian cuối năm 2022. Điều đặc biệt, số ca tử vong tại Venezuela gần như là không thay đổi trong khoảng thời gian trên và có tăng nhẹ vào ngày 18 tháng 12 nhưng sau đó giảm lại vào ngày cuối cùng.
