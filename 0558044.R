file_path <- "C:/Users/ASUS/Desktop/R語言/報告/107072908043137.csv"
dengue1 <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE,
                     sep = ",")
View(dengue1)

file_path <- "C:/Users/ASUS/Desktop/R語言/報告/107080508113237.csv"
dengue2 <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE,
                     sep = ",")
View(dengue2)

file_path <- "C:/Users/ASUS/Desktop/R語言/報告/107081208183337.csv"
dengue3 <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE,
                      sep = ",")
View(dengue3)

file_path <- "C:/Users/ASUS/Desktop/R語言/報告/107081908253437.csv"
dengue4 <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE,
                      sep = ",")
View(dengue4)

file_path <- "C:/Users/ASUS/Desktop/R語言/報告/107082609013537.csv"
dengue5 <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE,
                      sep = ",")
View(dengue5)

dengue <- rbind( dengue1,dengue2,dengue3,dengue4, dengue5 ) 
View(dengue)

dengue$日期<-2018/7/30


dengue_1 <-dengue[order(as.Date(dengue$日期, format="%Y/%m/%d")),]

dengue_2 <-dengue_1[-1:-79,]
dengue_3 <-dengue_2[-806:-819,]


names(dengue_3)[18] <- "緯度"
names(dengue_3)[19] <- "經度"
dengue_3


str(dengue_3)
summary(dengue_3)


install.packages("ggmap")
install.packages("mapproj")
library(ggplot2, lib.loc="~/R/win-library/3.5")
library(ggmap, lib.loc="~/R/win-library/3.5")
library(mapproj, lib.loc="~/R/win-library/3.5")

map_area = c(left=120,right=122,bottom=21.5,top=25.5)


map <- get_map(location = map_area, zoom = 7,
               language = "zh-TW", maptype = "roadmap")
ggmap(map, darken = c(0.5, "white")) +
geom_point(aes(x = 經度, y = 緯度),
             color = "red", data = dengue_3)

map <- get_map(location = map_area, zoom = 8,
               language = "zh-TW", maptype = "roadmap")
ggmap(map, darken = c(0.5, "white")) +
  geom_point(aes(x = 經度, y = 緯度),
             color = "red", data = dengue_3) +
  geom_rect(aes(xmin = 120, xmax = 120.6, ymin = 22.8, ymax = 23.5),
            alpha = 0.1)



--------------------------------------------
  
filter.idx1 <- dengue_3$緯度 > 22.8 & dengue_3$緯度 < 23.5
filter.idx2 <- dengue_3$經度 > 120 & dengue_3$經度 < 120.6
dengue_3.tn <- dengue_3[filter.idx1 & filter.idx2, ]


map <- get_map(location = c(lon = 120.246100, lat = 23.121198),
               zoom = 10, language = "zh-TW")
ggmap(map, darken = c(0.5, "white")) +
  geom_point(aes(x = 經度, y = 緯度),
             color = "red", data = dengue_3.tn)
-------------------------------------------------
levels(dengue.tn$區別)
dengue_3[dengue_3.tn$區別 == "北　區", ]$區別 <- "北區"
dengue_3[dengue_3.tn$區別 == "東　區", ]$區別 <- "東區"
dengue_3[dengue_3.tn$區別 == "南　區" | dengue.tn$區別 == "南    區", ]$區別 <- "南區"
dengue_3[dengue_3.tn$區別 == "永康區 ", ]$區別 <- "永康區"

dengue_3.tn$區別 <- factor(dengue_3.tn$區別)
levels(dengue_3.tn$區別)
hist(as.Date(dengue_3.tn$日期), breaks = "weeks",
     freq = TRUE, main = "登革熱每週病例數", xlab = "日期",
     ylab = "病例數", format = "%m/%d")

---------------------------------------------------

t.test(dengue_3$調查容器戶內,dengue_3$陽性容器戶內 ,var.equal = TRUE)

names(dengue_3)[3] <- "area"
names(dengue_3)[11] <- "Positiveindoors"

require(reshape2)
dengue_3Anova <- aov(dengue_3$Positiveindoors~as.character(dengue_3$area), dengue_3)
View(dengue_3Anova)


install.packages("UsingR")
head(dengue_3)
ggplot(dengue_3, aes(x=Positiveindoors, y=調查容器戶內)) + geom_point() +
geom_smooth(method="lm") + labs(x="Positive container indoors", y="Investigation container indoors")


# ---------------------------------------------------------- #


dengue_3reg <- lm(formula=dengue_3$Positiveindoors ~ dengue_3$調查容器戶內 ,data=dengue_3)
dengue_3reg


