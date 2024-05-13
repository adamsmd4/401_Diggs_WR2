## Tank Dell Cleansed Data
dell_df = read.csv("C:/Users/miles/OneDrive/Desktop/401 Project/Tank_Dell.csv", encoding = 'UTF-8')
dell_df |> 
  dplyr::slice(-c(6, 12:17)) |>
  dplyr::mutate(dplyr::across(c(11, 16), function(x) as.numeric(stringr::str_remove(x, "%")) / 100)) |>
  dplyr::mutate(dplyr::across(Receiving.Yds:Off..Snaps.Pct, as.numeric)) -> dell_df

## Stefon Diggs Cleansed Data
diggs_df = read.csv("C:/Users/miles/OneDrive/Desktop/401 Project/Stefon_Diggs.csv", encoding = 'UTF-8')
diggs_df |>
  dplyr::mutate(dplyr::across(c(11, 16), function(x) as.numeric(stringr::str_remove(x, "%")) / 1)) |>
  dplyr::mutate(dplyr::across(Receiving.Yds:Off..Snaps.Pct, as.numeric)) -> diggs_df

## Make new df for only numerics
dell_stats <- dell_df |> 
  dplyr::select(-c(1)) |>
  dplyr::select(-c(2:5))
diggs_stats <- diggs_df |> 
  dplyr::select(-c(1)) |>
  dplyr::select(-c(2:5))
diggs_stats23 <- diggs_stats |> 
  dplyr::slice(-c(1:15))

##Make CSV's for these
write.csv(dell_stats, file = "dell_stats.csv", row.names = TRUE)
write.csv(diggs_stats23, file = "diggs_stats.csv", row.names = TRUE)

##Prove that Stefon Diggs is on the decline with scraping
library('rvest')
url = "https://www.pro-football-reference.com/players/D/DiggSt00.htm"
url |>
  rvest::read_html() |>
  rvest::html_elements("[id^='receiving_and_rushing.'] > td:nth-child(10)") |>
  rvest::html_text(trim = TRUE) -> new_df
new_df = as.numeric(new_df)
diggs_yds= new_df[1:9]

url |>
  rvest::read_html() |>
  rvest::html_elements("[id^='receiving_and_rushing.'] > td:nth-child(2)") |>
  rvest::html_text(trim = TRUE) -> new_df2
diggs_age= new_df2[1:9]

combined_data <- c(diggs_age, diggs_yds)
diggs_decline_df <- tibble::tibble(value = combined_data)
readr::write_csv(diggs_decline_df, "diggs_decline_df2.csv")

##Compare Tank vs Stef 23
plot(diggs_stats23$Week, diggs_stats23$Receiving.Yds, type = "l")
plot(dell_stats$Week, dell_stats$Receiving.Yds, type = "l")

mean(diggs_stats23$Receiving.Yds)
mean(dell_stats$Receiving.Yds)

##For other "Top WR rooms" WR1 has to perform at a high level
brown_df = read.csv("C:/Users/miles/OneDrive/Desktop/401 Project/AJ_Brown_2023.csv", encoding = 'UTF-8')
brown_df |>
  dplyr::mutate(dplyr::across(c(11, 15), function(x) as.numeric(stringr::str_remove(x, "%")) / 1)) |>
  dplyr::mutate(dplyr::across(Receiving.Yds:Off..Snaps.Pct, as.numeric)) -> brown_df
brown_stats <- brown_df |> 
  dplyr::select(-c(1)) |>
  dplyr::select(-c(2:5))

mean(brown_stats$Receiving.Yds)
write.csv(brown_stats, file = "brown_stats.csv")


## Look at Stefon compared to his 2023 WR Room
bills_df = read.csv("C:/Users/miles/OneDrive/Desktop/401 Project/Bills_wrs.csv")
bills_df

stef = jsonlite::fromJSON("https://www.thesportsdb.com/api/v1/json/3/searchplayers.php?p=Stefon_Diggs", flatten = TRUE)
stef
collins = jsonlite::fromJSON("https://www.thesportsdb.com/api/v1/json/3/searchplayers.php?p=Nico_Collins", flatten = TRUE)
collins
woods = jsonlite::fromJSON("https://www.thesportsdb.com/api/v1/json/3/searchplayers.php?p=Robert_Woods", flatten = TRUE)
woods
brown = jsonlite::fromJSON("https://www.thesportsdb.com/api/v1/json/3/searchplayers.php?p=Noah_Brown", flatten = TRUE)
brown

                              stefdf = as.data.frame(stef[1])
stefdfclean <- stefdf |> 
  dplyr::select(player.strPlayer, player.dateBorn, player.strHeight, player.strWeight)

collinsdf = as.data.frame(collins[1])
collinsdfclean <- collinsdf |> 
  dplyr::select(player.strPlayer, player.dateBorn, player.strHeight, player.strWeight)

woodsdf = as.data.frame(woods[1])
woodsdfclean <- woodsdf |> 
  dplyr::select(player.strPlayer, player.dateBorn, player.strHeight, player.strWeight)

browndf = as.data.frame(brown[1])
browndfclean <- browndf |> 
  dplyr::select(player.strPlayer, player.dateBorn, player.strHeight, player.strWeight)


fourplayers <- rbind(stefdfclean, woodsdfclean, collinsdfclean, browndfclean)

fourplayers$player.strHeight <- sub(" \\(.*\\)", "", fourplayers$player.strHeight)

fourplayers$player.strWeight <- sub(" \\(.*\\)", "", fourplayers$player.strWeight)


fourplayers$player.dateBorn <- as.Date(fourplayers$player.dateBorn)

library(lubridate)
# Calculate age using lubridate
fourplayers$Age <- interval(start = fourplayers$player.dateBorn, end = Sys.Date()) / years(1)
fourplayers$Age <- floor(fourplayers$Age)

fourplayers <- subset(fourplayers, select = -player.dateBorn)

colnames(fourplayers) <- c("Name", "Height", "Weight", "Age")
