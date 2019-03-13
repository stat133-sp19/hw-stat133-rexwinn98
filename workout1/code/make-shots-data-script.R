title: Shots Data
description: Making the Shots data csv
inputs: golden stat warriors csvs 
output: a csv file and summary text files 


```{r}
stephen.curry$shot_made_flag <- as.character(stephen.curry$shot_made_flag)
klay.thompson$shot_made_flag <- as.character(klay.thompson$shot_made_flag)
andre.iguodala$shot_made_flag <- as.character(andre.iguodala$shot_made_flag)
kevin.durant$shot_made_flag <- as.character(kevin.durant$shot_made_flag)
draymond.green$shot_made_flag <- as.character(draymond.green$shot_made_flag)

stephen.curry$action_type <- as.factor(stephen.curry$action_type)
klay.thompson$action_type <- as.factor(klay.thompson$action_type)
andre.iguodala$action_type <- as.factor(andre.iguodala$action_type)
kevin.durant$action_type <- as.factor(kevin.durant$action_type)
draymond.green$action_type <- as.factor(draymond.green$action_type)

stephen.curry$shot_type <- as.factor(stephen.curry$shot_type)
klay.thompson$shot_type <- as.factor(klay.thompson$shot_type)
andre.iguodala$shot_type <- as.factor(andre.iguodala$shot_type)
kevin.durant$shot_type <- as.factor(kevin.durant$shot_type)
draymond.green$shot_type <- as.factor(draymond.green$shot_type)
```

```{r}
klay.thompson$name <- "Klay Thompson"
andre.iguodala$name <- "Andre Iguodala"
draymond.green$name <- "Draymond Green"
kevin.durant$name <- "Kevin Durant"
stephen.curry$name <- "Stephen Curry"
```

```{r}
klay <- klay.thompson
klay[klay$shot_made_flag == 'n', 7] <- 'shot_no'
klay[klay$shot_made_flag == 'y', 7] <- "shot_yes"
green <- draymond.green
green[green$shot_made_flag == 'y', 7] <- "shot_yes"
green[green$shot_made_flag == 'n', 7] <- "shot_no"
iguodala <- andre.iguodala
iguodala[iguodala$shot_made_flag == 'y', 7] <- "shot_yes"
iguodala[iguodala$shot_made_flag == 'n', 7] <- "shot_no"
durant <- kevin.durant
durant[durant$shot_made_flag == 'y', 7] <- "shot_yes"
durant[durant$shot_made_flag == 'n', 7] <- "shot_no"
curry <- stephen.curry
curry[curry$shot_made_flag == 'y', 7] <- "shot_yes"
curry[curry$shot_made_flag == 'n', 7] <- "shot_no"

```

```{r}
klay$minute <- (klay$period - 1) * 12 + (12 -  klay$minutes_remaining)
green$minute <- (green$period - 1) * 12 + (12 -  green$minutes_remaining)
iguodala$minute <- (iguodala$period - 1) * 12 + (12 - iguodala$minutes_remaining)
durant$minute <- (durant$period - 1) * 12 + (12 -  durant$minutes_remaining)
curry$minute <- (curry$period -1) * 12 + (12 -  curry$minutes_remaining)
```

```{r}
sink(file = "andre-iguodala-summary.txt")
summary(iguodala)
sink()
```
```{r}
sink(file = "kevin-druant-summary.txt")
summary(durant)
sink()
```
```{r}
sink(file = "stephen-curry-summary.txt")
summary(curry)
sink()
```
```{r}
sink(file = "klay.thompson.txt")
summary(klay)
sink()
```
```{r}
sink(file = "draymon-green.txt")
summary(green)
sink()
```
```{r}
help(rbind)
shots_data <- rbind(curry,green,durant,klay,iguodala)
```


```{r}
write.csv(shots_data, file = 'shots-data.csv')
```