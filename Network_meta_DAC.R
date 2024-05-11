pacman::p_load(tidyverse,netmeta,meta,metafor, dmetar,DataEditR)
options(digits = 2)

# Mortality analysis

df <- read.csv2(file.choose())

p1 <- pairwise(data = df, treat = Grupo, event = Eventos, n = n, studlab = Estudo,
               sm = "RR")

nb <- netmeta(p1, reference.group = "Control", details.chkmultiarm = TRUE)

forest(nb, smlab = paste("Different exercise intensities \n","for mortality in CAD \n"),
       label.left = "Favours to Exercise",
       label.right = "Favours to Control", drop.reference.group = TRUE)

netgraph(nb, seq = "optimal", number.of.studies = TRUE, plastic = FALSE, cex.points = 3,
         col.number.of.studies = "white",
         col.multiarm = "white",
         col = "gray",
         col.points = "#454545",
         thickness = 'equal',
         offset = ifelse(trts == "MICT", 0.07, 
                         ifelse(trts %in% c("Control", "HICT"), 0.065, 0.035)),
         labels = paste0(trts, "\n(n = ", n.trts, ")"), points.max = 8)

Rank <- rankogram(nb,small.values = "good")

plot(Rank)

netsplit(nb) |> forest()

# VO2 analysis

## Importing the data frame and feature engineering 

df2 <- read.csv(file.choose())
df2$X <- NULL

df2 <- df2 %>% 
  rename(Group = GRUPO)

## Conventional meta-analysis

m1 <- metacont(data = df2, subgroup = Group, mean.e = media_2, mean.c = media_1,
               sd.e = dp_2, sd.c = dp_1, n.e = n2, n.c = n1, studlab = paste(Estudo))

forest(m1, width = 20, common = FALSE, common.subgroup = FALSE, layout = "Revman", xlim = c(-10,10), digits = 1,
       digits.sd = 1, digits.mean = 1, drop.reference.group = TRUE)

## Pairwise meta-analysis

p2 <- pairwise(data = df2, mean = mean, sd = sd, n = n, treat = Group,
               studlab = Estudo, sm = "MD", reference.group = "UC")

## Network meta-analysis

net2 <- netmeta(p2, reference.group = "UC", sep.trts = " vs. ", fixed = FALSE, random = TRUE,
                studlab = "Estudo", details.chkmultiarm = TRUE)

forest(net2, smlab = paste("Different exercise intensities \n","for Peak VO2 in CAD \n"),label.left = "Favours to Control",
       label.right = "Favours to Exercise", drop.reference.group = TRUE)

## Network graph

netgraph(net2, seq = "optimal", number.of.studies = TRUE, plastic = FALSE, cex.points = n.trts,
         col.number.of.studies = "white",
         col.multiarm = "white",
         col = "gray",
         col.points = "#454545",
         thickness = 'equal',
         offset = ifelse(trts == "UC", 0.07, 
                         ifelse(trts %in% c("Control", "HICT", "MICT", "HIIT"), 0.065, 0.035)),
         labels = paste0(trts, "\n(n = ", n.trts, ")"))

# Rankogram

rank2 <- netrank(net2, small.values = "bad")

plot(rank2)

## Funnel Plot

funnel(net2, order = c("HICT", "HIIT", "LICT", "UC","MICT"), 
       linreg = TRUE,
       col = c("red", "black", "gray", "coral",
               "aquamarine", "orange", "brown"))

## Assessment of direct and indirect estimates.

netsplit(net2) |> forest()

# Quality of life analysis

## Importing the data frame

df3 <- readxl::read_excel(file.choose())

## Feature engineering

df3$dp1 <- as.numeric(df3$dp1)
df3$dp2 <- as.numeric(df3$dp2)
df3$dp <- as.numeric(df3$dp)

df3 <- df3 |> 
  drop_na()

## Meta-analysis

m3 <- metacont(data = df3, subgroup = Grupo, mean.e = m2, mean.c = m1,
         sd.e = dp2, sd.c = dp1, n.e = n2, n.c = n1, studlab = paste(Estudo))

forest(m3, width = 20, common = FALSE, common.subgroup = FALSE, layout = "Revman", digits = 1,
       digits.sd = 1, digits.mean = 1, drop.reference.group = TRUE)

## Pairwise meta-analysis

p3 <- pairwise(data = df3, mean = mean, sd = dp, n = n, treat = Grupo,
         studlab = Estudo, sm = "SMD", reference.group = "UC")

## Network Meta-analysis,

net3 <- netmeta(p3, reference.group = "UC", sep.trts = " vs. ", fixed = FALSE, random = TRUE,
                studlab = "Estudo", details.chkmultiarm = TRUE)
### Forest plot
forest(net3, smlab = paste("Different exercise intensities \n","for Quality of Life in CAD \n"),label.left = "Favours to Control",
       label.right = "Favours to Exercise", drop.reference.group = TRUE)

## Network graph

netgraph(net3, seq = "optimal", number.of.studies = TRUE, plastic = FALSE, cex.points = n.trts, col.points = "grey",
         offset = ifelse(trts == "UC", 0.07, 
                         ifelse(trts %in% c("Control", "HICT", "MICT", "HIIT"), 0.065, 0.035)),
         labels = paste0(trts, "\n(n = ", n.trts, ")"))

## Assessment of direct and indirect estimates.

netsplit(net3) |> forest()

## Rankogram

rank3 <- netrank(net3, small.values = "bad")

plot(rank3)



