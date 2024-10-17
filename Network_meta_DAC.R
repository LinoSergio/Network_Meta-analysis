# Loading required packages

pacman::p_load(tidyverse,netmeta,meta,metafor,rio,dmetar)
options(digits = 2)

# Mortality analysis

## Datasheet

df <- read.csv2(file.choose())

## Pairwise meta-analysis

p1 <- pairwise(data = df, treat = Grupo, event = Eventos, n = n, studlab = Estudo,
               sm = "RR")

## Netowork meta-analysis

nb <- netmeta(p1, reference.group = "Control", details.chkmultiarm = TRUE, 
              small.values = 'desirable')

summary(nb)

## Pairwise meta-analysis forest plot

forest(nb, smlab = paste("Different exercise intensities \n","for mortality in CAD \n"),
       label.left = "Favours to Exercise",
       label.right = paste("Favours to Control (No Exercise)"), drop.reference.group = TRUE)

## Network meta-analysis forest plot

netgraph(nb, seq = "optimal", number.of.studies = TRUE, plastic = FALSE, cex.points = 3,
         col.number.of.studies = "white",
         col.multiarm = "white",
         col = "gray",
         col.points = "#454545",
         thickness = 'equal',
         offset = ifelse(trts == "MICT", 0.07, 
                         ifelse(trts %in% c("Control", "HICT"), 0.065, 0.035)),
         labels = paste0(trts, "\n(n = ", n.trts, ")"), points.max = 8)

## Rankogram analysis (mortality outcome)

rank <- rankogram(nb, cumulative.rankprob = TRUE, small.values = 'good')

plot(rank)

print(Rank)

## Comparing and visualizing direct and indirect treatment effects

netsplit(nb) |> forest()

# Direct evidence plot

direct.evidence.plot(nb)

# VO2 analysis

## Importing the data frame and feature engineering 

df2 <- read.csv(file.choose())
df2$X <- NULL

df2 <- df2 |>  
  rename(Group = GRUPO)

## Conventional meta-analysis

df2 <- df2 |> 
  mutate(se = sd/sqrt(n))

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

# Forest plot sorted by SUCRA

forest(net2, sortvar = sucra, drop.reference.group = TRUE,
       label.right = 'Favours to Exercise', 
       label.left = 'Favours to Usual Care',
       smlab = 'Diferent in Exercise intensities \n by SUCRA')

## Network graph

netgraph(net2, seq = "optimal", number.of.studies = TRUE, plastic = FALSE, cex.points = 3,
         col.number.of.studies = "white",
         col.multiarm = "white",
         col = "gray",
         col.points = "black",
         thickness = 'equal',
         offset = ifelse(trts == "UC", 0.07, 
                         ifelse(trts %in% c("Control", "HICT", "MICT", "HIIT"), 0.065, 0.035)),
         labels = paste0(trts, "\n(n = ", n.trts, ")"))


# Rankogram

rg2 <- rankogram(net2, cumulative.rankprob = TRUE, small.values = 'bad')

plot(rg2)

rank2 <- netrank(net2, small.values = "bad")

plot(rank2)

netleague(net2, bracket = '(',
          digits = 2,
          path = 'leaguetable.xlsx')
## Funnel Plot## Fudf2nnel Plot

funnel(net2, order = c("HICT", "HIIT", "LICT", "UC","MICT"), 
       linreg = TRUE,
       col = c("red", "black", "gray", "coral",
               "aquamarine", "orange", "brown"))

## Assessment of direct and indirect estimates.

netsplit(net2) |> forest()


# Bayesian Met-analysis

rd <- data(TherapyFormatsGeMTC)


armData <- data.frame(study = df2$Estudo,
                      treatment = df2$Group,
                      mean = df2$mean,
                      st.e = df2$se)

network <- mtc.network(data.ab = armData)

summary(network)

plot(network, vertex.color = 'white',
     vertex.label.dist = 3,
     vertex.label.cex = 1.5,
     edge.curve = .2,
     vertex.shape = 'sphere')

# Quality of life analysis

## Importing the data frame

df3 <- read_csv2(file.choose())

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

netgraph(net3, seq = "optimal", number.of.studies = TRUE, plastic = FALSE, cex.points = 3, col.number.of.studies = "white",
         col.multiarm = "white",
         col = "gray",
         col.points = "black",
         thickness = 'equal',
         offset = ifelse(trts == "UC", 0.07, 
                         ifelse(trts %in% c("Control", "HICT", "MICT", "HIIT"), 0.065, 0.035)),
         labels = paste0(trts, "\n(n = ", n.trts, ")"))

## Assessment of direct and indirect estimates.

netsplit(net3) |> forest()

## Rankogram

rank3 <- rankogram(net3, cumulative.rankprob = TRUE, small.values = "bad")

plot(rank3)


# Isolated analysis for HIIT in coronary artery disease.

## VO2 analysis - High-intensity interval training

df4 <- df2 |> 
  filter(Group == "HIIT")

g1 <- metacont(data = df4, mean.e = media_2, mean.c = media_1,
               sd.e = dp_2, sd.c = dp_1, n.e = n2, n.c = n1, studlab = paste(Estudo))  

forest(g1, width = 20, common = FALSE, 
       layout = "Revman",
       digits = 1, xlim = c(-15,15),
       digits.sd = 1, digits.mean = 1,
       label.right = 'Favours to HIIT',
       label.left = 'Favours to No exercise')

## Quality of Life - High-intensity interval training 

df5 <- df3 |> 
  filter(Grupo == "HIIT") |> 
  rename(Group = Grupo)

g2 <- metacont(data = df5, mean.e = m2, mean.c = m1,
               sd.e = dp2, sd.c = dp1, n.e = n, n.c = n2,
               studlab = paste(Estudo),
               sm = "SMD")

forest(g2, width = 20, common = FALSE, layout = "Revman",
       digits = 1, xlim = c(-5,5),
       digits.sd = 1, digits.mean = 1,
       label.right = "Favours to HIIT",
       label.left = 'Favours to No exercise')

## Funnel Plot

funnel(g1)

funnel(g2)

