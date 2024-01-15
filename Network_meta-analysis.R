# Loading required packages

pacman::p_load(netmeta,tidyverse, gtsummary, dmetar)

# Defining settings

settings.meta(digits = 2, digits.se = 3)

cilayout("(", "-")

# Conducting a pairwise meta-analysis

dat <- data("Dogliotti2014") # available dataset

# Pairwise meta-analysis

pw <- pairwise(treat = treatment, n = total, event = stroke, 
               studlab = study, data = Dogliotti2014, sm = "OR")


# Conducting a network meta-analysis

net <- netmeta(pw, reference.group = "plac")

netgraph(net, seq = "optimal", number.of.studies = TRUE, plastic = FALSE,
         cex.points = n.trts, offset = ifelse(trts == "VKAs", 0.07, 
                                              ifelse(trts %in% c("Aspirin", "Apixaban"), 0.045, 0.035)),
         labels = paste0(trts, "\n(n = ", n.trts, ")"))

# Conducting a Mantel-Haenszel

net2 <- netmetabin(pw, reference.group = "plac")

# Merging the meta-analysis (Inverse Variance vs Mantel-Haenszel)

netb <- netbind(net, net2, random = FALSE, name = c("Inverse Variance", "Mantel-Haenszel"))

forest(net)

# Forest plot with extra annotations

forest(net,
       smlab = paste("Antithrombotic treatments \n",
                     "(non-valvular atrial fibrillation)"),
       label.left = "Favours to Treatment",
       label.right = "Favours to Placebo")

# Forest plot with merged analysis (Inverse Variance and Mantel-Haenzel) to assess model variance in pooled results.

forest(netb)

# Creating the "Rankogram" for each intervention assessed and their respectives probabilities.

rank <- rankogram(net, nsim = 100)

print(rank)

# Creating a graphical visualization ranking of interventions

rank <- rankogram(net)

plot(rank)

# Another option of "Rankogram" graphical visualization

p1 <- netrank(rank)

plot(p1)

# Creating a plot of direct/indirect evidence (network estimates)

direct.evidence.plot(net)

# Forest plot with estimates of direct and indirect evidence of each intervention

netsplit(net) %>% forest()
