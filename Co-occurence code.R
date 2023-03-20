# The code was written under R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# The code was written for my thesis regarding the invertebrate community
# in the Jordan Stream tributaries
getwd() #First' find if we are in the right directory
setwd("C:/Trapich/data science") #Change if not
library(cooccur)
library(vegan)
library(dplyr)

### We move to pairwise approach - calculate for each pair of species if they:
#1. Positively co-occur - maybe some kind of facilitation is taking place
#2. Negatively co-occur - maybe competition
#3. Randomly co-occur - no interaction

# For the pairwise approach, use package "cooccur"
data <- read.csv(file = "Ashalim_species.csv", header = TRUE)
data[data>0] <- 1 #Turn the data from quantities of invertebrates to presence/absence (0/1)
data_t = data.frame(t(data)) #Transpose the data - now species are in rows and sites are in columns
cooccurResults <- cooccur(mat=data_t,type="spp_site",thresh=TRUE,spp_names=TRUE)
# Want only the effect size? add only_effects = TRUE
# If only_effects = TRUE, standardize with eff_standard = TRUE
# If only_effects = TRUE, make a distance matrix with eff_matrix = TRUE
# It is a good, intuitive way to show the occurrence of every two species.

class(cooccurResults)
cooccurSES = effect.sizes(cooccurResults) #Standardize the effect size
plot(cooccurResults)  #plot a heat map of the results - hot color is positive
# occurence and dark colors are negative occurence between two species 

summary(cooccurSES)
prob.table(cooccurResults) #You can get the exact math probabily for two species
# to be sharing the habitat
pair.profile(cooccurResults) #See how much does the species contribute to 
# patterns of co-occurence
obs.v.exp(cooccurResults) #This creates a correlation graph with the 
#species that had interactions, observed vs expected
