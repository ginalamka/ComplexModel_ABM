### Eco-evolutionary models of population decline: how does migration effect long-term population persistence when extirpation risk is high?
**Gina Lamka and Dr. Janna Willoughby**

*Code accessed here is used for the complex model assignment in ABM class Fall 2021 at Auburn University*

Model description last updated: 12/2/2021

Keywords:
 - eco-evolutionary feedbacks
 - extinction vortex
 - population stability
 - migration
 - IUCN 

**Model Objectives**
1. Examine how the migration rate alters the long-term population outcome when migration is initiated at various population sizes and trajectories.
2. Determine the conditions at which consistent migration prevents population crashes, despite reductions in carrying capacity.
3. *Next Step: Assess if migration patterns better predict population crashes relative to other measures used to rank conservation concern (e.g., number of adults, IUCN).*

*These aims will support future considerations of promoting migration (via corridors or translocations) when managing species on the brink of extinction.*

**Model Overview**
We will simulate sexually reproducing diploid individuals (*i.e.*, males and females) with discrete time intervals and generation times. We will simulate a population that randomly mates, produces offspring, and is subject to mortality. Within this system, we will simulate varying levels of migration from a source population to the focal population when the focal population exists at varying population growth rates and under conditions representing habitat destruction (*i.e.*, reduced carrying capacity). We will output estimates of heterozygosity, adult and total population sizes, and proportion of migrants in the population at the population level and use these estimates to make inferences about the effects of migration on the focal population under these conditions. We will allow our model to persist for 100 years to understand how migration influences the population long-term and not just in the short-term period of potential population recovery.

**Initializing Model Design**
Populations will start at mean population size (K), the carrying capacity, with overlapping generations to simulate natural populations. Alleles (0 and 1) will be randomly assigned to each individual with equal probability for all loci. Loci will be simulated for SNP (n = 10) molecular markers, as the results provide conclusions from coding genetic data. Individuals will also be randomly assigned either an X or Y at the sex chromosome to designate the individual as male or female. Allele frequencies and sex determination will stay constant for the remainder of the individual’s lifetime. The maximum age of individuals (9 years), brood size (0-2), K (1000), and the age at maturity (1 year) will all be parameters fed to the simulation. A second population will be simulated and termed the “source” population, so that all migrants that move into the focal population will be simulated with the same parameters as the population of interest (see below for details of migration between populations).

**Model Details**
Individual agents will be simulated throughout their lifetime to determine the population-level effects of migrants into populations with varying sizes and trends (increasing, decreasing, or stable). After initializing the population, the main model will proceed through different forces that impact population characteristics. At the start of the year, individuals will be aged so that the age of every individual in the population will increase by one year. Following aging the population, we will remove individuals such that each individual has a probability of being removed from the population of 1/(maximum lifespan X 2). Migration will occur so that migrant(s) (0-1% of carrying capacity per generation, selected randomly from the source population) will enter the population and will alter the population-wide allele frequency. Individuals in the focal population will choose mates randomly so that one male and one female will mate per pair per generation, with reassignment to new mates in subsequent mating events. To determine the total number of offspring to create, we will estimate population size in the following year using the logistic growth equation
				N_(t+1)=(1+r(1-N_t/K))* N_t				(1)
where population size (Nt+1) was determined by the per capita growth rate (r = 0.1), carrying capacity (K), and population size prior to reproduction (Nt). For each year in the model, we will further modify this value using an estimate of density independent effects (e.g. environmental variables, etc.) using a random deviate from a normal distribution with a mean equal to Nt+1 and standard deviation equal to simulation parameter l, where l dictates the strength of density-independent processes (initially l = 5). Following mating, the offspring generated will then be assigned alleles assuming mendelian inheritance. All unique parameter combinations will be replicated 5 times, and the model will be run for a total of 100 years.

**Model Calibration with Empirical Data**
Further analysis and calibration of this simulation may exploit empirical data. For the simple model outlined here, however, empirical data will not be used.

**Expected Results**
The effects of migration on the population will be estimated from the output of the simulation. Fitness, here defined as population wide heterozygosity, will be estimated each generation. We expect that increased migration should increase fitness, increasing population size and reducing population decline. However, the extent to which this causes long-term population evolution is unclear.  

We expect that the results of migration on the population – observed and expected heterozygosity and total and adult population size – will not necessarily change independently, but in tandem. For example, if the K of the population is at 1000 individuals, and the next generation has an increase in population trajectory for the next generations, fewer migrants will be required to maintain the stability and genetic diversity. Oppositely, if the population of size 1000 is then under decline for the subsequent population, then the population size will then decrease and the number of migrants that introduce beneficial alleles will need to be at a higher number than if the populations were increasing to maintain high genetic diversity. In that same way, if one migrant is introduced into the population compared to five migrants in that same generation, the focal population with fewer migrants will have lower heterozygosity and less population stability. 

We will also evaluate our model for insight into using migration as a predictive metric of population extirpation. We will specifically compare the predictive utility of migration rate, likely in conjunction with population size or trajectory, to the predictive value of the number of adults in the population in understanding long-term population outcomes (*i.e.*, long term population size and trajectory). We hope these analyses will add utility to on-going efforts to identify conservation need in wild populations.

**Potential Challenges**
Our model gives equal probability of selecting migrants from the source to the focal population. However, literature suggests that migrants typically have lower fitness than the population (Buchan et al. 2019), perhaps to avoid unfavorable conditions in the source population. That is not always the case, however, as individuals that are bold are more likely to travel to new territories (Chapman et al. 2011), and boldness can lead to higher reproductive success (and therefore, fitness; Reale et al. 2009; Gasparini et al. 2019). To combat this issue, future analysis will examine these trends in our data by varying the probability of migration based on fitness of the individual (in terms of genetic diversity), and we will use sensitivity tests to determine if the effects on population genetic diversity and sustainability are negligible. 

**Note**: Use of the provided code requires a specific configuration of folders. Within the directory holding all of the provided scripts, there should be two folders: source  and output. All .R files that execute the simulation, except for Cover.R, are found in source into the 'source' directory. After defining parameter values within Cover.R and running the code, all output files (plots and tables) will be directed into the 'output' directory. Plots depicting sensitivity analyses are within the "Sensitivity Analysis" folder in "output".
