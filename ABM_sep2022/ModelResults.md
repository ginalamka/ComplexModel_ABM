### Eco-evolutionary models of population decline: how does migration effect long-term population persistence when extirpation risk is high?
**Gina Lamka and Dr. Janna Willoughby**

When the parameter set includes 1000 individuals in the starting population, a population
growth rate of 0.1, and a migration rate of 1-5 individuals per generation, the total population
size drops quickly for the first nine years, and stabilizes around ~160(+/- 30) individuals around and after year 10. This
drop in the population size is an artifact of the relationship of the death function and max age in the the simulation, 
and is cause for a removal of the first 10+ generations for stabilization.
The number of adults in the population is fewer than the number of total individuals, as expected.
Similarly, the proportion of the population that are migrants increases with time, stabilizing around 
20% for 100 years. The proportion of the population with migrant ancestry would be higher, though, and will 
require more investigation in future analyses. Statistical tests will be necessary to evaluate if
the ratio of expected to observed heterozygosity is significantly different than one, visualized with 
observed heterozygosity having a slightly smaller value than expected heterozygosity. The observed heterozygosity 
is highly variable between runs and replicates, a range of approximately 0.2-0.4(+/-0.1 in both directions) after 100 years. 
Further analyses would be required to determine if there is a relationship between the proportion of migrants 
in the population and observed heterozygosity. These results, in addition to the sensitivity analysis below, suggest 
that adding migrants into the population is necessary to maintain a population with these conditions, though the genetic benefits (in 
terms of heterozygosity) are highly variable. Next steps to further analyze these data include statistically evaluating results, 
adding additional genetic determinates of fitness and population persistence, and increasing the number of years and parameters. 


**Question 1:** *Does population size and stability change with varying population growth rates, when mortality rate is constant?*

![Change in population size with varying growth rates](https://user-images.githubusercontent.com/72942258/144495030-38c9da6d-0768-48db-af2c-86a10d714608.png)

These results suggest that the growth rate does have an effect on the population size and stability, with the population size at equilibrium greater as the population growth rate increases. 

**Question 2:** *Does population size and stability change with varying migration rates?*
![Change in population size with changing migration rates](https://user-images.githubusercontent.com/72942258/144495026-21685a2c-227e-4a24-944e-a72d391748cd.png)

In the absence of migration, a population with K = 1000 would crash after approximately 40 years. When gene flow occurs - in the form of migration from a source population - the population can persist for at least 100 generations. Higher migration rates result in higher total population sizes. 

**Question 3:** *Does population-wide heterozygosity change with varying carrying capacity?*
![Change in Ho with varying carrying capacities](https://user-images.githubusercontent.com/72942258/144495027-cdd4ffd3-669d-43ba-a715-d7a39baf9e80.png)

There does not seem to be a significant difference in observed heterozygosity with varying carrying capacity, though statistical tests would be required to confirm these results. 

**Question 4:** *Does population-wide observed heterozygosity change with varying population growth rates?*
![Change in Ho with varying growth rates](https://user-images.githubusercontent.com/72942258/144492866-c59c1613-90a4-40f3-a388-c40fd567f40b.png)

The greatest difference in observed heterozygosity is between simulations with the highest and lowest growth rates. Somewhat surprisingly, populations with a high growth rate have decreased heterozygosity compared to those with a low growth rate. 

**Question 5:** *Does population-wide heterozygosity change with varying migration rates?*
![Change in Ho with varying migration rates](https://user-images.githubusercontent.com/72942258/144495029-524a4091-902c-45a2-bf78-089e20c8b8e5.png)

Statistical tests will need to be conducted to determine if a migration rate of 1-5 individuals is significantly different than a migration rate of 5-10 individuals per generation. In the absence of migration, the population crashes after approximately 40 generations, and the observed heterozygosity is highly variable. 


### ***Final results suggest that migration rate, carrying capacity, and population growth rate contribute to population persistence and population-wide genetic diversity.***
