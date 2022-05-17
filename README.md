# CEFTA

This project is the code for my calculations regarding my master thesis for the economics program at the University of Tübingen
for the Chair in International Economics and European Integration 
The code is a combination of my own work and in large, that of one of my supervisors, Dr. Oliver Krebs, without whom this would not have been possible. 

This was the final submission, so dear reader please do note that some of the comments are a continuing conversation between Dr. Krebs and I,
and are not always intended to be descriptive or even articulate about the particular code.

The title of the thesis is "A Quantitative Spatial Analysis of the Western Balkans’ Accession to the EU"
My thesis explores the Central European Free Trade Agreement between the EU and the West Balkans in 2006 and onward. It analyzes
economic effects of free trade agreements between 170 countries over a 25 year time span. Using an iterative process, I simulate the
West Balkans joining the EU single market and analyze labor movements, as well as general economic outcomes, such as wage and price changes, and more importantly changes in trade flows between countries and their individual sectors.

1_WIOTprep

This file focuses on the data preparation. The data is taken from the EORA26 database (https://worldmrio.com/eora26), for, at that time, all available years (1990 - 2015)
The dataframe holds bilateral trade data for 190 countries, aggregated to a common 26-sector classification (i.e 190*26*26*190 datapoints).
Some of the limitations of the used model in later parts, is the inabiltity to handle negative trade flows(more precisely negative value added) between countries, hence the partial exclusion of countries
with nonsensical trade data (possible reasons are economic sanctions, as is the case for Iran). The exclusion of these countries has to be recitfied and adjusted in the dataframe, as trade flows are one of the main points of this thesis.

2_PPML_estimates

Continuation of data preparation, this time I use available data from Mario Larch's Regional Trade Agreements Database.
The data contains multilateral and bilateral regional trade agreements for the last 70 years. This data is vital for the later comparisons
of the effectiveness of CEFTA and the EU Accession of countries.
Halfway through the document we are finally able to actually estimate the effects of the trade agreements with the help of a 
pseudo poisson maximum likehood estimation. With the coefficient for the trade agreements in hand, one can apply these to the prepped wiot sets from part 1.

3_Parameter_calibration

Considering that the model also includes labor migration, we need labor and population data. This dataset is taken from ILO.
As is usual, data needs to be prepped further for smooth integration.
More importantly however is that the parameters need to be calibrated for a baseline scenario without trade deficits (limitations of the model).
Elasticities asre taken from Aichele and Heiland 2016, as well as Egger, and Krebs and Pflüger 2019.
Parameters are then calibrated through an iterative approach, where the iteration continues until the variables for changes in wage and population reach zero (equilibrium) or 10 000 iterations are done, whichever comes first. The calibration runs for a long time (up to 2 - 3 days).
With the calibration of the parameters we are then finally able to run the actual simulation.

4_simulationCEFTA

With all variables in hand we are able to finally run the simulation for the effects of CEFTA, to be more precise a post-ex analysis of the remifications, if
the West Balkans would not have joined the CEFTA agreement. The model for the simulation is based on the gravity model in line with Caliendo and Parro 2015.
The model allows for movement of labor, which are caused by the changes in wages and prices in affected countries. For the case of the CEFTA simulation, we
only allow for EU population to move within the EU, while popuilation changes are static in the West Balkan countries.

5_simulationEU

The code for this file is close to being the same as for the CEFTA simulation, here however we simulate if the West Balkan countries were to join the EU, and hence are allowed to have population movement freely within countries in the two trade areas.

cefta2006estimation

This file was a test to see if we were able to get significant results by testing the effects of the entry of the West Balkans to the Central European Trade Agreement in 2006. The difference here to compared to 4_simulationCEFTA is that the later tests the scenario if the West Balkans DIDN'T join CEFTA. The results were unfortunately insignificant, and hence excluded from further analysis. Potential reasons for this were the homogeneity of the region with the central eastern european countries, lack of actual implementation of CEFTA policies, as well as unstable political relations between West Balkan countries that impede trade.

graphresultssim

This file is simply the code for the visualisation of the results.
