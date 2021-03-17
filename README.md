# Bayesian Space-Time Models for Expected Possession added Value

## 2021 Big Data Cup Submission
## Brendan Kumagai, Mikael Nahabedian, Thibaud Chatel, Tyrel Stokes

### Final Paper

Contains a pdf with our submission for the 2021 Big Data Cup.


### Code

Our code for this project is broken down into the following sub-folders:

0. **Functions**: Contains plotting functions used to create INLA spatial maps and rink plots as well as sampling functions used in our simulations.

1. **Data Preparation**: All data preparation required to extract entry-to-exit sequences and reformat data to fit the Markov decision process as described in our paper.

2. **Action-Transition Models**: The spatiotemporal poisson and logistic models that were used to estimate the transition probabilities of our Markov decision process.

3. **Movement and Time Models**: Additional models required to make our simulation run smoothly such as predicting whether or not there is traffic on a shot, predicting the subsequent xy-location following an event, predicting whether an entry will be controlled or dumped, etc.

4. **Value Models**: The expected goals models (with and without pre-shot passes).

5. **Simulations**: Contains the generation of sample parameters to be used in our simulations and the simulation chain itself used to get expected possession value samples from our model.



### Data

Contains the outputs of our "Code/1. Data Preparation/Data Preparation.Rmd" file. Most notably, "otters_data_for_modelling.csv", the data set used for all of our model fitting. Additionally, we include fitted values of expected goals based off of our models in this folder.



### Figures

The figures folder contains pngs of various spatial maps from our core and glue transition models.


### Models

Due to file size constraints, we could not include any of our models in this repository. However, the code for our models is available in "Code/2. Action-Transition Models", "Code/3. Movement and Time Models" and "Code/4. Value Models".