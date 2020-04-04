# HarvardX-Capstone-Movielens

The recommendation systems offered by companies such as Amazon utilize previously rated, browsed, or purchased items to 
recommend items of interest to potential buyers. In this project, our aim is to develop a machine learning algorithm to 
predict movie ratings, and use it to recommend movies to users. The Movielens dataset used in this project contains 
10000054 movie ratings applied to 10677 movies by 69878 users and grouped into 797 unique genres. The dataset is collected 
by Harper and Konstan (2015) and is made available for public download through the GroupLens Research Group at the University 
of Minnesota. 

For this project, the data were first inspected in order to understand the pattern and data structure. Several plots have 
been created in ordere to visualize the effect of movies, users, movie age, and genres on average ratings. The edx dataset 
were then splitted into training and test sets and several different algorithms were tested to find the movie predictions 
with lowest Root Mean Square Error (RMSE). The final model were then used to calculate the RMSE on validation datasets. 
The RMSE is a measure of the differences between values predicted by a model and the values observed (i.e., model accuracy).
