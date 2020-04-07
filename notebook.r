
# Loading the data
heart_disease = read.csv("datasets/heart_disease_patients.csv")

# Print the first ten rows of the data set
# .... YOUR CODE FOR TASK 1 ....
head(heart_disease,10)
# Check that only numeric variables
lapply(heart_disease, is.numeric)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

soln_heart_disease <- read.csv('datasets/heart_disease_patients.csv')

run_tests({
    test_that("heart disease data loaded correctly", {
    expect_equal(heart_disease, soln_heart_disease, 
                 info="heart_disease does not have right data, check the csv file name")
    expect_identical(lapply(heart_disease, class)[1][[1]], 'integer', 
                     info="variables not all type numeric")
    })
})

# Evidence that the data should be scaled?
summary(heart_disease)

# Remove id
heart_disease = heart_disease[ , !(names(heart_disease) %in% c(....))]

# Scaling data and saving as a data frame
scaled = scale(....)

# What does data look like now?
summary(....)

soln_heart_disease = soln_heart_disease[ , !(names(soln_heart_disease) %in% c("id"))]
soln_scaled = scale(soln_heart_disease)


run_tests({
    test_that("remove correct column", {
        expect_identical(colnames(soln_heart_disease), colnames(heart_disease), info = "Did you remove the id column?")
    })
    
    test_that("scaled data properly", {
        expect_identical(soln_scaled, scaled, info = "Did you scale the proper data set?")
    })
})

# Set the seed so that results are reproducible
seed_val = ....
set.seed(...., kind = "Mersenne-Twister", normal.kind = "Inversion")

# Select a number of clusters
k = ....

# Run the k-means algorithms
first_clust = kmeans(...., centers = ...., nstart = ....)

# How many patients are in each group?
first_clust$....

soln_seed_val = 10
set.seed(soln_seed_val, kind = "Mersenne-Twister", normal.kind = "Inversion")
soln_k = 5
soln_first_clust = kmeans(soln_scaled, centers = soln_k, nstart = 1)


run_tests({
    test_that("correct seed", {
        expect_equal(soln_seed_val, seed_val, info = "Is the seed set to 10?")
    })
    
    test_that("correct number of clusters", {
        expect_equal(soln_k, k, info = "Are you using five clusters?")
    })
    test_that("correct implmentation of algorithm", {
        expect_equal(soln_first_clust$size, first_clust$size, info = "What is your nstart value?")
    })
})

# Set the seed
seed_val = ....
set.seed(...., kind = "Mersenne-Twister", normal.kind = "Inversion")

# Run the k-means algorithms
k = ....
second_clust = ....

# How many patients are in each group?
second_clust$....

seed_val_2 = 38
set.seed(seed_val_2, kind = "Mersenne-Twister", normal.kind = "Inversion")
k_2 = 5
soln_second_clust = kmeans(soln_scaled, centers = k_2, nstart = 1)

run_tests({
    test_that("correct seed", {
        expect_equal(seed_val_2, seed_val, info = "Is the seed set to 10?")
    })
    
    test_that("correct number of clusters", {
        expect_equal(k_2, k, info = "Are you using five clusters?")
    })
    test_that("correct implmentation of algorithm", {
        expect_equal(soln_second_clust$size, second_clust$size, info = "What is your nstart value?")
    })
})

# Adding cluster assignments to the data
heart_disease[....] = ....
heart_disease[....] = ....

# Load ggplot2
library(....)

# Creating the plots of age and chol for the first clustering algorithm
plot_one = ggplot(heart_disease, aes(x =...., y = ...., color = as.factor(first_clust))) + 
  geom_point()
plot_one 

# Creating the plots of age and chol for the second clustering algorithm
plot_two = ggplot(heart_disease, aes(x = ...., y = ...., color = as.factor(....))) + 
  #.... YOUR CODE FOR TASK 5 ....
plot_two

soln_heart_disease['first_clust'] = soln_first_clust$cluster
soln_heart_disease['second_clust'] = soln_second_clust$cluster

# creating the correct graphs and getting fingerprints
soln_plot_one = ggplot(soln_heart_disease, aes(x=age, y=chol, color=as.factor(first_clust))) + geom_point()
soln_plot_two = ggplot(soln_heart_disease, aes(x=age, y=chol, color=as.factor(second_clust))) + geom_point()

run_tests({
    test_that("cluster assignments added", {
        expect_identical(soln_heart_disease, heart_disease, 
                         info = "Did you add a column for both the first and second iteration?")
    })
    
    test_that("ggplot2 loaded", {
        expect_true('ggplot2' %in% .packages(), 
                    info = "Did you load ggplot2?")
    })

    test_that("first plot is correct", {
        expect_equal(soln_plot_one$labels, plot_one$labels, 
                         info = "Do you have the correct variables on the axes and used to color code?")
    })
    
    test_that("second plot is correct", {
        expect_equal(soln_plot_two$labels, plot_two$labels, 
                         info = "Do you have the correct variables on the axes and used to color code?")
    })
})

# Executing hierarchical clustering with complete linkage
hier_clust_1 = hclust(dist(....), method= ....)

# Printing the dendrogram
plot(....)

# Getting cluster assignments based on number of selected clusters
hc_1_assign <- cutree(...., ....)

soln_hier_clust_1 = hclust(dist(soln_scaled), method='complete')
soln_hc_1_assign = cutree(soln_hier_clust_1, 5)
                          
run_tests({
    test_that("correctly implemented clustering algorithm", {
        expect_identical(soln_hier_clust_1$merge, hier_clust_1$merge, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_1$labels, hier_clust_1$labels, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_1$method, hier_clust_1$method, 
                         info = "Did you use complete linkage?")   


    })
    
    test_that("correct cutoff for cluster assignments", {
        expect_identical(soln_hc_1_assign, hc_1_assign, 
                         info = "Did you select five clusters?")
    })
})

# Executing hierarchical clustering with single linkage
hier_clust_2 = hclust(....)

# Printing the dendrogram
plot(....)

# Getting cluster assignments based on number of selected clusters
hc_2_assign <- ....

soln_hier_clust_2 = hclust(dist(soln_scaled), method='single')
soln_hc_2_assign = cutree(soln_hier_clust_2, 5)
                          
run_tests({
    test_that("correctly implemented clustering algorithm", {
        expect_identical(soln_hier_clust_2$merge, hier_clust_2$merge, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_2$labels, hier_clust_2$labels, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_2$method, hier_clust_2$method, 
                         info = "Did you use single linkage?")     })
    
    test_that("correct cutoff for cluster assignments", {
        expect_identical(soln_hc_2_assign, hc_2_assign, info = "Did you select five clusters?")
    })
})

# Adding assignments of chosen hierarchical linkage
heart_disease['hc_clust'] = ....

# Remove 'sex', 'first_clust', and 'second_clust' variables
hd_simple = heart_disease[, !(names(heart_disease) %in% c(....))]

# Getting mean and standard deviation summary statistics
clust_summary = do.call(data.frame, aggregate(. ~ hc_clust, data = ...., function(x) c(avg = ...., sd = ....)))
clust_summary

soln_heart_disease["hc_clust"] = soln_hc_1_assign

soln_hd_simple = soln_heart_disease[, !(names(soln_heart_disease) %in% c("sex", "first_clust", "second_clust"))]

soln_clust_summary = do.call(data.frame, aggregate(. ~hc_clust, data = soln_hd_simple, function(x) c(avg = mean(x), sd = sd(x))))


run_tests({
    test_that("selected first cluster assignments", {
        expect_identical(heart_disease['hc_clust'], soln_heart_disease['hc_clust'], 
                         info = "You chose the incorrect hierarchical clustering assignments.")
    })
    
    test_that("removed columns properly", {
        expect_identical(soln_hd_simple, hd_simple, 
                         info = "Did you remove three columns?")
    })
    test_that("proper summary analysis", {
        expect_identical(soln_clust_summary, clust_summary, 
                         info = "Did you find the mean and standard deviation using mean(x) and sd(x)?")
    })
})

# Plotting age and chol
plot_one = ggplot(...., aes(x = ...., y = ...., color = as.factor(hc_clust))) + 
  geom_point()
plot_one 

# Plotting oldpeak and trestbps
plot_two = ggplot(....) + 
  geom_point()
plot_two

soln_plot_one = ggplot(soln_heart_disease, aes(x=age, y=chol, color=as.factor(hc_clust))) + geom_point()
soln_plot_two = ggplot(soln_heart_disease, aes(x=oldpeak, y=trestbps, color=as.factor(hc_clust))) + geom_point()
run_tests({
    test_that("plot one is correct", {
        expect_identical(soln_plot_one$labels, plot_one$labels, 
                         info = "Check that you are using the correct variables for the first plot")
    })
    
    test_that("plot two is correct", {
        expect_identical(soln_plot_two$labels, plot_two$labels, 
                         info = "Check that you are using the correct variables for the second plot")
    })
})

# Add TRUE if the algorithm shows promise, add FALSE if it does not
explore_kmeans = ....
explore_hierarch_complete = ....
explore_hierarch_single = ....

soln_1 = FALSE
soln_2 = TRUE
soln_3 = FALSE

run_tests({
    test_that("correct kmeans results", {
        expect_identical(soln_1, explore_kmeans, info = "Are the clusters stable between kmeans iterations?")
    })
    
    test_that("correct hierarchical with complete linkage results", {
        expect_identical(soln_2, explore_hierarch_complete, info = "Would you want to explore this method further?")
    })
    
    test_that("correct hierarchical with single linkage results", {
        expect_identical(soln_3, explore_hierarch_single, info = "Is the number of patients in each cluster balanced?")
    })
})
