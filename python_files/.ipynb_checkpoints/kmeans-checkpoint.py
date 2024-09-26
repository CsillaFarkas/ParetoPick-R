import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt
import numpy as np
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
import seaborn as sns
from scipy.spatial.distance import cdist
import configparser
from pathlib import Path

def remove_outliers(data, deviations, count):
    # Omit data points where "count" or more variables are "deviations" away from their respective means from clustering (these later become their own representative solutions)
    z_scores = (data - data.mean()) / data.std()
    outliers = (np.abs(z_scores) > deviations)
    outliers_count = outliers.sum(axis=1)
    data_no_outliers = data[outliers_count < count]
    num_outliers = np.sum(outliers_count >= count)
    #print(data[outliers_count >= count]) # Prints the data points considered outliers
    return data_no_outliers, num_outliers

def kmeans_clustering(pca_data, num_clusters):
    # Creates clusters using the kmeans algorithm from the sklearn package
    # Returns the cluster assignments and the coordinates of the cluster centers
    kmeans = KMeans(n_clusters=num_clusters, init='k-means++', n_init=100, random_state=58) # no large difference when switching between k-means++ and random as the initialization methods
    kmeans.fit(pca_data)
    labels = kmeans.predict(pca_data)
    return labels, kmeans.cluster_centers_

def find_closest_pareto_solution(centroid, input_data):
    # Intakes the center coordinates of a cluster and returns the closest pPreto optimal solution
    distances = cdist([centroid], input_data)
    closest_pos_idx = np.argmin(distances) #returns the index within the distances array 
    closest_idx = input_data.index[closest_pos_idx]
    return closest_idx, input_data.loc[[closest_idx]]

def run_kmeans_multiple_times(pca_data, min_clusters, max_clusters, input_data, pca, fixed_clusters_boolean, fixed_clusters): #here input data has the outliers removed if outliers are removed from the dataset
    # Iterates through the kmeans algorithm to determine which number of clusters performs the best according the silhouette score
    # Returns the solution with the highest silhouette score

    best_score = -1
    best_labels = None
    best_centroids = None
    if fixed_clusters_boolean == 1:
        best_labels, best_centroids = kmeans_clustering(pca_data, fixed_clusters)
        #pca_sil_score = silhouette_score(pca_data, best_labels)
        best_score = silhouette_score(input_data, best_labels)
        print(f"Clusters: {fixed_clusters}, Input Data Silhouette Score: {best_score:.4f}")        
    elif fixed_clusters_boolean == 0:
        for num_clusters in range(min_clusters, max_clusters + 1):
            labels, centroids = kmeans_clustering(pca_data, num_clusters)
            #pca_sil_score = silhouette_score(pca_data, labels)
            inputdata_sil_score = silhouette_score(input_data, labels)
            print(f"Clusters: {num_clusters}, Input Data Silhouette Score: {inputdata_sil_score:.4f}")
            if inputdata_sil_score > best_score:
                best_score = inputdata_sil_score
                best_labels = labels
                best_centroids = centroids
    else: print("error wrong input value for fixed_clusters_boolean")
    
    # Initialize a list of representative solutions
    representative_solutions = []
    representative_solutions_index = []

    # Unstandardize the centroids
    # Then we cast the data (which is currently in terms of the principal components) back to the original axes
    input_axes_centroids = pca.inverse_transform(best_centroids)

    # Intakes the un-transformed centroids of the clusters and finds the closest representative solution
    for centroid in input_axes_centroids:
        solution_index, closest_pareto_solution = find_closest_pareto_solution(centroid, input_data)
        representative_solutions.append(closest_pareto_solution)
        representative_solutions_index.append(solution_index)
    
    return best_labels, representative_solutions_index, representative_solutions, best_score

def non_integer_range(start, stop, step):
    current = start
    while current < stop:
        yield current
        current += step

def main():
    #### Load Configuration ####
    config_path = Path('..') / 'input' / 'config.ini'
    config = configparser.ConfigParser()
    config.read(config_path)

    input_file = config['Data']['input_file']
    columns = config['Data']['columns'].split(', ')
    min_clusters = config.getint('Clustering', 'min_clusters')
    max_clusters = config.getint('Clustering', 'max_clusters')
    fixed_clusters_boolean = config.getint('Clustering', 'fixed_clusters_boolean')
    fixed_clusters = config.getint('Clustering', 'fixed_clusters')
    handle_outliers_boolean = config.getint('Extreme_solutions', 'handle_outliers_boolean')
    deviations_min = config.getfloat('Extreme_solutions', 'deviations_min')
    deviations_max = config.getfloat('Extreme_solutions', 'deviations_max')
    deviations_step = config.getfloat('Extreme_solutions', 'deviations_step')
    count_min = config.getint('Extreme_solutions', 'count_min')
    count_max = config.getint('Extreme_solutions', 'count_max')
    outlier_to_cluster_ratio = config.getfloat('Extreme_solutions', 'outlier_to_cluster_ratio')
    min_components = config.getint('PCA', 'min_components')
    max_components = config.getint('PCA', 'max_components')
    num_variables_to_plot = config.getint('Plots','num_variables_to_plot')
    x_axis = config['Plots']['x_axis']
    x_axis_label = config['Plots']['x_axis_label']
    x_axis_min = config.getfloat('Plots', 'x_axis_min')
    x_axis_max = config.getfloat('Plots', 'x_axis_max')
    y_axis = config['Plots']['y_axis']
    y_axis_label = config['Plots']['y_axis_label']
    y_axis_min = config.getfloat('Plots', 'y_axis_min')
    y_axis_max = config.getfloat('Plots', 'y_axis_max')
    hue = config['Plots']['hue']
    hue_label = config['Plots']['hue_label']
    size = config['Plots']['size']
    size_label = config['Plots']['size_label']
    size_min = config.getfloat('Plots', 'size_min')
    size_max = config.getfloat('Plots', 'size_max')


    # Load data
    input_path = Path('..') / 'input' / input_file
    raw_data = pd.read_csv(input_path)

    # Subset the data to only include the columns for the framework
    input_data = raw_data[columns].copy()

    # Define output directory
    output_path = Path('..') / 'output' 
    output_path.mkdir(parents=True, exist_ok=True)  # Create the output directory if it doesn't exist

    final_score = -1
    final_labels = None
    final_rep_solutions = None
    final_rep_solutions_index = None
    final_input_data_no_outliers = None
    final_raw_data_no_oultiers = None
    final_num_outliers = None
    final_pca = None

    # if handle_outliers_boolean = 1 then extreme solutions will be defined based on the number of variables within the point (count) that are a certain number of standard deviations from the variable mean (deviations)
    # these solutions will then be ommitted from clustering an defined as their own representative solutions
    if handle_outliers_boolean == 1:
        # iterates through the range of principal component values
        for num_components in range(min_components, max_components + 1):
            print(f"\nNumber of Principal Components: {num_components}")

            # Define PCA
            pca = PCA(n_components=num_components)
            
            # iterates through the range of deviation and count values used for defining extreme solutions
            for d in non_integer_range(deviations_min, deviations_max+deviations_step, deviations_step):
                for c in range(count_min, count_max+1):

                    # Remove data points where three or more variable values are considered outliers before clustering
                    input_data_no_outliers, num_outliers = remove_outliers(input_data, d, c)

                    # Remove outliers from the original data
                    raw_data_no_outliers = raw_data[raw_data.index.isin(input_data_no_outliers.index)].copy()  # Filter based on input_data_no_outliers index

                    # Cast the input data onto the defined number of principal components
                    principal_components = pca.fit_transform(input_data_no_outliers)

                    # Create a DataFrame to hold the principal component data (aka the input data cast onto the x principal component axes)
                    pca_data = pd.DataFrame(data=principal_components, columns=[f"PC{i+1}" for i in range(num_components)])

                    # pca_data.to_csv("pca_data.csv", index=False) # Creaates a CSV file with the pca data incase that's of interest

                    # Perform k-means clustering multiple times and select the best result based on highest silhouette score
                    print(f"\nnumber of extreme solutions: {num_outliers}, deviations: {d}, count: {c}")
                    labels, representative_solutions_index, representative_solutions, silhouette_score = run_kmeans_multiple_times(pca_data, min_clusters, max_clusters, input_data_no_outliers, pca, fixed_clusters_boolean, fixed_clusters)
                    
                    # define the final solution as the solution with the highest silhouette score; the number of extreme solutions must also be less than the number of clusters multiplied by the outlier_to_cluster_ratio
                    if ((silhouette_score > final_score) and (num_outliers < (len(representative_solutions_index) * outlier_to_cluster_ratio))):
                        final_score = silhouette_score
                        final_labels = labels
                        final_rep_solutions = representative_solutions
                        final_rep_solutions_index = representative_solutions_index
                        final_input_data_no_outliers = input_data_no_outliers
                        final_raw_data_no_outliers = raw_data_no_outliers
                        final_num_outliers = num_outliers
                        final_components = num_components

        print(f"\nBest Silhouette Score: {final_score}, Number of Clusters: {len(final_rep_solutions_index)}, Number of Extreme Solutions: {final_num_outliers}, Num of PCA: {final_components}")
        
        # Add cluster labels to the dataframe
        final_raw_data_no_outliers['Cluster'] = final_labels

        # Add a column to the dataframe that indicates a representative solution
        final_raw_data_no_outliers.loc[final_raw_data_no_outliers.index.isin(final_rep_solutions_index), 'Representative_Solution'] = 1

        # Creates a dataframe of the outliers with the original columns
        outliers = raw_data[~raw_data.index.isin(final_input_data_no_outliers.index)].copy()
        
        # Creates a dataframe of the entire original dataset with a column labeled Cluster which represents the cluster number of the solution and a column labeled Representative_Solution which if one if a solution is representative of a cluster or if a solution is an outlier
        all_data = pd.concat([final_raw_data_no_outliers, outliers.assign(Cluster ='outlier',Representative_Solution=1)])

        # Exports final dataframe to a CSV
        out_file_path = output_path / "kmeans_data_w_clusters_representativesolutions_outliers.csv"
        all_data.to_csv(out_file_path, index=False)
        
        # Create 2D scatter plots comparing the objective space variables
        #sns.pairplot(all_data, hue='Cluster', vars=['BD', 'LF', 'NC', 'AY'], palette='viridis')
        #plt.show()

    # if extreme solutions are not to be handled
    else:        
        # iterate through the range of principal component values
        for num_components in range(min_components, max_components + 1):
            print(f"\nNumber of Principal Components: {num_components}")

            # Define PCA
            pca = PCA(n_components=num_components)

            # Cast the input data onto the defined number of principal components
            principal_components = pca.fit_transform(input_data)

            # Create a DataFrame to hold the principal component data (aka the input data cast onto the x principal component axes)
            pca_data = pd.DataFrame(data=principal_components, columns=[f"PC{i+1}" for i in range(num_components)])

            # pca_data.to_csv("pca_data.csv", index=False) # Creaates a CSV file with the pca data incase that's of interest
            
            # Perform k-means clustering multiple times and select the best result
            labels, representative_solutions_index, representative_solutions, silhouette_score = run_kmeans_multiple_times(pca_data, min_clusters, max_clusters, input_data, pca, fixed_clusters_boolean, fixed_clusters)

            if (silhouette_score > final_score):
                final_score = silhouette_score
                final_labels = labels
                final_rep_solutions = representative_solutions
                final_rep_solutions_index = representative_solutions_index
                final_components = num_components

        print(f"\nBest Silhouette Score: {final_score}, Number of Clusters: {len(final_rep_solutions_index)}, Num of PCA: {final_components}")

        # Save the raw data with clusters appended to a CSV file
        raw_data['Cluster'] = final_labels
        
        # Add a column to the dataframe that indicates a representative solution
        raw_data.loc[raw_data.index.isin(final_rep_solutions_index), 'Representative_Solution'] = 1
        
        # Export dataframe to a CSV
        out_file_path = output_path / "kmeans_data_w_clusters_representativesolutions.csv"
        raw_data.to_csv(out_file_path, index=False)

        # # Create 2D scatter plots comparing the objective space variables
        # sns.pairplot(raw_data, hue='Cluster', vars=['BD', 'LF', 'NC', 'AY'], palette='viridis')
        # plt.show()

        all_data = raw_data

    # Plot Final Representative Solutions
    rep_data = all_data[all_data['Representative_Solution']==1].copy()
    rep_data.rename(columns={hue: hue_label,size:size_label}, inplace=True)

    sns.set_theme() 
    
    # Plot data on a 2-D axis using color and size of points as additional inputs
    ax = sns.scatterplot(x=x_axis,y=y_axis, hue=hue_label, size=size_label, sizes=(size_min,size_max), data=rep_data, palette = 'viridis') #edgecolors='black', legend = 'brief', , alpha=0.5

    sns.move_legend(ax,"upper left", bbox_to_anchor=(1,1))
    ax.set_xlabel(x_axis_label)
    ax.set_ylabel(y_axis_label)

    ax.set_xlim(x_axis_min, x_axis_max)
    ax.set_ylim(y_axis_min, y_axis_max)

    # ax.set_xticks([1.75,2.25,2.75,3.25])
    # ax.set_yticks([0,10,20,30])

    # Show the plot
    plt.show()

    #### Additional Plots ####
    # # Create 2D pairplot comparing the several variables (more readable for smaller variable count)
    # sns.pairplot(rep_data, vars=['BD', 'Lowflow [m2/s]', 'Nitrate load [kg N/yr]', 'AY'])
    # plt.show()

    # # Plot a 3D scatter plot with color as the fourth dimension
    # fig = plt.figure()
    # ax3 = fig.add_subplot(projection='3d')

    # sp = ax3.scatter(rep_data['AY'], rep_data['BD'], rep_data['Nitrate load [kg N/yr]'], c=rep_data['Lowflow [m2/s]'])

    # ax3.set_xlabel('AY')
    # ax3.set_ylabel('BD')
    # ax3.set_zlabel('NC')

    # legend = ax3.legend(*sp.legend_elements(), title='AY', loc=2, bbox_to_anchor=(1.1,1))
    # ax3.add_artist(legend)

    # plt.show()

if __name__ == "__main__":
    main()

