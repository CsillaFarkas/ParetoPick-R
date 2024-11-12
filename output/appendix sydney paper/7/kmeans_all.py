import pandas as pd
import numpy as np
import configparser
from pathlib import Path
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score
from scipy.spatial.distance import cdist
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import seaborn as sns
import subprocess
from matplotlib.colors import Normalize
from matplotlib.lines import Line2D
from matplotlib import cm

def remove_outliers(data, deviations, count):
    # Omit data points where "count" or more variables are "deviations" away from their respective means from clustering (these later become their own representative solutions)
    z_scores = (data - data.mean()) / data.std()
    outliers = (np.abs(z_scores) > deviations)
    outliers_count = outliers.sum(axis=1)
    data_no_outliers = data[outliers_count < count]
    num_outliers = np.sum(outliers_count >= count)
    # print(data[outliers_count >= count]) # Prints the data points considered outliers
    return data_no_outliers, num_outliers

def kmeans_clustering(pca_data, num_clusters):
    # Creates clusters using the kmeans algorithm from the sklearn package
    # Returns the cluster assignments and the coordinates of the cluster centers
    kmeans = KMeans(n_clusters=num_clusters, init='k-means++', n_init=100, random_state=58) # initialization methods can be k-means++ or random 
    kmeans.fit(pca_data)
    labels = kmeans.predict(pca_data)
    return labels, kmeans.cluster_centers_

def find_closest_pareto_solution(centroid, input_data):
    # Intakes the center coordinates of a cluster and returns the closest Pareto optimal solution
    distances = cdist([centroid], input_data)
    closest_pos_idx = np.argmin(distances) # Returns the index within the distances array 
    closest_idx = input_data.index[closest_pos_idx] # Returns the index from the original data
    return closest_idx, input_data.loc[[closest_idx]]

def run_kmeans_multiple_times(pca_data, min_clusters, max_clusters, input_data, pca, fixed_clusters_boolean, fixed_clusters): #here input data has the outliers removed if outliers are removed from the dataset
    # Iterates through the kmeans algorithm to determine which number of clusters performs the best according the silhouette score
    # Returns the solution with the highest silhouette score
    best_score = -1
    best_labels = None
    best_centroids = None
    if fixed_clusters_boolean == True: # If the user has opted for a fixed number of clusters, the code will only run for fixed_clusters
        best_labels, best_centroids = kmeans_clustering(pca_data, fixed_clusters)
        #pca_sil_score = silhouette_score(pca_data, best_labels)
        best_score = silhouette_score(input_data, best_labels)
        print(f"Clusters: {fixed_clusters}, Input Data Silhouette Score: {best_score:.4f}")        
    elif fixed_clusters_boolean == False: # If the user has NOT opted for a fixed number of clusters, the code will itereate through min_clusters to max_clusters inclusive
        for num_clusters in range(min_clusters, max_clusters + 1):
            labels, centroids = kmeans_clustering(pca_data, num_clusters)
            #pca_sil_score = silhouette_score(pca_data, labels)
            inputdata_sil_score = silhouette_score(input_data, labels)
            print(f"Clusters: {num_clusters}, Input Data Silhouette Score: {inputdata_sil_score:.4f}")
            if inputdata_sil_score > best_score:
                best_score = inputdata_sil_score
                best_labels = labels
                best_centroids = centroids
    else: print("error: wrong input value for fixed_clusters_boolean")
    
    # Initialize a list of representative solutions
    representative_solutions = []
    representative_solutions_index = []

    # Unstandardize the centroids
    # Then cast the data (which is currently in terms of the principal components) back to the original axes
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
    fixed_clusters_boolean = config.getboolean('Clustering', 'fixed_clusters_boolean')
    fixed_clusters = config.getint('Clustering', 'fixed_clusters')
    handle_outliers_boolean = config.getboolean('Extreme_solutions', 'handle_outliers_boolean')
    deviations_min = config.getfloat('Extreme_solutions', 'deviations_min')
    deviations_max = config.getfloat('Extreme_solutions', 'deviations_max')
    deviations_step = config.getfloat('Extreme_solutions', 'deviations_step')
    count_min = config.getint('Extreme_solutions', 'count_min')
    count_max = config.getint('Extreme_solutions', 'count_max')
    outlier_to_cluster_ratio = config.getfloat('Extreme_solutions', 'outlier_to_cluster_ratio')
    min_components = config.getint('PCA', 'min_components')
    max_components = config.getint('PCA', 'max_components')
    num_variables_to_plot = config.getint('Plots','num_variables_to_plot')
    x_axis = config['Plots']['var_1']
    x_axis_label = config['Plots']['var_1_label']
    y_axis = config['Plots']['var_2']
    y_axis_label = config['Plots']['var_2_label']
    hue = config['Plots']['var_3']
    hue_label = config['Plots']['var_3_label']
    size = config['Plots']['var_4']
    size_label = config['Plots']['var_4_label']
    var_1 = config['Plots']['var_1']
    var_1_label = config['Plots']['var_1_label']
    var_2 = config['Plots']['var_2']
    var_2_label = config['Plots']['var_2_label']
    var_3 = config['Plots']['var_3']
    var_3_label = config['Plots']['var_3_label']
    var_4 = config['Plots']['var_4']
    var_4_label = config['Plots']['var_4_label']
    size_min = config.getfloat('Plots', 'size_min')
    size_max = config.getfloat('Plots', 'size_max')
    qualitative_clustering_columns = config['Qualitative_Clustering']['qualitative_clustering_columns'].split(', ')
    run_frequency_plot = config.getboolean('Frequency_Plots','plot_frequency_maps')
    
    # Load input data
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

    # If handle_outliers_boolean = True then extreme solutions will be defined based on the number of variables within the point (count) that are a certain number of standard deviations from the variable mean (deviations)
    # These solutions will then be ommitted from clustering and defined as their own representative solutions
    if handle_outliers_boolean == True:
        # Iterate through the range of principal component values
        for num_components in range(min_components, max_components + 1):
            print(f"\nNumber of Principal Components: {num_components}")

            # Define PCA
            pca = PCA(n_components=num_components)
            
            # Iterate through the range of deviation and count values used for defining extreme solutions
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

                    # pca_data.to_csv("pca_data.csv", index=False) # Creates a CSV file with the pca data incase that's of interest

                    # Perform k-means clustering multiple times and select the best result based on highest silhouette score
                    print(f"\nnumber of extreme solutions: {num_outliers}, deviations: {d}, count: {c}")
                    labels, representative_solutions_index, representative_solutions, silhouette_score = run_kmeans_multiple_times(pca_data, min_clusters, max_clusters, input_data_no_outliers, pca, fixed_clusters_boolean, fixed_clusters)
                    
                    # Define the final solution as the solution with the highest silhouette score; the number of extreme solutions must also be less than the number of clusters multiplied by the outlier_to_cluster_ratio
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
        final_raw_data_no_outliers.loc[final_raw_data_no_outliers.index.isin(final_rep_solutions_index), 'Representative_Solution'] = final_raw_data_no_outliers.loc[final_raw_data_no_outliers.index.isin(final_rep_solutions_index), 'Cluster'] #1

        # Creates a dataframe of the outliers with the original columns
        outliers = raw_data[~raw_data.index.isin(final_input_data_no_outliers.index)].copy()
        
        # Creates a dataframe of the entire original dataset with a column labeled Cluster which represents the cluster number of the solution and a column labeled Representative_Solution which if one if a solution is representative of a cluster or if a solution is an outlier
        all_data = pd.concat([final_raw_data_no_outliers, outliers.assign(Cluster ='outlier',Representative_Solution='outlier')])

        # Exports final dataframe to a CSV
        out_file_path = output_path / "kmeans_data_w_clusters_representativesolutions_outliers.csv"
        all_data.to_csv(out_file_path, index=False)

    # If extreme solutions are not to be handled
    else:        
        # Iterate through the range of principal component values
        for num_components in range(min_components, max_components + 1):
            print(f"\nNumber of Principal Components: {num_components}")

            # Define PCA
            pca = PCA(n_components=num_components)

            # Cast the input data onto the defined number of principal components
            principal_components = pca.fit_transform(input_data)

            # Create a DataFrame to hold the principal component data (aka the input data cast onto the x principal component axes)
            pca_data = pd.DataFrame(data=principal_components, columns=[f"PC{i+1}" for i in range(num_components)])

            # pca_data.to_csv("pca_data.csv", index=False) # Creates a CSV file with the pca data incase that's of interest
            
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
        raw_data.loc[raw_data.index.isin(final_rep_solutions_index), 'Representative_Solution'] = raw_data.loc[raw_data.index.isin(final_rep_solutions_index), 'Cluster'] #1
        
        # Export dataframe to a CSV
        out_file_path = output_path / "kmeans_data_w_clusters_representativesolutions.csv"
        raw_data.to_csv(out_file_path, index=False)

        all_data = raw_data


    
    ## Plot the Representative Solutions ##
    sns.set_theme()


     #Create the full plot
    # Create a figure with GridSpec
    fig3 = plt.figure(figsize=(10,6))
    gs = gridspec.GridSpec(1, 2, width_ratios=[4, 1])  # Allocate space for the plot and the legend

    # Create the main plot
    ax3 = fig3.add_subplot(gs[0])
    rep_data = all_data.copy()
    rep_data.rename(columns={hue: hue_label,size:size_label}, inplace=True)
    
    # Plot data on a 2-D axis using color and size of points as additional inputs
    fixed_size_norm = Normalize(vmin=-0.06, vmax=-0.049)  # Replace with desired range
    fixed_color_norm = Normalize(vmin=58.65, vmax=59.25)  # Replace with desired range for color
    if num_variables_to_plot == 4:
        sns.scatterplot(x=x_axis,y=y_axis, hue=hue_label, size=size_label,sizes=(7, 400),size_norm=fixed_size_norm,hue_norm=fixed_color_norm, data=rep_data, palette = 'viridis', ax=ax3)

    
    # Set axis labels
    ax3.set_xlabel(x_axis_label)
    ax3.set_ylabel(y_axis_label)

    # Creaate the legend in the allocated spcae
    ax3_legend = fig3.add_subplot(gs[1])
    ax3_legend.axis('off')  # Hide the axes for the legend subplot

    
      # Create the main plot
    fig2 = plt.figure(figsize=(10,6))
    gs = gridspec.GridSpec(1, 2, width_ratios=[4, 1])  # Allocate space for the plot and the legend
    ax2 = fig2.add_subplot(gs[0])
    plt.title(" ")

    # Plot Final Representative Solutions
    #rep_data = all_data[all_data['Representative_Solution'].notnull()].copy()
    rep_data = all_data.copy()

    rep_data.rename(columns={var_3:var_3_label,var_4:var_4_label}, inplace=True)
    
    # Plot data on a 2-D axis using color and size of points as additional inputs when necessary
    if num_variables_to_plot == 4:
        sns.scatterplot(x=var_1,y=var_2, hue=var_3_label, size=var_4_label, sizes=(size_min,size_max), data=rep_data, palette = 'viridis', ax=ax2)
        ax2.get_legend().remove() # Remove the legend from the main plot    
    elif num_variables_to_plot == 3: 
        sns.scatterplot(x=var_1,y=var_2, hue=var_3_label, data=rep_data, palette = 'viridis', ax=ax2)
        ax2.get_legend().remove() # Remove the legend from the main plot
    elif num_variables_to_plot == 2: 
        sns.scatterplot(x=var_1,y=var_2, data=rep_data, ax=ax2) 
    else: 
        raise ValueError("num_variables_to_plot must be between 2 and 4")

    # Annotate points with the values from 'Representative_Solution'
    # for line in range(0, rep_data.shape[0]):
    #     value = rep_data['Representative_Solution'].iloc[line]
    #     if isinstance(value, (int, float)):
    #         annotation = int(value)  # Convert to integer if it's a number
    #     else:
    #         annotation = value  # Keep as text if it's text
    #     ax2.text(rep_data[var_1].iloc[line],
    #          rep_data[var_2].iloc[line],
    #          annotation,
    #          horizontalalignment='left',
    #          size='medium',
    #          color='black',
    #          weight='semibold')
    
    # Set axis labels
    ax2.set_xlabel(var_1_label)
    ax2.set_ylabel(var_2_label)


    # Creaate the legend in the allocated space
    ax2_legend = fig2.add_subplot(gs[1])
    ax2_legend.axis('off')  # Hide the axes for the legend subplot

 
    # Draw the legend
    if num_variables_to_plot == 4:
        handles, labels = ax2.get_legend_handles_labels()
        ax2_legend.legend(handles, labels, loc='center left')
    #elif num_variables_to_plot == 3:
    #    handles, labels = ax2.get_legend_handles_labels()
    #    ax2_legend.legend(handles, labels, title =hue_label, loc='center left')
    #elif num_variables_to_plot == 2:
      #  pass
    ax2.ticklabel_format(style='plain', axis='x')
    ax2.set_xbound([25000, -1000000])
    ax2.set_xticks([-1000000,-750000,-500000,-250000,0])
    ax2.set_ybound([-51.5,-69.5])
    # Show the plot
    plt.show()

   

if __name__ == "__main__":
    main()

