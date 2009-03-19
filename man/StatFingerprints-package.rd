\docType{package}
\name{StatFingerprints}
\alias{StatFingerprints}
\alias{statfingerprints}
\alias{StatFingerprint}
\alias{statfingerprint}
\alias{SF}
\alias{sf}
\title{Graphical user interface of StatFingerprints package}
\description{A Tcl/Tk GUI to use functions of the \code{StatFingerprints} package.}

\section{Menu bar}{
The menu bar at the top of the window is used to launch all the functions of the package.\cr

The code \code{Files} menu is used to create, save and load a new project, and to quit R:

- \code{New project...} is composed of two submenus: the first \code{Import fingerprint profiles} to import fingerprint profiles, and the second \code{Import variables (quantitative or qualitative)} to import tables of external variables. The fingerprint profiles can be imported in three different ways: i) from a folder containing FSA files obtained with an ABI Prism 310 or 3100 sequencer (choose \code{Convert FSA files and import}), ii) from a folder containing ASCII files (choose \code{Import ASCII files}), iii) from an ecological table with fingerprint profiles in rows or columns (choose \code{Import an ecological table}). The last two of these can read all ASCII formats.

- \code{Load project} is used to import a \code{StatFingerprints} project previously saved as an Rdata file.

- \code{Save project as...} is used to save a \code{StatFingerprints} project in a specific directory. The project is saved as an Rdata file.

- \code{Save} is used to save the current \code{StatFingerprints} project in its current directory.

- \code{Quit R} opens a dialog window asking whether the environment should be saved before quitting R.\cr
The \code{Edit} menu is used to rename, add or delete fingerprint profiles of the project:

- \code{Change names of fingerprint profiles} renames the name of the fingerprint profiles in the current project.

- \code{Add fingerprint profiles to the project} adds new fingerprint profiles within the current project. The new fingerprint profiles must be first processed in another new project and then this new project is merged with the current project.

- \code{Delete fingerprint profiles in the project} deletes fingerprint profiles in the current project.

- \code{Select fingerprint profiles using levels of factors} offers the possibility of selecting several fingerprint profiles in the current project corresponding to one or more levels of a factor.\cr

The \code{Fingerprint profiles processing} menu includes all functionalities required to process the fingerprint profiles. The fingerprint profiles must be processed before statistical analyses to make them comparable.

- \code{Step 1: define standard} is needed to define the peaks of the reference standard on which fingerprint profiles will be aligned together using the \code{Align fingerprint profiles one by one} function. The x-values of each peak of the reference standard can be either manually entered using \code{Define peaks of your own standard} or automatically selected if the ROX HD400 is used with the \code{Use peaks of ROX HD400 Applied Biosystems}function.
- \code{Step 2: align fingerprint profiles to the standard} firstly allows the fingerprint profiles to be aligned one by one using the \code{Align fingerprint profiles one by one} function and secondly to check the alignment using \code{Option: Check quality of alignment}.

- \code{Step 3: define a common baseline for all fingerprint profiles} is used to homogenise and reorientate the baseline between fingerprint profiles on a horizontal axis.

- \code{Option: Delete backgrounds under fingerprint profiles} can be used to deletebackground under fingerprint profile.

- \code{Step 4: define the range of the fingerprint profiles} allows just the section where the community appears to be selected by deleting extremities of the fingerprint profiles.

- \code{Option: Rebuild peaks of fingerprint profiles presenting defects} is used to redesign a peak presenting a defect into a fingerprint profile.

- \code{Step 5: normalise area under fingerprint profiles} is used to normalise the area under each fingerprint profile so that the area under a signal is equal to one. Three different algorithms are available to deal with minimum and negative values.

-\code{Option: Transform profiles into presence/absence profiles} allows quantitative fingerprint profiles to be transformed into binary fingerprint profiles so that areas with and without peaks are respectively equal to one and zero.\cr


The \code{Plot} menu includes all functionalities to plot the fingerprint profiles of the project and the ordination graphs:

- \code{Plot fingerprint profiles} is used to plot one or several fingerprint profiles in two dimensions or the entire set of fingerprint profiles in three dimensions for any processing step of the fingerprint profiles.

- \code{Plot saved nMDS/PCA: advanced tools} plots previously calculated non-Metric Multidimensional Scaling (nMDS) and Principal Components Analysis (PCA) in a 2- or 3-dimensional display. PCA and nMDS are plotted with coloured points according to a factor and/or with contour lines coming from a quantitative variable.\cr

The \code{Univariate statistic: diversity index} menu can be used both to estimate diversity indexes and to compute basic univariate statistics. It illustrates the effect of factors or external parameters on the diversity of the community.

- \code{Compute diversity index} allows various diversity indexes to be estimated.

- \code{Descriptive statistic} calculates the arithmetic mean and standard deviation of diversity indexes or imported quantitative variables.

- \code{Multifactor ANOVA} displays summary results of multifactor ANOVA using a diversity index or imported quantitative variables as the variable to be explained and one or more imported qualitative variables as explanatory variables.

- \code{Simple correlation} computes a simple correlation using a Pearson algorithm between two quantitative variables (diversity index or imported quantitative variable). This function returns the Pearson R-squared, its associated p value and the first degree regression coefficient.\cr


The \code{Multivariate statistic: structure} menu is composed of exploratory statistical methods and multivariate statistical tests. It allows the similarity between fingerprint profiles to be explored, to test the effect of a factor along the fingerprint profiles and to assess the relationship between the structure of the community and external parameters.

- \code{Non-Metric multiDimensional Scaling (nMDS)} computes iterative nMDS and keeps the best one, i.e. that with the lowest value of the Kruskal stress.

- \code{Principal Components Analysis (PCA)} computes PCA, optionally centred and or scaled.
- \code{Compare PCA vs nMDS} compares PCA and nMDS by calculating the Pearson R squared between the initial distances and the new distances computed by the two ordination methods.

- \code{Hierarchical clustering} displays a dendrogram. Several proximity indexes and dendrogram algorithms are proposed.

- \code{Heatmap} displays a coloured heatmap based on hierarchical clustering.

- \code{Multivariate ANOVA (50 50 F test and rotation)} computes general linear modelling of one or more qualitative variables with the fingerprint profile set.
- \code{ANalysis Of SIMilarity (ANOSIM)} tests whether there is a significant difference between two or more groups of fingerprint profiles. Groups of fingerprint profiles are designed according to the levels of a qualitative variable with the \code{Global ANOSIM: test effect of factor} function or to each pair of levels within a factor with the \code{Pairwise ANOSIM: test effect of levels within factor} function.

- \code{Within group variability} calculates within each group the variability between all fingerprint profiles and tests whether these variabilities are significantly different. Groups are designed according to the levels of a factor.

- \code{SIMilarity PERcentages procedure (SIMPER)} calculates the relative contribution of each variable of the fingerprint profiles to the total dissimilarity (measure by a proximity index) between two groups of fingerprint profiles. The two groups are designed according to the levels of a factor. It is a simple method for assessing which areas along the fingerprint profiles are primarily responsible for an observed difference between the two groups.
- \code{Iterative tests (ANOVA,Mann Whitney,Fisher s exact)} tests the significant differences of each variable along the fingerprint profile between two groups. The test used is the t test, the Mann Whitney test or the Fisher s exact test. The two groups are designed according to the levels of a factor. Consequently it allows assessing which areas of the fingerprint profiles are significantly responsible for the difference observed between the two groups.

- \code{Multivariate correlation (50 50 F test and rotation)} is used to test the relationship between the structure of the fingerprint profile set and an imported quantitative variable.\cr

The \code{Help} menu contains the user guide manual, an on line help, the most recent version of the present program and the bugs report.



}
\author{ Rory Michelland \email{rory.michelland@gmail.com}\cr
Laurent Cauquil \email{laurent.cauquil@toulouse.inra.fr}
}
\examples{
\dontrun{
## One of these lines allows the StatFingerprints program to be started
StatFingerprints()
StatFingerprint()
statfingerprints()
statfingerprint()
SF()
sf()
}
}
