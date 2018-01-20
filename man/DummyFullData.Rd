\name{DummyFullData}
\alias{DummyFullData}
\docType{data}
\title{
	Artificially generated Metabolomic Study on Cancer 
}
\description{
	This dataset is a matrix containing measurements of metabolite concentration.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("DummyFullData")}
\format{
	A datamatrix with 40 rows and 40 columns. Each row is a metabolite. The columns are the concentration measurements of metabolites from different subjects. 
}
\details{
	This is an artificially generated dataset that contain first 5 metabolites('Guanidoacetate', 'O-Acetylcarnitine',\cr
        '2-Aminobutyrate','Formate' and 'Xylose') with lower concentration and next 5 metabolites i.e., no. 6-10 metabolites('Valine', 'Hypoxanthine', 'Fumarate','Tyrosine' and 'Pantothenate') with higher concentration for the cancer patient.

%%  ~~ If necessary, more details than the __description__ above ~~
}

\examples{
data(DummyFullData)
## View the artificially generated dataset.
View(DummyFullData)
}
\keyword{datasets}
