# Some code exploring the H5File interface for reading from and
# writing to HDF5 files in R.
library(hdf5r)

# Load the "cars" data set.
data(cars)

# Initialize the HDF5 file. 
f <- H5File$new("cars.h5",mode = "w")

# Save the cars data frame in the HDF5 file. Here, the standard data
# frame attributes (row and column names) are automatically stored 
f[["cars.df"]] <- cars

# Save the cars data as a matrix in the HDF5 file. Here, the row and
# column names are not automatically stored, so we need to add
# attributes storing these.
cars2                <- as.matrix(cars,rownames.force = TRUE)
f[["cars.matrix"]]   <- cars2
x                    <- f[["cars.matrix"]]
h5attr(x,"rownames") <- rownames(cars2)
h5attr(x,"colnames") <- colnames(cars2)

# Close the HDF5 file and clear the environment of all references to
# HDF5 data objects.
f$close_all()
rm(cars,f)

# Read from the HDF5 file we created. The data frame is seamlessly loaded.
f    <- H5File$new("cars.h5",mode = "r")
cars <- f[["cars.df"]][]
print(head(cars))

# Again, recovering the matrix with the row and columns takes more
# effort; these attributes are not loaded seamlessly.
x               <- f[["cars.matrix"]]
cars2           <- x[,]
rownames(cars2) <- h5attr(x,"rownames")
colnames(cars2) <- h5attr(x,"colnames")
print(head(cars2))

# Close the HDF5 file again.
f$close_all()

# Load the "Iris" data set.
data(iris)
iris <- cbind(data.frame(id = sprintf("g%03d",1:150),
                         stringsAsFactors = FALSE),iris)

# Initialize the HDF5 file. 
f <- H5File$new("iris.h5",mode = "w")

# Save the iris data frame in the HDF5 file.
f[["iris"]] <- iris

# Close the HDF5 file and clear the environment of all references to
# HDF5 data objects.
f$close_all()
rm(iris,f)

# Read from the HDF5 file we created. The data frame (including
# character and factor columns) is seamlessly loaded.
f    <- H5File$new("iris.h5",mode = "r")
iris <- f[["iris"]][]
print(head(iris))
print(summary(iris))
f$close_all()

# Initialize another HDF5 file. 
f <- H5File$new("test.h5",mode = "w")

# Create two arrays---one numeric, and one with character strings.
f[["x"]] <- array(rnorm(60),dim = c(5,3,4))
f[["y"]] <- array(letters[1:24],dim = c(2,4,3))
z        <- array(rnorm(60))
z[17]    <- NA
f[["z"]] <- z
f$close_all()
