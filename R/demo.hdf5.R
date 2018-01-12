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
