#--------------------------------------------------------------------------------------------------
# This script visualizes the results of the 1D Poisson equation solver.
#--------------------------------------------------------------------------------------------------
import numpy as np
import matplotlib.pyplot as plt

# Load the results from the output file
results = np.loadtxt('poisson_results.dat')

# Extract the x and u values
x = results[:, 0]
u = results[:, 1]

# Plot the results
plt.figure(figsize=(10, 6))
plt.plot(x, u, label='Numerical Solution', marker='o')
plt.title('1D Poisson Equation Solution')
plt.xlabel('x')
plt.ylabel('u(x)')
plt.grid()
plt.legend()
plt.show()