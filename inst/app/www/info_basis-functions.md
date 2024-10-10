The "Number of basis functions" governs the number of basis functions used for the approximation
of the surface, so called approximate thin-plate-splines
(see e.g. [doi:10.1007/3-540-47977-5_2](https://link.springer.com/chapter/10.1007/3-540-47977-5_2) or [doi:10.1111/1467-9868.00374](https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/1467-9868.00374) 
for further information).

A higher number delivers more exact results and enables the estimation of more complex 2-D surfaces,
but uses much more computational power. In general, there is little gain from very high numbers and
there is no additional gain from selecting a higher number than there are unique combinations of
longitude and latitude. <br><br>

#### Selection of the number of basis functions (Rule of Thumb)

**1. Start Simple**

- Begin with a small number of basis functions. This prevents overfitting and allows you to model
  the overall trend without capturing too much noise.

**2. Data Complexity**

- **Smooth Data, Simple Relationship**: Fewer basis functions are needed.
- **Complex Patterns**: If the data has non-linear relationships, many peaks, and troughs, more 
  basis functions may be required.

**3. Sample Size Consideration**

- The number of basis functions should generally be much smaller than the number of data points.
- A typical range might be between 5% to 20% of the sample size, depending on the complexity of the data.
- **Example**: With 100 data points, start with 5 to 20 basis functions.

**4. Avoid Overfitting**

- As you increase the number of basis functions, the risk of overfitting increases.
- Keep the number low enough to avoid capturing noise in the data.

**Example Guidelines**

- **Low Complexity (e.g., Linear Trend)**: Start with 2-5 basis functions.
- **Moderate Complexity (e.g., Quadratic or Cubic Trend)**: Consider 5-10 basis functions.
- **High Complexity (e.g., Highly Non-linear Relationship)**: Explore 10-20 or more basis functions,
  but monitor for overfitting closely.

**Summary**

Begin with a small number of basis functions, and increase incrementally to find the optimal number 
that balances model complexity and performance. The exact number will depend on the nature of your 
data and the specific application.
