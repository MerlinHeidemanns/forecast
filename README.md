# German Election Poll Tracker

A tool for collecting, analyzing, and visualizing German election polling data.

## Project Structure

- `/estimation`: Statistical modeling and trend estimation
    - `/dta`: Raw and processed polling data
    - `/plt`: Generated plots and visualizations
    - `/stan`: Stan models for Bayesian inference
    - `main.R`: Core estimation pipeline
    - `plotting.R`: Visualization functions
    - `utils.R`: Helper functions

- `/scraping`: Data collection scripts
    - `scrapers.py`: Poll scraping implementations
    - `utils.py`: Helper functions for scraping

- `/web`: Frontend visualization
    - `/public`: Static assets and data files
    - `script.js`: Frontend logic
    - `package.json`: Project dependencies

## Features

- Automated scraping of German polling data
- Bayesian trend estimation using Stan
- Interactive web visualization of polling trends
- Time-varying volatility analysis
- Multi-party correlation structure


# License

© 2024. All rights reserved.

This software is made available for academic and research purposes only. Commercial use is strictly prohibited.
