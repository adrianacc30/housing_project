# Rent Growth and Student Entrants in Germany

Do German university districts with stronger rent growth also experience different changes in student entrants?

This exploratory R project combines university-level winter-semester entrants, district population counts, and RWI-GEO-REDX apartment-rent indices. It compares **2015–2024 rent growth** with the change in **entrants per 100 residents aged 18–25**, highlighting Mannheim.

## Code and figure

`rent_student_entrants.R` builds the university–district links, joins year-specific population data, and exports the scatterplot and matching diagnostics.

Install dependencies with `Rscript install_packages.R`, then run `Rscript rent_student_entrants.R` from this folder. Input requirements are in [DATA.md](DATA.md).

**Status:** University locations, historical district boundaries, and student-count definitions still need verification. 

The outcome measures **new entrants**, not total enrolled students or residents attending university. Any resulting association is descriptive.
