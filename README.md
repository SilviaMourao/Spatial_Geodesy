# Spatial Geodesy Class Projects

**Institution**: Faculdade de Ciências da Universidade de Lisboa  
**Course**: Spatial Geodesy  
**Project Date**: 2022  
**Project Language**: English/Portuguese  

## Table of Contents

- [Overview](#overview)
- [Projects](#projects)
  - [Date Conversion Algorithm (Fortran)](#date-conversion-algorithm-fortran)
  - [Coordinate Transformation using Helmert Transformation (Fortran)](#coordinate-transformation-using-helmert-transformation-fortran)
  - [Article Analysis: Relationship of Soil Moisture and Reflected GPS Signal Strength](#article-analysis-relationship-of-soil-moisture-and-reflected-gps-signal-strength)
  - [GPS Position Deformation at AMU2 Station](#gps-position-deformation-at-amu2-station)
- [Folder Structure](#folder-structure)
- [Conclusion](#conclusion)

## Overview

This repository contains four projects developed for the Spatial Geodesy class, covering topics like date conversion, coordinate transformation between reference frames, GNSS signal reflection analysis, and deformation evaluation at a GPS station.

## Projects

### Date Conversion Algorithm (Fortran)

- **Objective**: Develop a **Fortran** algorithm to convert between different date formats, including Year-Month-Day-Hour, Julian Date, and Day of Year.
  
- **Code**:
  - Converts dates using multiple formats, such as Gregorian, Julian Date, Modified Julian Date, and GPS week and seconds.
  - Accounts for leap years and returns outputs in various formats.
  
- **Features**:
  - Input date formats: **Year-Month-Day-Hour**, **Julian Date**, or **Day of Year**.
  - Outputs include the corresponding formats for JD, MJD, DOY, and GPSW.

### Coordinate Transformation using Helmert Transformation (Fortran)

- **Objective**: Implement a **Fortran** algorithm to perform coordinate transformations between different versions of the **International Terrestrial Reference Frame (ITRF)** using the **Helmert Transformation**.

- **Code**:
  - Transforms coordinates from ITRF2014 to ITRF2008, ITRF2005, ITRF2000, and ETRF2005 using transformation matrices and velocity adjustments.
  
- **Features**:
  - Handles file-based input (e.g., `ITRF2014.txt`) or manual entry of coordinates.
  - Outputs transformed coordinates in all the listed reference frames.

### Article Analysis: Relationship of Soil Moisture and Reflected GPS Signal Strength

- **Objective**: Analyze the article *"Relationship of Soil Moisture and Reflected GPS Signal Strength"* by Privette III et al. (2016).
  
- **Deliverables**:
  - A report and presentation summarizing the article's findings on the relationship between GPS signal reflection and soil moisture levels.

- **Key Insights**:
  - The article discusses how GPS signal strength varies with soil moisture and how this data can be used for remote soil moisture estimation.

### GPS Position Deformation at AMU2 Station

- **Objective**: Analyze and evaluate position deformations at the **AMU2 GPS station** using **RTKLIB** (an open-source GNSS processing software).

- **Processing**:
  - Both **Static PPP** and **Kinematic PPP** methods were used to evaluate positioning shifts.
  - Results were compared with outputs from the **Jet Propulsion Lab (JPL)** online processing service.

- **Findings**:
  - Horizontal deformations were linked to **ice sheet movement**.
  - Vertical deformations were more complex, likely caused by **climate change**, with some links to **ice creep**.

## Folder Structure

- `/Article_Analysis/`: Files for the article analysis project.
  - `Analysis_report.pdf`: Report analyzing the Privette III et al. (2016) article.
  - `presentation.pptx`: Presentation summarizing the article analysis.
  - `Privette_2016.pdf`: The article being analyzed.

- `/Coordinate_Transformation/`: Files for the coordinate transformation project.
  - `Transf.f`: Fortran code for coordinate transformations using Helmert transformations.
  - `ITRF2014.txt`: Example input file with initial coordinates.

- `/Date/`: Files for the date conversion project.
  - `Date.f`: Fortran code for date format conversion.

- `/GPS_position_deformation/`: Files for the GPS deformation analysis project.
  - `GPS_deformation_report.pdf`: Report analyzing deformations at the AMU2 GPS station.

## Conclusion

These projects showcase the diverse applications of **Spatial Geodesy**, including developing algorithms in **Fortran** for date conversion and coordinate transformations, as well as analyzing **GNSS data** for deformation studies at the AMU2 GPS station. The analysis of GPS signals for environmental monitoring further demonstrates the wide-ranging applications of geodetic techniques.
