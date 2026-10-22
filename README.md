# Gerrit Cole vs. Boston Red Sox: 2026 Pitch Matchup Analysis

### MLB Baseball Analytics Portfolio Project | R & Statcast

## Project Overview

This project analyzes Gerrit Cole's 2026 pitching performance and evaluates potential pitch matchups against Boston Red Sox hitters using Baseball Savant Statcast data.

The goal was to understand Cole's pitch arsenal, identify his most effective pitches, examine how Boston hitters perform against different pitch types, and highlight potential opportunities for Cole in a matchup against the Red Sox.

## Tools and Data

- **R:** Data cleaning, statistical analysis, and visualization
- **ggplot2:** Data visualizations and heatmaps
- **dplyr:** Data filtering, grouping, and summarizing
- **Baseball Savant / Statcast:** Pitch level and hitter performance data
- **CSV:** Data storage and matchup results

## Analysis and Visualizations

The project includes 11 visualizations covering:

1. Pitch usage and arsenal
2. Average pitch velocity
3. Horizontal and vertical pitch movement
4. Whiff rate by pitch type
5. Whiff rate by batter handedness
6. Pitch location heatmaps
7. Whiff rate by pitch location
8. Pitch selection by count situation
9. Pitch selection by exact count
10. Boston Red Sox hitter whiff rates by pitch type
11. Gerrit Cole vs. Boston Red Sox matchup heatmap

All charts are available in the [visualizations folder](visualizations/).

## Key Findings

**Pitch Arsenal:** Cole relied heavily on his four-seam fastball (49.1%), followed by his slider (20.0%), knuckle curve (14.1%), and changeup (12.9%).

**Pitch Velocity:** His four-seam fastball averaged 96.8 mph, making it his hardest and most frequently used pitch.

**Swing-and-Miss Effectiveness:** Cole's slider generated his highest whiff rate at 26.3%, followed by his changeup at 22.1%.

**Pitch Location:** Cole's pitch location patterns showed distinct areas of concentration for his fastball, slider, curveball, and changeup.

**Boston Matchups:** Comparing Cole's pitch-specific whiff rates with Boston hitters' whiff rates identified several potential matchup opportunities.

## Top Five Potential Pitch Matchups

| Rank | Boston Hitter | Pitch Type | Matchup Score |
|---|---|---|---|
| 1 | Carlos Narváez | Slider | 36.7% |
| 2 | Anthony Seigler | Slider | 34.9% |
| 3 | Willson Contreras | Slider | 34.8% |
| 4 | Connor Wong | Changeup | 34.1% |
| 5 | Roman Anthony | Slider | 33.6% |

*Matchup scores represent the average of Cole's pitch-specific whiff rate and each hitter's whiff rate against that pitch type. They are exploratory comparison scores, not predicted whiff rates.*

## Scouting Report

The full scouting report includes detailed analysis, visualizations, matchup evaluations, and key takeaways.

[View the Gerrit Cole vs. Boston Red Sox Scouting Report](Gerrit_Cole_2026_Red_Sox_Scouting_Report_Portfolio_Format.pdf)

## Project Files

- [R Analysis Script](Cole_Red_Sox_Analysis.R)
- [Statcast Data and Matchup Results](data/)
- [All 11 Visualizations](visualizations/)
- [Scouting Report PDF](Gerrit_Cole_2026_Red_Sox_Scouting_Report_Portfolio_Format.pdf)

## Project Purpose

This project demonstrates how baseball analytics can support scouting and game preparation by combining pitcher performance, hitter tendencies, and pitch matchup analysis to help inform potential in-game decisions.
