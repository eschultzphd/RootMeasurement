# RootMeasurement

Thank you for using the RootMeasurement R scripts, in conjunction with JFilament and ImageJ!

First run RootMeasureFirst, input appropriate values, then run RootMeasureSecond.

RootMeasureFirst:
"Enter Path" is path to Snake file, output from JFilament
"Enter Save Path" is where you want to save the file
"Enter Scale Correction Factor" is the value of how many pixels per mm, used in later calculations
"Enter Angle B Correction Factor" is the value to make vertical truly vertical.  Enter 0 if images are perfectly vertical, positive values if sloped from upper-left to lower-right, and negative values if sloped from upper-right to lower-left.

Automated way to processes Snake files (from JFilament plugin for ImageJ) into length, column length, angle of root (B), straightness of the root, wave density of root (number of bends per mm), and horizontal growth index.
Length (L):  Integrated length from start point to end point
Column length (Lc):  Straight-line from start point to end point
Angle (B):  Angle from start point to end point
Straightness (STR):  Lc/L
Wave density (WD):  Number of root bends per mm
Horizontal growth index (HGI):  How far horizontally the root grows
