# RootMeasurement

I have updated this code so that RootMeasure.R is a single step and can have multiple time points, if using a stack of images.
Just source the code in R, and follow the on-screen directions.

"Enter Scale Correction Factor" is the value of how many pixels per mm, used in later calculations
"Enter Angle B Correction Factor" is the value to make vertical truly vertical.  Enter 0 if images are perfectly vertical, positive values if sloped from upper-left to lower-right, and negative values if sloped from upper-right to lower-left.

This code provides an automated way to processes Snake files (from JFilament plugin for ImageJ) into length, column length, angle of root (B), straightness of the root, wave density of root (number of bends per mm), and horizontal growth index.

Length (L):                     Integrated length from start point to end point
Column length (Lc):             Straight-line from start point to end point
Straightness (STR):             Lc/L
Wave density (WD):              Number of root bends per mm
Angle (B):                      Angle from start point to end point
Horizontal growth index (HGI):  How far horizontally the root grows

Thank you for using the RootMeasurement R scripts, in conjunction with JFilament and ImageJ.
