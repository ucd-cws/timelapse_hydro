# Timelapse Hydrography & Thermohydrographs

This repository is set up to provide an R-based process to take timelapse photos from any given site, sync with flow or temperature data, and overlay plots (hydrograph or thermohydrograph) in the photos before stitching together into a mp4.

http://ucd-cws.github.io/timelapse_hydro/index.html

This is a process that can take some time; running in parallel may speed initial processing of photos.

### Coded by:

* Ryan Peek
* Eric Holmes
* Nick Santos

## Examples

*This is a brief example of a timelapse of the Tuolumne River at the Clavey confluence from fall 2015:*

<a href="https://dl.dropboxusercontent.com/u/108583959/tuolapse_2015-09-12.mp4">Tuolumne River Streamlapse Example 2015</a>
<iframe src="https://dl.dropboxusercontent.com/u/108583959/tuolapse_2015-09-12.mp4" frameborder="0" allowfullscreen></iframe>

For a comparable example video using code developed by Nick Santos (using python and perl):

<iframe src="https://player.vimeo.com/video/120654456" width="500" height="188" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe> <p><a href="https://vimeo.com/120654456">Cosumnes River Streamlapse 2015</a> from <a href="https://vimeo.com/user12653303">Center for Watershed Sciences</a> on <a href="https://vimeo.com">Vimeo</a>.</p> <p>A Streamlapse video of the Cosumnes River for the early 2015 floods</p>

<iframe src="https://player.vimeo.com/video/46239865?color=ffffff" width="500" height="278" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe> <p><a href="https://vimeo.com/46239865">Time Lapse Hydrography for North Fork American and South Fork Yuba Rivers in California</a> from <a href="https://vimeo.com/user12653303">Center for Watershed Sciences</a> on <a href="https://vimeo.com">Vimeo</a>.</p> <p>The Center for Watershed Sciences has a set of study sites on rivers throughout the northern Sierra Nevada range in California. Part of this research includes monitoring the river&#039;s edge habitat with remote game cameras. After a year&#039;s worth of captures, we realized we could do some other processing with these videos. This video shows images and hydrographs from two nearby rivers capturing the same moment in time.<br /> <br /> About the images:<br /> The images in this video are taken by a pair of identical game cameras that take an hourly picture, on the hour, 24 hours a day. The left images are from the North Fork American River in the 1000-2000 foot elevation band. The images on the right are from the South Fork Yuba River, about 20 miles north of the first camera in the Sierra Nevada. The main difference we&#039;re interested in from these cameras is that the South Fork Yuba is regulated - it has a dam upstream controlling the flow of the river - while the North Fork American is not regulated in this area. You can see the differences in how the flows change. The South Fork Yuba changes flows rapidly as flows lower while the North Fork American drops more slowly. At high flows (storm events) the North Fork American changes more rapidly - a good signal - while the South Fork Yuba is buffered by the dam&#039;s controlled flow.<br /> <br /> How the video is made:<br /> For this video, we pulled in river stage data from USGS flow gages at nearby, but not exact, locations. Using Python and matplotlib, we created a hydrograph for each time. Then, using Perl and ImageMagick, we overlaid the hydrographs with images based on the timestamps and then paired images together based upon closest timestamps (which also leads to the stuttering you see on the right side). Finally, we used Virtualdub to turn the completed frames into a 10 FPS movie.</p>
