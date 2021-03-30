# Bounding Box App
Command-line program for finding bounding boxes from input

See requirements doc here: BoundingBox.md

## Design Choices
The twin recursive algorithms at the heart of the program share a
cache of visited coordinates. This is highly over-engineered for the
task but would yield large performance gains over large inputs

I used Airframe (https://wvlet.org/airframe) for the console app 
framework and SBT Pack (https://github.com/xerial/sbt-pack) to generate
the bash launch script. 

## Building
You can build the application with the supplied SBT script:

Build:
`./sbt pack`

Run:
`cat groups.txt | ./target/pack/bin/bounding-box`

## Installing

You can install the program to your `~/local/bin` with the following:`./sbt packInstall`