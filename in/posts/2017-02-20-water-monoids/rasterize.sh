#!/usr/bin/env bash

inkscape --export-png=structure.png \
         --export-width=800         \
         --export-background=white  \
         structure.svg

inkscape --export-png=left-face.png \
         --export-width=800         \
         --export-background=white  \
         left-face.svg

inkscape --export-png=thumb.png \
         --export-width=800         \
         --export-background=white  \
         thumb.svg

convert structure.png   \
        -gravity center \
        -extent 800x400 \
        twitter.png
