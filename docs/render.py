#!/usr/bin/env python3
"""Rebuild an image from imageCompressor's output.

Usage: ./imageCompressor -n 8 -l 0.5 -f pixels.txt | python3 docs/render.py WIDTH HEIGHT > out.ppm

Every pixel listed under a cluster gets that cluster's centroid colour.
The result is written as a binary PPM (convert it with ImageMagick if needed).
"""

import re
import sys

PIXEL = re.compile(r"\((\d+),(\d+)\) \((\d+),(\d+),(\d+)\)")
COLOR = re.compile(r"\((\d+),(\d+),(\d+)\)")


def main():
    width, height = int(sys.argv[1]), int(sys.argv[2])
    image = bytearray(width * height * 3)
    centroid = (0, 0, 0)
    expect_centroid = False

    for line in sys.stdin:
        line = line.strip()
        if line == "--":
            expect_centroid = True
        elif expect_centroid:
            centroid = tuple(int(v) for v in COLOR.fullmatch(line).groups())
            expect_centroid = False
        elif (match := PIXEL.fullmatch(line)):
            x, y = int(match[1]), int(match[2])
            offset = (y * width + x) * 3
            image[offset:offset + 3] = bytes(centroid)

    sys.stdout.buffer.write(b"P6 %d %d 255\n" % (width, height))
    sys.stdout.buffer.write(image)


if __name__ == "__main__":
    main()
