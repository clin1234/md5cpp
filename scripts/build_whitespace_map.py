#!/usr/bin/env python3

import os
import sys
import textwrap
import pathlib

codepoint_list = []
category_list = ["Zs"]

path = pathlib.Path("unicode/DerivedGeneralCategory.txt")
with path.open() as f:
    # Filter codepoints falling in the right category:
    for line in f:
        comment_off = line.find("#")
        if comment_off >= 0:
            line = line[:comment_off]
        line = line.strip()
        if not line:
            continue

        char_range, category = line.split(";")
        char_range = char_range.strip()
        category = category.strip()

        if category not in category_list:
            continue

        delim_off = char_range.find("..")
        if delim_off >= 0:
            codepoint0 = int(char_range[:delim_off], 16)
            codepoint1 = int(char_range[delim_off + 2 :], 16)
            for codepoint in range(codepoint0, codepoint1 + 1):
                codepoint_list.append(codepoint)
        else:
            codepoint = int(char_range, 16)
            codepoint_list.append(codepoint)

codepoint_list.sort()

index0 = 0
count = len(codepoint_list)

records = list()
while index0 < count:
    index1 = index0 + 1
    while index1 < count and codepoint_list[index1] == codepoint_list[index1 - 1] + 1:
        index1 += 1

    if index1 - index0 > 1:
        # Range of codepoints
        records.append(
            "R(0x{:04x},0x{:04x})".format(
                codepoint_list[index0], codepoint_list[index1 - 1]
            )
        )
    else:
        # Single codepoint
        records.append("S(0x{:04x})".format(codepoint_list[index0]))

    index0 = index1

print("static constexpr unsigned WHITESPACE_MAP[] = {")
print(
    textwrap.fill(
        ", ".join(records), 110, initial_indent="    ", subsequent_indent="    "
    )
)
print("}")
