#!/usr/bin/env python3

# Taken from https://stackoverflow.com/questions/46952210/remove-html-comments-from-markdown-file
import sys
from bs4 import BeautifulSoup, Comment

md_input = sys.stdin.read()

soup = BeautifulSoup(md_input, "html5lib")

for element in soup(text=lambda text: isinstance(text, Comment)):
    element.extract()

# bs4 wraps the text in <html><head></head><body>…</body></html>,
# so we need to extract it:

output = "".join(map(str, soup.find("body").contents))

print(output)
