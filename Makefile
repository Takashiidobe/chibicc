CFLAGS=-std=c11 -g -fno-common

chibicc: main.o
	$(CC) -o chibicc main.o $(LDFLAGS)

test: chibicc
	./test.sh

clean:
	rm -f chibicc *.o *~ tmp*

.PHONY: test clean

SOURCE_DOCS := $(shell find notes -type f -name "*.md")

HTML_FILES=$(SOURCE_DOCS:notes/%.md=site/%.html)

site: mkdir $(HTML_FILES) fix_links
	open site/index.html

site/%.html: notes/%.md templates/site.html
	pandoc -f markdown+fenced_divs -s $< -o $@ --table-of-contents --filter=pandoc-include --filter=pandoc-include-code --template templates/site.html

fix_links: $(HTML_FILES)
	./bin/convert-html.sh

site_clean:
	rm -r site/*

.PHONY: mkdir
mkdir:
	mkdir -p site
	rsync -a --include='*/' \
	--include="*.png" \
	--include="*.jpg" \
	--include="*.jpeg" \
	--exclude='*' notes/ site/

