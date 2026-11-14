TRUNK ?= /home/deepayan/svn/all/r-project/R/trunk

all: trunk

trunk:
	Rscript index_builder.R --trunk=$(TRUNK)

cran:
	@if [ -z "$(PKG)" ] || [ -z "$(TARBALL)$(SOURCE)" ]; then \
		echo "Usage: make cran PKG=<name> TARBALL=<path.tar.gz> (or SOURCE=<dir>)"; \
		exit 1; \
	fi
	@if [ -n "$(TARBALL)" ]; then \
		Rscript index_builder.R --pkg=$(PKG) --tarball=$(TARBALL); \
	else \
		Rscript index_builder.R --pkg=$(PKG) --source=$(SOURCE); \
	fi

search-index:
	Rscript index_builder.R --build-search-index

clean:
	rm -rf metadata/
	rm -f taginfo.json
