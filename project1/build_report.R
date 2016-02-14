
# Load packages
require(knitr)
require(markdown)


# Create .md, .html, and .pdf files
knit("PA1_template.Rmd")
markdownToHTML('My_Analysis.md', 'My_Analysis.html', options=c("use_xhml"))
system("pandoc -s My_Analysis.html -o My_Analysis.pdf")
